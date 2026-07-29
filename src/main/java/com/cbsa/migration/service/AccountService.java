package com.cbsa.migration.service;

import com.cbsa.migration.dto.AccountDTO;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.CustomerRepository;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Read-only account retrieval migrated from the INQACC and INQACCCU COBOL programs.
 */
@Service
public class AccountService {

    /** INQACCCU stores at most 20 accounts per customer (ACCOUNT-DETAILS OCCURS 1 TO 20) */
    public static final int MAX_ACCOUNTS_PER_CUSTOMER = 20;

    /** INQACC treats account number 99999999 as "read the last account" */
    public static final String LAST_ACCOUNT_NUMBER = "99999999";

    private final AccountRepository accountRepository;
    private final CustomerRepository customerRepository;
    private final SortCodeService sortCodeService;

    public AccountService(AccountRepository accountRepository,
                          CustomerRepository customerRepository,
                          SortCodeService sortCodeService) {
        this.accountRepository = accountRepository;
        this.customerRepository = customerRepository;
        this.sortCodeService = sortCodeService;
    }

    /**
     * Retrieve a single account on the composite key (sort code + account number).
     * Equivalent to INQACC: the sort code comes from the SORTCODE copybook and the
     * sentinel account number 99999999 returns the highest-numbered account.
     */
    public Optional<AccountDTO> getAccount(String sortCode, String accountNumber) {
        String effectiveSortCode = resolveSortCode(sortCode);

        Optional<Account> account = LAST_ACCOUNT_NUMBER.equals(accountNumber)
                ? accountRepository.findTopBySortCodeOrderByAccountNumberDesc(effectiveSortCode)
                : accountRepository.findById(effectiveSortCode, accountNumber);

        return account.map(this::toDto);
    }

    /**
     * Retrieve the accounts belonging to a customer, browsing the cursor from the
     * optional {@code cursor} account number. Equivalent to INQACCCU: the customer
     * is verified first (fail code '1' when absent), rows are restricted to the
     * bank sort code, and no more than {@link #MAX_ACCOUNTS_PER_CUSTOMER} accounts
     * are ever returned for a customer.
     *
     * @return empty when the customer does not exist
     */
    public Optional<AccountDTO.AccountListDTO> getAccountsForCustomer(Long customerNumber,
                                                                     String sortCode,
                                                                     String cursor,
                                                                     Integer limit) {
        String effectiveSortCode = resolveSortCode(sortCode);

        if (customerRepository.findById(effectiveSortCode, customerNumber).isEmpty()) {
            return Optional.empty();
        }

        int pageSize = resolvePageSize(limit);

        List<Account> customerAccounts = accountRepository.findByCustomerNumber(customerNumber).stream()
                .filter(account -> effectiveSortCode.equals(account.getSortCode()))
                .sorted(Comparator.comparing(Account::getAccountNumber))
                .limit(MAX_ACCOUNTS_PER_CUSTOMER)
                .collect(Collectors.toList());

        List<Account> remaining = customerAccounts.stream()
                .filter(account -> cursor == null || account.getAccountNumber().compareTo(cursor) > 0)
                .collect(Collectors.toList());

        List<AccountDTO> page = remaining.stream()
                .limit(pageSize)
                .map(this::toDto)
                .collect(Collectors.toList());

        boolean hasMore = remaining.size() > page.size();

        return Optional.of(AccountDTO.AccountListDTO.builder()
                .customerNumber(customerNumber)
                .numberOfAccounts(page.size())
                .accounts(page)
                .nextCursor(hasMore ? page.get(page.size() - 1).getAccountNumber() : null)
                .hasMore(hasMore)
                .maxAccountsPerCustomer(MAX_ACCOUNTS_PER_CUSTOMER)
                .build());
    }

    private String resolveSortCode(String sortCode) {
        return (sortCode == null || sortCode.isEmpty()) ? sortCodeService.getSortCode() : sortCode;
    }

    private int resolvePageSize(Integer limit) {
        if (limit == null || limit < 1) {
            return MAX_ACCOUNTS_PER_CUSTOMER;
        }
        return Math.min(limit, MAX_ACCOUNTS_PER_CUSTOMER);
    }

    private AccountDTO toDto(Account account) {
        return AccountDTO.builder()
                .eyeCatcher(account.getEyeCatcher())
                .customerNumber(account.getCustomerNumber())
                .sortCode(account.getSortCode())
                .accountNumber(account.getAccountNumber())
                .accountType(account.getAccountType())
                .interestRate(account.getInterestRate())
                .openedDate(account.getOpenedDate())
                .overdraftLimit(account.getOverdraftLimit())
                .lastStatementDate(account.getLastStatementDate())
                .nextStatementDate(account.getNextStatementDate())
                .availableBalance(account.getAvailableBalance())
                .actualBalance(account.getActualBalance())
                .build();
    }
}
