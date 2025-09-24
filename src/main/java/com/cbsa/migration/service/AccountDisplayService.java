package com.cbsa.migration.service;

import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.dto.AccountResponseDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Optional;

/**
 * This service replicates the functionality of the BNK1DAC COBOL program.
 * It handles account information display and inquiry operations.
 * Maps to INQACC.cpy structure for account data retrieval.
 */
@Service
public class AccountDisplayService {

    private final AccountRepository accountRepository;
    private final CustomerRepository customerRepository;

    @Autowired
    public AccountDisplayService(AccountRepository accountRepository, CustomerRepository customerRepository) {
        this.accountRepository = accountRepository;
        this.customerRepository = customerRepository;
    }

    /**
     * Displays account information by sort code and account number.
     * Replicates BNK1DAC COBOL program logic:
     * - Retrieves account data from database
     * - Includes customer name for convenience
     * - Returns success/failure status
     *
     * @param sortCode the sort code
     * @param accountNumber the account number
     * @return AccountResponseDto with account information, or null if not found
     */
    public AccountResponseDto displayAccount(String sortCode, String accountNumber) {
        try {
            Optional<Account> accountOpt = accountRepository.findById(sortCode, accountNumber);
            
            if (!accountOpt.isPresent()) {
                return null;
            }
            
            Account account = accountOpt.get();
            
            String customerName = null;
            Optional<Customer> customerOpt = customerRepository.findById(sortCode, account.getCustomerNumber());
            if (customerOpt.isPresent()) {
                customerName = customerOpt.get().getName();
            }
            
            return AccountResponseDto.builder()
                .accountNumber(account.getAccountNumber())
                .sortCode(account.getSortCode())
                .accountType(account.getAccountType())
                .customerNumber(account.getCustomerNumber())
                .customerName(customerName)
                .availableBalance(account.getAvailableBalance())
                .actualBalance(account.getActualBalance())
                .interestRate(account.getInterestRate())
                .openedDate(account.getOpenedDate())
                .overdraftLimit(account.getOverdraftLimit())
                .lastStatementDate(account.getLastStatementDate())
                .nextStatementDate(account.getNextStatementDate())
                .status("ACTIVE")
                .build();
                
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Gets account information for basic lookup.
     * Used for simple account retrieval operations.
     *
     * @param sortCode the sort code
     * @param accountNumber the account number
     * @return the account if found, empty otherwise
     */
    public Optional<Account> getAccount(String sortCode, String accountNumber) {
        return accountRepository.findById(sortCode, accountNumber);
    }
}
