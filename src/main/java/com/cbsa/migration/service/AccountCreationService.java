package com.cbsa.migration.service;

import com.cbsa.migration.dto.AccountCreationRequestDto;
import com.cbsa.migration.dto.AccountCreationResponseDto;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.repository.AccountRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

@Service
public class AccountCreationService {

    private final AccountRepository accountRepository;
    private final SortCodeService sortCodeService;

    @Autowired
    public AccountCreationService(AccountRepository accountRepository, SortCodeService sortCodeService) {
        this.accountRepository = accountRepository;
        this.sortCodeService = sortCodeService;
    }

    public AccountCreationResponseDto createAccount(AccountCreationRequestDto request) {
        try {
            if (!validateCustomerExists(request.getCustomerNumber())) {
                return new AccountCreationResponseDto(false, "1");
            }

            if (!validateAccountLimit(request.getCustomerNumber())) {
                return new AccountCreationResponseDto(false, "8");
            }

            if (!validateAccountType(request.getAccountType())) {
                return new AccountCreationResponseDto(false, "2");
            }

            String sortCode = sortCodeService.getSortCode();
            String accountNumber = generateNextAccountNumber(sortCode);

            Account account = createAccountEntity(request, sortCode, accountNumber);
            Account savedAccount = accountRepository.save(account);

            return mapToResponse(savedAccount, true, null);

        } catch (Exception e) {
            return new AccountCreationResponseDto(false, "9");
        }
    }

    private boolean validateCustomerExists(Long customerNumber) {
        return customerNumber != null && customerNumber > 0;
    }

    private boolean validateAccountLimit(Long customerNumber) {
        List<Account> existingAccounts = accountRepository.findByCustomerNumber(customerNumber);
        return existingAccounts.size() < 9;
    }

    private boolean validateAccountType(String accountType) {
        if (accountType == null || accountType.trim().isEmpty()) {
            return false;
        }
        
        String[] validTypes = {"CURRENT", "SAVINGS", "LOAN", "MORTGAGE", "ISA"};
        String trimmedType = accountType.trim().toUpperCase();
        
        for (String validType : validTypes) {
            if (validType.equals(trimmedType)) {
                return true;
            }
        }
        return false;
    }

    private String generateNextAccountNumber(String sortCode) {
        return accountRepository.findTopBySortCodeOrderByAccountNumberDesc(sortCode)
                .map(account -> {
                    long nextNumber = Long.parseLong(account.getAccountNumber()) + 1;
                    return String.format("%08d", nextNumber);
                })
                .orElse("00000001");
    }

    private Account createAccountEntity(AccountCreationRequestDto request, String sortCode, String accountNumber) {
        Account account = new Account();
        account.setEyeCatcher("ACCT");
        account.setCustomerNumber(request.getCustomerNumber());
        account.setSortCode(sortCode);
        account.setAccountNumber(accountNumber);
        account.setAccountType(request.getAccountType().trim().toUpperCase());
        account.setInterestRate(request.getInterestRate());
        account.setOpenedDate(LocalDate.now());
        account.setOverdraftLimit(request.getOverdraftLimit());
        account.setLastStatementDate(LocalDate.now());
        account.setNextStatementDate(LocalDate.now().plusMonths(1));
        account.setAvailableBalance(BigDecimal.ZERO);
        account.setActualBalance(BigDecimal.ZERO);
        return account;
    }

    private AccountCreationResponseDto mapToResponse(Account account, boolean success, String failCode) {
        AccountCreationResponseDto response = new AccountCreationResponseDto(success, failCode);
        
        if (success && account != null) {
            response.setEyeCatcher(account.getEyeCatcher());
            response.setCustomerNumber(account.getCustomerNumber());
            response.setSortCode(account.getSortCode());
            response.setAccountNumber(account.getAccountNumber());
            response.setAccountType(account.getAccountType());
            response.setInterestRate(account.getInterestRate());
            response.setOpenedDate(account.getOpenedDate());
            response.setOverdraftLimit(account.getOverdraftLimit());
            response.setLastStatementDate(account.getLastStatementDate());
            response.setNextStatementDate(account.getNextStatementDate());
            response.setAvailableBalance(account.getAvailableBalance());
            response.setActualBalance(account.getActualBalance());
        }
        
        return response;
    }
}
