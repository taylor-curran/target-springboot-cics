package com.cbsa.migration.service;

import com.cbsa.migration.model.Account;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.dto.AccountUpdateRequestDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Optional;

/**
 * This service replicates the functionality of the UPDACC COBOL program.
 * It handles account updates for account type, interest rate, and overdraft limit.
 * Balance fields are not updated as per COBOL business logic.
 */
@Service
public class AccountUpdateService {

    private final AccountRepository accountRepository;

    @Autowired
    public AccountUpdateService(AccountRepository accountRepository) {
        this.accountRepository = accountRepository;
    }

    /**
     * Updates an account with the provided information.
     * Replicates UPDACC COBOL program logic:
     * - Only updates account type, interest rate, and overdraft limit
     * - Validates account type is not blank or spaces
     * - Returns success/failure status
     *
     * @param sortCode the sort code
     * @param accountNumber the account number
     * @param updateRequest the update request data
     * @return true if update was successful, false otherwise
     */
    public boolean updateAccount(String sortCode, String accountNumber, AccountUpdateRequestDto updateRequest) {
        try {
            Optional<Account> accountOpt = accountRepository.findById(sortCode, accountNumber);
            
            if (!accountOpt.isPresent()) {
                return false;
            }
            
            Account account = accountOpt.get();
            
            String accountType = updateRequest.getAccountType();
            if (accountType == null || accountType.trim().isEmpty()) {
                return false;
            }
            
            account.setAccountType(accountType.trim());
            
            if (updateRequest.getInterestRate() != null) {
                account.setInterestRate(updateRequest.getInterestRate());
            }
            
            if (updateRequest.getOverdraftLimit() != null) {
                account.setOverdraftLimit(updateRequest.getOverdraftLimit());
            }
            
            accountRepository.save(account);
            
            return true;
            
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Gets account information for display purposes.
     * Used to retrieve current account data before updates.
     *
     * @param sortCode the sort code
     * @param accountNumber the account number
     * @return the account if found, empty otherwise
     */
    public Optional<Account> getAccount(String sortCode, String accountNumber) {
        return accountRepository.findById(sortCode, accountNumber);
    }
}
