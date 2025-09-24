package com.cbsa.migration.service;

import com.cbsa.migration.dto.AccountInquiryResponseDto;
import com.cbsa.migration.dto.AccountUpdateRequestDto;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.repository.AccountRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class AccountMaintenanceService {

    private final AccountRepository accountRepository;

    @Autowired
    public AccountMaintenanceService(AccountRepository accountRepository) {
        this.accountRepository = accountRepository;
    }

    public AccountInquiryResponseDto updateAccount(String sortCode, String accountNumber, AccountUpdateRequestDto request) {
        try {
            if (!validateAccountType(request.getAccountType())) {
                return new AccountInquiryResponseDto(false);
            }

            Optional<Account> accountOpt = accountRepository.findById(sortCode, accountNumber);
            
            if (accountOpt.isEmpty()) {
                return new AccountInquiryResponseDto(false);
            }

            Account account = accountOpt.get();
            
            account.setAccountType(request.getAccountType().trim().toUpperCase());
            account.setInterestRate(request.getInterestRate());
            account.setOverdraftLimit(request.getOverdraftLimit());

            Account updatedAccount = accountRepository.save(account);
            return mapToResponse(updatedAccount, true);

        } catch (Exception e) {
            return new AccountInquiryResponseDto(false);
        }
    }

    private boolean validateAccountType(String accountType) {
        return accountType != null && 
               !accountType.trim().isEmpty() && 
               !accountType.startsWith(" ");
    }

    private AccountInquiryResponseDto mapToResponse(Account account, boolean success) {
        AccountInquiryResponseDto response = new AccountInquiryResponseDto(success);
        
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
