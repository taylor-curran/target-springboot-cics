package com.cbsa.migration.service;

import com.cbsa.migration.dto.AccountInquiryResponseDto;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.repository.AccountRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class AccountInquiryService {

    private final AccountRepository accountRepository;

    @Autowired
    public AccountInquiryService(AccountRepository accountRepository) {
        this.accountRepository = accountRepository;
    }

    public AccountInquiryResponseDto inquireAccount(String sortCode, String accountNumber) {
        try {
            Optional<Account> accountOpt;
            
            if ("99999999".equals(accountNumber)) {
                accountOpt = accountRepository.findTopBySortCodeOrderByAccountNumberDesc(sortCode);
            } else {
                accountOpt = accountRepository.findById(sortCode, accountNumber);
            }

            if (accountOpt.isPresent()) {
                Account account = accountOpt.get();
                return mapToResponse(account, true);
            } else {
                return new AccountInquiryResponseDto(false);
            }

        } catch (Exception e) {
            return new AccountInquiryResponseDto(false);
        }
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
