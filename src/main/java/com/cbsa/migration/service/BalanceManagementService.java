package com.cbsa.migration.service;

import com.cbsa.migration.dto.BalanceUpdateRequestDto;
import com.cbsa.migration.dto.BalanceUpdateResponseDto;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.repository.AccountRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.Optional;

@Service
public class BalanceManagementService {

    private final AccountRepository accountRepository;
    private final SortCodeService sortCodeService;

    @Autowired
    public BalanceManagementService(AccountRepository accountRepository, SortCodeService sortCodeService) {
        this.accountRepository = accountRepository;
        this.sortCodeService = sortCodeService;
    }

    public BalanceUpdateResponseDto updateBalance(String sortCode, String accountNumber, BalanceUpdateRequestDto request) {
        try {
            Optional<Account> accountOpt = accountRepository.findById(sortCode, accountNumber);
            
            if (accountOpt.isEmpty()) {
                return new BalanceUpdateResponseDto(false, "1");
            }

            Account account = accountOpt.get();
            
            if (!validateAccountTypeForPayment(account, request)) {
                return new BalanceUpdateResponseDto(false, "4");
            }

            if (!validateSufficientFunds(account, request)) {
                return new BalanceUpdateResponseDto(false, "3");
            }

            BigDecimal newAvailableBalance = account.getAvailableBalance().add(request.getAmount());
            BigDecimal newActualBalance = account.getActualBalance().add(request.getAmount());

            account.setAvailableBalance(newAvailableBalance);
            account.setActualBalance(newActualBalance);

            Account updatedAccount = accountRepository.save(account);

            BalanceUpdateResponseDto response = new BalanceUpdateResponseDto(true, "0");
            response.setSortCode(updatedAccount.getSortCode());
            response.setAvailableBalance(updatedAccount.getAvailableBalance());
            response.setActualBalance(updatedAccount.getActualBalance());

            return response;

        } catch (Exception e) {
            return new BalanceUpdateResponseDto(false, "2");
        }
    }

    private boolean validateAccountTypeForPayment(Account account, BalanceUpdateRequestDto request) {
        Integer facilityType = request.getFacilityType();
        String accountType = account.getAccountType();
        
        if (facilityType != null && facilityType == 496) {
            return !("MORTGAGE".equals(accountType) || "LOAN".equals(accountType));
        }
        
        return true;
    }

    private boolean validateSufficientFunds(Account account, BalanceUpdateRequestDto request) {
        BigDecimal amount = request.getAmount();
        Integer facilityType = request.getFacilityType();
        
        if (amount.compareTo(BigDecimal.ZERO) < 0 && facilityType != null && facilityType == 496) {
            BigDecimal resultingBalance = account.getAvailableBalance().add(amount);
            return resultingBalance.compareTo(BigDecimal.ZERO) >= 0;
        }
        
        return true;
    }
}
