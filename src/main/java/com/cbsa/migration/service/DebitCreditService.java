package com.cbsa.migration.service;

import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.DebitCreditRequest;
import com.cbsa.migration.model.DebitCreditResponse;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Optional;

/**
 * This service replicates the functionality of the DBCRFUN COBOL program.
 * It handles debit and credit operations including balance validation,
 * business rule enforcement, and transaction logging.
 */
@Service
public class DebitCreditService {

    private final AccountRepository accountRepository;
    private final TransactionRepository transactionRepository;

    @Autowired
    public DebitCreditService(AccountRepository accountRepository, TransactionRepository transactionRepository) {
        this.accountRepository = accountRepository;
        this.transactionRepository = transactionRepository;
    }

    /**
     * Processes a debit or credit transaction
     *
     * @param request The debit/credit request
     * @return DebitCreditResponse with updated balances or failure information
     */
    @Transactional
    public DebitCreditResponse processTransaction(DebitCreditRequest request) {
        try {
            Optional<Account> accountOpt = accountRepository.findById(request.getSortCode(), request.getAccountNumber());
            
            if (!accountOpt.isPresent()) {
                return DebitCreditResponse.builder()
                        .success(false)
                        .failureCode("1")
                        .build();
            }

            Account account = accountOpt.get();

            if (request.getAmount().compareTo(BigDecimal.ZERO) < 0) {
                DebitCreditResponse validationResult = validateDebitTransaction(account, request);
                if (!validationResult.isSuccess()) {
                    return validationResult;
                }
            } else {
                DebitCreditResponse validationResult = validateCreditTransaction(account, request);
                if (!validationResult.isSuccess()) {
                    return validationResult;
                }
            }

            BigDecimal newAvailableBalance = account.getAvailableBalance().add(request.getAmount());
            BigDecimal newActualBalance = account.getActualBalance().add(request.getAmount());

            account.setAvailableBalance(newAvailableBalance);
            account.setActualBalance(newActualBalance);

            accountRepository.save(account);

            createTransactionRecord(account, request);

            return DebitCreditResponse.builder()
                    .success(true)
                    .failureCode("0")
                    .sortCode(request.getSortCode())
                    .accountNumber(request.getAccountNumber())
                    .availableBalance(newAvailableBalance)
                    .actualBalance(newActualBalance)
                    .build();

        } catch (Exception e) {
            return DebitCreditResponse.builder()
                    .success(false)
                    .failureCode("2")
                    .build();
        }
    }

    private DebitCreditResponse validateDebitTransaction(Account account, DebitCreditRequest request) {
        if (("MORTGAGE".equals(account.getAccountType()) || "LOAN".equals(account.getAccountType())) 
            && request.getFacilityType() != null && request.getFacilityType() == 496) {
            return DebitCreditResponse.builder()
                    .success(false)
                    .failureCode("4")
                    .build();
        }

        BigDecimal difference = account.getAvailableBalance().add(request.getAmount());
        if (difference.compareTo(BigDecimal.ZERO) < 0 && 
            request.getFacilityType() != null && request.getFacilityType() == 496) {
            return DebitCreditResponse.builder()
                    .success(false)
                    .failureCode("3")
                    .build();
        }

        return DebitCreditResponse.builder().success(true).build();
    }

    private DebitCreditResponse validateCreditTransaction(Account account, DebitCreditRequest request) {
        if (("MORTGAGE".equals(account.getAccountType()) || "LOAN".equals(account.getAccountType())) 
            && request.getFacilityType() != null && request.getFacilityType() == 496) {
            return DebitCreditResponse.builder()
                    .success(false)
                    .failureCode("4")
                    .build();
        }

        return DebitCreditResponse.builder().success(true).build();
    }

    private void createTransactionRecord(Account account, DebitCreditRequest request) {
        String transactionType;
        String description;

        if (request.getAmount().compareTo(BigDecimal.ZERO) < 0) {
            if (request.getFacilityType() != null && request.getFacilityType() == 496) {
                transactionType = "PDR";
                description = request.getOrigin() != null ? request.getOrigin().substring(0, Math.min(14, request.getOrigin().length())) : "PAYMENT DEBIT";
            } else {
                transactionType = "DEB";
                description = "COUNTER WTHDRW";
            }
        } else {
            if (request.getFacilityType() != null && request.getFacilityType() == 496) {
                transactionType = "PCR";
                description = request.getOrigin() != null ? request.getOrigin().substring(0, Math.min(14, request.getOrigin().length())) : "PAYMENT CREDIT";
            } else {
                transactionType = "CRE";
                description = "COUNTER RECVED";
            }
        }

        Transaction transaction = Transaction.builder()
                .eyeCatcher("PRTR")
                .logicallyDeleted(false)
                .sortCode(request.getSortCode())
                .accountNumber(request.getAccountNumber())
                .transactionDate(LocalDate.now())
                .transactionTime(LocalTime.now())
                .referenceNumber(System.currentTimeMillis())
                .transactionType(transactionType)
                .description(description)
                .amount(request.getAmount())
                .build();

        transactionRepository.save(transaction);
    }
}
