package com.cbsa.migration.service;

import com.cbsa.migration.dto.DebitCreditRequestDto;
import com.cbsa.migration.dto.DebitCreditResponseDto;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Optional;
import java.util.Random;

@Service
public class DebitCreditService {

    private static final Logger logger = LoggerFactory.getLogger(DebitCreditService.class);
    
    private static final int FACILITY_TYPE_PAYMENT = 496;
    private static final String ACCOUNT_TYPE_MORTGAGE = "MORTGAGE";
    private static final String ACCOUNT_TYPE_LOAN = "LOAN    ";
    
    private static final String TRANSACTION_TYPE_DEBIT = "DEB";
    private static final String TRANSACTION_TYPE_CREDIT = "CRE";
    private static final String TRANSACTION_TYPE_PAYMENT_DEBIT = "PDR";
    private static final String TRANSACTION_TYPE_PAYMENT_CREDIT = "PCR";
    
    private static final String DESC_COUNTER_WITHDRAW = "COUNTER WTHDRW";
    private static final String DESC_COUNTER_RECEIVED = "COUNTER RECVED";
    
    private static final char FAIL_CODE_SUCCESS = '0';
    private static final char FAIL_CODE_NOT_FOUND = '1';
    private static final char FAIL_CODE_DB_ERROR = '2';
    private static final char FAIL_CODE_INSUFFICIENT_FUNDS = '3';
    private static final char FAIL_CODE_INVALID_ACCOUNT_TYPE = '4';
    
    private final AccountRepository accountRepository;
    private final TransactionRepository transactionRepository;
    private final Random random;
    
    public DebitCreditService(AccountRepository accountRepository, 
                             TransactionRepository transactionRepository) {
        this.accountRepository = accountRepository;
        this.transactionRepository = transactionRepository;
        this.random = new Random();
        
        logger.info("DebitCreditService initialized");
    }
    
    @Transactional
    public DebitCreditResponseDto processDebitCredit(DebitCreditRequestDto request) {
        logger.info("Processing debit/credit for account: {}-{}, amount: {}, facilityType: {}", 
                   request.getSortCode(), request.getAccountNumber(), 
                   request.getAmount(), request.getFacilityType());
        
        try {
            Optional<Account> accountOpt = accountRepository.findById(
                request.getSortCode(), request.getAccountNumber());
            
            if (accountOpt.isEmpty()) {
                logger.warn("Account not found: {}-{}", request.getSortCode(), request.getAccountNumber());
                return DebitCreditResponseDto.failure(FAIL_CODE_NOT_FOUND);
            }
            
            Account account = accountOpt.get();
            
            Character validationFailCode = validateTransaction(account, request);
            if (validationFailCode != null) {
                return DebitCreditResponseDto.failure(validationFailCode);
            }
            
            account.setAvailableBalance(account.getAvailableBalance().add(request.getAmount()));
            account.setActualBalance(account.getActualBalance().add(request.getAmount()));
            
            Account updatedAccount = accountRepository.save(account);
            
            Transaction transaction = createTransaction(account, request);
            transactionRepository.save(transaction);
            
            logger.info("Successfully processed debit/credit for account {}-{}, new balance: {}", 
                       request.getSortCode(), request.getAccountNumber(), 
                       updatedAccount.getAvailableBalance());
            
            return DebitCreditResponseDto.success(
                updatedAccount.getAvailableBalance(),
                updatedAccount.getActualBalance()
            );
            
        } catch (Exception e) {
            logger.error("Error processing debit/credit for account {}-{}: {}", 
                        request.getSortCode(), request.getAccountNumber(), e.getMessage(), e);
            return DebitCreditResponseDto.failure(FAIL_CODE_DB_ERROR);
        }
    }
    
    private Character validateTransaction(Account account, DebitCreditRequestDto request) {
        boolean isDebit = request.getAmount().compareTo(BigDecimal.ZERO) < 0;
        boolean isPayment = request.getFacilityType() == FACILITY_TYPE_PAYMENT;
        
        if (isPayment && isRestrictedAccountType(account.getAccountType())) {
            logger.warn("Invalid account type for payment: {} for account {}-{}", 
                       account.getAccountType(), request.getSortCode(), request.getAccountNumber());
            return FAIL_CODE_INVALID_ACCOUNT_TYPE;
        }
        
        if (isDebit && isPayment) {
            BigDecimal newBalance = account.getAvailableBalance().add(request.getAmount());
            if (newBalance.compareTo(BigDecimal.ZERO) < 0) {
                logger.warn("Insufficient funds for account {}-{}: available={}, amount={}", 
                           request.getSortCode(), request.getAccountNumber(), 
                           account.getAvailableBalance(), request.getAmount());
                return FAIL_CODE_INSUFFICIENT_FUNDS;
            }
        }
        
        return null;
    }
    
    private boolean isRestrictedAccountType(String accountType) {
        return ACCOUNT_TYPE_MORTGAGE.equals(accountType) || 
               ACCOUNT_TYPE_LOAN.trim().equals(accountType.trim());
    }
    
    private Transaction createTransaction(Account account, DebitCreditRequestDto request) {
        boolean isDebit = request.getAmount().compareTo(BigDecimal.ZERO) < 0;
        boolean isPayment = request.getFacilityType() == FACILITY_TYPE_PAYMENT;
        
        String transactionType;
        String description;
        
        if (isDebit) {
            if (isPayment) {
                transactionType = TRANSACTION_TYPE_PAYMENT_DEBIT;
                description = request.getOriginDescription();
            } else {
                transactionType = TRANSACTION_TYPE_DEBIT;
                description = DESC_COUNTER_WITHDRAW;
            }
        } else {
            if (isPayment) {
                transactionType = TRANSACTION_TYPE_PAYMENT_CREDIT;
                description = request.getOriginDescription();
            } else {
                transactionType = TRANSACTION_TYPE_CREDIT;
                description = DESC_COUNTER_RECEIVED;
            }
        }
        
        long referenceNumber = (System.currentTimeMillis() % 2000000) * 1000 + random.nextInt(1000);
        
        return Transaction.builder()
            .eyeCatcher(Transaction.VALID_EYECATCHER)
            .sortCode(request.getSortCode())
            .accountNumber(request.getAccountNumber())
            .transactionDate(LocalDate.now())
            .transactionTime(LocalTime.now())
            .referenceNumber(referenceNumber)
            .transactionType(transactionType)
            .description(description)
            .amount(request.getAmount())
            .logicallyDeleted(false)
            .build();
    }
}
