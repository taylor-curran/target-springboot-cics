package com.cbsa.migration.service;

import com.cbsa.migration.dto.TransferRequestDto;
import com.cbsa.migration.dto.TransferResponseDto;
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
import java.util.concurrent.atomic.AtomicLong;

@Service
public class TransferFundsService {

    private static final Logger logger = LoggerFactory.getLogger(TransferFundsService.class);

    private static final String FAIL_CODE_FROM_NOT_FOUND = "1";
    private static final String FAIL_CODE_TO_NOT_FOUND = "2";
    private static final String FAIL_CODE_UNEXPECTED_ERROR = "3";
    private static final String FAIL_CODE_INVALID_AMOUNT = "4";

    private static final String INVALID_ACCOUNT_NUMBER = "00000000";

    private final AccountRepository accountRepository;
    private final TransactionRepository transactionRepository;
    private final AtomicLong referenceNumberGenerator;

    public TransferFundsService(AccountRepository accountRepository, 
                                TransactionRepository transactionRepository) {
        this.accountRepository = accountRepository;
        this.transactionRepository = transactionRepository;
        this.referenceNumberGenerator = new AtomicLong(System.currentTimeMillis());
    }

    @Transactional
    public TransferResponseDto transferFunds(TransferRequestDto request) {
        logger.info("Processing transfer request: {} {} -> {} {} amount={}", 
                   request.getFromSortCode(), request.getFromAccountNumber(),
                   request.getToSortCode(), request.getToAccountNumber(),
                   request.getAmount());

        if (request.getFromAccountNumber().equals(INVALID_ACCOUNT_NUMBER)) {
            logger.warn("FROM account number 00000000 is not valid");
            return buildErrorResponse(FAIL_CODE_FROM_NOT_FOUND, 
                    "Account number 00000000 is not valid", request.getAmount());
        }

        if (request.getToAccountNumber().equals(INVALID_ACCOUNT_NUMBER)) {
            logger.warn("TO account number 00000000 is not valid");
            return buildErrorResponse(FAIL_CODE_TO_NOT_FOUND, 
                    "Account number 00000000 is not valid", request.getAmount());
        }

        if (request.getFromSortCode().equals(request.getToSortCode()) &&
            request.getFromAccountNumber().equals(request.getToAccountNumber())) {
            logger.warn("FROM and TO accounts must be different");
            return buildErrorResponse(FAIL_CODE_UNEXPECTED_ERROR, 
                    "FROM and TO accounts must be different", request.getAmount());
        }

        if (request.getAmount() == null || request.getAmount().compareTo(BigDecimal.ZERO) <= 0) {
            logger.warn("Transfer amount must be greater than zero");
            return buildErrorResponse(FAIL_CODE_INVALID_AMOUNT, 
                    "Amount must be greater than zero", request.getAmount());
        }

        Optional<Account> fromAccountOpt = accountRepository.findById(
                request.getFromSortCode(), request.getFromAccountNumber());
        
        if (fromAccountOpt.isEmpty()) {
            logger.warn("FROM account not found: {} {}", 
                       request.getFromSortCode(), request.getFromAccountNumber());
            return buildErrorResponse(FAIL_CODE_FROM_NOT_FOUND, 
                    "FROM account " + request.getFromAccountNumber() + " not found", 
                    request.getAmount());
        }

        Optional<Account> toAccountOpt = accountRepository.findById(
                request.getToSortCode(), request.getToAccountNumber());
        
        if (toAccountOpt.isEmpty()) {
            logger.warn("TO account not found: {} {}", 
                       request.getToSortCode(), request.getToAccountNumber());
            return buildErrorResponse(FAIL_CODE_TO_NOT_FOUND, 
                    "TO account " + request.getToAccountNumber() + " not found", 
                    request.getAmount());
        }

        Account fromAccount = fromAccountOpt.get();
        Account toAccount = toAccountOpt.get();

        BigDecimal fromAvailableBalance = fromAccount.getAvailableBalance();
        if (fromAvailableBalance.compareTo(request.getAmount()) < 0) {
            logger.warn("Insufficient funds in FROM account: available={}, requested={}", 
                       fromAvailableBalance, request.getAmount());
            return buildErrorResponse(FAIL_CODE_UNEXPECTED_ERROR, 
                    "Insufficient funds in FROM account", request.getAmount());
        }

        try {
            fromAccount.setAvailableBalance(
                    fromAccount.getAvailableBalance().subtract(request.getAmount()));
            fromAccount.setActualBalance(
                    fromAccount.getActualBalance().subtract(request.getAmount()));
            accountRepository.save(fromAccount);

            toAccount.setAvailableBalance(
                    toAccount.getAvailableBalance().add(request.getAmount()));
            toAccount.setActualBalance(
                    toAccount.getActualBalance().add(request.getAmount()));
            accountRepository.save(toAccount);

            recordTransferTransaction(request, fromAccount, toAccount);

            logger.info("Transfer completed successfully: {} {} -> {} {} amount={}", 
                       request.getFromSortCode(), request.getFromAccountNumber(),
                       request.getToSortCode(), request.getToAccountNumber(),
                       request.getAmount());

            return buildSuccessResponse(request.getAmount(), fromAccount, toAccount);

        } catch (Exception e) {
            logger.error("Unexpected error during transfer: {}", e.getMessage(), e);
            return buildErrorResponse(FAIL_CODE_UNEXPECTED_ERROR, 
                    "Unexpected error during transfer: " + e.getMessage(), 
                    request.getAmount());
        }
    }

    private void recordTransferTransaction(TransferRequestDto request, 
                                           Account fromAccount, 
                                           Account toAccount) {
        LocalDate today = LocalDate.now();
        LocalTime now = LocalTime.now();
        long referenceNumber = referenceNumberGenerator.incrementAndGet();

        Transaction fromTransaction = Transaction.builder()
                .eyeCatcher(Transaction.VALID_EYECATCHER)
                .logicallyDeleted(false)
                .sortCode(fromAccount.getSortCode())
                .accountNumber(fromAccount.getAccountNumber())
                .transactionDate(today)
                .transactionTime(now)
                .referenceNumber(referenceNumber)
                .transactionType(Transaction.TYPE_TRANSFER)
                .description("Transfer to " + toAccount.getAccountNumber())
                .amount(request.getAmount().negate())
                .targetSortCode(toAccount.getSortCode())
                .targetAccountNumber(toAccount.getAccountNumber())
                .build();

        transactionRepository.save(fromTransaction);

        Transaction toTransaction = Transaction.builder()
                .eyeCatcher(Transaction.VALID_EYECATCHER)
                .logicallyDeleted(false)
                .sortCode(toAccount.getSortCode())
                .accountNumber(toAccount.getAccountNumber())
                .transactionDate(today)
                .transactionTime(now)
                .referenceNumber(referenceNumber + 1)
                .transactionType(Transaction.TYPE_TRANSFER)
                .description("Transfer from " + fromAccount.getAccountNumber())
                .amount(request.getAmount())
                .targetSortCode(fromAccount.getSortCode())
                .targetAccountNumber(fromAccount.getAccountNumber())
                .build();

        transactionRepository.save(toTransaction);

        logger.debug("Recorded transfer transactions: fromRef={}, toRef={}", 
                    referenceNumber, referenceNumber + 1);
    }

    private TransferResponseDto buildSuccessResponse(BigDecimal amount, 
                                                     Account fromAccount, 
                                                     Account toAccount) {
        return TransferResponseDto.builder()
                .success(true)
                .transferAmount(amount)
                .fromAccount(TransferResponseDto.AccountBalanceDto.builder()
                        .sortCode(fromAccount.getSortCode())
                        .accountNumber(fromAccount.getAccountNumber())
                        .availableBalance(fromAccount.getAvailableBalance())
                        .actualBalance(fromAccount.getActualBalance())
                        .build())
                .toAccount(TransferResponseDto.AccountBalanceDto.builder()
                        .sortCode(toAccount.getSortCode())
                        .accountNumber(toAccount.getAccountNumber())
                        .availableBalance(toAccount.getAvailableBalance())
                        .actualBalance(toAccount.getActualBalance())
                        .build())
                .build();
    }

    private TransferResponseDto buildErrorResponse(String failCode, 
                                                   String errorMessage, 
                                                   BigDecimal amount) {
        return TransferResponseDto.builder()
                .success(false)
                .failCode(failCode)
                .errorMessage(errorMessage)
                .transferAmount(amount)
                .build();
    }
}
