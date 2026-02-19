package com.cbsa.migration.service;

import com.cbsa.migration.dto.TransferResponseDto;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DeadlockLoserDataAccessException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.DefaultTransactionDefinition;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Optional;
import java.util.concurrent.ThreadLocalRandom;

@Service
public class TransferService {

    private static final Logger logger = LoggerFactory.getLogger(TransferService.class);

    private static final int MAX_DEADLOCK_RETRIES = 5;
    private static final long DEADLOCK_RETRY_DELAY_MS = 1000;

    private final AccountRepository accountRepository;
    private final TransactionRepository transactionRepository;
    private final PlatformTransactionManager transactionManager;

    public TransferService(AccountRepository accountRepository,
                           TransactionRepository transactionRepository,
                           PlatformTransactionManager transactionManager) {
        this.accountRepository = accountRepository;
        this.transactionRepository = transactionRepository;
        this.transactionManager = transactionManager;
    }

    public TransferResponseDto transferLocal(String fromSortCode, String fromAccountNumber,
                                             String toSortCode, String toAccountNumber,
                                             BigDecimal amount) {

        if (amount.compareTo(BigDecimal.ZERO) <= 0) {
            return buildFailureResponse(fromSortCode, fromAccountNumber,
                    toSortCode, toAccountNumber, amount,
                    "4", "Transfer amount must be greater than zero");
        }

        if (fromSortCode.equals(toSortCode) && fromAccountNumber.equals(toAccountNumber)) {
            return buildFailureResponse(fromSortCode, fromAccountNumber,
                    toSortCode, toAccountNumber, amount,
                    "SAME", "Cannot transfer to the same account");
        }

        for (int attempt = 1; attempt <= MAX_DEADLOCK_RETRIES; attempt++) {
            try {
                return executeTransfer(fromSortCode, fromAccountNumber,
                        toSortCode, toAccountNumber, amount);
            } catch (DeadlockLoserDataAccessException e) {
                logger.warn("Deadlock detected on attempt {}/{} for transfer {} -> {}",
                        attempt, MAX_DEADLOCK_RETRIES, fromAccountNumber, toAccountNumber);

                if (attempt >= MAX_DEADLOCK_RETRIES) {
                    logger.error("Transfer failed after {} deadlock retries", MAX_DEADLOCK_RETRIES, e);
                    return buildFailureResponse(fromSortCode, fromAccountNumber,
                            toSortCode, toAccountNumber, amount,
                            "DEAD", "Transfer failed due to persistent deadlock after "
                                    + MAX_DEADLOCK_RETRIES + " retries");
                }

                try {
                    Thread.sleep(DEADLOCK_RETRY_DELAY_MS);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    return buildFailureResponse(fromSortCode, fromAccountNumber,
                            toSortCode, toAccountNumber, amount,
                            "INTR", "Transfer interrupted during deadlock retry");
                }
            }
        }

        return buildFailureResponse(fromSortCode, fromAccountNumber,
                toSortCode, toAccountNumber, amount,
                "DEAD", "Transfer failed due to persistent deadlock");
    }

    private TransferResponseDto executeTransfer(String fromSortCode, String fromAccountNumber,
                                                String toSortCode, String toAccountNumber,
                                                BigDecimal amount) {

        DefaultTransactionDefinition txDef = new DefaultTransactionDefinition();
        txDef.setIsolationLevel(TransactionDefinition.ISOLATION_SERIALIZABLE);
        txDef.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW);
        TransactionStatus txStatus = transactionManager.getTransaction(txDef);

        try {
            String firstSortCode;
            String firstAccountNumber;
            String secondSortCode;
            String secondAccountNumber;
            boolean fromFirst;

            int accountComparison = fromAccountNumber.compareTo(toAccountNumber);
            if (accountComparison < 0) {
                firstSortCode = fromSortCode;
                firstAccountNumber = fromAccountNumber;
                secondSortCode = toSortCode;
                secondAccountNumber = toAccountNumber;
                fromFirst = true;
            } else {
                firstSortCode = toSortCode;
                firstAccountNumber = toAccountNumber;
                secondSortCode = fromSortCode;
                secondAccountNumber = fromAccountNumber;
                fromFirst = false;
            }

            Optional<Account> firstAccountOpt = accountRepository.findById(firstSortCode, firstAccountNumber);
            if (firstAccountOpt.isEmpty()) {
                transactionManager.rollback(txStatus);
                String failCode = fromFirst ? "1" : "2";
                String which = fromFirst ? "Source" : "Target";
                return buildFailureResponse(fromSortCode, fromAccountNumber,
                        toSortCode, toAccountNumber, amount,
                        failCode, which + " account not found: " + firstSortCode + "/" + firstAccountNumber);
            }
            Account firstAccount = firstAccountOpt.get();

            Optional<Account> secondAccountOpt = accountRepository.findById(secondSortCode, secondAccountNumber);
            if (secondAccountOpt.isEmpty()) {
                transactionManager.rollback(txStatus);
                String failCode = fromFirst ? "2" : "1";
                String which = fromFirst ? "Target" : "Source";
                return buildFailureResponse(fromSortCode, fromAccountNumber,
                        toSortCode, toAccountNumber, amount,
                        failCode, which + " account not found: " + secondSortCode + "/" + secondAccountNumber);
            }
            Account secondAccount = secondAccountOpt.get();

            Account fromAccount;
            Account toAccount;
            if (fromFirst) {
                fromAccount = firstAccount;
                toAccount = secondAccount;
            } else {
                fromAccount = secondAccount;
                toAccount = firstAccount;
            }

            fromAccount.setAvailableBalance(fromAccount.getAvailableBalance().subtract(amount));
            fromAccount.setActualBalance(fromAccount.getActualBalance().subtract(amount));

            toAccount.setAvailableBalance(toAccount.getAvailableBalance().add(amount));
            toAccount.setActualBalance(toAccount.getActualBalance().add(amount));

            if (fromFirst) {
                accountRepository.save(firstAccount);
                accountRepository.save(secondAccount);
            } else {
                accountRepository.save(firstAccount);
                accountRepository.save(secondAccount);
            }

            writeTransferAuditLog(fromSortCode, fromAccountNumber,
                    toSortCode, toAccountNumber, amount);

            transactionManager.commit(txStatus);

            return TransferResponseDto.builder()
                    .success(true)
                    .fromSortCode(fromSortCode)
                    .fromAccountNumber(fromAccountNumber)
                    .fromAvailableBalance(fromAccount.getAvailableBalance())
                    .fromActualBalance(fromAccount.getActualBalance())
                    .toSortCode(toSortCode)
                    .toAccountNumber(toAccountNumber)
                    .toAvailableBalance(toAccount.getAvailableBalance())
                    .toActualBalance(toAccount.getActualBalance())
                    .amount(amount)
                    .build();

        } catch (DeadlockLoserDataAccessException e) {
            transactionManager.rollback(txStatus);
            throw e;
        } catch (Exception e) {
            transactionManager.rollback(txStatus);
            logger.error("Transfer failed unexpectedly", e);
            return buildFailureResponse(fromSortCode, fromAccountNumber,
                    toSortCode, toAccountNumber, amount,
                    "3", "Unexpected error during transfer: " + e.getMessage());
        }
    }

    private void writeTransferAuditLog(String fromSortCode, String fromAccountNumber,
                                       String toSortCode, String toAccountNumber,
                                       BigDecimal amount) {
        LocalDate now = LocalDate.now();
        LocalTime time = LocalTime.now();

        String description = "TFR " + toSortCode + " " + toAccountNumber;
        if (description.length() > 40) {
            description = description.substring(0, 40);
        }

        long referenceNumber = ThreadLocalRandom.current().nextLong(1L, 999999999999L);

        Transaction auditTransaction = Transaction.builder()
                .eyeCatcher(Transaction.VALID_EYECATCHER)
                .logicallyDeleted(false)
                .sortCode(fromSortCode)
                .accountNumber(fromAccountNumber)
                .transactionDate(now)
                .transactionTime(time)
                .referenceNumber(referenceNumber)
                .transactionType(Transaction.TYPE_TRANSFER)
                .description(description)
                .amount(amount)
                .targetSortCode(toSortCode)
                .targetAccountNumber(toAccountNumber)
                .build();

        transactionRepository.save(auditTransaction);
    }

    private TransferResponseDto buildFailureResponse(String fromSortCode, String fromAccountNumber,
                                                     String toSortCode, String toAccountNumber,
                                                     BigDecimal amount,
                                                     String failureCode, String failureMessage) {
        return TransferResponseDto.builder()
                .success(false)
                .failureCode(failureCode)
                .failureMessage(failureMessage)
                .fromSortCode(fromSortCode)
                .fromAccountNumber(fromAccountNumber)
                .toSortCode(toSortCode)
                .toAccountNumber(toAccountNumber)
                .amount(amount)
                .build();
    }
}
