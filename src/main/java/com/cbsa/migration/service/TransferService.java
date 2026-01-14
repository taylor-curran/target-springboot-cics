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

/**
 * Service for Transfer Funds operations.
 * Migrated from COBOL XFRFUN program (BNK1TFN.cbl interface).
 * 
 * This service implements the core business logic for transferring funds
 * between accounts within the same bank. The accounts can belong to the
 * same or different customers.
 * 
 * Error Codes:
 * - '1': FROM account not found
 * - '2': TO account not found
 * - '3': Unexpected error during transfer
 * - '4': Amount must be greater than zero (or invalid amount)
 * - '5': Insufficient funds in FROM account
 * - '6': FROM and TO accounts must be different
 * - '7': Invalid account number (00000000)
 */
@Service
public class TransferService {

    private static final Logger logger = LoggerFactory.getLogger(TransferService.class);
    
    private static final String INVALID_ACCOUNT_NUMBER = "00000000";
    
    private final AccountRepository accountRepository;
    private final TransactionRepository transactionRepository;
    
    // Simple reference number generator (in production, this would be more sophisticated)
    private final AtomicLong referenceNumberGenerator = new AtomicLong(System.currentTimeMillis());

    public TransferService(AccountRepository accountRepository, 
                          TransactionRepository transactionRepository) {
        this.accountRepository = accountRepository;
        this.transactionRepository = transactionRepository;
    }

    /**
     * Transfer funds from one account to another.
     * 
     * This method performs the following validations:
     * 1. Amount must be greater than zero
     * 2. FROM and TO account numbers must not be '00000000'
     * 3. FROM and TO accounts must be different
     * 4. FROM account must exist
     * 5. TO account must exist
     * 6. FROM account must have sufficient available balance
     * 
     * The transfer is atomic - both account updates succeed or fail together.
     * 
     * @param request the transfer request containing account details and amount
     * @return the transfer response with success/failure status and new balances
     */
    @Transactional
    public TransferResponseDto transferFunds(TransferRequestDto request) {
        logger.info("Processing transfer request: from {}/{} to {}/{}, amount: {}",
                request.getFromSortCode(), request.getFromAccountNumber(),
                request.getToSortCode(), request.getToAccountNumber(),
                request.getAmount());

        try {
            // Validation 1: Amount must be greater than zero
            if (request.getAmount() == null || request.getAmount().compareTo(BigDecimal.ZERO) <= 0) {
                logger.warn("Transfer failed: Amount must be greater than zero");
                return TransferResponseDto.failure("4", "Amount must be greater than zero");
            }

            // Validation 2: FROM account number must not be '00000000'
            if (INVALID_ACCOUNT_NUMBER.equals(request.getFromAccountNumber())) {
                logger.warn("Transfer failed: FROM account number cannot be 00000000");
                return TransferResponseDto.failure("7", "FROM account number cannot be 00000000");
            }

            // Validation 3: TO account number must not be '00000000'
            if (INVALID_ACCOUNT_NUMBER.equals(request.getToAccountNumber())) {
                logger.warn("Transfer failed: TO account number cannot be 00000000");
                return TransferResponseDto.failure("7", "TO account number cannot be 00000000");
            }

            // Validation 4: FROM and TO accounts must be different
            if (request.getFromSortCode().equals(request.getToSortCode()) &&
                request.getFromAccountNumber().equals(request.getToAccountNumber())) {
                logger.warn("Transfer failed: FROM and TO accounts must be different");
                return TransferResponseDto.failure("6", "FROM and TO accounts must be different");
            }

            // Validation 5: FROM account must exist
            Optional<Account> fromAccountOpt = accountRepository.findById(
                    request.getFromSortCode(), request.getFromAccountNumber());
            if (fromAccountOpt.isEmpty()) {
                logger.warn("Transfer failed: FROM account not found: {}/{}",
                        request.getFromSortCode(), request.getFromAccountNumber());
                return TransferResponseDto.failure("1", "FROM account not found");
            }

            // Validation 6: TO account must exist
            Optional<Account> toAccountOpt = accountRepository.findById(
                    request.getToSortCode(), request.getToAccountNumber());
            if (toAccountOpt.isEmpty()) {
                logger.warn("Transfer failed: TO account not found: {}/{}",
                        request.getToSortCode(), request.getToAccountNumber());
                return TransferResponseDto.failure("2", "TO account not found");
            }

            Account fromAccount = fromAccountOpt.get();
            Account toAccount = toAccountOpt.get();

            // Validation 7: FROM account must have sufficient available balance
            // Available balance includes overdraft limit
            BigDecimal availableWithOverdraft = fromAccount.getAvailableBalance()
                    .add(BigDecimal.valueOf(fromAccount.getOverdraftLimit()));
            if (availableWithOverdraft.compareTo(request.getAmount()) < 0) {
                logger.warn("Transfer failed: Insufficient funds in FROM account. " +
                        "Available: {}, Overdraft: {}, Required: {}",
                        fromAccount.getAvailableBalance(), fromAccount.getOverdraftLimit(),
                        request.getAmount());
                return TransferResponseDto.failure("5", "Insufficient funds in FROM account");
            }

            // Perform the transfer - debit FROM account, credit TO account
            BigDecimal newFromBalance = fromAccount.getAvailableBalance().subtract(request.getAmount());
            BigDecimal newFromActualBalance = fromAccount.getActualBalance().subtract(request.getAmount());
            fromAccount.setAvailableBalance(newFromBalance);
            fromAccount.setActualBalance(newFromActualBalance);

            BigDecimal newToBalance = toAccount.getAvailableBalance().add(request.getAmount());
            BigDecimal newToActualBalance = toAccount.getActualBalance().add(request.getAmount());
            toAccount.setAvailableBalance(newToBalance);
            toAccount.setActualBalance(newToActualBalance);

            // Save both accounts atomically
            accountRepository.save(fromAccount);
            accountRepository.save(toAccount);

            // Generate transaction reference
            Long transactionReference = referenceNumberGenerator.incrementAndGet();

            // Record the transfer transaction for audit trail (on FROM account)
            recordTransferTransaction(fromAccount, toAccount, request.getAmount(), 
                    transactionReference, true);
            
            // Record the transfer transaction for audit trail (on TO account)
            recordTransferTransaction(toAccount, fromAccount, request.getAmount(), 
                    transactionReference, false);

            logger.info("Transfer successful: {} from {}/{} to {}/{}. " +
                    "New balances - FROM: {}, TO: {}. Reference: {}",
                    request.getAmount(),
                    request.getFromSortCode(), request.getFromAccountNumber(),
                    request.getToSortCode(), request.getToAccountNumber(),
                    newFromBalance, newToBalance, transactionReference);

            return TransferResponseDto.success(
                    request.getFromSortCode(), request.getFromAccountNumber(), newFromBalance,
                    request.getToSortCode(), request.getToAccountNumber(), newToBalance,
                    request.getAmount(), transactionReference);

        } catch (Exception e) {
            logger.error("Unexpected error during transfer: {}", e.getMessage(), e);
            return TransferResponseDto.failure("3", "Unexpected error during transfer: " + e.getMessage());
        }
    }

    /**
     * Record a transfer transaction in the audit trail.
     * 
     * @param account the account to record the transaction for
     * @param otherAccount the other account involved in the transfer
     * @param amount the transfer amount
     * @param referenceNumber the transaction reference number
     * @param isDebit true if this is the debit side (FROM account), false for credit side (TO account)
     */
    private void recordTransferTransaction(Account account, Account otherAccount, 
                                          BigDecimal amount, Long referenceNumber, 
                                          boolean isDebit) {
        Transaction transaction = Transaction.builder()
                .eyeCatcher(Transaction.VALID_EYECATCHER)
                .logicallyDeleted(false)
                .sortCode(account.getSortCode())
                .accountNumber(account.getAccountNumber())
                .transactionDate(LocalDate.now())
                .transactionTime(LocalTime.now())
                .referenceNumber(referenceNumber)
                .transactionType(Transaction.TYPE_TRANSFER)
                .description(buildTransferDescription(otherAccount, isDebit))
                .targetSortCode(otherAccount.getSortCode())
                .targetAccountNumber(otherAccount.getAccountNumber())
                .amount(isDebit ? amount.negate() : amount)
                .build();

        transactionRepository.save(transaction);
    }

    /**
     * Build a description for the transfer transaction.
     * 
     * @param otherAccount the other account involved in the transfer
     * @param isDebit true if this is the debit side (FROM account)
     * @return the transaction description
     */
    private String buildTransferDescription(Account otherAccount, boolean isDebit) {
        if (isDebit) {
            return String.format("Transfer to %s/%s", 
                    otherAccount.getSortCode(), otherAccount.getAccountNumber());
        } else {
            return String.format("Transfer from %s/%s", 
                    otherAccount.getSortCode(), otherAccount.getAccountNumber());
        }
    }
}
