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
import java.util.concurrent.atomic.AtomicLong;

/**
 * Credit/Debit Service - implements the OCRA transaction business logic.
 * Migrated from COBOL programs BNK1CRA.cbl (presentation) and DBCRFUN.cbl (business logic).
 *
 * This service handles:
 * - Account lookup by sort code + account number
 * - MORTGAGE/LOAN restriction for payment channel
 * - Insufficient funds check for payment channel debits
 * - Balance update (both available and actual)
 * - PROCTRAN audit record creation with correct type codes
 * - Transactional integrity (rollback on failure)
 */
@Service
public class CreditDebitService {

    private static final Logger logger = LoggerFactory.getLogger(CreditDebitService.class);

    private static final String FAIL_CODE_SUCCESS = "0";
    private static final String FAIL_CODE_ACCOUNT_NOT_FOUND = "1";
    private static final String FAIL_CODE_DB_ERROR = "2";
    private static final String FAIL_CODE_INSUFFICIENT_FUNDS = "3";
    private static final String FAIL_CODE_MORTGAGE_LOAN_PAYMENT = "4";

    private static final String CHANNEL_PAYMENT = "PAYMENT";

    private static final String ACCOUNT_TYPE_MORTGAGE = "MORTGAGE";
    private static final String ACCOUNT_TYPE_LOAN = "LOAN";

    private final AccountRepository accountRepository;
    private final TransactionRepository transactionRepository;
    private final SortCodeService sortCodeService;
    private final AtomicLong referenceCounter;

    public CreditDebitService(AccountRepository accountRepository,
                              TransactionRepository transactionRepository,
                              SortCodeService sortCodeService) {
        this.accountRepository = accountRepository;
        this.transactionRepository = transactionRepository;
        this.sortCodeService = sortCodeService;
        this.referenceCounter = new AtomicLong(System.currentTimeMillis() % 1000000000000L);
    }

    /**
     * Process a credit or debit operation on an account.
     * Equivalent to COBOL DBCRFUN UPDATE-ACCOUNT-DB2 and WRITE-TO-PROCTRAN sections.
     *
     * @param request the debit/credit request
     * @return response with updated balances or failure details
     */
    @Transactional
    public DebitCreditResponseDto debitCreditAccount(DebitCreditRequestDto request) {
        String sortCode = sortCodeService.getSortCode();
        String channelType = request.getChannelType() != null ? request.getChannelType() : "TELLER";
        boolean isPaymentChannel = CHANNEL_PAYMENT.equals(channelType);

        BigDecimal signedAmount = "-".equals(request.getSign())
                ? request.getAmount().negate()
                : request.getAmount();

        logger.info("Processing {} {} for account {}-{}, amount={}",
                channelType, "-".equals(request.getSign()) ? "debit" : "credit",
                sortCode, request.getAccountNumber(), signedAmount);

        // Look up the account (COBOL: SELECT FROM ACCOUNT)
        Optional<Account> accountOpt = accountRepository.findById(sortCode, request.getAccountNumber());

        if (accountOpt.isEmpty()) {
            logger.warn("Account not found: {}-{}", sortCode, request.getAccountNumber());
            return DebitCreditResponseDto.builder()
                    .success(false)
                    .failCode(FAIL_CODE_ACCOUNT_NOT_FOUND)
                    .message("Account not found for sort code " + sortCode)
                    .sortCode(sortCode)
                    .build();
        }

        Account account = accountOpt.get();
        String accountType = account.getAccountType() != null ? account.getAccountType().trim() : "";

        // COBOL: Debit checks (COMM-AMT < 0)
        if (signedAmount.compareTo(BigDecimal.ZERO) < 0) {
            // MORTGAGE/LOAN restriction for payment channel
            if (isPaymentChannel && isMortgageOrLoan(accountType)) {
                logger.warn("Payment debit denied for MORTGAGE/LOAN account {}-{}", sortCode, request.getAccountNumber());
                return DebitCreditResponseDto.builder()
                        .success(false)
                        .failCode(FAIL_CODE_MORTGAGE_LOAN_PAYMENT)
                        .message("Debit not permitted on " + accountType + " account via payment channel")
                        .sortCode(sortCode)
                        .build();
            }

            // Insufficient funds check for payment channel only
            BigDecimal newAvailableBalance = account.getAvailableBalance().add(signedAmount);
            if (isPaymentChannel && newAvailableBalance.compareTo(BigDecimal.ZERO) < 0) {
                logger.warn("Insufficient funds for payment debit on account {}-{}: available={}, requested={}",
                        sortCode, request.getAccountNumber(), account.getAvailableBalance(), signedAmount);
                return DebitCreditResponseDto.builder()
                        .success(false)
                        .failCode(FAIL_CODE_INSUFFICIENT_FUNDS)
                        .message("Insufficient funds available to process the request")
                        .sortCode(sortCode)
                        .build();
            }
        }

        // COBOL: Credit checks (COMM-AMT >= 0)
        if (signedAmount.compareTo(BigDecimal.ZERO) >= 0 && isPaymentChannel && isMortgageOrLoan(accountType)) {
            logger.warn("Payment credit denied for MORTGAGE/LOAN account {}-{}", sortCode, request.getAccountNumber());
            return DebitCreditResponseDto.builder()
                    .success(false)
                    .failCode(FAIL_CODE_MORTGAGE_LOAN_PAYMENT)
                    .message("Credit not permitted on " + accountType + " account via payment channel")
                    .sortCode(sortCode)
                    .build();
        }

        // Update balances (COBOL: COMPUTE HV-ACCOUNT-AVAIL-BAL + COMM-AMT)
        BigDecimal updatedAvailableBalance = account.getAvailableBalance().add(signedAmount);
        BigDecimal updatedActualBalance = account.getActualBalance().add(signedAmount);

        account.setAvailableBalance(updatedAvailableBalance);
        account.setActualBalance(updatedActualBalance);

        try {
            // Persist the account update
            accountRepository.save(account);
        } catch (Exception e) {
            logger.error("Failed to update account {}-{}: {}", sortCode, request.getAccountNumber(), e.getMessage(), e);
            return DebitCreditResponseDto.builder()
                    .success(false)
                    .failCode(FAIL_CODE_DB_ERROR)
                    .message("Amount could not be applied due to an unexpected error")
                    .sortCode(sortCode)
                    .build();
        }

        // Write PROCTRAN audit record
        try {
            Transaction proctran = buildProctranRecord(
                    sortCode, request.getAccountNumber(), signedAmount, isPaymentChannel, channelType);
            transactionRepository.save(proctran);
        } catch (Exception e) {
            logger.error("Failed to write PROCTRAN for account {}-{}: {}. Rolling back.",
                    sortCode, request.getAccountNumber(), e.getMessage(), e);
            throw new RuntimeException("Failed to write audit record, transaction rolled back", e);
        }

        logger.info("Successfully applied {} to account {}-{}: availBal={}, actBal={}",
                signedAmount, sortCode, request.getAccountNumber(),
                updatedAvailableBalance, updatedActualBalance);

        return DebitCreditResponseDto.builder()
                .success(true)
                .failCode(FAIL_CODE_SUCCESS)
                .message("Amount successfully applied to the account")
                .sortCode(sortCode)
                .availableBalance(updatedAvailableBalance)
                .actualBalance(updatedActualBalance)
                .build();
    }

    /**
     * Build a PROCTRAN (processed transaction) audit record.
     * Equivalent to COBOL WRITE-TO-PROCTRAN-DB2 section.
     */
    private Transaction buildProctranRecord(String sortCode, String accountNumber,
                                            BigDecimal signedAmount, boolean isPaymentChannel,
                                            String channelType) {
        String transactionType;
        String description;

        if (signedAmount.compareTo(BigDecimal.ZERO) < 0) {
            // Debit
            if (isPaymentChannel) {
                transactionType = Transaction.TYPE_PAYMENT_DEBIT;   // "PDR"
                description = channelType;
            } else {
                transactionType = Transaction.TYPE_DEBIT;           // "DEB"
                description = "COUNTER WTHDRW";
            }
        } else {
            // Credit
            if (isPaymentChannel) {
                transactionType = Transaction.TYPE_PAYMENT_CREDIT;  // "PCR"
                description = channelType;
            } else {
                transactionType = Transaction.TYPE_CREDIT;          // "CRE"
                description = "COUNTER RECVED";
            }
        }

        return Transaction.builder()
                .eyeCatcher(Transaction.VALID_EYECATCHER)
                .logicallyDeleted(false)
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .transactionDate(LocalDate.now())
                .transactionTime(LocalTime.now().withNano(0))
                .referenceNumber(referenceCounter.incrementAndGet())
                .transactionType(transactionType)
                .description(description)
                .amount(signedAmount)
                .build();
    }

    private boolean isMortgageOrLoan(String accountType) {
        return ACCOUNT_TYPE_MORTGAGE.equalsIgnoreCase(accountType)
                || ACCOUNT_TYPE_LOAN.equalsIgnoreCase(accountType);
    }
}
