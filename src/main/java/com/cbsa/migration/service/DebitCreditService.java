package com.cbsa.migration.service;

import com.cbsa.migration.dto.DebitCreditRequest;
import com.cbsa.migration.dto.DebitCreditResponse;
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
 * Service implementing the business logic from DBCRFUN.cbl.
 *
 * DBCRFUN is the shared business-logic program that:
 * <ul>
 *   <li>Takes an account number and signed amount</li>
 *   <li>Retrieves the associated Account record from DB2</li>
 *   <li>Applies the credit or debit amount to actual and available balances</li>
 *   <li>Updates the ACCOUNT table</li>
 *   <li>Writes an audit record to the PROCTRAN (bank_transaction) table</li>
 *   <li>Distinguishes teller traffic (DEB/CRE) from payment/API traffic (PDR/PCR)
 *       using COMM-FACILTYPE (496 = payment origin)</li>
 *   <li>Handles rollback on failure and sets SUCCESS flag to 'N' with a fail code</li>
 * </ul>
 *
 * Fail codes (from COBOL):
 * <pre>
 *   0 - Success
 *   1 - Account not found (SQLCODE +100)
 *   2 - Unexpected database error
 *   3 - Insufficient funds for debit (payment origin only)
 *   4 - Cannot debit/credit MORTGAGE or LOAN account (payment origin only)
 * </pre>
 */
@Service
public class DebitCreditService {

    private static final Logger log = LoggerFactory.getLogger(DebitCreditService.class);

    /** Facility type value indicating payment/API origin (COMM-FACILTYPE = 496 in COBOL). */
    private static final int FACILITY_TYPE_PAYMENT = 496;

    /** Sort code from SORTCODE.cpy: 77 SORTCODE PIC 9(6) VALUE 987654. */
    private static final String SORT_CODE = "987654";

    private final AccountRepository accountRepository;
    private final TransactionRepository transactionRepository;
    private final SortCodeService sortCodeService;

    /** Simple in-memory counter to generate unique reference numbers (replaces EIBTASKN). */
    private final AtomicLong referenceCounter = new AtomicLong(System.currentTimeMillis() % 1_000_000_000_000L);

    public DebitCreditService(AccountRepository accountRepository,
                              TransactionRepository transactionRepository,
                              SortCodeService sortCodeService) {
        this.accountRepository = accountRepository;
        this.transactionRepository = transactionRepository;
        this.sortCodeService = sortCodeService;
    }

    /**
     * Credit an account (positive amount applied).
     * Mirrors DBCRFUN logic when COMM-AMT >= 0.
     *
     * @param accountNumber 8-digit account number
     * @param request       credit request with amount and origin info
     * @return response with updated balances or error info
     */
    @Transactional
    public DebitCreditResponse creditAccount(String accountNumber, DebitCreditRequest request) {
        return applyDebitCredit(accountNumber, request.getAmount(), request);
    }

    /**
     * Debit an account (negative amount applied).
     * Mirrors DBCRFUN logic when COMM-AMT < 0.
     *
     * @param accountNumber 8-digit account number
     * @param request       debit request with amount and origin info
     * @return response with updated balances or error info
     */
    @Transactional
    public DebitCreditResponse debitAccount(String accountNumber, DebitCreditRequest request) {
        return applyDebitCredit(accountNumber, request.getAmount().negate(), request);
    }

    /**
     * Core logic from UPDATE-ACCOUNT-DB2 section in DBCRFUN.cbl.
     *
     * @param accountNumber the 8-digit account number
     * @param signedAmount  positive for credit, negative for debit
     * @param request       the original request with facility type info
     * @return response
     */
    private DebitCreditResponse applyDebitCredit(String accountNumber,
                                                  BigDecimal signedAmount,
                                                  DebitCreditRequest request) {
        String sortCode = sortCodeService.getSortCode();
        int facilityType = request.getFacilityType() != null ? request.getFacilityType() : 0;

        // --- Retrieve the account (DBCRFUN lines 245-273) ---
        Optional<Account> optAccount = accountRepository.findById(sortCode, accountNumber);

        if (optAccount.isEmpty()) {
            // SQLCODE = +100 -> fail code '1'
            log.warn("Account not found: sortCode={}, accountNumber={}", sortCode, accountNumber);
            return DebitCreditResponse.builder()
                    .success(false)
                    .failCode("1")
                    .message("Sorry but the ACCOUNT no was not found for SORTCODE "
                            + sortCode + ". Amount not applied.")
                    .accountNumber(accountNumber)
                    .sortCode(sortCode)
                    .build();
        }

        Account account = optAccount.get();

        // --- Debit-specific validations (DBCRFUN lines 307-376) ---
        if (signedAmount.compareTo(BigDecimal.ZERO) < 0) {
            // Check MORTGAGE/LOAN restriction for payment origin (DBCRFUN lines 330-338)
            if (facilityType == FACILITY_TYPE_PAYMENT) {
                String accType = account.getAccountType() != null
                        ? account.getAccountType().trim() : "";
                if ("MORTGAGE".equals(accType) || "LOAN".equals(accType)) {
                    log.info("Payment debit rejected for {} account: {}", accType, accountNumber);
                    return DebitCreditResponse.builder()
                            .success(false)
                            .failCode("4")
                            .message("Cannot process payment debit on a " + accType + " account.")
                            .accountNumber(accountNumber)
                            .sortCode(sortCode)
                            .actualBalance(account.getActualBalance())
                            .availableBalance(account.getAvailableBalance())
                            .build();
                }
            }

            // Insufficient funds check for payment origin (DBCRFUN lines 340-350)
            if (facilityType == FACILITY_TYPE_PAYMENT) {
                BigDecimal difference = account.getAvailableBalance().add(signedAmount);
                if (difference.compareTo(BigDecimal.ZERO) < 0) {
                    log.info("Insufficient funds for payment debit on account {}", accountNumber);
                    return DebitCreditResponse.builder()
                            .success(false)
                            .failCode("3")
                            .message("Sorry insufficient funds available to process the request.")
                            .accountNumber(accountNumber)
                            .sortCode(sortCode)
                            .actualBalance(account.getActualBalance())
                            .availableBalance(account.getAvailableBalance())
                            .build();
                }
            }
        }

        // --- Credit-specific MORTGAGE/LOAN check for payment origin (DBCRFUN lines 368-376) ---
        if (signedAmount.compareTo(BigDecimal.ZERO) >= 0 && facilityType == FACILITY_TYPE_PAYMENT) {
            String accType = account.getAccountType() != null
                    ? account.getAccountType().trim() : "";
            if ("MORTGAGE".equals(accType) || "LOAN".equals(accType)) {
                log.info("Payment credit rejected for {} account: {}", accType, accountNumber);
                return DebitCreditResponse.builder()
                        .success(false)
                        .failCode("4")
                        .message("Cannot process payment credit on a " + accType + " account.")
                        .accountNumber(accountNumber)
                        .sortCode(sortCode)
                        .actualBalance(account.getActualBalance())
                        .availableBalance(account.getAvailableBalance())
                        .build();
            }
        }

        // --- Apply amount to balances (DBCRFUN lines 384-387) ---
        BigDecimal newAvailableBalance = account.getAvailableBalance().add(signedAmount);
        BigDecimal newActualBalance = account.getActualBalance().add(signedAmount);

        account.setAvailableBalance(newAvailableBalance);
        account.setActualBalance(newActualBalance);

        // --- Update the account record (DBCRFUN lines 392-408) ---
        try {
            accountRepository.save(account);
        } catch (Exception e) {
            log.error("Failed to update account {}: {}", accountNumber, e.getMessage(), e);
            return DebitCreditResponse.builder()
                    .success(false)
                    .failCode("2")
                    .message("Sorry but the AMOUNT could not be applied due to an unexpected error.")
                    .accountNumber(accountNumber)
                    .sortCode(sortCode)
                    .actualBalance(newActualBalance)
                    .availableBalance(newAvailableBalance)
                    .build();
        }

        // --- Write PROCTRAN audit record (DBCRFUN lines 447-652) ---
        try {
            writeProcessedTransaction(accountNumber, sortCode, signedAmount,
                    facilityType, request.getOriginDescription());
        } catch (Exception e) {
            // DBCRFUN issues SYNCPOINT ROLLBACK here; @Transactional handles that
            log.error("Failed to write PROCTRAN for account {}: {}", accountNumber, e.getMessage(), e);
            throw new RuntimeException(
                    "PROCTRAN write failed for account " + accountNumber + ": " + e.getMessage(), e);
        }

        // --- Success (DBCRFUN line 649-650) ---
        log.info("Successfully applied {} to account {}", signedAmount, accountNumber);
        return DebitCreditResponse.builder()
                .success(true)
                .failCode("0")
                .message("Amount successfully applied to the account.")
                .accountNumber(accountNumber)
                .sortCode(sortCode)
                .actualBalance(newActualBalance)
                .availableBalance(newAvailableBalance)
                .build();
    }

    /**
     * Writes a processed transaction (audit) record to the bank_transaction table.
     * Mirrors WRITE-TO-PROCTRAN-DB2 section in DBCRFUN.cbl (lines 460-652).
     *
     * Transaction type logic (DBCRFUN lines 491-517):
     * <pre>
     *   if amount < 0:
     *     type = 'DEB', desc = 'COUNTER WTHDRW'      (teller debit)
     *     if facilityType == 496:
     *       type = 'PDR', desc = origin(1:14)          (payment debit)
     *   else:
     *     type = 'CRE', desc = 'COUNTER RECVED'      (teller credit)
     *     if facilityType == 496:
     *       type = 'PCR', desc = origin(1:14)          (payment credit)
     * </pre>
     */
    private void writeProcessedTransaction(String accountNumber,
                                            String sortCode,
                                            BigDecimal signedAmount,
                                            int facilityType,
                                            String originDescription) {
        String transactionType;
        String description;

        if (signedAmount.compareTo(BigDecimal.ZERO) < 0) {
            // Debit
            transactionType = Transaction.TYPE_DEBIT;          // "DEB"
            description = "COUNTER WTHDRW";

            if (facilityType == FACILITY_TYPE_PAYMENT) {
                transactionType = Transaction.TYPE_PAYMENT_DEBIT;  // "PDR"
                description = truncateOrigin(originDescription);
            }
        } else {
            // Credit
            transactionType = Transaction.TYPE_CREDIT;         // "CRE"
            description = "COUNTER RECVED";

            if (facilityType == FACILITY_TYPE_PAYMENT) {
                transactionType = Transaction.TYPE_PAYMENT_CREDIT; // "PCR"
                description = truncateOrigin(originDescription);
            }
        }

        long referenceNumber = referenceCounter.incrementAndGet();

        Transaction transaction = Transaction.builder()
                .eyeCatcher(Transaction.VALID_EYECATCHER)    // "PRTR"
                .logicallyDeleted(false)
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .transactionDate(LocalDate.now())
                .transactionTime(LocalTime.now().withNano(0))
                .referenceNumber(referenceNumber)
                .transactionType(transactionType)
                .description(description)
                .amount(signedAmount)
                .build();

        transactionRepository.save(transaction);
        log.debug("PROCTRAN record written: type={}, account={}, amount={}",
                transactionType, accountNumber, signedAmount);
    }

    /**
     * Truncates origin description to 14 characters, matching COBOL's
     * MOVE COMM-ORIGIN(1:14) TO HV-PROCTRAN-DESC.
     */
    private String truncateOrigin(String originDescription) {
        if (originDescription == null || originDescription.isEmpty()) {
            return "";
        }
        return originDescription.length() > 14
                ? originDescription.substring(0, 14)
                : originDescription;
    }
}
