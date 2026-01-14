package com.cbsa.migration.service;

import com.cbsa.migration.dto.FundsTransferRequest;
import com.cbsa.migration.dto.FundsTransferResult;
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
 * Service for funds transfer operations between bank accounts.
 * 
 * This class is a direct migration of the COBOL XFRFUN program (src/base/cobol_src/XFRFUN.cbl)
 * from the original CICS Banking Sample Application. The COBOL program was approximately
 * 1925 lines and handled funds transfers with CICS transaction management.
 * 
 * Key COBOL-to-Java mappings:
 * - COBOL COMMAREA fields -> FundsTransferRequest/FundsTransferResult DTOs
 * - CICS SYNCPOINT ROLLBACK -> Spring @Transactional annotation
 * - DB2 ACCOUNT table operations -> AccountRepository
 * - DB2 PROCTRAN table operations -> TransactionRepository
 * 
 * The original COBOL program used CICS SYNCONRETURN for commit coordination.
 * In Java, we achieve the same atomicity guarantees using Spring's declarative
 * transaction management with the @Transactional annotation.
 * 
 * Business Rules (preserved from COBOL):
 * 1. Transfer amount must be greater than zero (COBOL: IF COMM-AMT <= ZERO)
 * 2. Cannot transfer to the same account (COBOL: IF COMM-FACCNO = COMM-TACCNO)
 * 3. Both source and target accounts must exist
 * 4. Source account must have sufficient funds (overdraft prevention)
 * 5. Both available and actual balances are updated atomically
 * 6. Successful transfers are recorded in PROCTRAN for audit trail
 * 
 * @see com.cbsa.migration.dto.FundsTransferRequest
 * @see com.cbsa.migration.dto.FundsTransferResult
 */
@Service
public class FundsTransferService {

    private static final Logger logger = LoggerFactory.getLogger(FundsTransferService.class);

    /**
     * Repository for account data access operations.
     * Replaces COBOL EXEC SQL operations on the ACCOUNT table.
     */
    private final AccountRepository accountRepository;
    
    /**
     * Repository for transaction (PROCTRAN) data access operations.
     * Replaces COBOL EXEC SQL INSERT INTO PROCTRAN operations.
     */
    private final TransactionRepository transactionRepository;
    
    /**
     * Counter for generating unique transaction reference numbers.
     * In COBOL, this was derived from EIBTASKN (CICS task number).
     * We use AtomicLong to ensure thread-safe unique reference generation.
     */
    private final AtomicLong referenceCounter = new AtomicLong(System.currentTimeMillis());

    /**
     * Constructor with dependency injection for repositories.
     * 
     * @param accountRepository repository for account operations
     * @param transactionRepository repository for transaction audit records
     */
    public FundsTransferService(AccountRepository accountRepository, 
                                TransactionRepository transactionRepository) {
        this.accountRepository = accountRepository;
        this.transactionRepository = transactionRepository;
    }

    /**
     * Transfer funds between two accounts atomically.
     * 
     * This method implements the core business logic from COBOL XFRFUN program's
     * UPDATE-ACCOUNT-DB2 section. The transfer is atomic - if any part fails,
     * all changes are rolled back automatically by Spring's transaction manager.
     * 
     * Processing steps (matching COBOL flow):
     * 1. Validate transfer amount (must be > 0)
     * 2. Check for same-account transfer (not allowed)
     * 3. Verify FROM account exists
     * 4. Verify TO account exists  
     * 5. Check sufficient funds in FROM account
     * 6. Debit FROM account (both available and actual balance)
     * 7. Credit TO account (both available and actual balance)
     * 8. Record transaction in PROCTRAN for audit trail
     * 
     * COBOL Fail Code Mapping:
     * - '1' = FROM account not found (SQLCODE +100 on FROM account SELECT)
     * - '2' = TO account not found (SQLCODE +100 on TO account SELECT)
     * - '3' = Database error (other SQLCODE errors)
     * - '4' = Invalid amount (<= 0)
     * - '5' = Insufficient funds (Java addition for overdraft prevention)
     * - '6' = Same account transfer (COBOL abended with 'SAME' code)
     * 
     * @param request the transfer request containing:
     *                - fromSortCode: 6-digit sort code of source account
     *                - fromAccountNumber: 8-digit source account number
     *                - toSortCode: 6-digit sort code of target account
     *                - toAccountNumber: 8-digit target account number
     *                - amount: transfer amount (must be positive)
     * @return FundsTransferResult containing:
     *         - success: true if transfer completed, false otherwise
     *         - failCode: error code if failed (see codes above)
     *         - fromAvailableBalance: updated FROM account available balance
     *         - fromActualBalance: updated FROM account actual balance
     *         - toAvailableBalance: updated TO account available balance
     *         - toActualBalance: updated TO account actual balance
     */
    @Transactional
    public FundsTransferResult transfer(FundsTransferRequest request) {
        // Log the incoming transfer request for debugging and audit purposes
        logger.info("Processing funds transfer: from {}/{} to {}/{}, amount {}",
                request.getFromSortCode(), request.getFromAccountNumber(),
                request.getToSortCode(), request.getToAccountNumber(),
                request.getAmount());

        // ============================================================
        // STEP 1: Validate transfer amount
        // COBOL equivalent: IF COMM-AMT <= ZERO ... MOVE '4' TO COMM-FAIL-CODE
        // ============================================================
        if (request.getAmount() == null || request.getAmount().compareTo(BigDecimal.ZERO) <= 0) {
            logger.warn("Transfer rejected: invalid amount {}", request.getAmount());
            return FundsTransferResult.failure(
                    FundsTransferResult.FAIL_CODE_INVALID_AMOUNT,
                    "Transfer amount must be greater than zero");
        }

        // ============================================================
        // STEP 2: Check for same-account transfer (not allowed)
        // COBOL equivalent: IF COMM-FACCNO = COMM-TACCNO AND COMM-FSCODE = COMM-TSCODE
        //                   ... EXEC CICS ABEND ABCODE('SAME') ...
        // In Java, we return a failure instead of abending
        // ============================================================
        if (isSameAccount(request)) {
            logger.warn("Transfer rejected: cannot transfer to the same account");
            return FundsTransferResult.failure(
                    FundsTransferResult.FAIL_CODE_SAME_ACCOUNT,
                    "Cannot transfer to the same account");
        }

        // ============================================================
        // STEP 3: Verify FROM account exists
        // COBOL equivalent: SELECT ... FROM ACCOUNT WHERE ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE
        //                   AND ACCOUNT_NUMBER = :HV-ACCOUNT-ACC-NO
        //                   IF SQLCODE = +100 ... MOVE '1' TO COMM-FAIL-CODE
        // ============================================================
        Optional<Account> fromAccountOpt = accountRepository.findById(
                request.getFromSortCode(), request.getFromAccountNumber());
        if (fromAccountOpt.isEmpty()) {
            logger.warn("Transfer rejected: FROM account not found {}/{}",
                    request.getFromSortCode(), request.getFromAccountNumber());
            return FundsTransferResult.failure(
                    FundsTransferResult.FAIL_CODE_FROM_ACCOUNT_NOT_FOUND,
                    "FROM account not found");
        }

        // ============================================================
        // STEP 4: Verify TO account exists
        // COBOL equivalent: SELECT ... FROM ACCOUNT WHERE ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE
        //                   AND ACCOUNT_NUMBER = :HV-ACCOUNT-ACC-NO
        //                   IF SQLCODE = +100 ... MOVE '2' TO COMM-FAIL-CODE
        // ============================================================
        Optional<Account> toAccountOpt = accountRepository.findById(
                request.getToSortCode(), request.getToAccountNumber());
        if (toAccountOpt.isEmpty()) {
            logger.warn("Transfer rejected: TO account not found {}/{}",
                    request.getToSortCode(), request.getToAccountNumber());
            return FundsTransferResult.failure(
                    FundsTransferResult.FAIL_CODE_TO_ACCOUNT_NOT_FOUND,
                    "TO account not found");
        }

        // Extract the account objects for balance operations
        Account fromAccount = fromAccountOpt.get();
        Account toAccount = toAccountOpt.get();

        // ============================================================
        // STEP 5: Check sufficient funds in FROM account
        // Note: The original COBOL program did NOT check for overdrafts
        // (comment in XFRFUN.cbl line 21: "No checking is made on overdraft limits")
        // However, we add this check as a business rule enhancement
        // ============================================================
        if (fromAccount.getAvailableBalance().compareTo(request.getAmount()) < 0) {
            logger.warn("Transfer rejected: insufficient funds. Available: {}, Requested: {}",
                    fromAccount.getAvailableBalance(), request.getAmount());
            return FundsTransferResult.failure(
                    FundsTransferResult.FAIL_CODE_INSUFFICIENT_FUNDS,
                    "Insufficient funds in source account");
        }

        try {
            // ============================================================
            // STEP 6: Debit FROM account
            // COBOL equivalent (UPDATE-ACCOUNT-DB2-FROM section):
            //   COMPUTE HV-ACCOUNT-AVAIL-BAL = HV-ACCOUNT-AVAIL-BAL - COMM-AMT
            //   COMPUTE HV-ACCOUNT-ACTUAL-BAL = HV-ACCOUNT-ACTUAL-BAL - COMM-AMT
            //   EXEC SQL UPDATE ACCOUNT SET ... WHERE ...
            // ============================================================
            BigDecimal newFromAvailBal = fromAccount.getAvailableBalance().subtract(request.getAmount());
            BigDecimal newFromActBal = fromAccount.getActualBalance().subtract(request.getAmount());
            fromAccount.setAvailableBalance(newFromAvailBal);
            fromAccount.setActualBalance(newFromActBal);
            accountRepository.save(fromAccount);

            // ============================================================
            // STEP 7: Credit TO account
            // COBOL equivalent (UPDATE-ACCOUNT-DB2-TO section):
            //   COMPUTE HV-ACCOUNT-AVAIL-BAL = HV-ACCOUNT-AVAIL-BAL + COMM-AMT
            //   COMPUTE HV-ACCOUNT-ACTUAL-BAL = HV-ACCOUNT-ACTUAL-BAL + COMM-AMT
            //   EXEC SQL UPDATE ACCOUNT SET ... WHERE ...
            // ============================================================
            BigDecimal newToAvailBal = toAccount.getAvailableBalance().add(request.getAmount());
            BigDecimal newToActBal = toAccount.getActualBalance().add(request.getAmount());
            toAccount.setAvailableBalance(newToAvailBal);
            toAccount.setActualBalance(newToActBal);
            accountRepository.save(toAccount);

            // ============================================================
            // STEP 8: Record transaction in PROCTRAN for audit trail
            // COBOL equivalent: PERFORM WRITE-TO-PROCTRAN-DB2
            // ============================================================
            recordTransaction(request, fromAccount);

            // Log successful completion
            logger.info("Transfer successful: {} transferred from {}/{} to {}/{}",
                    request.getAmount(),
                    request.getFromSortCode(), request.getFromAccountNumber(),
                    request.getToSortCode(), request.getToAccountNumber());

            // Return success result with updated balances
            // COBOL equivalent: MOVE 'Y' TO COMM-SUCCESS, MOVE balances to COMM-*BAL fields
            return FundsTransferResult.success(
                    newFromAvailBal, newFromActBal,
                    newToAvailBal, newToActBal);

        } catch (Exception e) {
            // Any exception will cause Spring to rollback the transaction
            // COBOL equivalent: EXEC CICS SYNCPOINT ROLLBACK
            logger.error("Transfer failed due to database error", e);
            throw new RuntimeException("Transfer failed: " + e.getMessage(), e);
        }
    }

    /**
     * Check if the transfer is attempting to move funds to the same account.
     * 
     * COBOL equivalent (from XFRFUN.cbl lines 316-376):
     *   IF COMM-FACCNO = COMM-TACCNO AND COMM-FSCODE = COMM-TSCODE
     *      EXEC CICS ABEND ABCODE('SAME') NODUMP CANCEL END-EXEC
     * 
     * In the Java implementation, we return a failure result instead of abending,
     * which is more appropriate for a REST API service.
     * 
     * @param request the transfer request to validate
     * @return true if FROM and TO accounts are the same, false otherwise
     */
    private boolean isSameAccount(FundsTransferRequest request) {
        return request.getFromAccountNumber().equals(request.getToAccountNumber())
                && request.getFromSortCode().equals(request.getToSortCode());
    }

    /**
     * Record the successful transfer in the PROCTRAN (Processed Transaction) table.
     * 
     * This method implements the COBOL WRITE-TO-PROCTRAN-DB2 section (lines 1571-1723).
     * The PROCTRAN table serves as an audit trail for all banking transactions.
     * 
     * COBOL field mappings:
     * - HV-PROCTRAN-EYECATCHER = 'PRTR' -> Transaction.VALID_EYECATCHER
     * - HV-PROCTRAN-SORT-CODE = COMM-FSCODE -> request.getFromSortCode()
     * - HV-PROCTRAN-ACC-NUMBER = COMM-FACCNO -> request.getFromAccountNumber()
     * - HV-PROCTRAN-DATE = formatted current date -> LocalDate.now()
     * - HV-PROCTRAN-TIME = formatted current time -> LocalTime.now()
     * - HV-PROCTRAN-REF = EIBTASKN -> referenceCounter (unique ID)
     * - HV-PROCTRAN-TYPE = 'TFR' -> Transaction.TYPE_TRANSFER
     * - HV-PROCTRAN-DESC = transfer description -> buildTransferDescription()
     * - HV-PROCTRAN-AMOUNT = COMM-AMT -> request.getAmount()
     * 
     * @param request the original transfer request
     * @param fromAccount the source account (used for sort code and account number)
     */
    private void recordTransaction(FundsTransferRequest request, Account fromAccount) {
        Transaction transaction = Transaction.builder()
                // Eye catcher identifies this as a PROCTRAN record ('PRTR')
                .eyeCatcher(Transaction.VALID_EYECATCHER)
                // Not logically deleted - this is a new active record
                .logicallyDeleted(false)
                // Source account identifiers (where the money came FROM)
                .sortCode(request.getFromSortCode())
                .accountNumber(request.getFromAccountNumber())
                // Current date and time of the transaction
                .transactionDate(LocalDate.now())
                .transactionTime(LocalTime.now())
                // Unique reference number (replaces COBOL EIBTASKN)
                .referenceNumber(referenceCounter.incrementAndGet())
                // Transaction type 'TFR' for transfer
                .transactionType(Transaction.TYPE_TRANSFER)
                // Human-readable description of the transfer
                .description(buildTransferDescription(request))
                // Target account identifiers (where the money went TO)
                .targetSortCode(request.getToSortCode())
                .targetAccountNumber(request.getToAccountNumber())
                // Transfer amount
                .amount(request.getAmount())
                .build();

        // Persist the transaction record to the database
        transactionRepository.save(transaction);
        logger.debug("Transaction record created: {}", transaction.getCompositeId());
    }

    /**
     * Build the transfer description string for the PROCTRAN record.
     * 
     * COBOL equivalent (from XFRFUN.cbl lines 1609-1614):
     *   SET PROC-TRAN-DESC-XFR-FLAG IN PROCTRAN-AREA TO TRUE
     *   MOVE COMM-TSCODE TO PROC-TRAN-DESC-XFR-SORTCODE IN PROCTRAN-AREA
     *   MOVE COMM-TACCNO TO PROC-TRAN-DESC-XFR-ACCOUNT IN PROCTRAN-AREA
     * 
     * The description format is: "TFR TO {sortCode}/{accountNumber}"
     * This allows easy identification of the transfer destination in audit reports.
     * 
     * @param request the transfer request containing target account details
     * @return formatted description string for the transaction record
     */
    private String buildTransferDescription(FundsTransferRequest request) {
        return String.format("TFR TO %s/%s", 
                request.getToSortCode(), 
                request.getToAccountNumber());
    }
}
