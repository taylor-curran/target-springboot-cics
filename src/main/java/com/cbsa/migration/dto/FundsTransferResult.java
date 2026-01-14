package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * Result DTO for funds transfer operations.
 * 
 * This class maps to the output fields of the COBOL XFRFUN COMMAREA structure
 * defined in XFRFUN.cpy. After a transfer operation, the COMMAREA was populated
 * with the result status and updated account balances.
 * 
 * COBOL COMMAREA output structure (from XFRFUN.cpy):
 *   03 COMM-FAVBAL               PIC S9(10)V99. -> fromAvailableBalance
 *   03 COMM-FACTBAL              PIC S9(10)V99. -> fromActualBalance
 *   03 COMM-TAVBAL               PIC S9(10)V99. -> toAvailableBalance
 *   03 COMM-TACTBAL              PIC S9(10)V99. -> toActualBalance
 *   03 COMM-FAIL-CODE            PIC X.         -> failCode
 *   03 COMM-SUCCESS              PIC X.         -> success ('Y' or 'N')
 * 
 * The fail codes are preserved from the original COBOL implementation to maintain
 * compatibility with any existing error handling logic that depends on these codes.
 * 
 * @see com.cbsa.migration.service.FundsTransferService
 * @see com.cbsa.migration.dto.FundsTransferRequest
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FundsTransferResult {

    // ============================================================
    // Fail codes matching COBOL XFRFUN COMM-FAIL-CODE values
    // These codes are used to identify the specific reason for
    // transfer failure, matching the original COBOL behavior.
    // ============================================================
    
    /**
     * Fail code '1': FROM account not found.
     * COBOL: Set when SQLCODE = +100 on FROM account SELECT.
     */
    public static final String FAIL_CODE_FROM_ACCOUNT_NOT_FOUND = "1";
    
    /**
     * Fail code '2': TO account not found.
     * COBOL: Set when SQLCODE = +100 on TO account SELECT.
     */
    public static final String FAIL_CODE_TO_ACCOUNT_NOT_FOUND = "2";
    
    /**
     * Fail code '3': Other database error.
     * COBOL: Set when SQLCODE indicates a database error other than not found.
     */
    public static final String FAIL_CODE_DATABASE_ERROR = "3";
    
    /**
     * Fail code '4': Invalid amount (<= 0).
     * COBOL: Set when COMM-AMT <= ZERO.
     */
    public static final String FAIL_CODE_INVALID_AMOUNT = "4";
    
    /**
     * Fail code '5': Insufficient funds in source account.
     * Note: This is a Java addition for overdraft prevention.
     * The original COBOL did not check overdraft limits.
     */
    public static final String FAIL_CODE_INSUFFICIENT_FUNDS = "5";
    
    /**
     * Fail code '6': Same account transfer not allowed.
     * COBOL: Originally caused ABEND with code 'SAME'.
     * In Java, we return a failure result instead of abending.
     */
    public static final String FAIL_CODE_SAME_ACCOUNT = "6";

    /**
     * Whether the transfer was successful
     * COMM-SUCCESS PIC X
     */
    private boolean success;

    /**
     * Fail code if transfer failed
     * COMM-FAIL-CODE PIC X
     */
    private String failCode;

    /**
     * FROM account available balance after transfer
     * COMM-FAVBAL PIC S9(10)V99
     */
    private BigDecimal fromAvailableBalance;

    /**
     * FROM account actual balance after transfer
     * COMM-FACTBAL PIC S9(10)V99
     */
    private BigDecimal fromActualBalance;

    /**
     * TO account available balance after transfer
     * COMM-TAVBAL PIC S9(10)V99
     */
    private BigDecimal toAvailableBalance;

    /**
     * TO account actual balance after transfer
     * COMM-TACTBAL PIC S9(10)V99
     */
    private BigDecimal toActualBalance;

    /**
     * Error message for failed transfers
     */
    private String errorMessage;

    /**
     * Create a successful result
     */
    public static FundsTransferResult success(BigDecimal fromAvailBal, BigDecimal fromActBal,
                                               BigDecimal toAvailBal, BigDecimal toActBal) {
        return FundsTransferResult.builder()
                .success(true)
                .fromAvailableBalance(fromAvailBal)
                .fromActualBalance(fromActBal)
                .toAvailableBalance(toAvailBal)
                .toActualBalance(toActBal)
                .build();
    }

    /**
     * Create a failure result
     */
    public static FundsTransferResult failure(String failCode, String errorMessage) {
        return FundsTransferResult.builder()
                .success(false)
                .failCode(failCode)
                .errorMessage(errorMessage)
                .build();
    }
}
