package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * Response DTO for debit/credit operations.
 * Replaces the output portion of the PAYDBCR COMMAREA returned by DBCRFUN.
 *
 * COBOL COMMAREA fields mapped:
 *   COMM-SUCCESS    -> success ('Y'/'N')
 *   COMM-FAIL-CODE  -> failCode
 *   COMM-ACT-BAL    -> actualBalance
 *   COMM-AV-BAL     -> availableBalance
 *   COMM-SORTC      -> sortCode
 *   COMM-ACCNO      -> accountNumber
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DebitCreditResponse {

    /**
     * Whether the operation was successful.
     * Maps to COMM-SUCCESS: 'Y' = true, 'N' = false.
     */
    private boolean success;

    /**
     * Fail code when success is false.
     * Maps to COMM-FAIL-CODE in COBOL:
     *   '0' - No error
     *   '1' - Account not found
     *   '2' - Unexpected database error
     *   '3' - Insufficient funds (debit only, payment origin)
     *   '4' - Cannot debit/credit MORTGAGE or LOAN account (payment origin)
     */
    private String failCode;

    /**
     * Human-readable message describing the result.
     * Replaces the BMS screen MESSAGEO field from BNK1CRA.
     */
    private String message;

    /**
     * The account number that was debited/credited.
     */
    private String accountNumber;

    /**
     * The sort code of the account.
     */
    private String sortCode;

    /**
     * The updated actual balance (settled transactions only).
     * Maps to COMM-ACT-BAL / SUBPGM-ACT-BAL.
     */
    private BigDecimal actualBalance;

    /**
     * The updated available balance (including pending transactions).
     * Maps to COMM-AV-BAL / SUBPGM-AV-BAL.
     */
    private BigDecimal availableBalance;
}
