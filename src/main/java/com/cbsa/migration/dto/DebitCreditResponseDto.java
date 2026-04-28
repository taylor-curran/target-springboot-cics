package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * Response DTO for credit/debit operations
 * Maps to COBOL DBCRFUN COMMAREA return fields
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DebitCreditResponseDto {

    /**
     * Whether the operation was successful
     * COMM-SUCCESS PIC X ('Y' or 'N')
     */
    private boolean success;

    /**
     * Fail code from DBCRFUN business logic
     * COMM-FAIL-CODE PIC X
     * 0 = success, 1 = account not found, 2 = unexpected DB error,
     * 3 = insufficient funds (payment channel), 4 = MORTGAGE/LOAN via payment channel
     */
    private String failCode;

    /**
     * Human-readable message describing the result
     */
    private String message;

    /**
     * Sort code of the account's branch
     * COMM-SORTC PIC 9(6)
     */
    private String sortCode;

    /**
     * Updated available balance after the operation
     * COMM-AV-BAL PIC S9(10)V99
     */
    private BigDecimal availableBalance;

    /**
     * Updated actual balance after the operation
     * COMM-ACT-BAL PIC S9(10)V99
     */
    private BigDecimal actualBalance;
}
