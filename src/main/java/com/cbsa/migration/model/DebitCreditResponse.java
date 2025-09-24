package com.cbsa.migration.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * Response DTO for debit/credit operations
 * Based on PAYDBCR.cpy copybook structure
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DebitCreditResponse {
    
    private boolean success;
    private String failureCode;
    private String sortCode;
    private String accountNumber;
    private BigDecimal availableBalance;
    private BigDecimal actualBalance;
}
