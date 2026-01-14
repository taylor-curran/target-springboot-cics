package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.math.BigDecimal;

/**
 * Request DTO for Transfer Funds operations.
 * Migrated from COBOL XFRFUN program (BNK1TFN.cbl interface).
 * 
 * Validates input data before processing:
 * - FROM account number: Must be numeric and not '00000000'
 * - TO account number: Must be numeric and not '00000000'
 * - Different accounts: FROM and TO accounts must be different
 * - Amount: Must be greater than zero
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TransferRequestDto {
    
    /**
     * Source account sort code (FROM account)
     */
    @NotBlank(message = "From sort code is required")
    @Size(min = 6, max = 6, message = "From sort code must be exactly 6 characters")
    @Pattern(regexp = "\\d{6}", message = "From sort code must be numeric")
    private String fromSortCode;
    
    /**
     * Source account number (FROM account)
     * Must be numeric and not '00000000'
     */
    @NotBlank(message = "From account number is required")
    @Size(min = 8, max = 8, message = "From account number must be exactly 8 characters")
    @Pattern(regexp = "\\d{8}", message = "From account number must be numeric")
    private String fromAccountNumber;
    
    /**
     * Target account sort code (TO account)
     */
    @NotBlank(message = "To sort code is required")
    @Size(min = 6, max = 6, message = "To sort code must be exactly 6 characters")
    @Pattern(regexp = "\\d{6}", message = "To sort code must be numeric")
    private String toSortCode;
    
    /**
     * Target account number (TO account)
     * Must be numeric and not '00000000'
     */
    @NotBlank(message = "To account number is required")
    @Size(min = 8, max = 8, message = "To account number must be exactly 8 characters")
    @Pattern(regexp = "\\d{8}", message = "To account number must be numeric")
    private String toAccountNumber;
    
    /**
     * Transfer amount
     * Must be greater than zero
     */
    @NotNull(message = "Transfer amount is required")
    @DecimalMin(value = "0.01", message = "Transfer amount must be greater than zero")
    private BigDecimal amount;
}
