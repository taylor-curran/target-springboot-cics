package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.Size;
import javax.validation.constraints.Min;
import java.math.BigDecimal;

/**
 * Request DTO for Account update operations.
 * Maps to COBOL UPDACC.cpy structure for account updates.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccountUpdateRequestDto {
    
    /**
     * Account type (e.g., CURRENT, SAVINGS)
     * Maps to COMM-ACC-TYPE PIC X(8)
     */
    @NotBlank(message = "Account type is required and cannot be blank")
    @Size(max = 8, message = "Account type must not exceed 8 characters")
    private String accountType;
    
    /**
     * Interest rate percentage
     * Maps to COMM-INT-RATE PIC 9(4)V99
     */
    @DecimalMin(value = "0.0", message = "Interest rate must be non-negative")
    private BigDecimal interestRate;
    
    /**
     * Overdraft limit
     * Maps to COMM-OVERDRAFT PIC 9(8)
     */
    @Min(value = 0, message = "Overdraft limit must be non-negative")
    private Integer overdraftLimit;
}
