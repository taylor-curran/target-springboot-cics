package com.cbsa.migration.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import java.math.BigDecimal;

/**
 * Request DTO for debit/credit operations
 * Based on PAYDBCR.cpy copybook structure
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DebitCreditRequest {
    
    @NotBlank
    @Pattern(regexp = "\\d{6}")
    private String sortCode;
    
    @NotBlank
    @Pattern(regexp = "\\d{8}")
    private String accountNumber;
    
    @NotNull
    private BigDecimal amount;
    
    private Integer facilityType;
    
    private String origin;
}
