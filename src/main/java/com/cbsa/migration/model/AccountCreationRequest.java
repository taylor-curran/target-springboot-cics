package com.cbsa.migration.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.math.BigDecimal;

/**
 * Request DTO for account creation operations
 * Based on CREACC.cpy copybook structure
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccountCreationRequest {
    
    @NotNull
    private Long customerNumber;
    
    @NotBlank
    @Size(max = 8)
    private String accountType;
    
    @NotNull
    private BigDecimal interestRate;
    
    @NotNull
    private Integer overdraftLimit;
    
    @NotNull
    private BigDecimal initialBalance;
}
