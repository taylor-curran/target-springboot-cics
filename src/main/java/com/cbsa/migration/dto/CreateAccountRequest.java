package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.math.BigDecimal;

/**
 * Request DTO for the Create Account (CREACC) operation.
 * Contains the input fields required to create a new bank account.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CreateAccountRequest {

    /**
     * Customer number who will own this account.
     * COMM-CUSTNO PIC 9(10)
     */
    @NotNull(message = "Customer number is required")
    private Long customerNumber;

    /**
     * Account type (ISA, MORTGAGE, SAVING, CURRENT, LOAN).
     * COMM-ACC-TYPE PIC X(8)
     */
    @NotBlank(message = "Account type is required")
    @Size(max = 8, message = "Account type must not exceed 8 characters")
    private String accountType;

    /**
     * Interest rate for the account.
     * COMM-INT-RATE PIC 9(4)V99
     */
    @NotNull(message = "Interest rate is required")
    @DecimalMin(value = "0.0", message = "Interest rate must be non-negative")
    private BigDecimal interestRate;

    /**
     * Overdraft limit for the account.
     * COMM-OVERDRAFT PIC 9(8)
     */
    @NotNull(message = "Overdraft limit is required")
    private Integer overdraftLimit;
}
