package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.Digits;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import java.math.BigDecimal;

/**
 * Request DTO for credit/debit operations
 * Maps to COBOL BNK1CRA screen input fields and DBCRFUN COMMAREA
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DebitCreditRequestDto {

    /**
     * Account number - must be exactly 8 numeric digits, non-zero
     * COMM-ACCNO PIC X(8)
     */
    @NotBlank(message = "Account number is required")
    @Pattern(regexp = "\\d{8}", message = "Account number must be exactly 8 digits")
    private String accountNumber;

    /**
     * Sign indicating credit (+) or debit (-)
     * COMM-SIGN PIC X
     */
    @NotBlank(message = "Sign is required")
    @Pattern(regexp = "[+\\-]", message = "Sign must be '+' or '-'")
    private String sign;

    /**
     * Amount to credit or debit - must be positive, non-zero, max 2 decimal places
     * COMM-AMT PIC 9(12) (combined with sign to form signed amount)
     */
    @NotNull(message = "Amount is required")
    @DecimalMin(value = "0.01", message = "Amount must be greater than zero")
    @Digits(integer = 10, fraction = 2, message = "Amount must have at most 10 integer digits and 2 decimal places")
    private BigDecimal amount;

    /**
     * Channel type: TELLER for over-the-counter, PAYMENT for payment channel
     * Derived from COBOL COMM-FACILTYPE (496 = PAYMENT, other = TELLER)
     */
    @Pattern(regexp = "TELLER|PAYMENT", message = "Channel type must be 'TELLER' or 'PAYMENT'")
    private String channelType;
}
