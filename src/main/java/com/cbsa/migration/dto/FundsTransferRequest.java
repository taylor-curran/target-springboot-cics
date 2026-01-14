package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Positive;
import java.math.BigDecimal;

/**
 * Request DTO for funds transfer operations.
 * 
 * This class maps to the input fields of the COBOL XFRFUN COMMAREA structure
 * defined in XFRFUN.cpy. The COMMAREA was used to pass data between the
 * calling program (BNK1TFN) and the business logic program (XFRFUN).
 * 
 * COBOL COMMAREA structure (from XFRFUN.cpy):
 *   03 COMM-FACCNO               PIC 9(8).    -> fromAccountNumber
 *   03 COMM-FSCODE               PIC 9(6).    -> fromSortCode
 *   03 COMM-TACCNO               PIC 9(8).    -> toAccountNumber
 *   03 COMM-TSCODE               PIC 9(6).    -> toSortCode
 *   03 COMM-AMT                  PIC S9(10)V99. -> amount
 * 
 * Validation rules are enforced using Bean Validation annotations to match
 * the COBOL PIC clauses (e.g., 8 digits for account numbers, 6 for sort codes).
 * 
 * @see com.cbsa.migration.service.FundsTransferService
 * @see com.cbsa.migration.dto.FundsTransferResult
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FundsTransferRequest {

    /**
     * FROM account number (8 digits)
     * COMM-FACCNO PIC 9(8)
     */
    @NotBlank(message = "From account number is required")
    @Pattern(regexp = "\\d{8}", message = "From account number must be 8 digits")
    private String fromAccountNumber;

    /**
     * FROM sort code (6 digits)
     * COMM-FSCODE PIC 9(6)
     */
    @NotBlank(message = "From sort code is required")
    @Pattern(regexp = "\\d{6}", message = "From sort code must be 6 digits")
    private String fromSortCode;

    /**
     * TO account number (8 digits)
     * COMM-TACCNO PIC 9(8)
     */
    @NotBlank(message = "To account number is required")
    @Pattern(regexp = "\\d{8}", message = "To account number must be 8 digits")
    private String toAccountNumber;

    /**
     * TO sort code (6 digits)
     * COMM-TSCODE PIC 9(6)
     */
    @NotBlank(message = "To sort code is required")
    @Pattern(regexp = "\\d{6}", message = "To sort code must be 6 digits")
    private String toSortCode;

    /**
     * Transfer amount
     * COMM-AMT PIC S9(10)V99
     */
    @NotNull(message = "Amount is required")
    @Positive(message = "Amount must be positive")
    private BigDecimal amount;
}
