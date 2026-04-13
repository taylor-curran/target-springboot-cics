package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Positive;
import java.math.BigDecimal;

/**
 * Request DTO for debit/credit operations.
 * Replaces the input portion of the PAYDBCR COMMAREA used by BNK1CRA/DBCRFUN.
 *
 * COBOL COMMAREA fields mapped:
 *   COMM-AMT        -> amount
 *   COMM-FACILTYPE  -> facilityType (496 = payment origin, other = teller/counter)
 *   COMM-ORIGIN     -> originDescription (first 14 chars used for PROCTRAN desc)
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DebitCreditRequest {

    /**
     * The monetary amount to credit or debit (always positive; the endpoint
     * determines whether it is a credit or debit).
     */
    @NotNull(message = "Amount is required")
    @Positive(message = "Amount must be greater than zero")
    private BigDecimal amount;

    /**
     * Facility type from COMM-FACILTYPE.
     * 496 indicates a payment/API origin (maps to PDR/PCR transaction types).
     * Any other value (or null) indicates a teller/counter origin (maps to DEB/CRE).
     */
    private Integer facilityType;

    /**
     * Origin description, used when facilityType is 496 (payment origin).
     * Up to 14 characters are written to the PROCTRAN description field.
     * Corresponds to COMM-ORIGIN(1:14) in COBOL.
     */
    private String originDescription;
}
