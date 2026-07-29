package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

/**
 * Response DTO for a single customer inquiry.
 * Mirrors the commarea returned by the INQCUST.cbl COBOL program
 * (INQCUST copybook fields) for read operations.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CustomerDTO {

    /**
     * Eye catcher for the record - always "CUST".
     * INQCUST-EYE PIC X(4)
     */
    private String eyeCatcher;

    /**
     * Branch sort code.
     * INQCUST-SCODE PIC X(6)
     */
    private String sortCode;

    /**
     * Customer's unique identifier.
     * INQCUST-CUSTNO PIC 9(10)
     */
    private Long customerNumber;

    /**
     * Customer's full name.
     * INQCUST-NAME PIC X(60)
     */
    private String name;

    /**
     * Customer's full address.
     * INQCUST-ADDR PIC X(160)
     */
    private String address;

    /**
     * Customer's date of birth.
     * INQCUST-DOB (DDMMYYYY)
     */
    private LocalDate dateOfBirth;

    /**
     * Customer's credit score.
     * INQCUST-CREDIT-SCORE PIC 999
     */
    private Integer creditScore;

    /**
     * Date when the credit score was last reviewed.
     * INQCUST-CS-REVIEW-DT (DDMMYYYY)
     */
    private LocalDate creditScoreReviewDate;
}
