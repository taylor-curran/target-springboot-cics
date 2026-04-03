package com.cbsa.migration.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

/**
 * Data Transfer Object mapping the COBOL CUSTOMER copybook record fields
 * to a REST-friendly JSON format.
 *
 * Based on CUSTOMER.cpy:
 *   03 CUSTOMER-RECORD.
 *     05 CUSTOMER-EYECATCHER       PIC X(4).      -> eyeCatcher ("CUST")
 *     05 CUSTOMER-KEY.
 *       07 CUSTOMER-SORTCODE       PIC 9(6).      -> sortCode
 *       07 CUSTOMER-NUMBER         PIC 9(10).     -> customerNumber
 *     05 CUSTOMER-NAME             PIC X(60).     -> name
 *     05 CUSTOMER-ADDRESS          PIC X(160).    -> address
 *     05 CUSTOMER-DATE-OF-BIRTH    PIC 9(8).      -> dateOfBirth
 *     05 CUSTOMER-CREDIT-SCORE     PIC 999.       -> creditScore
 *     05 CUSTOMER-CS-REVIEW-DATE   PIC 9(8).      -> creditScoreReviewDate
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CustomerDTO {

    /**
     * Eye catcher identifying the record type - always "CUST"
     */
    @JsonProperty("eyeCatcher")
    private String eyeCatcher;

    /**
     * Bank sort code (6-digit branch identifier)
     */
    @JsonProperty("sortCode")
    private String sortCode;

    /**
     * Unique customer number
     */
    @JsonProperty("customerNumber")
    private Long customerNumber;

    /**
     * Customer full name (up to 60 chars in COBOL)
     */
    @JsonProperty("name")
    private String name;

    /**
     * Customer address (up to 160 chars in COBOL)
     */
    @JsonProperty("address")
    private String address;

    /**
     * Date of birth (COBOL format DDMMYYYY, exposed as ISO date)
     */
    @JsonProperty("dateOfBirth")
    private LocalDate dateOfBirth;

    /**
     * Credit score (0-999)
     */
    @JsonProperty("creditScore")
    private Integer creditScore;

    /**
     * Date the credit score was last reviewed (COBOL format DDMMYYYY, exposed as ISO date)
     */
    @JsonProperty("creditScoreReviewDate")
    private LocalDate creditScoreReviewDate;
}
