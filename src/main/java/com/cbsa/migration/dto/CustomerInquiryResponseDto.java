package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

/**
 * Response DTO for Customer Inquiry operations (INQCUST migration).
 * Maps to the COBOL INQCUST commarea response structure.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CustomerInquiryResponseDto {
    
    /**
     * Eye catcher for the record - "CUST" for valid customer records
     * Maps to INQCUST-EYE in COBOL
     */
    private String eyeCatcher;
    
    /**
     * Branch sort code
     * Maps to INQCUST-SCODE in COBOL
     */
    private String sortCode;
    
    /**
     * Customer number (business identifier)
     * Maps to INQCUST-CUSTNO in COBOL
     */
    private Long customerNumber;
    
    /**
     * Customer's full name
     * Maps to INQCUST-NAME in COBOL
     */
    private String name;
    
    /**
     * Customer's address
     * Maps to INQCUST-ADDR in COBOL
     */
    private String address;
    
    /**
     * Customer's date of birth
     * Maps to INQCUST-DOB in COBOL
     */
    private LocalDate dateOfBirth;
    
    /**
     * Customer's credit score
     * Maps to INQCUST-CREDIT-SCORE in COBOL
     */
    private Integer creditScore;
    
    /**
     * Date when the credit score was last reviewed
     * Maps to INQCUST-CS-REVIEW-DT in COBOL
     */
    private LocalDate creditScoreReviewDate;
    
    /**
     * Indicates if the inquiry was successful
     * Maps to INQCUST-INQ-SUCCESS in COBOL ('Y' or 'N')
     */
    private boolean success;
    
    /**
     * Failure code if inquiry was not successful
     * Maps to INQCUST-INQ-FAIL-CD in COBOL
     * '0' = Success
     * '1' = Customer not found
     * '2' = Storm drain condition (VSAM RLS abend)
     * '9' = System error
     */
    private String failureCode;
    
    /**
     * Human-readable error message for failed inquiries
     */
    private String errorMessage;
}
