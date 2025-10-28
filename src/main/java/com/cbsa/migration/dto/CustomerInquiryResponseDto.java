package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

/**
 * Response DTO for Customer Inquiry operations.
 * Migrated from COBOL INQCUST.cpy copybook.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CustomerInquiryResponseDto {
    
    public static final String SUCCESS_CODE = "0";
    public static final String NOT_FOUND_CODE = "1";
    public static final String DATABASE_ERROR_CODE = "9";
    
    /**
     * Eye catcher - always "CUST"
     * Maps to INQCUST-EYE PIC X(4)
     */
    private String eyeCatcher;
    
    /**
     * Branch sort code
     * Maps to INQCUST-SCODE PIC X(6)
     */
    private String sortCode;
    
    /**
     * Customer number
     * Maps to INQCUST-CUSTNO PIC 9(10)
     */
    private Long customerNumber;
    
    /**
     * Customer name
     * Maps to INQCUST-NAME PIC X(60)
     */
    private String name;
    
    /**
     * Customer address
     * Maps to INQCUST-ADDR PIC X(160)
     */
    private String address;
    
    /**
     * Date of birth
     * Maps to INQCUST-DOB (DDMMYYYY in COBOL)
     */
    private LocalDate dateOfBirth;
    
    /**
     * Credit score (1-999)
     * Maps to INQCUST-CREDIT-SCORE PIC 999
     */
    private Integer creditScore;
    
    /**
     * Credit score review date
     * Maps to INQCUST-CS-REVIEW-DT (DDMMYYYY in COBOL)
     */
    private LocalDate creditScoreReviewDate;
    
    /**
     * Inquiry success flag ('Y' or 'N')
     * Maps to INQCUST-INQ-SUCCESS PIC X
     */
    private boolean success;
    
    /**
     * Failure code
     * Maps to INQCUST-INQ-FAIL-CD PIC X
     * 
     * Possible values:
     * - '0': Success
     * - '1': Customer not found
     * - '9': Database/NCS error
     */
    private String failCode;
}
