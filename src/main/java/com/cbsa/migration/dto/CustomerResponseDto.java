package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.util.List;

/**
 * Response DTO for Customer information.
 * Provides a clean API response without exposing internal database fields.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CustomerResponseDto {
    
    /**
     * Customer's full name
     */
    private String name;
    
    /**
     * Customer's address
     */
    private String address;
    
    /**
     * Customer's date of birth
     */
    private LocalDate dateOfBirth;
    
    /**
     * Customer number (business identifier)
     */
    private Long customerNumber;
    
    /**
     * Branch sort code
     */
    private String sortCode;
    
    /**
     * Summary of customer's accounts (optional)
     */
    private List<AccountSummaryDto> accounts;
    
    /**
     * Total number of accounts
     */
    private Integer accountCount;
    
    /**
     * Customer status (derived field)
     */
    private String status;
    
    /**
     * Customer's credit score (from credit agency check)
     */
    private Integer creditScore;
    
    /**
     * Date when the credit score should be reviewed
     */
    private LocalDate creditScoreReviewDate;
    
    /**
     * COBOL fail code (null on success, e.g. O, Y, Z, 1, 3, G on failure)
     */
    private String failCode;
    
    /**
     * Whether the operation was successful
     */
    private Boolean success;
}
