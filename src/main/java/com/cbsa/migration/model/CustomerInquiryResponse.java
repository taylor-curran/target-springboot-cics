package com.cbsa.migration.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

/**
 * Response DTO for customer inquiry operations
 * Based on INQCUST.cpy copybook structure
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CustomerInquiryResponse {
    
    private boolean success;
    private String failureCode;
    private String eyeCatcher;
    private String sortCode;
    private Long customerNumber;
    private String name;
    private String address;
    private LocalDate dateOfBirth;
    private Integer creditScore;
    private LocalDate creditScoreReviewDate;
}
