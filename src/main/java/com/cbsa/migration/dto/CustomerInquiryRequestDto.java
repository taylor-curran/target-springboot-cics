package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

/**
 * Request DTO for Customer Inquiry operations.
 * Migrated from COBOL INQCUST.cpy copybook.
 * 
 * Special customer numbers:
 * - 0: Generate and return a random customer
 * - 9999999999: Return the last (highest) customer number in use
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CustomerInquiryRequestDto {
    
    /**
     * Branch sort code (6 digits)
     * Maps to INQCUST-SCODE PIC X(6)
     */
    @NotBlank(message = "Sort code is required")
    @Size(min = 6, max = 6, message = "Sort code must be exactly 6 characters")
    private String sortCode;
    
    /**
     * Customer number to inquire about
     * Maps to INQCUST-CUSTNO PIC 9(10)
     * 
     * Special values:
     * - 0: Random customer
     * - 9999999999: Last customer
     */
    @NotNull(message = "Customer number is required")
    private Long customerNumber;
}
