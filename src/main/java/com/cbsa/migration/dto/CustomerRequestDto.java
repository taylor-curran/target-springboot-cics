package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.time.LocalDate;

/**
 * Request DTO for Customer creation/update operations.
 * Validates input data before processing.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CustomerRequestDto {
    
    /**
     * Customer's full name
     */
    @NotBlank(message = "Customer name is required")
    @Size(max = 60, message = "Customer name must not exceed 60 characters")
    private String name;
    
    /**
     * Customer's address
     */
    @NotBlank(message = "Customer address is required")
    @Size(max = 160, message = "Customer address must not exceed 160 characters")
    private String address;
    
    /**
     * Customer's date of birth
     */
    @NotNull(message = "Date of birth is required")
    private LocalDate dateOfBirth;
    
    /**
     * Branch sort code (for new customers)
     */
    @NotBlank(message = "Sort code is required")
    @Size(min = 6, max = 6, message = "Sort code must be exactly 6 characters")
    private String sortCode;
    
    /**
     * Initial credit score (optional, system can calculate)
     */
    private Integer creditScore;
}
