package com.cbsa.migration.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Response DTO for account creation operations
 * Based on CREACC.cpy copybook structure
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccountCreationResponse {
    
    private boolean success;
    private String failureCode;
    private String accountNumber;
    private Long customerNumber;
    private String name;
    private String address;
    private String dateOfBirth;
}
