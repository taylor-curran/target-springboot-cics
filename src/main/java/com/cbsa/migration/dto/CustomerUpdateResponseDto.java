package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CustomerUpdateResponseDto {
    
    private boolean success;
    
    private String failCode;
    
    private String errorMessage;
    
    private String eyeCatcher;
    
    private String sortCode;
    
    private Long customerNumber;
    
    private String name;
    
    private String address;
    
    private LocalDate dateOfBirth;
    
    private Integer creditScore;
    
    private LocalDate creditScoreReviewDate;
}
