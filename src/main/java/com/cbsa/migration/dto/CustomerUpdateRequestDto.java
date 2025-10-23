package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CustomerUpdateRequestDto {
    
    @NotBlank(message = "Sort code is required")
    @Pattern(regexp = "\\d{6}", message = "Sort code must be 6 digits")
    private String sortCode;
    
    @NotNull(message = "Customer number is required")
    private Long customerNumber;
    
    @Size(max = 60, message = "Customer name must not exceed 60 characters")
    private String name;
    
    @Size(max = 160, message = "Customer address must not exceed 160 characters")
    private String address;
}
