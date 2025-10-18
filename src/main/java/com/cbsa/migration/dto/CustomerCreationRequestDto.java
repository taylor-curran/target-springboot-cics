package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.time.LocalDate;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CustomerCreationRequestDto {
    
    @NotBlank
    @Size(min = 6, max = 6)
    private String sortCode;
    
    @NotBlank
    @Size(max = 60)
    private String name;
    
    @NotBlank
    @Size(max = 160)
    private String address;
    
    @NotNull
    private LocalDate dateOfBirth;
}
