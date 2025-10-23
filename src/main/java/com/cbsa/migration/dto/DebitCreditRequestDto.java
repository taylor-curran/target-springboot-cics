package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.math.BigDecimal;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DebitCreditRequestDto {
    
    @NotBlank(message = "Account number is required")
    @Size(min = 8, max = 8, message = "Account number must be exactly 8 characters")
    private String accountNumber;
    
    @NotNull(message = "Amount is required")
    private BigDecimal amount;
    
    @NotBlank(message = "Sort code is required")
    @Pattern(regexp = "\\d{6}", message = "Sort code must be exactly 6 digits")
    private String sortCode;
    
    @NotNull(message = "Facility type is required")
    private Integer facilityType;
    
    @Size(max = 8)
    private String applicationId;
    
    @Size(max = 8)
    private String userId;
    
    @Size(max = 8)
    private String facilityName;
    
    @Size(max = 8)
    private String networkId;
    
    public String getOriginDescription() {
        String origin = (applicationId != null ? applicationId : "") +
                       (userId != null ? userId : "");
        return origin.length() > 14 ? origin.substring(0, 14) : origin;
    }
}
