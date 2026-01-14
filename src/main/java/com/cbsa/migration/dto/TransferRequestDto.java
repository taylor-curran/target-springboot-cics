package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.math.BigDecimal;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TransferRequestDto {

    @NotBlank(message = "FROM sort code is required")
    @Size(min = 6, max = 6, message = "FROM sort code must be exactly 6 digits")
    @Pattern(regexp = "\\d{6}", message = "FROM sort code must be numeric")
    private String fromSortCode;

    @NotBlank(message = "FROM account number is required")
    @Size(min = 8, max = 8, message = "FROM account number must be exactly 8 digits")
    @Pattern(regexp = "\\d{8}", message = "FROM account number must be numeric")
    private String fromAccountNumber;

    @NotBlank(message = "TO sort code is required")
    @Size(min = 6, max = 6, message = "TO sort code must be exactly 6 digits")
    @Pattern(regexp = "\\d{6}", message = "TO sort code must be numeric")
    private String toSortCode;

    @NotBlank(message = "TO account number is required")
    @Size(min = 8, max = 8, message = "TO account number must be exactly 8 digits")
    @Pattern(regexp = "\\d{8}", message = "TO account number must be numeric")
    private String toAccountNumber;

    @NotNull(message = "Transfer amount is required")
    @DecimalMin(value = "0.01", message = "Transfer amount must be greater than zero")
    private BigDecimal amount;
}
