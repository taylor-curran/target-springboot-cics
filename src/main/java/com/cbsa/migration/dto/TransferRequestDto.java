package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.math.BigDecimal;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TransferRequestDto {

    @NotBlank(message = "Source sort code is required")
    @Size(min = 6, max = 6, message = "Source sort code must be exactly 6 characters")
    private String fromSortCode;

    @NotBlank(message = "Source account number is required")
    @Size(min = 8, max = 8, message = "Source account number must be exactly 8 characters")
    private String fromAccountNumber;

    @NotBlank(message = "Target sort code is required")
    @Size(min = 6, max = 6, message = "Target sort code must be exactly 6 characters")
    private String toSortCode;

    @NotBlank(message = "Target account number is required")
    @Size(min = 8, max = 8, message = "Target account number must be exactly 8 characters")
    private String toAccountNumber;

    @NotNull(message = "Transfer amount is required")
    @DecimalMin(value = "0.01", message = "Transfer amount must be greater than zero")
    private BigDecimal amount;
}
