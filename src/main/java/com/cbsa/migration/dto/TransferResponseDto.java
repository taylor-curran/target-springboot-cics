package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TransferResponseDto {

    private boolean success;

    private String failureCode;

    private String failureMessage;

    private String fromSortCode;

    private String fromAccountNumber;

    private BigDecimal fromAvailableBalance;

    private BigDecimal fromActualBalance;

    private String toSortCode;

    private String toAccountNumber;

    private BigDecimal toAvailableBalance;

    private BigDecimal toActualBalance;

    private BigDecimal amount;
}
