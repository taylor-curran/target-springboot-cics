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

    private String failCode;

    private String errorMessage;

    private BigDecimal transferAmount;

    private AccountBalanceDto fromAccount;

    private AccountBalanceDto toAccount;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AccountBalanceDto {
        private String sortCode;
        private String accountNumber;
        private BigDecimal availableBalance;
        private BigDecimal actualBalance;
    }
}
