package com.cbsa.migration.dto;

import java.math.BigDecimal;

public class BalanceUpdateResponseDto {

    private String sortCode;
    private BigDecimal availableBalance;
    private BigDecimal actualBalance;
    private boolean success;
    private String failCode;

    public BalanceUpdateResponseDto() {}

    public BalanceUpdateResponseDto(boolean success, String failCode) {
        this.success = success;
        this.failCode = failCode;
    }

    public String getSortCode() {
        return sortCode;
    }

    public void setSortCode(String sortCode) {
        this.sortCode = sortCode;
    }

    public BigDecimal getAvailableBalance() {
        return availableBalance;
    }

    public void setAvailableBalance(BigDecimal availableBalance) {
        this.availableBalance = availableBalance;
    }

    public BigDecimal getActualBalance() {
        return actualBalance;
    }

    public void setActualBalance(BigDecimal actualBalance) {
        this.actualBalance = actualBalance;
    }

    public boolean isSuccess() {
        return success;
    }

    public void setSuccess(boolean success) {
        this.success = success;
    }

    public String getFailCode() {
        return failCode;
    }

    public void setFailCode(String failCode) {
        this.failCode = failCode;
    }
}
