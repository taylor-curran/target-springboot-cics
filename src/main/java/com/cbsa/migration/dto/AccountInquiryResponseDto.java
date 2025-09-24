package com.cbsa.migration.dto;

import java.math.BigDecimal;
import java.time.LocalDate;

public class AccountInquiryResponseDto {

    private String eyeCatcher;
    private Long customerNumber;
    private String sortCode;
    private String accountNumber;
    private String accountType;
    private BigDecimal interestRate;
    private LocalDate openedDate;
    private Integer overdraftLimit;
    private LocalDate lastStatementDate;
    private LocalDate nextStatementDate;
    private BigDecimal availableBalance;
    private BigDecimal actualBalance;
    private boolean success;

    public AccountInquiryResponseDto() {}

    public AccountInquiryResponseDto(boolean success) {
        this.success = success;
    }

    public String getEyeCatcher() {
        return eyeCatcher;
    }

    public void setEyeCatcher(String eyeCatcher) {
        this.eyeCatcher = eyeCatcher;
    }

    public Long getCustomerNumber() {
        return customerNumber;
    }

    public void setCustomerNumber(Long customerNumber) {
        this.customerNumber = customerNumber;
    }

    public String getSortCode() {
        return sortCode;
    }

    public void setSortCode(String sortCode) {
        this.sortCode = sortCode;
    }

    public String getAccountNumber() {
        return accountNumber;
    }

    public void setAccountNumber(String accountNumber) {
        this.accountNumber = accountNumber;
    }

    public String getAccountType() {
        return accountType;
    }

    public void setAccountType(String accountType) {
        this.accountType = accountType;
    }

    public BigDecimal getInterestRate() {
        return interestRate;
    }

    public void setInterestRate(BigDecimal interestRate) {
        this.interestRate = interestRate;
    }

    public LocalDate getOpenedDate() {
        return openedDate;
    }

    public void setOpenedDate(LocalDate openedDate) {
        this.openedDate = openedDate;
    }

    public Integer getOverdraftLimit() {
        return overdraftLimit;
    }

    public void setOverdraftLimit(Integer overdraftLimit) {
        this.overdraftLimit = overdraftLimit;
    }

    public LocalDate getLastStatementDate() {
        return lastStatementDate;
    }

    public void setLastStatementDate(LocalDate lastStatementDate) {
        this.lastStatementDate = lastStatementDate;
    }

    public LocalDate getNextStatementDate() {
        return nextStatementDate;
    }

    public void setNextStatementDate(LocalDate nextStatementDate) {
        this.nextStatementDate = nextStatementDate;
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
}
