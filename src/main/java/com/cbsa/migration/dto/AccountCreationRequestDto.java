package com.cbsa.migration.dto;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Positive;
import javax.validation.constraints.Size;

import java.math.BigDecimal;

public class AccountCreationRequestDto {

    @NotNull(message = "Customer number is required")
    @Positive(message = "Customer number must be positive")
    private Long customerNumber;

    @NotNull(message = "Account type is required")
    @Size(min = 1, max = 8, message = "Account type must be between 1 and 8 characters")
    @Pattern(regexp = "^[A-Z][A-Z0-9\\s]*$", message = "Account type must start with a letter and contain only uppercase letters, numbers, and spaces")
    private String accountType;

    @NotNull(message = "Interest rate is required")
    private BigDecimal interestRate;

    @NotNull(message = "Overdraft limit is required")
    private Integer overdraftLimit;

    public AccountCreationRequestDto() {}

    public AccountCreationRequestDto(Long customerNumber, String accountType, BigDecimal interestRate, Integer overdraftLimit) {
        this.customerNumber = customerNumber;
        this.accountType = accountType;
        this.interestRate = interestRate;
        this.overdraftLimit = overdraftLimit;
    }

    public Long getCustomerNumber() {
        return customerNumber;
    }

    public void setCustomerNumber(Long customerNumber) {
        this.customerNumber = customerNumber;
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

    public Integer getOverdraftLimit() {
        return overdraftLimit;
    }

    public void setOverdraftLimit(Integer overdraftLimit) {
        this.overdraftLimit = overdraftLimit;
    }
}
