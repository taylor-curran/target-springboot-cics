package com.cbsa.migration.dto;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import java.math.BigDecimal;

public class AccountUpdateRequestDto {

    @NotNull(message = "Account type is required")
    @Size(min = 1, max = 8, message = "Account type must be between 1 and 8 characters")
    @Pattern(regexp = "^[A-Z][A-Z0-9\\s]*$", message = "Account type must start with a letter and contain only uppercase letters, numbers, and spaces")
    private String accountType;

    @NotNull(message = "Interest rate is required")
    private BigDecimal interestRate;

    @NotNull(message = "Overdraft limit is required")
    private Integer overdraftLimit;

    public AccountUpdateRequestDto() {}

    public AccountUpdateRequestDto(String accountType, BigDecimal interestRate, Integer overdraftLimit) {
        this.accountType = accountType;
        this.interestRate = interestRate;
        this.overdraftLimit = overdraftLimit;
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
