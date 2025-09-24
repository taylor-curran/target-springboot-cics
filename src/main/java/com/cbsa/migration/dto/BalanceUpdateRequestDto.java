package com.cbsa.migration.dto;

import javax.validation.constraints.NotNull;

import java.math.BigDecimal;

public class BalanceUpdateRequestDto {

    @NotNull(message = "Amount is required")
    private BigDecimal amount;

    private String applicationId;
    private String userId;
    private String facilityName;
    private String networkId;
    private Integer facilityType;

    public BalanceUpdateRequestDto() {}

    public BalanceUpdateRequestDto(BigDecimal amount) {
        this.amount = amount;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public String getApplicationId() {
        return applicationId;
    }

    public void setApplicationId(String applicationId) {
        this.applicationId = applicationId;
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getFacilityName() {
        return facilityName;
    }

    public void setFacilityName(String facilityName) {
        this.facilityName = facilityName;
    }

    public String getNetworkId() {
        return networkId;
    }

    public void setNetworkId(String networkId) {
        this.networkId = networkId;
    }

    public Integer getFacilityType() {
        return facilityType;
    }

    public void setFacilityType(Integer facilityType) {
        this.facilityType = facilityType;
    }
}
