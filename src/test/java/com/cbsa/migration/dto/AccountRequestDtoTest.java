package com.cbsa.migration.dto;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for AccountRequestDto
 * Tests DTO structure and validation
 */
class AccountRequestDtoTest {

    @Test
    @DisplayName("Should create AccountRequestDto with builder")
    void shouldCreateWithBuilder() {
        // When
        AccountRequestDto dto = AccountRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .initialDeposit(new BigDecimal("500.00"))
                .build();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getSortCode()).isEqualTo("987654");
        assertThat(dto.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(dto.getAccountType()).isEqualTo("CURRENT");
        assertThat(dto.getInterestRate()).isEqualByComparingTo(new BigDecimal("0.10"));
        assertThat(dto.getOverdraftLimit()).isEqualTo(1000);
        assertThat(dto.getInitialDeposit()).isEqualByComparingTo(new BigDecimal("500.00"));
    }

    @Test
    @DisplayName("Should create empty AccountRequestDto")
    void shouldCreateEmpty() {
        // When
        AccountRequestDto dto = new AccountRequestDto();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getSortCode()).isNull();
        assertThat(dto.getCustomerNumber()).isNull();
        assertThat(dto.getAccountType()).isNull();
        assertThat(dto.getInterestRate()).isNull();
        assertThat(dto.getOverdraftLimit()).isNull();
        assertThat(dto.getInitialDeposit()).isNull();
    }

    @Test
    @DisplayName("Should support setter methods")
    void shouldSupportSetters() {
        // Given
        AccountRequestDto dto = new AccountRequestDto();

        // When
        dto.setSortCode("123456");
        dto.setCustomerNumber(9876543210L);
        dto.setAccountType("SAVINGS");
        dto.setInterestRate(new BigDecimal("2.50"));
        dto.setOverdraftLimit(500);
        dto.setInitialDeposit(new BigDecimal("1000.00"));

        // Then
        assertThat(dto.getSortCode()).isEqualTo("123456");
        assertThat(dto.getCustomerNumber()).isEqualTo(9876543210L);
        assertThat(dto.getAccountType()).isEqualTo("SAVINGS");
        assertThat(dto.getInterestRate()).isEqualByComparingTo(new BigDecimal("2.50"));
        assertThat(dto.getOverdraftLimit()).isEqualTo(500);
        assertThat(dto.getInitialDeposit()).isEqualByComparingTo(new BigDecimal("1000.00"));
    }

    @Test
    @DisplayName("Should support equals and hashCode")
    void shouldSupportEqualsAndHashCode() {
        // Given
        AccountRequestDto dto1 = AccountRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .initialDeposit(new BigDecimal("500.00"))
                .build();

        AccountRequestDto dto2 = AccountRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .initialDeposit(new BigDecimal("500.00"))
                .build();

        // Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
    }

    @Test
    @DisplayName("Should support toString")
    void shouldSupportToString() {
        // Given
        AccountRequestDto dto = AccountRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .initialDeposit(new BigDecimal("500.00"))
                .build();

        // When
        String toString = dto.toString();

        // Then
        assertThat(toString).isNotNull();
        assertThat(toString).contains("987654");
        assertThat(toString).contains("1234567890");
        assertThat(toString).contains("CURRENT");
    }
}
