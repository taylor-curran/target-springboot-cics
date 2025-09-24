package com.cbsa.migration.dto;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for AccountSummaryDto
 * Tests DTO structure and validation
 */
class AccountSummaryDtoTest {

    @Test
    @DisplayName("Should create AccountSummaryDto with builder")
    void shouldCreateWithBuilder() {
        // When
        AccountSummaryDto dto = AccountSummaryDto.builder()
                .accountNumber("12345678")
                .accountType("CURRENT")
                .availableBalance(new BigDecimal("1500.00"))
                .actualBalance(new BigDecimal("1750.00"))
                .status("ACTIVE")
                .build();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getAccountNumber()).isEqualTo("12345678");
        assertThat(dto.getAccountType()).isEqualTo("CURRENT");
        assertThat(dto.getAvailableBalance()).isEqualByComparingTo(new BigDecimal("1500.00"));
        assertThat(dto.getActualBalance()).isEqualByComparingTo(new BigDecimal("1750.00"));
        assertThat(dto.getStatus()).isEqualTo("ACTIVE");
    }

    @Test
    @DisplayName("Should create empty AccountSummaryDto")
    void shouldCreateEmpty() {
        // When
        AccountSummaryDto dto = new AccountSummaryDto();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getAccountNumber()).isNull();
        assertThat(dto.getAccountType()).isNull();
        assertThat(dto.getAvailableBalance()).isNull();
        assertThat(dto.getActualBalance()).isNull();
        assertThat(dto.getStatus()).isNull();
    }

    @Test
    @DisplayName("Should support setter methods")
    void shouldSupportSetters() {
        // Given
        AccountSummaryDto dto = new AccountSummaryDto();

        // When
        dto.setAccountNumber("87654321");
        dto.setAccountType("SAVINGS");
        dto.setAvailableBalance(new BigDecimal("2000.00"));
        dto.setActualBalance(new BigDecimal("2000.00"));
        dto.setStatus("ACTIVE");

        // Then
        assertThat(dto.getAccountNumber()).isEqualTo("87654321");
        assertThat(dto.getAccountType()).isEqualTo("SAVINGS");
        assertThat(dto.getAvailableBalance()).isEqualByComparingTo(new BigDecimal("2000.00"));
        assertThat(dto.getActualBalance()).isEqualByComparingTo(new BigDecimal("2000.00"));
        assertThat(dto.getStatus()).isEqualTo("ACTIVE");
    }

    @Test
    @DisplayName("Should support equals and hashCode")
    void shouldSupportEqualsAndHashCode() {
        // Given
        AccountSummaryDto dto1 = AccountSummaryDto.builder()
                .accountNumber("12345678")
                .accountType("CURRENT")
                .availableBalance(new BigDecimal("1500.00"))
                .actualBalance(new BigDecimal("1750.00"))
                .status("ACTIVE")
                .build();

        AccountSummaryDto dto2 = AccountSummaryDto.builder()
                .accountNumber("12345678")
                .accountType("CURRENT")
                .availableBalance(new BigDecimal("1500.00"))
                .actualBalance(new BigDecimal("1750.00"))
                .status("ACTIVE")
                .build();

        // Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
    }

    @Test
    @DisplayName("Should support toString")
    void shouldSupportToString() {
        // Given
        AccountSummaryDto dto = AccountSummaryDto.builder()
                .accountNumber("12345678")
                .accountType("CURRENT")
                .availableBalance(new BigDecimal("1500.00"))
                .actualBalance(new BigDecimal("1750.00"))
                .status("ACTIVE")
                .build();

        // When
        String toString = dto.toString();

        // Then
        assertThat(toString).isNotNull();
        assertThat(toString).contains("12345678");
        assertThat(toString).contains("CURRENT");
        assertThat(toString).contains("ACTIVE");
    }
}
