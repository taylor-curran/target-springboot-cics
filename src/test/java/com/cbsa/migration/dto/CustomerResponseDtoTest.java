package com.cbsa.migration.dto;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for CustomerResponseDto
 * Tests DTO structure and validation
 */
class CustomerResponseDtoTest {

    @Test
    @DisplayName("Should create CustomerResponseDto with builder")
    void shouldCreateWithBuilder() {
        // Given
        List<AccountSummaryDto> accounts = Arrays.asList(
                AccountSummaryDto.builder()
                        .accountNumber("12345678")
                        .accountType("CURRENT")
                        .build(),
                AccountSummaryDto.builder()
                        .accountNumber("87654321")
                        .accountType("SAVINGS")
                        .build()
        );

        // When
        CustomerResponseDto dto = CustomerResponseDto.builder()
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accounts(accounts)
                .accountCount(2)
                .status("GOOD_STANDING")
                .build();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getName()).isEqualTo("John Doe");
        assertThat(dto.getAddress()).isEqualTo("123 Main St");
        assertThat(dto.getDateOfBirth()).isEqualTo(LocalDate.of(1990, 5, 15));
        assertThat(dto.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(dto.getSortCode()).isEqualTo("987654");
        assertThat(dto.getAccounts()).hasSize(2);
        assertThat(dto.getAccountCount()).isEqualTo(2);
        assertThat(dto.getStatus()).isEqualTo("GOOD_STANDING");
    }

    @Test
    @DisplayName("Should create empty CustomerResponseDto")
    void shouldCreateEmpty() {
        // When
        CustomerResponseDto dto = new CustomerResponseDto();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getName()).isNull();
        assertThat(dto.getAddress()).isNull();
        assertThat(dto.getDateOfBirth()).isNull();
        assertThat(dto.getCustomerNumber()).isNull();
        assertThat(dto.getSortCode()).isNull();
        assertThat(dto.getAccounts()).isNull();
        assertThat(dto.getAccountCount()).isNull();
        assertThat(dto.getStatus()).isNull();
    }

    @Test
    @DisplayName("Should support setter methods")
    void shouldSupportSetters() {
        // Given
        CustomerResponseDto dto = new CustomerResponseDto();
        List<AccountSummaryDto> accounts = Arrays.asList(
                AccountSummaryDto.builder()
                        .accountNumber("11111111")
                        .accountType("CURRENT")
                        .build()
        );

        // When
        dto.setName("Jane Smith");
        dto.setAddress("456 Oak Ave");
        dto.setDateOfBirth(LocalDate.of(1985, 8, 20));
        dto.setCustomerNumber(9876543210L);
        dto.setSortCode("123456");
        dto.setAccounts(accounts);
        dto.setAccountCount(1);
        dto.setStatus("FAIR");

        // Then
        assertThat(dto.getName()).isEqualTo("Jane Smith");
        assertThat(dto.getAddress()).isEqualTo("456 Oak Ave");
        assertThat(dto.getDateOfBirth()).isEqualTo(LocalDate.of(1985, 8, 20));
        assertThat(dto.getCustomerNumber()).isEqualTo(9876543210L);
        assertThat(dto.getSortCode()).isEqualTo("123456");
        assertThat(dto.getAccounts()).hasSize(1);
        assertThat(dto.getAccountCount()).isEqualTo(1);
        assertThat(dto.getStatus()).isEqualTo("FAIR");
    }

    @Test
    @DisplayName("Should support equals and hashCode")
    void shouldSupportEqualsAndHashCode() {
        // Given
        CustomerResponseDto dto1 = CustomerResponseDto.builder()
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountCount(2)
                .status("GOOD_STANDING")
                .build();

        CustomerResponseDto dto2 = CustomerResponseDto.builder()
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountCount(2)
                .status("GOOD_STANDING")
                .build();

        // Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
    }

    @Test
    @DisplayName("Should support toString")
    void shouldSupportToString() {
        // Given
        CustomerResponseDto dto = CustomerResponseDto.builder()
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountCount(2)
                .status("GOOD_STANDING")
                .build();

        // When
        String toString = dto.toString();

        // Then
        assertThat(toString).isNotNull();
        assertThat(toString).contains("John Doe");
        assertThat(toString).contains("123 Main St");
        assertThat(toString).contains("1234567890");
        assertThat(toString).contains("987654");
        assertThat(toString).contains("GOOD_STANDING");
    }
}
