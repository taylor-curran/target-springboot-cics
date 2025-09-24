package com.cbsa.migration.dto;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for CreditScoreRequestDto
 * Tests DTO structure and validation
 */
class CreditScoreRequestDtoTest {

    @Test
    @DisplayName("Should create CreditScoreRequestDto with builder")
    void shouldCreateWithBuilder() {
        // When
        CreditScoreRequestDto dto = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .currentCreditScore(750)
                .build();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getSortCode()).isEqualTo("987654");
        assertThat(dto.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(dto.getName()).isEqualTo("John Doe");
        assertThat(dto.getAddress()).isEqualTo("123 Main St");
        assertThat(dto.getDateOfBirth()).isEqualTo(LocalDate.of(1990, 5, 15));
        assertThat(dto.getCurrentCreditScore()).isEqualTo(750);
    }

    @Test
    @DisplayName("Should create empty CreditScoreRequestDto")
    void shouldCreateEmpty() {
        // When
        CreditScoreRequestDto dto = new CreditScoreRequestDto();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getSortCode()).isNull();
        assertThat(dto.getCustomerNumber()).isNull();
        assertThat(dto.getName()).isNull();
        assertThat(dto.getAddress()).isNull();
        assertThat(dto.getDateOfBirth()).isNull();
        assertThat(dto.getCurrentCreditScore()).isNull();
    }

    @Test
    @DisplayName("Should support setter methods")
    void shouldSupportSetters() {
        // Given
        CreditScoreRequestDto dto = new CreditScoreRequestDto();

        // When
        dto.setSortCode("123456");
        dto.setCustomerNumber(9876543210L);
        dto.setName("Jane Smith");
        dto.setAddress("456 Oak Ave");
        dto.setDateOfBirth(LocalDate.of(1985, 8, 20));
        dto.setCurrentCreditScore(680);

        // Then
        assertThat(dto.getSortCode()).isEqualTo("123456");
        assertThat(dto.getCustomerNumber()).isEqualTo(9876543210L);
        assertThat(dto.getName()).isEqualTo("Jane Smith");
        assertThat(dto.getAddress()).isEqualTo("456 Oak Ave");
        assertThat(dto.getDateOfBirth()).isEqualTo(LocalDate.of(1985, 8, 20));
        assertThat(dto.getCurrentCreditScore()).isEqualTo(680);
    }

    @Test
    @DisplayName("Should support equals and hashCode")
    void shouldSupportEqualsAndHashCode() {
        // Given
        CreditScoreRequestDto dto1 = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .currentCreditScore(750)
                .build();

        CreditScoreRequestDto dto2 = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .currentCreditScore(750)
                .build();

        // Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
    }

    @Test
    @DisplayName("Should support toString")
    void shouldSupportToString() {
        // Given
        CreditScoreRequestDto dto = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .currentCreditScore(750)
                .build();

        // When
        String toString = dto.toString();

        // Then
        assertThat(toString).isNotNull();
        assertThat(toString).contains("987654");
        assertThat(toString).contains("1234567890");
        assertThat(toString).contains("John Doe");
    }
}
