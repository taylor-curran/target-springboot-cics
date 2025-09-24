package com.cbsa.migration.dto;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for CustomerRequestDto
 * Tests DTO structure and validation
 */
class CustomerRequestDtoTest {

    @Test
    @DisplayName("Should create CustomerRequestDto with builder")
    void shouldCreateWithBuilder() {
        // When
        CustomerRequestDto dto = CustomerRequestDto.builder()
                .sortCode("987654")
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .creditScore(750)
                .build();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getSortCode()).isEqualTo("987654");
        assertThat(dto.getName()).isEqualTo("John Doe");
        assertThat(dto.getAddress()).isEqualTo("123 Main St");
        assertThat(dto.getDateOfBirth()).isEqualTo(LocalDate.of(1990, 5, 15));
        assertThat(dto.getCreditScore()).isEqualTo(750);
    }

    @Test
    @DisplayName("Should create empty CustomerRequestDto")
    void shouldCreateEmpty() {
        // When
        CustomerRequestDto dto = new CustomerRequestDto();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getSortCode()).isNull();
        assertThat(dto.getName()).isNull();
        assertThat(dto.getAddress()).isNull();
        assertThat(dto.getDateOfBirth()).isNull();
        assertThat(dto.getCreditScore()).isNull();
    }

    @Test
    @DisplayName("Should support setter methods")
    void shouldSupportSetters() {
        // Given
        CustomerRequestDto dto = new CustomerRequestDto();

        // When
        dto.setSortCode("123456");
        dto.setName("Jane Smith");
        dto.setAddress("456 Oak Ave");
        dto.setDateOfBirth(LocalDate.of(1985, 8, 20));
        dto.setCreditScore(680);

        // Then
        assertThat(dto.getSortCode()).isEqualTo("123456");
        assertThat(dto.getName()).isEqualTo("Jane Smith");
        assertThat(dto.getAddress()).isEqualTo("456 Oak Ave");
        assertThat(dto.getDateOfBirth()).isEqualTo(LocalDate.of(1985, 8, 20));
        assertThat(dto.getCreditScore()).isEqualTo(680);
    }

    @Test
    @DisplayName("Should support equals and hashCode")
    void shouldSupportEqualsAndHashCode() {
        // Given
        CustomerRequestDto dto1 = CustomerRequestDto.builder()
                .sortCode("987654")
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .creditScore(750)
                .build();

        CustomerRequestDto dto2 = CustomerRequestDto.builder()
                .sortCode("987654")
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .creditScore(750)
                .build();

        // Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
    }

    @Test
    @DisplayName("Should support toString")
    void shouldSupportToString() {
        // Given
        CustomerRequestDto dto = CustomerRequestDto.builder()
                .sortCode("987654")
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .creditScore(750)
                .build();

        // When
        String toString = dto.toString();

        // Then
        assertThat(toString).isNotNull();
        assertThat(toString).contains("987654");
        assertThat(toString).contains("John Doe");
        assertThat(toString).contains("123 Main St");
    }
}
