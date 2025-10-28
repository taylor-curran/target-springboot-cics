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

    @Test
    @DisplayName("Should create CustomerRequestDto with all args constructor")
    void shouldCreateWithAllArgsConstructor() {
        // When
        CustomerRequestDto dto = new CustomerRequestDto(
                "Jane Smith",
                "456 Oak Ave",
                LocalDate.of(1985, 8, 20),
                "123456",
                680
        );

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getName()).isEqualTo("Jane Smith");
        assertThat(dto.getAddress()).isEqualTo("456 Oak Ave");
        assertThat(dto.getDateOfBirth()).isEqualTo(LocalDate.of(1985, 8, 20));
        assertThat(dto.getSortCode()).isEqualTo("123456");
        assertThat(dto.getCreditScore()).isEqualTo(680);
    }

    @Test
    @DisplayName("Should support partial builder usage")
    void shouldSupportPartialBuilder() {
        // When
        CustomerRequestDto dto = CustomerRequestDto.builder()
                .name("Partial User")
                .sortCode("111111")
                .build();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getName()).isEqualTo("Partial User");
        assertThat(dto.getSortCode()).isEqualTo("111111");
        assertThat(dto.getAddress()).isNull();
        assertThat(dto.getDateOfBirth()).isNull();
        assertThat(dto.getCreditScore()).isNull();
    }

    @Test
    @DisplayName("Should support builder method chaining")
    void shouldSupportBuilderMethodChaining() {
        // When
        CustomerRequestDto dto = CustomerRequestDto.builder()
                .name("Chain User")
                .address("Chain Address")
                .dateOfBirth(LocalDate.of(2000, 1, 1))
                .sortCode("222222")
                .creditScore(800)
                .build();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getName()).isEqualTo("Chain User");
        assertThat(dto.getAddress()).isEqualTo("Chain Address");
        assertThat(dto.getDateOfBirth()).isEqualTo(LocalDate.of(2000, 1, 1));
        assertThat(dto.getSortCode()).isEqualTo("222222");
        assertThat(dto.getCreditScore()).isEqualTo(800);
    }

    @Test
    @DisplayName("Should handle null values in equals comparison")
    void shouldHandleNullValuesInEquals() {
        // Given
        CustomerRequestDto dto1 = new CustomerRequestDto();
        CustomerRequestDto dto2 = new CustomerRequestDto();

        // Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
    }

    @Test
    @DisplayName("Should handle different objects in equals comparison")
    void shouldHandleDifferentObjectsInEquals() {
        // Given
        CustomerRequestDto dto1 = CustomerRequestDto.builder()
                .name("User1")
                .sortCode("111111")
                .build();

        CustomerRequestDto dto2 = CustomerRequestDto.builder()
                .name("User2")
                .sortCode("222222")
                .build();

        // Then
        assertThat(dto1).isNotEqualTo(dto2);
        assertThat(dto1.hashCode()).isNotEqualTo(dto2.hashCode());
    }

    @Test
    @DisplayName("Should handle toString with null values")
    void shouldHandleToStringWithNullValues() {
        // Given
        CustomerRequestDto dto = new CustomerRequestDto();

        // When
        String toString = dto.toString();

        // Then
        assertThat(toString).isNotNull();
        assertThat(toString).contains("CustomerRequestDto");
    }
}
