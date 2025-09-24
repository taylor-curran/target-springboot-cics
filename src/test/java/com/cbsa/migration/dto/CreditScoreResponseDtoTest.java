package com.cbsa.migration.dto;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for CreditScoreResponseDto
 * Tests DTO structure and validation
 */
class CreditScoreResponseDtoTest {

    @Test
    @DisplayName("Should create CreditScoreResponseDto with builder")
    void shouldCreateWithBuilder() {
        // When
        CreditScoreResponseDto dto = CreditScoreResponseDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .success(true)
                .updatedCreditScore(780)
                .scoreReviewDate(LocalDate.of(2023, 6, 15))
                .processingTimeMs(150L)
                .errorMessage("Credit score updated successfully")
                .build();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getSortCode()).isEqualTo("987654");
        assertThat(dto.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(dto.getSuccess()).isTrue();
        assertThat(dto.getUpdatedCreditScore()).isEqualTo(780);
        assertThat(dto.getScoreReviewDate()).isEqualTo(LocalDate.of(2023, 6, 15));
        assertThat(dto.getProcessingTimeMs()).isEqualTo(150L);
        assertThat(dto.getErrorMessage()).isEqualTo("Credit score updated successfully");
    }

    @Test
    @DisplayName("Should create empty CreditScoreResponseDto")
    void shouldCreateEmpty() {
        // When
        CreditScoreResponseDto dto = new CreditScoreResponseDto();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getSortCode()).isNull();
        assertThat(dto.getCustomerNumber()).isNull();
        assertThat(dto.getSuccess()).isNull();
        assertThat(dto.getUpdatedCreditScore()).isNull();
        assertThat(dto.getScoreReviewDate()).isNull();
        assertThat(dto.getProcessingTimeMs()).isNull();
        assertThat(dto.getErrorMessage()).isNull();
    }

    @Test
    @DisplayName("Should support setter methods")
    void shouldSupportSetters() {
        // Given
        CreditScoreResponseDto dto = new CreditScoreResponseDto();

        // When
        dto.setSortCode("123456");
        dto.setCustomerNumber(9876543210L);
        dto.setSuccess(false);
        dto.setUpdatedCreditScore(650);
        dto.setScoreReviewDate(LocalDate.of(2023, 7, 1));
        dto.setProcessingTimeMs(200L);
        dto.setErrorMessage("Credit score review failed");

        // Then
        assertThat(dto.getSortCode()).isEqualTo("123456");
        assertThat(dto.getCustomerNumber()).isEqualTo(9876543210L);
        assertThat(dto.getSuccess()).isFalse();
        assertThat(dto.getUpdatedCreditScore()).isEqualTo(650);
        assertThat(dto.getScoreReviewDate()).isEqualTo(LocalDate.of(2023, 7, 1));
        assertThat(dto.getProcessingTimeMs()).isEqualTo(200L);
        assertThat(dto.getErrorMessage()).isEqualTo("Credit score review failed");
    }

    @Test
    @DisplayName("Should support equals and hashCode")
    void shouldSupportEqualsAndHashCode() {
        // Given
        CreditScoreResponseDto dto1 = CreditScoreResponseDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .success(true)
                .updatedCreditScore(780)
                .scoreReviewDate(LocalDate.of(2023, 6, 15))
                .processingTimeMs(150L)
                .errorMessage("Credit score updated successfully")
                .build();

        CreditScoreResponseDto dto2 = CreditScoreResponseDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .success(true)
                .updatedCreditScore(780)
                .scoreReviewDate(LocalDate.of(2023, 6, 15))
                .processingTimeMs(150L)
                .errorMessage("Credit score updated successfully")
                .build();

        // Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
    }

    @Test
    @DisplayName("Should support toString")
    void shouldSupportToString() {
        // Given
        CreditScoreResponseDto dto = CreditScoreResponseDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .success(true)
                .updatedCreditScore(780)
                .scoreReviewDate(LocalDate.of(2023, 6, 15))
                .processingTimeMs(150L)
                .errorMessage("Credit score updated successfully")
                .build();

        // When
        String toString = dto.toString();

        // Then
        assertThat(toString).isNotNull();
        assertThat(toString).contains("987654");
        assertThat(toString).contains("1234567890");
        assertThat(toString).contains("true");
        assertThat(toString).contains("780");
        assertThat(toString).contains("Credit score updated successfully");
    }

    @Test
    @DisplayName("Should create CreditScoreResponseDto with all args constructor")
    void shouldCreateWithAllArgsConstructor() {
        // When
        CreditScoreResponseDto dto = new CreditScoreResponseDto(
                "654321",
                9876543210L,
                650,
                LocalDate.of(2023, 7, 1),
                200L,
                false,
                "Credit score review failed"
        );

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getSortCode()).isEqualTo("654321");
        assertThat(dto.getCustomerNumber()).isEqualTo(9876543210L);
        assertThat(dto.getSuccess()).isFalse();
        assertThat(dto.getUpdatedCreditScore()).isEqualTo(650);
        assertThat(dto.getScoreReviewDate()).isEqualTo(LocalDate.of(2023, 7, 1));
        assertThat(dto.getProcessingTimeMs()).isEqualTo(200L);
        assertThat(dto.getErrorMessage()).isEqualTo("Credit score review failed");
    }

    @Test
    @DisplayName("Should support partial builder usage")
    void shouldSupportPartialBuilder() {
        // When
        CreditScoreResponseDto dto = CreditScoreResponseDto.builder()
                .sortCode("111111")
                .customerNumber(1111111111L)
                .success(true)
                .build();

        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getSortCode()).isEqualTo("111111");
        assertThat(dto.getCustomerNumber()).isEqualTo(1111111111L);
        assertThat(dto.getSuccess()).isTrue();
        assertThat(dto.getUpdatedCreditScore()).isNull();
        assertThat(dto.getScoreReviewDate()).isNull();
        assertThat(dto.getProcessingTimeMs()).isNull();
        assertThat(dto.getErrorMessage()).isNull();
    }

    @Test
    @DisplayName("Should handle null values in equals comparison")
    void shouldHandleNullValuesInEquals() {
        // Given
        CreditScoreResponseDto dto1 = new CreditScoreResponseDto();
        CreditScoreResponseDto dto2 = new CreditScoreResponseDto();

        // Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
    }

    @Test
    @DisplayName("Should handle different objects in equals comparison")
    void shouldHandleDifferentObjectsInEquals() {
        // Given
        CreditScoreResponseDto dto1 = CreditScoreResponseDto.builder()
                .sortCode("111111")
                .customerNumber(1111111111L)
                .success(true)
                .build();

        CreditScoreResponseDto dto2 = CreditScoreResponseDto.builder()
                .sortCode("222222")
                .customerNumber(2222222222L)
                .success(false)
                .build();

        // Then
        assertThat(dto1).isNotEqualTo(dto2);
        assertThat(dto1.hashCode()).isNotEqualTo(dto2.hashCode());
    }

    @Test
    @DisplayName("Should handle toString with null values")
    void shouldHandleToStringWithNullValues() {
        // Given
        CreditScoreResponseDto dto = new CreditScoreResponseDto();

        // When
        String toString = dto.toString();

        // Then
        assertThat(toString).isNotNull();
        assertThat(toString).contains("CreditScoreResponseDto");
    }

    @Test
    @DisplayName("Should handle boolean values correctly")
    void shouldHandleBooleanValuesCorrectly() {
        // Given
        CreditScoreResponseDto successDto = CreditScoreResponseDto.builder()
                .success(true)
                .build();

        CreditScoreResponseDto failureDto = CreditScoreResponseDto.builder()
                .success(false)
                .build();

        // Then
        assertThat(successDto.getSuccess()).isTrue();
        assertThat(failureDto.getSuccess()).isFalse();
        assertThat(successDto).isNotEqualTo(failureDto);
    }
}
