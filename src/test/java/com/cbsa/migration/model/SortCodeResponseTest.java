package com.cbsa.migration.model;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for SortCodeResponse model
 * Tests the COBOL GETSCODE copybook structure migration
 */
class SortCodeResponseTest {

    @Test
    @DisplayName("Should create SortCodeResponse with no-args constructor")
    void testNoArgsConstructor_createsValidObject() {
        // When
        SortCodeResponse response = new SortCodeResponse();
        
        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSortCode()).isNull();
    }

    @Test
    @DisplayName("Should create SortCodeResponse with parameterized constructor")
    void testParameterizedConstructor_setsProperty() {
        // Given
        String sortCode = "123456";
        
        // When
        SortCodeResponse response = new SortCodeResponse(sortCode);
        
        // Then
        assertThat(response.getSortCode()).isEqualTo(sortCode);
    }

    @Test
    @DisplayName("Should set and get sort code")
    void testSetterAndGetter_workCorrectly() {
        // Given
        SortCodeResponse response = new SortCodeResponse();
        String sortCode = "987654";
        
        // When
        response.setSortCode(sortCode);
        
        // Then
        assertThat(response.getSortCode()).isEqualTo(sortCode);
    }

    @Test
    @DisplayName("Should handle null sort code")
    void testNullSortCode_handledCorrectly() {
        // Given
        SortCodeResponse response = new SortCodeResponse();
        
        // When
        response.setSortCode(null);
        
        // Then
        assertThat(response.getSortCode()).isNull();
    }

    @Test
    @DisplayName("Should handle empty sort code")
    void testEmptySortCode_handledCorrectly() {
        // Given
        SortCodeResponse response = new SortCodeResponse();
        String emptySortCode = "";
        
        // When
        response.setSortCode(emptySortCode);
        
        // Then
        assertThat(response.getSortCode()).isEqualTo(emptySortCode);
        assertThat(response.getSortCode()).isEmpty();
    }

    @Test
    @DisplayName("Should handle various sort code formats")
    void testVariousSortCodeFormats_handledCorrectly() {
        // Given
        SortCodeResponse response = new SortCodeResponse();
        
        response.setSortCode("123456");
        assertThat(response.getSortCode()).isEqualTo("123456");
        
        response.setSortCode("ABC123");
        assertThat(response.getSortCode()).isEqualTo("ABC123");
        
        response.setSortCode("12-34-56");
        assertThat(response.getSortCode()).isEqualTo("12-34-56");
        
        response.setSortCode("1234567890ABCDEF");
        assertThat(response.getSortCode()).isEqualTo("1234567890ABCDEF");
    }

    @Test
    @DisplayName("Should create with null parameter in constructor")
    void testConstructorWithNull_handledCorrectly() {
        // When
        SortCodeResponse response = new SortCodeResponse(null);
        
        // Then
        assertThat(response.getSortCode()).isNull();
    }

    @Test
    @DisplayName("Should override sort code value")
    void testOverrideSortCode_updatesCorrectly() {
        // Given
        SortCodeResponse response = new SortCodeResponse("123456");
        
        // When
        response.setSortCode("987654");
        
        // Then
        assertThat(response.getSortCode()).isEqualTo("987654");
    }

    @Test
    @DisplayName("Should handle whitespace in sort code")
    void testWhitespaceInSortCode_preservedCorrectly() {
        // Given
        SortCodeResponse response = new SortCodeResponse();
        String sortCodeWithSpaces = " 12 34 56 ";
        
        // When
        response.setSortCode(sortCodeWithSpaces);
        
        // Then
        assertThat(response.getSortCode()).isEqualTo(sortCodeWithSpaces);
        assertThat(response.getSortCode()).contains(" ");
    }

    @Test
    @DisplayName("Should test object state consistency")
    void testObjectStateConsistency() {
        // Given
        String sortCode = "123456";
        SortCodeResponse response1 = new SortCodeResponse(sortCode);
        SortCodeResponse response2 = new SortCodeResponse();
        response2.setSortCode(sortCode);
        
        // Then
        assertThat(response1.getSortCode()).isEqualTo(response2.getSortCode());
    }
}
