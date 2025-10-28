package com.cbsa.migration.model;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for CompanyInfoResponse model
 * Tests the COBOL GETCOMPY copybook structure migration
 */
class CompanyInfoResponseTest {

    @Test
    @DisplayName("Should create CompanyInfoResponse with no-args constructor")
    void testNoArgsConstructor_createsValidObject() {
        // When
        CompanyInfoResponse response = new CompanyInfoResponse();
        
        // Then
        assertThat(response).isNotNull();
        assertThat(response.getCompanyName()).isNull();
    }

    @Test
    @DisplayName("Should create CompanyInfoResponse with parameterized constructor")
    void testParameterizedConstructor_setsProperty() {
        // Given
        String companyName = "CBSA Banking Corporation";
        
        // When
        CompanyInfoResponse response = new CompanyInfoResponse(companyName);
        
        // Then
        assertThat(response.getCompanyName()).isEqualTo(companyName);
    }

    @Test
    @DisplayName("Should set and get company name")
    void testSetterAndGetter_workCorrectly() {
        // Given
        CompanyInfoResponse response = new CompanyInfoResponse();
        String companyName = "Test Banking Ltd";
        
        // When
        response.setCompanyName(companyName);
        
        // Then
        assertThat(response.getCompanyName()).isEqualTo(companyName);
    }

    @Test
    @DisplayName("Should handle null company name")
    void testNullCompanyName_handledCorrectly() {
        // Given
        CompanyInfoResponse response = new CompanyInfoResponse();
        
        // When
        response.setCompanyName(null);
        
        // Then
        assertThat(response.getCompanyName()).isNull();
    }

    @Test
    @DisplayName("Should handle empty company name")
    void testEmptyCompanyName_handledCorrectly() {
        // Given
        CompanyInfoResponse response = new CompanyInfoResponse();
        String emptyCompanyName = "";
        
        // When
        response.setCompanyName(emptyCompanyName);
        
        // Then
        assertThat(response.getCompanyName()).isEqualTo(emptyCompanyName);
        assertThat(response.getCompanyName()).isEmpty();
    }

    @Test
    @DisplayName("Should handle various company name formats")
    void testVariousCompanyNameFormats_handledCorrectly() {
        // Given
        CompanyInfoResponse response = new CompanyInfoResponse();
        
        response.setCompanyName("CBSA Bank");
        assertThat(response.getCompanyName()).isEqualTo("CBSA Bank");
        
        response.setCompanyName("CBSA Banking & Finance Corp.");
        assertThat(response.getCompanyName()).isEqualTo("CBSA Banking & Finance Corp.");
        
        response.setCompanyName("The Central Banking and Savings Association International Corporation Limited");
        assertThat(response.getCompanyName()).isEqualTo("The Central Banking and Savings Association International Corporation Limited");
        
        response.setCompanyName("Bank123 Financial Services");
        assertThat(response.getCompanyName()).isEqualTo("Bank123 Financial Services");
    }

    @Test
    @DisplayName("Should create with null parameter in constructor")
    void testConstructorWithNull_handledCorrectly() {
        // When
        CompanyInfoResponse response = new CompanyInfoResponse(null);
        
        // Then
        assertThat(response.getCompanyName()).isNull();
    }

    @Test
    @DisplayName("Should override company name value")
    void testOverrideCompanyName_updatesCorrectly() {
        // Given
        CompanyInfoResponse response = new CompanyInfoResponse("Original Bank");
        
        // When
        response.setCompanyName("Updated Bank");
        
        // Then
        assertThat(response.getCompanyName()).isEqualTo("Updated Bank");
    }

    @Test
    @DisplayName("Should handle whitespace in company name")
    void testWhitespaceInCompanyName_preservedCorrectly() {
        // Given
        CompanyInfoResponse response = new CompanyInfoResponse();
        String companyNameWithSpaces = "  CBSA Banking Corporation  ";
        
        // When
        response.setCompanyName(companyNameWithSpaces);
        
        // Then
        assertThat(response.getCompanyName()).isEqualTo(companyNameWithSpaces);
        assertThat(response.getCompanyName()).startsWith("  ");
        assertThat(response.getCompanyName()).endsWith("  ");
    }

    @Test
    @DisplayName("Should test object state consistency")
    void testObjectStateConsistency() {
        // Given
        String companyName = "CBSA Banking Corporation";
        CompanyInfoResponse response1 = new CompanyInfoResponse(companyName);
        CompanyInfoResponse response2 = new CompanyInfoResponse();
        response2.setCompanyName(companyName);
        
        // Then
        assertThat(response1.getCompanyName()).isEqualTo(response2.getCompanyName());
    }

    @Test
    @DisplayName("Should handle Unicode characters in company name")
    void testUnicodeCharacters_handledCorrectly() {
        // Given
        CompanyInfoResponse response = new CompanyInfoResponse();
        String unicodeCompanyName = "CBSA Banking™ Corporation® Ltd.";
        
        // When
        response.setCompanyName(unicodeCompanyName);
        
        // Then
        assertThat(response.getCompanyName()).isEqualTo(unicodeCompanyName);
        assertThat(response.getCompanyName()).contains("™");
        assertThat(response.getCompanyName()).contains("®");
    }
}
