package com.cbsa.migration.controller;

import com.cbsa.migration.service.CompanyInfoService;
import com.cbsa.migration.service.SortCodeService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Unit tests for UtilityController
 * Tests REST API endpoints for utility functions migrated from COBOL
 */
@WebMvcTest(UtilityController.class)
class UtilityControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private SortCodeService sortCodeService;

    @MockBean
    private CompanyInfoService companyInfoService;

    @Test
    @DisplayName("Should return sort code successfully")
    void shouldReturnSortCodeSuccessfully() throws Exception {
        // Given
        when(sortCodeService.getSortCode()).thenReturn("987654");

        // When & Then
        mockMvc.perform(get("/api/utility/sortcode"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.sortCode").value("987654"));
    }

    @Test
    @DisplayName("Should return company name successfully")
    void shouldReturnCompanyNameSuccessfully() throws Exception {
        // Given
        when(companyInfoService.getCompanyName()).thenReturn("CBSA Bank Ltd");

        // When & Then
        mockMvc.perform(get("/api/utility/company-name"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.companyName").value("CBSA Bank Ltd"));
    }

    @Test
    @DisplayName("Should handle different sort codes")
    void shouldHandleDifferentSortCodes() throws Exception {
        // Given
        when(sortCodeService.getSortCode()).thenReturn("123456");

        // When & Then
        mockMvc.perform(get("/api/utility/sortcode"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.sortCode").value("123456"));
    }

    @Test
    @DisplayName("Should handle different company names")
    void shouldHandleDifferentCompanyNames() throws Exception {
        // Given
        when(companyInfoService.getCompanyName()).thenReturn("Test Bank Corporation");

        // When & Then
        mockMvc.perform(get("/api/utility/company-name"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.companyName").value("Test Bank Corporation"));
    }

    @Test
    @DisplayName("Should handle empty sort code")
    void shouldHandleEmptySortCode() throws Exception {
        // Given
        when(sortCodeService.getSortCode()).thenReturn("");

        // When & Then
        mockMvc.perform(get("/api/utility/sortcode"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.sortCode").value(""));
    }

    @Test
    @DisplayName("Should handle empty company name")
    void shouldHandleEmptyCompanyName() throws Exception {
        // Given
        when(companyInfoService.getCompanyName()).thenReturn("");

        // When & Then
        mockMvc.perform(get("/api/utility/company-name"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.companyName").value(""));
    }
}
