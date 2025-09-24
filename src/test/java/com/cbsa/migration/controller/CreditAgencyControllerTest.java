package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CreditScoreRequestDto;
import com.cbsa.migration.dto.CreditScoreResponseDto;
import com.cbsa.migration.service.CreditAgencyService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Unit tests for CreditAgencyController
 * Tests REST API endpoints for credit score processing
 */
@WebMvcTest(CreditAgencyController.class)
class CreditAgencyControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private CreditAgencyService creditAgencyService;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    @DisplayName("Should process credit score request successfully")
    void shouldProcessCreditSuccessfully() throws Exception {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(java.time.LocalDate.of(1990, 5, 15))
                .currentCreditScore(700)
                .build();

        CreditScoreResponseDto response = CreditScoreResponseDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .success(true)
                .updatedCreditScore(750)
                .processingTimeMs(150L)
                .build();

        when(creditAgencyService.processCredit(any(CreditScoreRequestDto.class)))
                .thenReturn(response);

        // When & Then
        mockMvc.perform(post("/api/credit-agency/score")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.sortCode").value("987654"))
                .andExpect(jsonPath("$.customerNumber").value(1234567890L))
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.updatedCreditScore").value(750))
                .andExpect(jsonPath("$.processingTimeMs").value(150L));
    }

    @Test
    @DisplayName("Should return NOT_FOUND when customer not found")
    void shouldReturnNotFoundWhenCustomerNotFound() throws Exception {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(9999999999L)
                .name("Unknown Customer")
                .address("Unknown Address")
                .dateOfBirth(java.time.LocalDate.of(1990, 1, 1))
                .currentCreditScore(600)
                .build();

        CreditScoreResponseDto response = CreditScoreResponseDto.builder()
                .sortCode("987654")
                .customerNumber(9999999999L)
                .success(false)
                .errorMessage("Customer not found")
                .processingTimeMs(0L)
                .build();

        when(creditAgencyService.processCredit(any(CreditScoreRequestDto.class)))
                .thenReturn(response);

        // When & Then
        mockMvc.perform(post("/api/credit-agency/score")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.errorMessage").value("Customer not found"));
    }

    @Test
    @DisplayName("Should return BAD_REQUEST when data mismatch occurs")
    void shouldReturnBadRequestWhenDataMismatch() throws Exception {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(java.time.LocalDate.of(1990, 5, 15))
                .currentCreditScore(700)
                .build();

        CreditScoreResponseDto response = CreditScoreResponseDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .success(false)
                .errorMessage("Customer data mismatch")
                .processingTimeMs(0L)
                .build();

        when(creditAgencyService.processCredit(any(CreditScoreRequestDto.class)))
                .thenReturn(response);

        // When & Then
        mockMvc.perform(post("/api/credit-agency/score")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.errorMessage").value("Customer data mismatch"));
    }

    @Test
    @DisplayName("Should return REQUEST_TIMEOUT when timeout occurs")
    void shouldReturnRequestTimeoutWhenTimeout() throws Exception {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(java.time.LocalDate.of(1990, 5, 15))
                .currentCreditScore(700)
                .build();

        CreditScoreResponseDto response = CreditScoreResponseDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .success(false)
                .errorMessage("Request timeout occurred")
                .processingTimeMs(0L)
                .build();

        when(creditAgencyService.processCredit(any(CreditScoreRequestDto.class)))
                .thenReturn(response);

        // When & Then
        mockMvc.perform(post("/api/credit-agency/score")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isRequestTimeout())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.errorMessage").value("Request timeout occurred"));
    }

    @Test
    @DisplayName("Should return INTERNAL_SERVER_ERROR when service throws exception")
    void shouldReturnInternalServerErrorWhenServiceThrowsException() throws Exception {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(java.time.LocalDate.of(1990, 5, 15))
                .currentCreditScore(700)
                .build();

        when(creditAgencyService.processCredit(any(CreditScoreRequestDto.class)))
                .thenThrow(new RuntimeException("Database connection failed"));

        // When & Then
        mockMvc.perform(post("/api/credit-agency/score")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.errorMessage").value("Internal server error"));
    }

    @Test
    @DisplayName("Should return INTERNAL_SERVER_ERROR for unknown error message")
    void shouldReturnInternalServerErrorForUnknownError() throws Exception {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(java.time.LocalDate.of(1990, 5, 15))
                .currentCreditScore(700)
                .build();

        CreditScoreResponseDto response = CreditScoreResponseDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .success(false)
                .errorMessage("Unknown processing error")
                .processingTimeMs(0L)
                .build();

        when(creditAgencyService.processCredit(any(CreditScoreRequestDto.class)))
                .thenReturn(response);

        // When & Then
        mockMvc.perform(post("/api/credit-agency/score")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.errorMessage").value("Unknown processing error"));
    }

    @Test
    @DisplayName("Should handle null error message")
    void shouldHandleNullErrorMessage() throws Exception {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(java.time.LocalDate.of(1990, 5, 15))
                .currentCreditScore(700)
                .build();

        CreditScoreResponseDto response = CreditScoreResponseDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .success(false)
                .errorMessage(null)
                .processingTimeMs(0L)
                .build();

        when(creditAgencyService.processCredit(any(CreditScoreRequestDto.class)))
                .thenReturn(response);

        // When & Then
        mockMvc.perform(post("/api/credit-agency/score")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.success").value(false));
    }

    @Test
    @DisplayName("Should return BAD_REQUEST for invalid request data")
    void shouldReturnBadRequestForInvalidData() throws Exception {
        String invalidJson = "{ \"sortCode\": \"\" }";

        // When & Then
        mockMvc.perform(post("/api/credit-agency/score")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(invalidJson))
                .andExpect(status().isBadRequest());
    }
}
