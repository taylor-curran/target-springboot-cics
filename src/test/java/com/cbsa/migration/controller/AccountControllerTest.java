package com.cbsa.migration.controller;

import com.cbsa.migration.dto.AccountRequestDto;
import com.cbsa.migration.dto.AccountResponseDto;
import com.cbsa.migration.service.AccountService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Integration tests for AccountController using MockMvc
 * Tests REST API endpoints with mocked service layer
 */
@WebMvcTest(AccountController.class)
class AccountControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @MockBean
    private AccountService accountService;

    @Test
    void testCreateAccount_validRequest_returnsCreatedResponse() throws Exception {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .accountType("CURRENT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .initialDeposit(new BigDecimal("500.00"))
                .build();

        AccountResponseDto mockResponse = AccountResponseDto.builder()
                .accountNumber("12345678")
                .sortCode("987654")
                .accountType("CURRENT")
                .customerNumber(1234567890L)
                .availableBalance(new BigDecimal("500.00"))
                .actualBalance(new BigDecimal("500.00"))
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .openedDate(LocalDate.now())
                .status("ACTIVE")
                .build();

        when(accountService.createAccount(any(AccountRequestDto.class))).thenReturn(mockResponse);

        // When & Then
        mockMvc.perform(post("/api/accounts")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isCreated())
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(jsonPath("$.accountNumber", is("12345678")))
            .andExpect(jsonPath("$.sortCode", is("987654")))
            .andExpect(jsonPath("$.accountType", is("CURRENT")))
            .andExpect(jsonPath("$.customerNumber", is(1234567890)))
            .andExpect(jsonPath("$.availableBalance", is(500.00)))
            .andExpect(jsonPath("$.status", is("ACTIVE")));
    }

    @Test
    void testCreateAccount_invalidRequest_returnsBadRequest() throws Exception {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .customerNumber(1234567890L)
                .sortCode("987654")
                .build();

        // When & Then
        mockMvc.perform(post("/api/accounts")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isBadRequest());
    }

    @Test
    void testCreateAccount_serviceThrowsException_returnsBadRequest() throws Exception {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .accountType("CURRENT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .build();

        when(accountService.createAccount(any(AccountRequestDto.class)))
            .thenThrow(new IllegalArgumentException("Customer not found"));

        // When & Then
        mockMvc.perform(post("/api/accounts")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isBadRequest());
    }

    @Test
    void testUpdateAccount_validRequest_returnsOkResponse() throws Exception {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .accountType("SAVINGS")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .interestRate(new BigDecimal("2.50"))
                .overdraftLimit(2000)
                .build();

        AccountResponseDto mockResponse = AccountResponseDto.builder()
                .accountNumber("12345678")
                .sortCode("987654")
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("2.50"))
                .overdraftLimit(2000)
                .status("ACTIVE")
                .build();

        when(accountService.updateAccount(anyString(), anyString(), any(AccountRequestDto.class)))
            .thenReturn(mockResponse);

        // When & Then
        mockMvc.perform(put("/api/accounts/987654/12345678")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isOk())
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(jsonPath("$.accountType", is("SAVINGS")))
            .andExpect(jsonPath("$.interestRate", is(2.50)))
            .andExpect(jsonPath("$.overdraftLimit", is(2000)));
    }

    @Test
    void testUpdateAccount_accountNotFound_returnsNotFound() throws Exception {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .accountType("SAVINGS")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .build();

        when(accountService.updateAccount(anyString(), anyString(), any(AccountRequestDto.class)))
            .thenThrow(new IllegalArgumentException("Account not found"));

        // When & Then
        mockMvc.perform(put("/api/accounts/987654/12345678")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isNotFound());
    }

    @Test
    void testGetAccountBalance_validRequest_returnsOkResponse() throws Exception {
        // Given
        AccountResponseDto mockResponse = AccountResponseDto.builder()
                .accountNumber("12345678")
                .sortCode("987654")
                .availableBalance(new BigDecimal("1500.00"))
                .actualBalance(new BigDecimal("1750.00"))
                .status("ACTIVE")
                .build();

        when(accountService.getAccountBalance("987654", "12345678")).thenReturn(mockResponse);

        // When & Then
        mockMvc.perform(get("/api/accounts/987654/12345678/balance"))
            .andExpect(status().isOk())
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(jsonPath("$.availableBalance", is(1500.00)))
            .andExpect(jsonPath("$.actualBalance", is(1750.00)))
            .andExpect(jsonPath("$.status", is("ACTIVE")));
    }

    @Test
    void testGetAccountBalance_accountNotFound_returnsNotFound() throws Exception {
        // Given
        when(accountService.getAccountBalance("987654", "12345678"))
            .thenThrow(new IllegalArgumentException("Account not found"));

        // When & Then
        mockMvc.perform(get("/api/accounts/987654/12345678/balance"))
            .andExpect(status().isNotFound());
    }

    @Test
    void testProcessOverdraft_validRequest_returnsOkResponse() throws Exception {
        // Given
        Map<String, BigDecimal> request = new HashMap<>();
        request.put("amount", new BigDecimal("200.00"));

        AccountResponseDto mockResponse = AccountResponseDto.builder()
                .accountNumber("12345678")
                .sortCode("987654")
                .availableBalance(new BigDecimal("-100.00"))
                .actualBalance(new BigDecimal("-100.00"))
                .status("OVERDRAFT")
                .build();

        when(accountService.processOverdraft("987654", "12345678", new BigDecimal("200.00")))
            .thenReturn(mockResponse);

        // When & Then
        mockMvc.perform(post("/api/accounts/987654/12345678/overdraft")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isOk())
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(jsonPath("$.availableBalance", is(-100.00)))
            .andExpect(jsonPath("$.status", is("OVERDRAFT")));
    }

    @Test
    void testProcessOverdraft_missingAmount_returnsBadRequest() throws Exception {
        // Given
        Map<String, String> request = new HashMap<>();
        request.put("description", "test");

        // When & Then
        mockMvc.perform(post("/api/accounts/987654/12345678/overdraft")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isBadRequest());
    }

    @Test
    void testProcessOverdraft_overdraftExceedsLimit_returnsBadRequest() throws Exception {
        // Given
        Map<String, BigDecimal> request = new HashMap<>();
        request.put("amount", new BigDecimal("1000.00"));

        when(accountService.processOverdraft("987654", "12345678", new BigDecimal("1000.00")))
            .thenThrow(new IllegalArgumentException("Overdraft exceeds limit"));

        // When & Then
        mockMvc.perform(post("/api/accounts/987654/12345678/overdraft")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isBadRequest());
    }

    @Test
    void testGetAccountDetails_validRequest_returnsOkResponse() throws Exception {
        // Given
        AccountResponseDto mockResponse = AccountResponseDto.builder()
                .accountNumber("12345678")
                .sortCode("987654")
                .accountType("CURRENT")
                .customerNumber(1234567890L)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .openedDate(LocalDate.of(2023, 1, 15))
                .status("ACTIVE")
                .build();

        when(accountService.getAccountDetails("987654", "12345678")).thenReturn(mockResponse);

        // When & Then
        mockMvc.perform(get("/api/accounts/987654/12345678"))
            .andExpect(status().isOk())
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(jsonPath("$.accountNumber", is("12345678")))
            .andExpect(jsonPath("$.accountType", is("CURRENT")))
            .andExpect(jsonPath("$.customerNumber", is(1234567890)))
            .andExpect(jsonPath("$.overdraftLimit", is(1000)));
    }

    @Test
    void testGetAccountsByCustomer_validRequest_returnsOkResponse() throws Exception {
        // Given
        List<AccountResponseDto> mockResponse = Arrays.asList(
                AccountResponseDto.builder()
                        .accountNumber("12345678")
                        .sortCode("987654")
                        .accountType("CURRENT")
                        .customerNumber(1234567890L)
                        .status("ACTIVE")
                        .build(),
                AccountResponseDto.builder()
                        .accountNumber("12345679")
                        .sortCode("987654")
                        .accountType("SAVINGS")
                        .customerNumber(1234567890L)
                        .status("ACTIVE")
                        .build()
        );

        when(accountService.getAccountsByCustomer(1234567890L)).thenReturn(mockResponse);

        // When & Then
        mockMvc.perform(get("/api/accounts/customer/1234567890"))
            .andExpect(status().isOk())
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(jsonPath("$", hasSize(2)))
            .andExpect(jsonPath("$[0].accountNumber", is("12345678")))
            .andExpect(jsonPath("$[0].accountType", is("CURRENT")))
            .andExpect(jsonPath("$[1].accountNumber", is("12345679")))
            .andExpect(jsonPath("$[1].accountType", is("SAVINGS")));
    }
}
