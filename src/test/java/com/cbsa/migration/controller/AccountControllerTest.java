package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CreateAccountRequest;
import com.cbsa.migration.dto.CreateAccountResponse;
import com.cbsa.migration.service.AccountCreationService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Web layer tests for AccountController using MockMvc.
 */
@WebMvcTest(AccountController.class)
class AccountControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private AccountCreationService accountCreationService;

    @Autowired
    private ObjectMapper objectMapper;

    private CreateAccountRequest buildValidRequest() {
        return CreateAccountRequest.builder()
                .customerNumber(1000001L)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("2.50"))
                .overdraftLimit(500)
                .build();
    }

    @Test
    void createAccount_success_returns201() throws Exception {
        LocalDate today = LocalDate.now();
        CreateAccountResponse successResponse = CreateAccountResponse.success(
                "10000001", "987654", 1000001L, "CURRENT",
                new BigDecimal("2.50"), today, 500,
                today, today.plusDays(30),
                BigDecimal.ZERO, BigDecimal.ZERO
        );
        when(accountCreationService.createAccount(any(CreateAccountRequest.class)))
                .thenReturn(successResponse);

        mockMvc.perform(post("/api/accounts")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(buildValidRequest())))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.accountNumber").value("10000001"))
                .andExpect(jsonPath("$.sortCode").value("987654"))
                .andExpect(jsonPath("$.accountType").value("CURRENT"));
    }

    @Test
    void createAccount_customerNotFound_returns404() throws Exception {
        when(accountCreationService.createAccount(any(CreateAccountRequest.class)))
                .thenReturn(CreateAccountResponse.failure("1", "Customer not found"));

        mockMvc.perform(post("/api/accounts")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(buildValidRequest())))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.failCode").value("1"));
    }

    @Test
    void createAccount_invalidAccountType_returns400() throws Exception {
        when(accountCreationService.createAccount(any(CreateAccountRequest.class)))
                .thenReturn(CreateAccountResponse.failure("A", "Invalid account type"));

        mockMvc.perform(post("/api/accounts")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(buildValidRequest())))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.failCode").value("A"));
    }

    @Test
    void createAccount_tooManyAccounts_returns409() throws Exception {
        when(accountCreationService.createAccount(any(CreateAccountRequest.class)))
                .thenReturn(CreateAccountResponse.failure("8", "Too many accounts"));

        mockMvc.perform(post("/api/accounts")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(buildValidRequest())))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.failCode").value("8"));
    }

    @Test
    void createAccount_lockFailed_returns500() throws Exception {
        when(accountCreationService.createAccount(any(CreateAccountRequest.class)))
                .thenReturn(CreateAccountResponse.failure("3", "Lock failed"));

        mockMvc.perform(post("/api/accounts")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(buildValidRequest())))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.failCode").value("3"));
    }

    @Test
    void createAccount_accountInsertFailed_returns500() throws Exception {
        when(accountCreationService.createAccount(any(CreateAccountRequest.class)))
                .thenReturn(CreateAccountResponse.failure("7", "Account insert failed"));

        mockMvc.perform(post("/api/accounts")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(buildValidRequest())))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.failCode").value("7"));
    }

    @Test
    void createAccount_countError_returns500() throws Exception {
        when(accountCreationService.createAccount(any(CreateAccountRequest.class)))
                .thenReturn(CreateAccountResponse.failure("9", "Error counting accounts"));

        mockMvc.perform(post("/api/accounts")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(buildValidRequest())))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.failCode").value("9"));
    }

    @Test
    void createAccount_dequeueFailed_returns500() throws Exception {
        when(accountCreationService.createAccount(any(CreateAccountRequest.class)))
                .thenReturn(CreateAccountResponse.failure("5", "Dequeue failed"));

        mockMvc.perform(post("/api/accounts")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(buildValidRequest())))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.failCode").value("5"));
    }

    @Test
    void createAccount_unknownFailCode_returns500() throws Exception {
        when(accountCreationService.createAccount(any(CreateAccountRequest.class)))
                .thenReturn(CreateAccountResponse.failure("X", "Unknown error"));

        mockMvc.perform(post("/api/accounts")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(buildValidRequest())))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.failCode").value("X"));
    }

    @Test
    void createAccount_nullFailCode_returns500() throws Exception {
        CreateAccountResponse response = CreateAccountResponse.builder()
                .success(false)
                .build();
        when(accountCreationService.createAccount(any(CreateAccountRequest.class)))
                .thenReturn(response);

        mockMvc.perform(post("/api/accounts")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(buildValidRequest())))
                .andExpect(status().isInternalServerError());
    }
}
