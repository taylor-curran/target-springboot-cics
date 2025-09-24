package com.cbsa.migration.controller;

import com.cbsa.migration.model.AccountCreationRequest;
import com.cbsa.migration.model.AccountCreationResponse;
import com.cbsa.migration.model.DebitCreditRequest;
import com.cbsa.migration.model.DebitCreditResponse;
import com.cbsa.migration.service.AccountCreationService;
import com.cbsa.migration.service.DebitCreditService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigDecimal;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(AccountController.class)
class AccountControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private AccountCreationService accountCreationService;

    @MockBean
    private DebitCreditService debitCreditService;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void testCreateAccount_Success() throws Exception {
        AccountCreationRequest request = AccountCreationRequest.builder()
                .customerNumber(1234567890L)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("2.5"))
                .overdraftLimit(1000)
                .initialBalance(new BigDecimal("100.00"))
                .build();

        AccountCreationResponse response = AccountCreationResponse.builder()
                .success(true)
                .failureCode("0")
                .accountNumber("12345678")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth("01011980")
                .build();

        when(accountCreationService.createAccount(any(AccountCreationRequest.class))).thenReturn(response);

        mockMvc.perform(post("/api/account/create")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.failureCode").value("0"))
                .andExpect(jsonPath("$.accountNumber").value("12345678"));
    }

    @Test
    void testProcessDebitCredit_Success() throws Exception {
        DebitCreditRequest request = DebitCreditRequest.builder()
                .sortCode("987654")
                .accountNumber("12345678")
                .amount(new BigDecimal("100.00"))
                .build();

        DebitCreditResponse response = DebitCreditResponse.builder()
                .success(true)
                .failureCode("0")
                .sortCode("987654")
                .accountNumber("12345678")
                .availableBalance(new BigDecimal("600.00"))
                .actualBalance(new BigDecimal("600.00"))
                .build();

        when(debitCreditService.processTransaction(any(DebitCreditRequest.class))).thenReturn(response);

        mockMvc.perform(post("/api/account/debit-credit")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.failureCode").value("0"))
                .andExpect(jsonPath("$.availableBalance").value(600.00));
    }
}
