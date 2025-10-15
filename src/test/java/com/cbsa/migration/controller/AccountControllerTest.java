package com.cbsa.migration.controller;

import com.cbsa.migration.dto.AccountRequestDto;
import com.cbsa.migration.dto.AccountResponseDto;
import com.cbsa.migration.exception.AccountCreationException;
import com.cbsa.migration.exception.AccountNotFoundException;
import com.cbsa.migration.service.AccountService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(AccountController.class)
class AccountControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @MockBean
    private AccountService accountService;

    private AccountRequestDto testRequest;
    private AccountResponseDto testResponse;

    @BeforeEach
    void setUp() {
        testRequest = AccountRequestDto.builder()
            .sortCode("987654")
            .customerNumber(1L)
            .accountType("CURRENT")
            .interestRate(new BigDecimal("2.5"))
            .overdraftLimit(1000)
            .initialDeposit(new BigDecimal("100.00"))
            .build();

        testResponse = AccountResponseDto.builder()
            .accountNumber("00000001")
            .sortCode("987654")
            .customerNumber(1L)
            .customerName("John Doe")
            .accountType("CURRENT")
            .interestRate(new BigDecimal("2.5"))
            .overdraftLimit(1000)
            .availableBalance(new BigDecimal("100.00"))
            .actualBalance(new BigDecimal("100.00"))
            .openedDate(LocalDate.now())
            .status("ACTIVE")
            .build();
    }

    @Test
    void testCreateAccount_Success() throws Exception {
        when(accountService.createAccount(any(AccountRequestDto.class))).thenReturn(testResponse);

        mockMvc.perform(post("/api/account")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testRequest)))
            .andExpect(status().isCreated())
            .andExpect(jsonPath("$.accountNumber").value("00000001"))
            .andExpect(jsonPath("$.sortCode").value("987654"))
            .andExpect(jsonPath("$.customerNumber").value(1))
            .andExpect(jsonPath("$.accountType").value("CURRENT"));

        verify(accountService).createAccount(any(AccountRequestDto.class));
    }

    @Test
    void testCreateAccount_CustomerNotFound() throws Exception {
        when(accountService.createAccount(any(AccountRequestDto.class)))
            .thenThrow(new AccountCreationException("Customer not found", "1"));

        mockMvc.perform(post("/api/account")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testRequest)))
            .andExpect(status().isBadRequest());
    }

    @Test
    void testCreateAccount_TooManyAccounts() throws Exception {
        when(accountService.createAccount(any(AccountRequestDto.class)))
            .thenThrow(new AccountCreationException("Customer already has maximum number of accounts", "8"));

        mockMvc.perform(post("/api/account")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testRequest)))
            .andExpect(status().isBadRequest());
    }

    @Test
    void testGetAccount_Success() throws Exception {
        when(accountService.getAccount("987654", "00000001")).thenReturn(testResponse);

        mockMvc.perform(get("/api/account/987654/00000001"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.accountNumber").value("00000001"))
            .andExpect(jsonPath("$.sortCode").value("987654"))
            .andExpect(jsonPath("$.customerName").value("John Doe"));

        verify(accountService).getAccount("987654", "00000001");
    }

    @Test
    void testGetAccount_NotFound() throws Exception {
        when(accountService.getAccount("987654", "99999999"))
            .thenThrow(new AccountNotFoundException("Account not found"));

        mockMvc.perform(get("/api/account/987654/99999999"))
            .andExpect(status().isNotFound());
    }

    @Test
    void testUpdateAccount_Success() throws Exception {
        AccountRequestDto updateRequest = AccountRequestDto.builder()
            .accountType("SAVINGS")
            .interestRate(new BigDecimal("3.0"))
            .overdraftLimit(500)
            .build();

        AccountResponseDto updatedResponse = AccountResponseDto.builder()
            .accountNumber("00000001")
            .sortCode("987654")
            .accountType("SAVINGS")
            .interestRate(new BigDecimal("3.0"))
            .overdraftLimit(500)
            .build();

        when(accountService.updateAccount(eq("987654"), eq("00000001"), any(AccountRequestDto.class)))
            .thenReturn(updatedResponse);

        mockMvc.perform(put("/api/account/987654/00000001")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(updateRequest)))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.accountType").value("SAVINGS"))
            .andExpect(jsonPath("$.interestRate").value(3.0));

        verify(accountService).updateAccount(eq("987654"), eq("00000001"), any(AccountRequestDto.class));
    }

    @Test
    void testDeleteAccount_Success() throws Exception {
        doNothing().when(accountService).deleteAccount("987654", "00000001");

        mockMvc.perform(delete("/api/account/987654/00000001"))
            .andExpect(status().isNoContent());

        verify(accountService).deleteAccount("987654", "00000001");
    }

    @Test
    void testGetAccountsByCustomer_Success() throws Exception {
        List<AccountResponseDto> accounts = Arrays.asList(testResponse);
        when(accountService.getAccountsByCustomer("987654", 1L)).thenReturn(accounts);

        mockMvc.perform(get("/api/account/customer/987654/1"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$").isArray())
            .andExpect(jsonPath("$[0].accountNumber").value("00000001"));

        verify(accountService).getAccountsByCustomer("987654", 1L);
    }
}
