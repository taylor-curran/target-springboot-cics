package com.cbsa.migration.controller;

import com.cbsa.migration.dto.AccountCreationRequestDto;
import com.cbsa.migration.dto.AccountCreationResponseDto;
import com.cbsa.migration.dto.AccountInquiryResponseDto;
import com.cbsa.migration.dto.AccountUpdateRequestDto;
import com.cbsa.migration.dto.BalanceUpdateRequestDto;
import com.cbsa.migration.dto.BalanceUpdateResponseDto;
import com.cbsa.migration.service.AccountCreationService;
import com.cbsa.migration.service.AccountInquiryService;
import com.cbsa.migration.service.AccountMaintenanceService;
import com.cbsa.migration.service.BalanceManagementService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigDecimal;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(AccountController.class)
class AccountControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @MockBean
    private AccountCreationService accountCreationService;

    @MockBean
    private AccountInquiryService accountInquiryService;

    @MockBean
    private AccountMaintenanceService accountMaintenanceService;

    @MockBean
    private BalanceManagementService balanceManagementService;

    @Test
    void createAccount_WithValidRequest_ShouldReturnCreatedStatus() throws Exception {
        AccountCreationRequestDto request = new AccountCreationRequestDto(
            1234567890L,
            "CURRENT",
            new BigDecimal("2.50"),
            1000
        );

        AccountCreationResponseDto response = new AccountCreationResponseDto(true, null);
        response.setCustomerNumber(1234567890L);
        response.setAccountType("CURRENT");
        response.setSortCode("123456");
        response.setAccountNumber("00000001");

        when(accountCreationService.createAccount(any(AccountCreationRequestDto.class))).thenReturn(response);

        mockMvc.perform(post("/api/accounts")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.customerNumber").value(1234567890L))
                .andExpect(jsonPath("$.accountType").value("CURRENT"))
                .andExpect(jsonPath("$.sortCode").value("123456"))
                .andExpect(jsonPath("$.accountNumber").value("00000001"));
    }

    @Test
    void createAccount_WithInvalidRequest_ShouldReturnBadRequestStatus() throws Exception {
        AccountCreationRequestDto request = new AccountCreationRequestDto(
            1234567890L,
            "CURRENT",
            new BigDecimal("2.50"),
            1000
        );

        AccountCreationResponseDto response = new AccountCreationResponseDto(false, "1");

        when(accountCreationService.createAccount(any(AccountCreationRequestDto.class))).thenReturn(response);

        mockMvc.perform(post("/api/accounts")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.failCode").value("1"));
    }

    @Test
    void inquireAccount_WithValidAccount_ShouldReturnAccountDetails() throws Exception {
        AccountInquiryResponseDto response = new AccountInquiryResponseDto(true);
        response.setCustomerNumber(1234567890L);
        response.setAccountType("CURRENT");
        response.setSortCode("123456");
        response.setAccountNumber("00000001");

        when(accountInquiryService.inquireAccount("123456", "00000001")).thenReturn(response);

        mockMvc.perform(get("/api/accounts/123456/00000001"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.customerNumber").value(1234567890L))
                .andExpect(jsonPath("$.accountType").value("CURRENT"))
                .andExpect(jsonPath("$.sortCode").value("123456"))
                .andExpect(jsonPath("$.accountNumber").value("00000001"));
    }

    @Test
    void inquireAccount_WithNonExistentAccount_ShouldReturnNotFoundStatus() throws Exception {
        AccountInquiryResponseDto response = new AccountInquiryResponseDto(false);

        when(accountInquiryService.inquireAccount("123456", "99999999")).thenReturn(response);

        mockMvc.perform(get("/api/accounts/123456/99999999"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.success").value(false));
    }

    @Test
    void updateAccount_WithValidRequest_ShouldReturnUpdatedAccount() throws Exception {
        AccountUpdateRequestDto request = new AccountUpdateRequestDto(
            "SAVINGS",
            new BigDecimal("3.00"),
            1500
        );

        AccountInquiryResponseDto response = new AccountInquiryResponseDto(true);
        response.setAccountType("SAVINGS");
        response.setInterestRate(new BigDecimal("3.00"));
        response.setOverdraftLimit(1500);

        when(accountMaintenanceService.updateAccount(eq("123456"), eq("00000001"), any(AccountUpdateRequestDto.class)))
            .thenReturn(response);

        mockMvc.perform(put("/api/accounts/123456/00000001")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.accountType").value("SAVINGS"))
                .andExpect(jsonPath("$.interestRate").value(3.00))
                .andExpect(jsonPath("$.overdraftLimit").value(1500));
    }

    @Test
    void updateBalance_WithValidRequest_ShouldReturnUpdatedBalance() throws Exception {
        BalanceUpdateRequestDto request = new BalanceUpdateRequestDto(new BigDecimal("100.00"));

        BalanceUpdateResponseDto response = new BalanceUpdateResponseDto(true, "0");
        response.setSortCode("123456");
        response.setAvailableBalance(new BigDecimal("600.00"));
        response.setActualBalance(new BigDecimal("600.00"));

        when(balanceManagementService.updateBalance(eq("123456"), eq("00000001"), any(BalanceUpdateRequestDto.class)))
            .thenReturn(response);

        mockMvc.perform(post("/api/accounts/123456/00000001/balance")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.failCode").value("0"))
                .andExpect(jsonPath("$.sortCode").value("123456"))
                .andExpect(jsonPath("$.availableBalance").value(600.00))
                .andExpect(jsonPath("$.actualBalance").value(600.00));
    }

    @Test
    void updateBalance_WithInvalidRequest_ShouldReturnBadRequestStatus() throws Exception {
        BalanceUpdateRequestDto request = new BalanceUpdateRequestDto(new BigDecimal("-600.00"));

        BalanceUpdateResponseDto response = new BalanceUpdateResponseDto(false, "3");

        when(balanceManagementService.updateBalance(eq("123456"), eq("00000001"), any(BalanceUpdateRequestDto.class)))
            .thenReturn(response);

        mockMvc.perform(post("/api/accounts/123456/00000001/balance")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.failCode").value("3"));
    }
}
