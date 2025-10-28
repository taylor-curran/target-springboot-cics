package com.cbsa.migration.controller;

import com.cbsa.migration.model.Control;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Unit tests for StatusController
 * Tests REST API endpoints for application status information
 */
@WebMvcTest(StatusController.class)
@TestPropertySource(properties = {"spring.datasource.url=jdbc:h2:mem:testdb"})
class StatusControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private AccountRepository accountRepository;

    @MockBean
    private CustomerRepository customerRepository;

    @MockBean
    private TransactionRepository transactionRepository;

    @MockBean
    private ControlRepository controlRepository;

    @Test
    @DisplayName("Should return application status successfully")
    void shouldReturnApplicationStatusSuccessfully() throws Exception {
        // Given
        Control mockControl = new Control();
        mockControl.setLastCustomerNumber(1000000L);
        mockControl.setLastAccountNumber(2000000);

        when(controlRepository.initializeControlRecord()).thenReturn(mockControl);
        when(customerRepository.count()).thenReturn(25);
        when(accountRepository.count()).thenReturn(50);
        when(transactionRepository.count()).thenReturn(150);

        // When & Then
        mockMvc.perform(get("/api/status"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.application").value("CBSA Java Migration"))
                .andExpect(jsonPath("$.version").value("0.0.1"))
                .andExpect(jsonPath("$.database").value("jdbc:h2:mem:testdb"))
                .andExpect(jsonPath("$.customers").value(25))
                .andExpect(jsonPath("$.accounts").value(50))
                .andExpect(jsonPath("$.transactions").value(150))
                .andExpect(jsonPath("$.control.lastCustomerNumber").value(1000000))
                .andExpect(jsonPath("$.control.lastAccountNumber").value(2000000));
    }

    @Test
    @DisplayName("Should handle zero entity counts")
    void shouldHandleZeroEntityCounts() throws Exception {
        // Given
        Control mockControl = new Control();
        mockControl.setLastCustomerNumber(0L);
        mockControl.setLastAccountNumber(0);

        when(controlRepository.initializeControlRecord()).thenReturn(mockControl);
        when(customerRepository.count()).thenReturn(0);
        when(accountRepository.count()).thenReturn(0);
        when(transactionRepository.count()).thenReturn(0);

        // When & Then
        mockMvc.perform(get("/api/status"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.application").value("CBSA Java Migration"))
                .andExpect(jsonPath("$.version").value("0.0.1"))
                .andExpect(jsonPath("$.customers").value(0))
                .andExpect(jsonPath("$.accounts").value(0))
                .andExpect(jsonPath("$.transactions").value(0))
                .andExpect(jsonPath("$.control.lastCustomerNumber").value(0))
                .andExpect(jsonPath("$.control.lastAccountNumber").value(0));
    }

    @Test
    @DisplayName("Should handle large entity counts")
    void shouldHandleLargeEntityCounts() throws Exception {
        // Given
        Control mockControl = new Control();
        mockControl.setLastCustomerNumber(9999999L);
        mockControl.setLastAccountNumber(8888888);

        when(controlRepository.initializeControlRecord()).thenReturn(mockControl);
        when(customerRepository.count()).thenReturn(10000);
        when(accountRepository.count()).thenReturn(25000);
        when(transactionRepository.count()).thenReturn(100000);

        // When & Then
        mockMvc.perform(get("/api/status"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.application").value("CBSA Java Migration"))
                .andExpect(jsonPath("$.version").value("0.0.1"))
                .andExpect(jsonPath("$.customers").value(10000))
                .andExpect(jsonPath("$.accounts").value(25000))
                .andExpect(jsonPath("$.transactions").value(100000))
                .andExpect(jsonPath("$.control.lastCustomerNumber").value(9999999))
                .andExpect(jsonPath("$.control.lastAccountNumber").value(8888888));
    }
}
