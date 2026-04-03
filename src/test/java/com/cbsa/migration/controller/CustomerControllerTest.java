package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerDTO;
import com.cbsa.migration.service.CustomerService;
import com.cbsa.migration.service.CustomerService.InquiryResult;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.Matchers.*;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * MockMvc tests for CustomerController.
 * Tests REST endpoints for the migrated INQCUST COBOL program.
 */
@WebMvcTest(CustomerController.class)
class CustomerControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private CustomerService customerService;

    private CustomerDTO createTestCustomerDTO() {
        return CustomerDTO.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(1L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 1, 15))
                .creditScore(750)
                .creditScoreReviewDate(LocalDate.of(2024, 6, 1))
                .build();
    }

    // ========== GET /api/customers/{sortCode}/{customerNumber} ==========

    @Test
    void getCustomer_found_returns200() throws Exception {
        // Given
        CustomerDTO dto = createTestCustomerDTO();
        when(customerService.inquireCustomer("987654", 1L))
                .thenReturn(InquiryResult.success(dto));

        // When & Then
        mockMvc.perform(get("/api/customers/987654/1"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.eyeCatcher", is("CUST")))
                .andExpect(jsonPath("$.sortCode", is("987654")))
                .andExpect(jsonPath("$.customerNumber", is(1)))
                .andExpect(jsonPath("$.name", is("John Doe")))
                .andExpect(jsonPath("$.address", is("123 Main St")))
                .andExpect(jsonPath("$.dateOfBirth", is("1990-01-15")))
                .andExpect(jsonPath("$.creditScore", is(750)))
                .andExpect(jsonPath("$.creditScoreReviewDate", is("2024-06-01")));
    }

    @Test
    void getCustomer_notFound_returns404() throws Exception {
        // Given
        when(customerService.inquireCustomer("987654", 999L))
                .thenReturn(InquiryResult.failure(CustomerService.FAIL_CODE_NOT_FOUND));

        // When & Then
        mockMvc.perform(get("/api/customers/987654/999"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.success", is(false)))
                .andExpect(jsonPath("$.failCode", is("1")))
                .andExpect(jsonPath("$.message", is("Customer not found")))
                .andExpect(jsonPath("$.sortCode", is("987654")))
                .andExpect(jsonPath("$.customerNumber", is(999)));
    }

    @Test
    void getCustomer_systemError_returns500() throws Exception {
        // Given
        when(customerService.inquireCustomer("987654", 1L))
                .thenReturn(InquiryResult.failure(CustomerService.FAIL_CODE_SYSTEM_ERROR));

        // When & Then
        mockMvc.perform(get("/api/customers/987654/1"))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.success", is(false)))
                .andExpect(jsonPath("$.failCode", is("2")))
                .andExpect(jsonPath("$.message", is("Internal error during customer inquiry")));
    }

    @Test
    void getCustomer_randomCustomer_returns200() throws Exception {
        // Given - customerNumber=0 triggers random customer lookup
        CustomerDTO dto = createTestCustomerDTO();
        when(customerService.inquireCustomer("987654", 0L))
                .thenReturn(InquiryResult.success(dto));

        // When & Then
        mockMvc.perform(get("/api/customers/987654/0"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.eyeCatcher", is("CUST")))
                .andExpect(jsonPath("$.name", is("John Doe")));
    }

    @Test
    void getCustomer_lastCustomer_returns200() throws Exception {
        // Given - customerNumber=9999999999 triggers last customer lookup
        CustomerDTO dto = CustomerDTO.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(100L)
                .name("Last Customer")
                .address("End of the road")
                .dateOfBirth(LocalDate.of(1970, 1, 1))
                .creditScore(600)
                .build();
        when(customerService.inquireCustomer("987654", 9999999999L))
                .thenReturn(InquiryResult.success(dto));

        // When & Then
        mockMvc.perform(get("/api/customers/987654/9999999999"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.customerNumber", is(100)))
                .andExpect(jsonPath("$.name", is("Last Customer")));
    }

    @Test
    void getCustomer_randomCustomer_notFound_returns404() throws Exception {
        // Given
        when(customerService.inquireCustomer("987654", 0L))
                .thenReturn(InquiryResult.failure(CustomerService.FAIL_CODE_NOT_FOUND));

        // When & Then
        mockMvc.perform(get("/api/customers/987654/0"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.failCode", is("1")));
    }

    @Test
    void getCustomer_lastCustomer_notFound_returns404() throws Exception {
        // Given
        when(customerService.inquireCustomer("987654", 9999999999L))
                .thenReturn(InquiryResult.failure(CustomerService.FAIL_CODE_NOT_FOUND));

        // When & Then
        mockMvc.perform(get("/api/customers/987654/9999999999"))
                .andExpect(status().isNotFound());
    }

    @Test
    void getCustomer_nullCreditScoreReviewDate_returns200() throws Exception {
        // Given
        CustomerDTO dto = CustomerDTO.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(5L)
                .name("No Review")
                .address("Nowhere")
                .dateOfBirth(LocalDate.of(2000, 6, 15))
                .creditScore(500)
                .creditScoreReviewDate(null)
                .build();
        when(customerService.inquireCustomer("987654", 5L))
                .thenReturn(InquiryResult.success(dto));

        // When & Then
        mockMvc.perform(get("/api/customers/987654/5"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.creditScoreReviewDate").doesNotExist());
    }

    // ========== GET /api/customers ==========

    @Test
    void listCustomers_noFilter_returnsAll() throws Exception {
        // Given
        List<CustomerDTO> customers = Arrays.asList(
                createTestCustomerDTO(),
                CustomerDTO.builder()
                        .eyeCatcher("CUST")
                        .sortCode("987654")
                        .customerNumber(2L)
                        .name("Jane Smith")
                        .address("456 Oak")
                        .dateOfBirth(LocalDate.of(1985, 5, 20))
                        .creditScore(680)
                        .build()
        );
        when(customerService.listCustomers()).thenReturn(customers);

        // When & Then
        mockMvc.perform(get("/api/customers"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$", hasSize(2)))
                .andExpect(jsonPath("$[0].name", is("John Doe")))
                .andExpect(jsonPath("$[1].name", is("Jane Smith")));

        verify(customerService).listCustomers();
        verify(customerService, never()).searchCustomersByName(anyString());
    }

    @Test
    void listCustomers_emptyResult_returns200WithEmptyList() throws Exception {
        // Given
        when(customerService.listCustomers()).thenReturn(Collections.emptyList());

        // When & Then
        mockMvc.perform(get("/api/customers"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$", hasSize(0)));
    }

    @Test
    void listCustomers_withNameFilter_searchesByName() throws Exception {
        // Given
        List<CustomerDTO> customers = Collections.singletonList(createTestCustomerDTO());
        when(customerService.searchCustomersByName("Doe")).thenReturn(customers);

        // When & Then
        mockMvc.perform(get("/api/customers").param("name", "Doe"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$", hasSize(1)))
                .andExpect(jsonPath("$[0].name", is("John Doe")));

        verify(customerService).searchCustomersByName("Doe");
        verify(customerService, never()).listCustomers();
    }

    @Test
    void listCustomers_withBlankNameFilter_returnsAll() throws Exception {
        // Given
        List<CustomerDTO> customers = Arrays.asList(createTestCustomerDTO());
        when(customerService.listCustomers()).thenReturn(customers);

        // When & Then
        mockMvc.perform(get("/api/customers").param("name", "   "))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$", hasSize(1)));

        verify(customerService).listCustomers();
        verify(customerService, never()).searchCustomersByName(anyString());
    }

    @Test
    void listCustomers_withEmptyNameFilter_returnsAll() throws Exception {
        // Given
        List<CustomerDTO> customers = Arrays.asList(createTestCustomerDTO());
        when(customerService.listCustomers()).thenReturn(customers);

        // When & Then
        mockMvc.perform(get("/api/customers").param("name", ""))
                .andExpect(status().isOk());

        verify(customerService).listCustomers();
    }

    @Test
    void searchCustomers_noResults_returnsEmptyList() throws Exception {
        // Given
        when(customerService.searchCustomersByName("Nobody")).thenReturn(Collections.emptyList());

        // When & Then
        mockMvc.perform(get("/api/customers").param("name", "Nobody"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$", hasSize(0)));
    }

    // ========== GET /api/customers/count ==========

    @Test
    void getCustomerCount_returnsCount() throws Exception {
        // Given
        when(customerService.getCustomerCount()).thenReturn(42);

        // When & Then
        mockMvc.perform(get("/api/customers/count"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.count", is(42)));
    }

    @Test
    void getCustomerCount_zeroCustomers_returnsZero() throws Exception {
        // Given
        when(customerService.getCustomerCount()).thenReturn(0);

        // When & Then
        mockMvc.perform(get("/api/customers/count"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.count", is(0)));
    }
}
