package com.cbsa.migration.controller;

import com.cbsa.migration.dto.InquiryCustomerResponseDto;
import com.cbsa.migration.service.CustomerService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.time.LocalDate;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(CustomerController.class)
class CustomerControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private CustomerService customerService;

    private InquiryCustomerResponseDto successResponse;
    private InquiryCustomerResponseDto notFoundResponse;
    private InquiryCustomerResponseDto systemErrorResponse;

    @BeforeEach
    void setUp() {
        successResponse = InquiryCustomerResponseDto.builder()
            .eyeCatcher("CUST")
            .sortCode("987654")
            .customerNumber(100001L)
            .name("John Doe")
            .address("123 Main St, London, UK")
            .dateOfBirth(LocalDate.of(1990, 1, 15))
            .creditScore(750)
            .creditScoreReviewDate(LocalDate.of(2024, 1, 1))
            .success(true)
            .failureCode("0")
            .build();

        notFoundResponse = InquiryCustomerResponseDto.builder()
            .sortCode("987654")
            .customerNumber(999999L)
            .success(false)
            .failureCode("1")
            .errorMessage("Customer not found")
            .build();

        systemErrorResponse = InquiryCustomerResponseDto.builder()
            .sortCode("987654")
            .customerNumber(100001L)
            .success(false)
            .failureCode("2")
            .errorMessage("System error: Database connection failed")
            .build();
    }

    @Test
    void inquireCustomer_WhenCustomerExists_Returns200WithCustomerData() throws Exception {
        when(customerService.findCustomer("987654", 100001L))
            .thenReturn(successResponse);

        mockMvc.perform(get("/api/customers/987654/100001")
                .accept(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(jsonPath("$.success").value(true))
            .andExpect(jsonPath("$.failureCode").value("0"))
            .andExpect(jsonPath("$.eyeCatcher").value("CUST"))
            .andExpect(jsonPath("$.sortCode").value("987654"))
            .andExpect(jsonPath("$.customerNumber").value(100001))
            .andExpect(jsonPath("$.name").value("John Doe"))
            .andExpect(jsonPath("$.address").value("123 Main St, London, UK"))
            .andExpect(jsonPath("$.creditScore").value(750));
    }

    @Test
    void inquireCustomer_WhenCustomerNotFound_Returns404() throws Exception {
        when(customerService.findCustomer("987654", 999999L))
            .thenReturn(notFoundResponse);

        mockMvc.perform(get("/api/customers/987654/999999")
                .accept(MediaType.APPLICATION_JSON))
            .andExpect(status().isNotFound())
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(jsonPath("$.success").value(false))
            .andExpect(jsonPath("$.failureCode").value("1"))
            .andExpect(jsonPath("$.errorMessage").value("Customer not found"))
            .andExpect(jsonPath("$.sortCode").value("987654"))
            .andExpect(jsonPath("$.customerNumber").value(999999));
    }

    @Test
    void inquireCustomer_WhenSystemError_Returns500() throws Exception {
        when(customerService.findCustomer("987654", 100001L))
            .thenReturn(systemErrorResponse);

        mockMvc.perform(get("/api/customers/987654/100001")
                .accept(MediaType.APPLICATION_JSON))
            .andExpect(status().isInternalServerError())
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(jsonPath("$.success").value(false))
            .andExpect(jsonPath("$.failureCode").value("2"))
            .andExpect(jsonPath("$.errorMessage").exists());
    }

    @Test
    void inquireCustomer_WithDifferentSortCode_CallsServiceWithCorrectParameters() throws Exception {
        String sortCode = "123456";
        Long customerNumber = 200002L;

        InquiryCustomerResponseDto response = InquiryCustomerResponseDto.builder()
            .eyeCatcher("CUST")
            .sortCode(sortCode)
            .customerNumber(customerNumber)
            .name("Jane Smith")
            .address("456 Oak Ave, Manchester, UK")
            .dateOfBirth(LocalDate.of(1985, 5, 20))
            .creditScore(820)
            .success(true)
            .failureCode("0")
            .build();

        when(customerService.findCustomer(sortCode, customerNumber))
            .thenReturn(response);

        mockMvc.perform(get("/api/customers/" + sortCode + "/" + customerNumber)
                .accept(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.sortCode").value(sortCode))
            .andExpect(jsonPath("$.customerNumber").value(customerNumber))
            .andExpect(jsonPath("$.name").value("Jane Smith"));
    }

    @Test
    void inquireCustomer_WithValidRequest_ReturnsJsonResponse() throws Exception {
        when(customerService.findCustomer("987654", 100001L))
            .thenReturn(successResponse);

        mockMvc.perform(get("/api/customers/987654/100001"))
            .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }
}
