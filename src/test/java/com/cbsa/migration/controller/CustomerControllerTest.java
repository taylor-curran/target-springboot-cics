package com.cbsa.migration.controller;

import com.cbsa.migration.model.CustomerInquiryResponse;
import com.cbsa.migration.service.CustomerInquiryService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import java.time.LocalDate;

import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(CustomerController.class)
class CustomerControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private CustomerInquiryService customerInquiryService;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void testInquireCustomer_Success() throws Exception {
        CustomerInquiryResponse response = CustomerInquiryResponse.builder()
                .success(true)
                .failureCode("0")
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1980, 1, 1))
                .creditScore(750)
                .build();

        when(customerInquiryService.inquireCustomer(1234567890L)).thenReturn(response);

        mockMvc.perform(get("/api/customer/inquiry/1234567890"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.failureCode").value("0"))
                .andExpect(jsonPath("$.customerNumber").value(1234567890L))
                .andExpect(jsonPath("$.name").value("John Doe"));
    }

    @Test
    void testInquireCustomer_NotFound() throws Exception {
        CustomerInquiryResponse response = CustomerInquiryResponse.builder()
                .success(false)
                .failureCode("1")
                .customerNumber(1234567890L)
                .build();

        when(customerInquiryService.inquireCustomer(1234567890L)).thenReturn(response);

        mockMvc.perform(get("/api/customer/inquiry/1234567890"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.failureCode").value("1"));
    }

    @Test
    void testInquireCustomer_RandomCustomer() throws Exception {
        CustomerInquiryResponse response = CustomerInquiryResponse.builder()
                .success(true)
                .failureCode("0")
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(5555555555L)
                .name("Random Customer")
                .address("456 Random St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .creditScore(680)
                .build();

        when(customerInquiryService.inquireCustomer(0L)).thenReturn(response);

        mockMvc.perform(get("/api/customer/inquiry/0"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.failureCode").value("0"))
                .andExpect(jsonPath("$.name").value("Random Customer"));
    }
}
