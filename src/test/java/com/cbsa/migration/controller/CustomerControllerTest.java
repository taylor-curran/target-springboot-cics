package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerCreationRequestDto;
import com.cbsa.migration.dto.CustomerCreationResponseDto;
import com.cbsa.migration.service.CustomerCreationService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.time.LocalDate;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(CustomerController.class)
class CustomerControllerTest {

    @Autowired
    private MockMvc mockMvc;
    
    @Autowired
    private ObjectMapper objectMapper;
    
    @MockBean
    private CustomerCreationService customerCreationService;
    
    @Test
    void shouldCreateCustomerSuccessfully() throws Exception {
        CustomerCreationRequestDto request = CustomerCreationRequestDto.builder()
            .sortCode("987654")
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1990, 1, 15))
            .build();
            
        CustomerCreationResponseDto response = CustomerCreationResponseDto.builder()
            .eyeCatcher("CUST")
            .sortCode("987654")
            .customerNumber(100001L)
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1990, 1, 15))
            .creditScore(750)
            .creditScoreReviewDate(LocalDate.now().plusDays(10))
            .success(true)
            .build();
            
        when(customerCreationService.createCustomer(any())).thenReturn(response);
        
        mockMvc.perform(post("/api/customers")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isCreated())
            .andExpect(jsonPath("$.success").value(true))
            .andExpect(jsonPath("$.customerNumber").value(100001))
            .andExpect(jsonPath("$.sortCode").value("987654"));
    }
    
    @Test
    void shouldReturnBadRequestWhenValidationFails() throws Exception {
        CustomerCreationResponseDto response = CustomerCreationResponseDto.builder()
            .success(false)
            .failCode("O")
            .errorMessage("Date of birth validation failed")
            .build();
            
        when(customerCreationService.createCustomer(any())).thenReturn(response);
        
        CustomerCreationRequestDto request = CustomerCreationRequestDto.builder()
            .sortCode("987654")
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1600, 12, 31))
            .build();
        
        mockMvc.perform(post("/api/customers")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isBadRequest())
            .andExpect(jsonPath("$.success").value(false))
            .andExpect(jsonPath("$.failCode").value("O"));
    }
}
