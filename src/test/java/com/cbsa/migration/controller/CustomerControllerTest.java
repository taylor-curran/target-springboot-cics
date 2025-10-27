package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerRequestDto;
import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.service.CustomerService;
import com.cbsa.migration.service.SortCodeService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.time.LocalDate;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(CustomerController.class)
class CustomerControllerTest {
    
    @Autowired
    private MockMvc mockMvc;
    
    @Autowired
    private ObjectMapper objectMapper;
    
    @MockBean
    private CustomerService customerService;
    
    @MockBean
    private SortCodeService sortCodeService;
    
    @Test
    void testGetCustomer_Found() throws Exception {
        String sortCode = "123456";
        Long customerNumber = 100001L;
        Customer customer = Customer.builder()
            .sortCode(sortCode)
            .customerNumber(customerNumber)
            .name("Mr John Smith")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1980, 1, 1))
            .creditScore(750)
            .build();
        
        when(sortCodeService.getSortCode()).thenReturn(sortCode);
        when(customerService.inquireCustomer(sortCode, customerNumber))
            .thenReturn(Optional.of(customer));
        
        mockMvc.perform(get("/api/customers/{customerNumber}", customerNumber))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.name").value("Mr John Smith"))
            .andExpect(jsonPath("$.customerNumber").value(100001));
    }
    
    @Test
    void testGetCustomer_NotFound() throws Exception {
        String sortCode = "123456";
        Long customerNumber = 100001L;
        
        when(sortCodeService.getSortCode()).thenReturn(sortCode);
        when(customerService.inquireCustomer(sortCode, customerNumber))
            .thenReturn(Optional.empty());
        
        mockMvc.perform(get("/api/customers/{customerNumber}", customerNumber))
            .andExpect(status().isNotFound());
    }
    
    @Test
    void testCreateCustomer_Success() throws Exception {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .sortCode("123456")
            .name("Mr John Smith")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1980, 1, 1))
            .build();
        
        Customer createdCustomer = Customer.builder()
            .sortCode("123456")
            .customerNumber(100001L)
            .name("Mr John Smith")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1980, 1, 1))
            .creditScore(750)
            .build();
        
        when(customerService.createCustomer(any(CustomerRequestDto.class)))
            .thenReturn(createdCustomer);
        
        mockMvc.perform(post("/api/customers")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isCreated())
            .andExpect(jsonPath("$.customerNumber").value(100001))
            .andExpect(jsonPath("$.name").value("Mr John Smith"));
    }
    
    @Test
    void testUpdateCustomer_Success() throws Exception {
        String sortCode = "123456";
        Long customerNumber = 100001L;
        CustomerRequestDto request = CustomerRequestDto.builder()
            .name("Mr John Doe")
            .address("456 Oak St")
            .build();
        
        Customer updatedCustomer = Customer.builder()
            .sortCode(sortCode)
            .customerNumber(customerNumber)
            .name("Mr John Doe")
            .address("456 Oak St")
            .dateOfBirth(LocalDate.of(1980, 1, 1))
            .build();
        
        when(sortCodeService.getSortCode()).thenReturn(sortCode);
        when(customerService.updateCustomer(eq(sortCode), eq(customerNumber), anyString(), anyString()))
            .thenReturn(updatedCustomer);
        
        mockMvc.perform(put("/api/customers/{customerNumber}", customerNumber)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.name").value("Mr John Doe"))
            .andExpect(jsonPath("$.address").value("456 Oak St"));
    }
    
    @Test
    void testDeleteCustomer_Success() throws Exception {
        String sortCode = "123456";
        Long customerNumber = 100001L;
        
        when(sortCodeService.getSortCode()).thenReturn(sortCode);
        doNothing().when(customerService).deleteCustomer(sortCode, customerNumber);
        
        mockMvc.perform(delete("/api/customers/{customerNumber}", customerNumber))
            .andExpect(status().isNoContent());
        
        verify(customerService).deleteCustomer(sortCode, customerNumber);
    }
}
