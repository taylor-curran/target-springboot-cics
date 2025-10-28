package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerRequestDto;
import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.service.CustomerCreationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.time.LocalDate;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

class CustomerControllerTest {

    @Mock
    private CustomerCreationService customerCreationService;
    
    private CustomerController customerController;
    
    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        customerController = new CustomerController(customerCreationService);
    }
    
    @Test
    void testCreateCustomer_Success() {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1990, 1, 1))
            .sortCode("123456")
            .build();
        
        CustomerResponseDto serviceResponse = CustomerResponseDto.builder()
            .name("John Doe")
            .sortCode("123456")
            .customerNumber(1000L)
            .status("ACTIVE")
            .build();
        
        when(customerCreationService.createCustomer(any(CustomerRequestDto.class)))
            .thenReturn(serviceResponse);
        
        ResponseEntity<CustomerResponseDto> response = customerController.createCustomer(request);
        
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("John Doe", response.getBody().getName());
        assertEquals(1000L, response.getBody().getCustomerNumber());
    }
    
    @Test
    void testCreateCustomer_ValidationError() {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.now().plusDays(1))
            .sortCode("123456")
            .build();
        
        when(customerCreationService.createCustomer(any(CustomerRequestDto.class)))
            .thenThrow(new IllegalArgumentException("Date of birth cannot be in the future"));
        
        ResponseEntity<CustomerResponseDto> response = customerController.createCustomer(request);
        
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().getStatus().contains("VALIDATION_FAILED"));
    }
    
    @Test
    void testCreateCustomer_InternalError() {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1990, 1, 1))
            .sortCode("123456")
            .build();
        
        when(customerCreationService.createCustomer(any(CustomerRequestDto.class)))
            .thenThrow(new RuntimeException("Database error"));
        
        ResponseEntity<CustomerResponseDto> response = customerController.createCustomer(request);
        
        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().getStatus().contains("ERROR"));
    }
}
