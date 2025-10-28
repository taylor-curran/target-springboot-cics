package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerRequestDto;
import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.dto.CreditScoreRequestDto;
import com.cbsa.migration.dto.CreditScoreResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.time.LocalDate;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

class CustomerCreationServiceTest {

    @Mock
    private CustomerRepository customerRepository;
    
    @Mock
    private ControlRepository controlRepository;
    
    @Mock
    private TransactionRepository transactionRepository;
    
    @Mock
    private CreditAgencyService creditAgencyService;
    
    private CustomerCreationService customerCreationService;
    
    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        customerCreationService = new CustomerCreationService(
            customerRepository, controlRepository, transactionRepository, creditAgencyService);
    }
    
    @Test
    void testCreateCustomer_Success() {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1990, 1, 1))
            .sortCode("123456")
            .build();
        
        when(controlRepository.getNextCustomerNumber()).thenReturn(1000L);
        
        CreditScoreResponseDto creditResponse = CreditScoreResponseDto.builder()
            .success(true)
            .updatedCreditScore(750)
            .build();
        when(creditAgencyService.processCredit(any(CreditScoreRequestDto.class)))
            .thenReturn(creditResponse);
        
        Customer savedCustomer = Customer.builder()
            .eyeCatcher("CUST")
            .sortCode("123456")
            .customerNumber(1000L)
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1990, 1, 1))
            .creditScore(750)
            .build();
        when(customerRepository.save(any(Customer.class))).thenReturn(savedCustomer);
        
        CustomerResponseDto response = customerCreationService.createCustomer(request);
        
        assertNotNull(response);
        assertEquals("John Doe", response.getName());
        assertEquals("123456", response.getSortCode());
        assertEquals(1000L, response.getCustomerNumber());
        assertEquals("ACTIVE", response.getStatus());
        
        verify(customerRepository).save(any(Customer.class));
        verify(transactionRepository).save(any());
    }
    
    @Test
    void testValidateDateOfBirth_YearTooEarly() {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1600, 1, 1))
            .sortCode("123456")
            .build();
        
        when(controlRepository.getNextCustomerNumber()).thenReturn(1000L);
        
        assertThrows(IllegalArgumentException.class, 
            () -> customerCreationService.createCustomer(request));
    }
    
    @Test
    void testValidateDateOfBirth_Future() {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.now().plusDays(1))
            .sortCode("123456")
            .build();
        
        when(controlRepository.getNextCustomerNumber()).thenReturn(1000L);
        
        assertThrows(IllegalArgumentException.class, 
            () -> customerCreationService.createCustomer(request));
    }
    
    @Test
    void testValidateDateOfBirth_TooOld() {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.now().minusYears(151))
            .sortCode("123456")
            .build();
        
        when(controlRepository.getNextCustomerNumber()).thenReturn(1000L);
        
        assertThrows(IllegalArgumentException.class, 
            () -> customerCreationService.createCustomer(request));
    }
    
    @Test
    void testCreateCustomer_CreditCheckFails() {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1990, 1, 1))
            .sortCode("123456")
            .build();
        
        when(controlRepository.getNextCustomerNumber()).thenReturn(1000L);
        
        CreditScoreResponseDto creditResponse = CreditScoreResponseDto.builder()
            .success(false)
            .errorMessage("Credit check failed")
            .build();
        when(creditAgencyService.processCredit(any(CreditScoreRequestDto.class)))
            .thenReturn(creditResponse);
        
        ArgumentCaptor<Customer> customerCaptor = ArgumentCaptor.forClass(Customer.class);
        
        CustomerResponseDto response = customerCreationService.createCustomer(request);
        
        verify(customerRepository).save(customerCaptor.capture());
        Customer savedCustomer = customerCaptor.getValue();
        assertEquals(0, savedCustomer.getCreditScore());
        assertNotNull(savedCustomer.getCreditScoreReviewDate());
    }
}
