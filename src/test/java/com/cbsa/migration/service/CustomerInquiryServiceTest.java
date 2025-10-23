package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerInquiryRequestDto;
import com.cbsa.migration.dto.CustomerInquiryResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

/**
 * Unit tests for CustomerInquiryService
 * Tests business logic with mocked repository
 */
@ExtendWith(MockitoExtension.class)
class CustomerInquiryServiceTest {
    
    @Mock
    private CustomerRepository customerRepository;
    
    private CustomerInquiryService service;
    
    private Customer testCustomer;
    
    @BeforeEach
    void setUp() {
        service = new CustomerInquiryService(customerRepository);
        
        testCustomer = Customer.builder()
            .eyeCatcher("CUST")
            .sortCode("987654")
            .customerNumber(12345L)
            .name("John Doe")
            .address("123 Main St, Anytown, USA")
            .dateOfBirth(LocalDate.of(1980, 5, 15))
            .creditScore(750)
            .creditScoreReviewDate(LocalDate.of(2023, 1, 1))
            .build();
    }
    
    @Test
    void inquireCustomer_StandardLookup_Success() {
        when(customerRepository.findById("987654", 12345L))
            .thenReturn(Optional.of(testCustomer));
        
        CustomerInquiryRequestDto request = CustomerInquiryRequestDto.builder()
            .sortCode("987654")
            .customerNumber(12345L)
            .build();
        
        CustomerInquiryResponseDto response = service.inquireCustomer(request);
        
        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isEqualTo(CustomerInquiryResponseDto.SUCCESS_CODE);
        assertThat(response.getEyeCatcher()).isEqualTo("CUST");
        assertThat(response.getSortCode()).isEqualTo("987654");
        assertThat(response.getCustomerNumber()).isEqualTo(12345L);
        assertThat(response.getName()).isEqualTo("John Doe");
        assertThat(response.getAddress()).isEqualTo("123 Main St, Anytown, USA");
        assertThat(response.getDateOfBirth()).isEqualTo(LocalDate.of(1980, 5, 15));
        assertThat(response.getCreditScore()).isEqualTo(750);
        assertThat(response.getCreditScoreReviewDate()).isEqualTo(LocalDate.of(2023, 1, 1));
        
        verify(customerRepository).findById("987654", 12345L);
    }
    
    @Test
    void inquireCustomer_StandardLookup_NotFound() {
        when(customerRepository.findById("987654", 99999L))
            .thenReturn(Optional.empty());
        
        CustomerInquiryRequestDto request = CustomerInquiryRequestDto.builder()
            .sortCode("987654")
            .customerNumber(99999L)
            .build();
        
        CustomerInquiryResponseDto response = service.inquireCustomer(request);
        
        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo(CustomerInquiryResponseDto.NOT_FOUND_CODE);
        assertThat(response.getSortCode()).isEqualTo("987654");
        assertThat(response.getCustomerNumber()).isEqualTo(99999L);
        assertThat(response.getName()).isNull();
        
        verify(customerRepository).findById("987654", 99999L);
    }
    
    @Test
    void inquireCustomer_LastCustomer_Success() {
        Customer lastCustomer = Customer.builder()
            .eyeCatcher("CUST")
            .sortCode("987654")
            .customerNumber(50000L)
            .name("Last Customer")
            .address("999 End St")
            .dateOfBirth(LocalDate.of(1990, 12, 31))
            .creditScore(800)
            .creditScoreReviewDate(LocalDate.of(2023, 6, 1))
            .build();
        
        when(customerRepository.findLastCustomer("987654"))
            .thenReturn(Optional.of(lastCustomer));
        when(customerRepository.findById("987654", 50000L))
            .thenReturn(Optional.of(lastCustomer));
        
        CustomerInquiryRequestDto request = CustomerInquiryRequestDto.builder()
            .sortCode("987654")
            .customerNumber(9999999999L)
            .build();
        
        CustomerInquiryResponseDto response = service.inquireCustomer(request);
        
        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isEqualTo(CustomerInquiryResponseDto.SUCCESS_CODE);
        assertThat(response.getCustomerNumber()).isEqualTo(50000L);
        assertThat(response.getName()).isEqualTo("Last Customer");
        
        verify(customerRepository).findLastCustomer("987654");
        verify(customerRepository).findById("987654", 50000L);
    }
    
    @Test
    void inquireCustomer_LastCustomer_NoCustomersExist() {
        when(customerRepository.findLastCustomer("987654"))
            .thenReturn(Optional.empty());
        
        CustomerInquiryRequestDto request = CustomerInquiryRequestDto.builder()
            .sortCode("987654")
            .customerNumber(9999999999L)
            .build();
        
        CustomerInquiryResponseDto response = service.inquireCustomer(request);
        
        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo(CustomerInquiryResponseDto.DATABASE_ERROR_CODE);
        assertThat(response.getCustomerNumber()).isEqualTo(9999999999L);
        
        verify(customerRepository).findLastCustomer("987654");
        verify(customerRepository, never()).findById(anyString(), anyLong());
    }
    
    @Test
    void inquireCustomer_RandomCustomer_Success() {
        Customer lastCustomer = Customer.builder()
            .customerNumber(100L)
            .build();
        
        when(customerRepository.findLastCustomer("987654"))
            .thenReturn(Optional.of(lastCustomer));
        when(customerRepository.findById(eq("987654"), anyLong()))
            .thenReturn(Optional.empty())
            .thenReturn(Optional.empty())
            .thenReturn(Optional.of(testCustomer));
        
        CustomerInquiryRequestDto request = CustomerInquiryRequestDto.builder()
            .sortCode("987654")
            .customerNumber(0L)
            .build();
        
        CustomerInquiryResponseDto response = service.inquireCustomer(request);
        
        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isEqualTo(CustomerInquiryResponseDto.SUCCESS_CODE);
        assertThat(response.getName()).isEqualTo("John Doe");
        
        verify(customerRepository).findLastCustomer("987654");
        verify(customerRepository, atLeast(3)).findById(eq("987654"), anyLong());
    }
    
    @Test
    void inquireCustomer_RandomCustomer_MaxRetriesExceeded() {
        Customer lastCustomer = Customer.builder()
            .customerNumber(100L)
            .build();
        
        when(customerRepository.findLastCustomer("987654"))
            .thenReturn(Optional.of(lastCustomer));
        when(customerRepository.findById(eq("987654"), anyLong()))
            .thenReturn(Optional.empty());
        
        CustomerInquiryRequestDto request = CustomerInquiryRequestDto.builder()
            .sortCode("987654")
            .customerNumber(0L)
            .build();
        
        CustomerInquiryResponseDto response = service.inquireCustomer(request);
        
        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo(CustomerInquiryResponseDto.NOT_FOUND_CODE);
        assertThat(response.getCustomerNumber()).isEqualTo(0L);
        
        verify(customerRepository).findLastCustomer("987654");
        verify(customerRepository, times(1000)).findById(eq("987654"), anyLong());
    }
    
    @Test
    void inquireCustomer_DatabaseError_ReturnsErrorCode() {
        when(customerRepository.findById("987654", 12345L))
            .thenThrow(new RuntimeException("Database connection failed"));
        
        CustomerInquiryRequestDto request = CustomerInquiryRequestDto.builder()
            .sortCode("987654")
            .customerNumber(12345L)
            .build();
        
        CustomerInquiryResponseDto response = service.inquireCustomer(request);
        
        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo(CustomerInquiryResponseDto.DATABASE_ERROR_CODE);
        assertThat(response.getSortCode()).isEqualTo("987654");
        assertThat(response.getCustomerNumber()).isEqualTo(12345L);
    }
}
