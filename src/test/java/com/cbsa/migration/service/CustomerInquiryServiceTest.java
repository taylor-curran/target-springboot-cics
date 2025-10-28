package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerInquiryResponseDto;
import com.cbsa.migration.model.Control;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CustomerInquiryServiceTest {

    @Mock
    private CustomerRepository customerRepository;

    @Mock
    private ControlRepository controlRepository;

    private CustomerInquiryService customerInquiryService;

    private Customer testCustomer;
    private Control testControl;

    @BeforeEach
    void setUp() {
        customerInquiryService = new CustomerInquiryService(customerRepository, controlRepository);

        testCustomer = Customer.builder()
            .eyeCatcher("CUST")
            .sortCode("987654")
            .customerNumber(1000001L)
            .name("John Doe")
            .address("123 Main St, London")
            .dateOfBirth(LocalDate.of(1980, 1, 15))
            .creditScore(750)
            .creditScoreReviewDate(LocalDate.of(2024, 1, 1))
            .build();

        testControl = Control.builder()
            .customerCount(100L)
            .lastCustomerNumber(1000100L)
            .accountCount(200)
            .lastAccountNumber(20000000)
            .build();
    }

    @Test
    void testInquireCustomer_Success() {
        when(customerRepository.findById("987654", 1000001L))
            .thenReturn(Optional.of(testCustomer));

        CustomerInquiryResponseDto response = customerInquiryService.inquireCustomer("987654", 1000001L);

        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getFailureCode()).isEqualTo("0");
        assertThat(response.getCustomerNumber()).isEqualTo(1000001L);
        assertThat(response.getName()).isEqualTo("John Doe");
        assertThat(response.getAddress()).isEqualTo("123 Main St, London");
        assertThat(response.getCreditScore()).isEqualTo(750);
        
        verify(customerRepository).findById("987654", 1000001L);
    }

    @Test
    void testInquireCustomer_NotFound() {
        when(customerRepository.findById("987654", 9999999L))
            .thenReturn(Optional.empty());

        CustomerInquiryResponseDto response = customerInquiryService.inquireCustomer("987654", 9999999L);

        assertThat(response.getSuccess()).isFalse();
        assertThat(response.getFailureCode()).isEqualTo("1");
        assertThat(response.getErrorMessage()).isEqualTo("Customer not found");
        assertThat(response.getCustomerNumber()).isEqualTo(9999999L);
        
        verify(customerRepository).findById("987654", 9999999L);
    }

    @Test
    void testInquireCustomer_RandomCustomer() {
        when(controlRepository.getControl()).thenReturn(Optional.of(testControl));
        when(customerRepository.findById(eq("987654"), any(Long.class)))
            .thenReturn(Optional.of(testCustomer));

        CustomerInquiryResponseDto response = customerInquiryService.inquireCustomer("987654", 0L);

        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getFailureCode()).isEqualTo("0");
        assertThat(response.getName()).isEqualTo("John Doe");
        
        verify(controlRepository).getControl();
        verify(customerRepository, atLeastOnce()).findById(eq("987654"), any(Long.class));
    }

    @Test
    void testInquireCustomer_LastCustomer() {
        when(controlRepository.getControl()).thenReturn(Optional.of(testControl));
        when(customerRepository.findById("987654", 1000100L))
            .thenReturn(Optional.of(testCustomer));

        CustomerInquiryResponseDto response = customerInquiryService.inquireCustomer("987654", 9999999999L);

        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getFailureCode()).isEqualTo("0");
        
        verify(controlRepository).getControl();
        verify(customerRepository).findById("987654", 1000100L);
    }

    @Test
    void testInquireCustomer_LastCustomer_NotFound() {
        when(controlRepository.getControl()).thenReturn(Optional.of(testControl));
        when(customerRepository.findById("987654", 1000100L))
            .thenReturn(Optional.empty());

        CustomerInquiryResponseDto response = customerInquiryService.inquireCustomer("987654", 9999999999L);

        assertThat(response.getSuccess()).isFalse();
        assertThat(response.getFailureCode()).isEqualTo("1");
        
        verify(controlRepository).getControl();
        verify(customerRepository).findById("987654", 1000100L);
    }

    @Test
    void testInquireCustomer_RandomCustomer_NoCustomers() {
        Control emptyControl = Control.builder()
            .customerCount(0L)
            .lastCustomerNumber(0L)
            .accountCount(0)
            .lastAccountNumber(0)
            .build();
        when(controlRepository.getControl()).thenReturn(Optional.of(emptyControl));

        CustomerInquiryResponseDto response = customerInquiryService.inquireCustomer("987654", 0L);

        assertThat(response.getSuccess()).isFalse();
        assertThat(response.getFailureCode()).isEqualTo("9");
        assertThat(response.getErrorMessage()).contains("No customers available");
        
        verify(controlRepository).getControl();
    }

    @Test
    void testInquireCustomer_ControlRecordMissing_Initializes() {
        when(controlRepository.getControl()).thenReturn(Optional.empty());
        when(controlRepository.initializeControlRecord()).thenReturn(testControl);
        when(customerRepository.findById(eq("987654"), any(Long.class)))
            .thenReturn(Optional.of(testCustomer));

        CustomerInquiryResponseDto response = customerInquiryService.inquireCustomer("987654", 0L);

        verify(controlRepository).getControl();
        verify(controlRepository).initializeControlRecord();
    }
}
