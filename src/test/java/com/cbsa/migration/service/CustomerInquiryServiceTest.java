package com.cbsa.migration.service;

import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.CustomerInquiryResponse;
import com.cbsa.migration.repository.CustomerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class CustomerInquiryServiceTest {

    @Mock
    private CustomerRepository customerRepository;

    @Mock
    private SortCodeService sortCodeService;

    @InjectMocks
    private CustomerInquiryService customerInquiryService;

    private Customer testCustomer;

    @BeforeEach
    void setUp() {
        testCustomer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1980, 1, 1))
                .creditScore(750)
                .creditScoreReviewDate(LocalDate.of(2023, 1, 1))
                .build();

        when(sortCodeService.getSortCode()).thenReturn("987654");
    }

    @Test
    void testInquireCustomer_DirectLookup_Success() {
        when(customerRepository.findById("987654", 1234567890L)).thenReturn(Optional.of(testCustomer));

        CustomerInquiryResponse response = customerInquiryService.inquireCustomer(1234567890L);

        assertTrue(response.isSuccess());
        assertEquals("0", response.getFailureCode());
        assertEquals("CUST", response.getEyeCatcher());
        assertEquals("987654", response.getSortCode());
        assertEquals(1234567890L, response.getCustomerNumber());
        assertEquals("John Doe", response.getName());
    }

    @Test
    void testInquireCustomer_DirectLookup_NotFound() {
        when(customerRepository.findById("987654", 1234567890L)).thenReturn(Optional.empty());

        CustomerInquiryResponse response = customerInquiryService.inquireCustomer(1234567890L);

        assertFalse(response.isSuccess());
        assertEquals("1", response.getFailureCode());
        assertEquals(1234567890L, response.getCustomerNumber());
    }

    @Test
    void testInquireCustomer_RandomCustomer_Success() {
        when(customerRepository.findAll()).thenReturn(Arrays.asList(testCustomer));

        CustomerInquiryResponse response = customerInquiryService.inquireCustomer(0L);

        assertTrue(response.isSuccess());
        assertEquals("0", response.getFailureCode());
        assertEquals("John Doe", response.getName());
    }

    @Test
    void testInquireCustomer_RandomCustomer_NoCustomers() {
        when(customerRepository.findAll()).thenReturn(Collections.emptyList());

        CustomerInquiryResponse response = customerInquiryService.inquireCustomer(0L);

        assertFalse(response.isSuccess());
        assertEquals("1", response.getFailureCode());
    }

    @Test
    void testInquireCustomer_LastCustomer_Success() {
        Customer customer1 = Customer.builder()
                .customerNumber(1000L)
                .name("Customer 1")
                .build();
        Customer customer2 = Customer.builder()
                .customerNumber(2000000000L)
                .name("Customer 2")
                .build();

        when(customerRepository.findAll()).thenReturn(Arrays.asList(customer1, testCustomer, customer2));

        CustomerInquiryResponse response = customerInquiryService.inquireCustomer(9999999999L);

        assertTrue(response.isSuccess());
        assertEquals("0", response.getFailureCode());
        assertEquals(2000000000L, response.getCustomerNumber());
        assertEquals("Customer 2", response.getName());
    }

    @Test
    void testInquireCustomer_LastCustomer_NoCustomers() {
        when(customerRepository.findAll()).thenReturn(Collections.emptyList());

        CustomerInquiryResponse response = customerInquiryService.inquireCustomer(9999999999L);

        assertFalse(response.isSuccess());
        assertEquals("9", response.getFailureCode());
    }
}
