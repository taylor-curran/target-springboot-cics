package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerDTO;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.service.CustomerService.InquiryResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

/**
 * Unit tests for CustomerService (INQCUST business logic).
 * Tests composite key lookups, random/last customer modes, error handling,
 * and list/search operations.
 */
@ExtendWith(MockitoExtension.class)
class CustomerServiceTest {

    @Mock
    private CustomerRepository customerRepository;

    @InjectMocks
    private CustomerService customerService;

    private Customer testCustomer1;
    private Customer testCustomer2;
    private Customer testCustomer3;

    @BeforeEach
    void setUp() {
        testCustomer1 = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(1L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 1, 15))
                .creditScore(750)
                .creditScoreReviewDate(LocalDate.of(2024, 6, 1))
                .build();

        testCustomer2 = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(50L)
                .name("Jane Smith")
                .address("456 Oak Ave")
                .dateOfBirth(LocalDate.of(1985, 5, 20))
                .creditScore(680)
                .creditScoreReviewDate(LocalDate.of(2024, 3, 15))
                .build();

        testCustomer3 = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(100L)
                .name("Bob Johnson")
                .address("789 Pine Rd")
                .dateOfBirth(LocalDate.of(1975, 11, 30))
                .creditScore(820)
                .creditScoreReviewDate(null)
                .build();
    }

    // ========== Direct Lookup Tests ==========

    @Test
    void inquireCustomer_specificCustomer_found() {
        // Given
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.of(testCustomer1));

        // When
        InquiryResult result = customerService.inquireCustomer("987654", 1L);

        // Then
        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getFailCode()).isEqualTo(CustomerService.FAIL_CODE_SUCCESS);
        assertThat(result.getCustomer()).isNotNull();
        assertThat(result.getCustomer().getCustomerNumber()).isEqualTo(1L);
        assertThat(result.getCustomer().getName()).isEqualTo("John Doe");
        assertThat(result.getCustomer().getEyeCatcher()).isEqualTo("CUST");
        assertThat(result.getCustomer().getSortCode()).isEqualTo("987654");
        verify(customerRepository).findById("987654", 1L);
    }

    @Test
    void inquireCustomer_specificCustomer_notFound() {
        // Given
        when(customerRepository.findById("987654", 999L)).thenReturn(Optional.empty());

        // When
        InquiryResult result = customerService.inquireCustomer("987654", 999L);

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo(CustomerService.FAIL_CODE_NOT_FOUND);
        assertThat(result.getCustomer()).isNull();
    }

    @Test
    void inquireCustomer_specificCustomer_allFieldsMapped() {
        // Given
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.of(testCustomer1));

        // When
        InquiryResult result = customerService.inquireCustomer("987654", 1L);

        // Then
        CustomerDTO dto = result.getCustomer();
        assertThat(dto.getEyeCatcher()).isEqualTo("CUST");
        assertThat(dto.getSortCode()).isEqualTo("987654");
        assertThat(dto.getCustomerNumber()).isEqualTo(1L);
        assertThat(dto.getName()).isEqualTo("John Doe");
        assertThat(dto.getAddress()).isEqualTo("123 Main St");
        assertThat(dto.getDateOfBirth()).isEqualTo(LocalDate.of(1990, 1, 15));
        assertThat(dto.getCreditScore()).isEqualTo(750);
        assertThat(dto.getCreditScoreReviewDate()).isEqualTo(LocalDate.of(2024, 6, 1));
    }

    @Test
    void inquireCustomer_specificCustomer_nullReviewDate() {
        // Given - testCustomer3 has null creditScoreReviewDate
        when(customerRepository.findById("987654", 100L)).thenReturn(Optional.of(testCustomer3));

        // When
        InquiryResult result = customerService.inquireCustomer("987654", 100L);

        // Then
        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getCustomer().getCreditScoreReviewDate()).isNull();
    }

    // ========== Random Customer Tests (customerNumber = 0) ==========

    @Test
    void inquireCustomer_randomCustomer_returnsRandomFromList() {
        // Given
        when(customerRepository.findAll()).thenReturn(Arrays.asList(testCustomer1, testCustomer2, testCustomer3));

        // When
        InquiryResult result = customerService.inquireCustomer("987654", 0L);

        // Then
        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getFailCode()).isEqualTo(CustomerService.FAIL_CODE_SUCCESS);
        assertThat(result.getCustomer()).isNotNull();
        // Should be one of the three test customers
        assertThat(result.getCustomer().getCustomerNumber()).isIn(1L, 50L, 100L);
        verify(customerRepository).findAll();
        verify(customerRepository, never()).findById(anyString(), anyLong());
    }

    @Test
    void inquireCustomer_randomCustomer_emptyDatabase() {
        // Given
        when(customerRepository.findAll()).thenReturn(Collections.emptyList());

        // When
        InquiryResult result = customerService.inquireCustomer("987654", 0L);

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo(CustomerService.FAIL_CODE_NOT_FOUND);
        assertThat(result.getCustomer()).isNull();
    }

    @Test
    void inquireCustomer_randomCustomer_singleCustomerInDb() {
        // Given
        when(customerRepository.findAll()).thenReturn(Collections.singletonList(testCustomer1));

        // When
        InquiryResult result = customerService.inquireCustomer("987654", 0L);

        // Then
        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getCustomer().getCustomerNumber()).isEqualTo(1L);
    }

    // ========== Last Customer Tests (customerNumber = 9999999999) ==========

    @Test
    void inquireCustomer_lastCustomer_returnsHighestNumber() {
        // Given
        when(customerRepository.findAll()).thenReturn(Arrays.asList(testCustomer1, testCustomer2, testCustomer3));

        // When
        InquiryResult result = customerService.inquireCustomer("987654", 9999999999L);

        // Then
        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getCustomer().getCustomerNumber()).isEqualTo(100L);
        assertThat(result.getCustomer().getName()).isEqualTo("Bob Johnson");
    }

    @Test
    void inquireCustomer_lastCustomer_emptyDatabase() {
        // Given
        when(customerRepository.findAll()).thenReturn(Collections.emptyList());

        // When
        InquiryResult result = customerService.inquireCustomer("987654", 9999999999L);

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo(CustomerService.FAIL_CODE_NOT_FOUND);
    }

    @Test
    void inquireCustomer_lastCustomer_singleCustomer() {
        // Given
        when(customerRepository.findAll()).thenReturn(Collections.singletonList(testCustomer2));

        // When
        InquiryResult result = customerService.inquireCustomer("987654", 9999999999L);

        // Then
        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getCustomer().getCustomerNumber()).isEqualTo(50L);
    }

    // ========== System Error Tests ==========

    @Test
    void inquireCustomer_repositoryThrowsException_returnsSystemError() {
        // Given
        when(customerRepository.findById("987654", 1L))
                .thenThrow(new RuntimeException("Database connection failed"));

        // When
        InquiryResult result = customerService.inquireCustomer("987654", 1L);

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo(CustomerService.FAIL_CODE_SYSTEM_ERROR);
        assertThat(result.getCustomer()).isNull();
    }

    @Test
    void inquireCustomer_randomCustomer_repositoryThrowsException() {
        // Given
        when(customerRepository.findAll()).thenThrow(new RuntimeException("Timeout"));

        // When
        InquiryResult result = customerService.inquireCustomer("987654", 0L);

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo(CustomerService.FAIL_CODE_SYSTEM_ERROR);
    }

    @Test
    void inquireCustomer_lastCustomer_repositoryThrowsException() {
        // Given
        when(customerRepository.findAll()).thenThrow(new RuntimeException("IO Error"));

        // When
        InquiryResult result = customerService.inquireCustomer("987654", 9999999999L);

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo(CustomerService.FAIL_CODE_SYSTEM_ERROR);
    }

    // ========== List/Search Tests ==========

    @Test
    void listCustomers_returnsAll() {
        // Given
        when(customerRepository.findAll()).thenReturn(Arrays.asList(testCustomer1, testCustomer2, testCustomer3));

        // When
        List<CustomerDTO> result = customerService.listCustomers();

        // Then
        assertThat(result).hasSize(3);
        assertThat(result.get(0).getName()).isEqualTo("John Doe");
        assertThat(result.get(1).getName()).isEqualTo("Jane Smith");
        assertThat(result.get(2).getName()).isEqualTo("Bob Johnson");
    }

    @Test
    void listCustomers_emptyDatabase() {
        // Given
        when(customerRepository.findAll()).thenReturn(Collections.emptyList());

        // When
        List<CustomerDTO> result = customerService.listCustomers();

        // Then
        assertThat(result).isEmpty();
    }

    @Test
    void searchCustomersByName_found() {
        // Given
        when(customerRepository.findByNameContaining("Doe"))
                .thenReturn(Collections.singletonList(testCustomer1));

        // When
        List<CustomerDTO> result = customerService.searchCustomersByName("Doe");

        // Then
        assertThat(result).hasSize(1);
        assertThat(result.get(0).getName()).isEqualTo("John Doe");
    }

    @Test
    void searchCustomersByName_notFound() {
        // Given
        when(customerRepository.findByNameContaining("Nobody"))
                .thenReturn(Collections.emptyList());

        // When
        List<CustomerDTO> result = customerService.searchCustomersByName("Nobody");

        // Then
        assertThat(result).isEmpty();
    }

    @Test
    void searchCustomersByName_multipleResults() {
        // Given
        when(customerRepository.findByNameContaining("J"))
                .thenReturn(Arrays.asList(testCustomer1, testCustomer2, testCustomer3));

        // When
        List<CustomerDTO> result = customerService.searchCustomersByName("J");

        // Then
        assertThat(result).hasSize(3);
    }

    // ========== Count Tests ==========

    @Test
    void getCustomerCount_returnsCount() {
        // Given
        when(customerRepository.count()).thenReturn(42);

        // When
        int count = customerService.getCustomerCount();

        // Then
        assertThat(count).isEqualTo(42);
    }

    @Test
    void getCustomerCount_emptyDatabase() {
        // Given
        when(customerRepository.count()).thenReturn(0);

        // When
        int count = customerService.getCustomerCount();

        // Then
        assertThat(count).isEqualTo(0);
    }

    // ========== InquiryResult Tests ==========

    @Test
    void inquiryResult_success_hasCorrectProperties() {
        CustomerDTO dto = CustomerDTO.builder().name("Test").build();
        InquiryResult result = InquiryResult.success(dto);

        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getFailCode()).isEqualTo("0");
        assertThat(result.getCustomer()).isEqualTo(dto);
    }

    @Test
    void inquiryResult_failure_hasCorrectProperties() {
        InquiryResult result = InquiryResult.failure("1");

        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("1");
        assertThat(result.getCustomer()).isNull();
    }

    @Test
    void inquiryResult_failure_systemError() {
        InquiryResult result = InquiryResult.failure("2");

        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("2");
        assertThat(result.getCustomer()).isNull();
    }

    // ========== Constants Tests ==========

    @Test
    void constants_haveCorrectValues() {
        assertThat(CustomerService.FAIL_CODE_SUCCESS).isEqualTo("0");
        assertThat(CustomerService.FAIL_CODE_NOT_FOUND).isEqualTo("1");
        assertThat(CustomerService.FAIL_CODE_SYSTEM_ERROR).isEqualTo("2");
        assertThat(CustomerService.CUSTOMER_NUMBER_RANDOM).isEqualTo(0L);
        assertThat(CustomerService.CUSTOMER_NUMBER_LAST).isEqualTo(9999999999L);
    }
}
