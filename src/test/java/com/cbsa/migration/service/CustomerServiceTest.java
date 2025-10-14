package com.cbsa.migration.service;

import com.cbsa.migration.dto.InquiryCustomerResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class CustomerServiceTest {

    @Mock
    private CustomerRepository customerRepository;

    @InjectMocks
    private CustomerService customerService;

    private Customer sampleCustomer;
    private String sortCode;
    private Long customerNumber;

    @BeforeEach
    void setUp() {
        sortCode = "987654";
        customerNumber = 100001L;

        sampleCustomer = Customer.builder()
            .eyeCatcher("CUST")
            .sortCode(sortCode)
            .customerNumber(customerNumber)
            .name("John Doe")
            .address("123 Main St, London, UK")
            .dateOfBirth(LocalDate.of(1990, 1, 15))
            .creditScore(750)
            .creditScoreReviewDate(LocalDate.of(2024, 1, 1))
            .build();
    }

    @Test
    void findCustomer_WhenCustomerExists_ReturnsSuccessResponse() {
        when(customerRepository.findById(sortCode, customerNumber))
            .thenReturn(Optional.of(sampleCustomer));

        InquiryCustomerResponseDto response = customerService.findCustomer(sortCode, customerNumber);

        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getFailureCode()).isEqualTo("0");
        assertThat(response.getEyeCatcher()).isEqualTo("CUST");
        assertThat(response.getSortCode()).isEqualTo(sortCode);
        assertThat(response.getCustomerNumber()).isEqualTo(customerNumber);
        assertThat(response.getName()).isEqualTo("John Doe");
        assertThat(response.getAddress()).isEqualTo("123 Main St, London, UK");
        assertThat(response.getDateOfBirth()).isEqualTo(LocalDate.of(1990, 1, 15));
        assertThat(response.getCreditScore()).isEqualTo(750);
        assertThat(response.getCreditScoreReviewDate()).isEqualTo(LocalDate.of(2024, 1, 1));
    }

    @Test
    void findCustomer_WhenCustomerNotFound_ReturnsNotFoundResponse() {
        when(customerRepository.findById(sortCode, customerNumber))
            .thenReturn(Optional.empty());

        InquiryCustomerResponseDto response = customerService.findCustomer(sortCode, customerNumber);

        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isFalse();
        assertThat(response.getFailureCode()).isEqualTo("1");
        assertThat(response.getSortCode()).isEqualTo(sortCode);
        assertThat(response.getCustomerNumber()).isEqualTo(customerNumber);
        assertThat(response.getErrorMessage()).isEqualTo("Customer not found");
        assertThat(response.getName()).isNull();
        assertThat(response.getAddress()).isNull();
    }

    @Test
    void findCustomer_WhenRepositoryThrowsException_ReturnsSystemErrorResponse() {
        when(customerRepository.findById(sortCode, customerNumber))
            .thenThrow(new RuntimeException("Database connection failed"));

        InquiryCustomerResponseDto response = customerService.findCustomer(sortCode, customerNumber);

        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isFalse();
        assertThat(response.getFailureCode()).isEqualTo("2");
        assertThat(response.getSortCode()).isEqualTo(sortCode);
        assertThat(response.getCustomerNumber()).isEqualTo(customerNumber);
        assertThat(response.getErrorMessage()).contains("System error");
        assertThat(response.getErrorMessage()).contains("Database connection failed");
    }

    @Test
    void findCustomer_WithNullCreditScoreReviewDate_ReturnsSuccessfully() {
        sampleCustomer.setCreditScoreReviewDate(null);
        when(customerRepository.findById(sortCode, customerNumber))
            .thenReturn(Optional.of(sampleCustomer));

        InquiryCustomerResponseDto response = customerService.findCustomer(sortCode, customerNumber);

        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getCreditScoreReviewDate()).isNull();
    }

    @Test
    void findCustomer_WithDifferentSortCode_UsesCorrectParameters() {
        String differentSortCode = "123456";
        Long differentCustomerNumber = 200002L;

        Customer differentCustomer = Customer.builder()
            .eyeCatcher("CUST")
            .sortCode(differentSortCode)
            .customerNumber(differentCustomerNumber)
            .name("Jane Smith")
            .address("456 Oak Ave, Manchester, UK")
            .dateOfBirth(LocalDate.of(1985, 5, 20))
            .creditScore(820)
            .creditScoreReviewDate(LocalDate.of(2024, 2, 1))
            .build();

        when(customerRepository.findById(differentSortCode, differentCustomerNumber))
            .thenReturn(Optional.of(differentCustomer));

        InquiryCustomerResponseDto response = customerService.findCustomer(differentSortCode, differentCustomerNumber);

        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getSortCode()).isEqualTo(differentSortCode);
        assertThat(response.getCustomerNumber()).isEqualTo(differentCustomerNumber);
        assertThat(response.getName()).isEqualTo("Jane Smith");
    }
}
