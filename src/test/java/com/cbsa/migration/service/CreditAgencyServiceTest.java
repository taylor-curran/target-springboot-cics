package com.cbsa.migration.service;

import com.cbsa.migration.dto.CreditScoreRequestDto;
import com.cbsa.migration.dto.CreditScoreResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Unit tests for CreditAgencyService
 * Tests credit score processing business logic
 */
@ExtendWith(MockitoExtension.class)
class CreditAgencyServiceTest {

    @Mock
    private CustomerRepository customerRepository;

    private CreditAgencyService creditAgencyService;

    @BeforeEach
    void setUp() {
        creditAgencyService = new CreditAgencyService(
            customerRepository,
            false, // delayEnabled = false for faster tests
            3,     // maxDelaySeconds
            300,   // minScore
            850    // maxScore
        );
    }

    @Test
    @DisplayName("Should process credit successfully for existing customer")
    void shouldProcessCreditSuccessfullyForExistingCustomer() {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .currentCreditScore(700)
                .build();

        Customer customer = new Customer();
        customer.setCustomerNumber(1234567890L);
        customer.setSortCode("987654");
        customer.setName("John Doe");
        customer.setAddress("123 Main St");
        customer.setDateOfBirth(LocalDate.of(1990, 5, 15));
        customer.setCreditScore(700);

        when(customerRepository.findById("987654", 1234567890L))
                .thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class)))
                .thenAnswer(invocation -> {
                    Customer savedCustomer = invocation.getArgument(0);
                    savedCustomer.setCreditScoreReviewDate(LocalDate.now());
                    return savedCustomer;
                });

        // When
        CreditScoreResponseDto response = creditAgencyService.processCredit(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getSortCode()).isEqualTo("987654");
        assertThat(response.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(response.getUpdatedCreditScore()).isBetween(300, 850);
        assertThat(response.getProcessingTimeMs()).isGreaterThanOrEqualTo(0L);
        assertThat(response.getErrorMessage()).isNull();
    }

    @Test
    @DisplayName("Should return error when customer not found")
    void shouldReturnErrorWhenCustomerNotFound() {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(9999999999L)
                .name("Unknown Customer")
                .address("Unknown Address")
                .dateOfBirth(LocalDate.of(1990, 1, 1))
                .build();

        when(customerRepository.findById("987654", 9999999999L))
                .thenReturn(Optional.empty());

        // When
        CreditScoreResponseDto response = creditAgencyService.processCredit(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isFalse();
        assertThat(response.getSortCode()).isEqualTo("987654");
        assertThat(response.getCustomerNumber()).isEqualTo(9999999999L);
        assertThat(response.getUpdatedCreditScore()).isNull();
        assertThat(response.getProcessingTimeMs()).isGreaterThanOrEqualTo(0L);
        assertThat(response.getErrorMessage()).isEqualTo("Customer not found");
    }

    @Test
    @DisplayName("Should return error when sort code mismatch")
    void shouldReturnErrorWhenSortCodeMismatch() {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("123456")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .build();

        // Repository should return empty for wrong sort code
        when(customerRepository.findById("123456", 1234567890L))
                .thenReturn(Optional.empty());

        // When
        CreditScoreResponseDto response = creditAgencyService.processCredit(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isFalse();
        assertThat(response.getSortCode()).isEqualTo("123456");
        assertThat(response.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(response.getUpdatedCreditScore()).isNull();
        assertThat(response.getProcessingTimeMs()).isGreaterThanOrEqualTo(0L);
        assertThat(response.getErrorMessage()).isEqualTo("Customer not found");
    }

    @Test
    @DisplayName("Should handle customer with high credit score")
    void shouldHandleCustomerWithHighCreditScore() {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("Jane Smith")
                .address("456 Oak Ave")
                .dateOfBirth(LocalDate.of(1985, 8, 20))
                .currentCreditScore(820)
                .build();

        Customer customer = new Customer();
        customer.setCustomerNumber(1234567890L);
        customer.setSortCode("987654");
        customer.setName("Jane Smith");
        customer.setAddress("456 Oak Ave");
        customer.setDateOfBirth(LocalDate.of(1985, 8, 20));
        customer.setCreditScore(820);

        when(customerRepository.findById("987654", 1234567890L))
                .thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class)))
                .thenAnswer(invocation -> {
                    Customer savedCustomer = invocation.getArgument(0);
                    savedCustomer.setCreditScoreReviewDate(LocalDate.now());
                    return savedCustomer;
                });

        // When
        CreditScoreResponseDto response = creditAgencyService.processCredit(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getUpdatedCreditScore()).isBetween(300, 850);
        assertThat(response.getProcessingTimeMs()).isGreaterThanOrEqualTo(0L);
    }

    @Test
    @DisplayName("Should handle customer with low credit score")
    void shouldHandleCustomerWithLowCreditScore() {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("Bob Johnson")
                .address("789 Pine St")
                .dateOfBirth(LocalDate.of(1995, 12, 10))
                .currentCreditScore(350)
                .build();

        Customer customer = new Customer();
        customer.setCustomerNumber(1234567890L);
        customer.setSortCode("987654");
        customer.setName("Bob Johnson");
        customer.setAddress("789 Pine St");
        customer.setDateOfBirth(LocalDate.of(1995, 12, 10));
        customer.setCreditScore(350);

        when(customerRepository.findById("987654", 1234567890L))
                .thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class)))
                .thenAnswer(invocation -> {
                    Customer savedCustomer = invocation.getArgument(0);
                    savedCustomer.setCreditScoreReviewDate(LocalDate.now());
                    return savedCustomer;
                });

        // When
        CreditScoreResponseDto response = creditAgencyService.processCredit(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getUpdatedCreditScore()).isBetween(300, 850);
        assertThat(response.getProcessingTimeMs()).isGreaterThanOrEqualTo(0L);
    }

    @Test
    @DisplayName("Should handle null credit score")
    void shouldHandleNullCreditScore() {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("Alice Brown")
                .address("321 Elm Dr")
                .dateOfBirth(LocalDate.of(1988, 3, 25))
                .build();

        Customer customer = new Customer();
        customer.setCustomerNumber(1234567890L);
        customer.setSortCode("987654");
        customer.setName("Alice Brown");
        customer.setAddress("321 Elm Dr");
        customer.setDateOfBirth(LocalDate.of(1988, 3, 25));
        customer.setCreditScore(null);

        when(customerRepository.findById("987654", 1234567890L))
                .thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class)))
                .thenAnswer(invocation -> {
                    Customer savedCustomer = invocation.getArgument(0);
                    savedCustomer.setCreditScoreReviewDate(LocalDate.now());
                    return savedCustomer;
                });

        // When
        CreditScoreResponseDto response = creditAgencyService.processCredit(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getUpdatedCreditScore()).isBetween(300, 850);
        assertThat(response.getProcessingTimeMs()).isGreaterThanOrEqualTo(0L);
    }

    @Test
    @DisplayName("Should simulate processing delay")
    void shouldSimulateProcessingDelay() {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("Test User")
                .address("555 Test Blvd")
                .dateOfBirth(LocalDate.of(1992, 7, 8))
                .currentCreditScore(650)
                .build();

        Customer customer = new Customer();
        customer.setCustomerNumber(1234567890L);
        customer.setSortCode("987654");
        customer.setName("Test User");
        customer.setAddress("555 Test Blvd");
        customer.setDateOfBirth(LocalDate.of(1992, 7, 8));
        customer.setCreditScore(650);

        when(customerRepository.findById("987654", 1234567890L))
                .thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class)))
                .thenAnswer(invocation -> {
                    Customer savedCustomer = invocation.getArgument(0);
                    savedCustomer.setCreditScoreReviewDate(LocalDate.now());
                    return savedCustomer;
                });

        // When
        long startTime = System.currentTimeMillis();
        CreditScoreResponseDto response = creditAgencyService.processCredit(request);
        long endTime = System.currentTimeMillis();

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getProcessingTimeMs()).isGreaterThanOrEqualTo(0L);
        assertThat(endTime - startTime).isGreaterThanOrEqualTo(response.getProcessingTimeMs());
    }

    @Test
    @DisplayName("Should return error when name mismatch")
    void shouldReturnErrorWhenNameMismatch() {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("Wrong Name")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .build();

        Customer customer = new Customer();
        customer.setCustomerNumber(1234567890L);
        customer.setSortCode("987654");
        customer.setName("John Doe"); // Different name
        customer.setAddress("123 Main St");
        customer.setDateOfBirth(LocalDate.of(1990, 5, 15));
        customer.setCreditScore(700);

        when(customerRepository.findById("987654", 1234567890L))
                .thenReturn(Optional.of(customer));

        // When
        CreditScoreResponseDto response = creditAgencyService.processCredit(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isFalse();
        assertThat(response.getErrorMessage()).isEqualTo("Customer data mismatch");
    }

    @Test
    @DisplayName("Should return error when address mismatch")
    void shouldReturnErrorWhenAddressMismatch() {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("Wrong Address")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .build();

        Customer customer = new Customer();
        customer.setCustomerNumber(1234567890L);
        customer.setSortCode("987654");
        customer.setName("John Doe");
        customer.setAddress("123 Main St"); // Different address
        customer.setDateOfBirth(LocalDate.of(1990, 5, 15));
        customer.setCreditScore(700);

        when(customerRepository.findById("987654", 1234567890L))
                .thenReturn(Optional.of(customer));

        // When
        CreditScoreResponseDto response = creditAgencyService.processCredit(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isFalse();
        assertThat(response.getErrorMessage()).isEqualTo("Customer data mismatch");
    }

    @Test
    @DisplayName("Should return error when date of birth mismatch")
    void shouldReturnErrorWhenDateOfBirthMismatch() {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1985, 1, 1)) // Different date
                .build();

        Customer customer = new Customer();
        customer.setCustomerNumber(1234567890L);
        customer.setSortCode("987654");
        customer.setName("John Doe");
        customer.setAddress("123 Main St");
        customer.setDateOfBirth(LocalDate.of(1990, 5, 15));
        customer.setCreditScore(700);

        when(customerRepository.findById("987654", 1234567890L))
                .thenReturn(Optional.of(customer));

        // When
        CreditScoreResponseDto response = creditAgencyService.processCredit(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isFalse();
        assertThat(response.getErrorMessage()).isEqualTo("Customer data mismatch");
    }

    @Test
    @DisplayName("Should handle repository save exception")
    void shouldHandleRepositorySaveException() {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .currentCreditScore(700)
                .build();

        Customer customer = new Customer();
        customer.setCustomerNumber(1234567890L);
        customer.setSortCode("987654");
        customer.setName("John Doe");
        customer.setAddress("123 Main St");
        customer.setDateOfBirth(LocalDate.of(1990, 5, 15));
        customer.setCreditScore(700);

        when(customerRepository.findById("987654", 1234567890L))
                .thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class)))
                .thenThrow(new RuntimeException("Database error"));

        // When
        CreditScoreResponseDto response = creditAgencyService.processCredit(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isFalse();
        assertThat(response.getErrorMessage()).contains("error");
    }

    @Test
    @DisplayName("Should test with delay enabled")
    void shouldTestWithDelayEnabled() {
        CreditAgencyService delayEnabledService = new CreditAgencyService(
            customerRepository,
            true,  // delayEnabled = true
            1,     // maxDelaySeconds = 1 for faster test
            300,   // minScore
            850    // maxScore
        );

        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .currentCreditScore(700)
                .build();

        Customer customer = new Customer();
        customer.setCustomerNumber(1234567890L);
        customer.setSortCode("987654");
        customer.setName("John Doe");
        customer.setAddress("123 Main St");
        customer.setDateOfBirth(LocalDate.of(1990, 5, 15));
        customer.setCreditScore(700);

        when(customerRepository.findById("987654", 1234567890L))
                .thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class)))
                .thenAnswer(invocation -> {
                    Customer savedCustomer = invocation.getArgument(0);
                    savedCustomer.setCreditScoreReviewDate(LocalDate.now());
                    return savedCustomer;
                });

        // When
        long startTime = System.currentTimeMillis();
        CreditScoreResponseDto response = delayEnabledService.processCredit(request);
        long endTime = System.currentTimeMillis();

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getProcessingTimeMs()).isGreaterThanOrEqualTo(0L);
        assertThat(endTime - startTime).isGreaterThanOrEqualTo(response.getProcessingTimeMs());
    }

    @Test
    @DisplayName("Should handle edge case with minimum score boundary")
    void shouldHandleMinimumScoreBoundary() {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .currentCreditScore(299) // Below minimum
                .build();

        Customer customer = new Customer();
        customer.setCustomerNumber(1234567890L);
        customer.setSortCode("987654");
        customer.setName("John Doe");
        customer.setAddress("123 Main St");
        customer.setDateOfBirth(LocalDate.of(1990, 5, 15));
        customer.setCreditScore(299);

        when(customerRepository.findById("987654", 1234567890L))
                .thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class)))
                .thenAnswer(invocation -> {
                    Customer savedCustomer = invocation.getArgument(0);
                    savedCustomer.setCreditScoreReviewDate(LocalDate.now());
                    return savedCustomer;
                });

        // When
        CreditScoreResponseDto response = creditAgencyService.processCredit(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getUpdatedCreditScore()).isBetween(300, 850);
    }

    @Test
    @DisplayName("Should handle edge case with maximum score boundary")
    void shouldHandleMaximumScoreBoundary() {
        // Given
        CreditScoreRequestDto request = CreditScoreRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .currentCreditScore(851) // Above maximum
                .build();

        Customer customer = new Customer();
        customer.setCustomerNumber(1234567890L);
        customer.setSortCode("987654");
        customer.setName("John Doe");
        customer.setAddress("123 Main St");
        customer.setDateOfBirth(LocalDate.of(1990, 5, 15));
        customer.setCreditScore(851);

        when(customerRepository.findById("987654", 1234567890L))
                .thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class)))
                .thenAnswer(invocation -> {
                    Customer savedCustomer = invocation.getArgument(0);
                    savedCustomer.setCreditScoreReviewDate(LocalDate.now());
                    return savedCustomer;
                });

        // When
        CreditScoreResponseDto response = creditAgencyService.processCredit(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getUpdatedCreditScore()).isBetween(300, 850);
    }
}
