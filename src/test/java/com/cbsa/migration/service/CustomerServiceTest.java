package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerInquiryResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataAccessResourceFailureException;

import java.time.LocalDate;
import java.util.Optional;
import java.util.Random;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Unit tests for CustomerService implementing INQCUST COBOL program logic.
 * Tests cover three customer lookup scenarios:
 * 1. Regular customer lookup
 * 2. Random customer lookup (customer number = 0)
 * 3. Last customer lookup (customer number = 9999999999)
 */
@ExtendWith(MockitoExtension.class)
class CustomerServiceTest {

    private static final String DEFAULT_SORT_CODE = "987654";
    private static final long RANDOM_CUSTOMER_NUMBER = 0L;
    private static final long LAST_CUSTOMER_NUMBER = 9999999999L;

    @Mock
    private CustomerRepository customerRepository;

    @Mock
    private SortCodeService sortCodeService;

    @InjectMocks
    private CustomerService customerService;

    private Customer testCustomer;

    @BeforeEach
    void setUp() {
        testCustomer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode(DEFAULT_SORT_CODE)
                .customerNumber(12345L)
                .name("John Doe")
                .address("123 Main Street, City, Country")
                .dateOfBirth(LocalDate.of(1980, 5, 15))
                .creditScore(750)
                .creditScoreReviewDate(LocalDate.of(2025, 1, 10))
                .build();
    }

    @Nested
    class RegularCustomerLookup {

        @Test
        void shouldReturnCustomerWhenFound() {
            when(customerRepository.findById(DEFAULT_SORT_CODE, 12345L))
                    .thenReturn(Optional.of(testCustomer));

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, 12345L);

            assertThat(response.isSuccess()).isTrue();
            assertThat(response.getFailureCode()).isEqualTo("0");
            assertThat(response.getEyeCatcher()).isEqualTo("CUST");
            assertThat(response.getSortCode()).isEqualTo(DEFAULT_SORT_CODE);
            assertThat(response.getCustomerNumber()).isEqualTo(12345L);
            assertThat(response.getName()).isEqualTo("John Doe");
            assertThat(response.getAddress()).isEqualTo("123 Main Street, City, Country");
            assertThat(response.getDateOfBirth()).isEqualTo(LocalDate.of(1980, 5, 15));
            assertThat(response.getCreditScore()).isEqualTo(750);
            assertThat(response.getCreditScoreReviewDate()).isEqualTo(LocalDate.of(2025, 1, 10));
            assertThat(response.getErrorMessage()).isNull();

            verify(customerRepository).findById(DEFAULT_SORT_CODE, 12345L);
        }

        @Test
        void shouldReturnNotFoundWhenCustomerDoesNotExist() {
            when(customerRepository.findById(DEFAULT_SORT_CODE, 99999L))
                    .thenReturn(Optional.empty());

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, 99999L);

            assertThat(response.isSuccess()).isFalse();
            assertThat(response.getFailureCode()).isEqualTo("1");
            assertThat(response.getSortCode()).isEqualTo(DEFAULT_SORT_CODE);
            assertThat(response.getCustomerNumber()).isEqualTo(99999L);
            assertThat(response.getErrorMessage()).contains("not found");

            verify(customerRepository).findById(DEFAULT_SORT_CODE, 99999L);
        }

        @Test
        void shouldUseDefaultSortCodeWhenOnlyCustomerNumberProvided() {
            when(sortCodeService.getSortCode()).thenReturn(DEFAULT_SORT_CODE);
            when(customerRepository.findById(DEFAULT_SORT_CODE, 12345L))
                    .thenReturn(Optional.of(testCustomer));

            CustomerInquiryResponseDto response = customerService.inquireCustomer(12345L);

            assertThat(response.isSuccess()).isTrue();
            verify(sortCodeService).getSortCode();
            verify(customerRepository).findById(DEFAULT_SORT_CODE, 12345L);
        }
    }

    @Nested
    class RandomCustomerLookup {

        @Test
        void shouldReturnRandomCustomerWhenFound() {
            Customer randomCustomer = Customer.builder()
                    .eyeCatcher("CUST")
                    .sortCode(DEFAULT_SORT_CODE)
                    .customerNumber(50L)
                    .name("Random Customer")
                    .address("Random Address")
                    .dateOfBirth(LocalDate.of(1990, 3, 20))
                    .creditScore(600)
                    .build();

            when(customerRepository.findMaxCustomerNumber(DEFAULT_SORT_CODE))
                    .thenReturn(Optional.of(100L));
            when(customerRepository.findById(eq(DEFAULT_SORT_CODE), anyLong()))
                    .thenReturn(Optional.of(randomCustomer));

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, RANDOM_CUSTOMER_NUMBER);

            assertThat(response.isSuccess()).isTrue();
            assertThat(response.getFailureCode()).isEqualTo("0");
            assertThat(response.getName()).isEqualTo("Random Customer");

            verify(customerRepository).findMaxCustomerNumber(DEFAULT_SORT_CODE);
        }

        @Test
        void shouldRetryUpTo1000TimesForRandomCustomer() {
            when(customerRepository.findMaxCustomerNumber(DEFAULT_SORT_CODE))
                    .thenReturn(Optional.of(100L));
            when(customerRepository.findById(eq(DEFAULT_SORT_CODE), anyLong()))
                    .thenReturn(Optional.empty());

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, RANDOM_CUSTOMER_NUMBER);

            assertThat(response.isSuccess()).isFalse();
            assertThat(response.getFailureCode()).isEqualTo("1");
            assertThat(response.getErrorMessage()).contains("maximum retry attempts");

            verify(customerRepository, times(1000)).findById(eq(DEFAULT_SORT_CODE), anyLong());
        }

        @Test
        void shouldReturnNotFoundWhenNoCustomersExist() {
            when(customerRepository.findMaxCustomerNumber(DEFAULT_SORT_CODE))
                    .thenReturn(Optional.empty());

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, RANDOM_CUSTOMER_NUMBER);

            assertThat(response.isSuccess()).isFalse();
            assertThat(response.getFailureCode()).isEqualTo("1");
            assertThat(response.getErrorMessage()).contains("No customers available");

            verify(customerRepository, never()).findById(anyString(), anyLong());
        }

        @Test
        void shouldReturnNotFoundWhenMaxCustomerNumberIsZero() {
            when(customerRepository.findMaxCustomerNumber(DEFAULT_SORT_CODE))
                    .thenReturn(Optional.of(0L));

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, RANDOM_CUSTOMER_NUMBER);

            assertThat(response.isSuccess()).isFalse();
            assertThat(response.getFailureCode()).isEqualTo("1");
        }

        @Test
        void shouldFindCustomerOnSecondAttempt() {
            Customer randomCustomer = Customer.builder()
                    .eyeCatcher("CUST")
                    .sortCode(DEFAULT_SORT_CODE)
                    .customerNumber(25L)
                    .name("Found Customer")
                    .address("Found Address")
                    .dateOfBirth(LocalDate.of(1985, 7, 12))
                    .creditScore(700)
                    .build();

            when(customerRepository.findMaxCustomerNumber(DEFAULT_SORT_CODE))
                    .thenReturn(Optional.of(100L));
            when(customerRepository.findById(eq(DEFAULT_SORT_CODE), anyLong()))
                    .thenReturn(Optional.empty())
                    .thenReturn(Optional.of(randomCustomer));

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, RANDOM_CUSTOMER_NUMBER);

            assertThat(response.isSuccess()).isTrue();
            assertThat(response.getName()).isEqualTo("Found Customer");

            verify(customerRepository, times(2)).findById(eq(DEFAULT_SORT_CODE), anyLong());
        }
    }

    @Nested
    class LastCustomerLookup {

        @Test
        void shouldReturnLastCustomerWhenFound() {
            Customer lastCustomer = Customer.builder()
                    .eyeCatcher("CUST")
                    .sortCode(DEFAULT_SORT_CODE)
                    .customerNumber(500L)
                    .name("Last Customer")
                    .address("Last Address")
                    .dateOfBirth(LocalDate.of(1975, 12, 1))
                    .creditScore(800)
                    .build();

            when(customerRepository.findMaxCustomerNumber(DEFAULT_SORT_CODE))
                    .thenReturn(Optional.of(500L));
            when(customerRepository.findById(DEFAULT_SORT_CODE, 500L))
                    .thenReturn(Optional.of(lastCustomer));

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, LAST_CUSTOMER_NUMBER);

            assertThat(response.isSuccess()).isTrue();
            assertThat(response.getFailureCode()).isEqualTo("0");
            assertThat(response.getCustomerNumber()).isEqualTo(500L);
            assertThat(response.getName()).isEqualTo("Last Customer");

            verify(customerRepository).findMaxCustomerNumber(DEFAULT_SORT_CODE);
            verify(customerRepository).findById(DEFAULT_SORT_CODE, 500L);
        }

        @Test
        void shouldReturnSystemErrorWhenNoCustomersExist() {
            when(customerRepository.findMaxCustomerNumber(DEFAULT_SORT_CODE))
                    .thenReturn(Optional.empty());

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, LAST_CUSTOMER_NUMBER);

            assertThat(response.isSuccess()).isFalse();
            assertThat(response.getFailureCode()).isEqualTo("9");
            assertThat(response.getErrorMessage()).contains("No customers exist");

            verify(customerRepository, never()).findById(anyString(), anyLong());
        }

        @Test
        void shouldReturnSystemErrorWhenMaxCustomerNumberIsZero() {
            when(customerRepository.findMaxCustomerNumber(DEFAULT_SORT_CODE))
                    .thenReturn(Optional.of(0L));

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, LAST_CUSTOMER_NUMBER);

            assertThat(response.isSuccess()).isFalse();
            assertThat(response.getFailureCode()).isEqualTo("9");
        }

        @Test
        void shouldReturnSystemErrorWhenLastCustomerNotFoundAfterMaxLookup() {
            when(customerRepository.findMaxCustomerNumber(DEFAULT_SORT_CODE))
                    .thenReturn(Optional.of(500L));
            when(customerRepository.findById(DEFAULT_SORT_CODE, 500L))
                    .thenReturn(Optional.empty());

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, LAST_CUSTOMER_NUMBER);

            assertThat(response.isSuccess()).isFalse();
            assertThat(response.getFailureCode()).isEqualTo("9");
            assertThat(response.getErrorMessage()).contains("system error");
        }
    }

    @Nested
    class ErrorHandling {

        @Test
        void shouldReturnSystemErrorOnDatabaseException() {
            when(customerRepository.findById(DEFAULT_SORT_CODE, 12345L))
                    .thenThrow(new DataAccessResourceFailureException("Database connection failed"));

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, 12345L);

            assertThat(response.isSuccess()).isFalse();
            assertThat(response.getFailureCode()).isEqualTo("9");
            assertThat(response.getErrorMessage()).contains("system error");
            assertThat(response.getErrorMessage()).contains("try again later");
        }

        @Test
        void shouldReturnSystemErrorOnDatabaseExceptionForRandomCustomer() {
            when(customerRepository.findMaxCustomerNumber(DEFAULT_SORT_CODE))
                    .thenThrow(new DataAccessResourceFailureException("Database connection failed"));

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, RANDOM_CUSTOMER_NUMBER);

            assertThat(response.isSuccess()).isFalse();
            assertThat(response.getFailureCode()).isEqualTo("9");
        }

        @Test
        void shouldReturnSystemErrorOnDatabaseExceptionForLastCustomer() {
            when(customerRepository.findMaxCustomerNumber(DEFAULT_SORT_CODE))
                    .thenThrow(new DataAccessResourceFailureException("Database connection failed"));

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, LAST_CUSTOMER_NUMBER);

            assertThat(response.isSuccess()).isFalse();
            assertThat(response.getFailureCode()).isEqualTo("9");
        }
    }

    @Nested
    class RandomNumberGeneration {

        @Test
        void shouldGenerateNumberBetweenOneAndMax() {
            Random random = new Random(42);
            long maxCustomerNumber = 100L;

            for (int i = 0; i < 100; i++) {
                long result = customerService.generateRandomCustomerNumber(maxCustomerNumber, random);
                assertThat(result).isBetween(1L, maxCustomerNumber);
            }
        }

        @Test
        void shouldReturnOneWhenMaxIsOne() {
            Random random = new Random();
            long result = customerService.generateRandomCustomerNumber(1L, random);
            assertThat(result).isEqualTo(1L);
        }

        @Test
        void shouldReturnOneWhenMaxIsZero() {
            Random random = new Random();
            long result = customerService.generateRandomCustomerNumber(0L, random);
            assertThat(result).isEqualTo(1L);
        }

        @Test
        void shouldGenerateDifferentNumbersWithDifferentSeeds() {
            long maxCustomerNumber = 1000000L;
            Random random1 = new Random(1);
            Random random2 = new Random(2);

            long result1 = customerService.generateRandomCustomerNumber(maxCustomerNumber, random1);
            long result2 = customerService.generateRandomCustomerNumber(maxCustomerNumber, random2);

            assertThat(result1).isNotEqualTo(result2);
        }
    }

    @Nested
    class ResponseMapping {

        @Test
        void shouldMapAllCustomerFieldsToResponse() {
            Customer fullCustomer = Customer.builder()
                    .eyeCatcher("CUST")
                    .sortCode(DEFAULT_SORT_CODE)
                    .customerNumber(99999L)
                    .name("Full Name Customer")
                    .address("Full Address, City, State, ZIP")
                    .dateOfBirth(LocalDate.of(1970, 1, 1))
                    .creditScore(999)
                    .creditScoreReviewDate(LocalDate.of(2025, 12, 31))
                    .build();

            when(customerRepository.findById(DEFAULT_SORT_CODE, 99999L))
                    .thenReturn(Optional.of(fullCustomer));

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, 99999L);

            assertThat(response.getEyeCatcher()).isEqualTo("CUST");
            assertThat(response.getSortCode()).isEqualTo(DEFAULT_SORT_CODE);
            assertThat(response.getCustomerNumber()).isEqualTo(99999L);
            assertThat(response.getName()).isEqualTo("Full Name Customer");
            assertThat(response.getAddress()).isEqualTo("Full Address, City, State, ZIP");
            assertThat(response.getDateOfBirth()).isEqualTo(LocalDate.of(1970, 1, 1));
            assertThat(response.getCreditScore()).isEqualTo(999);
            assertThat(response.getCreditScoreReviewDate()).isEqualTo(LocalDate.of(2025, 12, 31));
        }

        @Test
        void shouldHandleNullCreditScoreReviewDate() {
            Customer customerWithoutReviewDate = Customer.builder()
                    .eyeCatcher("CUST")
                    .sortCode(DEFAULT_SORT_CODE)
                    .customerNumber(11111L)
                    .name("No Review Date Customer")
                    .address("Address")
                    .dateOfBirth(LocalDate.of(1990, 6, 15))
                    .creditScore(500)
                    .creditScoreReviewDate(null)
                    .build();

            when(customerRepository.findById(DEFAULT_SORT_CODE, 11111L))
                    .thenReturn(Optional.of(customerWithoutReviewDate));

            CustomerInquiryResponseDto response = customerService.inquireCustomer(DEFAULT_SORT_CODE, 11111L);

            assertThat(response.isSuccess()).isTrue();
            assertThat(response.getCreditScoreReviewDate()).isNull();
        }
    }
}
