package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerRequestDto;
import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.dto.ErrorResponseDto;
import com.cbsa.migration.dto.mapper.DtoMapper;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CustomerServiceTest {

    @Mock private CustomerRepository customerRepository;
    @Mock private CreditAgencyService creditAgencyService;
    @Mock private NamedCounterService namedCounterService;
    @Mock private TransactionRepository transactionRepository;
    @Mock private ErrorLoggingService errorLoggingService;
    @Mock private SortCodeService sortCodeService;

    private DtoMapper dtoMapper;
    private CustomerService customerService;

    @BeforeEach
    void setUp() {
        dtoMapper = new DtoMapper();
        customerService = new CustomerService(
                customerRepository,
                creditAgencyService,
                namedCounterService,
                transactionRepository,
                errorLoggingService,
                sortCodeService,
                dtoMapper
        );
    }

    // --- Happy Path Tests ---

    @Test
    void createCustomer_happyPath_createsCustomerWithCreditScoreAndProctran() {
        CustomerRequestDto request = buildValidRequest();

        when(namedCounterService.getNextCustomerNumberWithLock("987654")).thenReturn(100001L);
        when(creditAgencyService.generateCreditScore()).thenReturn(750);
        when(customerRepository.save(any(Customer.class))).thenAnswer(i -> i.getArgument(0));
        when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

        CustomerResponseDto response = customerService.createCustomer(request);

        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getCustomerNumber()).isEqualTo(100001L);
        assertThat(response.getSortCode()).isEqualTo("987654");
        assertThat(response.getName()).isEqualTo("John Smith");
        assertThat(response.getStatus()).isEqualTo("CREATED");
        assertThat(response.getCreditScore()).isEqualTo(750);

        verify(customerRepository).save(any(Customer.class));
        verify(transactionRepository).save(any(Transaction.class));
        verify(namedCounterService).releaseLock();
    }

    @Test
    void createCustomer_happyPath_correctCustomerNumber() {
        CustomerRequestDto request = buildValidRequest();

        when(namedCounterService.getNextCustomerNumberWithLock("987654")).thenReturn(200050L);
        when(creditAgencyService.generateCreditScore()).thenReturn(600);
        when(customerRepository.save(any(Customer.class))).thenAnswer(i -> i.getArgument(0));
        when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

        CustomerResponseDto response = customerService.createCustomer(request);

        assertThat(response.getCustomerNumber()).isEqualTo(200050L);
        verify(namedCounterService).releaseLock();
    }

    // --- DOB Validation Tests ---

    @Test
    void createCustomer_dobYearBefore1601_failsWithCodeO() {
        CustomerRequestDto request = buildValidRequest();
        request.setDateOfBirth(LocalDate.of(1600, 1, 1));

        assertThatThrownBy(() -> customerService.createCustomer(request))
                .isInstanceOf(CustomerCreationException.class)
                .satisfies(e -> assertThat(((CustomerCreationException) e).getFailCode()).isEqualTo("O"));
    }

    @Test
    void createCustomer_dobInFuture_failsWithCodeY() {
        CustomerRequestDto request = buildValidRequest();
        request.setDateOfBirth(LocalDate.now().plusDays(1));

        assertThatThrownBy(() -> customerService.createCustomer(request))
                .isInstanceOf(CustomerCreationException.class)
                .satisfies(e -> assertThat(((CustomerCreationException) e).getFailCode()).isEqualTo("Y"));
    }

    @Test
    void createCustomer_ageOver150_failsWithCodeO() {
        CustomerRequestDto request = buildValidRequest();
        request.setDateOfBirth(LocalDate.now().minusYears(151));

        assertThatThrownBy(() -> customerService.createCustomer(request))
                .isInstanceOf(CustomerCreationException.class)
                .satisfies(e -> assertThat(((CustomerCreationException) e).getFailCode()).isEqualTo("O"));
    }

    @Test
    void createCustomer_dobNull_failsWithCodeZ() {
        CustomerRequestDto request = buildValidRequest();
        request.setDateOfBirth(null);

        assertThatThrownBy(() -> customerService.createCustomer(request))
                .isInstanceOf(CustomerCreationException.class)
                .satisfies(e -> assertThat(((CustomerCreationException) e).getFailCode()).isEqualTo("Z"));
    }

    // --- Credit Check Tests ---

    @Test
    void createCustomer_allAgenciesRespond_averagesScores() {
        CustomerRequestDto request = buildValidRequest();

        when(namedCounterService.getNextCustomerNumberWithLock("987654")).thenReturn(100001L);
        when(creditAgencyService.generateCreditScore()).thenReturn(700);
        when(customerRepository.save(any(Customer.class))).thenAnswer(i -> i.getArgument(0));
        when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

        CustomerResponseDto response = customerService.createCustomer(request);

        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getCreditScore()).isEqualTo(700);
        verify(namedCounterService).releaseLock();
    }

    @Test
    void createCustomer_noAgenciesRespond_scorIsZeroReviewToday() {
        CustomerRequestDto request = buildValidRequest();

        when(namedCounterService.getNextCustomerNumberWithLock("987654")).thenReturn(100001L);
        when(creditAgencyService.generateCreditScore())
                .thenThrow(new RuntimeException("Service unavailable"));
        when(customerRepository.save(any(Customer.class))).thenAnswer(i -> i.getArgument(0));
        when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

        CustomerResponseDto response = customerService.createCustomer(request);

        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getCreditScore()).isEqualTo(0);
        assertThat(response.getCreditScoreReviewDate()).isEqualTo(LocalDate.now());
        verify(namedCounterService).releaseLock();
    }

    @Test
    void createCustomer_creditAgencyThrows_treatedAsNoResponse() {
        CustomerRequestDto request = buildValidRequest();

        when(namedCounterService.getNextCustomerNumberWithLock("987654")).thenReturn(100001L);
        when(creditAgencyService.generateCreditScore())
                .thenThrow(new RuntimeException("Connection refused"));
        when(customerRepository.save(any(Customer.class))).thenAnswer(i -> i.getArgument(0));
        when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

        CustomerResponseDto response = customerService.createCustomer(request);

        assertThat(response.getSuccess()).isTrue();
        assertThat(response.getCreditScore()).isEqualTo(0);
        verify(namedCounterService).releaseLock();
    }

    // --- VSAM Write Failure Tests ---

    @Test
    void createCustomer_vsamWriteFails_rollsBackCounterAndFailsWithCode1() {
        CustomerRequestDto request = buildValidRequest();

        when(namedCounterService.getNextCustomerNumberWithLock("987654")).thenReturn(100001L);
        when(creditAgencyService.generateCreditScore()).thenReturn(500);
        when(customerRepository.save(any(Customer.class)))
                .thenThrow(new RuntimeException("VSAM write error"));

        assertThatThrownBy(() -> customerService.createCustomer(request))
                .isInstanceOf(CustomerCreationException.class)
                .satisfies(e -> assertThat(((CustomerCreationException) e).getFailCode()).isEqualTo("1"));

        verify(namedCounterService).rollbackCustomerNumber();
        verify(namedCounterService).releaseLock();
    }

    // --- PROCTRAN Write Failure Tests ---

    @Test
    void createCustomer_proctranWriteFails_rollsBackAndLogsError() {
        CustomerRequestDto request = buildValidRequest();

        when(namedCounterService.getNextCustomerNumberWithLock("987654")).thenReturn(100001L);
        when(creditAgencyService.generateCreditScore()).thenReturn(500);
        when(customerRepository.save(any(Customer.class))).thenAnswer(i -> i.getArgument(0));
        when(transactionRepository.save(any(Transaction.class)))
                .thenThrow(new RuntimeException("DB2 write error"));
        when(errorLoggingService.logError(anyString(), any(Exception.class)))
                .thenReturn(ErrorResponseDto.failure("logged", "now"));

        assertThatThrownBy(() -> customerService.createCustomer(request))
                .isInstanceOf(CustomerCreationException.class)
                .satisfies(e -> assertThat(((CustomerCreationException) e).getFailCode()).isEqualTo("4"));

        verify(namedCounterService).rollbackCustomerNumber();
        verify(errorLoggingService).logError(eq("CRECUST"), any(Exception.class));
        verify(namedCounterService).releaseLock();
    }

    // --- Customer Read Tests (migrate_001) ---

    @Test
    void getCustomer_found_returnsDto() {
        Customer customer = buildTestCustomer();
        when(customerRepository.findById("987654", 100001L)).thenReturn(Optional.of(customer));

        CustomerResponseDto result = customerService.getCustomer("987654", 100001L);

        assertThat(result.getName()).isEqualTo("John Smith");
        assertThat(result.getSortCode()).isEqualTo("987654");
        assertThat(result.getCustomerNumber()).isEqualTo(100001L);
        assertThat(result.getCreditScore()).isEqualTo(750);
        assertThat(result.getCreditScoreReviewDate()).isNotNull();
    }

    @Test
    void getCustomer_notFound_throwsException() {
        when(customerRepository.findById("987654", 999999L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> customerService.getCustomer("987654", 999999L))
                .isInstanceOf(CustomerService.CustomerNotFoundException.class);
    }

    @Test
    void getCustomerByName_found_returnsList() {
        Customer customer = buildTestCustomer();
        when(customerRepository.findByNameContaining("Smith")).thenReturn(List.of(customer));

        List<CustomerResponseDto> results = customerService.getCustomerByName("Smith");

        assertThat(results).hasSize(1);
        assertThat(results.get(0).getName()).isEqualTo("John Smith");
    }

    @Test
    void getCustomerByName_notFound_returnsEmptyList() {
        when(customerRepository.findByNameContaining("Nobody")).thenReturn(Collections.emptyList());

        List<CustomerResponseDto> results = customerService.getCustomerByName("Nobody");

        assertThat(results).isEmpty();
    }

    // --- Helper Methods ---

    private CustomerRequestDto buildValidRequest() {
        return CustomerRequestDto.builder()
                .name("John Smith")
                .address("123 Main St, London")
                .dateOfBirth(LocalDate.of(1985, 6, 15))
                .sortCode("987654")
                .build();
    }

    private Customer buildTestCustomer() {
        return Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(100001L)
                .name("John Smith")
                .address("123 Main St, London")
                .dateOfBirth(LocalDate.of(1985, 6, 15))
                .creditScore(750)
                .creditScoreReviewDate(LocalDate.now().plusDays(14))
                .build();
    }
}
