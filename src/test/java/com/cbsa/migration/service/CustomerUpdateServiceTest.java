package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerUpdateRequestDto;
import com.cbsa.migration.dto.CustomerUpdateResponseDto;
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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CustomerUpdateServiceTest {

    @Mock
    private CustomerRepository customerRepository;

    private CustomerUpdateService service;

    @BeforeEach
    void setUp() {
        service = new CustomerUpdateService(customerRepository);
    }

    @Test
    void shouldSuccessfullyUpdateBothNameAndAddress() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(1000000001L)
            .name("Mr John Smith")
            .address("123 Main Street, London")
            .build();

        Customer existingCustomer = createTestCustomer();
        Customer updatedCustomer = createTestCustomer();
        updatedCustomer.setName("Mr John Smith");
        updatedCustomer.setAddress("123 Main Street, London");

        when(customerRepository.findById("123456", 1000000001L))
            .thenReturn(Optional.of(existingCustomer));
        when(customerRepository.save(any(Customer.class)))
            .thenReturn(updatedCustomer);

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isNull();
        assertThat(response.getName()).isEqualTo("Mr John Smith");
        assertThat(response.getAddress()).isEqualTo("123 Main Street, London");
        verify(customerRepository).save(any(Customer.class));
    }

    @Test
    void shouldUpdateOnlyNameWhenAddressIsEmpty() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(1000000001L)
            .name("Mrs Jane Doe")
            .address("")
            .build();

        Customer existingCustomer = createTestCustomer();
        String originalAddress = existingCustomer.getAddress();

        when(customerRepository.findById("123456", 1000000001L))
            .thenReturn(Optional.of(existingCustomer));
        when(customerRepository.save(any(Customer.class)))
            .thenAnswer(invocation -> invocation.getArgument(0));

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getName()).isEqualTo("Mrs Jane Doe");
        assertThat(response.getAddress()).isEqualTo(originalAddress);
    }

    @Test
    void shouldUpdateOnlyAddressWhenNameIsEmpty() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(1000000001L)
            .name("")
            .address("456 Oak Avenue, Manchester")
            .build();

        Customer existingCustomer = createTestCustomer();
        String originalName = existingCustomer.getName();

        when(customerRepository.findById("123456", 1000000001L))
            .thenReturn(Optional.of(existingCustomer));
        when(customerRepository.save(any(Customer.class)))
            .thenAnswer(invocation -> invocation.getArgument(0));

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getName()).isEqualTo(originalName);
        assertThat(response.getAddress()).isEqualTo("456 Oak Avenue, Manchester");
    }

    @Test
    void shouldFailWhenBothNameAndAddressAreEmpty() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(1000000001L)
            .name("")
            .address("")
            .build();

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("4");
        assertThat(response.getErrorMessage()).contains("At least one");
        verify(customerRepository, never()).findById(any(), any());
    }

    @Test
    void shouldFailWhenBothNameAndAddressStartWithSpace() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(1000000001L)
            .name(" ")
            .address(" ")
            .build();

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("4");
    }

    @Test
    void shouldAcceptValidTitle_Professor() {
        testValidTitle("Professor Albert Einstein");
    }

    @Test
    void shouldAcceptValidTitle_Mr() {
        testValidTitle("Mr John Smith");
    }

    @Test
    void shouldAcceptValidTitle_Mrs() {
        testValidTitle("Mrs Jane Doe");
    }

    @Test
    void shouldAcceptValidTitle_Miss() {
        testValidTitle("Miss Emily Brown");
    }

    @Test
    void shouldAcceptValidTitle_Ms() {
        testValidTitle("Ms Sarah Wilson");
    }

    @Test
    void shouldAcceptValidTitle_Dr() {
        testValidTitle("Dr Michael Johnson");
    }

    @Test
    void shouldAcceptValidTitle_Drs() {
        testValidTitle("Drs Smith and Jones");
    }

    @Test
    void shouldAcceptValidTitle_Lord() {
        testValidTitle("Lord Wellington");
    }

    @Test
    void shouldAcceptValidTitle_Sir() {
        testValidTitle("Sir Isaac Newton");
    }

    @Test
    void shouldAcceptValidTitle_Lady() {
        testValidTitle("Lady Catherine");
    }

    @Test
    void shouldRejectInvalidTitle() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(1000000001L)
            .name("Captain Jack Sparrow")
            .address("123 Main Street")
            .build();

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("T");
        assertThat(response.getErrorMessage()).contains("Invalid title");
        verify(customerRepository, never()).findById(any(), any());
    }

    @Test
    void shouldFailWhenCustomerNotFound() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(9999999999L)
            .name("Mr Test User")
            .address("Test Address")
            .build();

        when(customerRepository.findById("123456", 9999999999L))
            .thenReturn(Optional.empty());

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("1");
        assertThat(response.getErrorMessage()).isEqualTo("Customer not found");
        verify(customerRepository, never()).save(any());
    }

    @Test
    void shouldFailOnDatabaseReadError() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(1000000001L)
            .name("Mr Test User")
            .address("Test Address")
            .build();

        when(customerRepository.findById("123456", 1000000001L))
            .thenThrow(new RuntimeException("Database connection failed"));

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("2");
        assertThat(response.getErrorMessage()).contains("Database error");
    }

    @Test
    void shouldFailOnDatabaseSaveError() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(1000000001L)
            .name("Mr Test User")
            .address("Test Address")
            .build();

        Customer existingCustomer = createTestCustomer();

        when(customerRepository.findById("123456", 1000000001L))
            .thenReturn(Optional.of(existingCustomer));
        when(customerRepository.save(any(Customer.class)))
            .thenThrow(new RuntimeException("Failed to save customer"));

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("3");
        assertThat(response.getErrorMessage()).contains("Failed to save");
    }

    @Test
    void shouldReturnAllCustomerFieldsOnSuccess() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(1000000001L)
            .name("Dr Alice Cooper")
            .address("789 Pine Road, Birmingham")
            .build();

        Customer customer = createTestCustomer();
        customer.setName("Dr Alice Cooper");
        customer.setAddress("789 Pine Road, Birmingham");

        when(customerRepository.findById("123456", 1000000001L))
            .thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class)))
            .thenReturn(customer);

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getEyeCatcher()).isEqualTo("CUST");
        assertThat(response.getSortCode()).isEqualTo("123456");
        assertThat(response.getCustomerNumber()).isEqualTo(1000000001L);
        assertThat(response.getDateOfBirth()).isNotNull();
        assertThat(response.getCreditScore()).isNotNull();
        assertThat(response.getCreditScoreReviewDate()).isNotNull();
    }

    private void testValidTitle(String nameWithTitle) {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(1000000001L)
            .name(nameWithTitle)
            .address("Test Address")
            .build();

        Customer customer = createTestCustomer();
        customer.setName(nameWithTitle);

        when(customerRepository.findById("123456", 1000000001L))
            .thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class)))
            .thenReturn(customer);

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isNull();
    }

    private Customer createTestCustomer() {
        return Customer.builder()
            .eyeCatcher("CUST")
            .sortCode("123456")
            .customerNumber(1000000001L)
            .name("Mr Original Name")
            .address("Original Address")
            .dateOfBirth(LocalDate.of(1980, 1, 15))
            .creditScore(750)
            .creditScoreReviewDate(LocalDate.now())
            .build();
    }
}
