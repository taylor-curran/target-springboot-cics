package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerUpdateRequestDto;
import com.cbsa.migration.dto.CustomerUpdateResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

import java.time.LocalDate;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@ActiveProfiles("test")
class CustomerUpdateServiceIntegrationTest {

    @Autowired
    private CustomerUpdateService service;

    @Autowired
    private CustomerRepository customerRepository;

    private Customer testCustomer;

    @BeforeEach
    void setUp() {
        testCustomer = Customer.builder()
            .eyeCatcher("CUST")
            .sortCode("123456")
            .customerNumber(100000001L)
            .name("Mr Test Customer")
            .address("123 Test Street, Test City")
            .dateOfBirth(LocalDate.of(1985, 5, 20))
            .creditScore(700)
            .creditScoreReviewDate(LocalDate.now())
            .build();
        
        customerRepository.save(testCustomer);
    }

    @Test
    void shouldUpdateCustomerInDatabase() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(100000001L)
            .name("Mrs Updated Customer")
            .address("456 New Address, New City")
            .build();

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getName()).isEqualTo("Mrs Updated Customer");
        assertThat(response.getAddress()).isEqualTo("456 New Address, New City");

        Optional<Customer> updatedCustomer = customerRepository.findById("123456", 100000001L);
        assertThat(updatedCustomer).isPresent();
        assertThat(updatedCustomer.get().getName()).isEqualTo("Mrs Updated Customer");
        assertThat(updatedCustomer.get().getAddress()).isEqualTo("456 New Address, New City");
    }

    @Test
    void shouldUpdateOnlyNameInDatabase() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(100000001L)
            .name("Dr Changed Name")
            .address("")
            .build();

        String originalAddress = testCustomer.getAddress();

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isTrue();

        Optional<Customer> updatedCustomer = customerRepository.findById("123456", 100000001L);
        assertThat(updatedCustomer).isPresent();
        assertThat(updatedCustomer.get().getName()).isEqualTo("Dr Changed Name");
        assertThat(updatedCustomer.get().getAddress()).isEqualTo(originalAddress);
    }

    @Test
    void shouldUpdateOnlyAddressInDatabase() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("123456")
            .customerNumber(100000001L)
            .name("")
            .address("789 Different Street, Different City")
            .build();

        String originalName = testCustomer.getName();

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isTrue();

        Optional<Customer> updatedCustomer = customerRepository.findById("123456", 100000001L);
        assertThat(updatedCustomer).isPresent();
        assertThat(updatedCustomer.get().getName()).isEqualTo(originalName);
        assertThat(updatedCustomer.get().getAddress()).isEqualTo("789 Different Street, Different City");
    }

    @Test
    void shouldFailWhenCustomerDoesNotExist() {
        CustomerUpdateRequestDto request = CustomerUpdateRequestDto.builder()
            .sortCode("999999")
            .customerNumber(999999999L)
            .name("Mr Nonexistent")
            .address("Nowhere")
            .build();

        CustomerUpdateResponseDto response = service.updateCustomer(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("1");
    }
}
