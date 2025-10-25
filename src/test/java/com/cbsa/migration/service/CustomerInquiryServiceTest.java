package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.dto.mapper.DtoMapper;
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
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * Unit tests for CustomerInquiryService
 * Tests the service layer with mocked repository and mapper
 */
@ExtendWith(MockitoExtension.class)
class CustomerInquiryServiceTest {

    @Mock
    private CustomerRepository customerRepository;

    @Mock
    private DtoMapper dtoMapper;

    private CustomerInquiryService customerInquiryService;

    @BeforeEach
    void setUp() {
        customerInquiryService = new CustomerInquiryService(customerRepository, dtoMapper);
    }

    @Test
    void inquireCustomer_WhenCustomerExists_ReturnsCustomerDto() {
        String sortCode = "987654";
        Long customerNumber = 100001L;
        
        Customer customer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode(sortCode)
                .customerNumber(customerNumber)
                .name("John Doe")
                .address("123 Main St, London, UK")
                .dateOfBirth(LocalDate.of(1980, 1, 15))
                .creditScore(750)
                .creditScoreReviewDate(LocalDate.of(2023, 12, 1))
                .build();

        CustomerResponseDto expectedDto = CustomerResponseDto.builder()
                .sortCode(sortCode)
                .customerNumber(customerNumber)
                .name("John Doe")
                .address("123 Main St, London, UK")
                .dateOfBirth(LocalDate.of(1980, 1, 15))
                .creditScore(750)
                .creditScoreReviewDate(LocalDate.of(2023, 12, 1))
                .status("GOOD_STANDING")
                .build();

        when(customerRepository.findById(sortCode, customerNumber))
                .thenReturn(Optional.of(customer));
        when(dtoMapper.toCustomerResponseDto(customer))
                .thenReturn(expectedDto);

        Optional<CustomerResponseDto> result = customerInquiryService.inquireCustomer(sortCode, customerNumber);

        assertThat(result).isPresent();
        CustomerResponseDto dto = result.get();
        assertThat(dto.getSortCode()).isEqualTo(sortCode);
        assertThat(dto.getCustomerNumber()).isEqualTo(customerNumber);
        assertThat(dto.getName()).isEqualTo("John Doe");
        assertThat(dto.getAddress()).isEqualTo("123 Main St, London, UK");
        assertThat(dto.getDateOfBirth()).isEqualTo(LocalDate.of(1980, 1, 15));
        assertThat(dto.getCreditScore()).isEqualTo(750);
        assertThat(dto.getCreditScoreReviewDate()).isEqualTo(LocalDate.of(2023, 12, 1));
        assertThat(dto.getStatus()).isEqualTo("GOOD_STANDING");

        verify(customerRepository).findById(sortCode, customerNumber);
        verify(dtoMapper).toCustomerResponseDto(customer);
    }

    @Test
    void inquireCustomer_WhenCustomerNotFound_ReturnsEmptyOptional() {
        String sortCode = "987654";
        Long customerNumber = 999999L;

        when(customerRepository.findById(sortCode, customerNumber))
                .thenReturn(Optional.empty());

        Optional<CustomerResponseDto> result = customerInquiryService.inquireCustomer(sortCode, customerNumber);

        assertThat(result).isEmpty();

        verify(customerRepository).findById(sortCode, customerNumber);
        verify(dtoMapper, never()).toCustomerResponseDto(any());
    }

    @Test
    void inquireCustomer_WhenRepositoryThrowsException_ThrowsRuntimeException() {
        String sortCode = "987654";
        Long customerNumber = 100001L;

        when(customerRepository.findById(sortCode, customerNumber))
                .thenThrow(new RuntimeException("Database connection error"));

        assertThatThrownBy(() -> customerInquiryService.inquireCustomer(sortCode, customerNumber))
                .isInstanceOf(RuntimeException.class)
                .hasMessageContaining("Failed to inquire customer")
                .hasCauseInstanceOf(RuntimeException.class);

        verify(customerRepository).findById(sortCode, customerNumber);
        verify(dtoMapper, never()).toCustomerResponseDto(any());
    }

    @Test
    void inquireCustomer_WithDifferentSortCode_HandlesCorrectly() {
        String sortCode = "123456";
        Long customerNumber = 100002L;

        Customer customer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode(sortCode)
                .customerNumber(customerNumber)
                .name("Jane Smith")
                .address("456 Oak Ave, Manchester, UK")
                .dateOfBirth(LocalDate.of(1975, 6, 20))
                .creditScore(650)
                .creditScoreReviewDate(LocalDate.of(2023, 11, 15))
                .build();

        CustomerResponseDto expectedDto = CustomerResponseDto.builder()
                .sortCode(sortCode)
                .customerNumber(customerNumber)
                .name("Jane Smith")
                .address("456 Oak Ave, Manchester, UK")
                .dateOfBirth(LocalDate.of(1975, 6, 20))
                .creditScore(650)
                .creditScoreReviewDate(LocalDate.of(2023, 11, 15))
                .status("FAIR")
                .build();

        when(customerRepository.findById(sortCode, customerNumber))
                .thenReturn(Optional.of(customer));
        when(dtoMapper.toCustomerResponseDto(customer))
                .thenReturn(expectedDto);

        Optional<CustomerResponseDto> result = customerInquiryService.inquireCustomer(sortCode, customerNumber);

        assertThat(result).isPresent();
        assertThat(result.get().getSortCode()).isEqualTo(sortCode);
        assertThat(result.get().getCustomerNumber()).isEqualTo(customerNumber);

        verify(customerRepository).findById(sortCode, customerNumber);
        verify(dtoMapper).toCustomerResponseDto(customer);
    }
}
