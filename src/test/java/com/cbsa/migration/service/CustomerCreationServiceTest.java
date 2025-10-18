package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerCreationRequestDto;
import com.cbsa.migration.dto.CustomerCreationResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CustomerCreationServiceTest {

    @Mock
    private CustomerRepository customerRepository;
    
    @Mock
    private ControlRepository controlRepository;
    
    @Mock
    private TransactionRepository transactionRepository;
    
    @Mock
    private CreditAgencyService creditAgencyService;
    
    private CustomerCreationService service;
    
    @BeforeEach
    void setUp() {
        service = new CustomerCreationService(
            customerRepository,
            controlRepository,
            transactionRepository,
            creditAgencyService,
            false,
            5
        );
    }
    
    @Test
    void shouldCreateCustomerSuccessfully() {
        CustomerCreationRequestDto request = CustomerCreationRequestDto.builder()
            .sortCode("987654")
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1990, 1, 15))
            .build();
            
        when(controlRepository.getNextCustomerNumber()).thenReturn(100001L);
        when(customerRepository.save(any(Customer.class))).thenAnswer(i -> i.getArguments()[0]);
        
        CustomerCreationResponseDto response = service.createCustomer(request);
        
        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getCustomerNumber()).isEqualTo(100001L);
        assertThat(response.getSortCode()).isEqualTo("987654");
        assertThat(response.getEyeCatcher()).isEqualTo("CUST");
        assertThat(response.getFailCode()).isNull();
        
        verify(controlRepository).getNextCustomerNumber();
        verify(customerRepository).save(any(Customer.class));
        verify(transactionRepository).save(any());
    }
    
    @Test
    void shouldRejectDateOfBirthBefore1601() {
        CustomerCreationRequestDto request = CustomerCreationRequestDto.builder()
            .sortCode("987654")
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1600, 12, 31))
            .build();
        
        CustomerCreationResponseDto response = service.createCustomer(request);
        
        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("O");
        verify(controlRepository, never()).getNextCustomerNumber();
    }
    
    @Test
    void shouldRejectDateOfBirthInFuture() {
        CustomerCreationRequestDto request = CustomerCreationRequestDto.builder()
            .sortCode("987654")
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.now().plusDays(1))
            .build();
        
        CustomerCreationResponseDto response = service.createCustomer(request);
        
        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("Y");
        verify(controlRepository, never()).getNextCustomerNumber();
    }
    
    @Test
    void shouldRejectAgeOver150Years() {
        CustomerCreationRequestDto request = CustomerCreationRequestDto.builder()
            .sortCode("987654")
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.now().minusYears(151))
            .build();
        
        CustomerCreationResponseDto response = service.createCustomer(request);
        
        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("O");
        verify(controlRepository, never()).getNextCustomerNumber();
    }
    
    @Test
    void shouldSetCreditScoreToZeroWhenCreditCheckFails() {
        CustomerCreationRequestDto request = CustomerCreationRequestDto.builder()
            .sortCode("987654")
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1990, 1, 15))
            .build();
            
        when(controlRepository.getNextCustomerNumber()).thenReturn(100001L);
        when(customerRepository.save(any(Customer.class))).thenAnswer(i -> i.getArguments()[0]);
        when(creditAgencyService.generateCreditScore()).thenThrow(new RuntimeException("Credit check failed"));
        
        CustomerCreationService serviceWithCreditCheck = new CustomerCreationService(
            customerRepository,
            controlRepository,
            transactionRepository,
            creditAgencyService,
            true,
            5
        );
        
        CustomerCreationResponseDto response = serviceWithCreditCheck.createCustomer(request);
        
        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getCreditScore()).isEqualTo(0);
        assertThat(response.getCreditScoreReviewDate()).isEqualTo(LocalDate.now());
    }
}
