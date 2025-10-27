package com.cbsa.migration.service;

import com.cbsa.migration.dto.CreditScoreRequestDto;
import com.cbsa.migration.dto.CreditScoreResponseDto;
import com.cbsa.migration.dto.CustomerRequestDto;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Control;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class CustomerServiceTest {
    
    @Mock
    private CustomerRepository customerRepository;
    
    @Mock
    private ControlRepository controlRepository;
    
    @Mock
    private CreditAgencyService creditAgencyService;
    
    @Mock
    private TransactionRepository transactionRepository;
    
    @Mock
    private SortCodeService sortCodeService;
    
    @Mock
    private AccountRepository accountRepository;
    
    private CustomerService customerService;
    
    @BeforeEach
    void setUp() {
        customerService = new CustomerService(
            customerRepository,
            controlRepository,
            creditAgencyService,
            transactionRepository,
            sortCodeService,
            accountRepository
        );
    }
    
    @Test
    void testInquireCustomer_Normal() {
        String sortCode = "123456";
        Long customerNumber = 100001L;
        Customer expectedCustomer = Customer.builder()
            .sortCode(sortCode)
            .customerNumber(customerNumber)
            .name("Mr John Smith")
            .build();
        
        when(customerRepository.findById(sortCode, customerNumber))
            .thenReturn(Optional.of(expectedCustomer));
        
        Optional<Customer> result = customerService.inquireCustomer(sortCode, customerNumber);
        
        assertThat(result).isPresent();
        assertThat(result.get().getCustomerNumber()).isEqualTo(customerNumber);
    }
    
    @Test
    void testInquireCustomer_Last() {
        String sortCode = "123456";
        Long lastCustomerNumber = 100050L;
        Control control = Control.builder()
            .lastCustomerNumber(lastCustomerNumber)
            .build();
        Customer expectedCustomer = Customer.builder()
            .sortCode(sortCode)
            .customerNumber(lastCustomerNumber)
            .name("Mr John Smith")
            .build();
        
        when(controlRepository.getControl()).thenReturn(Optional.of(control));
        when(customerRepository.findById(sortCode, lastCustomerNumber))
            .thenReturn(Optional.of(expectedCustomer));
        
        Optional<Customer> result = customerService.inquireCustomer(sortCode, 9999999999L);
        
        assertThat(result).isPresent();
        assertThat(result.get().getCustomerNumber()).isEqualTo(lastCustomerNumber);
    }
    
    @Test
    void testInquireCustomer_Random() {
        String sortCode = "123456";
        Long lastCustomerNumber = 100050L;
        Control control = Control.builder()
            .lastCustomerNumber(lastCustomerNumber)
            .build();
        Customer expectedCustomer = Customer.builder()
            .sortCode(sortCode)
            .customerNumber(100025L)
            .name("Mr John Smith")
            .build();
        
        when(controlRepository.getControl()).thenReturn(Optional.of(control));
        when(customerRepository.findById(eq(sortCode), any(Long.class)))
            .thenReturn(Optional.empty())
            .thenReturn(Optional.of(expectedCustomer));
        
        Optional<Customer> result = customerService.inquireCustomer(sortCode, 0L);
        
        assertThat(result).isPresent();
    }
    
    @Test
    void testInquireCustomer_NotFound() {
        String sortCode = "123456";
        Long customerNumber = 100001L;
        
        when(customerRepository.findById(sortCode, customerNumber))
            .thenReturn(Optional.empty());
        
        Optional<Customer> result = customerService.inquireCustomer(sortCode, customerNumber);
        
        assertThat(result).isEmpty();
    }
    
    @Test
    void testCreateCustomer_Success() {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .sortCode("123456")
            .name("Mr John Smith")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1980, 1, 1))
            .build();
        
        Long nextCustomerNumber = 100001L;
        when(controlRepository.getNextCustomerNumber()).thenReturn(nextCustomerNumber);
        
        CreditScoreResponseDto creditResponse = CreditScoreResponseDto.builder()
            .success(true)
            .updatedCreditScore(750)
            .build();
        when(creditAgencyService.processCredit(any(CreditScoreRequestDto.class)))
            .thenReturn(creditResponse);
        
        Customer savedCustomer = Customer.builder()
            .sortCode("123456")
            .customerNumber(nextCustomerNumber)
            .name("Mr John Smith")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1980, 1, 1))
            .creditScore(750)
            .build();
        when(customerRepository.save(any(Customer.class))).thenReturn(savedCustomer);
        
        Customer result = customerService.createCustomer(request);
        
        assertThat(result.getCustomerNumber()).isEqualTo(nextCustomerNumber);
        assertThat(result.getCreditScore()).isGreaterThan(0);
        verify(transactionRepository).save(any(Transaction.class));
    }
    
    @Test
    void testCreateCustomer_DateValidation_TooOld() {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .sortCode("123456")
            .name("Mr John Smith")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1600, 1, 1))
            .build();
        
        assertThatThrownBy(() -> customerService.createCustomer(request))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("1601");
    }
    
    @Test
    void testCreateCustomer_DateValidation_TooYoung() {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .sortCode("123456")
            .name("Mr John Smith")
            .address("123 Main St")
            .dateOfBirth(LocalDate.now().minusYears(151))
            .build();
        
        assertThatThrownBy(() -> customerService.createCustomer(request))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("150");
    }
    
    @Test
    void testCreateCustomer_DateValidation_FutureDate() {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .sortCode("123456")
            .name("Mr John Smith")
            .address("123 Main St")
            .dateOfBirth(LocalDate.now().plusDays(1))
            .build();
        
        assertThatThrownBy(() -> customerService.createCustomer(request))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("future");
    }
    
    @Test
    void testCreateCustomer_CreditCheckFailure() {
        CustomerRequestDto request = CustomerRequestDto.builder()
            .sortCode("123456")
            .name("Mr John Smith")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1980, 1, 1))
            .build();
        
        Long nextCustomerNumber = 100001L;
        when(controlRepository.getNextCustomerNumber()).thenReturn(nextCustomerNumber);
        
        when(creditAgencyService.processCredit(any(CreditScoreRequestDto.class)))
            .thenThrow(new RuntimeException("Service unavailable"));
        
        Customer savedCustomer = Customer.builder()
            .sortCode("123456")
            .customerNumber(nextCustomerNumber)
            .name("Mr John Smith")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1980, 1, 1))
            .creditScore(0)
            .build();
        when(customerRepository.save(any(Customer.class))).thenReturn(savedCustomer);
        
        Customer result = customerService.createCustomer(request);
        
        assertThat(result.getCreditScore()).isEqualTo(0);
    }
    
    @Test
    void testUpdateCustomer_Success() {
        String sortCode = "123456";
        Long customerNumber = 100001L;
        String newName = "Mr John Doe";
        String newAddress = "456 Oak St";
        
        Customer existingCustomer = Customer.builder()
            .sortCode(sortCode)
            .customerNumber(customerNumber)
            .name("Mr John Smith")
            .address("123 Main St")
            .build();
        
        when(customerRepository.findById(sortCode, customerNumber))
            .thenReturn(Optional.of(existingCustomer));
        when(customerRepository.save(any(Customer.class))).thenReturn(existingCustomer);
        
        Customer result = customerService.updateCustomer(sortCode, customerNumber, newName, newAddress);
        
        assertThat(result.getName()).isEqualTo(newName);
        assertThat(result.getAddress()).isEqualTo(newAddress);
        verify(transactionRepository, never()).save(any());
    }
    
    @Test
    void testUpdateCustomer_InvalidTitle() {
        String sortCode = "123456";
        Long customerNumber = 100001L;
        String invalidName = "InvalidTitle John Smith";
        
        Customer existingCustomer = Customer.builder()
            .sortCode(sortCode)
            .customerNumber(customerNumber)
            .name("Mr John Smith")
            .build();
        
        when(customerRepository.findById(sortCode, customerNumber))
            .thenReturn(Optional.of(existingCustomer));
        
        assertThatThrownBy(() -> customerService.updateCustomer(sortCode, customerNumber, invalidName, null))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("title");
    }
    
    @Test
    void testUpdateCustomer_BothFieldsEmpty() {
        String sortCode = "123456";
        Long customerNumber = 100001L;
        
        assertThatThrownBy(() -> customerService.updateCustomer(sortCode, customerNumber, "", ""))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("cannot both be empty");
    }
    
    @Test
    void testUpdateCustomer_NotFound() {
        String sortCode = "123456";
        Long customerNumber = 100001L;
        
        when(customerRepository.findById(sortCode, customerNumber))
            .thenReturn(Optional.empty());
        
        assertThatThrownBy(() -> customerService.updateCustomer(sortCode, customerNumber, "Mr John Doe", null))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("not found");
    }
    
    @Test
    void testDeleteCustomer_WithAccounts() {
        String sortCode = "123456";
        Long customerNumber = 100001L;
        
        Customer customer = Customer.builder()
            .sortCode(sortCode)
            .customerNumber(customerNumber)
            .name("Mr John Smith")
            .dateOfBirth(LocalDate.of(1980, 1, 1))
            .build();
        
        Account account1 = Account.builder()
            .sortCode(sortCode)
            .accountNumber("10000001")
            .build();
        Account account2 = Account.builder()
            .sortCode(sortCode)
            .accountNumber("10000002")
            .build();
        
        when(customerRepository.findById(sortCode, customerNumber))
            .thenReturn(Optional.of(customer));
        when(accountRepository.findByCustomerNumber(customerNumber))
            .thenReturn(Arrays.asList(account1, account2));
        when(customerRepository.deleteById(sortCode, customerNumber)).thenReturn(true);
        
        customerService.deleteCustomer(sortCode, customerNumber);
        
        verify(accountRepository, times(2)).deleteById(anyString(), anyString());
        verify(customerRepository).deleteById(sortCode, customerNumber);
        verify(transactionRepository).save(any(Transaction.class));
    }
    
    @Test
    void testDeleteCustomer_WithoutAccounts() {
        String sortCode = "123456";
        Long customerNumber = 100001L;
        
        Customer customer = Customer.builder()
            .sortCode(sortCode)
            .customerNumber(customerNumber)
            .name("Mr John Smith")
            .dateOfBirth(LocalDate.of(1980, 1, 1))
            .build();
        
        when(customerRepository.findById(sortCode, customerNumber))
            .thenReturn(Optional.of(customer));
        when(accountRepository.findByCustomerNumber(customerNumber))
            .thenReturn(Collections.emptyList());
        when(customerRepository.deleteById(sortCode, customerNumber)).thenReturn(true);
        
        customerService.deleteCustomer(sortCode, customerNumber);
        
        verify(accountRepository, never()).deleteById(anyString(), anyString());
        verify(customerRepository).deleteById(sortCode, customerNumber);
        verify(transactionRepository).save(any(Transaction.class));
    }
    
    @Test
    void testDeleteCustomer_NotFound() {
        String sortCode = "123456";
        Long customerNumber = 100001L;
        
        when(customerRepository.findById(sortCode, customerNumber))
            .thenReturn(Optional.empty());
        
        assertThatThrownBy(() -> customerService.deleteCustomer(sortCode, customerNumber))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("not found");
    }
}
