package com.cbsa.migration.service;

import com.cbsa.migration.model.AccountCreationRequest;
import com.cbsa.migration.model.AccountCreationResponse;
import com.cbsa.migration.model.CustomerInquiryResponse;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class AccountCreationServiceTest {

    @Mock
    private CustomerInquiryService customerInquiryService;

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private TransactionRepository transactionRepository;

    @Mock
    private ControlRepository controlRepository;

    @Mock
    private SortCodeService sortCodeService;

    @InjectMocks
    private AccountCreationService accountCreationService;

    private AccountCreationRequest testRequest;
    private CustomerInquiryResponse successfulCustomerResponse;

    @BeforeEach
    void setUp() {
        testRequest = AccountCreationRequest.builder()
                .customerNumber(1234567890L)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("2.5"))
                .overdraftLimit(1000)
                .initialBalance(new BigDecimal("100.00"))
                .build();

        successfulCustomerResponse = CustomerInquiryResponse.builder()
                .success(true)
                .failureCode("0")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1980, 1, 1))
                .build();

    }

    @Test
    void testCreateAccount_Success() {
        when(sortCodeService.getSortCode()).thenReturn("987654");
        when(customerInquiryService.inquireCustomer(1234567890L)).thenReturn(successfulCustomerResponse);
        when(accountRepository.countByCustomerNumber(1234567890L)).thenReturn(2);
        when(controlRepository.getControlValueNum("987654-ACCOUNT-LAST")).thenReturn(1000);
        when(controlRepository.getControlValueNum("987654-ACCOUNT-COUNT")).thenReturn(50);

        AccountCreationResponse response = accountCreationService.createAccount(testRequest);

        assertTrue(response.isSuccess());
        assertEquals("0", response.getFailureCode());
        assertEquals("00001001", response.getAccountNumber());
        assertEquals(1234567890L, response.getCustomerNumber());
        assertEquals("John Doe", response.getName());
    }

    @Test
    void testCreateAccount_CustomerNotFound() {
        CustomerInquiryResponse failedCustomerResponse = CustomerInquiryResponse.builder()
                .success(false)
                .failureCode("1")
                .build();

        when(customerInquiryService.inquireCustomer(1234567890L)).thenReturn(failedCustomerResponse);

        AccountCreationResponse response = accountCreationService.createAccount(testRequest);

        assertFalse(response.isSuccess());
        assertEquals("1", response.getFailureCode());
    }

    @Test
    void testCreateAccount_TooManyAccounts() {
        when(customerInquiryService.inquireCustomer(1234567890L)).thenReturn(successfulCustomerResponse);
        when(accountRepository.countByCustomerNumber(1234567890L)).thenReturn(9);

        AccountCreationResponse response = accountCreationService.createAccount(testRequest);

        assertFalse(response.isSuccess());
        assertEquals("8", response.getFailureCode());
    }

    @Test
    void testCreateAccount_InvalidAccountType() {
        testRequest.setAccountType("INVALID");

        when(customerInquiryService.inquireCustomer(1234567890L)).thenReturn(successfulCustomerResponse);
        when(accountRepository.countByCustomerNumber(1234567890L)).thenReturn(2);

        AccountCreationResponse response = accountCreationService.createAccount(testRequest);

        assertFalse(response.isSuccess());
        assertEquals("7", response.getFailureCode());
    }
}
