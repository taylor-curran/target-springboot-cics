package com.cbsa.migration.service;

import com.cbsa.migration.dto.AccountRequestDto;
import com.cbsa.migration.dto.AccountResponseDto;
import com.cbsa.migration.exception.AccountCreationException;
import com.cbsa.migration.exception.AccountNotFoundException;
import com.cbsa.migration.exception.CustomerNotFoundException;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class AccountServiceTest {

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private CustomerRepository customerRepository;

    @Mock
    private TransactionRepository transactionRepository;

    @InjectMocks
    private AccountService accountService;

    private Customer testCustomer;
    private Account testAccount;
    private AccountRequestDto testRequest;

    @BeforeEach
    void setUp() {
        testCustomer = Customer.builder()
            .sortCode("987654")
            .customerNumber(1L)
            .name("John Doe")
            .address("123 Main St")
            .dateOfBirth(LocalDate.of(1980, 1, 1))
            .creditScore(750)
            .build();

        testAccount = Account.builder()
            .eyeCatcher("ACCT")
            .sortCode("987654")
            .accountNumber("00000001")
            .customerNumber(1L)
            .accountType("CURRENT")
            .interestRate(new BigDecimal("2.5"))
            .openedDate(LocalDate.now())
            .overdraftLimit(1000)
            .availableBalance(new BigDecimal("100.00"))
            .actualBalance(new BigDecimal("100.00"))
            .build();

        testRequest = AccountRequestDto.builder()
            .sortCode("987654")
            .customerNumber(1L)
            .accountType("CURRENT")
            .interestRate(new BigDecimal("2.5"))
            .overdraftLimit(1000)
            .initialDeposit(new BigDecimal("100.00"))
            .build();
    }

    @Test
    void testCreateAccount_Success() {
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.of(testCustomer));
        when(accountRepository.findByCustomerNumber(1L)).thenReturn(new ArrayList<>());
        when(accountRepository.save(any(Account.class))).thenReturn(testAccount);

        AccountResponseDto response = accountService.createAccount(testRequest);

        assertNotNull(response);
        assertEquals("987654", response.getSortCode());
        assertEquals(1L, response.getCustomerNumber());
        assertEquals("CURRENT", response.getAccountType());
        assertEquals("John Doe", response.getCustomerName());
        verify(accountRepository).save(any(Account.class));
    }

    @Test
    void testCreateAccount_CustomerNotFound() {
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.empty());

        AccountCreationException exception = assertThrows(AccountCreationException.class, () -> {
            accountService.createAccount(testRequest);
        });

        assertEquals("Customer not found", exception.getMessage());
        assertEquals("1", exception.getErrorCode());
        verify(accountRepository, never()).save(any(Account.class));
    }

    @Test
    void testCreateAccount_TooManyAccounts() {
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.of(testCustomer));
        
        List<Account> tenAccounts = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            tenAccounts.add(testAccount);
        }
        when(accountRepository.findByCustomerNumber(1L)).thenReturn(tenAccounts);

        AccountCreationException exception = assertThrows(AccountCreationException.class, () -> {
            accountService.createAccount(testRequest);
        });

        assertEquals("Customer already has maximum number of accounts", exception.getMessage());
        assertEquals("8", exception.getErrorCode());
        verify(accountRepository, never()).save(any(Account.class));
    }

    @Test
    void testCreateAccount_InvalidAccountType() {
        testRequest.setAccountType("INVALID");
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.of(testCustomer));
        when(accountRepository.findByCustomerNumber(1L)).thenReturn(new ArrayList<>());

        assertThrows(IllegalArgumentException.class, () -> {
            accountService.createAccount(testRequest);
        });

        verify(accountRepository, never()).save(any(Account.class));
    }

    @Test
    void testGetAccount_Success() {
        when(accountRepository.findById("987654", "00000001")).thenReturn(Optional.of(testAccount));
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.of(testCustomer));

        AccountResponseDto response = accountService.getAccount("987654", "00000001");

        assertNotNull(response);
        assertEquals("00000001", response.getAccountNumber());
        assertEquals("987654", response.getSortCode());
        assertEquals("John Doe", response.getCustomerName());
    }

    @Test
    void testGetAccount_NotFound() {
        when(accountRepository.findById("987654", "00000001")).thenReturn(Optional.empty());

        assertThrows(AccountNotFoundException.class, () -> {
            accountService.getAccount("987654", "00000001");
        });
    }

    @Test
    void testUpdateAccount_Success() {
        AccountRequestDto updateRequest = AccountRequestDto.builder()
            .accountType("SAVINGS")
            .interestRate(new BigDecimal("3.0"))
            .overdraftLimit(500)
            .build();

        when(accountRepository.findById("987654", "00000001")).thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class))).thenReturn(testAccount);
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.of(testCustomer));

        AccountResponseDto response = accountService.updateAccount("987654", "00000001", updateRequest);

        assertNotNull(response);
        verify(accountRepository).save(any(Account.class));
    }

    @Test
    void testUpdateAccount_AccountNotFound() {
        AccountRequestDto updateRequest = AccountRequestDto.builder()
            .accountType("SAVINGS")
            .interestRate(new BigDecimal("3.0"))
            .build();

        when(accountRepository.findById("987654", "00000001")).thenReturn(Optional.empty());

        assertThrows(AccountNotFoundException.class, () -> {
            accountService.updateAccount("987654", "00000001", updateRequest);
        });
    }

    @Test
    void testUpdateAccount_InvalidAccountType() {
        AccountRequestDto updateRequest = AccountRequestDto.builder()
            .accountType("   ")
            .build();

        when(accountRepository.findById("987654", "00000001")).thenReturn(Optional.of(testAccount));

        assertThrows(IllegalArgumentException.class, () -> {
            accountService.updateAccount("987654", "00000001", updateRequest);
        });

        verify(accountRepository, never()).save(any(Account.class));
    }

    @Test
    void testDeleteAccount_Success() {
        when(accountRepository.findById("987654", "00000001")).thenReturn(Optional.of(testAccount));
        when(accountRepository.deleteById("987654", "00000001")).thenReturn(true);

        assertDoesNotThrow(() -> {
            accountService.deleteAccount("987654", "00000001");
        });

        verify(accountRepository).deleteById("987654", "00000001");
        verify(transactionRepository).save(any(Transaction.class));
    }

    @Test
    void testDeleteAccount_NotFound() {
        when(accountRepository.findById("987654", "00000001")).thenReturn(Optional.empty());

        assertThrows(AccountNotFoundException.class, () -> {
            accountService.deleteAccount("987654", "00000001");
        });

        verify(accountRepository, never()).deleteById(anyString(), anyString());
    }

    @Test
    void testGetAccountsByCustomer_Success() {
        List<Account> accounts = Arrays.asList(testAccount);
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.of(testCustomer));
        when(accountRepository.findByCustomerNumber(1L)).thenReturn(accounts);

        List<AccountResponseDto> response = accountService.getAccountsByCustomer("987654", 1L);

        assertNotNull(response);
        assertEquals(1, response.size());
        assertEquals("00000001", response.get(0).getAccountNumber());
    }

    @Test
    void testGetAccountsByCustomer_CustomerNotFound() {
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.empty());

        assertThrows(CustomerNotFoundException.class, () -> {
            accountService.getAccountsByCustomer("987654", 1L);
        });
    }

    @Test
    void testGetAccountsByCustomer_NoAccounts() {
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.of(testCustomer));
        when(accountRepository.findByCustomerNumber(1L)).thenReturn(new ArrayList<>());

        List<AccountResponseDto> response = accountService.getAccountsByCustomer("987654", 1L);

        assertNotNull(response);
        assertEquals(0, response.size());
    }

    @Test
    void testCreateAccount_WithNullValues() {
        AccountRequestDto requestWithNulls = AccountRequestDto.builder()
            .sortCode("987654")
            .customerNumber(1L)
            .accountType("SAVINGS")
            .interestRate(null)
            .overdraftLimit(null)
            .initialDeposit(null)
            .build();

        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.of(testCustomer));
        when(accountRepository.findByCustomerNumber(1L)).thenReturn(new ArrayList<>());
        when(accountRepository.save(any(Account.class))).thenAnswer(invocation -> {
            Account account = invocation.getArgument(0);
            assertEquals(BigDecimal.ZERO, account.getInterestRate());
            assertEquals(0, account.getOverdraftLimit());
            assertEquals(BigDecimal.ZERO, account.getAvailableBalance());
            assertEquals(BigDecimal.ZERO, account.getActualBalance());
            return account;
        });

        AccountResponseDto response = accountService.createAccount(requestWithNulls);

        assertNotNull(response);
        verify(accountRepository).save(any(Account.class));
    }

    @Test
    void testUpdateAccount_WithNullValues() {
        AccountRequestDto updateRequest = AccountRequestDto.builder()
            .accountType("SAVINGS")
            .interestRate(null)
            .overdraftLimit(null)
            .build();

        when(accountRepository.findById("987654", "00000001")).thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class))).thenAnswer(invocation -> {
            Account account = invocation.getArgument(0);
            assertEquals(testAccount.getInterestRate(), account.getInterestRate());
            assertEquals(testAccount.getOverdraftLimit(), account.getOverdraftLimit());
            return account;
        });
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.of(testCustomer));

        AccountResponseDto response = accountService.updateAccount("987654", "00000001", updateRequest);

        assertNotNull(response);
        verify(accountRepository).save(any(Account.class));
    }

    @Test
    void testGetAccount_CustomerNotFound() {
        when(accountRepository.findById("987654", "00000001")).thenReturn(Optional.of(testAccount));
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.empty());

        assertThrows(CustomerNotFoundException.class, () -> {
            accountService.getAccount("987654", "00000001");
        });
    }

    @Test
    void testUpdateAccount_CustomerNotFound() {
        AccountRequestDto updateRequest = AccountRequestDto.builder()
            .accountType("SAVINGS")
            .interestRate(new BigDecimal("3.0"))
            .build();

        when(accountRepository.findById("987654", "00000001")).thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class))).thenReturn(testAccount);
        when(customerRepository.findById("987654", 1L)).thenReturn(Optional.empty());

        assertThrows(CustomerNotFoundException.class, () -> {
            accountService.updateAccount("987654", "00000001", updateRequest);
        });
    }

    @Test
    void testDeleteAccount_DeleteFailed() {
        when(accountRepository.findById("987654", "00000001")).thenReturn(Optional.of(testAccount));
        when(accountRepository.deleteById("987654", "00000001")).thenReturn(false);

        assertThrows(com.cbsa.migration.exception.AccountDeletionException.class, () -> {
            accountService.deleteAccount("987654", "00000001");
        });

        verify(accountRepository).deleteById("987654", "00000001");
        verify(transactionRepository, never()).save(any(Transaction.class));
    }
}
