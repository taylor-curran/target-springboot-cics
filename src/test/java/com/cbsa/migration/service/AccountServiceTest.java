package com.cbsa.migration.service;

import com.cbsa.migration.dto.AccountRequestDto;
import com.cbsa.migration.dto.AccountResponseDto;
import com.cbsa.migration.dto.mapper.DtoMapper;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Control;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class AccountServiceTest {

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private CustomerRepository customerRepository;

    @Mock
    private ControlRepository controlRepository;

    @Mock
    private DtoMapper dtoMapper;

    private AccountService accountService;

    @BeforeEach
    void setUp() {
        accountService = new AccountService(accountRepository, customerRepository, controlRepository, dtoMapper);
    }

    @Test
    @DisplayName("Should create account successfully")
    void shouldCreateAccountSuccessfully() {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .accountType("CURRENT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .initialDeposit(new BigDecimal("500.00"))
                .build();

        Customer customer = new Customer();
        Control control = new Control();
        control.setLastAccountNumber(12345678);
        control.setAccountCount(10);

        Account mockAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345679")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("500.00"))
                .actualBalance(new BigDecimal("500.00"))
                .openedDate(LocalDate.now())
                .build();

        AccountResponseDto expectedResponse = AccountResponseDto.builder()
                .accountNumber("12345679")
                .sortCode("987654")
                .accountType("CURRENT")
                .customerNumber(1234567890L)
                .availableBalance(new BigDecimal("500.00"))
                .actualBalance(new BigDecimal("500.00"))
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .openedDate(LocalDate.now())
                .status("ACTIVE")
                .build();

        when(customerRepository.findById("987654", 1234567890L)).thenReturn(Optional.of(customer));
        when(controlRepository.initializeControlRecord()).thenReturn(control);
        when(dtoMapper.toAccount(request)).thenReturn(mockAccount);
        when(accountRepository.save(any(Account.class))).thenReturn(mockAccount);
        when(dtoMapper.toAccountResponseDto(mockAccount)).thenReturn(expectedResponse);

        // When
        AccountResponseDto result = accountService.createAccount(request);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getAccountNumber()).isEqualTo("12345679");
        assertThat(result.getAvailableBalance()).isEqualTo(new BigDecimal("500.00"));
        verify(controlRepository).save(any(Control.class));
    }

    @Test
    @DisplayName("Should throw exception when customer not found during account creation")
    void shouldThrowExceptionWhenCustomerNotFound() {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .accountType("CURRENT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .build();

        when(customerRepository.findById("987654", 1234567890L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> accountService.createAccount(request))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Customer not found: 1234567890");
    }

    @Test
    @DisplayName("Should update account successfully")
    void shouldUpdateAccountSuccessfully() {
        // Given
        String sortCode = "987654";
        String accountNumber = "12345678";
        AccountRequestDto request = AccountRequestDto.builder()
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("2.50"))
                .overdraftLimit(2000)
                .build();

        Account existingAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .build();

        Account updatedAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("2.50"))
                .overdraftLimit(2000)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .build();

        AccountResponseDto expectedResponse = AccountResponseDto.builder()
                .accountNumber(accountNumber)
                .sortCode(sortCode)
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("2.50"))
                .overdraftLimit(2000)
                .build();

        when(accountRepository.findById(sortCode, accountNumber)).thenReturn(Optional.of(existingAccount));
        when(accountRepository.save(any(Account.class))).thenReturn(updatedAccount);
        when(dtoMapper.toAccountResponseDto(updatedAccount)).thenReturn(expectedResponse);

        // When
        AccountResponseDto result = accountService.updateAccount(sortCode, accountNumber, request);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getAccountType()).isEqualTo("SAVINGS");
        assertThat(result.getInterestRate()).isEqualTo(new BigDecimal("2.50"));
        assertThat(result.getOverdraftLimit()).isEqualTo(2000);
    }

    @Test
    @DisplayName("Should throw exception when account not found during update")
    void shouldThrowExceptionWhenAccountNotFoundDuringUpdate() {
        // Given
        String sortCode = "987654";
        String accountNumber = "12345678";
        AccountRequestDto request = AccountRequestDto.builder().build();

        when(accountRepository.findById(sortCode, accountNumber)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> accountService.updateAccount(sortCode, accountNumber, request))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Account not found: " + sortCode + accountNumber);
    }

    @Test
    @DisplayName("Should get account balance successfully")
    void shouldGetAccountBalanceSuccessfully() {
        // Given
        String sortCode = "987654";
        String accountNumber = "12345678";
        Account account = Account.builder()
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .availableBalance(new BigDecimal("1500.00"))
                .actualBalance(new BigDecimal("1750.00"))
                .build();

        AccountResponseDto expectedResponse = AccountResponseDto.builder()
                .accountNumber(accountNumber)
                .sortCode(sortCode)
                .availableBalance(new BigDecimal("1500.00"))
                .actualBalance(new BigDecimal("1750.00"))
                .build();

        when(accountRepository.findById(sortCode, accountNumber)).thenReturn(Optional.of(account));
        when(dtoMapper.toAccountResponseDto(account)).thenReturn(expectedResponse);

        // When
        AccountResponseDto result = accountService.getAccountBalance(sortCode, accountNumber);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getAvailableBalance()).isEqualTo(new BigDecimal("1500.00"));
        assertThat(result.getActualBalance()).isEqualTo(new BigDecimal("1750.00"));
    }

    @Test
    @DisplayName("Should process overdraft successfully")
    void shouldProcessOverdraftSuccessfully() {
        // Given
        String sortCode = "987654";
        String accountNumber = "12345678";
        BigDecimal overdraftAmount = new BigDecimal("200.00");

        Account account = Account.builder()
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .availableBalance(new BigDecimal("100.00"))
                .actualBalance(new BigDecimal("100.00"))
                .overdraftLimit(500)
                .build();

        Account updatedAccount = Account.builder()
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .availableBalance(new BigDecimal("-100.00"))
                .actualBalance(new BigDecimal("-100.00"))
                .overdraftLimit(500)
                .build();

        AccountResponseDto expectedResponse = AccountResponseDto.builder()
                .accountNumber(accountNumber)
                .sortCode(sortCode)
                .availableBalance(new BigDecimal("-100.00"))
                .actualBalance(new BigDecimal("-100.00"))
                .status("OVERDRAFT")
                .build();

        when(accountRepository.findById(sortCode, accountNumber)).thenReturn(Optional.of(account));
        when(accountRepository.save(any(Account.class))).thenReturn(updatedAccount);
        when(dtoMapper.toAccountResponseDto(updatedAccount)).thenReturn(expectedResponse);

        // When
        AccountResponseDto result = accountService.processOverdraft(sortCode, accountNumber, overdraftAmount);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getAvailableBalance()).isEqualTo(new BigDecimal("-100.00"));
        assertThat(result.getStatus()).isEqualTo("OVERDRAFT");
    }

    @Test
    @DisplayName("Should throw exception when overdraft exceeds limit")
    void shouldThrowExceptionWhenOverdraftExceedsLimit() {
        // Given
        String sortCode = "987654";
        String accountNumber = "12345678";
        BigDecimal overdraftAmount = new BigDecimal("1000.00");

        Account account = Account.builder()
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .availableBalance(new BigDecimal("100.00"))
                .actualBalance(new BigDecimal("100.00"))
                .overdraftLimit(500)
                .build();

        when(accountRepository.findById(sortCode, accountNumber)).thenReturn(Optional.of(account));

        assertThatThrownBy(() -> accountService.processOverdraft(sortCode, accountNumber, overdraftAmount))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Overdraft exceeds limit");
    }

    @Test
    @DisplayName("Should throw exception for negative overdraft amount")
    void shouldThrowExceptionForNegativeOverdraftAmount() {
        // Given
        String sortCode = "987654";
        String accountNumber = "12345678";
        BigDecimal negativeAmount = new BigDecimal("-100.00");

        Account account = Account.builder()
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .availableBalance(new BigDecimal("100.00"))
                .actualBalance(new BigDecimal("100.00"))
                .overdraftLimit(500)
                .build();

        when(accountRepository.findById(sortCode, accountNumber)).thenReturn(Optional.of(account));

        assertThatThrownBy(() -> accountService.processOverdraft(sortCode, accountNumber, negativeAmount))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Overdraft amount must be positive");
    }

    @Test
    @DisplayName("Should get accounts by customer successfully")
    void shouldGetAccountsByCustomerSuccessfully() {
        // Given
        Long customerNumber = 1234567890L;
        List<Account> accounts = Arrays.asList(
                Account.builder().accountNumber("12345678").sortCode("987654").build(),
                Account.builder().accountNumber("12345679").sortCode("987654").build()
        );

        List<AccountResponseDto> expectedResponse = Arrays.asList(
                AccountResponseDto.builder().accountNumber("12345678").sortCode("987654").build(),
                AccountResponseDto.builder().accountNumber("12345679").sortCode("987654").build()
        );

        when(accountRepository.findByCustomerNumber(customerNumber)).thenReturn(accounts);
        when(dtoMapper.toAccountResponseDto(accounts.get(0))).thenReturn(expectedResponse.get(0));
        when(dtoMapper.toAccountResponseDto(accounts.get(1))).thenReturn(expectedResponse.get(1));

        // When
        List<AccountResponseDto> result = accountService.getAccountsByCustomer(customerNumber);

        // Then
        assertThat(result).hasSize(2);
        assertThat(result.get(0).getAccountNumber()).isEqualTo("12345678");
        assertThat(result.get(1).getAccountNumber()).isEqualTo("12345679");
    }

    @Test
    @DisplayName("Should use default interest rate for SAVINGS account when not provided")
    void shouldUseDefaultInterestRateForSavingsAccount() {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .accountType("SAVINGS")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .initialDeposit(new BigDecimal("1000.00"))
                .build();

        Customer customer = new Customer();
        Control control = new Control();
        control.setLastAccountNumber(12345678);
        control.setAccountCount(10);

        Account mockAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345679")
                .accountType("SAVINGS")
                .interestRate(null)
                .overdraftLimit(0)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .openedDate(LocalDate.now())
                .build();

        when(customerRepository.findById("987654", 1234567890L)).thenReturn(Optional.of(customer));
        when(controlRepository.initializeControlRecord()).thenReturn(control);
        when(dtoMapper.toAccount(request)).thenReturn(mockAccount);
        when(accountRepository.save(any(Account.class))).thenReturn(mockAccount);
        when(dtoMapper.toAccountResponseDto(mockAccount)).thenReturn(AccountResponseDto.builder().build());

        // When
        accountService.createAccount(request);

        // Then
        verify(accountRepository).save(argThat(account -> 
            account.getInterestRate().compareTo(new BigDecimal("2.50")) == 0));
    }

    @Test
    @DisplayName("Should use default interest rate for CURRENT account when not provided")
    void shouldUseDefaultInterestRateForCurrentAccount() {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .accountType("CURRENT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .initialDeposit(new BigDecimal("500.00"))
                .build();

        Customer customer = new Customer();
        Control control = new Control();
        control.setLastAccountNumber(12345678);
        control.setAccountCount(10);

        Account mockAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345679")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(0)
                .availableBalance(new BigDecimal("500.00"))
                .actualBalance(new BigDecimal("500.00"))
                .openedDate(LocalDate.now())
                .build();

        when(customerRepository.findById("987654", 1234567890L)).thenReturn(Optional.of(customer));
        when(controlRepository.initializeControlRecord()).thenReturn(control);
        when(dtoMapper.toAccount(request)).thenReturn(mockAccount);
        when(accountRepository.save(any(Account.class))).thenReturn(mockAccount);
        when(dtoMapper.toAccountResponseDto(mockAccount)).thenReturn(AccountResponseDto.builder().build());

        // When
        accountService.createAccount(request);

        // Then
        verify(accountRepository).save(argThat(account -> 
            account.getInterestRate().compareTo(new BigDecimal("0.10")) == 0));
    }

    @Test
    @DisplayName("Should use default interest rate for unknown account type")
    void shouldUseDefaultInterestRateForUnknownAccountType() {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .accountType("UNKNOWN")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .initialDeposit(new BigDecimal("750.00"))
                .build();

        Customer customer = new Customer();
        Control control = new Control();
        control.setLastAccountNumber(12345678);
        control.setAccountCount(10);

        Account mockAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345679")
                .accountType("UNKNOWN")
                .interestRate(null)
                .overdraftLimit(0)
                .availableBalance(new BigDecimal("750.00"))
                .actualBalance(new BigDecimal("750.00"))
                .openedDate(LocalDate.now())
                .build();

        when(customerRepository.findById("987654", 1234567890L)).thenReturn(Optional.of(customer));
        when(controlRepository.initializeControlRecord()).thenReturn(control);
        when(dtoMapper.toAccount(request)).thenReturn(mockAccount);
        when(accountRepository.save(any(Account.class))).thenReturn(mockAccount);
        when(dtoMapper.toAccountResponseDto(mockAccount)).thenReturn(AccountResponseDto.builder().build());

        // When
        accountService.createAccount(request);

        // Then
        verify(accountRepository).save(argThat(account -> 
            account.getInterestRate().compareTo(new BigDecimal("1.00")) == 0));
    }

    @Test
    @DisplayName("Should get account details successfully")
    void shouldGetAccountDetailsSuccessfully() {
        // Given
        String sortCode = "987654";
        String accountNumber = "12345678";
        Account account = Account.builder()
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .accountType("CURRENT")
                .customerNumber(1234567890L)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .openedDate(LocalDate.of(2023, 1, 15))
                .build();

        AccountResponseDto expectedResponse = AccountResponseDto.builder()
                .accountNumber(accountNumber)
                .sortCode(sortCode)
                .accountType("CURRENT")
                .customerNumber(1234567890L)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .openedDate(LocalDate.of(2023, 1, 15))
                .status("ACTIVE")
                .build();

        when(accountRepository.findById(sortCode, accountNumber)).thenReturn(Optional.of(account));
        when(dtoMapper.toAccountResponseDto(account)).thenReturn(expectedResponse);

        // When
        AccountResponseDto result = accountService.getAccountDetails(sortCode, accountNumber);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getAccountNumber()).isEqualTo(accountNumber);
        assertThat(result.getAccountType()).isEqualTo("CURRENT");
        assertThat(result.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(result.getOverdraftLimit()).isEqualTo(1000);
    }

    @Test
    @DisplayName("Should throw exception when account not found for getAccountDetails")
    void shouldThrowExceptionWhenAccountNotFoundForGetAccountDetails() {
        // Given
        String sortCode = "987654";
        String accountNumber = "12345678";

        when(accountRepository.findById(sortCode, accountNumber)).thenReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> accountService.getAccountDetails(sortCode, accountNumber))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Account not found: " + sortCode + accountNumber);
    }

    @Test
    @DisplayName("Should set default overdraft limit when not provided")
    void shouldSetDefaultOverdraftLimitWhenNotProvided() {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .accountType("CURRENT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .interestRate(new BigDecimal("0.10"))
                .initialDeposit(new BigDecimal("500.00"))
                .build();

        Customer customer = new Customer();
        Control control = new Control();
        control.setLastAccountNumber(12345678);
        control.setAccountCount(10);

        Account mockAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345679")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(0)
                .availableBalance(new BigDecimal("500.00"))
                .actualBalance(new BigDecimal("500.00"))
                .openedDate(LocalDate.now())
                .build();

        when(customerRepository.findById("987654", 1234567890L)).thenReturn(Optional.of(customer));
        when(controlRepository.initializeControlRecord()).thenReturn(control);
        when(dtoMapper.toAccount(request)).thenReturn(mockAccount);
        when(accountRepository.save(any(Account.class))).thenReturn(mockAccount);
        when(dtoMapper.toAccountResponseDto(mockAccount)).thenReturn(AccountResponseDto.builder().build());

        // When
        accountService.createAccount(request);

        // Then
        verify(accountRepository).save(argThat(account -> 
            account.getOverdraftLimit().equals(0)));
    }

    @Test
    @DisplayName("Should set zero balance when no initial deposit provided")
    void shouldSetZeroBalanceWhenNoInitialDepositProvided() {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .accountType("CURRENT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .build();

        Customer customer = new Customer();
        Control control = new Control();
        control.setLastAccountNumber(12345678);
        control.setAccountCount(10);

        Account mockAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345679")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .availableBalance(BigDecimal.ZERO)
                .actualBalance(BigDecimal.ZERO)
                .openedDate(LocalDate.now())
                .build();

        when(customerRepository.findById("987654", 1234567890L)).thenReturn(Optional.of(customer));
        when(controlRepository.initializeControlRecord()).thenReturn(control);
        when(dtoMapper.toAccount(request)).thenReturn(mockAccount);
        when(accountRepository.save(any(Account.class))).thenReturn(mockAccount);
        when(dtoMapper.toAccountResponseDto(mockAccount)).thenReturn(AccountResponseDto.builder().build());

        // When
        accountService.createAccount(request);

        // Then
        verify(accountRepository).save(argThat(account -> 
            account.getAvailableBalance().compareTo(BigDecimal.ZERO) == 0 &&
            account.getActualBalance().compareTo(BigDecimal.ZERO) == 0));
    }

    @Test
    @DisplayName("Should update only provided fields in updateAccount")
    void shouldUpdateOnlyProvidedFieldsInUpdateAccount() {
        // Given
        String sortCode = "987654";
        String accountNumber = "12345678";
        AccountRequestDto request = AccountRequestDto.builder()
                .accountType("SAVINGS")
                .build();

        Account existingAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .build();

        Account updatedAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("0.10"))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .build();

        when(accountRepository.findById(sortCode, accountNumber)).thenReturn(Optional.of(existingAccount));
        when(accountRepository.save(any(Account.class))).thenReturn(updatedAccount);
        when(dtoMapper.toAccountResponseDto(updatedAccount)).thenReturn(AccountResponseDto.builder().build());

        // When
        accountService.updateAccount(sortCode, accountNumber, request);

        // Then
        verify(accountRepository).save(argThat(account -> 
            account.getAccountType().equals("SAVINGS") &&
            account.getInterestRate().compareTo(new BigDecimal("0.10")) == 0 &&
            account.getOverdraftLimit().equals(1000)));
    }
}
