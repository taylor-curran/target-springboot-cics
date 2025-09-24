package com.cbsa.migration.service;

import com.cbsa.migration.dto.AccountCreationRequestDto;
import com.cbsa.migration.dto.AccountCreationResponseDto;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.repository.AccountRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class AccountCreationServiceTest {

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private SortCodeService sortCodeService;

    @InjectMocks
    private AccountCreationService accountCreationService;

    private AccountCreationRequestDto validRequest;

    @BeforeEach
    void setUp() {
        validRequest = new AccountCreationRequestDto(
            1234567890L,
            "CURRENT",
            new BigDecimal("2.50"),
            1000
        );
    }

    @Test
    void createAccount_WithValidRequest_ShouldReturnSuccessResponse() {
        when(sortCodeService.getSortCode()).thenReturn("123456");
        when(accountRepository.findByCustomerNumber(1234567890L)).thenReturn(new ArrayList<>());
        when(accountRepository.findTopBySortCodeOrderByAccountNumberDesc("123456")).thenReturn(Optional.empty());
        when(accountRepository.save(any(Account.class))).thenAnswer(invocation -> invocation.getArgument(0));

        AccountCreationResponseDto response = accountCreationService.createAccount(validRequest);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isNull();
        assertThat(response.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(response.getAccountType()).isEqualTo("CURRENT");
        assertThat(response.getSortCode()).isEqualTo("123456");
        assertThat(response.getAccountNumber()).isEqualTo("00000001");
    }

    @Test
    void createAccount_WithInvalidCustomerNumber_ShouldReturnFailureResponse() {
        AccountCreationRequestDto invalidRequest = new AccountCreationRequestDto(
            null,
            "CURRENT",
            new BigDecimal("2.50"),
            1000
        );

        AccountCreationResponseDto response = accountCreationService.createAccount(invalidRequest);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("1");
    }

    @Test
    void createAccount_WithTooManyExistingAccounts_ShouldReturnFailureResponse() {
        List<Account> existingAccounts = new ArrayList<>();
        for (int i = 0; i < 9; i++) {
            existingAccounts.add(new Account());
        }
        when(accountRepository.findByCustomerNumber(1234567890L)).thenReturn(existingAccounts);

        AccountCreationResponseDto response = accountCreationService.createAccount(validRequest);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("8");
    }

    @Test
    void createAccount_WithInvalidAccountType_ShouldReturnFailureResponse() {
        AccountCreationRequestDto invalidRequest = new AccountCreationRequestDto(
            1234567890L,
            "INVALID",
            new BigDecimal("2.50"),
            1000
        );

        AccountCreationResponseDto response = accountCreationService.createAccount(invalidRequest);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("2");
    }

    @Test
    void createAccount_WithExistingAccounts_ShouldGenerateNextAccountNumber() {
        Account existingAccount = new Account();
        existingAccount.setAccountNumber("00000005");
        
        when(sortCodeService.getSortCode()).thenReturn("123456");
        when(accountRepository.findByCustomerNumber(1234567890L)).thenReturn(new ArrayList<>());
        when(accountRepository.findTopBySortCodeOrderByAccountNumberDesc("123456"))
            .thenReturn(Optional.of(existingAccount));
        when(accountRepository.save(any(Account.class))).thenAnswer(invocation -> invocation.getArgument(0));

        AccountCreationResponseDto response = accountCreationService.createAccount(validRequest);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getAccountNumber()).isEqualTo("00000006");
    }

    @Test
    void createAccount_WithValidAccountTypes_ShouldAcceptAllValidTypes() {
        String[] validTypes = {"CURRENT", "SAVINGS", "LOAN", "MORTGAGE", "ISA"};
        
        when(sortCodeService.getSortCode()).thenReturn("123456");
        when(accountRepository.findByCustomerNumber(1234567890L)).thenReturn(new ArrayList<>());
        when(accountRepository.findTopBySortCodeOrderByAccountNumberDesc(anyString())).thenReturn(Optional.empty());
        when(accountRepository.save(any(Account.class))).thenAnswer(invocation -> invocation.getArgument(0));

        for (String accountType : validTypes) {
            AccountCreationRequestDto request = new AccountCreationRequestDto(
                1234567890L,
                accountType,
                new BigDecimal("2.50"),
                1000
            );

            AccountCreationResponseDto response = accountCreationService.createAccount(request);

            assertThat(response.isSuccess()).isTrue();
            assertThat(response.getAccountType()).isEqualTo(accountType);
        }
    }
}
