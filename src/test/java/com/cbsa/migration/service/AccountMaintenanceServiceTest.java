package com.cbsa.migration.service;

import com.cbsa.migration.dto.AccountInquiryResponseDto;
import com.cbsa.migration.dto.AccountUpdateRequestDto;
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
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class AccountMaintenanceServiceTest {

    @Mock
    private AccountRepository accountRepository;

    @InjectMocks
    private AccountMaintenanceService accountMaintenanceService;

    private Account testAccount;
    private AccountUpdateRequestDto validUpdateRequest;

    @BeforeEach
    void setUp() {
        testAccount = new Account();
        testAccount.setEyeCatcher("ACCT");
        testAccount.setCustomerNumber(1234567890L);
        testAccount.setSortCode("123456");
        testAccount.setAccountNumber("00000001");
        testAccount.setAccountType("CURRENT");
        testAccount.setInterestRate(new BigDecimal("2.50"));
        testAccount.setOpenedDate(LocalDate.now());
        testAccount.setOverdraftLimit(1000);
        testAccount.setLastStatementDate(LocalDate.now());
        testAccount.setNextStatementDate(LocalDate.now().plusMonths(1));
        testAccount.setAvailableBalance(new BigDecimal("500.00"));
        testAccount.setActualBalance(new BigDecimal("500.00"));

        validUpdateRequest = new AccountUpdateRequestDto(
            "SAVINGS",
            new BigDecimal("3.00"),
            1500
        );
    }

    @Test
    void updateAccount_WithValidRequest_ShouldUpdateAccountSuccessfully() {
        when(accountRepository.findById("123456", "00000001")).thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class))).thenAnswer(invocation -> invocation.getArgument(0));

        AccountInquiryResponseDto response = accountMaintenanceService.updateAccount("123456", "00000001", validUpdateRequest);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getAccountType()).isEqualTo("SAVINGS");
        assertThat(response.getInterestRate()).isEqualTo(new BigDecimal("3.00"));
        assertThat(response.getOverdraftLimit()).isEqualTo(1500);
        assertThat(response.getAvailableBalance()).isEqualTo(new BigDecimal("500.00"));
        assertThat(response.getActualBalance()).isEqualTo(new BigDecimal("500.00"));
    }

    @Test
    void updateAccount_WithNonExistentAccount_ShouldReturnFailureResponse() {
        when(accountRepository.findById("123456", "99999999")).thenReturn(Optional.empty());

        AccountInquiryResponseDto response = accountMaintenanceService.updateAccount("123456", "99999999", validUpdateRequest);

        assertThat(response.isSuccess()).isFalse();
    }

    @Test
    void updateAccount_WithEmptyAccountType_ShouldReturnFailureResponse() {
        AccountUpdateRequestDto invalidRequest = new AccountUpdateRequestDto(
            "",
            new BigDecimal("3.00"),
            1500
        );

        AccountInquiryResponseDto response = accountMaintenanceService.updateAccount("123456", "00000001", invalidRequest);

        assertThat(response.isSuccess()).isFalse();
    }

    @Test
    void updateAccount_WithSpacePrefixedAccountType_ShouldReturnFailureResponse() {
        AccountUpdateRequestDto invalidRequest = new AccountUpdateRequestDto(
            " SAVINGS",
            new BigDecimal("3.00"),
            1500
        );

        AccountInquiryResponseDto response = accountMaintenanceService.updateAccount("123456", "00000001", invalidRequest);

        assertThat(response.isSuccess()).isFalse();
    }

    @Test
    void updateAccount_WithNullAccountType_ShouldReturnFailureResponse() {
        AccountUpdateRequestDto invalidRequest = new AccountUpdateRequestDto(
            null,
            new BigDecimal("3.00"),
            1500
        );

        AccountInquiryResponseDto response = accountMaintenanceService.updateAccount("123456", "00000001", invalidRequest);

        assertThat(response.isSuccess()).isFalse();
    }

    @Test
    void updateAccount_ShouldPreserveBalances() {
        BigDecimal originalAvailableBalance = testAccount.getAvailableBalance();
        BigDecimal originalActualBalance = testAccount.getActualBalance();

        when(accountRepository.findById("123456", "00000001")).thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class))).thenAnswer(invocation -> invocation.getArgument(0));

        AccountInquiryResponseDto response = accountMaintenanceService.updateAccount("123456", "00000001", validUpdateRequest);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getAvailableBalance()).isEqualTo(originalAvailableBalance);
        assertThat(response.getActualBalance()).isEqualTo(originalActualBalance);
    }

    @Test
    void updateAccount_WithRepositoryException_ShouldReturnFailureResponse() {
        when(accountRepository.findById("123456", "00000001"))
            .thenThrow(new RuntimeException("Database error"));

        AccountInquiryResponseDto response = accountMaintenanceService.updateAccount("123456", "00000001", validUpdateRequest);

        assertThat(response.isSuccess()).isFalse();
    }
}
