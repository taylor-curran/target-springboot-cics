package com.cbsa.migration.service;

import com.cbsa.migration.dto.AccountInquiryResponseDto;
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
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class AccountInquiryServiceTest {

    @Mock
    private AccountRepository accountRepository;

    @InjectMocks
    private AccountInquiryService accountInquiryService;

    private Account testAccount;

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
    }

    @Test
    void inquireAccount_WithValidAccountNumber_ShouldReturnAccountDetails() {
        when(accountRepository.findById("123456", "00000001")).thenReturn(Optional.of(testAccount));

        AccountInquiryResponseDto response = accountInquiryService.inquireAccount("123456", "00000001");

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getEyeCatcher()).isEqualTo("ACCT");
        assertThat(response.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(response.getSortCode()).isEqualTo("123456");
        assertThat(response.getAccountNumber()).isEqualTo("00000001");
        assertThat(response.getAccountType()).isEqualTo("CURRENT");
        assertThat(response.getInterestRate()).isEqualTo(new BigDecimal("2.50"));
        assertThat(response.getOverdraftLimit()).isEqualTo(1000);
        assertThat(response.getAvailableBalance()).isEqualTo(new BigDecimal("500.00"));
        assertThat(response.getActualBalance()).isEqualTo(new BigDecimal("500.00"));
    }

    @Test
    void inquireAccount_WithNonExistentAccount_ShouldReturnFailureResponse() {
        when(accountRepository.findById("123456", "88888888")).thenReturn(Optional.empty());

        AccountInquiryResponseDto response = accountInquiryService.inquireAccount("123456", "88888888");

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getAccountNumber()).isNull();
    }

    @Test
    void inquireAccount_WithSpecialAccountNumber99999999_ShouldReturnLastAccount() {
        when(accountRepository.findTopBySortCodeOrderByAccountNumberDesc("123456"))
            .thenReturn(Optional.of(testAccount));

        AccountInquiryResponseDto response = accountInquiryService.inquireAccount("123456", "99999999");

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getAccountNumber()).isEqualTo("00000001");
        assertThat(response.getCustomerNumber()).isEqualTo(1234567890L);
    }

    @Test
    void inquireAccount_WithSpecialAccountNumberButNoAccounts_ShouldReturnFailureResponse() {
        when(accountRepository.findTopBySortCodeOrderByAccountNumberDesc("123456"))
            .thenReturn(Optional.empty());

        AccountInquiryResponseDto response = accountInquiryService.inquireAccount("123456", "99999999");

        assertThat(response.isSuccess()).isFalse();
    }

    @Test
    void inquireAccount_WithRepositoryException_ShouldReturnFailureResponse() {
        when(accountRepository.findById("123456", "00000001"))
            .thenThrow(new RuntimeException("Database error"));

        AccountInquiryResponseDto response = accountInquiryService.inquireAccount("123456", "00000001");

        assertThat(response.isSuccess()).isFalse();
    }
}
