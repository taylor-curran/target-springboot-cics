package com.cbsa.migration.service;

import com.cbsa.migration.dto.BalanceUpdateRequestDto;
import com.cbsa.migration.dto.BalanceUpdateResponseDto;
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
class BalanceManagementServiceTest {

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private SortCodeService sortCodeService;

    @InjectMocks
    private BalanceManagementService balanceManagementService;

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
    void updateBalance_WithCreditAmount_ShouldIncreaseBalance() {
        BalanceUpdateRequestDto request = new BalanceUpdateRequestDto(new BigDecimal("100.00"));
        
        when(accountRepository.findById("123456", "00000001")).thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class))).thenAnswer(invocation -> invocation.getArgument(0));

        BalanceUpdateResponseDto response = balanceManagementService.updateBalance("123456", "00000001", request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isEqualTo("0");
        assertThat(response.getAvailableBalance()).isEqualTo(new BigDecimal("600.00"));
        assertThat(response.getActualBalance()).isEqualTo(new BigDecimal("600.00"));
    }

    @Test
    void updateBalance_WithDebitAmount_ShouldDecreaseBalance() {
        BalanceUpdateRequestDto request = new BalanceUpdateRequestDto(new BigDecimal("-100.00"));
        
        when(accountRepository.findById("123456", "00000001")).thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class))).thenAnswer(invocation -> invocation.getArgument(0));

        BalanceUpdateResponseDto response = balanceManagementService.updateBalance("123456", "00000001", request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isEqualTo("0");
        assertThat(response.getAvailableBalance()).isEqualTo(new BigDecimal("400.00"));
        assertThat(response.getActualBalance()).isEqualTo(new BigDecimal("400.00"));
    }

    @Test
    void updateBalance_WithNonExistentAccount_ShouldReturnFailureResponse() {
        BalanceUpdateRequestDto request = new BalanceUpdateRequestDto(new BigDecimal("100.00"));
        
        when(accountRepository.findById("123456", "99999999")).thenReturn(Optional.empty());

        BalanceUpdateResponseDto response = balanceManagementService.updateBalance("123456", "99999999", request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("1");
    }

    @Test
    void updateBalance_WithInsufficientFundsForPayment_ShouldReturnFailureResponse() {
        BalanceUpdateRequestDto request = new BalanceUpdateRequestDto(new BigDecimal("-600.00"));
        request.setFacilityType(496);
        
        when(accountRepository.findById("123456", "00000001")).thenReturn(Optional.of(testAccount));

        BalanceUpdateResponseDto response = balanceManagementService.updateBalance("123456", "00000001", request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("3");
    }

    @Test
    void updateBalance_WithMortgageAccountAndPaymentFacility_ShouldReturnFailureResponse() {
        testAccount.setAccountType("MORTGAGE");
        BalanceUpdateRequestDto request = new BalanceUpdateRequestDto(new BigDecimal("100.00"));
        request.setFacilityType(496);
        
        when(accountRepository.findById("123456", "00000001")).thenReturn(Optional.of(testAccount));

        BalanceUpdateResponseDto response = balanceManagementService.updateBalance("123456", "00000001", request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("4");
    }

    @Test
    void updateBalance_WithLoanAccountAndPaymentFacility_ShouldReturnFailureResponse() {
        testAccount.setAccountType("LOAN");
        BalanceUpdateRequestDto request = new BalanceUpdateRequestDto(new BigDecimal("-100.00"));
        request.setFacilityType(496);
        
        when(accountRepository.findById("123456", "00000001")).thenReturn(Optional.of(testAccount));

        BalanceUpdateResponseDto response = balanceManagementService.updateBalance("123456", "00000001", request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("4");
    }

    @Test
    void updateBalance_WithMortgageAccountAndNonPaymentFacility_ShouldSucceed() {
        testAccount.setAccountType("MORTGAGE");
        BalanceUpdateRequestDto request = new BalanceUpdateRequestDto(new BigDecimal("100.00"));
        request.setFacilityType(123);
        
        when(accountRepository.findById("123456", "00000001")).thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class))).thenAnswer(invocation -> invocation.getArgument(0));

        BalanceUpdateResponseDto response = balanceManagementService.updateBalance("123456", "00000001", request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isEqualTo("0");
    }

    @Test
    void updateBalance_WithRepositoryException_ShouldReturnFailureResponse() {
        BalanceUpdateRequestDto request = new BalanceUpdateRequestDto(new BigDecimal("100.00"));
        
        when(accountRepository.findById("123456", "00000001"))
            .thenThrow(new RuntimeException("Database error"));

        BalanceUpdateResponseDto response = balanceManagementService.updateBalance("123456", "00000001", request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("2");
    }
}
