package com.cbsa.migration.service;

import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.DebitCreditRequest;
import com.cbsa.migration.model.DebitCreditResponse;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class DebitCreditServiceTest {

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private TransactionRepository transactionRepository;

    @InjectMocks
    private DebitCreditService debitCreditService;

    private Account testAccount;
    private DebitCreditRequest creditRequest;
    private DebitCreditRequest debitRequest;

    @BeforeEach
    void setUp() {
        testAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345678")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("2.5"))
                .openedDate(LocalDate.now())
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("500.00"))
                .actualBalance(new BigDecimal("500.00"))
                .build();

        creditRequest = DebitCreditRequest.builder()
                .sortCode("987654")
                .accountNumber("12345678")
                .amount(new BigDecimal("100.00"))
                .build();

        debitRequest = DebitCreditRequest.builder()
                .sortCode("987654")
                .accountNumber("12345678")
                .amount(new BigDecimal("-50.00"))
                .build();
    }

    @Test
    void testProcessTransaction_Credit_Success() {
        when(accountRepository.findById("987654", "12345678")).thenReturn(Optional.of(testAccount));

        DebitCreditResponse response = debitCreditService.processTransaction(creditRequest);

        assertTrue(response.isSuccess());
        assertEquals("0", response.getFailureCode());
        assertEquals(new BigDecimal("600.00"), response.getAvailableBalance());
        assertEquals(new BigDecimal("600.00"), response.getActualBalance());
    }

    @Test
    void testProcessTransaction_Debit_Success() {
        when(accountRepository.findById("987654", "12345678")).thenReturn(Optional.of(testAccount));

        DebitCreditResponse response = debitCreditService.processTransaction(debitRequest);

        assertTrue(response.isSuccess());
        assertEquals("0", response.getFailureCode());
        assertEquals(new BigDecimal("450.00"), response.getAvailableBalance());
        assertEquals(new BigDecimal("450.00"), response.getActualBalance());
    }

    @Test
    void testProcessTransaction_AccountNotFound() {
        when(accountRepository.findById("987654", "12345678")).thenReturn(Optional.empty());

        DebitCreditResponse response = debitCreditService.processTransaction(creditRequest);

        assertFalse(response.isSuccess());
        assertEquals("1", response.getFailureCode());
    }

    @Test
    void testProcessTransaction_InsufficientFunds() {
        DebitCreditRequest largeDebitRequest = DebitCreditRequest.builder()
                .sortCode("987654")
                .accountNumber("12345678")
                .amount(new BigDecimal("-600.00"))
                .facilityType(496)
                .build();

        when(accountRepository.findById("987654", "12345678")).thenReturn(Optional.of(testAccount));

        DebitCreditResponse response = debitCreditService.processTransaction(largeDebitRequest);

        assertFalse(response.isSuccess());
        assertEquals("3", response.getFailureCode());
    }

    @Test
    void testProcessTransaction_MortgageAccountPayment() {
        testAccount.setAccountType("MORTGAGE");
        
        DebitCreditRequest paymentRequest = DebitCreditRequest.builder()
                .sortCode("987654")
                .accountNumber("12345678")
                .amount(new BigDecimal("-100.00"))
                .facilityType(496)
                .build();

        when(accountRepository.findById("987654", "12345678")).thenReturn(Optional.of(testAccount));

        DebitCreditResponse response = debitCreditService.processTransaction(paymentRequest);

        assertFalse(response.isSuccess());
        assertEquals("4", response.getFailureCode());
    }
}
