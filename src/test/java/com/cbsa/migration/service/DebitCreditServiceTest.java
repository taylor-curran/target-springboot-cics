package com.cbsa.migration.service;

import com.cbsa.migration.dto.DebitCreditRequestDto;
import com.cbsa.migration.dto.DebitCreditResponseDto;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class DebitCreditServiceTest {

    @Mock
    private AccountRepository accountRepository;
    
    @Mock
    private TransactionRepository transactionRepository;
    
    private DebitCreditService service;
    
    private Account testAccount;
    
    @BeforeEach
    void setUp() {
        service = new DebitCreditService(accountRepository, transactionRepository);
        
        testAccount = Account.builder()
            .eyeCatcher("ACCT")
            .sortCode("987654")
            .accountNumber("12345678")
            .customerNumber(1000000001L)
            .accountType("CURRENT")
            .interestRate(new BigDecimal("1.50"))
            .openedDate(LocalDate.now())
            .overdraftLimit(1000)
            .availableBalance(new BigDecimal("500.00"))
            .actualBalance(new BigDecimal("500.00"))
            .build();
    }
    
    @Test
    void shouldProcessDebitSuccessfully_TellerTransaction() {
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("-50.00"))
            .facilityType(100)
            .applicationId("APP001")
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class)))
            .thenReturn(testAccount);
        when(transactionRepository.save(any(Transaction.class)))
            .thenReturn(new Transaction());
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('Y');
        assertThat(response.getFailCode()).isEqualTo('0');
        assertThat(response.getAvailableBalance()).isEqualTo(new BigDecimal("450.00"));
        assertThat(response.getActualBalance()).isEqualTo(new BigDecimal("450.00"));
        
        verify(accountRepository).save(any(Account.class));
        verify(transactionRepository).save(any(Transaction.class));
    }
    
    @Test
    void shouldProcessCreditSuccessfully_TellerTransaction() {
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("100.00"))
            .facilityType(100)
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class)))
            .thenReturn(testAccount);
        when(transactionRepository.save(any(Transaction.class)))
            .thenReturn(new Transaction());
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('Y');
        assertThat(response.getFailCode()).isEqualTo('0');
        assertThat(response.getAvailableBalance()).isEqualTo(new BigDecimal("600.00"));
        assertThat(response.getActualBalance()).isEqualTo(new BigDecimal("600.00"));
    }
    
    @Test
    void shouldProcessPaymentDebitSuccessfully() {
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("-50.00"))
            .facilityType(496)
            .applicationId("APP001")
            .userId("USER001")
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class)))
            .thenReturn(testAccount);
        when(transactionRepository.save(any(Transaction.class)))
            .thenReturn(new Transaction());
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('Y');
        assertThat(response.getFailCode()).isEqualTo('0');
        
        ArgumentCaptor<Transaction> transactionCaptor = ArgumentCaptor.forClass(Transaction.class);
        verify(transactionRepository).save(transactionCaptor.capture());
        assertThat(transactionCaptor.getValue().getTransactionType()).isEqualTo("PDR");
    }
    
    @Test
    void shouldProcessPaymentCreditSuccessfully() {
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("100.00"))
            .facilityType(496)
            .applicationId("APP001")
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class)))
            .thenReturn(testAccount);
        when(transactionRepository.save(any(Transaction.class)))
            .thenReturn(new Transaction());
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('Y');
        
        ArgumentCaptor<Transaction> transactionCaptor = ArgumentCaptor.forClass(Transaction.class);
        verify(transactionRepository).save(transactionCaptor.capture());
        assertThat(transactionCaptor.getValue().getTransactionType()).isEqualTo("PCR");
    }
    
    @Test
    void shouldFailWhenAccountNotFound() {
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("99999999")
            .amount(new BigDecimal("-50.00"))
            .facilityType(100)
            .build();
        
        when(accountRepository.findById("987654", "99999999"))
            .thenReturn(Optional.empty());
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('N');
        assertThat(response.getFailCode()).isEqualTo('1');
        verify(accountRepository, never()).save(any());
        verify(transactionRepository, never()).save(any());
    }
    
    @Test
    void shouldFailWhenInsufficientFunds_PaymentDebit() {
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("-600.00"))
            .facilityType(496)
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenReturn(Optional.of(testAccount));
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('N');
        assertThat(response.getFailCode()).isEqualTo('3');
        verify(accountRepository, never()).save(any());
        verify(transactionRepository, never()).save(any());
    }
    
    @Test
    void shouldFailWhenDebitFromMortgageAccount_Payment() {
        testAccount.setAccountType("MORTGAGE");
        
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("-50.00"))
            .facilityType(496)
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenReturn(Optional.of(testAccount));
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('N');
        assertThat(response.getFailCode()).isEqualTo('4');
        verify(accountRepository, never()).save(any());
    }
    
    @Test
    void shouldFailWhenDebitFromLoanAccount_Payment() {
        testAccount.setAccountType("LOAN    ");
        
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("-50.00"))
            .facilityType(496)
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenReturn(Optional.of(testAccount));
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('N');
        assertThat(response.getFailCode()).isEqualTo('4');
    }
    
    @Test
    void shouldFailWhenCreditToMortgageAccount_Payment() {
        testAccount.setAccountType("MORTGAGE");
        
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("100.00"))
            .facilityType(496)
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenReturn(Optional.of(testAccount));
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('N');
        assertThat(response.getFailCode()).isEqualTo('4');
    }
    
    @Test
    void shouldFailWhenCreditToLoanAccount_Payment() {
        testAccount.setAccountType("LOAN    ");
        
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("100.00"))
            .facilityType(496)
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenReturn(Optional.of(testAccount));
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('N');
        assertThat(response.getFailCode()).isEqualTo('4');
    }
    
    @Test
    void shouldAllowDebitFromMortgageAccount_TellerTransaction() {
        testAccount.setAccountType("MORTGAGE");
        
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("-50.00"))
            .facilityType(100)
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class)))
            .thenReturn(testAccount);
        when(transactionRepository.save(any(Transaction.class)))
            .thenReturn(new Transaction());
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('Y');
        assertThat(response.getFailCode()).isEqualTo('0');
    }
    
    @Test
    void shouldAllowOverdraft_TellerTransaction() {
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("-600.00"))
            .facilityType(100)
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class)))
            .thenReturn(testAccount);
        when(transactionRepository.save(any(Transaction.class)))
            .thenReturn(new Transaction());
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('Y');
        assertThat(response.getAvailableBalance()).isEqualTo(new BigDecimal("-100.00"));
    }
    
    @Test
    void shouldHandleDatabaseError() {
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("-50.00"))
            .facilityType(100)
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenThrow(new RuntimeException("Database connection error"));
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('N');
        assertThat(response.getFailCode()).isEqualTo('2');
    }
    
    @Test
    void shouldCreateCorrectTransactionType_Debit() {
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("-50.00"))
            .facilityType(100)
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class)))
            .thenReturn(testAccount);
        when(transactionRepository.save(any(Transaction.class)))
            .thenReturn(new Transaction());
        
        service.processDebitCredit(request);
        
        ArgumentCaptor<Transaction> captor = ArgumentCaptor.forClass(Transaction.class);
        verify(transactionRepository).save(captor.capture());
        Transaction savedTransaction = captor.getValue();
        
        assertThat(savedTransaction.getTransactionType()).isEqualTo("DEB");
        assertThat(savedTransaction.getDescription()).isEqualTo("COUNTER WTHDRW");
        assertThat(savedTransaction.getAmount()).isEqualTo(new BigDecimal("-50.00"));
    }
    
    @Test
    void shouldCreateCorrectTransactionType_Credit() {
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("12345678")
            .amount(new BigDecimal("100.00"))
            .facilityType(100)
            .build();
        
        when(accountRepository.findById("987654", "12345678"))
            .thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class)))
            .thenReturn(testAccount);
        when(transactionRepository.save(any(Transaction.class)))
            .thenReturn(new Transaction());
        
        service.processDebitCredit(request);
        
        ArgumentCaptor<Transaction> captor = ArgumentCaptor.forClass(Transaction.class);
        verify(transactionRepository).save(captor.capture());
        Transaction savedTransaction = captor.getValue();
        
        assertThat(savedTransaction.getTransactionType()).isEqualTo("CRE");
        assertThat(savedTransaction.getDescription()).isEqualTo("COUNTER RECVED");
    }
}
