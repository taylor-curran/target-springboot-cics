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
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Unit tests for CreditDebitService
 * Tests all DBCRFUN business logic paths including:
 * - Successful credit and debit
 * - Account not found (fail code 1)
 * - Insufficient funds for payment channel (fail code 3)
 * - MORTGAGE/LOAN restriction for payment channel (fail code 4)
 * - DB error handling (fail code 2)
 * - PROCTRAN audit record creation
 */
@ExtendWith(MockitoExtension.class)
class CreditDebitServiceTest {

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private TransactionRepository transactionRepository;

    private CreditDebitService creditDebitService;

    private static final String SORT_CODE = "987654";

    @BeforeEach
    void setUp() {
        SortCodeService sortCodeService = new SortCodeService();
        creditDebitService = new CreditDebitService(accountRepository, transactionRepository, sortCodeService);
    }

    private Account buildAccount(String accountNumber, String accountType, BigDecimal availableBalance, BigDecimal actualBalance) {
        return Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1L)
                .sortCode(SORT_CODE)
                .accountNumber(accountNumber)
                .accountType(accountType)
                .interestRate(BigDecimal.ZERO)
                .openedDate(java.time.LocalDate.of(2020, 1, 1))
                .overdraftLimit(0)
                .availableBalance(availableBalance)
                .actualBalance(actualBalance)
                .build();
    }

    // --- Successful credit (teller) ---

    @Test
    void shouldCreditAccountSuccessfully_teller() {
        Account account = buildAccount("12345678", "CURRENT", new BigDecimal("1000.00"), new BigDecimal("1000.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));
        when(accountRepository.save(any(Account.class))).thenReturn(account);
        when(transactionRepository.save(any(Transaction.class))).thenReturn(Transaction.builder().build());

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("+")
                .amount(new BigDecimal("250.50"))
                .channelType("TELLER")
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isEqualTo("0");
        assertThat(response.getAvailableBalance()).isEqualByComparingTo(new BigDecimal("1250.50"));
        assertThat(response.getActualBalance()).isEqualByComparingTo(new BigDecimal("1250.50"));
        assertThat(response.getSortCode()).isEqualTo(SORT_CODE);

        ArgumentCaptor<Transaction> txCaptor = ArgumentCaptor.forClass(Transaction.class);
        verify(transactionRepository).save(txCaptor.capture());
        Transaction savedTx = txCaptor.getValue();
        assertThat(savedTx.getTransactionType()).isEqualTo("CRE");
        assertThat(savedTx.getDescription()).isEqualTo("COUNTER RECVED");
        assertThat(savedTx.getAmount()).isEqualByComparingTo(new BigDecimal("250.50"));
    }

    // --- Successful debit (teller) ---

    @Test
    void shouldDebitAccountSuccessfully_teller() {
        Account account = buildAccount("12345678", "CURRENT", new BigDecimal("1000.00"), new BigDecimal("1000.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));
        when(accountRepository.save(any(Account.class))).thenReturn(account);
        when(transactionRepository.save(any(Transaction.class))).thenReturn(Transaction.builder().build());

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("-")
                .amount(new BigDecimal("200.00"))
                .channelType("TELLER")
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isEqualTo("0");
        assertThat(response.getAvailableBalance()).isEqualByComparingTo(new BigDecimal("800.00"));
        assertThat(response.getActualBalance()).isEqualByComparingTo(new BigDecimal("800.00"));

        ArgumentCaptor<Transaction> txCaptor = ArgumentCaptor.forClass(Transaction.class);
        verify(transactionRepository).save(txCaptor.capture());
        assertThat(txCaptor.getValue().getTransactionType()).isEqualTo("DEB");
        assertThat(txCaptor.getValue().getDescription()).isEqualTo("COUNTER WTHDRW");
    }

    // --- Successful debit (payment channel) ---

    @Test
    void shouldDebitAccountSuccessfully_paymentChannel() {
        Account account = buildAccount("12345678", "CURRENT", new BigDecimal("500.00"), new BigDecimal("500.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));
        when(accountRepository.save(any(Account.class))).thenReturn(account);
        when(transactionRepository.save(any(Transaction.class))).thenReturn(Transaction.builder().build());

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("-")
                .amount(new BigDecimal("100.00"))
                .channelType("PAYMENT")
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getAvailableBalance()).isEqualByComparingTo(new BigDecimal("400.00"));

        ArgumentCaptor<Transaction> txCaptor = ArgumentCaptor.forClass(Transaction.class);
        verify(transactionRepository).save(txCaptor.capture());
        assertThat(txCaptor.getValue().getTransactionType()).isEqualTo("PDR");
    }

    // --- Successful credit (payment channel) ---

    @Test
    void shouldCreditAccountSuccessfully_paymentChannel() {
        Account account = buildAccount("12345678", "CURRENT", new BigDecimal("500.00"), new BigDecimal("500.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));
        when(accountRepository.save(any(Account.class))).thenReturn(account);
        when(transactionRepository.save(any(Transaction.class))).thenReturn(Transaction.builder().build());

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("+")
                .amount(new BigDecimal("300.00"))
                .channelType("PAYMENT")
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getAvailableBalance()).isEqualByComparingTo(new BigDecimal("800.00"));

        ArgumentCaptor<Transaction> txCaptor = ArgumentCaptor.forClass(Transaction.class);
        verify(transactionRepository).save(txCaptor.capture());
        assertThat(txCaptor.getValue().getTransactionType()).isEqualTo("PCR");
    }

    // --- Account not found (fail code 1) ---

    @Test
    void shouldReturnFailCode1_whenAccountNotFound() {
        when(accountRepository.findById(SORT_CODE, "99999999")).thenReturn(Optional.empty());

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("99999999")
                .sign("+")
                .amount(new BigDecimal("100.00"))
                .channelType("TELLER")
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("1");
        assertThat(response.getSortCode()).isEqualTo(SORT_CODE);
        verify(accountRepository, never()).save(any());
        verify(transactionRepository, never()).save(any());
    }

    // --- Insufficient funds for payment channel (fail code 3) ---

    @Test
    void shouldReturnFailCode3_whenInsufficientFunds_paymentChannel() {
        Account account = buildAccount("12345678", "CURRENT", new BigDecimal("50.00"), new BigDecimal("50.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("-")
                .amount(new BigDecimal("100.00"))
                .channelType("PAYMENT")
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("3");
        verify(accountRepository, never()).save(any());
        verify(transactionRepository, never()).save(any());
    }

    // --- Teller CAN overdraw (no fail code 3) ---

    @Test
    void shouldAllowOverdraw_forTellerDebit() {
        Account account = buildAccount("12345678", "CURRENT", new BigDecimal("50.00"), new BigDecimal("50.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));
        when(accountRepository.save(any(Account.class))).thenReturn(account);
        when(transactionRepository.save(any(Transaction.class))).thenReturn(Transaction.builder().build());

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("-")
                .amount(new BigDecimal("100.00"))
                .channelType("TELLER")
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getAvailableBalance()).isEqualByComparingTo(new BigDecimal("-50.00"));
    }

    // --- MORTGAGE debit via payment channel (fail code 4) ---

    @Test
    void shouldReturnFailCode4_whenDebitMortgageViaPayment() {
        Account account = buildAccount("12345678", "MORTGAGE", new BigDecimal("5000.00"), new BigDecimal("5000.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("-")
                .amount(new BigDecimal("100.00"))
                .channelType("PAYMENT")
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("4");
        verify(accountRepository, never()).save(any());
    }

    // --- LOAN credit via payment channel (fail code 4) ---

    @Test
    void shouldReturnFailCode4_whenCreditLoanViaPayment() {
        Account account = buildAccount("12345678", "LOAN", new BigDecimal("5000.00"), new BigDecimal("5000.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("+")
                .amount(new BigDecimal("100.00"))
                .channelType("PAYMENT")
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("4");
        verify(accountRepository, never()).save(any());
    }

    // --- MORTGAGE via teller is allowed ---

    @Test
    void shouldAllowMortgageDebit_viaTeller() {
        Account account = buildAccount("12345678", "MORTGAGE", new BigDecimal("5000.00"), new BigDecimal("5000.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));
        when(accountRepository.save(any(Account.class))).thenReturn(account);
        when(transactionRepository.save(any(Transaction.class))).thenReturn(Transaction.builder().build());

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("-")
                .amount(new BigDecimal("100.00"))
                .channelType("TELLER")
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isTrue();
    }

    // --- DB error on account save (fail code 2) ---

    @Test
    void shouldReturnFailCode2_whenAccountSaveFails() {
        Account account = buildAccount("12345678", "CURRENT", new BigDecimal("1000.00"), new BigDecimal("1000.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));
        when(accountRepository.save(any(Account.class))).thenThrow(new RuntimeException("DB error"));

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("+")
                .amount(new BigDecimal("100.00"))
                .channelType("TELLER")
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("2");
        verify(transactionRepository, never()).save(any());
    }

    // --- PROCTRAN write failure rolls back ---

    @Test
    void shouldThrowException_whenProctranWriteFails() {
        Account account = buildAccount("12345678", "CURRENT", new BigDecimal("1000.00"), new BigDecimal("1000.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));
        when(accountRepository.save(any(Account.class))).thenReturn(account);
        when(transactionRepository.save(any(Transaction.class))).thenThrow(new RuntimeException("PROCTRAN write failed"));

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("+")
                .amount(new BigDecimal("100.00"))
                .channelType("TELLER")
                .build();

        assertThatThrownBy(() -> creditDebitService.debitCreditAccount(request))
                .isInstanceOf(RuntimeException.class)
                .hasMessageContaining("Failed to write audit record");
    }

    // --- Default channel type is TELLER ---

    @Test
    void shouldDefaultToTeller_whenChannelTypeNotProvided() {
        Account account = buildAccount("12345678", "CURRENT", new BigDecimal("1000.00"), new BigDecimal("1000.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));
        when(accountRepository.save(any(Account.class))).thenReturn(account);
        when(transactionRepository.save(any(Transaction.class))).thenReturn(Transaction.builder().build());

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("+")
                .amount(new BigDecimal("100.00"))
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isTrue();

        ArgumentCaptor<Transaction> txCaptor = ArgumentCaptor.forClass(Transaction.class);
        verify(transactionRepository).save(txCaptor.capture());
        assertThat(txCaptor.getValue().getTransactionType()).isEqualTo("CRE");
    }

    // --- LOAN debit via payment channel (fail code 4) ---

    @Test
    void shouldReturnFailCode4_whenDebitLoanViaPayment() {
        Account account = buildAccount("12345678", "LOAN", new BigDecimal("5000.00"), new BigDecimal("5000.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("-")
                .amount(new BigDecimal("100.00"))
                .channelType("PAYMENT")
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("4");
    }

    // --- MORTGAGE credit via payment channel (fail code 4) ---

    @Test
    void shouldReturnFailCode4_whenCreditMortgageViaPayment() {
        Account account = buildAccount("12345678", "MORTGAGE", new BigDecimal("5000.00"), new BigDecimal("5000.00"));
        when(accountRepository.findById(SORT_CODE, "12345678")).thenReturn(Optional.of(account));

        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
                .accountNumber("12345678")
                .sign("+")
                .amount(new BigDecimal("100.00"))
                .channelType("PAYMENT")
                .build();

        DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("4");
    }
}
