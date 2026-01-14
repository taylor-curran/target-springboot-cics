package com.cbsa.migration.service;

import com.cbsa.migration.dto.FundsTransferRequest;
import com.cbsa.migration.dto.FundsTransferResult;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * Comprehensive unit tests for FundsTransferService.
 * 
 * These tests validate the migration of COBOL XFRFUN program to Java,
 * ensuring all business rules and error handling are correctly implemented.
 * 
 * Test scenarios cover:
 * 1. Successful transfer between two valid accounts
 * 2. Rejection when source account has insufficient funds
 * 3. Rejection with invalid amount (zero or negative)
 * 4. Rejection when source account doesn't exist
 * 5. Rejection when target account doesn't exist
 * 6. Rejection when transferring to the same account
 * 7. Verification that failed transfers don't modify any account balances
 * 8. Verification that successful transfers are recorded in PROCTRAN
 */
@ExtendWith(MockitoExtension.class)
class FundsTransferServiceTest {

    // ============================================================
    // Test constants matching COBOL data patterns
    // ============================================================
    private static final String FROM_SORT_CODE = "987654";
    private static final String FROM_ACCOUNT_NUMBER = "12345678";
    private static final String TO_SORT_CODE = "987654";
    private static final String TO_ACCOUNT_NUMBER = "87654321";
    private static final BigDecimal INITIAL_FROM_BALANCE = new BigDecimal("1000.00");
    private static final BigDecimal INITIAL_TO_BALANCE = new BigDecimal("500.00");
    private static final BigDecimal TRANSFER_AMOUNT = new BigDecimal("100.00");

    // ============================================================
    // Mock dependencies
    // ============================================================
    @Mock
    private AccountRepository accountRepository;

    @Mock
    private TransactionRepository transactionRepository;

    // ============================================================
    // Service under test
    // ============================================================
    @InjectMocks
    private FundsTransferService fundsTransferService;

    // ============================================================
    // Argument captors for verifying saved entities
    // ============================================================
    @Captor
    private ArgumentCaptor<Account> accountCaptor;

    @Captor
    private ArgumentCaptor<Transaction> transactionCaptor;

    // ============================================================
    // Test data objects
    // ============================================================
    private Account fromAccount;
    private Account toAccount;
    private FundsTransferRequest validRequest;

    /**
     * Set up test data before each test.
     * Creates valid FROM and TO accounts with known balances.
     */
    @BeforeEach
    void setUp() {
        // Create FROM account with sufficient balance
        fromAccount = Account.builder()
                .eyeCatcher(Account.VALID_EYECATCHER)
                .customerNumber(1000000001L)
                .sortCode(FROM_SORT_CODE)
                .accountNumber(FROM_ACCOUNT_NUMBER)
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("2.50"))
                .openedDate(LocalDate.of(2020, 1, 1))
                .overdraftLimit(0)
                .availableBalance(INITIAL_FROM_BALANCE)
                .actualBalance(INITIAL_FROM_BALANCE)
                .build();

        // Create TO account
        toAccount = Account.builder()
                .eyeCatcher(Account.VALID_EYECATCHER)
                .customerNumber(1000000002L)
                .sortCode(TO_SORT_CODE)
                .accountNumber(TO_ACCOUNT_NUMBER)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("1.00"))
                .openedDate(LocalDate.of(2021, 6, 15))
                .overdraftLimit(500)
                .availableBalance(INITIAL_TO_BALANCE)
                .actualBalance(INITIAL_TO_BALANCE)
                .build();

        // Create valid transfer request
        validRequest = FundsTransferRequest.builder()
                .fromSortCode(FROM_SORT_CODE)
                .fromAccountNumber(FROM_ACCOUNT_NUMBER)
                .toSortCode(TO_SORT_CODE)
                .toAccountNumber(TO_ACCOUNT_NUMBER)
                .amount(TRANSFER_AMOUNT)
                .build();
    }

    // ============================================================
    // Nested test class for successful transfer scenarios
    // ============================================================
    @Nested
    @DisplayName("Successful Transfer Tests")
    class SuccessfulTransferTests {

        @Test
        @DisplayName("Should successfully transfer funds between two valid accounts")
        void shouldTransferFundsBetweenValidAccounts() {
            // Given: Both accounts exist and FROM account has sufficient funds
            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(fromAccount));
            when(accountRepository.findById(TO_SORT_CODE, TO_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(toAccount));
            when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
            when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

            // When: Transfer is executed
            FundsTransferResult result = fundsTransferService.transfer(validRequest);

            // Then: Transfer should succeed
            assertThat(result.isSuccess()).isTrue();
            assertThat(result.getFailCode()).isNull();

            // Verify FROM account balance was debited
            BigDecimal expectedFromBalance = INITIAL_FROM_BALANCE.subtract(TRANSFER_AMOUNT);
            assertThat(result.getFromAvailableBalance()).isEqualByComparingTo(expectedFromBalance);
            assertThat(result.getFromActualBalance()).isEqualByComparingTo(expectedFromBalance);

            // Verify TO account balance was credited
            BigDecimal expectedToBalance = INITIAL_TO_BALANCE.add(TRANSFER_AMOUNT);
            assertThat(result.getToAvailableBalance()).isEqualByComparingTo(expectedToBalance);
            assertThat(result.getToActualBalance()).isEqualByComparingTo(expectedToBalance);
        }

        @Test
        @DisplayName("Should update both available and actual balances for both accounts")
        void shouldUpdateBothBalanceTypes() {
            // Given: Both accounts exist
            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(fromAccount));
            when(accountRepository.findById(TO_SORT_CODE, TO_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(toAccount));
            when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
            when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

            // When: Transfer is executed
            fundsTransferService.transfer(validRequest);

            // Then: Both accounts should be saved with updated balances
            verify(accountRepository, times(2)).save(accountCaptor.capture());
            
            // Verify the saved accounts have correct balances
            var savedAccounts = accountCaptor.getAllValues();
            
            // Find FROM account (debited)
            Account savedFromAccount = savedAccounts.stream()
                    .filter(a -> a.getAccountNumber().equals(FROM_ACCOUNT_NUMBER))
                    .findFirst()
                    .orElseThrow();
            assertThat(savedFromAccount.getAvailableBalance())
                    .isEqualByComparingTo(INITIAL_FROM_BALANCE.subtract(TRANSFER_AMOUNT));
            assertThat(savedFromAccount.getActualBalance())
                    .isEqualByComparingTo(INITIAL_FROM_BALANCE.subtract(TRANSFER_AMOUNT));

            // Find TO account (credited)
            Account savedToAccount = savedAccounts.stream()
                    .filter(a -> a.getAccountNumber().equals(TO_ACCOUNT_NUMBER))
                    .findFirst()
                    .orElseThrow();
            assertThat(savedToAccount.getAvailableBalance())
                    .isEqualByComparingTo(INITIAL_TO_BALANCE.add(TRANSFER_AMOUNT));
            assertThat(savedToAccount.getActualBalance())
                    .isEqualByComparingTo(INITIAL_TO_BALANCE.add(TRANSFER_AMOUNT));
        }

        @Test
        @DisplayName("Should record successful transfer in PROCTRAN table")
        void shouldRecordTransactionInProctran() {
            // Given: Both accounts exist
            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(fromAccount));
            when(accountRepository.findById(TO_SORT_CODE, TO_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(toAccount));
            when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
            when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

            // When: Transfer is executed
            fundsTransferService.transfer(validRequest);

            // Then: Transaction should be recorded
            verify(transactionRepository).save(transactionCaptor.capture());
            Transaction savedTransaction = transactionCaptor.getValue();

            // Verify transaction record fields match COBOL PROCTRAN structure
            assertThat(savedTransaction.getEyeCatcher()).isEqualTo(Transaction.VALID_EYECATCHER);
            assertThat(savedTransaction.getSortCode()).isEqualTo(FROM_SORT_CODE);
            assertThat(savedTransaction.getAccountNumber()).isEqualTo(FROM_ACCOUNT_NUMBER);
            assertThat(savedTransaction.getTransactionType()).isEqualTo(Transaction.TYPE_TRANSFER);
            assertThat(savedTransaction.getAmount()).isEqualByComparingTo(TRANSFER_AMOUNT);
            assertThat(savedTransaction.getTargetSortCode()).isEqualTo(TO_SORT_CODE);
            assertThat(savedTransaction.getTargetAccountNumber()).isEqualTo(TO_ACCOUNT_NUMBER);
            assertThat(savedTransaction.isLogicallyDeleted()).isFalse();
            assertThat(savedTransaction.getTransactionDate()).isNotNull();
            assertThat(savedTransaction.getTransactionTime()).isNotNull();
            assertThat(savedTransaction.getReferenceNumber()).isPositive();
        }

        @Test
        @DisplayName("Should handle transfer of entire available balance")
        void shouldHandleTransferOfEntireBalance() {
            // Given: Transfer amount equals entire available balance
            FundsTransferRequest fullBalanceRequest = FundsTransferRequest.builder()
                    .fromSortCode(FROM_SORT_CODE)
                    .fromAccountNumber(FROM_ACCOUNT_NUMBER)
                    .toSortCode(TO_SORT_CODE)
                    .toAccountNumber(TO_ACCOUNT_NUMBER)
                    .amount(INITIAL_FROM_BALANCE)
                    .build();

            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(fromAccount));
            when(accountRepository.findById(TO_SORT_CODE, TO_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(toAccount));
            when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
            when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

            // When: Transfer is executed
            FundsTransferResult result = fundsTransferService.transfer(fullBalanceRequest);

            // Then: Transfer should succeed with zero remaining balance
            assertThat(result.isSuccess()).isTrue();
            assertThat(result.getFromAvailableBalance()).isEqualByComparingTo(BigDecimal.ZERO);
            assertThat(result.getFromActualBalance()).isEqualByComparingTo(BigDecimal.ZERO);
        }
    }

    // ============================================================
    // Nested test class for insufficient funds scenarios
    // ============================================================
    @Nested
    @DisplayName("Insufficient Funds Tests")
    class InsufficientFundsTests {

        @Test
        @DisplayName("Should reject transfer when source account has insufficient funds")
        void shouldRejectTransferWithInsufficientFunds() {
            // Given: Transfer amount exceeds available balance
            FundsTransferRequest largeAmountRequest = FundsTransferRequest.builder()
                    .fromSortCode(FROM_SORT_CODE)
                    .fromAccountNumber(FROM_ACCOUNT_NUMBER)
                    .toSortCode(TO_SORT_CODE)
                    .toAccountNumber(TO_ACCOUNT_NUMBER)
                    .amount(new BigDecimal("2000.00")) // More than available
                    .build();

            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(fromAccount));
            when(accountRepository.findById(TO_SORT_CODE, TO_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(toAccount));

            // When: Transfer is executed
            FundsTransferResult result = fundsTransferService.transfer(largeAmountRequest);

            // Then: Transfer should fail with insufficient funds code
            assertThat(result.isSuccess()).isFalse();
            assertThat(result.getFailCode())
                    .isEqualTo(FundsTransferResult.FAIL_CODE_INSUFFICIENT_FUNDS);
            assertThat(result.getErrorMessage()).contains("Insufficient funds");
        }

        @Test
        @DisplayName("Should not modify any account balances when insufficient funds")
        void shouldNotModifyBalancesWhenInsufficientFunds() {
            // Given: Transfer amount exceeds available balance
            FundsTransferRequest largeAmountRequest = FundsTransferRequest.builder()
                    .fromSortCode(FROM_SORT_CODE)
                    .fromAccountNumber(FROM_ACCOUNT_NUMBER)
                    .toSortCode(TO_SORT_CODE)
                    .toAccountNumber(TO_ACCOUNT_NUMBER)
                    .amount(new BigDecimal("2000.00"))
                    .build();

            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(fromAccount));
            when(accountRepository.findById(TO_SORT_CODE, TO_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(toAccount));

            // When: Transfer is executed
            fundsTransferService.transfer(largeAmountRequest);

            // Then: No accounts should be saved (no modifications)
            verify(accountRepository, never()).save(any(Account.class));
            verify(transactionRepository, never()).save(any(Transaction.class));
        }
    }

    // ============================================================
    // Nested test class for invalid amount scenarios
    // ============================================================
    @Nested
    @DisplayName("Invalid Amount Tests")
    class InvalidAmountTests {

        @Test
        @DisplayName("Should reject transfer with zero amount")
        void shouldRejectTransferWithZeroAmount() {
            // Given: Transfer amount is zero
            FundsTransferRequest zeroAmountRequest = FundsTransferRequest.builder()
                    .fromSortCode(FROM_SORT_CODE)
                    .fromAccountNumber(FROM_ACCOUNT_NUMBER)
                    .toSortCode(TO_SORT_CODE)
                    .toAccountNumber(TO_ACCOUNT_NUMBER)
                    .amount(BigDecimal.ZERO)
                    .build();

            // When: Transfer is executed
            FundsTransferResult result = fundsTransferService.transfer(zeroAmountRequest);

            // Then: Transfer should fail with invalid amount code (COBOL fail code '4')
            assertThat(result.isSuccess()).isFalse();
            assertThat(result.getFailCode())
                    .isEqualTo(FundsTransferResult.FAIL_CODE_INVALID_AMOUNT);
            assertThat(result.getErrorMessage()).contains("greater than zero");
        }

        @Test
        @DisplayName("Should reject transfer with negative amount")
        void shouldRejectTransferWithNegativeAmount() {
            // Given: Transfer amount is negative
            FundsTransferRequest negativeAmountRequest = FundsTransferRequest.builder()
                    .fromSortCode(FROM_SORT_CODE)
                    .fromAccountNumber(FROM_ACCOUNT_NUMBER)
                    .toSortCode(TO_SORT_CODE)
                    .toAccountNumber(TO_ACCOUNT_NUMBER)
                    .amount(new BigDecimal("-100.00"))
                    .build();

            // When: Transfer is executed
            FundsTransferResult result = fundsTransferService.transfer(negativeAmountRequest);

            // Then: Transfer should fail with invalid amount code
            assertThat(result.isSuccess()).isFalse();
            assertThat(result.getFailCode())
                    .isEqualTo(FundsTransferResult.FAIL_CODE_INVALID_AMOUNT);
        }

        @Test
        @DisplayName("Should reject transfer with null amount")
        void shouldRejectTransferWithNullAmount() {
            // Given: Transfer amount is null
            FundsTransferRequest nullAmountRequest = FundsTransferRequest.builder()
                    .fromSortCode(FROM_SORT_CODE)
                    .fromAccountNumber(FROM_ACCOUNT_NUMBER)
                    .toSortCode(TO_SORT_CODE)
                    .toAccountNumber(TO_ACCOUNT_NUMBER)
                    .amount(null)
                    .build();

            // When: Transfer is executed
            FundsTransferResult result = fundsTransferService.transfer(nullAmountRequest);

            // Then: Transfer should fail with invalid amount code
            assertThat(result.isSuccess()).isFalse();
            assertThat(result.getFailCode())
                    .isEqualTo(FundsTransferResult.FAIL_CODE_INVALID_AMOUNT);
        }

        @Test
        @DisplayName("Should not call repository when amount is invalid")
        void shouldNotCallRepositoryWhenAmountInvalid() {
            // Given: Transfer amount is zero
            FundsTransferRequest zeroAmountRequest = FundsTransferRequest.builder()
                    .fromSortCode(FROM_SORT_CODE)
                    .fromAccountNumber(FROM_ACCOUNT_NUMBER)
                    .toSortCode(TO_SORT_CODE)
                    .toAccountNumber(TO_ACCOUNT_NUMBER)
                    .amount(BigDecimal.ZERO)
                    .build();

            // When: Transfer is executed
            fundsTransferService.transfer(zeroAmountRequest);

            // Then: No repository methods should be called
            verifyNoInteractions(accountRepository);
            verifyNoInteractions(transactionRepository);
        }
    }

    // ============================================================
    // Nested test class for account not found scenarios
    // ============================================================
    @Nested
    @DisplayName("Account Not Found Tests")
    class AccountNotFoundTests {

        @Test
        @DisplayName("Should reject transfer when source account doesn't exist")
        void shouldRejectTransferWhenSourceAccountNotFound() {
            // Given: FROM account doesn't exist
            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.empty());

            // When: Transfer is executed
            FundsTransferResult result = fundsTransferService.transfer(validRequest);

            // Then: Transfer should fail with FROM account not found code (COBOL fail code '1')
            assertThat(result.isSuccess()).isFalse();
            assertThat(result.getFailCode())
                    .isEqualTo(FundsTransferResult.FAIL_CODE_FROM_ACCOUNT_NOT_FOUND);
            assertThat(result.getErrorMessage()).contains("FROM account not found");
        }

        @Test
        @DisplayName("Should reject transfer when target account doesn't exist")
        void shouldRejectTransferWhenTargetAccountNotFound() {
            // Given: FROM account exists but TO account doesn't
            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(fromAccount));
            when(accountRepository.findById(TO_SORT_CODE, TO_ACCOUNT_NUMBER))
                    .thenReturn(Optional.empty());

            // When: Transfer is executed
            FundsTransferResult result = fundsTransferService.transfer(validRequest);

            // Then: Transfer should fail with TO account not found code (COBOL fail code '2')
            assertThat(result.isSuccess()).isFalse();
            assertThat(result.getFailCode())
                    .isEqualTo(FundsTransferResult.FAIL_CODE_TO_ACCOUNT_NOT_FOUND);
            assertThat(result.getErrorMessage()).contains("TO account not found");
        }

        @Test
        @DisplayName("Should not modify any accounts when source account not found")
        void shouldNotModifyAccountsWhenSourceNotFound() {
            // Given: FROM account doesn't exist
            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.empty());

            // When: Transfer is executed
            fundsTransferService.transfer(validRequest);

            // Then: No accounts should be saved
            verify(accountRepository, never()).save(any(Account.class));
            verify(transactionRepository, never()).save(any(Transaction.class));
        }

        @Test
        @DisplayName("Should not modify any accounts when target account not found")
        void shouldNotModifyAccountsWhenTargetNotFound() {
            // Given: FROM account exists but TO account doesn't
            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(fromAccount));
            when(accountRepository.findById(TO_SORT_CODE, TO_ACCOUNT_NUMBER))
                    .thenReturn(Optional.empty());

            // When: Transfer is executed
            fundsTransferService.transfer(validRequest);

            // Then: No accounts should be saved
            verify(accountRepository, never()).save(any(Account.class));
            verify(transactionRepository, never()).save(any(Transaction.class));
        }
    }

    // ============================================================
    // Nested test class for same account transfer scenarios
    // ============================================================
    @Nested
    @DisplayName("Same Account Transfer Tests")
    class SameAccountTransferTests {

        @Test
        @DisplayName("Should reject transfer to the same account")
        void shouldRejectTransferToSameAccount() {
            // Given: FROM and TO accounts are the same
            FundsTransferRequest sameAccountRequest = FundsTransferRequest.builder()
                    .fromSortCode(FROM_SORT_CODE)
                    .fromAccountNumber(FROM_ACCOUNT_NUMBER)
                    .toSortCode(FROM_SORT_CODE)
                    .toAccountNumber(FROM_ACCOUNT_NUMBER)
                    .amount(TRANSFER_AMOUNT)
                    .build();

            // When: Transfer is executed
            FundsTransferResult result = fundsTransferService.transfer(sameAccountRequest);

            // Then: Transfer should fail with same account code (COBOL ABEND 'SAME')
            assertThat(result.isSuccess()).isFalse();
            assertThat(result.getFailCode())
                    .isEqualTo(FundsTransferResult.FAIL_CODE_SAME_ACCOUNT);
            assertThat(result.getErrorMessage()).contains("same account");
        }

        @Test
        @DisplayName("Should not call repository when transferring to same account")
        void shouldNotCallRepositoryWhenSameAccount() {
            // Given: FROM and TO accounts are the same
            FundsTransferRequest sameAccountRequest = FundsTransferRequest.builder()
                    .fromSortCode(FROM_SORT_CODE)
                    .fromAccountNumber(FROM_ACCOUNT_NUMBER)
                    .toSortCode(FROM_SORT_CODE)
                    .toAccountNumber(FROM_ACCOUNT_NUMBER)
                    .amount(TRANSFER_AMOUNT)
                    .build();

            // When: Transfer is executed
            fundsTransferService.transfer(sameAccountRequest);

            // Then: No repository methods should be called
            verifyNoInteractions(accountRepository);
            verifyNoInteractions(transactionRepository);
        }

        @Test
        @DisplayName("Should allow transfer between accounts with same sort code but different account numbers")
        void shouldAllowTransferBetweenDifferentAccountsSameSortCode() {
            // Given: Same sort code but different account numbers
            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(fromAccount));
            when(accountRepository.findById(TO_SORT_CODE, TO_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(toAccount));
            when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
            when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

            // When: Transfer is executed
            FundsTransferResult result = fundsTransferService.transfer(validRequest);

            // Then: Transfer should succeed
            assertThat(result.isSuccess()).isTrue();
        }
    }

    // ============================================================
    // Nested test class for transaction description tests
    // ============================================================
    @Nested
    @DisplayName("Transaction Description Tests")
    class TransactionDescriptionTests {

        @Test
        @DisplayName("Should create correct transfer description format")
        void shouldCreateCorrectTransferDescription() {
            // Given: Both accounts exist
            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(fromAccount));
            when(accountRepository.findById(TO_SORT_CODE, TO_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(toAccount));
            when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
            when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

            // When: Transfer is executed
            fundsTransferService.transfer(validRequest);

            // Then: Transaction description should match COBOL format
            verify(transactionRepository).save(transactionCaptor.capture());
            Transaction savedTransaction = transactionCaptor.getValue();
            
            String expectedDescription = String.format("TFR TO %s/%s", TO_SORT_CODE, TO_ACCOUNT_NUMBER);
            assertThat(savedTransaction.getDescription()).isEqualTo(expectedDescription);
        }
    }

    // ============================================================
    // Nested test class for decimal precision tests
    // ============================================================
    @Nested
    @DisplayName("Decimal Precision Tests")
    class DecimalPrecisionTests {

        @Test
        @DisplayName("Should handle transfer with decimal amounts correctly")
        void shouldHandleDecimalAmountsCorrectly() {
            // Given: Transfer amount with decimal places
            BigDecimal decimalAmount = new BigDecimal("123.45");
            FundsTransferRequest decimalRequest = FundsTransferRequest.builder()
                    .fromSortCode(FROM_SORT_CODE)
                    .fromAccountNumber(FROM_ACCOUNT_NUMBER)
                    .toSortCode(TO_SORT_CODE)
                    .toAccountNumber(TO_ACCOUNT_NUMBER)
                    .amount(decimalAmount)
                    .build();

            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(fromAccount));
            when(accountRepository.findById(TO_SORT_CODE, TO_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(toAccount));
            when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
            when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

            // When: Transfer is executed
            FundsTransferResult result = fundsTransferService.transfer(decimalRequest);

            // Then: Balances should be calculated correctly with decimal precision
            assertThat(result.isSuccess()).isTrue();
            assertThat(result.getFromAvailableBalance())
                    .isEqualByComparingTo(INITIAL_FROM_BALANCE.subtract(decimalAmount));
            assertThat(result.getToAvailableBalance())
                    .isEqualByComparingTo(INITIAL_TO_BALANCE.add(decimalAmount));
        }

        @Test
        @DisplayName("Should handle very small transfer amounts")
        void shouldHandleVerySmallAmounts() {
            // Given: Very small transfer amount (1 cent)
            BigDecimal smallAmount = new BigDecimal("0.01");
            FundsTransferRequest smallRequest = FundsTransferRequest.builder()
                    .fromSortCode(FROM_SORT_CODE)
                    .fromAccountNumber(FROM_ACCOUNT_NUMBER)
                    .toSortCode(TO_SORT_CODE)
                    .toAccountNumber(TO_ACCOUNT_NUMBER)
                    .amount(smallAmount)
                    .build();

            when(accountRepository.findById(FROM_SORT_CODE, FROM_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(fromAccount));
            when(accountRepository.findById(TO_SORT_CODE, TO_ACCOUNT_NUMBER))
                    .thenReturn(Optional.of(toAccount));
            when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
            when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

            // When: Transfer is executed
            FundsTransferResult result = fundsTransferService.transfer(smallRequest);

            // Then: Transfer should succeed
            assertThat(result.isSuccess()).isTrue();
            assertThat(result.getFromAvailableBalance())
                    .isEqualByComparingTo(new BigDecimal("999.99"));
        }
    }
}
