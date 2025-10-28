package com.cbsa.migration.dto.mapper;

import com.cbsa.migration.dto.*;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for DtoMapper
 * Tests mapping between domain models and DTOs
 */
class DtoMapperTest {

    private DtoMapper dtoMapper;

    @BeforeEach
    void setUp() {
        dtoMapper = new DtoMapper();
    }

    @Test
    @DisplayName("Should map Account to AccountResponseDto")
    void testToAccountResponseDto() {
        // Given
        Account account = Account.builder()
                .eyeCatcher("ACCT")
                .sortCode("987654")
                .accountNumber("12345678")
                .customerNumber(1234567890L)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .openedDate(LocalDate.of(2023, 1, 15))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("1500.00"))
                .actualBalance(new BigDecimal("1750.00"))
                .build();

        // When
        AccountResponseDto result = dtoMapper.toAccountResponseDto(account);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getSortCode()).isEqualTo("987654");
        assertThat(result.getAccountNumber()).isEqualTo("12345678");
        assertThat(result.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(result.getAccountType()).isEqualTo("CURRENT");
        assertThat(result.getInterestRate()).isEqualByComparingTo(new BigDecimal("0.10"));
        assertThat(result.getOpenedDate()).isEqualTo(LocalDate.of(2023, 1, 15));
        assertThat(result.getOverdraftLimit()).isEqualTo(1000);
        assertThat(result.getAvailableBalance()).isEqualByComparingTo(new BigDecimal("1500.00"));
        assertThat(result.getActualBalance()).isEqualByComparingTo(new BigDecimal("1750.00"));
        assertThat(result.getStatus()).isEqualTo("OVERDRAWN");
    }

    @Test
    @DisplayName("Should map AccountRequestDto to Account")
    void testToAccount() {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("2.50"))
                .overdraftLimit(500)
                .initialDeposit(new BigDecimal("1000.00"))
                .build();

        // When
        Account result = dtoMapper.toAccount(request);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getSortCode()).isEqualTo("987654");
        assertThat(result.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(result.getAccountType()).isEqualTo("SAVINGS");
        assertThat(result.getInterestRate()).isEqualByComparingTo(new BigDecimal("2.50"));
        assertThat(result.getOverdraftLimit()).isEqualTo(500);
    }

    @Test
    @DisplayName("Should map Customer to CustomerResponseDto")
    void testToCustomerResponseDto() {
        // Given
        Customer customer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .creditScore(750)
                .build();

        // When
        CustomerResponseDto result = dtoMapper.toCustomerResponseDto(customer);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getSortCode()).isEqualTo("987654");
        assertThat(result.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(result.getName()).isEqualTo("John Doe");
        assertThat(result.getAddress()).isEqualTo("123 Main St");
        assertThat(result.getDateOfBirth()).isEqualTo(LocalDate.of(1990, 5, 15));
        assertThat(result.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(result.getStatus()).isEqualTo("GOOD_STANDING");
    }

    @Test
    @DisplayName("Should map CustomerRequestDto to Customer")
    void testToCustomer() {
        // Given
        CustomerRequestDto request = CustomerRequestDto.builder()
                .sortCode("987654")
                .name("Jane Smith")
                .address("456 Oak Ave")
                .dateOfBirth(LocalDate.of(1985, 8, 20))
                .creditScore(700)
                .build();

        // When
        Customer result = dtoMapper.toCustomer(request);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getSortCode()).isEqualTo("987654");
        assertThat(result.getName()).isEqualTo("Jane Smith");
        assertThat(result.getAddress()).isEqualTo("456 Oak Ave");
        assertThat(result.getDateOfBirth()).isEqualTo(LocalDate.of(1985, 8, 20));
    }

    @Test
    @DisplayName("Should map Transaction to TransactionResponseDto")
    void testToTransactionResponseDto() {
        // Given
        Transaction transaction = Transaction.builder()
                .eyeCatcher("TRAN")
                .logicallyDeleted(false)
                .sortCode("987654")
                .accountNumber("12345678")
                .transactionDate(LocalDate.of(2023, 6, 15))
                .transactionTime(LocalTime.of(14, 30, 0))
                .referenceNumber(123456789012L)
                .transactionType("DEPOSIT")
                .description("Salary deposit")
                .targetSortCode("123456")
                .targetAccountNumber("87654321")
                .amount(new BigDecimal("2500.00"))
                .build();

        // When
        TransactionResponseDto result = dtoMapper.toTransactionResponseDto(transaction);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getSortCode()).isEqualTo("987654");
        assertThat(result.getAccountNumber()).isEqualTo("12345678");
        assertThat(result.getTransactionDate()).isEqualTo(LocalDate.of(2023, 6, 15));
        assertThat(result.getTransactionTime()).isEqualTo(LocalTime.of(14, 30, 0));
        assertThat(result.getReferenceNumber()).isEqualTo(123456789012L);
        assertThat(result.getTransactionType()).isEqualTo("DEPOSIT");
        assertThat(result.getDescription()).isEqualTo("Salary deposit");
        assertThat(result.getTargetSortCode()).isEqualTo("123456");
        assertThat(result.getTargetAccountNumber()).isEqualTo("87654321");
        assertThat(result.getAmount()).isEqualByComparingTo(new BigDecimal("2500.00"));
        assertThat(result.getStatus()).isEqualTo("COMPLETED");
    }

    @Test
    @DisplayName("Should map TransactionRequestDto to Transaction")
    void testToTransaction() {
        // Given
        TransactionRequestDto request = TransactionRequestDto.builder()
                .sortCode("987654")
                .accountNumber("12345678")
                .transactionType("WITHDRAWAL")
                .description("ATM withdrawal")
                .targetSortCode("123456")
                .targetAccountNumber("87654321")
                .amount(new BigDecimal("100.00"))
                .build();

        // When
        Transaction result = dtoMapper.toTransaction(request);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getSortCode()).isEqualTo("987654");
        assertThat(result.getAccountNumber()).isEqualTo("12345678");
        assertThat(result.getTransactionType()).isEqualTo("WITHDRAWAL");
        assertThat(result.getDescription()).isEqualTo("ATM withdrawal");
        assertThat(result.getTargetSortCode()).isEqualTo("123456");
        assertThat(result.getTargetAccountNumber()).isEqualTo("87654321");
        assertThat(result.getAmount()).isEqualByComparingTo(new BigDecimal("100.00"));
    }

    @Test
    @DisplayName("Should derive account status correctly")
    void testAccountStatusDerivation() {
        Account overdraftAccount = Account.builder()
                .eyeCatcher("ACCT")
                .sortCode("987654")
                .accountNumber("12345678")
                .customerNumber(1234567890L)
                .accountType("CURRENT")
                .availableBalance(new BigDecimal("-100.00"))
                .actualBalance(new BigDecimal("-100.00"))
                .build();

        // When
        AccountResponseDto result = dtoMapper.toAccountResponseDto(overdraftAccount);

        // Then
        assertThat(result.getStatus()).isEqualTo("OVERDRAFT");
    }

    @Test
    @DisplayName("Should derive customer status correctly")
    void testCustomerStatusDerivation() {
        Customer lowCreditCustomer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .creditScore(300)
                .build();

        // When
        CustomerResponseDto result = dtoMapper.toCustomerResponseDto(lowCreditCustomer);

        // Then
        assertThat(result.getStatus()).isEqualTo("REVIEW_REQUIRED");
    }

    @Test
    @DisplayName("Should derive transaction status correctly")
    void testTransactionStatusDerivation() {
        Transaction deletedTransaction = Transaction.builder()
                .eyeCatcher("TRAN")
                .logicallyDeleted(true)
                .sortCode("987654")
                .accountNumber("12345678")
                .transactionType("DEPOSIT")
                .amount(new BigDecimal("100.00"))
                .build();

        // When
        TransactionResponseDto result = dtoMapper.toTransactionResponseDto(deletedTransaction);

        // Then
        assertThat(result.getStatus()).isEqualTo("COMPLETED");
    }

    @Test
    @DisplayName("Should map Customer to CustomerResponseDto with accounts list")
    void testToCustomerResponseDtoWithAccounts() {
        // Given
        Customer customer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .creditScore(750)
                .build();

        List<Account> accounts = Arrays.asList(
                Account.builder()
                        .eyeCatcher("ACCT")
                        .sortCode("987654")
                        .accountNumber("12345678")
                        .customerNumber(1234567890L)
                        .accountType("CURRENT")
                        .availableBalance(new BigDecimal("1500.00"))
                        .actualBalance(new BigDecimal("1500.00"))
                        .build(),
                Account.builder()
                        .eyeCatcher("ACCT")
                        .sortCode("987654")
                        .accountNumber("87654321")
                        .customerNumber(1234567890L)
                        .accountType("SAVINGS")
                        .availableBalance(new BigDecimal("5000.00"))
                        .actualBalance(new BigDecimal("5000.00"))
                        .build()
        );

        // When
        CustomerResponseDto result = dtoMapper.toCustomerResponseDto(customer, accounts);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getName()).isEqualTo("John Doe");
        assertThat(result.getAccounts()).hasSize(2);
        assertThat(result.getAccountCount()).isEqualTo(2);
        assertThat(result.getAccounts().get(0).getAccountNumber()).isEqualTo("12345678");
        assertThat(result.getAccounts().get(1).getAccountNumber()).isEqualTo("87654321");
    }

    @Test
    @DisplayName("Should map Account to AccountResponseDto with customer name")
    void testToAccountResponseDtoWithCustomerName() {
        // Given
        Account account = Account.builder()
                .eyeCatcher("ACCT")
                .sortCode("987654")
                .accountNumber("12345678")
                .customerNumber(1234567890L)
                .accountType("CURRENT")
                .availableBalance(new BigDecimal("1500.00"))
                .actualBalance(new BigDecimal("1500.00"))
                .build();

        String customerName = "Jane Smith";

        // When
        AccountResponseDto result = dtoMapper.toAccountResponseDto(account, customerName);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getAccountNumber()).isEqualTo("12345678");
        assertThat(result.getCustomerName()).isEqualTo("Jane Smith");
        assertThat(result.getStatus()).isEqualTo("ACTIVE");
    }

    @Test
    @DisplayName("Should map Account to AccountSummaryDto")
    void testToAccountSummaryDto() {
        // Given
        Account account = Account.builder()
                .eyeCatcher("ACCT")
                .sortCode("987654")
                .accountNumber("12345678")
                .customerNumber(1234567890L)
                .accountType("SAVINGS")
                .availableBalance(new BigDecimal("2500.00"))
                .actualBalance(new BigDecimal("2500.00"))
                .build();

        // When
        AccountSummaryDto result = dtoMapper.toAccountSummaryDto(account);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getAccountNumber()).isEqualTo("12345678");
        assertThat(result.getAccountType()).isEqualTo("SAVINGS");
        assertThat(result.getAvailableBalance()).isEqualByComparingTo(new BigDecimal("2500.00"));
        assertThat(result.getActualBalance()).isEqualByComparingTo(new BigDecimal("2500.00"));
        assertThat(result.getStatus()).isEqualTo("ACTIVE");
    }

    @Test
    @DisplayName("Should map Customer to CreditScoreRequestDto")
    void testToCreditScoreRequestDto() {
        // Given
        Customer customer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .address("123 Main St")
                .dateOfBirth(LocalDate.of(1990, 5, 15))
                .creditScore(650)
                .build();

        // When
        CreditScoreRequestDto result = dtoMapper.toCreditScoreRequestDto(customer);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getSortCode()).isEqualTo("987654");
        assertThat(result.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(result.getName()).isEqualTo("John Doe");
        assertThat(result.getAddress()).isEqualTo("123 Main St");
        assertThat(result.getDateOfBirth()).isEqualTo(LocalDate.of(1990, 5, 15));
        assertThat(result.getCurrentCreditScore()).isEqualTo(650);
    }

    @Test
    @DisplayName("Should update Customer from successful CreditScoreResponseDto")
    void testUpdateCustomerFromCreditResponseSuccess() {
        // Given
        Customer customer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .creditScore(650)
                .build();

        CreditScoreResponseDto response = CreditScoreResponseDto.builder()
                .success(true)
                .updatedCreditScore(720)
                .scoreReviewDate(LocalDate.of(2023, 12, 1))
                .build();

        // When
        Customer result = dtoMapper.updateCustomerFromCreditResponse(customer, response);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getCreditScore()).isEqualTo(720);
        assertThat(result.getCreditScoreReviewDate()).isEqualTo(LocalDate.of(2023, 12, 1));
    }

    @Test
    @DisplayName("Should not update Customer from failed CreditScoreResponseDto")
    void testUpdateCustomerFromCreditResponseFailure() {
        // Given
        Customer customer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .creditScore(650)
                .build();

        CreditScoreResponseDto response = CreditScoreResponseDto.builder()
                .success(false)
                .errorMessage("Credit check failed")
                .build();

        // When
        Customer result = dtoMapper.updateCustomerFromCreditResponse(customer, response);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getCreditScore()).isEqualTo(650); // Should remain unchanged
        assertThat(result.getCreditScoreReviewDate()).isNull(); // Should remain unchanged
    }

    @Test
    @DisplayName("Should derive customer status for fair credit score")
    void testCustomerStatusDerivationFairCredit() {
        // Given
        Customer customer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .creditScore(600) // Fair credit
                .build();

        // When
        CustomerResponseDto result = dtoMapper.toCustomerResponseDto(customer);

        // Then
        assertThat(result.getStatus()).isEqualTo("FAIR");
    }

    @Test
    @DisplayName("Should derive customer status for null credit score")
    void testCustomerStatusDerivationNullCredit() {
        // Given
        Customer customer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber(1234567890L)
                .name("John Doe")
                .creditScore(null)
                .build();

        // When
        CustomerResponseDto result = dtoMapper.toCustomerResponseDto(customer);

        // Then
        assertThat(result.getStatus()).isEqualTo("REVIEW_REQUIRED");
    }

    @Test
    @DisplayName("Should derive account status for active account")
    void testAccountStatusDerivationActive() {
        // Given
        Account account = Account.builder()
                .eyeCatcher("ACCT")
                .sortCode("987654")
                .accountNumber("12345678")
                .customerNumber(1234567890L)
                .accountType("CURRENT")
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .build();

        // When
        AccountResponseDto result = dtoMapper.toAccountResponseDto(account);

        // Then
        assertThat(result.getStatus()).isEqualTo("ACTIVE");
    }

    @Test
    @DisplayName("Should handle null values in CustomerRequestDto")
    void testToCustomerWithNullCreditScore() {
        // Given
        CustomerRequestDto request = CustomerRequestDto.builder()
                .sortCode("987654")
                .name("Jane Smith")
                .address("456 Oak Ave")
                .dateOfBirth(LocalDate.of(1985, 8, 20))
                .creditScore(null) // Null credit score
                .build();

        // When
        Customer result = dtoMapper.toCustomer(request);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getCreditScore()).isNull();
    }

    @Test
    @DisplayName("Should handle null values in AccountRequestDto")
    void testToAccountWithNullValues() {
        // Given
        AccountRequestDto request = AccountRequestDto.builder()
                .sortCode("987654")
                .customerNumber(1234567890L)
                .accountType("SAVINGS")
                .interestRate(null) // Null interest rate
                .overdraftLimit(null) // Null overdraft limit
                .build();

        // When
        Account result = dtoMapper.toAccount(request);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getInterestRate()).isNull();
        assertThat(result.getOverdraftLimit()).isNull();
    }
}
