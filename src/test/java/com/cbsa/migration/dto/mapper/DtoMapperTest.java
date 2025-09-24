package com.cbsa.migration.dto.mapper;

import com.cbsa.migration.dto.*;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class DtoMapperTest {

    private DtoMapper dtoMapper;

    @BeforeEach
    void setUp() {
        dtoMapper = new DtoMapper();
    }

    @Test
    void testToCustomerResponseDto_WithCustomer() {
        Customer customer = new Customer();
        customer.setName("John Doe");
        customer.setAddress("123 Main St");
        customer.setDateOfBirth(LocalDate.of(1980, 1, 15));
        customer.setCustomerNumber(1234567890L);
        customer.setSortCode("123456");
        customer.setCreditScore(750);

        CustomerResponseDto result = dtoMapper.toCustomerResponseDto(customer);

        assertThat(result.getName()).isEqualTo("John Doe");
        assertThat(result.getAddress()).isEqualTo("123 Main St");
        assertThat(result.getDateOfBirth()).isEqualTo(LocalDate.of(1980, 1, 15));
        assertThat(result.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(result.getSortCode()).isEqualTo("123456");
        assertThat(result.getStatus()).isEqualTo("GOOD_STANDING");
    }

    @Test
    void testToCustomerResponseDto_WithCustomerAndAccounts() {
        Customer customer = new Customer();
        customer.setName("Jane Smith");
        customer.setCustomerNumber(9876543210L);
        customer.setCreditScore(600);

        Account account1 = new Account();
        account1.setAccountNumber("00000001");
        account1.setAccountType("CURRENT");
        account1.setAvailableBalance(new BigDecimal("1000.00"));
        account1.setActualBalance(new BigDecimal("1000.00"));

        Account account2 = new Account();
        account2.setAccountNumber("00000002");
        account2.setAccountType("SAVINGS");
        account2.setAvailableBalance(new BigDecimal("5000.00"));
        account2.setActualBalance(new BigDecimal("5000.00"));

        List<Account> accounts = Arrays.asList(account1, account2);

        CustomerResponseDto result = dtoMapper.toCustomerResponseDto(customer, accounts);

        assertThat(result.getName()).isEqualTo("Jane Smith");
        assertThat(result.getCustomerNumber()).isEqualTo(9876543210L);
        assertThat(result.getStatus()).isEqualTo("FAIR");
        assertThat(result.getAccountCount()).isEqualTo(2);
        assertThat(result.getAccounts()).hasSize(2);
        assertThat(result.getAccounts().get(0).getAccountNumber()).isEqualTo("00000001");
        assertThat(result.getAccounts().get(1).getAccountNumber()).isEqualTo("00000002");
    }

    @Test
    void testToCustomer_FromRequestDto() {
        CustomerRequestDto requestDto = CustomerRequestDto.builder()
                .name("Bob Johnson")
                .address("456 Oak Ave")
                .dateOfBirth(LocalDate.of(1975, 6, 20))
                .sortCode("654321")
                .creditScore(680)
                .build();

        Customer result = dtoMapper.toCustomer(requestDto);

        assertThat(result.getName()).isEqualTo("Bob Johnson");
        assertThat(result.getAddress()).isEqualTo("456 Oak Ave");
        assertThat(result.getDateOfBirth()).isEqualTo(LocalDate.of(1975, 6, 20));
        assertThat(result.getSortCode()).isEqualTo("654321");
        assertThat(result.getCreditScore()).isEqualTo(680);
    }

    @Test
    void testToCustomer_FromRequestDto_WithNullCreditScore() {
        CustomerRequestDto requestDto = CustomerRequestDto.builder()
                .name("Alice Brown")
                .address("789 Pine St")
                .dateOfBirth(LocalDate.of(1990, 3, 10))
                .sortCode("111222")
                .build();

        Customer result = dtoMapper.toCustomer(requestDto);

        assertThat(result.getName()).isEqualTo("Alice Brown");
        assertThat(result.getAddress()).isEqualTo("789 Pine St");
        assertThat(result.getDateOfBirth()).isEqualTo(LocalDate.of(1990, 3, 10));
        assertThat(result.getSortCode()).isEqualTo("111222");
        assertThat(result.getCreditScore()).isNull();
    }

    @Test
    void testToAccountResponseDto_WithAccount() {
        Account account = new Account();
        account.setAccountNumber("12345678");
        account.setSortCode("123456");
        account.setAccountType("CURRENT");
        account.setCustomerNumber(1234567890L);
        account.setAvailableBalance(new BigDecimal("500.00"));
        account.setActualBalance(new BigDecimal("500.00"));
        account.setInterestRate(new BigDecimal("2.50"));
        account.setOpenedDate(LocalDate.of(2023, 1, 15));
        account.setOverdraftLimit(1000);
        account.setLastStatementDate(LocalDate.of(2023, 1, 1));
        account.setNextStatementDate(LocalDate.of(2023, 2, 1));

        AccountResponseDto result = dtoMapper.toAccountResponseDto(account);

        assertThat(result.getAccountNumber()).isEqualTo("12345678");
        assertThat(result.getSortCode()).isEqualTo("123456");
        assertThat(result.getAccountType()).isEqualTo("CURRENT");
        assertThat(result.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(result.getAvailableBalance()).isEqualTo(new BigDecimal("500.00"));
        assertThat(result.getActualBalance()).isEqualTo(new BigDecimal("500.00"));
        assertThat(result.getInterestRate()).isEqualTo(new BigDecimal("2.50"));
        assertThat(result.getOpenedDate()).isEqualTo(LocalDate.of(2023, 1, 15));
        assertThat(result.getOverdraftLimit()).isEqualTo(1000);
        assertThat(result.getLastStatementDate()).isEqualTo(LocalDate.of(2023, 1, 1));
        assertThat(result.getNextStatementDate()).isEqualTo(LocalDate.of(2023, 2, 1));
        assertThat(result.getStatus()).isEqualTo("ACTIVE");
    }

    @Test
    void testToAccountResponseDto_WithCustomerName() {
        Account account = new Account();
        account.setAccountNumber("87654321");
        account.setAccountType("SAVINGS");
        account.setAvailableBalance(new BigDecimal("2000.00"));
        account.setActualBalance(new BigDecimal("2000.00"));

        AccountResponseDto result = dtoMapper.toAccountResponseDto(account, "John Doe");

        assertThat(result.getAccountNumber()).isEqualTo("87654321");
        assertThat(result.getAccountType()).isEqualTo("SAVINGS");
        assertThat(result.getCustomerName()).isEqualTo("John Doe");
        assertThat(result.getStatus()).isEqualTo("ACTIVE");
    }

    @Test
    void testToAccountSummaryDto() {
        Account account = new Account();
        account.setAccountNumber("11111111");
        account.setAccountType("MORTGAGE");
        account.setAvailableBalance(new BigDecimal("-50000.00"));
        account.setActualBalance(new BigDecimal("-50000.00"));

        AccountSummaryDto result = dtoMapper.toAccountSummaryDto(account);

        assertThat(result.getAccountNumber()).isEqualTo("11111111");
        assertThat(result.getAccountType()).isEqualTo("MORTGAGE");
        assertThat(result.getAvailableBalance()).isEqualTo(new BigDecimal("-50000.00"));
        assertThat(result.getActualBalance()).isEqualTo(new BigDecimal("-50000.00"));
        assertThat(result.getStatus()).isEqualTo("OVERDRAFT");
    }

    @Test
    void testToAccount_FromRequestDto() {
        AccountRequestDto requestDto = AccountRequestDto.builder()
                .accountType("CURRENT")
                .customerNumber(1234567890L)
                .sortCode("123456")
                .interestRate(new BigDecimal("3.00"))
                .overdraftLimit(1500)
                .build();

        Account result = dtoMapper.toAccount(requestDto);

        assertThat(result.getAccountType()).isEqualTo("CURRENT");
        assertThat(result.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(result.getSortCode()).isEqualTo("123456");
        assertThat(result.getInterestRate()).isEqualTo(new BigDecimal("3.00"));
        assertThat(result.getOverdraftLimit()).isEqualTo(1500);
    }

    @Test
    void testToAccount_FromRequestDto_WithNullValues() {
        AccountRequestDto requestDto = AccountRequestDto.builder()
                .accountType("SAVINGS")
                .customerNumber(9876543210L)
                .sortCode("654321")
                .build();

        Account result = dtoMapper.toAccount(requestDto);

        assertThat(result.getAccountType()).isEqualTo("SAVINGS");
        assertThat(result.getCustomerNumber()).isEqualTo(9876543210L);
        assertThat(result.getSortCode()).isEqualTo("654321");
        assertThat(result.getInterestRate()).isNull();
        assertThat(result.getOverdraftLimit()).isNull();
    }

    @Test
    void testToTransactionResponseDto() {
        Transaction transaction = new Transaction();
        transaction.setReferenceNumber(123456789012L);
        transaction.setSortCode("123456");
        transaction.setAccountNumber("12345678");
        transaction.setTransactionDate(LocalDate.of(2023, 6, 15));
        transaction.setTransactionTime(LocalTime.of(14, 30, 0));
        transaction.setTransactionType("DEBIT");
        transaction.setDescription("ATM Withdrawal");
        transaction.setAmount(new BigDecimal("100.00"));
        transaction.setTargetSortCode("654321");
        transaction.setTargetAccountNumber("87654321");

        TransactionResponseDto result = dtoMapper.toTransactionResponseDto(transaction);

        assertThat(result.getReferenceNumber()).isEqualTo(123456789012L);
        assertThat(result.getSortCode()).isEqualTo("123456");
        assertThat(result.getAccountNumber()).isEqualTo("12345678");
        assertThat(result.getTransactionDate()).isEqualTo(LocalDate.of(2023, 6, 15));
        assertThat(result.getTransactionTime()).isEqualTo(LocalTime.of(14, 30, 0));
        assertThat(result.getTransactionType()).isEqualTo("DEBIT");
        assertThat(result.getDescription()).isEqualTo("ATM Withdrawal");
        assertThat(result.getAmount()).isEqualTo(new BigDecimal("100.00"));
        assertThat(result.getTargetSortCode()).isEqualTo("654321");
        assertThat(result.getTargetAccountNumber()).isEqualTo("87654321");
        assertThat(result.getStatus()).isEqualTo("COMPLETED");
    }

    @Test
    void testToTransaction_FromRequestDto() {
        TransactionRequestDto requestDto = TransactionRequestDto.builder()
                .sortCode("123456")
                .accountNumber("12345678")
                .transactionType("CREDIT")
                .description("Salary Payment")
                .amount(new BigDecimal("2500.00"))
                .targetSortCode("111222")
                .targetAccountNumber("11223344")
                .build();

        Transaction result = dtoMapper.toTransaction(requestDto);

        assertThat(result.getSortCode()).isEqualTo("123456");
        assertThat(result.getAccountNumber()).isEqualTo("12345678");
        assertThat(result.getTransactionType()).isEqualTo("CREDIT");
        assertThat(result.getDescription()).isEqualTo("Salary Payment");
        assertThat(result.getAmount()).isEqualTo(new BigDecimal("2500.00"));
        assertThat(result.getTargetSortCode()).isEqualTo("111222");
        assertThat(result.getTargetAccountNumber()).isEqualTo("11223344");
    }

    @Test
    void testDeriveCustomerStatus_GoodStanding() {
        Customer customer = new Customer();
        customer.setCreditScore(750);

        CustomerResponseDto result = dtoMapper.toCustomerResponseDto(customer);

        assertThat(result.getStatus()).isEqualTo("GOOD_STANDING");
    }

    @Test
    void testDeriveCustomerStatus_Fair() {
        Customer customer = new Customer();
        customer.setCreditScore(600);

        CustomerResponseDto result = dtoMapper.toCustomerResponseDto(customer);

        assertThat(result.getStatus()).isEqualTo("FAIR");
    }

    @Test
    void testDeriveCustomerStatus_ReviewRequired() {
        Customer customer = new Customer();
        customer.setCreditScore(400);

        CustomerResponseDto result = dtoMapper.toCustomerResponseDto(customer);

        assertThat(result.getStatus()).isEqualTo("REVIEW_REQUIRED");
    }

    @Test
    void testDeriveCustomerStatus_NullCreditScore() {
        Customer customer = new Customer();
        customer.setCreditScore(null);

        CustomerResponseDto result = dtoMapper.toCustomerResponseDto(customer);

        assertThat(result.getStatus()).isEqualTo("REVIEW_REQUIRED");
    }

    @Test
    void testDeriveAccountStatus_Active() {
        Account account = new Account();
        account.setAvailableBalance(new BigDecimal("1000.00"));
        account.setActualBalance(new BigDecimal("1000.00"));

        AccountResponseDto result = dtoMapper.toAccountResponseDto(account);

        assertThat(result.getStatus()).isEqualTo("ACTIVE");
    }

    @Test
    void testDeriveAccountStatus_Overdraft() {
        Account account = new Account();
        account.setAvailableBalance(new BigDecimal("-100.00"));
        account.setActualBalance(new BigDecimal("-100.00"));

        AccountResponseDto result = dtoMapper.toAccountResponseDto(account);

        assertThat(result.getStatus()).isEqualTo("OVERDRAFT");
    }

    @Test
    void testDeriveAccountStatus_Overdrawn() {
        Account account = new Account();
        account.setAvailableBalance(new BigDecimal("500.00"));
        account.setActualBalance(new BigDecimal("1000.00"));

        AccountResponseDto result = dtoMapper.toAccountResponseDto(account);

        assertThat(result.getStatus()).isEqualTo("OVERDRAWN");
    }

    @Test
    void testToCreditScoreRequestDto() {
        Customer customer = new Customer();
        customer.setSortCode("123456");
        customer.setCustomerNumber(1234567890L);
        customer.setName("John Doe");
        customer.setAddress("123 Main St");
        customer.setDateOfBirth(LocalDate.of(1980, 1, 15));
        customer.setCreditScore(700);

        CreditScoreRequestDto result = dtoMapper.toCreditScoreRequestDto(customer);

        assertThat(result.getSortCode()).isEqualTo("123456");
        assertThat(result.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(result.getName()).isEqualTo("John Doe");
        assertThat(result.getAddress()).isEqualTo("123 Main St");
        assertThat(result.getDateOfBirth()).isEqualTo(LocalDate.of(1980, 1, 15));
        assertThat(result.getCurrentCreditScore()).isEqualTo(700);
    }

    @Test
    void testUpdateCustomerFromCreditResponse_Success() {
        Customer customer = new Customer();
        customer.setCreditScore(650);

        CreditScoreResponseDto response = CreditScoreResponseDto.builder()
                .success(true)
                .updatedCreditScore(720)
                .scoreReviewDate(LocalDate.of(2023, 6, 15))
                .build();

        Customer result = dtoMapper.updateCustomerFromCreditResponse(customer, response);

        assertThat(result.getCreditScore()).isEqualTo(720);
        assertThat(result.getCreditScoreReviewDate()).isEqualTo(LocalDate.of(2023, 6, 15));
    }

    @Test
    void testUpdateCustomerFromCreditResponse_Failure() {
        Customer customer = new Customer();
        customer.setCreditScore(650);

        CreditScoreResponseDto response = CreditScoreResponseDto.builder()
                .success(false)
                .build();

        Customer result = dtoMapper.updateCustomerFromCreditResponse(customer, response);

        assertThat(result.getCreditScore()).isEqualTo(650);
        assertThat(result.getCreditScoreReviewDate()).isNull();
    }

    @Test
    void testUpdateCustomerFromCreditResponse_NullUpdatedScore() {
        Customer customer = new Customer();
        customer.setCreditScore(650);

        CreditScoreResponseDto response = CreditScoreResponseDto.builder()
                .success(true)
                .updatedCreditScore(null)
                .build();

        Customer result = dtoMapper.updateCustomerFromCreditResponse(customer, response);

        assertThat(result.getCreditScore()).isEqualTo(650);
        assertThat(result.getCreditScoreReviewDate()).isNull();
    }
}
