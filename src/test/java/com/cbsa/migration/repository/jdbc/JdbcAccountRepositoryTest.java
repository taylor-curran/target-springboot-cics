package com.cbsa.migration.repository.jdbc;

import com.cbsa.migration.model.Account;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.JdbcTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Integration tests for JdbcAccountRepository
 * Uses H2 in-memory database for testing
 */
@JdbcTest
@ActiveProfiles("test")
class JdbcAccountRepositoryTest {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    private JdbcAccountRepository repository;

    @BeforeEach
    void setUp() {
        repository = new JdbcAccountRepository(jdbcTemplate);
        
        jdbcTemplate.update(
            "INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score) " +
            "VALUES (?, ?, ?, ?, ?, ?, ?)",
            "CUST", "987654", 1234567890L, "Test Customer", "123 Test St", "1990-01-01", 750
        );
    }

    @Test
    @DisplayName("Should save and find account by ID")
    void shouldSaveAndFindAccountById() {
        // Given
        Account account = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345678")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .openedDate(LocalDate.of(2023, 1, 15))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("1500.00"))
                .actualBalance(new BigDecimal("1750.00"))
                .build();

        // When
        Account savedAccount = repository.save(account);
        Optional<Account> foundAccount = repository.findById("987654", "12345678");

        // Then
        assertThat(savedAccount).isNotNull();
        assertThat(foundAccount).isPresent();
        assertThat(foundAccount.get().getAccountNumber()).isEqualTo("12345678");
        assertThat(foundAccount.get().getAccountType()).isEqualTo("CURRENT");
        assertThat(foundAccount.get().getAvailableBalance()).isEqualByComparingTo(new BigDecimal("1500.00"));
    }

    @Test
    @DisplayName("Should find accounts by customer number")
    void shouldFindAccountsByCustomerNumber() {
        // Given
        Account account1 = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345678")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .openedDate(LocalDate.of(2023, 1, 15))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .build();

        Account account2 = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345679")
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("2.50"))
                .openedDate(LocalDate.of(2023, 2, 1))
                .overdraftLimit(0)
                .availableBalance(new BigDecimal("2000.00"))
                .actualBalance(new BigDecimal("2000.00"))
                .build();

        repository.save(account1);
        repository.save(account2);

        // When
        List<Account> accounts = repository.findByCustomerNumber(1234567890L);

        // Then
        assertThat(accounts).hasSize(2);
        assertThat(accounts.get(0).getAccountType()).isIn("CURRENT", "SAVINGS");
        assertThat(accounts.get(1).getAccountType()).isIn("CURRENT", "SAVINGS");
    }

    @Test
    @DisplayName("Should update existing account")
    void shouldUpdateExistingAccount() {
        // Given
        Account account = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345678")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .openedDate(LocalDate.of(2023, 1, 15))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .build();

        repository.save(account);

        // When
        account.setAccountType("SAVINGS");
        account.setInterestRate(new BigDecimal("2.50"));
        account.setOverdraftLimit(2000);
        Account updatedAccount = repository.save(account);

        // Then
        Optional<Account> foundAccount = repository.findById("987654", "12345678");
        assertThat(foundAccount).isPresent();
        assertThat(foundAccount.get().getAccountType()).isEqualTo("SAVINGS");
        assertThat(foundAccount.get().getInterestRate()).isEqualByComparingTo(new BigDecimal("2.50"));
        assertThat(foundAccount.get().getOverdraftLimit()).isEqualTo(2000);
    }

    @Test
    @DisplayName("Should delete account by ID")
    void shouldDeleteAccountById() {
        // Given
        Account account = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345678")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .openedDate(LocalDate.of(2023, 1, 15))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .build();

        repository.save(account);

        // When
        boolean deleted = repository.deleteById("987654", "12345678");
        Optional<Account> foundAccount = repository.findById("987654", "12345678");

        // Then
        assertThat(deleted).isTrue();
        assertThat(foundAccount).isEmpty();
    }

    @Test
    @DisplayName("Should count all accounts")
    void shouldCountAllAccounts() {
        // Given
        Account account1 = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345678")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .openedDate(LocalDate.of(2023, 1, 15))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .build();

        Account account2 = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345679")
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("2.50"))
                .openedDate(LocalDate.of(2023, 2, 1))
                .overdraftLimit(0)
                .availableBalance(new BigDecimal("2000.00"))
                .actualBalance(new BigDecimal("2000.00"))
                .build();

        repository.save(account1);
        repository.save(account2);

        // When
        int count = repository.count();

        // Then
        assertThat(count).isEqualTo(2);
    }

    @Test
    @DisplayName("Should find accounts by sort code")
    void shouldFindAccountsBySortCode() {
        // Given
        Account account = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345678")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .openedDate(LocalDate.of(2023, 1, 15))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .build();

        repository.save(account);

        // When
        List<Account> accounts = repository.findBySortCode("987654");

        // Then
        assertThat(accounts).hasSize(1);
        assertThat(accounts.get(0).getSortCode()).isEqualTo("987654");
    }

    @Test
    @DisplayName("Should find top account by sort code ordered by account number desc")
    void shouldFindTopAccountBySortCodeOrderByAccountNumberDesc() {
        // Given
        Account account1 = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345678")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.10"))
                .openedDate(LocalDate.of(2023, 1, 15))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("1000.00"))
                .actualBalance(new BigDecimal("1000.00"))
                .build();

        Account account2 = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("987654")
                .accountNumber("12345679")
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("2.50"))
                .openedDate(LocalDate.of(2023, 2, 1))
                .overdraftLimit(0)
                .availableBalance(new BigDecimal("2000.00"))
                .actualBalance(new BigDecimal("2000.00"))
                .build();

        repository.save(account1);
        repository.save(account2);

        // When
        Optional<Account> topAccount = repository.findTopBySortCodeOrderByAccountNumberDesc("987654");

        // Then
        assertThat(topAccount).isPresent();
        assertThat(topAccount.get().getAccountNumber()).isEqualTo("12345679");
    }
}
