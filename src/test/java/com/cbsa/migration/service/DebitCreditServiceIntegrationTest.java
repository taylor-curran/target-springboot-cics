package com.cbsa.migration.service;

import com.cbsa.migration.dto.DebitCreditRequestDto;
import com.cbsa.migration.dto.DebitCreditResponseDto;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@ActiveProfiles("test")
class DebitCreditServiceIntegrationTest {

    @Autowired
    private DebitCreditService service;
    
    @Autowired
    private AccountRepository accountRepository;
    
    @Autowired
    private CustomerRepository customerRepository;
    
    @Autowired
    private TransactionRepository transactionRepository;
    
    @Autowired
    private JdbcTemplate jdbcTemplate;
    
    private Customer testCustomer;
    private Account testAccount;
    
    @BeforeEach
    void setUp() {
        jdbcTemplate.update("DELETE FROM bank_transaction WHERE sort_code = ?", "987654");
        jdbcTemplate.update("DELETE FROM account WHERE sort_code = ?", "987654");
        jdbcTemplate.update("DELETE FROM customer WHERE sort_code = ?", "987654");
        
        testCustomer = Customer.builder()
            .eyeCatcher("CUST")
            .sortCode("987654")
            .customerNumber(999999999L)
            .name("Test Customer")
            .address("123 Test Street, Test City")
            .dateOfBirth(LocalDate.of(1980, 1, 1))
            .creditScore(750)
            .build();
        
        customerRepository.save(testCustomer);
        
        testAccount = Account.builder()
            .eyeCatcher("ACCT")
            .sortCode("987654")
            .accountNumber("99999999")
            .customerNumber(999999999L)
            .accountType("CURRENT")
            .interestRate(new BigDecimal("1.50"))
            .openedDate(LocalDate.now())
            .overdraftLimit(500)
            .availableBalance(new BigDecimal("1000.00"))
            .actualBalance(new BigDecimal("1000.00"))
            .build();
        
        accountRepository.save(testAccount);
    }
    
    @Test
    void shouldProcessDebitAndCreateTransaction() {
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("99999999")
            .amount(new BigDecimal("-100.00"))
            .facilityType(100)
            .build();
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('Y');
        assertThat(response.getAvailableBalance()).isEqualByComparingTo(new BigDecimal("900.00"));
        assertThat(response.getActualBalance()).isEqualByComparingTo(new BigDecimal("900.00"));
        
        Optional<Account> updatedAccount = accountRepository.findById("987654", "99999999");
        assertThat(updatedAccount).isPresent();
        assertThat(updatedAccount.get().getAvailableBalance()).isEqualByComparingTo(new BigDecimal("900.00"));
        
        List<Transaction> transactions = transactionRepository.findByAccount("987654", "99999999");
        assertThat(transactions).hasSize(1);
        assertThat(transactions.get(0).getTransactionType()).isEqualTo("DEB");
        assertThat(transactions.get(0).getAmount()).isEqualByComparingTo(new BigDecimal("-100.00"));
    }
    
    @Test
    void shouldProcessCreditAndCreateTransaction() {
        DebitCreditRequestDto request = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("99999999")
            .amount(new BigDecimal("250.00"))
            .facilityType(100)
            .build();
        
        DebitCreditResponseDto response = service.processDebitCredit(request);
        
        assertThat(response.getSuccess()).isEqualTo('Y');
        assertThat(response.getAvailableBalance()).isEqualByComparingTo(new BigDecimal("1250.00"));
        
        Optional<Account> updatedAccount = accountRepository.findById("987654", "99999999");
        assertThat(updatedAccount).isPresent();
        assertThat(updatedAccount.get().getAvailableBalance()).isEqualByComparingTo(new BigDecimal("1250.00"));
        
        List<Transaction> transactions = transactionRepository.findByAccount("987654", "99999999");
        assertThat(transactions).hasSize(1);
        assertThat(transactions.get(0).getTransactionType()).isEqualTo("CRE");
    }
    
    @Test
    void shouldHandleMultipleTransactions() {
        DebitCreditRequestDto debit = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("99999999")
            .amount(new BigDecimal("-100.00"))
            .facilityType(100)
            .build();
        
        DebitCreditRequestDto credit = DebitCreditRequestDto.builder()
            .sortCode("987654")
            .accountNumber("99999999")
            .amount(new BigDecimal("200.00"))
            .facilityType(100)
            .build();
        
        service.processDebitCredit(debit);
        service.processDebitCredit(credit);
        
        Optional<Account> updatedAccount = accountRepository.findById("987654", "99999999");
        assertThat(updatedAccount).isPresent();
        assertThat(updatedAccount.get().getAvailableBalance()).isEqualByComparingTo(new BigDecimal("1100.00"));
        
        List<Transaction> transactions = transactionRepository.findByAccount("987654", "99999999");
        assertThat(transactions).hasSize(2);
    }
}
