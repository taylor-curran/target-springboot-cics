package com.cbsa.migration.datagen;

import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Control;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.TestPropertySource;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * Integration test for BankDataGenerator
 * Tests data generation with real database (H2 for tests)
 */
@SpringBootTest
@TestPropertySource(locations = "classpath:application-test.properties")
@Transactional
@org.springframework.test.context.ActiveProfiles("test")
class BankDataGeneratorTest {
    
    @Autowired
    private BankDataGenerator dataGenerator;
    
    @Autowired
    private JdbcTemplate jdbcTemplate;
    
    @Autowired
    private CustomerRepository customerRepository;
    
    @Autowired
    private AccountRepository accountRepository;
    
    @Autowired
    private ControlRepository controlRepository;
    
    @BeforeEach
    void setUp() {
        // Clear any existing data
        jdbcTemplate.update("DELETE FROM bank_transaction");
        jdbcTemplate.update("DELETE FROM account");
        jdbcTemplate.update("DELETE FROM customer");
        jdbcTemplate.update("DELETE FROM control");
    }
    
    @Test
    void testGenerateData_BasicGeneration() {
        // Generate 5 customers
        dataGenerator.generateData(1, 5, 1, 12345L);
        
        // Verify customers created
        int customerCount = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM customer", Integer.class);
        assertThat(customerCount).isEqualTo(5);
        
        // Verify accounts created (1-5 per customer, so at least 5)
        int accountCount = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM account", Integer.class);
        assertThat(accountCount).isGreaterThanOrEqualTo(5);
        assertThat(accountCount).isLessThanOrEqualTo(25); // Max 5 per customer
        
        // Verify control record updated
        Control control = controlRepository.getControl().orElseThrow();
        assertThat(control.getCustomerCount()).isEqualTo(5);
        assertThat(control.getAccountCount()).isEqualTo(accountCount);
    }
    
    @Test
    void testGenerateData_CustomerDataCorrectness() {
        // Generate 1 customer with fixed seed for predictable data
        dataGenerator.generateData(100, 100, 1, 99999L);
        
        List<Customer> customers = customerRepository.findAll();
        assertThat(customers).hasSize(1);
        
        Customer customer = customers.get(0);
        assertThat(customer.getEyeCatcher()).isEqualTo("CUST");
        assertThat(customer.getSortCode()).isEqualTo("987654");
        assertThat(customer.getCustomerNumber()).isEqualTo(100L);
        assertThat(customer.getName()).isNotEmpty();
        assertThat(customer.getAddress()).isNotEmpty();
        assertThat(customer.getDateOfBirth()).isNotNull();
        assertThat(customer.getCreditScore()).isBetween(1, 999);
        assertThat(customer.getCreditScoreReviewDate()).isNotNull();
    }
    
    @Test
    void testGenerateData_AccountDataCorrectness() {
        // Generate 1 customer with fixed seed
        dataGenerator.generateData(200, 200, 1, 88888L);
        
        List<Account> accounts = accountRepository.findByCustomerNumber(200L);
        assertThat(accounts).isNotEmpty();
        
        Account account = accounts.get(0);
        assertThat(account.getEyeCatcher()).isEqualTo("ACCT");
        assertThat(account.getSortCode()).isEqualTo("987654");
        assertThat(account.getCustomerNumber()).isEqualTo(200L);
        assertThat(account.getAccountNumber()).matches("\\d{8}");
        assertThat(account.getAccountType()).isIn("ISA", "SAVING", "CURRENT", "LOAN", "MORTGAGE");
        assertThat(account.getInterestRate()).isGreaterThanOrEqualTo(BigDecimal.ZERO);
        assertThat(account.getOpenedDate()).isNotNull();
        assertThat(account.getAvailableBalance()).isNotNull();
        assertThat(account.getActualBalance()).isNotNull();
    }
    
    @Test
    void testGenerateData_StepParameter() {
        // Generate customers 10, 20, 30
        dataGenerator.generateData(10, 30, 10, 77777L);
        
        // Verify only 3 customers created
        int customerCount = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM customer", Integer.class);
        assertThat(customerCount).isEqualTo(3);
        
        // Verify customer numbers
        List<Long> customerNumbers = jdbcTemplate.queryForList(
            "SELECT customer_number FROM customer ORDER BY customer_number", Long.class);
        assertThat(customerNumbers).containsExactly(10L, 20L, 30L);
    }
    
    @Test
    void testGenerateData_DataClearing() {
        // First generation
        dataGenerator.generateData(1, 3, 1, 11111L);
        int firstCount = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM customer", Integer.class);
        assertThat(firstCount).isEqualTo(3);
        
        // Second generation should clear first data
        dataGenerator.generateData(100, 102, 1, 22222L);
        int secondCount = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM customer", Integer.class);
        assertThat(secondCount).isEqualTo(3);
        
        // Verify these are different customers
        List<Long> customerNumbers = jdbcTemplate.queryForList(
            "SELECT customer_number FROM customer ORDER BY customer_number", Long.class);
        assertThat(customerNumbers).containsExactly(100L, 101L, 102L);
    }
    
    @Test
    void testGenerateData_ValidationErrors() {
        // Test end < start
        assertThatThrownBy(() -> dataGenerator.generateData(10, 5, 1, 12345L))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("Final customer number cannot be smaller");
        
        // Test step = 0
        assertThatThrownBy(() -> dataGenerator.generateData(1, 10, 0, 12345L))
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("Gap between customers cannot be zero");
    }
    
    @Test
    void testGenerateData_ReproducibilityWithSeed() {
        // Generate with same seed twice
        dataGenerator.generateData(1, 1, 1, 55555L);
        
        Customer customer1 = customerRepository.findById("987654", 1L).orElseThrow();
        String name1 = customer1.getName();
        String address1 = customer1.getAddress();
        
        // Clear and regenerate with same seed
        jdbcTemplate.update("DELETE FROM bank_transaction");
        jdbcTemplate.update("DELETE FROM account");
        jdbcTemplate.update("DELETE FROM customer");
        jdbcTemplate.update("DELETE FROM control");
        
        dataGenerator.generateData(1, 1, 1, 55555L);
        
        Customer customer2 = customerRepository.findById("987654", 1L).orElseThrow();
        String name2 = customer2.getName();
        String address2 = customer2.getAddress();
        
        // Should generate identical data with same seed
        assertThat(name2).isEqualTo(name1);
        assertThat(address2).isEqualTo(address1);
    }
    
    @Test
    void testGenerateData_MinimumDataRequirement() {
        // Test requirement: at least 3 rows of each entity type
        dataGenerator.generateData(1, 3, 1, 12345L);
        
        int customerCount = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM customer", Integer.class);
        assertThat(customerCount).isGreaterThanOrEqualTo(3);
        
        int accountCount = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM account", Integer.class);
        assertThat(accountCount).isGreaterThanOrEqualTo(3);
        
        // Note: Transaction generation will be added in Phase 2
    }
}
