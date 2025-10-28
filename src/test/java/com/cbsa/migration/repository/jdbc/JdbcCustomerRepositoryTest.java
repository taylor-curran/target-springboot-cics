package com.cbsa.migration.repository.jdbc;

import com.cbsa.migration.model.Customer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.JdbcTest;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.TestPropertySource;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Integration tests for JdbcCustomerRepository
 * Tests JDBC operations against H2 in-memory database
 */
@JdbcTest
@Import(JdbcCustomerRepository.class)
@TestPropertySource(locations = "classpath:application-test.properties")
class JdbcCustomerRepositoryTest {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Autowired
    private JdbcCustomerRepository customerRepository;

    @BeforeEach
    void setUp() {
        jdbcTemplate.execute("DELETE FROM customer");
    }

    @Test
    @DisplayName("Should save new customer successfully")
    void shouldSaveNewCustomer() {
        // Given
        Customer customer = createTestCustomer();

        // When
        Customer savedCustomer = customerRepository.save(customer);

        // Then
        assertThat(savedCustomer).isNotNull();
        assertThat(savedCustomer.getSortCode()).isEqualTo("123456");
        assertThat(savedCustomer.getCustomerNumber()).isEqualTo(12345678L);
        assertThat(savedCustomer.getName()).isEqualTo("John Doe");

        Optional<Customer> found = customerRepository.findById("123456", 12345678L);
        assertThat(found).isPresent();
        assertThat(found.get().getName()).isEqualTo("John Doe");
    }

    @Test
    @DisplayName("Should update existing customer successfully")
    void shouldUpdateExistingCustomer() {
        // Given
        Customer customer = createTestCustomer();
        customerRepository.save(customer);

        customer.setName("Jane Doe");
        customer.setAddress("456 Oak Ave");
        customer.setCreditScore(800);
        customer.setCreditScoreReviewDate(LocalDate.of(2023, 12, 1));
        Customer updatedCustomer = customerRepository.save(customer);

        // Then
        assertThat(updatedCustomer).isNotNull();
        assertThat(updatedCustomer.getName()).isEqualTo("Jane Doe");
        assertThat(updatedCustomer.getAddress()).isEqualTo("456 Oak Ave");
        assertThat(updatedCustomer.getCreditScore()).isEqualTo(800);

        Optional<Customer> found = customerRepository.findById("123456", 12345678L);
        assertThat(found).isPresent();
        assertThat(found.get().getName()).isEqualTo("Jane Doe");
        assertThat(found.get().getAddress()).isEqualTo("456 Oak Ave");
        assertThat(found.get().getCreditScore()).isEqualTo(800);
        assertThat(found.get().getCreditScoreReviewDate()).isEqualTo(LocalDate.of(2023, 12, 1));
    }

    @Test
    @DisplayName("Should find customer by ID successfully")
    void shouldFindCustomerById() {
        // Given
        Customer customer = createTestCustomer();
        customerRepository.save(customer);

        // When
        Optional<Customer> found = customerRepository.findById("123456", 12345678L);

        // Then
        assertThat(found).isPresent();
        assertThat(found.get().getSortCode()).isEqualTo("123456");
        assertThat(found.get().getCustomerNumber()).isEqualTo(12345678L);
        assertThat(found.get().getName()).isEqualTo("John Doe");
        assertThat(found.get().getAddress()).isEqualTo("123 Main St");
        assertThat(found.get().getDateOfBirth()).isEqualTo(LocalDate.of(1990, 5, 15));
        assertThat(found.get().getCreditScore()).isEqualTo(750);
    }

    @Test
    @DisplayName("Should return empty when customer not found by ID")
    void shouldReturnEmptyWhenCustomerNotFound() {
        // When
        Optional<Customer> found = customerRepository.findById("999999", 99999999L);

        // Then
        assertThat(found).isEmpty();
    }

    @Test
    @DisplayName("Should find customers by name containing")
    void shouldFindCustomersByNameContaining() {
        // Given
        Customer customer1 = createTestCustomer();
        customer1.setName("John Doe");
        customerRepository.save(customer1);

        Customer customer2 = createTestCustomer();
        customer2.setCustomerNumber(12345679L);
        customer2.setName("Jane Doe");
        customerRepository.save(customer2);

        Customer customer3 = createTestCustomer();
        customer3.setCustomerNumber(12345680L);
        customer3.setName("Bob Smith");
        customerRepository.save(customer3);

        // When
        List<Customer> foundByDoe = customerRepository.findByNameContaining("Doe");
        List<Customer> foundByJohn = customerRepository.findByNameContaining("John");
        List<Customer> foundBySmith = customerRepository.findByNameContaining("Smith");

        // Then
        assertThat(foundByDoe).hasSize(2);
        assertThat(foundByDoe).extracting(Customer::getName)
                .containsExactlyInAnyOrder("John Doe", "Jane Doe");

        assertThat(foundByJohn).hasSize(1);
        assertThat(foundByJohn.get(0).getName()).isEqualTo("John Doe");

        assertThat(foundBySmith).hasSize(1);
        assertThat(foundBySmith.get(0).getName()).isEqualTo("Bob Smith");
    }

    @Test
    @DisplayName("Should return empty list when no customers match name search")
    void shouldReturnEmptyListWhenNoCustomersMatchNameSearch() {
        // Given
        Customer customer = createTestCustomer();
        customerRepository.save(customer);

        // When
        List<Customer> found = customerRepository.findByNameContaining("NonExistent");

        // Then
        assertThat(found).isEmpty();
    }

    @Test
    @DisplayName("Should delete customer by ID successfully")
    void shouldDeleteCustomerById() {
        // Given
        Customer customer = createTestCustomer();
        customerRepository.save(customer);

        Optional<Customer> found = customerRepository.findById("123456", 12345678L);
        assertThat(found).isPresent();

        // When
        boolean deleted = customerRepository.deleteById("123456", 12345678L);

        // Then
        assertThat(deleted).isTrue();

        Optional<Customer> notFound = customerRepository.findById("123456", 12345678L);
        assertThat(notFound).isEmpty();
    }

    @Test
    @DisplayName("Should return false when deleting non-existent customer")
    void shouldReturnFalseWhenDeletingNonExistentCustomer() {
        // When
        boolean deleted = customerRepository.deleteById("999999", 99999999L);

        // Then
        assertThat(deleted).isFalse();
    }

    @Test
    @DisplayName("Should find all customers")
    void shouldFindAllCustomers() {
        // Given
        Customer customer1 = createTestCustomer();
        customer1.setName("John Doe");
        customerRepository.save(customer1);

        Customer customer2 = createTestCustomer();
        customer2.setCustomerNumber(12345679L);
        customer2.setName("Jane Doe");
        customerRepository.save(customer2);

        Customer customer3 = createTestCustomer();
        customer3.setCustomerNumber(12345680L);
        customer3.setName("Bob Smith");
        customerRepository.save(customer3);

        // When
        List<Customer> allCustomers = customerRepository.findAll();

        // Then
        assertThat(allCustomers).hasSize(3);
        assertThat(allCustomers).extracting(Customer::getName)
                .containsExactlyInAnyOrder("John Doe", "Jane Doe", "Bob Smith");
    }

    @Test
    @DisplayName("Should return empty list when no customers exist")
    void shouldReturnEmptyListWhenNoCustomersExist() {
        // When
        List<Customer> allCustomers = customerRepository.findAll();

        // Then
        assertThat(allCustomers).isEmpty();
    }

    @Test
    @DisplayName("Should count customers correctly")
    void shouldCountCustomersCorrectly() {
        int initialCount = customerRepository.count();
        assertThat(initialCount).isEqualTo(0);

        Customer customer1 = createTestCustomer();
        customerRepository.save(customer1);

        Customer customer2 = createTestCustomer();
        customer2.setCustomerNumber(12345679L);
        customerRepository.save(customer2);

        // When
        int count = customerRepository.count();

        // Then
        assertThat(count).isEqualTo(2);
    }

    @Test
    @DisplayName("Should handle customer with null credit score review date")
    void shouldHandleCustomerWithNullCreditScoreReviewDate() {
        // Given
        Customer customer = createTestCustomer();
        customer.setCreditScoreReviewDate(null);

        // When
        Customer savedCustomer = customerRepository.save(customer);

        // Then
        assertThat(savedCustomer).isNotNull();
        assertThat(savedCustomer.getCreditScoreReviewDate()).isNull();

        Optional<Customer> found = customerRepository.findById("123456", 12345678L);
        assertThat(found).isPresent();
        assertThat(found.get().getCreditScoreReviewDate()).isNull();
    }

    @Test
    @DisplayName("Should handle customer with credit score review date")
    void shouldHandleCustomerWithCreditScoreReviewDate() {
        // Given
        Customer customer = createTestCustomer();
        customer.setCreditScoreReviewDate(LocalDate.of(2023, 6, 15));

        // When
        Customer savedCustomer = customerRepository.save(customer);

        // Then
        assertThat(savedCustomer).isNotNull();
        assertThat(savedCustomer.getCreditScoreReviewDate()).isEqualTo(LocalDate.of(2023, 6, 15));

        Optional<Customer> found = customerRepository.findById("123456", 12345678L);
        assertThat(found).isPresent();
        assertThat(found.get().getCreditScoreReviewDate()).isEqualTo(LocalDate.of(2023, 6, 15));
    }

    @Test
    @DisplayName("Should handle row mapper with all fields populated")
    void shouldHandleRowMapperWithAllFieldsPopulated() {
        jdbcTemplate.update(
            "INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, " +
            "date_of_birth, credit_score, credit_score_review_date) " +
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
            "CUST", "654321", 98765432L, "Test User", "789 Test Ave",
            "1985-08-20", 680, "2023-07-01"
        );

        // When
        Optional<Customer> found = customerRepository.findById("654321", 98765432L);

        // Then
        assertThat(found).isPresent();
        Customer customer = found.get();
        assertThat(customer.getEyeCatcher()).isEqualTo("CUST");
        assertThat(customer.getSortCode()).isEqualTo("654321");
        assertThat(customer.getCustomerNumber()).isEqualTo(98765432L);
        assertThat(customer.getName()).isEqualTo("Test User");
        assertThat(customer.getAddress()).isEqualTo("789 Test Ave");
        assertThat(customer.getDateOfBirth()).isEqualTo(LocalDate.of(1985, 8, 20));
        assertThat(customer.getCreditScore()).isEqualTo(680);
        assertThat(customer.getCreditScoreReviewDate()).isEqualTo(LocalDate.of(2023, 7, 1));
    }

    @Test
    @DisplayName("Should handle row mapper with null credit score review date")
    void shouldHandleRowMapperWithNullCreditScoreReviewDate() {
        jdbcTemplate.update(
            "INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, " +
            "date_of_birth, credit_score, credit_score_review_date) " +
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
            "CUST", "654321", 98765432L, "Test User", "789 Test Ave",
            "1985-08-20", 680, null
        );

        // When
        Optional<Customer> found = customerRepository.findById("654321", 98765432L);

        // Then
        assertThat(found).isPresent();
        Customer customer = found.get();
        assertThat(customer.getCreditScoreReviewDate()).isNull();
    }

    @Test
    @DisplayName("Should handle row mapper with empty credit score review date")
    void shouldHandleRowMapperWithEmptyCreditScoreReviewDate() {
        jdbcTemplate.update(
            "INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, " +
            "date_of_birth, credit_score, credit_score_review_date) " +
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
            "CUST", "654321", 98765432L, "Test User", "789 Test Ave",
            "1985-08-20", 680, ""
        );

        // When
        Optional<Customer> found = customerRepository.findById("654321", 98765432L);

        // Then
        assertThat(found).isPresent();
        Customer customer = found.get();
        assertThat(customer.getCreditScoreReviewDate()).isNull();
    }

    @Test
    @DisplayName("Should handle exact case name search")
    void shouldHandleExactCaseNameSearch() {
        // Given
        Customer customer = createTestCustomer();
        customer.setName("John Doe");
        customerRepository.save(customer);

        // When
        List<Customer> foundExactCase = customerRepository.findByNameContaining("John");
        List<Customer> foundPartialMatch = customerRepository.findByNameContaining("Doe");
        List<Customer> foundNoMatch = customerRepository.findByNameContaining("jane");

        assertThat(foundExactCase).hasSize(1);
        assertThat(foundPartialMatch).hasSize(1);
        assertThat(foundNoMatch).isEmpty();
    }

    private Customer createTestCustomer() {
        Customer customer = new Customer();
        customer.setEyeCatcher("CUST");
        customer.setSortCode("123456");
        customer.setCustomerNumber(12345678L);
        customer.setName("John Doe");
        customer.setAddress("123 Main St");
        customer.setDateOfBirth(LocalDate.of(1990, 5, 15));
        customer.setCreditScore(750);
        return customer;
    }
}
