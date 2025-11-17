package com.cbsa.migration.repository.jdbc;

import com.cbsa.migration.model.Customer;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.JdbcTest;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;

import java.time.LocalDate;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Security tests for JdbcCustomerRepository.
 * Tests SQL injection protection in LIKE queries.
 */
@JdbcTest
@ActiveProfiles("test")
@Import(JdbcCustomerRepository.class)
class JdbcCustomerRepositorySecurityTest {
    
    @Autowired
    private JdbcCustomerRepository customerRepository;
    
    @Autowired
    private JdbcTemplate jdbcTemplate;
    
    @Test
    void testFindByNameContaining_SQLInjectionProtection() {
        Customer customer1 = new Customer();
        customer1.setEyeCatcher("CUST");
        customer1.setSortCode("987654");
        customer1.setCustomerNumber(1L);
        customer1.setName("John Doe");
        customer1.setAddress("123 Main St");
        customer1.setDateOfBirth(LocalDate.of(1980, 1, 1));
        customer1.setCreditScore(700);
        customerRepository.save(customer1);
        
        Customer customer2 = new Customer();
        customer2.setEyeCatcher("CUST");
        customer2.setSortCode("987654");
        customer2.setCustomerNumber(2L);
        customer2.setName("Jane Smith");
        customer2.setAddress("456 Oak Ave");
        customer2.setDateOfBirth(LocalDate.of(1985, 5, 15));
        customer2.setCreditScore(750);
        customerRepository.save(customer2);
        
        List<Customer> results = customerRepository.findByNameContaining("John");
        assertEquals(1, results.size());
        assertEquals("John Doe", results.get(0).getName());
        
        results = customerRepository.findByNameContaining("Jane");
        assertEquals(1, results.size());
        assertEquals("Jane Smith", results.get(0).getName());
        
        results = customerRepository.findByNameContaining("%");
        assertEquals(0, results.size());
        
        results = customerRepository.findByNameContaining("_");
        assertEquals(0, results.size());
        
        results = customerRepository.findByNameContaining("' OR '1'='1");
        assertEquals(0, results.size());
        
        results = customerRepository.findByNameContaining("'; DROP TABLE customer; --");
        assertEquals(0, results.size());
    }
    
    @Test
    void testFindByNameContaining_NullInput() {
        Customer customer = new Customer();
        customer.setEyeCatcher("CUST");
        customer.setSortCode("987654");
        customer.setCustomerNumber(1L);
        customer.setName("Test User");
        customer.setAddress("123 Test St");
        customer.setDateOfBirth(LocalDate.of(1990, 1, 1));
        customer.setCreditScore(650);
        customerRepository.save(customer);
        
        List<Customer> results = customerRepository.findByNameContaining(null);
        assertEquals(1, results.size());
    }
    
    @Test
    void testFindByNameContaining_WildcardEscaping() {
        Customer customer1 = new Customer();
        customer1.setEyeCatcher("CUST");
        customer1.setSortCode("987654");
        customer1.setCustomerNumber(1L);
        customer1.setName("Test_User");
        customer1.setAddress("123 Test St");
        customer1.setDateOfBirth(LocalDate.of(1990, 1, 1));
        customer1.setCreditScore(650);
        customerRepository.save(customer1);
        
        Customer customer2 = new Customer();
        customer2.setEyeCatcher("CUST");
        customer2.setSortCode("987654");
        customer2.setCustomerNumber(2L);
        customer2.setName("TestXUser");
        customer2.setAddress("456 Test Ave");
        customer2.setDateOfBirth(LocalDate.of(1991, 2, 2));
        customer2.setCreditScore(700);
        customerRepository.save(customer2);
        
        List<Customer> results = customerRepository.findByNameContaining("_");
        assertEquals(1, results.size());
        assertEquals("Test_User", results.get(0).getName());
        
        results = customerRepository.findByNameContaining("Test_User");
        assertEquals(1, results.size());
        assertEquals("Test_User", results.get(0).getName());
        
        results = customerRepository.findByNameContaining("TestXUser");
        assertEquals(1, results.size());
        assertEquals("TestXUser", results.get(0).getName());
    }
}
