package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerInquiryRequestDto;
import com.cbsa.migration.dto.CustomerInquiryResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Integration tests for CustomerInquiryService
 * Tests with real database (H2 for tests)
 */
@SpringBootTest
@ActiveProfiles("test")
@Transactional
class CustomerInquiryServiceIntegrationTest {
    
    @Autowired
    private CustomerInquiryService service;
    
    @Autowired
    private CustomerRepository customerRepository;
    
    @Autowired
    private JdbcTemplate jdbcTemplate;
    
    @BeforeEach
    void setUp() {
        jdbcTemplate.update("DELETE FROM customer");
        
        for (int i = 1; i <= 10; i++) {
            Customer customer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("987654")
                .customerNumber((long) i)
                .name("Customer " + i)
                .address("Address " + i)
                .dateOfBirth(LocalDate.of(1980 + i, 1, 1))
                .creditScore(700 + i)
                .creditScoreReviewDate(LocalDate.of(2023, 1, i))
                .build();
            customerRepository.save(customer);
        }
    }
    
    @Test
    void inquireCustomer_StandardLookup_WithRealDatabase() {
        CustomerInquiryRequestDto request = CustomerInquiryRequestDto.builder()
            .sortCode("987654")
            .customerNumber(5L)
            .build();
        
        CustomerInquiryResponseDto response = service.inquireCustomer(request);
        
        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isEqualTo("0");
        assertThat(response.getCustomerNumber()).isEqualTo(5L);
        assertThat(response.getName()).isEqualTo("Customer 5");
        assertThat(response.getAddress()).isEqualTo("Address 5");
        assertThat(response.getCreditScore()).isEqualTo(705);
    }
    
    @Test
    void inquireCustomer_NotFound_WithRealDatabase() {
        CustomerInquiryRequestDto request = CustomerInquiryRequestDto.builder()
            .sortCode("987654")
            .customerNumber(999L)
            .build();
        
        CustomerInquiryResponseDto response = service.inquireCustomer(request);
        
        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("1");
        assertThat(response.getCustomerNumber()).isEqualTo(999L);
    }
    
    @Test
    void inquireCustomer_LastCustomer_WithRealDatabase() {
        CustomerInquiryRequestDto request = CustomerInquiryRequestDto.builder()
            .sortCode("987654")
            .customerNumber(9999999999L)
            .build();
        
        CustomerInquiryResponseDto response = service.inquireCustomer(request);
        
        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isEqualTo("0");
        assertThat(response.getCustomerNumber()).isEqualTo(10L);
        assertThat(response.getName()).isEqualTo("Customer 10");
    }
    
    @Test
    void inquireCustomer_RandomCustomer_WithRealDatabase() {
        CustomerInquiryRequestDto request = CustomerInquiryRequestDto.builder()
            .sortCode("987654")
            .customerNumber(0L)
            .build();
        
        CustomerInquiryResponseDto response = service.inquireCustomer(request);
        
        assertThat(response.isSuccess()).isTrue();
        assertThat(response.getFailCode()).isEqualTo("0");
        assertThat(response.getCustomerNumber()).isBetween(1L, 10L);
        assertThat(response.getName()).startsWith("Customer ");
    }
    
    @Test
    void inquireCustomer_RandomCustomer_NoCustomersExist() {
        jdbcTemplate.update("DELETE FROM customer");
        
        CustomerInquiryRequestDto request = CustomerInquiryRequestDto.builder()
            .sortCode("987654")
            .customerNumber(0L)
            .build();
        
        CustomerInquiryResponseDto response = service.inquireCustomer(request);
        
        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("9");
    }
    
    @Test
    void inquireCustomer_LastCustomer_NoCustomersExist() {
        jdbcTemplate.update("DELETE FROM customer");
        
        CustomerInquiryRequestDto request = CustomerInquiryRequestDto.builder()
            .sortCode("987654")
            .customerNumber(9999999999L)
            .build();
        
        CustomerInquiryResponseDto response = service.inquireCustomer(request);
        
        assertThat(response.isSuccess()).isFalse();
        assertThat(response.getFailCode()).isEqualTo("9");
    }
}
