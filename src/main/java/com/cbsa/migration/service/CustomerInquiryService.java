package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerInquiryRequestDto;
import com.cbsa.migration.dto.CustomerInquiryResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.Random;

/**
 * Customer Inquiry Service
 * Migrated from COBOL program INQCUST
 * 
 * Handles customer inquiries by customer number, including special cases:
 * - Customer number 0: Generate and return a random customer
 * - Customer number 9999999999: Return the last (highest) customer number in use
 */
@Service
public class CustomerInquiryService {
    
    private static final Logger logger = LoggerFactory.getLogger(CustomerInquiryService.class);
    
    private static final long RANDOM_CUSTOMER_NUMBER = 0L;
    private static final long LAST_CUSTOMER_NUMBER = 9999999999L;
    private static final int MAX_RANDOM_RETRIES = 1000;
    
    private final CustomerRepository customerRepository;
    
    public CustomerInquiryService(CustomerRepository customerRepository) {
        this.customerRepository = customerRepository;
        logger.info("CustomerInquiryService initialized");
    }
    
    /**
     * Process customer inquiry request
     * Equivalent to COBOL PREMIERE SECTION P010
     */
    public CustomerInquiryResponseDto inquireCustomer(CustomerInquiryRequestDto request) {
        logger.info("Processing customer inquiry for sortCode={}, customerNumber={}", 
                   request.getSortCode(), request.getCustomerNumber());
        
        try {
            Long customerNumber = request.getCustomerNumber();
            
            if (customerNumber.equals(RANDOM_CUSTOMER_NUMBER) || 
                customerNumber.equals(LAST_CUSTOMER_NUMBER)) {
                
                return handleSpecialCase(request.getSortCode(), customerNumber);
            }
            
            return lookupCustomer(request.getSortCode(), customerNumber);
            
        } catch (Exception e) {
            logger.error("Error processing customer inquiry for sortCode={}, customerNumber={}: {}", 
                        request.getSortCode(), request.getCustomerNumber(), e.getMessage(), e);
            
            return CustomerInquiryResponseDto.builder()
                .sortCode(request.getSortCode())
                .customerNumber(request.getCustomerNumber())
                .success(false)
                .failCode(CustomerInquiryResponseDto.DATABASE_ERROR_CODE)
                .build();
        }
    }
    
    /**
     * Handle special cases: random customer or last customer
     * Equivalent to COBOL READ-CUSTOMER-NCS and conditional logic at lines 190-207
     */
    private CustomerInquiryResponseDto handleSpecialCase(String sortCode, Long customerNumber) {
        Optional<Customer> lastCustomerOpt = customerRepository.findLastCustomer(sortCode);
        
        if (lastCustomerOpt.isEmpty()) {
            logger.warn("No customers found for sortCode={}", sortCode);
            return CustomerInquiryResponseDto.builder()
                .sortCode(sortCode)
                .customerNumber(customerNumber)
                .success(false)
                .failCode(CustomerInquiryResponseDto.DATABASE_ERROR_CODE)
                .build();
        }
        
        Long lastCustomerNumber = lastCustomerOpt.get().getCustomerNumber();
        logger.debug("Last customer number for sortCode={}: {}", sortCode, lastCustomerNumber);
        
        if (customerNumber.equals(LAST_CUSTOMER_NUMBER)) {
            return lookupCustomer(sortCode, lastCustomerNumber);
        }
        
        return findRandomCustomer(sortCode, lastCustomerNumber);
    }
    
    /**
     * Find a random customer by generating random customer numbers until one is found
     * Equivalent to COBOL GENERATE-RANDOM-CUSTOMER and retry logic at lines 310-322
     */
    private CustomerInquiryResponseDto findRandomCustomer(String sortCode, Long maxCustomerNumber) {
        Random random = new Random(System.currentTimeMillis());
        
        for (int retry = 0; retry < MAX_RANDOM_RETRIES; retry++) {
            long randomCustomerNumber = (long) ((maxCustomerNumber - 1) * random.nextDouble()) + 1;
            
            logger.debug("Attempt {} - trying random customer number: {}", retry + 1, randomCustomerNumber);
            
            Optional<Customer> customerOpt = customerRepository.findById(sortCode, randomCustomerNumber);
            
            if (customerOpt.isPresent()) {
                Customer customer = customerOpt.get();
                logger.info("Found random customer on attempt {}: {}-{}", 
                           retry + 1, sortCode, randomCustomerNumber);
                return buildSuccessResponse(customer);
            }
        }
        
        logger.warn("Could not find random customer after {} attempts for sortCode={}", 
                   MAX_RANDOM_RETRIES, sortCode);
        
        return CustomerInquiryResponseDto.builder()
            .sortCode(sortCode)
            .customerNumber(RANDOM_CUSTOMER_NUMBER)
            .success(false)
            .failCode(CustomerInquiryResponseDto.NOT_FOUND_CODE)
            .build();
    }
    
    /**
     * Standard customer lookup by sort code and customer number
     * Equivalent to COBOL READ-CUSTOMER-VSAM section
     */
    private CustomerInquiryResponseDto lookupCustomer(String sortCode, Long customerNumber) {
        Optional<Customer> customerOpt = customerRepository.findById(sortCode, customerNumber);
        
        if (customerOpt.isPresent()) {
            Customer customer = customerOpt.get();
            logger.info("Customer found: {}-{}", sortCode, customerNumber);
            return buildSuccessResponse(customer);
        } else {
            logger.info("Customer not found: {}-{}", sortCode, customerNumber);
            return CustomerInquiryResponseDto.builder()
                .sortCode(sortCode)
                .customerNumber(customerNumber)
                .success(false)
                .failCode(CustomerInquiryResponseDto.NOT_FOUND_CODE)
                .build();
        }
    }
    
    /**
     * Build successful response from Customer entity
     * Equivalent to COBOL lines 220-238
     */
    private CustomerInquiryResponseDto buildSuccessResponse(Customer customer) {
        return CustomerInquiryResponseDto.builder()
            .eyeCatcher(customer.getEyeCatcher())
            .sortCode(customer.getSortCode())
            .customerNumber(customer.getCustomerNumber())
            .name(customer.getName())
            .address(customer.getAddress())
            .dateOfBirth(customer.getDateOfBirth())
            .creditScore(customer.getCreditScore())
            .creditScoreReviewDate(customer.getCreditScoreReviewDate())
            .success(true)
            .failCode(CustomerInquiryResponseDto.SUCCESS_CODE)
            .build();
    }
}
