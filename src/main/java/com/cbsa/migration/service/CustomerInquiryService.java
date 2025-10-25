package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.dto.mapper.DtoMapper;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.Optional;

/**
 * Customer Inquiry Service
 * Migrated from COBOL program INQCUST
 * 
 * Retrieves customer information by composite key (sortCode + customerNumber).
 * Returns customer data if found, Optional.empty() if not found (equivalent to
 * COBOL returning low values for missing customers).
 */
@Service
public class CustomerInquiryService {

    private static final Logger logger = LoggerFactory.getLogger(CustomerInquiryService.class);

    private final CustomerRepository customerRepository;
    private final DtoMapper dtoMapper;

    public CustomerInquiryService(CustomerRepository customerRepository, DtoMapper dtoMapper) {
        this.customerRepository = customerRepository;
        this.dtoMapper = dtoMapper;
    }

    /**
     * Inquire customer by composite key
     * Equivalent to COBOL INQCUST main processing logic
     * 
     * @param sortCode the sort code (branch identifier)
     * @param customerNumber the customer number
     * @return Optional containing CustomerResponseDto if found, Optional.empty() if not found
     */
    public Optional<CustomerResponseDto> inquireCustomer(String sortCode, Long customerNumber) {
        logger.info("Inquiring customer: sortCode={}, customerNumber={}", sortCode, customerNumber);
        
        try {
            Optional<Customer> customerOpt = customerRepository.findById(sortCode, customerNumber);
            
            if (customerOpt.isEmpty()) {
                logger.info("Customer not found: sortCode={}, customerNumber={}", sortCode, customerNumber);
                return Optional.empty();
            }

            Customer customer = customerOpt.get();
            CustomerResponseDto responseDto = dtoMapper.toCustomerResponseDto(customer);
            
            logger.info("Customer inquiry successful: sortCode={}, customerNumber={}, name={}", 
                       sortCode, customerNumber, customer.getName());
            
            return Optional.of(responseDto);

        } catch (Exception e) {
            logger.error("Error inquiring customer: sortCode={}, customerNumber={}, error={}", 
                        sortCode, customerNumber, e.getMessage(), e);
            throw new RuntimeException("Failed to inquire customer: " + e.getMessage(), e);
        }
    }
}
