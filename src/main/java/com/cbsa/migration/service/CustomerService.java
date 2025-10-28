package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class CustomerService {

    private static final Logger logger = LoggerFactory.getLogger(CustomerService.class);

    private final CustomerRepository customerRepository;

    public CustomerService(CustomerRepository customerRepository) {
        this.customerRepository = customerRepository;
        logger.info("CustomerService initialized");
    }

    public Optional<CustomerResponseDto> getCustomer(String sortCode, Long customerNumber) {
        logger.info("Retrieving customer: sortCode={}, customerNumber={}", sortCode, customerNumber);
        
        try {
            Optional<Customer> customerOpt = customerRepository.findById(sortCode, customerNumber);
            
            if (customerOpt.isEmpty()) {
                logger.info("Customer not found: sortCode={}, customerNumber={}", sortCode, customerNumber);
                return Optional.empty();
            }

            Customer customer = customerOpt.get();
            
            CustomerResponseDto responseDto = CustomerResponseDto.builder()
                .name(customer.getName())
                .address(customer.getAddress())
                .dateOfBirth(customer.getDateOfBirth())
                .customerNumber(customer.getCustomerNumber())
                .sortCode(customer.getSortCode())
                .status("ACTIVE")
                .build();
            
            logger.info("Customer retrieved successfully: sortCode={}, customerNumber={}, name={}", 
                       sortCode, customerNumber, customer.getName());
            
            return Optional.of(responseDto);

        } catch (Exception e) {
            logger.error("Error retrieving customer: sortCode={}, customerNumber={}: {}", 
                        sortCode, customerNumber, e.getMessage(), e);
            throw new RuntimeException("Error retrieving customer", e);
        }
    }
}
