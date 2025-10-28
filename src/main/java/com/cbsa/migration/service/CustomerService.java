package com.cbsa.migration.service;

import com.cbsa.migration.dto.InquiryCustomerResponseDto;
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
    }

    public InquiryCustomerResponseDto findCustomer(String sortCode, Long customerNumber) {
        logger.debug("Finding customer with sortCode={}, customerNumber={}", sortCode, customerNumber);

        try {
            Optional<Customer> customerOpt = customerRepository.findById(sortCode, customerNumber);

            if (customerOpt.isPresent()) {
                Customer customer = customerOpt.get();
                logger.info("Customer found: {}-{}", sortCode, customerNumber);

                return InquiryCustomerResponseDto.builder()
                    .eyeCatcher(customer.getEyeCatcher())
                    .sortCode(customer.getSortCode())
                    .customerNumber(customer.getCustomerNumber())
                    .name(customer.getName())
                    .address(customer.getAddress())
                    .dateOfBirth(customer.getDateOfBirth())
                    .creditScore(customer.getCreditScore())
                    .creditScoreReviewDate(customer.getCreditScoreReviewDate())
                    .success(true)
                    .failureCode("0")
                    .build();
            } else {
                logger.warn("Customer not found: {}-{}", sortCode, customerNumber);

                return InquiryCustomerResponseDto.builder()
                    .sortCode(sortCode)
                    .customerNumber(customerNumber)
                    .success(false)
                    .failureCode("1")
                    .errorMessage("Customer not found")
                    .build();
            }
        } catch (Exception e) {
            logger.error("Error finding customer {}-{}: {}", sortCode, customerNumber, e.getMessage(), e);

            return InquiryCustomerResponseDto.builder()
                .sortCode(sortCode)
                .customerNumber(customerNumber)
                .success(false)
                .failureCode("2")
                .errorMessage("System error: " + e.getMessage())
                .build();
        }
    }
}
