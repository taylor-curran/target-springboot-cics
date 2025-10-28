package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerInquiryResponseDto;
import com.cbsa.migration.model.Control;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.Random;

@Service
public class CustomerInquiryService {

    private static final Logger logger = LoggerFactory.getLogger(CustomerInquiryService.class);
    private static final Long RANDOM_CUSTOMER_INDICATOR = 0L;
    private static final Long LAST_CUSTOMER_INDICATOR = 9999999999L;
    private static final int MAX_RANDOM_RETRIES = 1000;

    private final CustomerRepository customerRepository;
    private final ControlRepository controlRepository;
    private final Random random;

    public CustomerInquiryService(CustomerRepository customerRepository, 
                                  ControlRepository controlRepository) {
        this.customerRepository = customerRepository;
        this.controlRepository = controlRepository;
        this.random = new Random();
        
        logger.info("CustomerInquiryService initialized");
    }

    public CustomerInquiryResponseDto inquireCustomer(String sortCode, Long customerNumber) {
        logger.info("Customer inquiry request: sortCode={}, customerNumber={}", sortCode, customerNumber);

        try {
            if (RANDOM_CUSTOMER_INDICATOR.equals(customerNumber)) {
                return getRandomCustomer(sortCode);
            } else if (LAST_CUSTOMER_INDICATOR.equals(customerNumber)) {
                return getLastCustomer(sortCode);
            } else {
                return getSpecificCustomer(sortCode, customerNumber);
            }
        } catch (Exception e) {
            logger.error("Unexpected error during customer inquiry: {}", e.getMessage(), e);
            return buildErrorResponse(sortCode, customerNumber, "9", 
                "System error: " + e.getMessage());
        }
    }

    private CustomerInquiryResponseDto getSpecificCustomer(String sortCode, Long customerNumber) {
        Optional<Customer> customerOpt = customerRepository.findById(sortCode, customerNumber);

        if (customerOpt.isEmpty()) {
            logger.info("Customer not found: {}-{}", sortCode, customerNumber);
            return buildNotFoundResponse(sortCode, customerNumber);
        }

        Customer customer = customerOpt.get();
        logger.info("Customer found: {}-{}", sortCode, customerNumber);
        return buildSuccessResponse(customer);
    }

    private CustomerInquiryResponseDto getRandomCustomer(String sortCode) {
        logger.info("Random customer request for sortCode: {}", sortCode);

        Long maxCustomerNumber = getMaxCustomerNumber();
        if (maxCustomerNumber == null || maxCustomerNumber == 0) {
            return buildErrorResponse(sortCode, RANDOM_CUSTOMER_INDICATOR, "9", 
                "No customers available for random selection");
        }

        for (int attempt = 0; attempt < MAX_RANDOM_RETRIES; attempt++) {
            Long randomCustomerNumber = generateRandomCustomerNumber(maxCustomerNumber);
            Optional<Customer> customerOpt = customerRepository.findById(sortCode, randomCustomerNumber);

            if (customerOpt.isPresent()) {
                logger.info("Random customer found: {}-{} (attempt {})", 
                    sortCode, randomCustomerNumber, attempt + 1);
                return buildSuccessResponse(customerOpt.get());
            }
        }

        logger.warn("Could not find random customer after {} attempts", MAX_RANDOM_RETRIES);
        return buildErrorResponse(sortCode, RANDOM_CUSTOMER_INDICATOR, "1", 
            "No customer found after random retries");
    }

    private CustomerInquiryResponseDto getLastCustomer(String sortCode) {
        logger.info("Last customer request for sortCode: {}", sortCode);

        Long lastCustomerNumber = getMaxCustomerNumber();
        if (lastCustomerNumber == null || lastCustomerNumber == 0) {
            return buildErrorResponse(sortCode, LAST_CUSTOMER_INDICATOR, "9", 
                "No last customer number available");
        }

        return getSpecificCustomer(sortCode, lastCustomerNumber);
    }

    private Long generateRandomCustomerNumber(Long maxCustomerNumber) {
        long randomValue = (long) ((maxCustomerNumber - 1) * random.nextDouble()) + 1;
        return randomValue;
    }

    private Long getMaxCustomerNumber() {
        Optional<Control> controlOpt = controlRepository.getControl();
        if (controlOpt.isEmpty()) {
            logger.warn("Control record not found, initializing...");
            Control control = controlRepository.initializeControlRecord();
            return control.getLastCustomerNumber();
        }
        return controlOpt.get().getLastCustomerNumber();
    }

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
            .failureCode("0")
            .build();
    }

    private CustomerInquiryResponseDto buildNotFoundResponse(String sortCode, Long customerNumber) {
        return CustomerInquiryResponseDto.builder()
            .sortCode(sortCode)
            .customerNumber(customerNumber)
            .success(false)
            .failureCode("1")
            .errorMessage("Customer not found")
            .build();
    }

    private CustomerInquiryResponseDto buildErrorResponse(String sortCode, Long customerNumber, 
                                                          String failureCode, String errorMessage) {
        return CustomerInquiryResponseDto.builder()
            .sortCode(sortCode)
            .customerNumber(customerNumber)
            .success(false)
            .failureCode(failureCode)
            .errorMessage(errorMessage)
            .build();
    }
}
