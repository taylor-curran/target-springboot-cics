package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerInquiryResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.Random;

/**
 * Service implementing the INQCUST COBOL program business logic.
 * Handles customer inquiry operations with three lookup scenarios:
 * 1. Regular customer lookup - uses provided customer number
 * 2. Random customer (customer number = 0) - generates random customer number
 * 3. Last customer (customer number = 9999999999) - retrieves highest customer number
 */
@Service
public class CustomerService {

    private static final Logger logger = LoggerFactory.getLogger(CustomerService.class);

    private static final long RANDOM_CUSTOMER_NUMBER = 0L;
    private static final long LAST_CUSTOMER_NUMBER = 9999999999L;
    private static final int MAX_RANDOM_RETRIES = 1000;

    private static final String FAIL_CODE_SUCCESS = "0";
    private static final String FAIL_CODE_NOT_FOUND = "1";
    private static final String FAIL_CODE_SYSTEM_ERROR = "9";

    private final CustomerRepository customerRepository;
    private final SortCodeService sortCodeService;

    public CustomerService(CustomerRepository customerRepository, SortCodeService sortCodeService) {
        this.customerRepository = customerRepository;
        this.sortCodeService = sortCodeService;
    }

    /**
     * Inquire customer information based on customer number.
     * Implements the INQCUST COBOL program logic.
     *
     * @param customerNumber the customer number to look up:
     *                       - 0: generate random customer
     *                       - 9999999999: get last (highest) customer
     *                       - other: regular lookup
     * @return CustomerInquiryResponseDto with customer data or error information
     */
    public CustomerInquiryResponseDto inquireCustomer(Long customerNumber) {
        return inquireCustomer(sortCodeService.getSortCode(), customerNumber);
    }

    /**
     * Inquire customer information based on sort code and customer number.
     * Implements the INQCUST COBOL program logic.
     *
     * @param sortCode the sort code for the customer's branch
     * @param customerNumber the customer number to look up:
     *                       - 0: generate random customer
     *                       - 9999999999: get last (highest) customer
     *                       - other: regular lookup
     * @return CustomerInquiryResponseDto with customer data or error information
     */
    public CustomerInquiryResponseDto inquireCustomer(String sortCode, Long customerNumber) {
        logger.debug("Inquiring customer with sortCode={}, customerNumber={}", sortCode, customerNumber);

        try {
            if (customerNumber == RANDOM_CUSTOMER_NUMBER) {
                return inquireRandomCustomer(sortCode, customerNumber);
            } else if (customerNumber == LAST_CUSTOMER_NUMBER) {
                return inquireLastCustomer(sortCode);
            } else {
                return inquireRegularCustomer(sortCode, customerNumber);
            }
        } catch (DataAccessException e) {
            logger.error("Database error during customer inquiry: {}", e.getMessage(), e);
            return buildErrorResponse(sortCode, customerNumber, FAIL_CODE_SYSTEM_ERROR,
                    "A system error occurred while accessing customer data. Please try again later or contact support if the problem persists.");
        }
    }

    /**
     * Regular customer lookup using the provided customer number.
     */
    private CustomerInquiryResponseDto inquireRegularCustomer(String sortCode, Long customerNumber) {
        logger.debug("Performing regular customer lookup for customerNumber={}", customerNumber);

        Optional<Customer> customerOpt = customerRepository.findById(sortCode, customerNumber);

        if (customerOpt.isPresent()) {
            return buildSuccessResponse(customerOpt.get());
        } else {
            logger.debug("Customer not found: sortCode={}, customerNumber={}", sortCode, customerNumber);
            return buildErrorResponse(sortCode, customerNumber, FAIL_CODE_NOT_FOUND,
                    "Customer not found with the provided customer number.");
        }
    }

    /**
     * Random customer lookup - generates a random customer number within the valid range.
     * Implements the COBOL random customer generation algorithm with up to 1000 retries.
     * 
     * COBOL algorithm (line 698):
     * COMPUTE RANDOM-CUSTOMER = ((NCS-CUST-NO-VALUE - 1) * FUNCTION RANDOM(EIBTASKN)) + 1
     */
    private CustomerInquiryResponseDto inquireRandomCustomer(String sortCode, Long originalCustomerNumber) {
        logger.debug("Performing random customer lookup");

        Optional<Long> maxCustomerNumberOpt = customerRepository.findMaxCustomerNumber(sortCode);

        if (maxCustomerNumberOpt.isEmpty() || maxCustomerNumberOpt.get() == 0) {
            logger.debug("No customers exist for random selection");
            return buildErrorResponse(sortCode, originalCustomerNumber, FAIL_CODE_NOT_FOUND,
                    "No customers available for random selection.");
        }

        long maxCustomerNumber = maxCustomerNumberOpt.get();
        Random random = new Random(System.nanoTime());

        for (int retry = 0; retry < MAX_RANDOM_RETRIES; retry++) {
            long randomCustomerNumber = generateRandomCustomerNumber(maxCustomerNumber, random);
            logger.debug("Random customer attempt {}: trying customerNumber={}", retry + 1, randomCustomerNumber);

            Optional<Customer> customerOpt = customerRepository.findById(sortCode, randomCustomerNumber);

            if (customerOpt.isPresent()) {
                logger.debug("Random customer found on attempt {}", retry + 1);
                return buildSuccessResponse(customerOpt.get());
            }
        }

        logger.warn("Failed to find random customer after {} attempts", MAX_RANDOM_RETRIES);
        return buildErrorResponse(sortCode, originalCustomerNumber, FAIL_CODE_NOT_FOUND,
                "Unable to find a valid random customer after maximum retry attempts.");
    }

    /**
     * Last customer lookup - retrieves the customer with the highest customer number.
     * Implements the COBOL GET-LAST-CUSTOMER-VSAM section logic.
     */
    private CustomerInquiryResponseDto inquireLastCustomer(String sortCode) {
        logger.debug("Performing last customer lookup");

        Optional<Long> maxCustomerNumberOpt = customerRepository.findMaxCustomerNumber(sortCode);

        if (maxCustomerNumberOpt.isEmpty() || maxCustomerNumberOpt.get() == 0) {
            logger.debug("No customers exist for last customer lookup");
            return buildErrorResponse(sortCode, LAST_CUSTOMER_NUMBER, FAIL_CODE_SYSTEM_ERROR,
                    "Unable to determine the last customer number. No customers exist in the system.");
        }

        long lastCustomerNumber = maxCustomerNumberOpt.get();
        Optional<Customer> customerOpt = customerRepository.findById(sortCode, lastCustomerNumber);

        if (customerOpt.isPresent()) {
            return buildSuccessResponse(customerOpt.get());
        } else {
            logger.error("Inconsistent state: max customer number {} exists but customer not found", lastCustomerNumber);
            return buildErrorResponse(sortCode, LAST_CUSTOMER_NUMBER, FAIL_CODE_SYSTEM_ERROR,
                    "A system error occurred while retrieving the last customer. Please contact support.");
        }
    }

    /**
     * Generates a random customer number using the COBOL algorithm.
     * Formula: ((maxCustomerNumber - 1) * random) + 1
     * This ensures the result is between 1 and maxCustomerNumber (inclusive).
     *
     * @param maxCustomerNumber the highest customer number in use
     * @param random the random number generator
     * @return a random customer number between 1 and maxCustomerNumber
     */
    long generateRandomCustomerNumber(long maxCustomerNumber, Random random) {
        if (maxCustomerNumber <= 1) {
            return 1L;
        }
        return (long) ((maxCustomerNumber - 1) * random.nextDouble()) + 1;
    }

    /**
     * Builds a successful response DTO from a Customer entity.
     * Maps fields according to the COBOL INQCUST commarea structure.
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
                .failureCode(FAIL_CODE_SUCCESS)
                .build();
    }

    /**
     * Builds an error response DTO with the specified failure information.
     */
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
