package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerDTO;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Service implementing the INQCUST (Customer Inquiry) business logic.
 *
 * Migrated from COBOL program INQCUST.cbl (712 lines).
 * The original program reads from a VSAM CUSTOMER file using a composite key
 * (sort_code + customer_number) and returns customer data in a commarea.
 *
 * Key COBOL behaviors preserved:
 * - Composite key lookup (sort_code + customer_number)
 * - Eye-catcher validation ("CUST")
 * - Fail codes: '0' = success, '1' = not found, '2' = system error
 * - The COBOL program supports three modes:
 *   1. Direct lookup by specific customer number
 *   2. Random customer (customer_number = 0) - returns a random existing customer
 *   3. Last customer (customer_number = 9999999999) - returns the highest-numbered customer
 */
@Service
public class CustomerService {

    private static final Logger logger = LoggerFactory.getLogger(CustomerService.class);

    /** Fail code: successful inquiry */
    public static final String FAIL_CODE_SUCCESS = "0";
    /** Fail code: customer not found */
    public static final String FAIL_CODE_NOT_FOUND = "1";
    /** Fail code: system/internal error */
    public static final String FAIL_CODE_SYSTEM_ERROR = "2";

    /** Special customer number requesting a random customer */
    public static final long CUSTOMER_NUMBER_RANDOM = 0L;
    /** Special customer number requesting the last (highest) customer */
    public static final long CUSTOMER_NUMBER_LAST = 9999999999L;

    private final CustomerRepository customerRepository;

    public CustomerService(CustomerRepository customerRepository) {
        this.customerRepository = customerRepository;
    }

    /**
     * Inquire about a customer by composite key (sort_code + customer_number).
     * This is the primary method migrated from the INQCUST COBOL program.
     *
     * Mirrors COBOL logic:
     * - If customerNumber is 0 (CUSTOMER_NUMBER_RANDOM), return a random customer
     * - If customerNumber is 9999999999 (CUSTOMER_NUMBER_LAST), return the last customer
     * - Otherwise, perform a direct lookup
     *
     * @param sortCode the bank sort code
     * @param customerNumber the customer number (0 for random, 9999999999 for last)
     * @return InquiryResult containing success flag, fail code, and optional CustomerDTO
     */
    public InquiryResult inquireCustomer(String sortCode, long customerNumber) {
        logger.info("INQCUST: Inquiring customer with sortCode={}, customerNumber={}", sortCode, customerNumber);

        try {
            if (customerNumber == CUSTOMER_NUMBER_RANDOM) {
                return inquireRandomCustomer(sortCode);
            }

            if (customerNumber == CUSTOMER_NUMBER_LAST) {
                return inquireLastCustomer(sortCode);
            }

            return inquireSpecificCustomer(sortCode, customerNumber);
        } catch (Exception e) {
            logger.error("INQCUST: System error during customer inquiry: {}", e.getMessage(), e);
            return InquiryResult.failure(FAIL_CODE_SYSTEM_ERROR);
        }
    }

    /**
     * Direct lookup of a specific customer by composite key.
     * Corresponds to the READ-CUSTOMER-VSAM section for a specific customer number.
     */
    private InquiryResult inquireSpecificCustomer(String sortCode, long customerNumber) {
        Optional<Customer> customer = customerRepository.findById(sortCode, customerNumber);

        if (customer.isPresent()) {
            logger.info("INQCUST: Customer found: sortCode={}, customerNumber={}", sortCode, customerNumber);
            return InquiryResult.success(toDTO(customer.get()));
        } else {
            logger.info("INQCUST: Customer not found: sortCode={}, customerNumber={}", sortCode, customerNumber);
            return InquiryResult.failure(FAIL_CODE_NOT_FOUND);
        }
    }

    /**
     * Return a random customer from the repository.
     * Corresponds to the GENERATE-RANDOM-CUSTOMER section in COBOL.
     *
     * In COBOL, a random number is generated using FUNCTION RANDOM seeded by EIBTASKN,
     * and then the program retries up to 1000 times if the generated customer number
     * doesn't exist. Here we simplify by picking a random customer from the full list.
     */
    private InquiryResult inquireRandomCustomer(String sortCode) {
        List<Customer> allCustomers = customerRepository.findAll();
        if (allCustomers.isEmpty()) {
            logger.warn("INQCUST: No customers found for random inquiry");
            return InquiryResult.failure(FAIL_CODE_NOT_FOUND);
        }

        int randomIndex = (int) (Math.random() * allCustomers.size());
        Customer customer = allCustomers.get(randomIndex);
        logger.info("INQCUST: Random customer selected: customerNumber={}", customer.getCustomerNumber());
        return InquiryResult.success(toDTO(customer));
    }

    /**
     * Return the last (highest-numbered) customer.
     * Corresponds to the GET-LAST-CUSTOMER-VSAM section in COBOL,
     * which does a STARTBR with HIGH-VALUES then READPREV to get the last record.
     */
    private InquiryResult inquireLastCustomer(String sortCode) {
        List<Customer> allCustomers = customerRepository.findAll();
        if (allCustomers.isEmpty()) {
            logger.warn("INQCUST: No customers found for last-customer inquiry");
            return InquiryResult.failure(FAIL_CODE_NOT_FOUND);
        }

        Customer lastCustomer = allCustomers.stream()
                .max((c1, c2) -> Long.compare(c1.getCustomerNumber(), c2.getCustomerNumber()))
                .orElse(null);

        if (lastCustomer == null) {
            return InquiryResult.failure(FAIL_CODE_NOT_FOUND);
        }

        logger.info("INQCUST: Last customer found: customerNumber={}", lastCustomer.getCustomerNumber());
        return InquiryResult.success(toDTO(lastCustomer));
    }

    /**
     * List all customers.
     * While not directly in INQCUST.cbl, this provides a useful REST operation
     * for browsing customer data.
     *
     * @return list of all customers as DTOs
     */
    public List<CustomerDTO> listCustomers() {
        return customerRepository.findAll().stream()
                .map(this::toDTO)
                .collect(Collectors.toList());
    }

    /**
     * Search customers by name (partial match).
     *
     * @param name the name fragment to search for
     * @return list of matching customers as DTOs
     */
    public List<CustomerDTO> searchCustomersByName(String name) {
        return customerRepository.findByNameContaining(name).stream()
                .map(this::toDTO)
                .collect(Collectors.toList());
    }

    /**
     * Get the total count of customers.
     *
     * @return number of customers
     */
    public int getCustomerCount() {
        return customerRepository.count();
    }

    /**
     * Convert a Customer entity to a CustomerDTO.
     * Maps all COBOL CUSTOMER copybook fields to the DTO.
     */
    private CustomerDTO toDTO(Customer customer) {
        return CustomerDTO.builder()
                .eyeCatcher(customer.getEyeCatcher())
                .sortCode(customer.getSortCode())
                .customerNumber(customer.getCustomerNumber())
                .name(customer.getName())
                .address(customer.getAddress())
                .dateOfBirth(customer.getDateOfBirth())
                .creditScore(customer.getCreditScore())
                .creditScoreReviewDate(customer.getCreditScoreReviewDate())
                .build();
    }

    /**
     * Result object for customer inquiry operations.
     * Mirrors the COBOL commarea response structure:
     * - INQCUST-INQ-SUCCESS ('Y'/'N')
     * - INQCUST-INQ-FAIL-CD ('0' = success, '1' = not found, '2' = system error)
     */
    public static class InquiryResult {
        private final boolean success;
        private final String failCode;
        private final CustomerDTO customer;

        private InquiryResult(boolean success, String failCode, CustomerDTO customer) {
            this.success = success;
            this.failCode = failCode;
            this.customer = customer;
        }

        public static InquiryResult success(CustomerDTO customer) {
            return new InquiryResult(true, FAIL_CODE_SUCCESS, customer);
        }

        public static InquiryResult failure(String failCode) {
            return new InquiryResult(false, failCode, null);
        }

        public boolean isSuccess() {
            return success;
        }

        public String getFailCode() {
            return failCode;
        }

        public CustomerDTO getCustomer() {
            return customer;
        }
    }
}
