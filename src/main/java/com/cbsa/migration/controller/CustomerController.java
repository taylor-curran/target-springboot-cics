package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerDTO;
import com.cbsa.migration.service.CustomerService;
import com.cbsa.migration.service.CustomerService.InquiryResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * REST Controller for customer inquiry operations.
 * Migrated from COBOL program INQCUST.cbl.
 *
 * Endpoints:
 *   GET /api/customers/{sortCode}/{customerNumber} - single customer lookup
 *   GET /api/customers                             - list/search customers
 *
 * The COBOL program reads from a VSAM CUSTOMER file using a composite key
 * (sort_code + customer_number). This controller preserves the same lookup
 * semantics while exposing them as REST endpoints.
 *
 * Special customer numbers (from COBOL):
 *   0          - return a random customer
 *   9999999999 - return the last (highest-numbered) customer
 *
 * HTTP status codes map to COBOL response codes:
 *   200 OK        - INQCUST-INQ-SUCCESS = 'Y', FAIL-CD = '0'
 *   404 Not Found - INQCUST-INQ-SUCCESS = 'N', FAIL-CD = '1'
 *   500 Error     - INQCUST-INQ-SUCCESS = 'N', FAIL-CD = '2' (system error)
 */
@RestController
@RequestMapping("/api/customers")
public class CustomerController {

    private static final Logger logger = LoggerFactory.getLogger(CustomerController.class);

    private final CustomerService customerService;

    public CustomerController(CustomerService customerService) {
        this.customerService = customerService;
    }

    /**
     * Look up a single customer by composite key (sortCode + customerNumber).
     * Corresponds to the INQCUST COBOL program main flow.
     *
     * Special values for customerNumber:
     *   0          - returns a random customer (COBOL: GENERATE-RANDOM-CUSTOMER)
     *   9999999999 - returns the last customer (COBOL: GET-LAST-CUSTOMER-VSAM)
     *
     * @param sortCode       the bank sort code (6-digit)
     * @param customerNumber the customer number
     * @return CustomerDTO or error response with appropriate HTTP status
     */
    @GetMapping("/{sortCode}/{customerNumber}")
    public ResponseEntity<?> getCustomer(
            @PathVariable String sortCode,
            @PathVariable long customerNumber) {

        logger.info("GET /api/customers/{}/{}", sortCode, customerNumber);

        InquiryResult result = customerService.inquireCustomer(sortCode, customerNumber);

        if (result.isSuccess()) {
            return ResponseEntity.ok(result.getCustomer());
        }

        String failCode = result.getFailCode();
        if (CustomerService.FAIL_CODE_NOT_FOUND.equals(failCode)) {
            Map<String, Object> errorBody = new LinkedHashMap<>();
            errorBody.put("success", false);
            errorBody.put("failCode", failCode);
            errorBody.put("message", "Customer not found");
            errorBody.put("sortCode", sortCode);
            errorBody.put("customerNumber", customerNumber);
            return ResponseEntity.status(404).body(errorBody);
        }

        // FAIL_CODE_SYSTEM_ERROR or any other failure
        Map<String, Object> errorBody = new LinkedHashMap<>();
        errorBody.put("success", false);
        errorBody.put("failCode", failCode);
        errorBody.put("message", "Internal error during customer inquiry");
        return ResponseEntity.status(500).body(errorBody);
    }

    /**
     * List or search customers.
     * If a 'name' query parameter is provided, search by name (partial match).
     * Otherwise, return all customers.
     *
     * @param name optional name filter for search
     * @return list of CustomerDTOs
     */
    @GetMapping
    public ResponseEntity<List<CustomerDTO>> listCustomers(
            @RequestParam(required = false) String name) {

        logger.info("GET /api/customers (name={})", name);

        List<CustomerDTO> customers;
        if (name != null && !name.trim().isEmpty()) {
            customers = customerService.searchCustomersByName(name.trim());
        } else {
            customers = customerService.listCustomers();
        }

        return ResponseEntity.ok(customers);
    }

    /**
     * Get the total count of customers.
     *
     * @return count wrapped in a simple JSON object
     */
    @GetMapping("/count")
    public ResponseEntity<Map<String, Integer>> getCustomerCount() {
        int count = customerService.getCustomerCount();
        return ResponseEntity.ok(Collections.singletonMap("count", count));
    }
}
