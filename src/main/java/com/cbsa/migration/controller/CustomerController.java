package com.cbsa.migration.controller;

import com.cbsa.migration.model.CustomerInquiryResponse;
import com.cbsa.migration.service.CustomerInquiryService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * REST Controller for customer operations migrated from COBOL.
 * This controller provides endpoints for functionality previously
 * implemented in INQCUST.cbl COBOL program.
 */
@RestController
@RequestMapping("/api/customer")
public class CustomerController {

    private final CustomerInquiryService customerInquiryService;

    @Autowired
    public CustomerController(CustomerInquiryService customerInquiryService) {
        this.customerInquiryService = customerInquiryService;
    }

    /**
     * Endpoint to inquire about a customer by customer number.
     * Corresponds to INQCUST.cbl COBOL program.
     * 
     * Special values:
     * - 0: returns a random customer
     * - 9999999999: returns the last (highest numbered) customer
     *
     * @param customerNumber the customer number to look up
     * @return ResponseEntity with customer information or error details
     */
    @GetMapping("/inquiry/{customerNumber}")
    public ResponseEntity<CustomerInquiryResponse> inquireCustomer(@PathVariable Long customerNumber) {
        CustomerInquiryResponse response = customerInquiryService.inquireCustomer(customerNumber);
        
        if (response.isSuccess()) {
            return ResponseEntity.ok(response);
        } else {
            return ResponseEntity.badRequest().body(response);
        }
    }
}
