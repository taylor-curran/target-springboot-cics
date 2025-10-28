package com.cbsa.migration.controller;

import com.cbsa.migration.service.CustomerDisplayService;
import com.cbsa.migration.dto.CustomerResponseDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * REST controller for Customer operations.
 * Provides endpoints for customer display operations.
 * Migrated from BNK1DCS COBOL program.
 */
@RestController
@RequestMapping("/api/customers")
public class CustomerController {

    private final CustomerDisplayService customerDisplayService;

    @Autowired
    public CustomerController(CustomerDisplayService customerDisplayService) {
        this.customerDisplayService = customerDisplayService;
    }

    /**
     * Get customer information.
     * Migrated from BNK1DCS COBOL program.
     * 
     * @param sortCode the sort code
     * @param customerNumber the customer number
     * @param includeAccounts whether to include account summary (default: false)
     * @return customer information or not found
     */
    @GetMapping("/{sortCode}/{customerNumber}")
    public ResponseEntity<CustomerResponseDto> getCustomer(
            @PathVariable String sortCode,
            @PathVariable Long customerNumber,
            @RequestParam(defaultValue = "false") boolean includeAccounts) {
        
        CustomerResponseDto customer = customerDisplayService.displayCustomer(sortCode, customerNumber, includeAccounts);
        
        if (customer != null) {
            return ResponseEntity.ok(customer);
        } else {
            return ResponseEntity.notFound().build();
        }
    }
}
