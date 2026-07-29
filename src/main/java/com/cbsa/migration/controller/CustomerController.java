package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerDTO;
import com.cbsa.migration.service.CustomerService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * REST Controller for customer read operations.
 * Migrated from the INQCUST.cbl COBOL program (customer inquiry by
 * composite key). Scope is read-only retrieval.
 */
@RestController
@RequestMapping("/api/customers")
@Tag(name = "Customer", description = "Customer inquiry API (migrated from INQCUST)")
public class CustomerController {

    private final CustomerService customerService;

    public CustomerController(CustomerService customerService) {
        this.customerService = customerService;
    }

    /**
     * Retrieve a single customer by composite key (sort code + customer number).
     * Corresponds to the INQCUST.cbl COBOL program.
     *
     * @param sortCode       the branch sort code
     * @param customerNumber the customer number
     * @return the customer, or 404 if no matching record exists
     */
    @GetMapping("/{sortCode}/{customerNumber}")
    @Operation(summary = "Get customer by composite key",
               description = "Retrieve a single customer by sort code and customer number. "
                       + "Equivalent to the COBOL INQCUST program.")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Customer found"),
        @ApiResponse(responseCode = "404", description = "Customer not found")
    })
    public ResponseEntity<CustomerDTO> getCustomer(
            @Parameter(description = "Branch sort code") @PathVariable String sortCode,
            @Parameter(description = "Customer number") @PathVariable Long customerNumber) {
        return customerService.getCustomer(sortCode, customerNumber)
                .map(ResponseEntity::ok)
                .orElseGet(() -> ResponseEntity.notFound().build());
    }
}
