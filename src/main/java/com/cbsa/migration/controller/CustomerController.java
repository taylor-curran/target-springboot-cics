package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.service.CustomerService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

@RestController
@RequestMapping("/api/customer")
@Validated
@Tag(name = "Customer", description = "Customer inquiry and retrieval services")
public class CustomerController {

    private static final Logger logger = LoggerFactory.getLogger(CustomerController.class);

    private final CustomerService customerService;

    public CustomerController(CustomerService customerService) {
        this.customerService = customerService;
    }

    @GetMapping("/{sortCode}/{customerNumber}")
    @Operation(
        summary = "Retrieve customer by composite key",
        description = "Retrieves customer information using sort code and customer number. " +
                     "Equivalent to COBOL INQCUST program functionality."
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Customer found and returned successfully"),
        @ApiResponse(responseCode = "404", description = "Customer not found"),
        @ApiResponse(responseCode = "500", description = "Internal server error")
    })
    public ResponseEntity<CustomerResponseDto> getCustomer(
            @Parameter(description = "Branch sort code (6 digits)", required = true, example = "987654")
            @PathVariable String sortCode,
            @Parameter(description = "Customer number (10 digits)", required = true, example = "1")
            @PathVariable Long customerNumber) {
        
        logger.info("Received customer inquiry request: sortCode={}, customerNumber={}", 
                   sortCode, customerNumber);

        try {
            Optional<CustomerResponseDto> customerOpt = customerService.getCustomer(sortCode, customerNumber);
            
            if (customerOpt.isPresent()) {
                CustomerResponseDto customer = customerOpt.get();
                logger.info("Customer inquiry successful: sortCode={}, customerNumber={}, name={}", 
                           sortCode, customerNumber, customer.getName());
                return ResponseEntity.ok(customer);
            } else {
                logger.info("Customer not found: sortCode={}, customerNumber={}", 
                           sortCode, customerNumber);
                return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
            }
            
        } catch (Exception e) {
            logger.error("Error processing customer inquiry: sortCode={}, customerNumber={}: {}", 
                        sortCode, customerNumber, e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }
}
