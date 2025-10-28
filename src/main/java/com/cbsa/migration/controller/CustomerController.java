package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerRequestDto;
import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.service.CustomerCreationService;
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

import javax.validation.Valid;

@RestController
@RequestMapping("/api/customers")
@Validated
@Tag(name = "Customer", description = "Customer management services")
public class CustomerController {

    private static final Logger logger = LoggerFactory.getLogger(CustomerController.class);

    private final CustomerCreationService customerCreationService;

    public CustomerController(CustomerCreationService customerCreationService) {
        this.customerCreationService = customerCreationService;
    }

    @PostMapping
    @Operation(
        summary = "Create a new customer",
        description = "Creates a new customer with validation, credit check, and transaction logging. " +
                     "Equivalent to COBOL CRECUST program functionality."
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "201", description = "Customer created successfully"),
        @ApiResponse(responseCode = "400", description = "Invalid request data or validation failure"),
        @ApiResponse(responseCode = "500", description = "Internal processing error")
    })
    public ResponseEntity<CustomerResponseDto> createCustomer(
            @Parameter(description = "Customer creation request with name, address, DOB, and sort code", required = true)
            @Valid @RequestBody CustomerRequestDto request) {
        
        logger.info("Received customer creation request: name={}, sortCode={}", 
                   request.getName(), request.getSortCode());

        try {
            CustomerResponseDto response = customerCreationService.createCustomer(request);
            
            logger.info("Customer created successfully: {}-{}", 
                       response.getSortCode(), response.getCustomerNumber());
            
            return ResponseEntity.status(HttpStatus.CREATED).body(response);
            
        } catch (IllegalArgumentException e) {
            logger.warn("Customer creation validation failed: {}", e.getMessage());
            
            CustomerResponseDto errorResponse = CustomerResponseDto.builder()
                .status("VALIDATION_FAILED: " + e.getMessage())
                .build();
            
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(errorResponse);
            
        } catch (Exception e) {
            logger.error("Unexpected error creating customer: {}", e.getMessage(), e);
            
            CustomerResponseDto errorResponse = CustomerResponseDto.builder()
                .status("ERROR: " + e.getMessage())
                .build();
            
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorResponse);
        }
    }
}
