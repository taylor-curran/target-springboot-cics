package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerRequestDto;
import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.service.CustomerCreationException;
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

import javax.validation.Valid;
import java.util.List;

/**
 * Customer Controller - REST API for customer read and create operations.
 * Migrated from COBOL programs INQCUST (migrate_001) and CRECUST (migrate_002).
 */
@RestController
@RequestMapping("/api/customers")
@Validated
@Tag(name = "Customer", description = "Customer read and create operations")
public class CustomerController {

    private static final Logger logger = LoggerFactory.getLogger(CustomerController.class);

    private final CustomerService customerService;

    public CustomerController(CustomerService customerService) {
        this.customerService = customerService;
    }

    @PostMapping
    @Operation(
        summary = "Create a new customer",
        description = "Creates a new customer with async credit check and PROCTRAN audit logging. " +
                     "Equivalent to COBOL CRECUST program functionality."
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "201", description = "Customer created successfully"),
        @ApiResponse(responseCode = "400", description = "Invalid request data or DOB validation failure"),
        @ApiResponse(responseCode = "500", description = "Internal processing error"),
        @ApiResponse(responseCode = "503", description = "Named counter lock failure")
    })
    public ResponseEntity<CustomerResponseDto> createCustomer(
            @Parameter(description = "Customer creation request", required = true)
            @Valid @RequestBody CustomerRequestDto request) {

        logger.info("Received customer creation request for sort code {}", request.getSortCode());

        try {
            CustomerResponseDto response = customerService.createCustomer(request);
            return ResponseEntity.status(HttpStatus.CREATED).body(response);
        } catch (CustomerCreationException e) {
            logger.warn("Customer creation failed with code {}: {}", e.getFailCode(), e.getMessage());
            HttpStatus status = mapFailCodeToHttpStatus(e.getFailCode());
            CustomerResponseDto errorResponse = CustomerResponseDto.builder()
                    .failCode(e.getFailCode())
                    .success(false)
                    .build();
            return ResponseEntity.status(status).body(errorResponse);
        } catch (Exception e) {
            logger.error("Unexpected error during customer creation: {}", e.getMessage(), e);
            CustomerResponseDto errorResponse = CustomerResponseDto.builder()
                    .failCode("X")
                    .success(false)
                    .build();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorResponse);
        }
    }

    @GetMapping("/{sortCode}/{customerNumber}")
    @Operation(
        summary = "Get customer by sort code and customer number",
        description = "Retrieves customer information. Equivalent to COBOL INQCUST program functionality."
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Customer found"),
        @ApiResponse(responseCode = "404", description = "Customer not found")
    })
    public ResponseEntity<CustomerResponseDto> getCustomer(
            @Parameter(description = "Bank sort code", required = true)
            @PathVariable String sortCode,
            @Parameter(description = "Customer number", required = true)
            @PathVariable Long customerNumber) {

        logger.info("Received customer inquiry for {}-{}", sortCode, customerNumber);

        try {
            CustomerResponseDto response = customerService.getCustomer(sortCode, customerNumber);
            return ResponseEntity.ok(response);
        } catch (CustomerService.CustomerNotFoundException e) {
            logger.info("Customer not found: {}-{}", sortCode, customerNumber);
            return ResponseEntity.notFound().build();
        }
    }

    @GetMapping
    @Operation(
        summary = "Search customers by name",
        description = "Searches for customers whose name contains the given string."
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Search results returned")
    })
    public ResponseEntity<List<CustomerResponseDto>> getCustomerByName(
            @Parameter(description = "Customer name to search for", required = true)
            @RequestParam String name) {

        logger.info("Searching customers by name: {}", name);
        List<CustomerResponseDto> results = customerService.getCustomerByName(name);
        return ResponseEntity.ok(results);
    }

    private HttpStatus mapFailCodeToHttpStatus(String failCode) {
        if (failCode == null) {
            return HttpStatus.INTERNAL_SERVER_ERROR;
        }
        switch (failCode) {
            case "O":
            case "Y":
            case "Z":
                return HttpStatus.BAD_REQUEST;
            case "3":
                return HttpStatus.SERVICE_UNAVAILABLE;
            case "1":
            case "4":
            case "5":
            default:
                return HttpStatus.INTERNAL_SERVER_ERROR;
        }
    }
}
