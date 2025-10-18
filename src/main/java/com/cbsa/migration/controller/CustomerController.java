package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CreateCustomerRequestDto;
import com.cbsa.migration.dto.CreateCustomerResponseDto;
import com.cbsa.migration.service.CreateCustomerService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/customers")
@Tag(name = "Customer", description = "Customer management operations")
public class CustomerController {

    private static final Logger logger = LoggerFactory.getLogger(CustomerController.class);
    
    private final CreateCustomerService createCustomerService;

    public CustomerController(CreateCustomerService createCustomerService) {
        this.createCustomerService = createCustomerService;
    }

    @PostMapping
    @Operation(summary = "Create a new customer", description = "Creates a new customer with validation and credit check")
    public ResponseEntity<CreateCustomerResponseDto> createCustomer(@Valid @RequestBody CreateCustomerRequestDto request) {
        logger.info("POST /api/customers - Creating customer: {}", request.getName());
        
        CreateCustomerResponseDto response = createCustomerService.createCustomer(request);
        
        if (response.isSuccess()) {
            logger.info("Customer created successfully: {}-{}", response.getSortCode(), response.getCustomerNumber());
            return ResponseEntity.status(HttpStatus.CREATED).body(response);
        } else {
            logger.warn("Customer creation failed with code: {}, message: {}", 
                response.getFailCode(), response.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }
}
