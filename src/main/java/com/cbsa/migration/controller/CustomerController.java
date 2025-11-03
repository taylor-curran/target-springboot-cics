package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.service.CustomerService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

@RestController
@RequestMapping("/api/customers")
@Tag(name = "Customer Operations", description = "Customer inquiry and management API")
public class CustomerController {
    
    private final CustomerService customerService;
    
    @Autowired
    public CustomerController(CustomerService customerService) {
        this.customerService = customerService;
    }
    
    @GetMapping("/{sortCode}/{customerNumber}")
    @Operation(summary = "Get customer by sort code and customer number", 
               description = "Retrieve customer information using composite key (sort code + customer number)")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Customer found and returned successfully"),
        @ApiResponse(responseCode = "404", description = "Customer not found")
    })
    public ResponseEntity<CustomerResponseDto> getCustomer(
            @Parameter(description = "Branch sort code (6 digits)", example = "123456")
            @PathVariable String sortCode,
            @Parameter(description = "Customer number (10 digits)", example = "1234567890")
            @PathVariable Long customerNumber) {
        
        Optional<CustomerResponseDto> customer = customerService.getCustomer(sortCode, customerNumber);
        
        return customer
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound().build());
    }
    
    @GetMapping("/health")
    @Operation(summary = "Customer service health check", 
               description = "Check if the customer service is operational")
    @ApiResponse(responseCode = "200", description = "Service is healthy")
    public ResponseEntity<String> healthCheck() {
        return ResponseEntity.ok("Customer service is operational");
    }
}
