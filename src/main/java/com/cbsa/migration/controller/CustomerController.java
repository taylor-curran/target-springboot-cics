package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerCreationRequestDto;
import com.cbsa.migration.dto.CustomerCreationResponseDto;
import com.cbsa.migration.service.CustomerCreationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/customers")
public class CustomerController {

    private final CustomerCreationService customerCreationService;

    @Autowired
    public CustomerController(CustomerCreationService customerCreationService) {
        this.customerCreationService = customerCreationService;
    }

    @PostMapping
    public ResponseEntity<CustomerCreationResponseDto> createCustomer(
            @Valid @RequestBody CustomerCreationRequestDto request) {
        
        CustomerCreationResponseDto response = customerCreationService.createCustomer(request);
        
        if (response.isSuccess()) {
            return ResponseEntity.status(HttpStatus.CREATED).body(response);
        } else {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }
}
