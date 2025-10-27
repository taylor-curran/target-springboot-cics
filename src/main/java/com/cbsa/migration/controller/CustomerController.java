package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerRequestDto;
import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.service.CustomerService;
import com.cbsa.migration.service.SortCodeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import java.util.Optional;

@RestController
@RequestMapping("/api/customers")
public class CustomerController {
    
    private final CustomerService customerService;
    private final SortCodeService sortCodeService;
    
    @Autowired
    public CustomerController(CustomerService customerService, SortCodeService sortCodeService) {
        this.customerService = customerService;
        this.sortCodeService = sortCodeService;
    }
    
    @GetMapping("/{customerNumber}")
    public ResponseEntity<CustomerResponseDto> getCustomer(@PathVariable Long customerNumber) {
        String sortCode = sortCodeService.getSortCode();
        Optional<Customer> customer = customerService.inquireCustomer(sortCode, customerNumber);
        
        if (customer.isEmpty()) {
            return ResponseEntity.notFound().build();
        }
        
        CustomerResponseDto response = mapToResponseDto(customer.get());
        return ResponseEntity.ok(response);
    }
    
    @PostMapping
    public ResponseEntity<CustomerResponseDto> createCustomer(@RequestBody @Valid CustomerRequestDto request) {
        Customer customer = customerService.createCustomer(request);
        CustomerResponseDto response = mapToResponseDto(customer);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
    
    @PutMapping("/{customerNumber}")
    public ResponseEntity<CustomerResponseDto> updateCustomer(
            @PathVariable Long customerNumber,
            @RequestBody CustomerRequestDto request) {
        String sortCode = sortCodeService.getSortCode();
        Customer customer = customerService.updateCustomer(
            sortCode, customerNumber, request.getName(), request.getAddress());
        CustomerResponseDto response = mapToResponseDto(customer);
        return ResponseEntity.ok(response);
    }
    
    @DeleteMapping("/{customerNumber}")
    public ResponseEntity<Void> deleteCustomer(@PathVariable Long customerNumber) {
        String sortCode = sortCodeService.getSortCode();
        customerService.deleteCustomer(sortCode, customerNumber);
        return ResponseEntity.noContent().build();
    }
    
    private CustomerResponseDto mapToResponseDto(Customer customer) {
        return CustomerResponseDto.builder()
            .name(customer.getName())
            .address(customer.getAddress())
            .dateOfBirth(customer.getDateOfBirth())
            .customerNumber(customer.getCustomerNumber())
            .sortCode(customer.getSortCode())
            .status("ACTIVE")
            .build();
    }
}
