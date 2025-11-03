package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class CustomerService {
    
    private final CustomerRepository customerRepository;
    
    @Autowired
    public CustomerService(CustomerRepository customerRepository) {
        this.customerRepository = customerRepository;
    }
    
    public Optional<CustomerResponseDto> getCustomer(String sortCode, Long customerNumber) {
        Optional<Customer> customerOpt = customerRepository.findById(sortCode, customerNumber);
        
        return customerOpt.map(this::mapToDto);
    }
    
    private CustomerResponseDto mapToDto(Customer customer) {
        return CustomerResponseDto.builder()
            .name(customer.getName())
            .address(customer.getAddress())
            .dateOfBirth(customer.getDateOfBirth())
            .customerNumber(customer.getCustomerNumber())
            .sortCode(customer.getSortCode())
            .creditScore(customer.getCreditScore())
            .creditScoreReviewDate(customer.getCreditScoreReviewDate())
            .build();
    }
}
