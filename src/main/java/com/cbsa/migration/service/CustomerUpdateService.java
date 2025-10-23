package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerUpdateRequestDto;
import com.cbsa.migration.dto.CustomerUpdateResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

@Service
public class CustomerUpdateService {

    private static final Logger logger = LoggerFactory.getLogger(CustomerUpdateService.class);
    
    private static final List<String> VALID_TITLES = Arrays.asList(
        "Professor", "Mr", "Mrs", "Miss", "Ms", "Dr", "Drs", "Lord", "Sir", "Lady", ""
    );

    private final CustomerRepository customerRepository;

    public CustomerUpdateService(CustomerRepository customerRepository) {
        this.customerRepository = customerRepository;
    }

    public CustomerUpdateResponseDto updateCustomer(CustomerUpdateRequestDto request) {
        logger.info("Processing customer update request for customer: {}-{}", 
                   request.getSortCode(), request.getCustomerNumber());
        
        try {
            String titleValidationResult = validateTitle(request.getName());
            if (titleValidationResult != null) {
                return buildErrorResponse(request, "T", titleValidationResult);
            }
            
            String nameAddressValidationResult = validateNameAndAddress(
                request.getName(), request.getAddress());
            if (nameAddressValidationResult != null) {
                return buildErrorResponse(request, "4", nameAddressValidationResult);
            }
            
            Optional<Customer> customerOpt = customerRepository.findById(
                request.getSortCode(), request.getCustomerNumber());
            
            if (customerOpt.isEmpty()) {
                return buildErrorResponse(request, "1", "Customer not found");
            }
            
            Customer customer = customerOpt.get();
            
            updateCustomerFields(customer, request.getName(), request.getAddress());
            
            Customer updatedCustomer = customerRepository.save(customer);
            
            logger.info("Successfully updated customer {}-{}", 
                       request.getSortCode(), request.getCustomerNumber());
            
            return buildSuccessResponse(updatedCustomer);
            
        } catch (Exception e) {
            logger.error("Error updating customer {}-{}: {}", 
                        request.getSortCode(), request.getCustomerNumber(), 
                        e.getMessage(), e);
            
            String failCode = "2";
            String errorMessage = "Database error: " + e.getMessage();
            
            if (e.getMessage() != null && 
                (e.getMessage().contains("save") || e.getMessage().contains("update"))) {
                failCode = "3";
                errorMessage = "Failed to save customer: " + e.getMessage();
            }
            
            return buildErrorResponse(request, failCode, errorMessage);
        }
    }

    private String validateTitle(String name) {
        if (name == null || name.trim().isEmpty()) {
            return null;
        }
        
        String title = name.trim().split("\\s+")[0];
        
        if (!VALID_TITLES.contains(title) && !title.isEmpty()) {
            logger.warn("Invalid title detected: {}", title);
            return "Invalid title. Must be one of: Professor, Mr, Mrs, Miss, Ms, Dr, Drs, Lord, Sir, Lady";
        }
        
        return null;
    }

    private String validateNameAndAddress(String name, String address) {
        boolean nameEmpty = name == null || name.isEmpty() || name.startsWith(" ");
        boolean addressEmpty = address == null || address.isEmpty() || address.startsWith(" ");
        
        if (nameEmpty && addressEmpty) {
            logger.warn("Both name and address are empty or start with space");
            return "At least one of name or address must be provided";
        }
        
        return null;
    }

    private void updateCustomerFields(Customer customer, String name, String address) {
        boolean nameEmpty = name == null || name.isEmpty() || name.startsWith(" ");
        boolean addressEmpty = address == null || address.isEmpty() || address.startsWith(" ");
        
        if (nameEmpty && !addressEmpty) {
            customer.setAddress(address);
            logger.debug("Updated only address for customer {}-{}", 
                        customer.getSortCode(), customer.getCustomerNumber());
        }
        else if (addressEmpty && !nameEmpty) {
            customer.setName(name);
            logger.debug("Updated only name for customer {}-{}", 
                        customer.getSortCode(), customer.getCustomerNumber());
        }
        else if (!nameEmpty && !addressEmpty) {
            customer.setName(name);
            customer.setAddress(address);
            logger.debug("Updated both name and address for customer {}-{}", 
                        customer.getSortCode(), customer.getCustomerNumber());
        }
    }

    private CustomerUpdateResponseDto buildErrorResponse(
            CustomerUpdateRequestDto request, String failCode, String errorMessage) {
        
        return CustomerUpdateResponseDto.builder()
            .success(false)
            .failCode(failCode)
            .errorMessage(errorMessage)
            .sortCode(request.getSortCode())
            .customerNumber(request.getCustomerNumber())
            .build();
    }

    private CustomerUpdateResponseDto buildSuccessResponse(Customer customer) {
        return CustomerUpdateResponseDto.builder()
            .success(true)
            .failCode(null)
            .errorMessage(null)
            .eyeCatcher(customer.getEyeCatcher())
            .sortCode(customer.getSortCode())
            .customerNumber(customer.getCustomerNumber())
            .name(customer.getName())
            .address(customer.getAddress())
            .dateOfBirth(customer.getDateOfBirth())
            .creditScore(customer.getCreditScore())
            .creditScoreReviewDate(customer.getCreditScoreReviewDate())
            .build();
    }
}
