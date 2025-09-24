package com.cbsa.migration.service;

import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.CustomerInquiryResponse;
import com.cbsa.migration.repository.CustomerRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Random;

/**
 * This service replicates the functionality of the INQCUST COBOL program.
 * It provides customer inquiry operations including direct lookup,
 * random customer selection, and last customer lookup.
 */
@Service
public class CustomerInquiryService {

    private final CustomerRepository customerRepository;
    private final SortCodeService sortCodeService;

    @Autowired
    public CustomerInquiryService(CustomerRepository customerRepository, SortCodeService sortCodeService) {
        this.customerRepository = customerRepository;
        this.sortCodeService = sortCodeService;
    }

    /**
     * Inquire about a customer by customer number.
     * Supports special cases:
     * - customerNumber = 0: returns a random customer
     * - customerNumber = 9999999999: returns the last (highest numbered) customer
     * - other values: direct lookup
     *
     * @param customerNumber The customer number to look up
     * @return CustomerInquiryResponse with customer data or failure information
     */
    public CustomerInquiryResponse inquireCustomer(Long customerNumber) {
        String sortCode = sortCodeService.getSortCode();
        
        try {
            if (customerNumber == 0L) {
                return handleRandomCustomer(sortCode);
            } else if (customerNumber == 9999999999L) {
                return handleLastCustomer(sortCode);
            } else {
                return handleDirectCustomerLookup(sortCode, customerNumber);
            }
        } catch (Exception e) {
            return CustomerInquiryResponse.builder()
                    .success(false)
                    .failureCode("9")
                    .build();
        }
    }

    private CustomerInquiryResponse handleDirectCustomerLookup(String sortCode, Long customerNumber) {
        Optional<Customer> customer = customerRepository.findById(sortCode, customerNumber);
        
        if (customer.isPresent()) {
            return buildSuccessResponse(customer.get());
        } else {
            return CustomerInquiryResponse.builder()
                    .success(false)
                    .failureCode("1")
                    .customerNumber(customerNumber)
                    .build();
        }
    }

    private CustomerInquiryResponse handleRandomCustomer(String sortCode) {
        List<Customer> allCustomers = customerRepository.findAll();
        
        if (allCustomers.isEmpty()) {
            return CustomerInquiryResponse.builder()
                    .success(false)
                    .failureCode("1")
                    .build();
        }

        Random random = new Random(System.currentTimeMillis());
        Customer randomCustomer = allCustomers.get(random.nextInt(allCustomers.size()));
        
        return buildSuccessResponse(randomCustomer);
    }

    private CustomerInquiryResponse handleLastCustomer(String sortCode) {
        List<Customer> allCustomers = customerRepository.findAll();
        
        if (allCustomers.isEmpty()) {
            return CustomerInquiryResponse.builder()
                    .success(false)
                    .failureCode("9")
                    .build();
        }

        Customer lastCustomer = allCustomers.stream()
                .max((c1, c2) -> c1.getCustomerNumber().compareTo(c2.getCustomerNumber()))
                .orElse(null);

        if (lastCustomer != null) {
            return buildSuccessResponse(lastCustomer);
        } else {
            return CustomerInquiryResponse.builder()
                    .success(false)
                    .failureCode("9")
                    .build();
        }
    }

    private CustomerInquiryResponse buildSuccessResponse(Customer customer) {
        return CustomerInquiryResponse.builder()
                .success(true)
                .failureCode("0")
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
