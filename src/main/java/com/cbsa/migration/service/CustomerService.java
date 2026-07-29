package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerDTO;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.springframework.stereotype.Service;

import java.util.Optional;

/**
 * Business logic for customer read operations.
 * Migrated from the INQCUST.cbl COBOL program (read-only inquiry by
 * composite key of sort code + customer number).
 */
@Service
public class CustomerService {

    private final CustomerRepository customerRepository;

    public CustomerService(CustomerRepository customerRepository) {
        this.customerRepository = customerRepository;
    }

    /**
     * Retrieve a single customer by its composite key.
     * Equivalent to the INQCUST VSAM READ on CUSTOMER-KY (sort code +
     * customer number). Returns an empty Optional when no matching record
     * exists, mirroring INQCUST's NOTFND path (INQCUST-INQ-SUCCESS = 'N',
     * INQCUST-INQ-FAIL-CD = '1').
     *
     * @param sortCode       the branch sort code
     * @param customerNumber the customer number
     * @return the mapped customer, or empty if not found
     */
    public Optional<CustomerDTO> getCustomer(String sortCode, Long customerNumber) {
        return customerRepository.findById(sortCode, customerNumber)
                .map(this::toDto);
    }

    private CustomerDTO toDto(Customer customer) {
        return CustomerDTO.builder()
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
