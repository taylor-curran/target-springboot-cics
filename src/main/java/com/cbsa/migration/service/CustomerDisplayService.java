package com.cbsa.migration.service;

import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.dto.AccountSummaryDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * This service replicates the functionality of the BNK1DCS COBOL program.
 * It handles customer information display and inquiry operations.
 * Maps to INQCUST.cpy structure for customer data retrieval.
 */
@Service
public class CustomerDisplayService {

    private final CustomerRepository customerRepository;
    private final AccountRepository accountRepository;

    @Autowired
    public CustomerDisplayService(CustomerRepository customerRepository, AccountRepository accountRepository) {
        this.customerRepository = customerRepository;
        this.accountRepository = accountRepository;
    }

    /**
     * Displays customer information by sort code and customer number.
     * Replicates BNK1DCS COBOL program logic:
     * - Retrieves customer data from database
     * - Optionally includes account summary information
     * - Returns success/failure status
     *
     * @param sortCode the sort code
     * @param customerNumber the customer number
     * @param includeAccounts whether to include account summary
     * @return CustomerResponseDto with customer information, or null if not found
     */
    public CustomerResponseDto displayCustomer(String sortCode, Long customerNumber, boolean includeAccounts) {
        try {
            Optional<Customer> customerOpt = customerRepository.findById(sortCode, customerNumber);
            
            if (!customerOpt.isPresent()) {
                return null;
            }
            
            Customer customer = customerOpt.get();
            
            CustomerResponseDto.CustomerResponseDtoBuilder responseBuilder = CustomerResponseDto.builder()
                .name(customer.getName())
                .address(customer.getAddress())
                .dateOfBirth(customer.getDateOfBirth())
                .customerNumber(customer.getCustomerNumber())
                .sortCode(customer.getSortCode())
                .status("ACTIVE");
            
            if (includeAccounts) {
                List<Account> accounts = accountRepository.findByCustomerNumber(customerNumber);
                List<AccountSummaryDto> accountSummaries = accounts.stream()
                    .map(this::mapToAccountSummary)
                    .collect(Collectors.toList());
                
                responseBuilder
                    .accounts(accountSummaries)
                    .accountCount(accounts.size());
            }
            
            return responseBuilder.build();
            
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Gets customer information without account details.
     * Simplified version for basic customer lookup.
     *
     * @param sortCode the sort code
     * @param customerNumber the customer number
     * @return the customer if found, empty otherwise
     */
    public Optional<Customer> getCustomer(String sortCode, Long customerNumber) {
        return customerRepository.findById(sortCode, customerNumber);
    }

    /**
     * Maps Account to AccountSummaryDto for customer response.
     */
    private AccountSummaryDto mapToAccountSummary(Account account) {
        return AccountSummaryDto.builder()
            .accountNumber(account.getAccountNumber())
            .accountType(account.getAccountType())
            .availableBalance(account.getAvailableBalance())
            .actualBalance(account.getActualBalance())
            .build();
    }
}
