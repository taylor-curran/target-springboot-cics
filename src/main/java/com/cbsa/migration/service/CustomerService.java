package com.cbsa.migration.service;

import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.CustomerRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * Service class implementing customer operations migrated from COBOL programs:
 * - INQCUST: Customer inquiry
 * - DELCUS: Customer deletion (with cascade delete of accounts)
 * - UPDCUST: Customer update
 * 
 * This service replicates the business logic from BNK1DCS.cbl which is the
 * BMS 3270 interface program for customer inquiry, update, and deletion.
 */
@Service
public class CustomerService {

    private static final Logger logger = LoggerFactory.getLogger(CustomerService.class);

    private static final Set<String> VALID_TITLES = Set.of(
            "Professor", "Mr", "Mrs", "Miss", "Ms", "Dr", "Drs", "Lord", "Sir", "Lady"
    );

    private final CustomerRepository customerRepository;
    private final AccountRepository accountRepository;
    private final SortCodeService sortCodeService;

    public CustomerService(CustomerRepository customerRepository,
                          AccountRepository accountRepository,
                          SortCodeService sortCodeService) {
        this.customerRepository = customerRepository;
        this.accountRepository = accountRepository;
        this.sortCodeService = sortCodeService;
    }

    /**
     * Retrieves a customer by customer number.
     * Implements INQCUST COBOL program logic.
     * 
     * @param customerNumber the customer number to look up
     * @return Optional containing the customer if found
     * @throws CustomerNotFoundException if customer is not found
     */
    public Optional<Customer> getCustomer(Long customerNumber) {
        logger.debug("Looking up customer with number: {}", customerNumber);
        
        if (customerNumber == null || customerNumber <= 0) {
            logger.warn("Invalid customer number provided: {}", customerNumber);
            return Optional.empty();
        }
        
        String sortCode = sortCodeService.getSortCode();
        return customerRepository.findById(sortCode, customerNumber);
    }

    /**
     * Retrieves a customer by sort code and customer number.
     * Implements INQCUST COBOL program logic with explicit sort code.
     * 
     * @param sortCode the sort code
     * @param customerNumber the customer number
     * @return Optional containing the customer if found
     */
    public Optional<Customer> getCustomer(String sortCode, Long customerNumber) {
        logger.debug("Looking up customer with sort code: {} and number: {}", sortCode, customerNumber);
        
        if (customerNumber == null || customerNumber <= 0) {
            logger.warn("Invalid customer number provided: {}", customerNumber);
            return Optional.empty();
        }
        
        if (sortCode == null || sortCode.isBlank()) {
            sortCode = sortCodeService.getSortCode();
        }
        
        return customerRepository.findById(sortCode, customerNumber);
    }

    /**
     * Deletes a customer and all associated accounts.
     * Implements DELCUS COBOL program logic with cascade deletion.
     * 
     * Error codes (matching COBOL):
     * - '1': Customer not found
     * - '2': Datastore error
     * - '3': Delete operation failure
     * 
     * @param customerNumber the customer number to delete
     * @return DeleteResult containing success status and any error information
     */
    @Transactional
    public DeleteResult deleteCustomer(Long customerNumber) {
        logger.info("Attempting to delete customer: {}", customerNumber);
        
        if (customerNumber == null || customerNumber <= 0) {
            logger.warn("Invalid customer number for deletion: {}", customerNumber);
            return new DeleteResult(false, "1", "Invalid customer number");
        }
        
        String sortCode = sortCodeService.getSortCode();
        
        Optional<Customer> existingCustomer = customerRepository.findById(sortCode, customerNumber);
        if (existingCustomer.isEmpty()) {
            logger.warn("Customer not found for deletion: {}", customerNumber);
            return new DeleteResult(false, "1", "Customer not found");
        }
        
        Customer customer = existingCustomer.get();
        
        try {
            List<Account> customerAccounts = accountRepository.findByCustomerNumber(customerNumber);
            logger.debug("Found {} accounts for customer {}", customerAccounts.size(), customerNumber);
            
            for (Account account : customerAccounts) {
                boolean accountDeleted = accountRepository.deleteById(account.getSortCode(), account.getAccountNumber());
                if (!accountDeleted) {
                    logger.error("Failed to delete account {} for customer {}", 
                            account.getAccountNumber(), customerNumber);
                    return new DeleteResult(false, "3", 
                            "Failed to delete account " + account.getAccountNumber());
                }
                logger.debug("Deleted account {} for customer {}", 
                        account.getAccountNumber(), customerNumber);
            }
            
            boolean customerDeleted = customerRepository.deleteById(sortCode, customerNumber);
            if (!customerDeleted) {
                logger.error("Failed to delete customer record: {}", customerNumber);
                return new DeleteResult(false, "3", "Failed to delete customer record");
            }
            
            logger.info("Successfully deleted customer {} and {} associated accounts", 
                    customerNumber, customerAccounts.size());
            return new DeleteResult(true, null, null, customer, customerAccounts.size());
            
        } catch (Exception e) {
            logger.error("Datastore error while deleting customer {}: {}", customerNumber, e.getMessage());
            return new DeleteResult(false, "2", "Datastore error: " + e.getMessage());
        }
    }

    /**
     * Updates a customer's name and/or address.
     * Implements UPDCUST COBOL program logic.
     * 
     * Validation rules (from COBOL):
     * - Name must start with a valid title (Professor, Mr, Mrs, Miss, Ms, Dr, Drs, Lord, Sir, Lady)
     * - At least one of name or address must be provided
     * - Neither name nor address can be all spaces
     * 
     * Error codes (matching COBOL):
     * - '1': Customer not found
     * - '2': Datastore error
     * - '3': Update operation failure
     * - '4': Both name and address are empty/spaces
     * - 'T': Invalid title
     * 
     * @param customerNumber the customer number to update
     * @param name the new name (optional if address provided)
     * @param address the new address (optional if name provided)
     * @return UpdateResult containing success status and updated customer or error information
     */
    @Transactional
    public UpdateResult updateCustomer(Long customerNumber, String name, String address) {
        logger.info("Attempting to update customer: {}", customerNumber);
        
        if (customerNumber == null || customerNumber <= 0) {
            logger.warn("Invalid customer number for update: {}", customerNumber);
            return new UpdateResult(false, "1", "Invalid customer number");
        }
        
        String sortCode = sortCodeService.getSortCode();
        
        boolean nameEmpty = name == null || name.isBlank();
        boolean addressEmpty = address == null || address.isBlank();
        
        if (nameEmpty && addressEmpty) {
            logger.warn("Both name and address are empty for customer update: {}", customerNumber);
            return new UpdateResult(false, "4", "Both name and address cannot be empty");
        }
        
        if (!nameEmpty) {
            String title = extractTitle(name);
            if (!isValidTitle(title)) {
                logger.warn("Invalid title '{}' in name for customer: {}", title, customerNumber);
                return new UpdateResult(false, "T", 
                        "Invalid title. Valid titles are: " + String.join(", ", VALID_TITLES));
            }
        }
        
        Optional<Customer> existingCustomer = customerRepository.findById(sortCode, customerNumber);
        if (existingCustomer.isEmpty()) {
            logger.warn("Customer not found for update: {}", customerNumber);
            return new UpdateResult(false, "1", "Customer not found");
        }
        
        Customer customer = existingCustomer.get();
        
        try {
            if (!nameEmpty) {
                customer.setName(name);
            }
            if (!addressEmpty) {
                customer.setAddress(address);
            }
            
            Customer updatedCustomer = customerRepository.save(customer);
            logger.info("Successfully updated customer: {}", customerNumber);
            return new UpdateResult(true, null, null, updatedCustomer);
            
        } catch (Exception e) {
            logger.error("Error updating customer {}: {}", customerNumber, e.getMessage());
            return new UpdateResult(false, "2", "Datastore error: " + e.getMessage());
        }
    }

    /**
     * Extracts the title from a customer name.
     * The title is expected to be the first word before a space.
     */
    private String extractTitle(String name) {
        if (name == null || name.isBlank()) {
            return "";
        }
        String[] parts = name.trim().split("\\s+", 2);
        return parts.length > 0 ? parts[0] : "";
    }

    /**
     * Validates that the title is one of the allowed values.
     * Matches COBOL UPDCUST validation logic.
     */
    private boolean isValidTitle(String title) {
        if (title == null || title.isBlank()) {
            return true;
        }
        return VALID_TITLES.contains(title);
    }

    /**
     * Result class for delete operations.
     */
    public static class DeleteResult {
        private final boolean success;
        private final String failCode;
        private final String errorMessage;
        private final Customer deletedCustomer;
        private final int deletedAccountCount;

        public DeleteResult(boolean success, String failCode, String errorMessage) {
            this(success, failCode, errorMessage, null, 0);
        }

        public DeleteResult(boolean success, String failCode, String errorMessage, 
                           Customer deletedCustomer, int deletedAccountCount) {
            this.success = success;
            this.failCode = failCode;
            this.errorMessage = errorMessage;
            this.deletedCustomer = deletedCustomer;
            this.deletedAccountCount = deletedAccountCount;
        }

        public boolean isSuccess() { return success; }
        public String getFailCode() { return failCode; }
        public String getErrorMessage() { return errorMessage; }
        public Customer getDeletedCustomer() { return deletedCustomer; }
        public int getDeletedAccountCount() { return deletedAccountCount; }
    }

    /**
     * Result class for update operations.
     */
    public static class UpdateResult {
        private final boolean success;
        private final String failCode;
        private final String errorMessage;
        private final Customer updatedCustomer;

        public UpdateResult(boolean success, String failCode, String errorMessage) {
            this(success, failCode, errorMessage, null);
        }

        public UpdateResult(boolean success, String failCode, String errorMessage, Customer updatedCustomer) {
            this.success = success;
            this.failCode = failCode;
            this.errorMessage = errorMessage;
            this.updatedCustomer = updatedCustomer;
        }

        public boolean isSuccess() { return success; }
        public String getFailCode() { return failCode; }
        public String getErrorMessage() { return errorMessage; }
        public Customer getUpdatedCustomer() { return updatedCustomer; }
    }
}
