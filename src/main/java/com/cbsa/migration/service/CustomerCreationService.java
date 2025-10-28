package com.cbsa.migration.service;

import com.cbsa.migration.dto.CreditScoreRequestDto;
import com.cbsa.migration.dto.CreditScoreResponseDto;
import com.cbsa.migration.dto.CustomerRequestDto;
import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.Period;

@Service
public class CustomerCreationService {

    private static final Logger logger = LoggerFactory.getLogger(CustomerCreationService.class);
    
    private final CustomerRepository customerRepository;
    private final ControlRepository controlRepository;
    private final TransactionRepository transactionRepository;
    private final CreditAgencyService creditAgencyService;
    
    public CustomerCreationService(
            CustomerRepository customerRepository,
            ControlRepository controlRepository,
            TransactionRepository transactionRepository,
            CreditAgencyService creditAgencyService) {
        this.customerRepository = customerRepository;
        this.controlRepository = controlRepository;
        this.transactionRepository = transactionRepository;
        this.creditAgencyService = creditAgencyService;
    }
    
    @Transactional
    public CustomerResponseDto createCustomer(CustomerRequestDto request) {
        logger.info("Creating customer: name={}, sortCode={}", request.getName(), request.getSortCode());
        
        validateDateOfBirth(request.getDateOfBirth());
        
        Long customerNumber = controlRepository.getNextCustomerNumber();
        logger.debug("Assigned customer number: {}", customerNumber);
        
        Integer creditScore = performCreditCheck(request, customerNumber);
        LocalDate creditScoreReviewDate = (creditScore == 0) ? LocalDate.now() : null;
        
        Customer customer = Customer.builder()
                .eyeCatcher(Customer.VALID_EYECATCHER)
                .sortCode(request.getSortCode())
                .customerNumber(customerNumber)
                .name(request.getName())
                .address(request.getAddress())
                .dateOfBirth(request.getDateOfBirth())
                .creditScore(creditScore)
                .creditScoreReviewDate(creditScoreReviewDate)
                .build();
        
        customerRepository.save(customer);
        logger.info("Customer saved: {}-{}", customer.getSortCode(), customer.getCustomerNumber());
        
        logCustomerCreationTransaction(customer);
        
        return CustomerResponseDto.builder()
                .name(customer.getName())
                .address(customer.getAddress())
                .dateOfBirth(customer.getDateOfBirth())
                .customerNumber(customer.getCustomerNumber())
                .sortCode(customer.getSortCode())
                .status("ACTIVE")
                .build();
    }
    
    private void validateDateOfBirth(LocalDate dateOfBirth) {
        if (dateOfBirth.getYear() < 1601) {
            throw new IllegalArgumentException("Birth year must be 1601 or later");
        }
        
        LocalDate today = LocalDate.now();
        if (dateOfBirth.isAfter(today)) {
            throw new IllegalArgumentException("Date of birth cannot be in the future");
        }
        
        int age = Period.between(dateOfBirth, today).getYears();
        if (age > 150) {
            throw new IllegalArgumentException("Age cannot exceed 150 years");
        }
    }
    
    private Integer performCreditCheck(CustomerRequestDto request, Long customerNumber) {
        try {
            CreditScoreRequestDto creditRequest = CreditScoreRequestDto.builder()
                    .sortCode(request.getSortCode())
                    .customerNumber(customerNumber)
                    .name(request.getName())
                    .address(request.getAddress())
                    .dateOfBirth(request.getDateOfBirth())
                    .currentCreditScore(0)
                    .build();
            
            CreditScoreResponseDto creditResponse = creditAgencyService.processCredit(creditRequest);
            
            if (creditResponse.getSuccess()) {
                logger.info("Credit check successful for customer {}-{}: score={}", 
                           request.getSortCode(), customerNumber, creditResponse.getUpdatedCreditScore());
                return creditResponse.getUpdatedCreditScore();
            } else {
                logger.warn("Credit check failed for customer {}-{}: {}", 
                           request.getSortCode(), customerNumber, creditResponse.getErrorMessage());
                return 0;
            }
        } catch (Exception e) {
            logger.error("Credit check error for customer {}-{}: {}", 
                        request.getSortCode(), customerNumber, e.getMessage(), e);
            return 0;
        }
    }
    
    private void logCustomerCreationTransaction(Customer customer) {
        String description = buildTransactionDescription(customer);
        
        Transaction transaction = Transaction.builder()
                .eyeCatcher(Transaction.VALID_EYECATCHER)
                .logicallyDeleted(false)
                .sortCode(customer.getSortCode())
                .accountNumber("00000000")
                .transactionDate(LocalDate.now())
                .transactionTime(LocalTime.now())
                .referenceNumber(System.currentTimeMillis())
                .transactionType(Transaction.TYPE_BRANCH_CREATE_CUSTOMER)
                .description(description)
                .amount(BigDecimal.ZERO)
                .build();
        
        transactionRepository.save(transaction);
        logger.debug("PROCTRAN transaction logged for customer {}-{}", 
                    customer.getSortCode(), customer.getCustomerNumber());
    }
    
    private String buildTransactionDescription(Customer customer) {
        String sortCode = customer.getSortCode();
        String custNo = String.format("%010d", customer.getCustomerNumber());
        String name = customer.getName().length() > 14 
                ? customer.getName().substring(0, 14) 
                : String.format("%-14s", customer.getName());
        String dob = customer.getDateOfBirth().toString();
        
        return sortCode + custNo + name + dob;
    }
}
