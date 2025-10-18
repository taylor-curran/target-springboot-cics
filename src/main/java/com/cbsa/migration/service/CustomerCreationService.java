package com.cbsa.migration.service;

import com.cbsa.migration.dto.CustomerCreationRequestDto;
import com.cbsa.migration.dto.CustomerCreationResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.Period;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

@Service
public class CustomerCreationService {

    private static final Logger logger = LoggerFactory.getLogger(CustomerCreationService.class);
    private static final int MIN_BIRTH_YEAR = 1601;
    private static final int MAX_AGE_YEARS = 150;
    
    private final CustomerRepository customerRepository;
    private final ControlRepository controlRepository;
    private final TransactionRepository transactionRepository;
    private final CreditAgencyService creditAgencyService;
    private final boolean creditCheckEnabled;
    private final int numberOfCreditAgencies;
    private final Random random;
    
    public CustomerCreationService(
            CustomerRepository customerRepository,
            ControlRepository controlRepository,
            TransactionRepository transactionRepository,
            CreditAgencyService creditAgencyService,
            @Value("${customer-creation.credit-check.enabled:true}") boolean creditCheckEnabled,
            @Value("${customer-creation.credit-check.agencies:5}") int numberOfCreditAgencies) {
        
        this.customerRepository = customerRepository;
        this.controlRepository = controlRepository;
        this.transactionRepository = transactionRepository;
        this.creditAgencyService = creditAgencyService;
        this.creditCheckEnabled = creditCheckEnabled;
        this.numberOfCreditAgencies = numberOfCreditAgencies;
        this.random = new Random();
        
        logger.info("CustomerCreationService initialized - creditCheckEnabled: {}, agencies: {}", 
                   creditCheckEnabled, numberOfCreditAgencies);
    }
    
    @Transactional
    public CustomerCreationResponseDto createCustomer(CustomerCreationRequestDto request) {
        logger.info("Creating customer: sortCode={}, name={}", request.getSortCode(), request.getName());
        
        String dobValidationError = validateDateOfBirth(request.getDateOfBirth());
        if (dobValidationError != null) {
            logger.warn("Date of birth validation failed: {}", dobValidationError);
            return CustomerCreationResponseDto.builder()
                .success(false)
                .failCode(dobValidationError)
                .errorMessage("Date of birth validation failed")
                .build();
        }
        
        Long customerNumber;
        try {
            customerNumber = controlRepository.getNextCustomerNumber();
            logger.info("Assigned customer number: {}", customerNumber);
        } catch (Exception e) {
            logger.error("Failed to get next customer number", e);
            return CustomerCreationResponseDto.builder()
                .success(false)
                .failCode("3")
                .errorMessage("Failed to allocate customer number")
                .build();
        }
        
        CreditCheckResult creditResult = performCreditCheck(request, customerNumber);
        
        Customer customer = Customer.builder()
            .eyeCatcher("CUST")
            .sortCode(request.getSortCode())
            .customerNumber(customerNumber)
            .name(request.getName())
            .address(request.getAddress())
            .dateOfBirth(request.getDateOfBirth())
            .creditScore(creditResult.score)
            .creditScoreReviewDate(creditResult.reviewDate)
            .build();
        
        try {
            customer = customerRepository.save(customer);
            logger.info("Customer saved: {}-{}", customer.getSortCode(), customer.getCustomerNumber());
        } catch (Exception e) {
            logger.error("Failed to save customer", e);
            return CustomerCreationResponseDto.builder()
                .success(false)
                .failCode("1")
                .errorMessage("Failed to save customer record")
                .build();
        }
        
        try {
            logCustomerCreation(customer);
        } catch (Exception e) {
            logger.error("Failed to log customer creation transaction", e);
        }
        
        return CustomerCreationResponseDto.builder()
            .eyeCatcher("CUST")
            .sortCode(customer.getSortCode())
            .customerNumber(customer.getCustomerNumber())
            .name(customer.getName())
            .address(customer.getAddress())
            .dateOfBirth(customer.getDateOfBirth())
            .creditScore(customer.getCreditScore())
            .creditScoreReviewDate(customer.getCreditScoreReviewDate())
            .success(true)
            .failCode(null)
            .build();
    }
    
    private String validateDateOfBirth(LocalDate dateOfBirth) {
        if (dateOfBirth.getYear() < MIN_BIRTH_YEAR) {
            return "O";
        }
        
        if (dateOfBirth.isAfter(LocalDate.now())) {
            return "Y";
        }
        
        int age = Period.between(dateOfBirth, LocalDate.now()).getYears();
        if (age > MAX_AGE_YEARS) {
            return "O";
        }
        
        return null;
    }
    
    private CreditCheckResult performCreditCheck(CustomerCreationRequestDto request, Long customerNumber) {
        if (!creditCheckEnabled) {
            logger.debug("Credit check disabled, using default score");
            return new CreditCheckResult(0, LocalDate.now());
        }
        
        List<Integer> scores = new ArrayList<>();
        
        for (int i = 0; i < numberOfCreditAgencies; i++) {
            try {
                int score = creditAgencyService.generateCreditScore();
                scores.add(score);
                logger.debug("Credit agency {} returned score: {}", i + 1, score);
            } catch (Exception e) {
                logger.warn("Credit agency {} failed: {}", i + 1, e.getMessage());
            }
        }
        
        if (scores.isEmpty()) {
            logger.warn("All credit agencies failed, setting score to 0");
            return new CreditCheckResult(0, LocalDate.now());
        }
        
        int averageScore = (int) scores.stream().mapToInt(Integer::intValue).average().orElse(0);
        
        int daysUntilReview = random.nextInt(21) + 1;
        LocalDate reviewDate = LocalDate.now().plusDays(daysUntilReview);
        
        logger.info("Credit check complete: score={}, reviewDate={}", averageScore, reviewDate);
        return new CreditCheckResult(averageScore, reviewDate);
    }
    
    private void logCustomerCreation(Customer customer) {
        Transaction transaction = new Transaction();
        transaction.setEyeCatcher("PRTR");
        transaction.setLogicallyDeleted(false);
        transaction.setSortCode(customer.getSortCode());
        transaction.setAccountNumber("00000000");
        transaction.setTransactionDate(LocalDate.now());
        transaction.setTransactionTime(LocalTime.now());
        transaction.setReferenceNumber(System.currentTimeMillis() % 1000000000L);
        transaction.setTransactionType("OCC");
        
        String description = String.format("%s%010d%-14s%s",
            customer.getSortCode(),
            customer.getCustomerNumber(),
            customer.getName().substring(0, Math.min(14, customer.getName().length())),
            customer.getDateOfBirth().toString());
        transaction.setDescription(description);
        
        transaction.setAmount(BigDecimal.ZERO);
        
        transactionRepository.save(transaction);
        logger.debug("Customer creation transaction logged");
    }
    
    private static class CreditCheckResult {
        final int score;
        final LocalDate reviewDate;
        
        CreditCheckResult(int score, LocalDate reviewDate) {
            this.score = score;
            this.reviewDate = reviewDate;
        }
    }
}
