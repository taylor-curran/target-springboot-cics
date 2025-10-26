package com.cbsa.migration.service;

import com.cbsa.migration.dto.CreditScoreRequestDto;
import com.cbsa.migration.dto.CreditScoreResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Timer;
import io.micrometer.core.instrument.Counter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.Optional;
import java.util.Random;

/**
 * Credit Agency Service - simulates external credit agency processing
 * Migrated from COBOL program CRDTAGY1
 * 
 * Key behaviors preserved from COBOL:
 * - Random delay simulation (0-3 seconds)
 * - Random credit score generation (1-999)
 * - Customer record updates
 */
@Service
public class CreditAgencyService {

    private static final Logger logger = LoggerFactory.getLogger(CreditAgencyService.class);

    private final CustomerRepository customerRepository;
    private final Random random;
    private final boolean delayEnabled;
    private final int maxDelaySeconds;
    private final int minScore;
    private final int maxScore;
    private final MeterRegistry meterRegistry;
    private final Counter successCounter;
    private final Counter failureCounter;
    private final Timer processingTimer;

    public CreditAgencyService(
            CustomerRepository customerRepository,
            @Value("${credit-agency.simulation.delay.enabled:true}") boolean delayEnabled,
            @Value("${credit-agency.simulation.delay.max-seconds:3}") int maxDelaySeconds,
            @Value("${credit-agency.simulation.score.min:1}") int minScore,
            @Value("${credit-agency.simulation.score.max:999}") int maxScore,
            MeterRegistry meterRegistry) {
        
        this.customerRepository = customerRepository;
        this.delayEnabled = delayEnabled;
        this.maxDelaySeconds = maxDelaySeconds;
        this.minScore = minScore;
        this.maxScore = maxScore;
        this.random = new Random();
        this.meterRegistry = meterRegistry;
        
        this.successCounter = Counter.builder("banking.credit.score.requests")
            .tag("status", "success")
            .description("Successful credit score requests")
            .register(meterRegistry);
            
        this.failureCounter = Counter.builder("banking.credit.score.requests")
            .tag("status", "failure")
            .description("Failed credit score requests")
            .register(meterRegistry);
            
        this.processingTimer = Timer.builder("banking.credit.score.processing.time")
            .description("Credit score processing time")
            .register(meterRegistry);
        
        logger.info("CreditAgencyService initialized with metrics - delayEnabled: {}, maxDelaySeconds: {}, scoreRange: {}-{}", 
                   delayEnabled, maxDelaySeconds, minScore, maxScore);
    }

    public CreditScoreResponseDto processCredit(CreditScoreRequestDto request) {
        logger.info("Processing credit score request for customer: {}-{}", 
                   request.getSortCode(), request.getCustomerNumber());
        
        return processingTimer.record(() -> {
            long startTime = System.currentTimeMillis();
            
            try {
                Optional<Customer> customerOpt = customerRepository.findById(
                    request.getSortCode(), request.getCustomerNumber());
                
                if (customerOpt.isEmpty()) {
                    failureCounter.increment();
                    return CreditScoreResponseDto.builder()
                        .sortCode(request.getSortCode())
                        .customerNumber(request.getCustomerNumber())
                        .success(false)
                        .errorMessage("Customer not found")
                        .processingTimeMs(System.currentTimeMillis() - startTime)
                        .build();
                }

                Customer customer = customerOpt.get();
                
                if (!customer.getName().equals(request.getName()) || 
                    !customer.getAddress().equals(request.getAddress()) ||
                    !customer.getDateOfBirth().equals(request.getDateOfBirth())) {
                    
                    failureCounter.increment();
                    return CreditScoreResponseDto.builder()
                        .sortCode(request.getSortCode())
                        .customerNumber(request.getCustomerNumber())
                        .success(false)
                        .errorMessage("Customer data mismatch")
                        .processingTimeMs(System.currentTimeMillis() - startTime)
                        .build();
                }

                simulateProcessingDelay();

                int newCreditScore = generateCreditScore();
                
                Customer updatedCustomer = updateCustomerCreditScore(customer, newCreditScore);

                long processingTime = System.currentTimeMillis() - startTime;
                
                successCounter.increment();
                
                logger.info("Credit score processing completed for customer {}-{}: {} -> {} ({}ms)", 
                           request.getSortCode(), request.getCustomerNumber(), 
                           request.getCurrentCreditScore(), newCreditScore, processingTime);

                return CreditScoreResponseDto.builder()
                    .sortCode(request.getSortCode())
                    .customerNumber(request.getCustomerNumber())
                    .updatedCreditScore(newCreditScore)
                    .scoreReviewDate(updatedCustomer.getCreditScoreReviewDate())
                    .processingTimeMs(processingTime)
                    .success(true)
                    .build();

            } catch (Exception e) {
                failureCounter.increment();
                logger.error("Error processing credit score for customer {}-{}: {}", 
                            request.getSortCode(), request.getCustomerNumber(), e.getMessage(), e);
                
                return CreditScoreResponseDto.builder()
                    .sortCode(request.getSortCode())
                    .customerNumber(request.getCustomerNumber())
                    .success(false)
                    .errorMessage("Processing error: " + e.getMessage())
                    .processingTimeMs(System.currentTimeMillis() - startTime)
                    .build();
            }
        });
    }

    public int generateCreditScore() {
        int score = random.nextInt(maxScore - minScore + 1) + minScore;
        logger.debug("Generated credit score: {}", score);
        return score;
    }

    public void simulateProcessingDelay() {
        if (!delayEnabled) {
            logger.debug("Processing delay disabled");
            return;
        }

        int delaySeconds = random.nextInt(maxDelaySeconds + 1); // 0 to maxDelaySeconds
        int delayMs = delaySeconds * 1000;
        
        logger.debug("Simulating processing delay: {}ms", delayMs);
        
        try {
            Thread.sleep(delayMs);
        } catch (InterruptedException e) {
            logger.warn("Processing delay interrupted", e);
            Thread.currentThread().interrupt();
        }
    }

    public Customer updateCustomerCreditScore(Customer customer, int newCreditScore) {
        customer.setCreditScore(newCreditScore);
        customer.setCreditScoreReviewDate(LocalDate.now());
        
        Customer savedCustomer = customerRepository.save(customer);
        
        logger.debug("Updated customer {}-{} credit score to {} on {}", 
                    customer.getSortCode(), customer.getCustomerNumber(), 
                    newCreditScore, customer.getCreditScoreReviewDate());
        
        return savedCustomer;
    }
}
