package com.cbsa.migration.service;

import com.cbsa.migration.dto.CreateCustomerRequestDto;
import com.cbsa.migration.dto.CreateCustomerResponseDto;
import com.cbsa.migration.dto.CreditScoreRequestDto;
import com.cbsa.migration.dto.CreditScoreResponseDto;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.Period;

@Service
public class CreateCustomerService {

    private static final Logger logger = LoggerFactory.getLogger(CreateCustomerService.class);
    private static final int MIN_BIRTH_YEAR = 1601;
    private static final int MAX_AGE_YEARS = 150;

    private final CustomerRepository customerRepository;
    private final ControlRepository controlRepository;
    private final CreditAgencyService creditAgencyService;

    public CreateCustomerService(
            CustomerRepository customerRepository,
            ControlRepository controlRepository,
            CreditAgencyService creditAgencyService) {
        this.customerRepository = customerRepository;
        this.controlRepository = controlRepository;
        this.creditAgencyService = creditAgencyService;
        logger.info("CreateCustomerService initialized");
    }

    public CreateCustomerResponseDto createCustomer(CreateCustomerRequestDto request) {
        logger.info("Creating customer with sortCode: {}, name: {}", request.getSortCode(), request.getName());
        
        try {
            DateValidationResult validationResult = validateDateOfBirth(request.getDateOfBirth());
            if (!validationResult.isValid()) {
                return CreateCustomerResponseDto.builder()
                    .success(false)
                    .failCode(validationResult.getFailCode())
                    .message(validationResult.getMessage())
                    .build();
            }

            Long customerNumber = controlRepository.getNextCustomerNumber();
            logger.info("Assigned customer number: {}", customerNumber);

            Customer customer = Customer.builder()
                .eyeCatcher(Customer.VALID_EYECATCHER)
                .sortCode(request.getSortCode())
                .customerNumber(customerNumber)
                .name(request.getName())
                .address(request.getAddress())
                .dateOfBirth(request.getDateOfBirth())
                .creditScore(0)
                .creditScoreReviewDate(LocalDate.now())
                .build();

            CreditScoreRequestDto creditRequest = CreditScoreRequestDto.builder()
                .sortCode(request.getSortCode())
                .customerNumber(customerNumber)
                .name(request.getName())
                .address(request.getAddress())
                .dateOfBirth(request.getDateOfBirth())
                .currentCreditScore(0)
                .build();

            CreditScoreResponseDto creditResponse = creditAgencyService.processCredit(creditRequest);
            if (Boolean.TRUE.equals(creditResponse.getSuccess())) {
                customer.setCreditScore(creditResponse.getUpdatedCreditScore());
                customer.setCreditScoreReviewDate(creditResponse.getScoreReviewDate());
                logger.info("Credit score assigned: {}", creditResponse.getUpdatedCreditScore());
            } else {
                logger.warn("Credit check failed, using default score 0");
            }

            Customer savedCustomer = customerRepository.save(customer);
            logger.info("Customer created successfully: {}-{}", savedCustomer.getSortCode(), savedCustomer.getCustomerNumber());

            return CreateCustomerResponseDto.builder()
                .sortCode(savedCustomer.getSortCode())
                .customerNumber(savedCustomer.getCustomerNumber())
                .name(savedCustomer.getName())
                .address(savedCustomer.getAddress())
                .dateOfBirth(savedCustomer.getDateOfBirth())
                .creditScore(savedCustomer.getCreditScore())
                .creditScoreReviewDate(savedCustomer.getCreditScoreReviewDate())
                .success(true)
                .build();

        } catch (Exception e) {
            logger.error("Error creating customer: {}", e.getMessage(), e);
            return CreateCustomerResponseDto.builder()
                .success(false)
                .failCode("1")
                .message("Error creating customer: " + e.getMessage())
                .build();
        }
    }

    private DateValidationResult validateDateOfBirth(LocalDate dateOfBirth) {
        if (dateOfBirth == null) {
            return new DateValidationResult(false, "Z", "Date of birth is required");
        }

        if (dateOfBirth.getYear() < MIN_BIRTH_YEAR) {
            return new DateValidationResult(false, "O", 
                "Birth year must be " + MIN_BIRTH_YEAR + " or later");
        }

        LocalDate today = LocalDate.now();

        if (dateOfBirth.isAfter(today)) {
            return new DateValidationResult(false, "Y", 
                "Date of birth cannot be in the future");
        }

        int age = Period.between(dateOfBirth, today).getYears();
        if (age > MAX_AGE_YEARS) {
            return new DateValidationResult(false, "O", 
                "Customer age cannot exceed " + MAX_AGE_YEARS + " years");
        }

        return new DateValidationResult(true, null, null);
    }

    private static class DateValidationResult {
        private final boolean valid;
        private final String failCode;
        private final String message;

        public DateValidationResult(boolean valid, String failCode, String message) {
            this.valid = valid;
            this.failCode = failCode;
            this.message = message;
        }

        public boolean isValid() {
            return valid;
        }

        public String getFailCode() {
            return failCode;
        }

        public String getMessage() {
            return message;
        }
    }
}
