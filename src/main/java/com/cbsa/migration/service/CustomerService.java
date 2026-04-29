package com.cbsa.migration.service;

import com.cbsa.migration.dto.CreditScoreRequestDto;
import com.cbsa.migration.dto.CreditScoreResponseDto;
import com.cbsa.migration.dto.CustomerRequestDto;
import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.dto.mapper.DtoMapper;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.Period;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * Main business logic service porting CRECUST.cbl PREMIERE SECTION (lines 353-420)
 * and INQCUST customer read operations.
 *
 * Covers migrate_001 (customer read) and migrate_002 (customer create).
 */
@Service
public class CustomerService {

    private static final Logger logger = LoggerFactory.getLogger(CustomerService.class);

    private final CustomerRepository customerRepository;
    private final CreditAgencyService creditAgencyService;
    private final NamedCounterService namedCounterService;
    private final TransactionRepository transactionRepository;
    private final ErrorLoggingService errorLoggingService;
    private final SortCodeService sortCodeService;
    private final DtoMapper dtoMapper;

    public CustomerService(CustomerRepository customerRepository,
                           CreditAgencyService creditAgencyService,
                           NamedCounterService namedCounterService,
                           TransactionRepository transactionRepository,
                           ErrorLoggingService errorLoggingService,
                           SortCodeService sortCodeService,
                           DtoMapper dtoMapper) {
        this.customerRepository = customerRepository;
        this.creditAgencyService = creditAgencyService;
        this.namedCounterService = namedCounterService;
        this.transactionRepository = transactionRepository;
        this.errorLoggingService = errorLoggingService;
        this.sortCodeService = sortCodeService;
        this.dtoMapper = dtoMapper;
    }

    /**
     * Creates a new customer following the CRECUST.cbl PREMIERE SECTION flow.
     * Steps: validate DOB -> async credit check -> acquire lock -> get next number
     * -> write customer -> write PROCTRAN -> release lock -> return.
     */
    public CustomerResponseDto createCustomer(CustomerRequestDto request) {
        logger.info("Creating customer for sort code {}", request.getSortCode());

        // a) Date of Birth Validation (COBOL DATE-OF-BIRTH-CHECK SECTION, lines 1364-1417)
        validateDateOfBirth(request.getDateOfBirth());

        // b) Async Credit Check (COBOL CREDIT-CHECK SECTION, lines 505-739)
        CreditCheckResult creditResult = performAsyncCreditCheck(request);

        // c) Acquire Lock + Get Next Customer Number
        Long customerNumber;
        try {
            customerNumber = namedCounterService.getNextCustomerNumberWithLock(request.getSortCode());
        } catch (CustomerCreationException e) {
            throw e;
        }

        try {
            // d) Write Customer Record (COBOL WRITE-CUSTOMER-VSAM SECTION, lines 1011-1088)
            Customer customer = Customer.builder()
                    .eyeCatcher(Customer.VALID_EYECATCHER)
                    .sortCode(request.getSortCode())
                    .customerNumber(customerNumber)
                    .name(request.getName())
                    .address(request.getAddress())
                    .dateOfBirth(request.getDateOfBirth())
                    .creditScore(creditResult.getCreditScore())
                    .creditScoreReviewDate(creditResult.getReviewDate())
                    .build();

            try {
                customerRepository.save(customer);
            } catch (Exception e) {
                logger.error("Failed to write customer record for {}-{}", request.getSortCode(), customerNumber, e);
                namedCounterService.rollbackCustomerNumber();
                throw new CustomerCreationException("1", "Failed to write customer record", e);
            }

            // e) Write PROCTRAN Audit Record (COBOL WRITE-PROCTRAN-DB2 SECTION, lines 1129-1193)
            try {
                writeProctranAuditRecord(customer);
            } catch (Exception e) {
                logger.error("Failed to write PROCTRAN audit record for customer {}-{}",
                        request.getSortCode(), customerNumber, e);
                errorLoggingService.logError("CRECUST", e);
                namedCounterService.rollbackCustomerNumber();
                throw new CustomerCreationException("4", "Failed to write PROCTRAN audit record", e);
            }

            // f) Return success
            logger.info("Customer created successfully: {}-{}", request.getSortCode(), customerNumber);
            return CustomerResponseDto.builder()
                    .sortCode(customer.getSortCode())
                    .customerNumber(customer.getCustomerNumber())
                    .name(customer.getName())
                    .address(customer.getAddress())
                    .dateOfBirth(customer.getDateOfBirth())
                    .creditScore(customer.getCreditScore())
                    .creditScoreReviewDate(customer.getCreditScoreReviewDate())
                    .status("CREATED")
                    .success(true)
                    .build();

        } finally {
            namedCounterService.releaseLock();
        }
    }

    /**
     * Retrieves a customer by sort code and customer number (migrate_001 - INQCUST).
     */
    public CustomerResponseDto getCustomer(String sortCode, Long customerNumber) {
        Customer customer = customerRepository.findById(sortCode, customerNumber)
                .orElseThrow(() -> new CustomerNotFoundException(
                        "Customer not found: " + sortCode + "-" + customerNumber));
        return dtoMapper.toCustomerResponseDto(customer);
    }

    /**
     * Searches for customers by name (partial match).
     */
    public List<CustomerResponseDto> getCustomerByName(String name) {
        return customerRepository.findByNameContaining(name).stream()
                .map(dtoMapper::toCustomerResponseDto)
                .collect(Collectors.toList());
    }

    /**
     * Validates date of birth following COBOL DATE-OF-BIRTH-CHECK SECTION (lines 1364-1417).
     */
    private void validateDateOfBirth(LocalDate dob) {
        if (dob == null) {
            throw new CustomerCreationException("Z", "Date of birth is required");
        }

        // DOB year < 1601 -> fail code 'O' (COBOL CEEDAYS limitation)
        if (dob.getYear() < 1601) {
            throw new CustomerCreationException("O", "Date of birth year before 1601 is not supported");
        }

        // DOB is in the future -> fail code 'Y'
        if (dob.isAfter(LocalDate.now())) {
            throw new CustomerCreationException("Y", "Date of birth cannot be in the future");
        }

        // Age > 150 years -> fail code 'O'
        int age = Period.between(dob, LocalDate.now()).getYears();
        if (age > 150) {
            throw new CustomerCreationException("O", "Customer age exceeds 150 years");
        }
    }

    /**
     * Performs async credit check by firing 5 parallel calls to CreditAgencyService,
     * replacing CICS RUN TRANSID OCR1-OCR5. Waits up to 3 seconds.
     */
    private CreditCheckResult performAsyncCreditCheck(CustomerRequestDto request) {
        CreditScoreRequestDto creditRequest = CreditScoreRequestDto.builder()
                .sortCode(request.getSortCode())
                .customerNumber(0L)
                .name(request.getName())
                .address(request.getAddress())
                .dateOfBirth(request.getDateOfBirth())
                .build();

        List<CompletableFuture<CreditScoreResponseDto>> futures = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            futures.add(CompletableFuture.supplyAsync(() -> {
                try {
                    return creditAgencyService.processCredit(creditRequest);
                } catch (Exception e) {
                    logger.warn("Credit agency call failed: {}", e.getMessage());
                    return null;
                }
            }));
        }

        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
                    .get(3, TimeUnit.SECONDS);
        } catch (Exception e) {
            logger.info("Credit check timeout or interruption (expected for partial responses): {}", e.getMessage());
        }

        List<CreditScoreResponseDto> responses = futures.stream()
                .filter(CompletableFuture::isDone)
                .map(f -> {
                    try {
                        return f.getNow(null);
                    } catch (Exception e) {
                        return null;
                    }
                })
                .filter(r -> r != null && r.getSuccess() != null && r.getSuccess()
                        && r.getUpdatedCreditScore() != null)
                .collect(Collectors.toList());

        if (responses.isEmpty()) {
            // No agencies responded -> set score=0, review date=today
            logger.warn("No credit agencies responded for customer request");
            return new CreditCheckResult(0, LocalDate.now());
        }

        // Compute average of returned scores
        int averageScore = (int) responses.stream()
                .mapToInt(CreditScoreResponseDto::getUpdatedCreditScore)
                .average()
                .orElse(0);

        // Random review date 1-21 days from today
        int daysToAdd = ThreadLocalRandom.current().nextInt(1, 22);
        LocalDate reviewDate = LocalDate.now().plusDays(daysToAdd);

        logger.info("Credit check completed: {} agencies responded, average score={}, review in {} days",
                responses.size(), averageScore, daysToAdd);
        return new CreditCheckResult(averageScore, reviewDate);
    }

    /**
     * Writes a PROCTRAN audit record for a new customer creation
     * (COBOL WRITE-PROCTRAN-DB2 SECTION, lines 1129-1193).
     */
    private void writeProctranAuditRecord(Customer customer) {
        String dobFormatted = String.format("%02d/%02d/%04d",
                customer.getDateOfBirth().getDayOfMonth(),
                customer.getDateOfBirth().getMonthValue(),
                customer.getDateOfBirth().getYear());

        // Build description: sortcode + customerNumber + name + dob (first 40 chars)
        String rawDescription = customer.getSortCode()
                + String.format("%010d", customer.getCustomerNumber())
                + customer.getName()
                + dobFormatted;
        String description = rawDescription.length() > 40
                ? rawDescription.substring(0, 40)
                : rawDescription;

        Transaction proctran = Transaction.builder()
                .eyeCatcher(Transaction.VALID_EYECATCHER)
                .sortCode(customer.getSortCode())
                .accountNumber("00000000")
                .transactionDate(LocalDate.now())
                .transactionTime(LocalTime.now().withNano(0))
                .referenceNumber(System.nanoTime())
                .transactionType(Transaction.TYPE_BRANCH_CREATE_CUSTOMER)
                .description(description)
                .amount(BigDecimal.ZERO)
                .logicallyDeleted(false)
                .build();

        transactionRepository.save(proctran);
        logger.info("PROCTRAN audit record written for customer {}-{}",
                customer.getSortCode(), customer.getCustomerNumber());
    }

    /**
     * Internal result holder for async credit check.
     */
    static class CreditCheckResult {
        private final int creditScore;
        private final LocalDate reviewDate;

        CreditCheckResult(int creditScore, LocalDate reviewDate) {
            this.creditScore = creditScore;
            this.reviewDate = reviewDate;
        }

        public int getCreditScore() {
            return creditScore;
        }

        public LocalDate getReviewDate() {
            return reviewDate;
        }
    }

    /**
     * Exception for customer not found (used by getCustomer).
     */
    public static class CustomerNotFoundException extends RuntimeException {
        public CustomerNotFoundException(String message) {
            super(message);
        }
    }
}
