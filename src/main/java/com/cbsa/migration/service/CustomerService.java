package com.cbsa.migration.service;

import com.cbsa.migration.dto.CreditScoreRequestDto;
import com.cbsa.migration.dto.CreditScoreResponseDto;
import com.cbsa.migration.dto.CustomerRequestDto;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Control;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

@Service
public class CustomerService {
    
    private static final Logger logger = LoggerFactory.getLogger(CustomerService.class);
    
    private final CustomerRepository customerRepository;
    private final ControlRepository controlRepository;
    private final CreditAgencyService creditAgencyService;
    private final TransactionRepository transactionRepository;
    private final SortCodeService sortCodeService;
    private final AccountRepository accountRepository;
    private final Random random;
    
    @Autowired
    public CustomerService(
            CustomerRepository customerRepository,
            ControlRepository controlRepository,
            CreditAgencyService creditAgencyService,
            TransactionRepository transactionRepository,
            SortCodeService sortCodeService,
            AccountRepository accountRepository) {
        this.customerRepository = customerRepository;
        this.controlRepository = controlRepository;
        this.creditAgencyService = creditAgencyService;
        this.transactionRepository = transactionRepository;
        this.sortCodeService = sortCodeService;
        this.accountRepository = accountRepository;
        this.random = new Random();
    }
    
    public Optional<Customer> inquireCustomer(String sortCode, Long customerNumber) {
        if (customerNumber == 9999999999L) {
            Control control = controlRepository.getControl()
                .orElseThrow(() -> new IllegalStateException("Control record not found"));
            customerNumber = control.getLastCustomerNumber();
        }
        
        if (customerNumber == 0L) {
            customerNumber = generateRandomCustomerNumber(sortCode);
        }
        
        return customerRepository.findById(sortCode, customerNumber);
    }
    
    private Long generateRandomCustomerNumber(String sortCode) {
        Control control = controlRepository.getControl()
            .orElseThrow(() -> new IllegalStateException("Control record not found"));
        Long maxCustomerNumber = control.getLastCustomerNumber();
        
        if (maxCustomerNumber == 0L || maxCustomerNumber == 100000L) {
            throw new IllegalStateException("No customers exist in the system");
        }
        
        for (int i = 0; i < 1000; i++) {
            long randomNumber = (long) (random.nextInt(maxCustomerNumber.intValue() - 100000) + 100001);
            Optional<Customer> customer = customerRepository.findById(sortCode, randomNumber);
            if (customer.isPresent()) {
                return randomNumber;
            }
        }
        
        throw new IllegalStateException("Could not find random customer after 1000 attempts");
    }
    
    @Transactional
    public Customer createCustomer(CustomerRequestDto request) {
        String sortCode = request.getSortCode();
        
        validateDateOfBirth(request.getDateOfBirth());
        
        Long customerNumber = controlRepository.getNextCustomerNumber();
        
        int creditScore;
        LocalDate reviewDate;
        try {
            creditScore = performAsyncCreditCheck(request, sortCode, customerNumber);
            reviewDate = LocalDate.now().plusDays(random.nextInt(21) + 1);
        } catch (Exception e) {
            logger.warn("Credit check failed for customer {}: {}", customerNumber, e.getMessage());
            creditScore = 0;
            reviewDate = LocalDate.now();
        }
        
        Customer customer = Customer.builder()
            .eyeCatcher("CUST")
            .sortCode(sortCode)
            .customerNumber(customerNumber)
            .name(request.getName())
            .address(request.getAddress())
            .dateOfBirth(request.getDateOfBirth())
            .creditScore(creditScore)
            .creditScoreReviewDate(reviewDate)
            .build();
        
        Customer savedCustomer = customerRepository.save(customer);
        
        writeCustomerAudit(savedCustomer, Transaction.TYPE_BRANCH_CREATE_CUSTOMER);
        
        return savedCustomer;
    }
    
    private void validateDateOfBirth(LocalDate dateOfBirth) {
        if (dateOfBirth == null) {
            throw new IllegalArgumentException("Date of birth is required");
        }
        
        if (dateOfBirth.getYear() < 1601) {
            throw new IllegalArgumentException("Year must be 1601 or later (fail code: O)");
        }
        
        if (dateOfBirth.isAfter(LocalDate.now())) {
            throw new IllegalArgumentException("Date of birth cannot be in the future (fail code: Y)");
        }
        
        LocalDate minDate = LocalDate.now().minusYears(150);
        if (dateOfBirth.isBefore(minDate)) {
            throw new IllegalArgumentException("Age cannot exceed 150 years (fail code: O)");
        }
    }
    
    private int performAsyncCreditCheck(CustomerRequestDto request, String sortCode, Long customerNumber) {
        List<CompletableFuture<CreditScoreResponseDto>> futures = new ArrayList<>();
        
        for (int i = 0; i < 5; i++) {
            CreditScoreRequestDto creditRequest = CreditScoreRequestDto.builder()
                .sortCode(sortCode)
                .customerNumber(customerNumber)
                .name(request.getName())
                .address(request.getAddress())
                .dateOfBirth(request.getDateOfBirth())
                .currentCreditScore(0)
                .build();
            
            CompletableFuture<CreditScoreResponseDto> future = CompletableFuture
                .supplyAsync(() -> creditAgencyService.processCredit(creditRequest));
            futures.add(future);
        }
        
        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
                .get(3, TimeUnit.SECONDS);
        } catch (TimeoutException e) {
            logger.debug("Some credit agencies did not respond within 3 seconds");
        } catch (Exception e) {
            logger.error("Error during async credit checks", e);
        }
        
        int totalScore = 0;
        int count = 0;
        
        for (CompletableFuture<CreditScoreResponseDto> future : futures) {
            if (future.isDone() && !future.isCompletedExceptionally()) {
                try {
                    CreditScoreResponseDto response = future.get(0, TimeUnit.MILLISECONDS);
                    if (response.getSuccess() != null && response.getSuccess()) {
                        totalScore += response.getUpdatedCreditScore();
                        count++;
                    }
                } catch (Exception e) {
                    logger.debug("Could not retrieve credit score from agency: {}", e.getMessage());
                }
            }
        }
        
        if (count == 0) {
            throw new IllegalStateException("No credit agencies responded (fail code: C)");
        }
        
        return totalScore / count;
    }
    
    @Transactional
    public Customer updateCustomer(String sortCode, Long customerNumber, String name, String address) {
        if ((name == null || name.trim().isEmpty()) && 
            (address == null || address.trim().isEmpty())) {
            throw new IllegalArgumentException("Name and address cannot both be empty (fail code: 4)");
        }
        
        Customer customer = customerRepository.findById(sortCode, customerNumber)
            .orElseThrow(() -> new IllegalArgumentException("Customer not found (fail code: 1)"));
        
        if (name != null && !name.trim().isEmpty()) {
            validateTitle(name);
            customer.setName(name);
        }
        if (address != null && !address.trim().isEmpty()) {
            customer.setAddress(address);
        }
        
        return customerRepository.save(customer);
    }
    
    private void validateTitle(String name) {
        String[] validTitles = {
            "Professor", "Mr", "Mrs", "Miss", "Ms", "Dr", "Drs", 
            "Lord", "Sir", "Lady", ""
        };
        
        String title = name.split(" ")[0];
        boolean validTitle = Arrays.asList(validTitles).contains(title);
        
        if (!validTitle) {
            throw new IllegalArgumentException("Invalid title (fail code: T)");
        }
    }
    
    @Transactional
    public void deleteCustomer(String sortCode, Long customerNumber) {
        Customer customer = customerRepository.findById(sortCode, customerNumber)
            .orElseThrow(() -> new IllegalArgumentException("Customer not found (fail code: 1)"));
        
        List<Account> accounts = accountRepository.findByCustomerNumber(customerNumber);
        
        for (Account account : accounts) {
            try {
                accountRepository.deleteById(account.getSortCode(), account.getAccountNumber());
            } catch (Exception e) {
                logger.warn("Account already deleted: {}-{}", account.getSortCode(), account.getAccountNumber());
            }
        }
        
        boolean deleted = customerRepository.deleteById(sortCode, customerNumber);
        if (!deleted) {
            throw new IllegalStateException("Failed to delete customer");
        }
        
        writeCustomerAudit(customer, Transaction.TYPE_BRANCH_DELETE_CUSTOMER);
    }
    
    private void writeCustomerAudit(Customer customer, String transactionType) {
        String description = String.format("%s-%d %s %s", 
            customer.getSortCode(), 
            customer.getCustomerNumber(),
            customer.getName().substring(0, Math.min(14, customer.getName().length())),
            customer.getDateOfBirth().toString());
        
        Transaction audit = Transaction.builder()
            .eyeCatcher("PRTR")
            .logicallyDeleted(false)
            .sortCode(customer.getSortCode())
            .accountNumber("00000000")
            .transactionDate(LocalDate.now())
            .transactionTime(LocalTime.now())
            .referenceNumber(System.currentTimeMillis())
            .transactionType(transactionType)
            .description(description)
            .amount(BigDecimal.ZERO)
            .build();
        
        transactionRepository.save(audit);
    }
}
