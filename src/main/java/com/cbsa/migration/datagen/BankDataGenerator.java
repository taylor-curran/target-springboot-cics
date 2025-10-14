package com.cbsa.migration.datagen;

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
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Bank data generator service - migrated from COBOL BANKDATA.cbl
 * Generates realistic test data for customers, accounts, and transactions
 */
@Service
@Transactional
public class BankDataGenerator {
    
    private static final Logger log = LoggerFactory.getLogger(BankDataGenerator.class);
    
    @Autowired
    private JdbcTemplate jdbcTemplate;
    
    @Autowired
    private CustomerRepository customerRepository;
    
    @Autowired
    private AccountRepository accountRepository;
    
    @Autowired
    private ControlRepository controlRepository;
    
    @Autowired
    private TransactionRepository transactionRepository;
    
    @Autowired
    private DataGenerationParams params;
    
    @Autowired
    private TransactionGenerator transactionGenerator;
    
    private static final String SORTCODE = "987654"; // Default sort code from tests
    
    /**
     * Main data generation method - mirrors COBOL BANKDATA logic
     * @param start Starting customer number (COBOL: START-KEY)
     * @param end Ending customer number (COBOL: END-KEY)
     * @param step Customer number increment (COBOL: STEP-KEY)
     * @param seed Random seed for reproducibility (COBOL: RANDOM-SEED)
     */
    public void generateData(int start, int end, int step, long seed) {
        log.info("Starting data generation: customers {}-{} step {}, seed {}", start, end, step, seed);
        
        // Validate parameters (mirrors COBOL validation)
        if (end < start) {
            throw new IllegalArgumentException("Final customer number cannot be smaller than first customer number");
        }
        if (step == 0) {
            throw new IllegalArgumentException("Gap between customers cannot be zero");
        }
        
        // Clear existing data (mirrors COBOL DELETE-DB2-ROWS)
        clearAllData();
        
        // Initialize control record
        initializeControlRecord();
        
        // Initialize random with seed (mirrors COBOL FUNCTION RANDOM)
        Random random = new Random(seed);
        
        int customerCount = 0;
        int accountCount = 0;
        int transactionCount = 0;
        
        // Collect all accounts for transaction generation
        List<Account> allAccounts = new ArrayList<>();
        
        // Generate customers (mirrors COBOL main loop)
        for (int customerNumber = start; customerNumber <= end; customerNumber += step) {
            Customer customer = generateCustomer(customerNumber, random);
            customerRepository.save(customer);
            customerCount++;
            
            // Generate 1-5 accounts per customer (mirrors COBOL DEFINE-ACC)
            int numAccounts = random.nextInt(5) + 1;
            for (int i = 0; i < numAccounts; i++) {
                Account account = generateAccount(customerNumber, accountCount + 1, random);
                accountRepository.save(account);
                accountCount++;
                allAccounts.add(account);
            }
            
            // Commit periodically for large datasets (mirrors COBOL COMMIT-COUNT)
            if (customerCount % 1000 == 0) {
                log.debug("Generated {} customers so far", customerCount);
            }
        }
        
        // Generate transactions for all accounts (Phase 2 - NEW)
        log.info("Generating transactions for {} accounts...", allAccounts.size());
        for (Account account : allAccounts) {
            List<Transaction> transactions = transactionGenerator.generateTransactionsForAccount(
                account, 
                params.getTransactionDays(), 
                random
            );
            transactionGenerator.saveTransactions(transactions);
            transactionCount += transactions.size();
            
            // Update the account with final balance after all transactions
            accountRepository.save(account);
        }
        
        // Update control counters
        updateControlCounters(customerCount, accountCount);
        
        log.info("Data generation complete: {} customers, {} accounts, {} transactions", 
            customerCount, accountCount, transactionCount);
    }
    
    /**
     * Clear all data from tables - mirrors COBOL DELETE-DB2-ROWS
     */
    private void clearAllData() {
        log.info("Clearing existing data...");
        
        // Delete in reverse dependency order
        jdbcTemplate.update("DELETE FROM bank_transaction");
        jdbcTemplate.update("DELETE FROM account");
        jdbcTemplate.update("DELETE FROM customer");
        jdbcTemplate.update("DELETE FROM control");
        jdbcTemplate.update("DELETE FROM application_error");
        
        log.info("All data cleared");
    }
    
    /**
     * Initialize control record with zeros
     */
    private void initializeControlRecord() {
        Control control = new Control();
        // Control has a static ID "CONTROL" - no need to set it
        control.setCustomerCount(0L);
        control.setLastCustomerNumber(0L);
        control.setAccountCount(0);
        control.setLastAccountNumber(0);
        controlRepository.save(control);
        log.debug("Control record initialized");
    }
    
    /**
     * Generate a customer - mirrors COBOL customer generation logic
     */
    private Customer generateCustomer(int customerNumber, Random random) {
        Customer customer = new Customer();
        
        // Set eye catcher (COBOL: SET CUSTOMER-EYECATCHER-VALUE TO TRUE)
        customer.setEyeCatcher("CUST");
        
        // Set sort code and customer number
        customer.setSortCode(SORTCODE);
        customer.setCustomerNumber((long) customerNumber);
        
        // Generate name (mirrors COBOL STRING operation)
        String title = RandomDataArrays.TITLES[random.nextInt(RandomDataArrays.TITLES.length)];
        String forename = RandomDataArrays.FORENAMES[random.nextInt(RandomDataArrays.FORENAMES.length)];
        char initial = RandomDataArrays.INITIALS.charAt(random.nextInt(RandomDataArrays.INITIALS.length()));
        String surname = RandomDataArrays.SURNAMES[random.nextInt(RandomDataArrays.SURNAMES.length)];
        
        String name = String.format("%s %s %c %s", title, forename, initial, surname);
        customer.setName(name);
        
        // Generate address (mirrors COBOL STRING operation)
        int houseNumber = random.nextInt(99) + 1;
        String streetTree = RandomDataArrays.STREET_TREES[random.nextInt(RandomDataArrays.STREET_TREES.length)];
        String streetRoad = RandomDataArrays.STREET_ROADS[random.nextInt(RandomDataArrays.STREET_ROADS.length)];
        String town = RandomDataArrays.TOWNS[random.nextInt(RandomDataArrays.TOWNS.length)];
        
        String address = String.format("%d %s %s, %s", houseNumber, streetTree, streetRoad, town);
        customer.setAddress(address);
        
        // Generate date of birth (mirrors COBOL date generation)
        int birthYear = 1900 + random.nextInt(100); // 1900-2000
        int birthMonth = random.nextInt(12) + 1;     // 1-12
        int birthDay = random.nextInt(28) + 1;       // 1-28 (safe for all months)
        
        LocalDate dateOfBirth = LocalDate.of(birthYear, birthMonth, birthDay);
        customer.setDateOfBirth(dateOfBirth);
        
        // Generate credit score (mirrors COBOL: 1-999)
        int creditScore = random.nextInt(999) + 1;
        customer.setCreditScore(creditScore);
        
        // Generate credit score review date (1-21 days from today)
        int daysToAdd = random.nextInt(21) + 1;
        LocalDate reviewDate = LocalDate.now().plusDays(daysToAdd);
        customer.setCreditScoreReviewDate(reviewDate);
        
        return customer;
    }
    
    /**
     * Generate an account - mirrors COBOL POPULATE-ACC
     */
    private Account generateAccount(int customerNumber, int accountNumber, Random random) {
        Account account = new Account();
        
        // Set eye catcher
        account.setEyeCatcher("ACCT");
        
        // Set customer and sort code
        account.setCustomerNumber((long) customerNumber);
        account.setSortCode(SORTCODE);
        
        // Generate 8-digit account number
        String accountNumStr = String.format("%08d", accountNumber);
        account.setAccountNumber(accountNumStr);
        
        // Select random account type
        int typeIndex = random.nextInt(RandomDataArrays.ACCOUNT_TYPES.length);
        String accountType = RandomDataArrays.ACCOUNT_TYPES[typeIndex];
        account.setAccountType(accountType);
        
        // Set interest rate based on account type
        double interestRate = RandomDataArrays.INTEREST_RATES[typeIndex];
        account.setInterestRate(BigDecimal.valueOf(interestRate));
        
        // Set account opened date (random date in past year)
        int daysAgo = random.nextInt(365);
        LocalDate openedDate = LocalDate.now().minusDays(daysAgo);
        account.setOpenedDate(openedDate);
        
        // Set overdraft limit based on account type
        int overdraftLimit = RandomDataArrays.OVERDRAFT_LIMITS[typeIndex];
        account.setOverdraftLimit(overdraftLimit);
        
        // Generate statement dates
        if (daysAgo > 30) {
            LocalDate lastStatement = openedDate.plusMonths(1);
            account.setLastStatementDate(lastStatement);
            
            LocalDate nextStatement = lastStatement.plusMonths(1);
            if (nextStatement.isAfter(LocalDate.now())) {
                account.setNextStatementDate(nextStatement);
            }
        }
        
        // Generate random balance (mirrors COBOL: various ranges)
        BigDecimal balance = generateRandomBalance(accountType, random);
        account.setAvailableBalance(balance);
        account.setActualBalance(balance);
        
        return account;
    }
    
    /**
     * Generate a realistic balance based on account type
     */
    private BigDecimal generateRandomBalance(String accountType, Random random) {
        switch (accountType) {
            case "ISA":
                // ISA: £0 - £20,000
                return new BigDecimal(random.nextInt(20000));
            case "SAVING":
                // Savings: £0 - £50,000
                return new BigDecimal(random.nextInt(50000));
            case "CURRENT":
                // Current: -£100 - £10,000 (can be overdrawn)
                return new BigDecimal(random.nextInt(10100) - 100);
            case "LOAN":
                // Loan: -£50,000 - £0 (negative balance = outstanding loan)
                return new BigDecimal(-random.nextInt(50000));
            case "MORTGAGE":
                // Mortgage: -£500,000 - £0 (negative balance = outstanding mortgage)
                return new BigDecimal(-random.nextInt(500000));
            default:
                return BigDecimal.ZERO;
        }
    }
    
    /**
     * Update control counters after generation
     */
    private void updateControlCounters(int customerCount, int accountCount) {
        Control control = controlRepository.getControl().orElseThrow(
            () -> new IllegalStateException("Control record not found"));
        
        control.setCustomerCount((long) customerCount);
        control.setLastCustomerNumber((long) customerCount);
        control.setAccountCount(accountCount);
        control.setLastAccountNumber(accountCount);
        
        controlRepository.save(control);
        log.debug("Control counters updated: {} customers, {} accounts", customerCount, accountCount);
    }
    
    /**
     * Convenience method using default parameters from configuration
     */
    public void generateData() {
        generateData(
            params.getCustomerStart(),
            params.getCustomerEnd(),
            params.getCustomerStep(),
            params.getSeed()
        );
    }
}
