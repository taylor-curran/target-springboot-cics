package com.cbsa.migration.datagen;

import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.TransactionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Generates realistic transaction history for accounts
 * This is new functionality not in COBOL but needed by the Java application
 */
@Component
public class TransactionGenerator {
    
    private static final Logger log = LoggerFactory.getLogger(TransactionGenerator.class);
    
    @Autowired
    private TransactionRepository transactionRepository;
    
    // Transaction type weights (must sum to 100)
    private static final int CREDIT_WEIGHT = 40;  // 40% deposits
    private static final int DEBIT_WEIGHT = 40;   // 40% withdrawals
    private static final int TRANSFER_WEIGHT = 15; // 15% transfers
    private static final int FEE_WEIGHT = 5;       // 5% fees
    
    // Amount ranges by transaction type
    private static final BigDecimal MIN_CREDIT = new BigDecimal("10");
    private static final BigDecimal MAX_CREDIT = new BigDecimal("5000");
    private static final BigDecimal MIN_DEBIT = new BigDecimal("5");
    private static final BigDecimal MAX_DEBIT = new BigDecimal("500");
    private static final BigDecimal MIN_TRANSFER = new BigDecimal("10");
    private static final BigDecimal MAX_TRANSFER = new BigDecimal("1000");
    private static final BigDecimal[] STANDARD_FEES = {
        new BigDecimal("2.50"),
        new BigDecimal("5.00"),
        new BigDecimal("10.00"),
        new BigDecimal("25.00")
    };
    
    /**
     * Generate transactions for an account over a period of days
     * @param account The account to generate transactions for
     * @param daysOfHistory How many days of history to generate
     * @param random Random number generator for consistency
     * @return List of generated transactions
     */
    public List<Transaction> generateTransactionsForAccount(Account account, int daysOfHistory, Random random) {
        List<Transaction> transactions = new ArrayList<>();
        
        LocalDate today = LocalDate.now();
        LocalDate startDate = today.minusDays(daysOfHistory);
        
        // Generate variable number of transactions based on account type
        int transactionsPerWeek = getTransactionsPerWeek(account.getAccountType(), random);
        
        // Spread transactions across the period
        LocalDate currentDate = startDate;
        long referenceNumber = 1000000L + (account.getCustomerNumber() * 1000);
        
        while (currentDate.isBefore(today) || currentDate.isEqual(today)) {
            // Random chance of transaction on this day
            if (random.nextInt(7) < transactionsPerWeek) {
                Transaction transaction = generateTransaction(
                    account, 
                    currentDate, 
                    referenceNumber++, 
                    random
                );
                transactions.add(transaction);
                
                // Update account balance based on transaction
                updateAccountBalance(account, transaction);
                
                // Small chance of multiple transactions on same day
                if (random.nextInt(100) < 20) { // 20% chance
                    Transaction secondTransaction = generateTransaction(
                        account,
                        currentDate,
                        referenceNumber++,
                        random
                    );
                    transactions.add(secondTransaction);
                    updateAccountBalance(account, secondTransaction);
                }
            }
            currentDate = currentDate.plusDays(1);
        }
        
        return transactions;
    }
    
    /**
     * Generate a single transaction
     */
    private Transaction generateTransaction(Account account, LocalDate date, long referenceNumber, Random random) {
        Transaction transaction = new Transaction();
        
        // Set basic fields
        transaction.setEyeCatcher("PRTR");
        transaction.setLogicallyDeleted(false);
        transaction.setSortCode(account.getSortCode());
        transaction.setAccountNumber(account.getAccountNumber());
        transaction.setTransactionDate(date);
        transaction.setTransactionTime(generateRandomTime(random));
        transaction.setReferenceNumber(referenceNumber);
        
        // Select transaction type based on weights
        String transactionType = selectTransactionType(random);
        transaction.setTransactionType(transactionType);
        
        // Generate amount and description based on type
        switch (transactionType) {
            case "CRE":
                transaction.setAmount(generateCreditAmount(random));
                transaction.setDescription(generateCreditDescription(random));
                break;
            case "DEB":
                transaction.setAmount(generateDebitAmount(random));
                transaction.setDescription(generateDebitDescription(random));
                break;
            case "TFR":
                transaction.setAmount(generateTransferAmount(random));
                transaction.setDescription(generateTransferDescription(random));
                // For transfers, set target account (random)
                transaction.setTargetSortCode("123456");
                transaction.setTargetAccountNumber(String.format("%08d", random.nextInt(99999999)));
                break;
            case "FEE":
                transaction.setAmount(generateFeeAmount(random));
                transaction.setDescription(generateFeeDescription(random));
                break;
            case "INT":
                // Interest calculation based on account balance
                BigDecimal interestAmount = calculateInterest(account);
                transaction.setAmount(interestAmount);
                transaction.setDescription("Monthly Interest Credit");
                break;
        }
        
        return transaction;
    }
    
    /**
     * Select transaction type based on weights
     */
    private String selectTransactionType(Random random) {
        int value = random.nextInt(100);
        
        if (value < CREDIT_WEIGHT) {
            return "CRE";
        } else if (value < CREDIT_WEIGHT + DEBIT_WEIGHT) {
            return "DEB";
        } else if (value < CREDIT_WEIGHT + DEBIT_WEIGHT + TRANSFER_WEIGHT) {
            return "TFR";
        } else {
            return "FEE";
        }
    }
    
    /**
     * Generate random time of day for transaction
     */
    private LocalTime generateRandomTime(Random random) {
        // Business hours 9 AM to 5 PM with some outliers
        int hour = 9 + random.nextInt(8);
        if (random.nextInt(100) < 10) { // 10% chance of after-hours
            hour = 6 + random.nextInt(18); // 6 AM to midnight
        }
        int minute = random.nextInt(60);
        int second = random.nextInt(60);
        return LocalTime.of(hour, minute, second);
    }
    
    /**
     * Determine transactions per week based on account type
     */
    private int getTransactionsPerWeek(String accountType, Random random) {
        switch (accountType) {
            case "CURRENT":
                return 3 + random.nextInt(5); // 3-7 transactions per week
            case "SAVING":
                return 1 + random.nextInt(2); // 1-2 transactions per week
            case "ISA":
                return random.nextInt(2);     // 0-1 transactions per week
            case "LOAN":
                return 1;                      // 1 transaction per week (payment)
            case "MORTGAGE":
                return 1;                      // 1 transaction per month
            default:
                return 2;
        }
    }
    
    // Amount generation methods
    private BigDecimal generateCreditAmount(Random random) {
        double range = MAX_CREDIT.subtract(MIN_CREDIT).doubleValue();
        double amount = MIN_CREDIT.doubleValue() + (range * random.nextDouble());
        return new BigDecimal(amount).setScale(2, RoundingMode.HALF_UP);
    }
    
    private BigDecimal generateDebitAmount(Random random) {
        double range = MAX_DEBIT.subtract(MIN_DEBIT).doubleValue();
        double amount = MIN_DEBIT.doubleValue() + (range * random.nextDouble());
        return new BigDecimal(amount).setScale(2, RoundingMode.HALF_UP);
    }
    
    private BigDecimal generateTransferAmount(Random random) {
        double range = MAX_TRANSFER.subtract(MIN_TRANSFER).doubleValue();
        double amount = MIN_TRANSFER.doubleValue() + (range * random.nextDouble());
        return new BigDecimal(amount).setScale(2, RoundingMode.HALF_UP);
    }
    
    private BigDecimal generateFeeAmount(Random random) {
        return STANDARD_FEES[random.nextInt(STANDARD_FEES.length)];
    }
    
    private BigDecimal calculateInterest(Account account) {
        BigDecimal balance = account.getActualBalance();
        BigDecimal rate = account.getInterestRate();
        // Monthly interest = balance * (annual rate / 12) / 100
        return balance.multiply(rate)
            .divide(new BigDecimal("1200"), 2, RoundingMode.HALF_UP);
    }
    
    // Description generation methods
    private String generateCreditDescription(Random random) {
        String[] descriptions = RandomDataArrays.CREDIT_DESCRIPTIONS;
        return descriptions[random.nextInt(descriptions.length)];
    }
    
    private String generateDebitDescription(Random random) {
        String[] descriptions = RandomDataArrays.DEBIT_DESCRIPTIONS;
        return descriptions[random.nextInt(descriptions.length)];
    }
    
    private String generateTransferDescription(Random random) {
        String[] templates = {
            "Transfer to %s",
            "Payment to %s",
            "Transfer - %s",
            "Online transfer to %s"
        };
        String template = templates[random.nextInt(templates.length)];
        String name = RandomDataArrays.SURNAMES[random.nextInt(RandomDataArrays.SURNAMES.length)];
        return String.format(template, name);
    }
    
    private String generateFeeDescription(Random random) {
        String[] descriptions = RandomDataArrays.FEE_DESCRIPTIONS;
        return descriptions[random.nextInt(descriptions.length)];
    }
    
    /**
     * Update account balance based on transaction
     * Note: This modifies the account object but doesn't persist it
     */
    private void updateAccountBalance(Account account, Transaction transaction) {
        BigDecimal amount = transaction.getAmount();
        BigDecimal currentBalance = account.getActualBalance();
        
        String type = transaction.getTransactionType();
        if ("CRE".equals(type) || "INT".equals(type)) {
            // Credits increase balance
            account.setActualBalance(currentBalance.add(amount));
            account.setAvailableBalance(account.getActualBalance());
        } else if ("DEB".equals(type) || "FEE".equals(type) || "TFR".equals(type)) {
            // Debits decrease balance
            account.setActualBalance(currentBalance.subtract(amount));
            account.setAvailableBalance(account.getActualBalance());
        }
    }
    
    /**
     * Save all transactions to database
     */
    public void saveTransactions(List<Transaction> transactions) {
        log.debug("Saving {} transactions", transactions.size());
        for (Transaction transaction : transactions) {
            transactionRepository.save(transaction);
        }
    }
}
