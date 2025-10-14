package com.cbsa.migration.datagen;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * Configuration parameters for data generation
 * Mirrors COBOL BANKDATA parameters: START-KEY, END-KEY, STEP-KEY, RANDOM-SEED
 */
@Component
@ConfigurationProperties(prefix = "data.generation")
public class DataGenerationParams {
    
    private boolean enabled = true;
    private int customerStart = 1;
    private int customerEnd = 100;
    private int customerStep = 1;
    private long seed = System.currentTimeMillis();
    private int transactionDays = 30;
    
    // Getters and setters
    public boolean isEnabled() {
        return enabled;
    }
    
    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }
    
    public int getCustomerStart() {
        return customerStart;
    }
    
    public void setCustomerStart(int customerStart) {
        this.customerStart = customerStart;
    }
    
    public int getCustomerEnd() {
        return customerEnd;
    }
    
    public void setCustomerEnd(int customerEnd) {
        this.customerEnd = customerEnd;
    }
    
    public int getCustomerStep() {
        return customerStep;
    }
    
    public void setCustomerStep(int customerStep) {
        this.customerStep = customerStep;
    }
    
    public long getSeed() {
        return seed;
    }
    
    public void setSeed(long seed) {
        this.seed = seed;
    }
    
    public int getTransactionDays() {
        return transactionDays;
    }
    
    public void setTransactionDays(int transactionDays) {
        this.transactionDays = transactionDays;
    }
    
    @Override
    public String toString() {
        return String.format("DataGenerationParams[enabled=%s, customers=%d-%d step %d, seed=%d, transactionDays=%d]",
            enabled, customerStart, customerEnd, customerStep, seed, transactionDays);
    }
}
