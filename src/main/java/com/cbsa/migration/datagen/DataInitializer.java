package com.cbsa.migration.datagen;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.annotation.Profile;
import org.springframework.context.event.EventListener;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * Automatically initializes database with test data on startup if empty
 * Only runs in non-production and non-test environments
 */
@Component
@Profile("!production & !test") // Never run in production or test
public class DataInitializer {
    
    private static final Logger log = LoggerFactory.getLogger(DataInitializer.class);
    
    @Autowired
    private DataGenerationParams params;
    
    @Autowired
    private BankDataGenerator dataGenerator;
    
    @Autowired
    private JdbcTemplate jdbcTemplate;
    
    /**
     * Check if database is empty and generate data if needed
     */
    @EventListener(ApplicationReadyEvent.class)
    public void onApplicationReady() {
        if (!params.isEnabled()) {
            log.info("Data generation is disabled");
            return;
        }
        
        if (isDatabaseEmpty()) {
            log.info("Database is empty - generating test data...");
            
            try {
                dataGenerator.generateData(
                    params.getCustomerStart(),
                    params.getCustomerEnd(),
                    params.getCustomerStep(),
                    params.getSeed()
                );
                
                // Log summary statistics
                int customerCount = jdbcTemplate.queryForObject(
                    "SELECT COUNT(*) FROM customer", Integer.class);
                int accountCount = jdbcTemplate.queryForObject(
                    "SELECT COUNT(*) FROM account", Integer.class);
                
                log.info("Test data generated successfully: {} customers, {} accounts", 
                    customerCount, accountCount);
                
            } catch (Exception e) {
                log.error("Failed to generate test data: {}", e.getMessage(), e);
            }
        } else {
            int customerCount = jdbcTemplate.queryForObject(
                "SELECT COUNT(*) FROM customer", Integer.class);
            log.info("Database already contains data: {} customers found", customerCount);
        }
    }
    
    /**
     * Check if database is empty (no customers)
     */
    private boolean isDatabaseEmpty() {
        try {
            Integer count = jdbcTemplate.queryForObject(
                "SELECT COUNT(*) FROM customer", Integer.class);
            return count == null || count == 0;
        } catch (Exception e) {
            // Table might not exist yet
            log.debug("Could not check if database is empty: {}", e.getMessage());
            return false;
        }
    }
}
