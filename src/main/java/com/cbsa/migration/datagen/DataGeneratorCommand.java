package com.cbsa.migration.datagen;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

/**
 * Command line interface for data generation
 * Usage: mvn spring-boot:run -Dspring-boot.run.arguments="generate-data"
 * Or with parameters: mvn spring-boot:run -Dspring-boot.run.arguments="generate-data --start=1 --end=50 --seed=12345"
 */
@Component
public class DataGeneratorCommand implements CommandLineRunner {
    
    private static final Logger log = LoggerFactory.getLogger(DataGeneratorCommand.class);
    
    @Autowired
    private BankDataGenerator dataGenerator;
    
    @Autowired
    private DataGenerationParams params;
    
    @Override
    public void run(String... args) throws Exception {
        // Only run if explicitly requested via command line
        if (args.length > 0 && "generate-data".equals(args[0])) {
            log.info("Manual data generation requested");
            
            // Parse optional parameters
            int start = params.getCustomerStart();
            int end = params.getCustomerEnd();
            int step = params.getCustomerStep();
            long seed = params.getSeed();
            
            for (int i = 1; i < args.length; i++) {
                String arg = args[i];
                if (arg.startsWith("--start=")) {
                    start = Integer.parseInt(arg.substring(8));
                } else if (arg.startsWith("--end=")) {
                    end = Integer.parseInt(arg.substring(6));
                } else if (arg.startsWith("--step=")) {
                    step = Integer.parseInt(arg.substring(7));
                } else if (arg.startsWith("--seed=")) {
                    seed = Long.parseLong(arg.substring(7));
                }
            }
            
            log.info("Generating data with parameters: start={}, end={}, step={}, seed={}", 
                start, end, step, seed);
            
            try {
                dataGenerator.generateData(start, end, step, seed);
                log.info("Data generation completed successfully");
            } catch (Exception e) {
                log.error("Data generation failed: {}", e.getMessage(), e);
                System.exit(1);
            }
            
            // Exit after generation to avoid running the web server
            System.exit(0);
        }
    }
}
