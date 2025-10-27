package com.cbsa.migration;

import com.cbsa.migration.datagen.PerformanceBaselineGenerator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Profile;

@SpringBootApplication
@Profile("generate-baselines")
public class PerformanceBaselineRunner implements CommandLineRunner {
    
    @Autowired
    private PerformanceBaselineGenerator generator;
    
    public static void main(String[] args) {
        System.setProperty("spring.profiles.active", "generate-baselines");
        SpringApplication.run(PerformanceBaselineRunner.class, args);
    }
    
    @Override
    public void run(String... args) throws Exception {
        generator.generateBaselines();
    }
}
