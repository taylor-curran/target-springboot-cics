package com.cbsa.migration;

import com.cbsa.migration.datagen.PerformanceBaselineGenerator;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

@Component
@Profile("generate-baselines")
public class PerformanceBaselineRunner implements CommandLineRunner {
    
    private final PerformanceBaselineGenerator generator;
    
    public PerformanceBaselineRunner(PerformanceBaselineGenerator generator) {
        this.generator = generator;
    }
    
    @Override
    public void run(String... args) throws Exception {
        generator.generateBaselines();
    }
}
