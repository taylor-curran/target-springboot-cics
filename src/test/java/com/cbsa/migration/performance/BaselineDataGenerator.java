package com.cbsa.migration.performance;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

import java.io.File;
import java.io.IOException;
import java.time.Instant;
import java.util.*;

public class BaselineDataGenerator {

    private static class ProgramConfig {
        String name;
        String operation;
        String description;
        long meanLatencyMs;
        long stdDevMs;

        ProgramConfig(String name, String operation, String description, long meanLatencyMs, long stdDevMs) {
            this.name = name;
            this.operation = operation;
            this.description = description;
            this.meanLatencyMs = meanLatencyMs;
            this.stdDevMs = stdDevMs;
        }
    }

    private static final List<ProgramConfig> PROGRAM_CONFIGS = Arrays.asList(
        new ProgramConfig("INQCUST", "Customer Read", 
            "Retrieve customer by composite key (sortcode + customer_number)", 85, 35),
        new ProgramConfig("CRECUST", "Customer Create",
            "Create new customer with async credit check and Named Counter", 350, 100),
        new ProgramConfig("UPDCUST", "Customer Update",
            "Update customer fields (limited fields, no PROCTRAN)", 120, 40),
        new ProgramConfig("DELCUS", "Customer Delete",
            "Delete customer with cascade to all accounts and PROCTRAN logging", 280, 80),
        new ProgramConfig("INQACC", "Account Read",
            "Retrieve single account by account number", 75, 30),
        new ProgramConfig("INQACCCU", "Account List by Customer",
            "List all accounts for a customer with cursor pagination", 95, 35),
        new ProgramConfig("CREACC", "Account Create",
            "Create new account with Named Counter for account number", 320, 90),
        new ProgramConfig("UPDACC", "Account Update",
            "Update account fields with business rule validation", 140, 45),
        new ProgramConfig("DELACC", "Account Delete",
            "Delete account with PROCTRAN audit logging", 200, 60),
        new ProgramConfig("XFRFUN", "Transfer Funds",
            "Atomic dual-account transfer with rollback on failure", 450, 120),
        new ProgramConfig("DBCRFUN", "Debit/Credit Funds",
            "Single account debit or credit with balance updates", 180, 55)
    );

    public static void main(String[] args) throws IOException {
        System.out.println("Generating performance baseline data for 11 COBOL programs...");
        
        Random random = new Random(12345);
        int samplesPerProgram = 100;
        
        Map<String, Object> baseline = new LinkedHashMap<>();
        baseline.put("baseline_version", "1.0");
        baseline.put("generated_timestamp", Instant.now().toString());
        baseline.put("environment", "COBOL/CICS Mainframe (Simulated)");
        baseline.put("sample_methodology", "Normal distribution with realistic latency parameters based on operation complexity");
        
        List<Map<String, Object>> programs = new ArrayList<>();
        int totalSamples = 0;
        
        for (ProgramConfig config : PROGRAM_CONFIGS) {
            System.out.printf("  Generating samples for %s (%s)...%n", config.name, config.operation);
            
            List<Long> samples = PerformanceTestFramework.generateSimulatedSamples(
                samplesPerProgram, config.meanLatencyMs, config.stdDevMs, random);
            
            PerformanceTestFramework.PerformanceMetrics metrics = 
                new PerformanceTestFramework.PerformanceMetrics(config.name, samples);
            
            Map<String, Object> programData = new LinkedHashMap<>();
            programData.put("program_name", config.name);
            programData.put("operation", config.operation);
            programData.put("description", config.description);
            programData.put("sample_count", metrics.getSampleCount());
            
            Map<String, Double> latencyData = new LinkedHashMap<>();
            latencyData.put("p50", Math.round(metrics.getP50() * 10.0) / 10.0);
            latencyData.put("p95", Math.round(metrics.getP95() * 10.0) / 10.0);
            latencyData.put("p99", Math.round(metrics.getP99() * 10.0) / 10.0);
            programData.put("latency_ms", latencyData);
            
            programs.add(programData);
            totalSamples += metrics.getSampleCount();
            
            System.out.printf("    %s%n", metrics);
        }
        
        baseline.put("programs", programs);
        
        Map<String, Object> summary = new LinkedHashMap<>();
        summary.put("total_programs", PROGRAM_CONFIGS.size());
        summary.put("total_samples", totalSamples);
        summary.put("samples_per_program", samplesPerProgram);
        summary.put("notes", "Baseline metrics represent typical COBOL/CICS transaction performance for migration validation. " +
            "These simulated baselines establish performance targets that Java implementations should meet or exceed.");
        baseline.put("summary", summary);
        
        ObjectMapper mapper = new ObjectMapper();
        mapper.enable(SerializationFeature.INDENT_OUTPUT);
        
        File outputFile = new File("performance_baseline.json");
        mapper.writeValue(outputFile, baseline);
        
        System.out.printf("%nBaseline data written to: %s%n", outputFile.getAbsolutePath());
        System.out.printf("Total programs: %d%n", PROGRAM_CONFIGS.size());
        System.out.printf("Total samples: %d%n", totalSamples);
        System.out.println("\nPerformance baseline generation complete!");
    }
}
