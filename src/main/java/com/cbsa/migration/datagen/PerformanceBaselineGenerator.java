package com.cbsa.migration.datagen;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

@Service
public class PerformanceBaselineGenerator {
    
    private static final Logger log = LoggerFactory.getLogger(PerformanceBaselineGenerator.class);
    private static final int SAMPLES_PER_PROGRAM = 100;
    private static final long SEED = 12345L;
    
    private static final double READ_BASE_LATENCY = 25.0;
    private static final double WRITE_BASE_LATENCY = 50.0;
    private static final double ATOMIC_BASE_LATENCY = 120.0;
    
    private static final double LOW_COMPLEXITY = 1.0;
    private static final double MEDIUM_COMPLEXITY = 1.5;
    private static final double HIGH_COMPLEXITY = 2.0;
    
    private static final List<ProgramDefinition> PROGRAMS = Arrays.asList(
        new ProgramDefinition("CRECUST", "Customer Create", "write", "high", 1440),
        new ProgramDefinition("INQCUST", "Customer Read", "read", "medium", 712),
        new ProgramDefinition("UPDCUST", "Customer Update", "write", "low", 365),
        new ProgramDefinition("DELCUS", "Customer Delete", "write", "medium", 762),
        new ProgramDefinition("CREACC", "Account Create", "write", "high", 1248),
        new ProgramDefinition("INQACC", "Account Read", "read", "medium", 1003),
        new ProgramDefinition("INQACCCU", "Account Read All (Customer)", "read", "medium", 883),
        new ProgramDefinition("UPDACC", "Account Update", "write", "low", 407),
        new ProgramDefinition("DELACC", "Account Delete", "write", "medium", 650),
        new ProgramDefinition("XFRFUN", "Transfer Funds", "atomic", "high", 1925),
        new ProgramDefinition("DBCRFUN", "Debit/Credit", "write", "medium", 862)
    );
    
    public void generateBaselines() throws IOException {
        log.info("Generating performance baselines for {} programs", PROGRAMS.size());
        
        Random random = new Random(SEED);
        Map<String, Object> baselineData = new LinkedHashMap<>();
        
        baselineData.put("generated_at", LocalDateTime.now().format(DateTimeFormatter.ISO_DATE_TIME));
        baselineData.put("description", "Synthetic performance baselines for COBOL program migration validation");
        baselineData.put("samples_per_program", SAMPLES_PER_PROGRAM);
        baselineData.put("seed", SEED);
        
        List<Map<String, Object>> programBaselines = new ArrayList<>();
        
        for (ProgramDefinition program : PROGRAMS) {
            Map<String, Object> baseline = generateProgramBaseline(program, random);
            programBaselines.add(baseline);
            log.info("Generated baseline for {}: P50={}, P95={}, P99={}", 
                program.name, baseline.get("p50_ms"), baseline.get("p95_ms"), baseline.get("p99_ms"));
        }
        
        baselineData.put("programs", programBaselines);
        
        ObjectMapper mapper = new ObjectMapper();
        mapper.enable(SerializationFeature.INDENT_OUTPUT);
        mapper.writeValue(new File("performance_baseline.json"), baselineData);
        log.info("Wrote performance_baseline.json");
        
        generateDashboard(programBaselines);
        log.info("Wrote performance_dashboard.html");
        
        log.info("Performance baseline generation complete");
    }
    
    private Map<String, Object> generateProgramBaseline(ProgramDefinition program, Random random) {
        double baseLatency = getBaseLatency(program.operationType);
        double complexityMultiplier = getComplexityMultiplier(program.complexity);
        double meanLatency = baseLatency * complexityMultiplier;
        
        double stddev = meanLatency * 0.2;
        List<Double> samples = new ArrayList<>();
        
        for (int i = 0; i < SAMPLES_PER_PROGRAM; i++) {
            double sample = meanLatency + (random.nextGaussian() * stddev);
            sample = Math.max(1.0, sample);
            samples.add(sample);
        }
        
        Collections.sort(samples);
        
        Map<String, Object> baseline = new LinkedHashMap<>();
        baseline.put("program_name", program.name);
        baseline.put("description", program.description);
        baseline.put("operation_type", program.operationType);
        baseline.put("complexity", program.complexity);
        baseline.put("cobol_lines", program.lines);
        baseline.put("sample_count", samples.size());
        baseline.put("p50_ms", Math.round(getPercentile(samples, 50) * 100.0) / 100.0);
        baseline.put("p95_ms", Math.round(getPercentile(samples, 95) * 100.0) / 100.0);
        baseline.put("p99_ms", Math.round(getPercentile(samples, 99) * 100.0) / 100.0);
        baseline.put("mean_ms", Math.round(samples.stream().mapToDouble(Double::doubleValue).average().orElse(0.0) * 100.0) / 100.0);
        baseline.put("min_ms", Math.round(samples.get(0) * 100.0) / 100.0);
        baseline.put("max_ms", Math.round(samples.get(samples.size() - 1) * 100.0) / 100.0);
        
        return baseline;
    }
    
    private double getBaseLatency(String operationType) {
        switch (operationType.toLowerCase()) {
            case "read": return READ_BASE_LATENCY;
            case "write": return WRITE_BASE_LATENCY;
            case "atomic": return ATOMIC_BASE_LATENCY;
            default: return WRITE_BASE_LATENCY;
        }
    }
    
    private double getComplexityMultiplier(String complexity) {
        switch (complexity.toLowerCase()) {
            case "low": return LOW_COMPLEXITY;
            case "medium": return MEDIUM_COMPLEXITY;
            case "high": return HIGH_COMPLEXITY;
            default: return MEDIUM_COMPLEXITY;
        }
    }
    
    private double getPercentile(List<Double> sortedSamples, int percentile) {
        int index = (int) Math.ceil(sortedSamples.size() * percentile / 100.0) - 1;
        index = Math.max(0, Math.min(index, sortedSamples.size() - 1));
        return sortedSamples.get(index);
    }
    
    private void generateDashboard(List<Map<String, Object>> programBaselines) throws IOException {
        StringBuilder html = new StringBuilder();
        html.append("<!DOCTYPE html>\n");
        html.append("<html lang=\"en\">\n");
        html.append("<head>\n");
        html.append("    <meta charset=\"UTF-8\">\n");
        html.append("    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n");
        html.append("    <title>COBOL Migration Performance Baselines</title>\n");
        html.append("    <style>\n");
        html.append("        body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }\n");
        html.append("        h1 { color: #333; }\n");
        html.append("        .summary { background-color: white; padding: 20px; margin-bottom: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }\n");
        html.append("        table { width: 100%; border-collapse: collapse; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }\n");
        html.append("        th { background-color: #4CAF50; color: white; padding: 12px; text-align: left; }\n");
        html.append("        td { padding: 12px; border-bottom: 1px solid #ddd; }\n");
        html.append("        tr:hover { background-color: #f5f5f5; }\n");
        html.append("        .low { color: green; }\n");
        html.append("        .medium { color: orange; }\n");
        html.append("        .high { color: red; }\n");
        html.append("        .chart-container { background-color: white; padding: 20px; margin: 20px 0; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }\n");
        html.append("    </style>\n");
        html.append("    <script src=\"https://cdn.jsdelivr.net/npm/chart.js\"></script>\n");
        html.append("</head>\n");
        html.append("<body>\n");
        html.append("    <h1>COBOL Migration Performance Baselines</h1>\n");
        html.append("    <div class=\"summary\">\n");
        html.append("        <h2>Summary</h2>\n");
        html.append("        <p><strong>Generated:</strong> ").append(LocalDateTime.now().format(DateTimeFormatter.ISO_DATE_TIME)).append("</p>\n");
        html.append("        <p><strong>Total Programs:</strong> ").append(programBaselines.size()).append("</p>\n");
        html.append("        <p><strong>Samples per Program:</strong> ").append(SAMPLES_PER_PROGRAM).append("</p>\n");
        html.append("        <p><strong>Purpose:</strong> Synthetic baseline targets for Spring Boot migration validation</p>\n");
        html.append("    </div>\n");
        
        html.append("    <table>\n");
        html.append("        <tr>\n");
        html.append("            <th>Program</th>\n");
        html.append("            <th>Description</th>\n");
        html.append("            <th>Type</th>\n");
        html.append("            <th>Complexity</th>\n");
        html.append("            <th>LOC</th>\n");
        html.append("            <th>P50 (ms)</th>\n");
        html.append("            <th>P95 (ms)</th>\n");
        html.append("            <th>P99 (ms)</th>\n");
        html.append("        </tr>\n");
        
        for (Map<String, Object> program : programBaselines) {
            String complexity = (String) program.get("complexity");
            html.append("        <tr>\n");
            html.append("            <td><strong>").append(program.get("program_name")).append("</strong></td>\n");
            html.append("            <td>").append(program.get("description")).append("</td>\n");
            html.append("            <td>").append(program.get("operation_type")).append("</td>\n");
            html.append("            <td class=\"").append(complexity).append("\">").append(complexity).append("</td>\n");
            html.append("            <td>").append(program.get("cobol_lines")).append("</td>\n");
            html.append("            <td>").append(program.get("p50_ms")).append("</td>\n");
            html.append("            <td>").append(program.get("p95_ms")).append("</td>\n");
            html.append("            <td>").append(program.get("p99_ms")).append("</td>\n");
            html.append("        </tr>\n");
        }
        
        html.append("    </table>\n");
        
        html.append("    <div class=\"chart-container\">\n");
        html.append("        <h2>P95 Latency Comparison</h2>\n");
        html.append("        <canvas id=\"latencyChart\"></canvas>\n");
        html.append("    </div>\n");
        
        html.append("    <script>\n");
        html.append("        const ctx = document.getElementById('latencyChart').getContext('2d');\n");
        html.append("        new Chart(ctx, {\n");
        html.append("            type: 'bar',\n");
        html.append("            data: {\n");
        html.append("                labels: [");
        
        for (int i = 0; i < programBaselines.size(); i++) {
            if (i > 0) html.append(", ");
            html.append("'").append(programBaselines.get(i).get("program_name")).append("'");
        }
        
        html.append("],\n");
        html.append("                datasets: [{\n");
        html.append("                    label: 'P95 Latency (ms)',\n");
        html.append("                    data: [");
        
        for (int i = 0; i < programBaselines.size(); i++) {
            if (i > 0) html.append(", ");
            html.append(programBaselines.get(i).get("p95_ms"));
        }
        
        html.append("],\n");
        html.append("                    backgroundColor: 'rgba(76, 175, 80, 0.6)',\n");
        html.append("                    borderColor: 'rgba(76, 175, 80, 1)',\n");
        html.append("                    borderWidth: 1\n");
        html.append("                }]\n");
        html.append("            },\n");
        html.append("            options: {\n");
        html.append("                responsive: true,\n");
        html.append("                scales: {\n");
        html.append("                    y: {\n");
        html.append("                        beginAtZero: true,\n");
        html.append("                        title: { display: true, text: 'Latency (ms)' }\n");
        html.append("                    },\n");
        html.append("                    x: {\n");
        html.append("                        title: { display: true, text: 'Program' }\n");
        html.append("                    }\n");
        html.append("                }\n");
        html.append("            }\n");
        html.append("        });\n");
        html.append("    </script>\n");
        html.append("</body>\n");
        html.append("</html>\n");
        
        try (FileWriter writer = new FileWriter("performance_dashboard.html")) {
            writer.write(html.toString());
        }
    }
    
    private static class ProgramDefinition {
        final String name;
        final String description;
        final String operationType;
        final String complexity;
        final int lines;
        
        ProgramDefinition(String name, String description, String operationType, String complexity, int lines) {
            this.name = name;
            this.description = description;
            this.operationType = operationType;
            this.complexity = complexity;
            this.lines = lines;
        }
    }
}
