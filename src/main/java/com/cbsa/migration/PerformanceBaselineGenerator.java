package com.cbsa.migration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

import java.io.File;
import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PerformanceBaselineGenerator {

    static class ProgramBaseline {
        public String name;
        public String description;
        public String status;
        public String complexity;
        public int linesOfCode;
        public int sampleCount;
        public Map<String, Integer> latencies;

        public ProgramBaseline(String name, String description, String complexity, int linesOfCode) {
            this.name = name;
            this.description = description;
            this.status = "not-migrated";
            this.complexity = complexity;
            this.linesOfCode = linesOfCode;
            this.sampleCount = 100;
            this.latencies = new HashMap<>();
        }

        public void setLatencies(int p50, int p95, int p99) {
            this.latencies.put("p50_ms", p50);
            this.latencies.put("p95_ms", p95);
            this.latencies.put("p99_ms", p99);
        }
    }

    public static void main(String[] args) throws IOException {
        List<ProgramBaseline> programs = new ArrayList<>();

        ProgramBaseline inqcust = new ProgramBaseline("INQCUST", 
            "Customer Read - Query customer by sort code and number", 
            "medium", 712);
        inqcust.setLatencies(50, 125, 250);
        programs.add(inqcust);

        ProgramBaseline crecust = new ProgramBaseline("CRECUST", 
            "Customer Create - Create customer with credit agency integration and Named Counter", 
            "high", 1440);
        crecust.setLatencies(150, 400, 800);
        programs.add(crecust);

        ProgramBaseline updcust = new ProgramBaseline("UPDCUST", 
            "Customer Update - Modify customer fields without PROCTRAN audit", 
            "low", 365);
        updcust.setLatencies(25, 75, 150);
        programs.add(updcust);

        ProgramBaseline delcus = new ProgramBaseline("DELCUS", 
            "Customer Delete - Cascade delete customer and all accounts with PROCTRAN", 
            "medium", 762);
        delcus.setLatencies(50, 125, 250);
        programs.add(delcus);

        ProgramBaseline inqacc = new ProgramBaseline("INQACC", 
            "Account Read - Query account by account number", 
            "medium", 1003);
        inqacc.setLatencies(50, 125, 250);
        programs.add(inqacc);

        ProgramBaseline inqacccu = new ProgramBaseline("INQACCCU", 
            "Account Read for Customer - Query all accounts for customer with cursor pagination", 
            "medium", 883);
        inqacccu.setLatencies(50, 125, 250);
        programs.add(inqacccu);

        ProgramBaseline creacc = new ProgramBaseline("CREACC", 
            "Account Create - Create account with Named Counter and PROCTRAN", 
            "high", 1248);
        creacc.setLatencies(150, 400, 800);
        programs.add(creacc);

        ProgramBaseline updacc = new ProgramBaseline("UPDACC", 
            "Account Update - Modify account fields with business rule validation", 
            "low", 407);
        updacc.setLatencies(25, 75, 150);
        programs.add(updacc);

        ProgramBaseline delacc = new ProgramBaseline("DELACC", 
            "Account Delete - Delete account with PROCTRAN audit", 
            "medium", 650);
        delacc.setLatencies(50, 125, 250);
        programs.add(delacc);

        ProgramBaseline xfrfun = new ProgramBaseline("XFRFUN", 
            "Transfer Funds - Atomic debit/credit operations across two accounts with rollback", 
            "high", 1925);
        xfrfun.setLatencies(200, 500, 1000);
        programs.add(xfrfun);

        ProgramBaseline dbcrfun = new ProgramBaseline("DBCRFUN", 
            "Debit/Credit - Cash deposits and withdrawals with balance updates and PROCTRAN", 
            "medium", 862);
        dbcrfun.setLatencies(50, 125, 250);
        programs.add(dbcrfun);

        Map<String, Object> baseline = new HashMap<>();
        baseline.put("generated_at", Instant.now().toString());
        baseline.put("baseline_type", "synthetic");
        baseline.put("description", "Synthetic performance baselines for COBOL business logic programs");
        baseline.put("environment", "COBOL/CICS Mainframe (estimated)");
        baseline.put("programs", programs);

        ObjectMapper mapper = new ObjectMapper();
        mapper.enable(SerializationFeature.INDENT_OUTPUT);
        mapper.writeValue(new File("performance_baseline.json"), baseline);

        System.out.println("Performance baseline JSON generated successfully!");
        System.out.println("Generated baselines for " + programs.size() + " programs");
    }
}
