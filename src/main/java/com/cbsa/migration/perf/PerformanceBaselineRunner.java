package com.cbsa.migration.perf;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

import java.io.File;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;

public class PerformanceBaselineRunner {

    static class ProgramSpec {
        final String name;
        final int lines;
        final int weight;
        ProgramSpec(String name, int lines, int weight) {
            this.name = name; this.lines = lines; this.weight = weight;
        }
    }

    static final List<ProgramSpec> PROGRAMS = Arrays.asList(
        new ProgramSpec("CRECUST", 1440, 3),
        new ProgramSpec("INQCUST",  712, 2),
        new ProgramSpec("UPDCUST",  365, 1),
        new ProgramSpec("DELCUS",   762, 2),
        new ProgramSpec("CREACC",  1248, 3),
        new ProgramSpec("INQACC",  1003, 2),
        new ProgramSpec("INQACCCU", 883, 2),
        new ProgramSpec("UPDACC",   407, 1),
        new ProgramSpec("DELACC",   650, 2),
        new ProgramSpec("XFRFUN",  1925, 3),
        new ProgramSpec("DBCRFUN",  862, 2)
    );

    static class Result {
        public String program;
        public int samples;
        public double p50_ms;
        public double p95_ms;
        public double p99_ms;
    }

    static class Baseline {
        public String generatedAt;
        public String methodology = "Synthetic baseline using CPU work scaled by COBOL program lines and complexity (low=1, medium=2, high=3) under concurrent load; deterministic seed.";
        public List<Result> results = new ArrayList<>();
    }

    public static void main(String[] args) throws Exception {
        final int samplesPerProgram = Integer.getInteger("samples", 1000);
        final int concurrency = Integer.getInteger("concurrency", Runtime.getRuntime().availableProcessors());
        final int warmup = Integer.getInteger("warmup", 100);
        final long seed = Long.getLong("seed", 12345L);

        ExecutorService pool = Executors.newFixedThreadPool(concurrency);
        try {
            List<Result> results = new ArrayList<>();
            for (ProgramSpec spec : PROGRAMS) {
                for (int i = 0; i < warmup; i++) simulateWork(spec, seed + i);

                List<Future<Long>> futures = new ArrayList<>(samplesPerProgram);
                for (int i = 0; i < samplesPerProgram; i++) {
                    final long s = seed + 100000L + i;
                    futures.add(pool.submit(() -> measureOnce(spec, s)));
                }
                List<Long> nanos = new ArrayList<>(samplesPerProgram);
                for (Future<Long> f : futures) nanos.add(f.get());
                results.add(toResult(spec.name, nanos));
            }

            Baseline baseline = new Baseline();
            baseline.generatedAt = DateTimeFormatter.ISO_INSTANT.format(Instant.now());
            baseline.results = results;

            File json = new File("performance_baseline.json");
            ObjectMapper om = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT);
            om.writeValue(json, baseline);

            File html = new File("performance_dashboard.html");
            String htmlContent = renderHtml(baseline);
            java.nio.file.Files.write(html.toPath(), htmlContent.getBytes(java.nio.charset.StandardCharsets.UTF_8));

            System.out.println("Wrote " + json.getAbsolutePath());
            System.out.println("Wrote " + html.getAbsolutePath());
        } finally {
            pool.shutdownNow();
        }
    }

    private static long measureOnce(ProgramSpec spec, long seed) {
        long t0 = System.nanoTime();
        simulateWork(spec, seed);
        return System.nanoTime() - t0;
    }

    private static void simulateWork(ProgramSpec spec, long seed) {
        final int baseUnits = Math.max(1, (spec.lines / 10) * spec.weight);
        Random rnd = new Random(seed ^ (long) spec.name.hashCode());
        double jitter = Math.exp(rnd.nextGaussian() * 0.25);
        int iterations = (int) Math.max(10, baseUnits * jitter);

        double acc = 0.0;
        for (int i = 0; i < iterations; i++) {
            acc += Math.sin(i) * Math.cos(i / 3.14159) + Math.sqrt((i % 97) + 1);
            if ((i & 1023) == 0) {
                acc *= 0.999999;
            }
        }
        if (acc == 42.4242) System.out.print("");
    }

    private static Result toResult(String program, List<Long> nanos) {
        Collections.sort(nanos);
        Result r = new Result();
        r.program = program;
        r.samples = nanos.size();
        r.p50_ms = nanosToMs(percentile(nanos, 50));
        r.p95_ms = nanosToMs(percentile(nanos, 95));
        r.p99_ms = nanosToMs(percentile(nanos, 99));
        return r;
    }

    private static double nanosToMs(long n) { return n / 1_000_000.0; }

    private static long percentile(List<Long> sorted, int p) {
        if (sorted.isEmpty()) return 0L;
        double rank = (p / 100.0) * (sorted.size() - 1);
        int idx = (int) Math.floor(rank);
        int idx2 = Math.min(sorted.size() - 1, idx + 1);
        double frac = rank - idx;
        return (long) Math.round(sorted.get(idx) + frac * (sorted.get(idx2) - sorted.get(idx)));
    }

    private static String renderHtml(Baseline baseline) {
        String jsonEscaped = baseline.results.stream()
            .map(r -> String.format("{\"program\":\"%s\",\"samples\":%d,\"p50_ms\":%.3f,\"p95_ms\":%.3f,\"p99_ms\":%.3f}",
                    r.program, r.samples, r.p50_ms, r.p95_ms, r.p99_ms))
            .collect(Collectors.joining(","));
        String genAt = baseline.generatedAt;

        return "<!doctype html><html><head><meta charset='utf-8'><title>Performance Baseline</title>"
            + "<style>body{font-family:system-ui,Arial,sans-serif;padding:16px}table{border-collapse:collapse}th,td{padding:8px 12px;border:1px solid #ddd}th{background:#f5f5f5}caption{margin-bottom:8px;font-weight:600}</style>"
            + "</head><body>"
            + "<h1>COBOL Program Performance Baseline</h1>"
            + "<p>Generated at: <b>" + genAt + "</b></p>"
            + "<table><caption>P50/P95/P99 Latencies (ms)</caption><thead><tr>"
            + "<th>Program</th><th>Samples</th><th>P50 (ms)</th><th>P95 (ms)</th><th>P99 (ms)</th></tr></thead><tbody id='rows'></tbody></table>"
            + "<script>const data={" 
            + "\"generatedAt\":\"" + genAt + "\","
            + "\"results\":[" + jsonEscaped + "]};"
            + "const tbody=document.getElementById('rows');"
            + "data.results.forEach(r=>{const tr=document.createElement('tr');"
            + "[r.program,r.samples,r.p50_ms.toFixed(3),r.p95_ms.toFixed(3),r.p99_ms.toFixed(3)]"
            + ".forEach(v=>{const td=document.createElement('td');td.textContent=v;tr.appendChild(td);});"
            + "tbody.appendChild(tr);});</script>"
            + "<p><small>Methodology: Synthetic CPU workload scaled by COBOL program lines and complexity, executed under concurrent load with warmup; deterministic seed.</small></p>"
            + "</body></html>";
    }
}
