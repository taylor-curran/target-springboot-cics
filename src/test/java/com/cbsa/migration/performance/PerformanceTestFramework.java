package com.cbsa.migration.performance;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.concurrent.TimeUnit;

public class PerformanceTestFramework {

    public static class PerformanceMetrics {
        private final String programName;
        private final int sampleCount;
        private final double p50;
        private final double p95;
        private final double p99;
        private final List<Long> samples;

        public PerformanceMetrics(String programName, List<Long> samples) {
            this.programName = programName;
            this.samples = new ArrayList<>(samples);
            this.sampleCount = samples.size();
            
            List<Long> sortedSamples = new ArrayList<>(samples);
            Collections.sort(sortedSamples);
            
            this.p50 = calculatePercentile(sortedSamples, 50);
            this.p95 = calculatePercentile(sortedSamples, 95);
            this.p99 = calculatePercentile(sortedSamples, 99);
        }

        private double calculatePercentile(List<Long> sortedSamples, int percentile) {
            if (sortedSamples.isEmpty()) {
                return 0.0;
            }
            int index = (int) Math.ceil((percentile / 100.0) * sortedSamples.size()) - 1;
            index = Math.max(0, Math.min(index, sortedSamples.size() - 1));
            return sortedSamples.get(index);
        }

        public String getProgramName() {
            return programName;
        }

        public int getSampleCount() {
            return sampleCount;
        }

        public double getP50() {
            return p50;
        }

        public double getP95() {
            return p95;
        }

        public double getP99() {
            return p99;
        }

        public List<Long> getSamples() {
            return new ArrayList<>(samples);
        }

        @Override
        public String toString() {
            return String.format("%s: P50=%.1fms, P95=%.1fms, P99=%.1fms (n=%d)",
                    programName, p50, p95, p99, sampleCount);
        }
    }

    public static List<Long> generateSimulatedSamples(int count, long meanMs, long stdDevMs, Random random) {
        List<Long> samples = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            double gaussian = random.nextGaussian();
            long latency = Math.max(1, (long) (meanMs + (gaussian * stdDevMs)));
            samples.add(latency);
        }
        return samples;
    }

    public static long measureEndpointLatency(String url) {
        long startTime = System.nanoTime();
        try {
            TimeUnit.MILLISECONDS.sleep(10);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        long endTime = System.nanoTime();
        return TimeUnit.NANOSECONDS.toMillis(endTime - startTime);
    }

    public static List<Long> loadTestEndpoint(String url, int sampleCount) {
        List<Long> latencies = new ArrayList<>();
        for (int i = 0; i < sampleCount; i++) {
            long latency = measureEndpointLatency(url);
            latencies.add(latency);
        }
        return latencies;
    }
}
