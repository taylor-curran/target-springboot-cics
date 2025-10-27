#!/usr/bin/env python3
import json
import random
import math
from datetime import datetime
from pathlib import Path

def load_migration_plan():
    with open('migration_plan.py', 'r') as f:
        content = f.read()
    
    exec_globals = {}
    exec(content, exec_globals)
    return exec_globals['migration_plan']

def generate_latency_samples(complexity, lines_of_code, num_samples=150):
    if complexity == "high":
        base_mean = 200.0
        base_stddev = 80.0
    elif complexity == "medium":
        base_mean = 100.0
        base_stddev = 40.0
    else:
        base_mean = 50.0
        base_stddev = 20.0
    
    loc_factor = 1.0 + (lines_of_code - 500) / 2000.0
    mean = base_mean * loc_factor
    stddev = base_stddev * loc_factor
    
    samples = []
    for _ in range(num_samples):
        sample = random.gauss(mean, stddev)
        sample = max(sample, mean * 0.3)
        sample = min(sample, mean * 3.0)
        samples.append(sample)
    
    return samples

def calculate_percentiles(samples):
    sorted_samples = sorted(samples)
    n = len(sorted_samples)
    
    mean_val = sum(samples) / len(samples)
    variance = sum((x - mean_val) ** 2 for x in samples) / len(samples)
    stddev = math.sqrt(variance)
    
    return {
        "p50": round(sorted_samples[int(n * 0.50)], 2),
        "p95": round(sorted_samples[int(n * 0.95)], 2),
        "p99": round(sorted_samples[int(n * 0.99)], 2),
        "min": round(min(samples), 2),
        "max": round(max(samples), 2),
        "mean": round(mean_val, 2),
        "stddev": round(stddev, 2)
    }

def get_program_description(program_name):
    descriptions = {
        "CRECUST": "Customer creation with credit agency integration",
        "INQCUST": "Customer inquiry operation",
        "UPDCUST": "Customer update operation",
        "DELCUS": "Customer deletion with cascade to accounts",
        "CREACC": "Account creation with Named Counter",
        "INQACC": "Account inquiry operation",
        "INQACCCU": "Account inquiry by customer",
        "UPDACC": "Account update operation",
        "DELACC": "Account deletion with PROCTRAN logging",
        "XFRFUN": "Transfer funds between accounts (atomic)",
        "DBCRFUN": "Debit/credit operations (deposits/withdrawals)"
    }
    return descriptions.get(program_name, "COBOL business logic program")

def main():
    random.seed(42)
    
    print("Loading migration plan...")
    migration_plan = load_migration_plan()
    
    programs_to_migrate = migration_plan['programs_to_migrate']
    
    baseline_data = {
        "generated_at": datetime.utcnow().isoformat() + "Z",
        "description": "Synthetic performance baseline for COBOL business logic programs",
        "methodology": "Synthetic data generated based on program complexity levels (high/medium/low) and lines of code. High complexity programs (1200-1900 LOC) baseline at ~200ms mean, medium complexity (650-1000 LOC) at ~100ms mean, low complexity (350-450 LOC) at ~50ms mean. Each program has 150 samples with realistic variance based on normal distribution. These baselines will be used to validate migration performance targets.",
        "sample_count": 150,
        "programs": {}
    }
    
    print(f"\nGenerating performance baselines for {len(programs_to_migrate)} programs...")
    
    for program_info in programs_to_migrate:
        program_name = program_info['program']
        complexity = program_info['complexity']
        lines_of_code = program_info['lines']
        
        print(f"  - {program_name} ({complexity} complexity, {lines_of_code} LOC)")
        
        samples = generate_latency_samples(complexity, lines_of_code)
        latencies = calculate_percentiles(samples)
        
        baseline_data['programs'][program_name] = {
            "name": program_name,
            "description": get_program_description(program_name),
            "complexity": complexity,
            "lines_of_code": lines_of_code,
            "samples": len(samples),
            "latencies_ms": latencies,
            "raw_samples": [round(s, 2) for s in samples]
        }
    
    output_file = 'performance_baseline.json'
    with open(output_file, 'w') as f:
        json.dump(baseline_data, f, indent=2)
    
    print(f"\n✅ Performance baseline generated: {output_file}")
    print(f"   Total programs: {len(baseline_data['programs'])}")
    print(f"   Samples per program: {baseline_data['sample_count']}")
    print(f"   Generated at: {baseline_data['generated_at']}")
    
    print("\nPerformance Summary:")
    print("-" * 80)
    print(f"{'Program':<12} {'Complexity':<10} {'LOC':<6} {'P50':<8} {'P95':<8} {'P99':<8}")
    print("-" * 80)
    
    for program_name in sorted(baseline_data['programs'].keys()):
        prog = baseline_data['programs'][program_name]
        latencies = prog['latencies_ms']
        print(f"{program_name:<12} {prog['complexity']:<10} {prog['lines_of_code']:<6} "
              f"{latencies['p50']:<8.2f} {latencies['p95']:<8.2f} {latencies['p99']:<8.2f}")
    
    print("-" * 80)

if __name__ == "__main__":
    main()
