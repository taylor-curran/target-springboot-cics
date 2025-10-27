#!/usr/bin/env python3
"""
Generate synthetic performance baselines for COBOL programs.

This script creates realistic P50/P95/P99 latency metrics for 11 unmigrated
COBOL business logic programs based on their complexity characteristics.
"""

import json
import random
import math
import statistics
from datetime import datetime, timezone

PROGRAMS = [
    {"program": "CRECUST", "lines": 1440, "complexity": "high", "operation": "create"},
    {"program": "INQCUST", "lines": 712, "complexity": "medium", "operation": "inquiry"},
    {"program": "UPDCUST", "lines": 365, "complexity": "low", "operation": "update"},
    {"program": "DELCUS", "lines": 762, "complexity": "medium", "operation": "delete"},
    {"program": "CREACC", "lines": 1248, "complexity": "high", "operation": "create"},
    {"program": "INQACC", "lines": 1003, "complexity": "medium", "operation": "inquiry"},
    {"program": "INQACCCU", "lines": 883, "complexity": "medium", "operation": "inquiry"},
    {"program": "UPDACC", "lines": 407, "complexity": "low", "operation": "update"},
    {"program": "DELACC", "lines": 650, "complexity": "medium", "operation": "delete"},
    {"program": "XFRFUN", "lines": 1925, "complexity": "high", "operation": "transaction"},
    {"program": "DBCRFUN", "lines": 862, "complexity": "medium", "operation": "transaction"}
]

OPERATION_MULTIPLIERS = {
    "inquiry": 0.7,
    "update": 1.0,
    "delete": 1.0,
    "create": 1.3,
    "transaction": 1.5
}

COMPLEXITY_MULTIPLIERS = {
    "low": 1.0,
    "medium": 1.3,
    "high": 1.7
}

def generate_latency_samples(program_data, sample_size=150):
    """Generate realistic latency samples using log-normal distribution."""
    base_latency = 150 + (program_data["lines"] / 10)
    
    operation_mult = OPERATION_MULTIPLIERS[program_data["operation"]]
    complexity_mult = COMPLEXITY_MULTIPLIERS[program_data["complexity"]]
    
    mean_latency = base_latency * operation_mult * complexity_mult
    
    random.seed(hash(program_data["program"]) % (2**32))
    
    shape = 0.3
    scale = mean_latency / math.exp(shape**2 / 2)
    
    samples = []
    for _ in range(sample_size):
        z = random.gauss(0, 1)
        sample = scale * math.exp(shape * z)
        samples.append(sample)
    
    return samples

def calculate_percentiles(samples):
    """Calculate P50, P95, P99 percentiles."""
    sorted_samples = sorted(samples)
    n = len(sorted_samples)
    
    def percentile(p):
        k = (n - 1) * p / 100
        f = math.floor(k)
        c = math.ceil(k)
        if f == c:
            return sorted_samples[int(k)]
        d0 = sorted_samples[int(f)] * (c - k)
        d1 = sorted_samples[int(c)] * (k - f)
        return d0 + d1
    
    return {
        "p50": round(percentile(50), 2),
        "p95": round(percentile(95), 2),
        "p99": round(percentile(99), 2)
    }

def generate_baseline():
    """Generate complete baseline data for all programs."""
    baseline = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "methodology": "Synthetic baselines based on COBOL program complexity analysis",
        "description": "Performance baselines for 11 business logic programs to be migrated from COBOL/CICS to Spring Boot",
        "sample_size_per_program": 150,
        "latency_calculation": {
            "base_formula": "150 + (lines_of_code / 10)",
            "operation_multipliers": OPERATION_MULTIPLIERS,
            "complexity_multipliers": COMPLEXITY_MULTIPLIERS,
            "distribution": "log-normal with shape=0.3 for realistic variance"
        },
        "programs": []
    }
    
    for program_data in PROGRAMS:
        samples = generate_latency_samples(program_data)
        percentiles = calculate_percentiles(samples)
        
        program_baseline = {
            "name": program_data["program"],
            "operation_type": program_data["operation"],
            "lines_of_code": program_data["lines"],
            "complexity": program_data["complexity"],
            "latency_ms": percentiles,
            "sample_count": len(samples),
            "samples": [round(s, 2) for s in samples[:10]]
        }
        
        baseline["programs"].append(program_baseline)
    
    return baseline

def main():
    """Generate and save performance baseline."""
    print("Generating performance baselines for 11 COBOL programs...")
    
    baseline = generate_baseline()
    
    output_file = "performance_baseline.json"
    with open(output_file, 'w') as f:
        json.dump(baseline, f, indent=2)
    
    print(f"✓ Generated {output_file}")
    print(f"  Total programs: {len(baseline['programs'])}")
    print(f"  Sample size per program: {baseline['sample_size_per_program']}")
    
    print("\nLatency Summary (P50/P95/P99 in ms):")
    for program in baseline["programs"]:
        lat = program["latency_ms"]
        print(f"  {program['name']:10} {lat['p50']:6.1f} / {lat['p95']:6.1f} / {lat['p99']:6.1f}")
    
    print("\n✓ Baseline generation complete!")

if __name__ == "__main__":
    main()
