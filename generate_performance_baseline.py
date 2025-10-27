#!/usr/bin/env python3
"""
Performance Baseline Generator for COBOL Programs

Generates simulated performance metrics for 11 COBOL business logic programs.
Creates baseline data that represents typical COBOL/CICS mainframe performance
characteristics for use as migration success criteria.
"""

import json
import random
from datetime import datetime
from typing import Dict, List, Tuple
import statistics


PROGRAM_CONFIGS = {
    "CRECUST": {
        "description": "Create Customer with Credit Agency Integration",
        "complexity": "high",
        "base_latency_ms": 35.0,
        "variance": 8.0,
        "lines_of_code": 1440,
        "operations": ["credit_check", "named_counter", "vsam_write", "proctran_audit"]
    },
    "INQCUST": {
        "description": "Inquire Customer (Read)",
        "complexity": "medium",
        "base_latency_ms": 12.0,
        "variance": 3.0,
        "lines_of_code": 712,
        "operations": ["vsam_read"]
    },
    "UPDCUST": {
        "description": "Update Customer",
        "complexity": "low",
        "base_latency_ms": 15.0,
        "variance": 4.0,
        "lines_of_code": 365,
        "operations": ["vsam_update"]
    },
    "DELCUS": {
        "description": "Delete Customer with Cascade",
        "complexity": "medium",
        "base_latency_ms": 25.0,
        "variance": 6.0,
        "lines_of_code": 762,
        "operations": ["cascade_delete", "proctran_audit"]
    },
    "CREACC": {
        "description": "Create Account with Named Counter",
        "complexity": "high",
        "base_latency_ms": 40.0,
        "variance": 9.0,
        "lines_of_code": 1248,
        "operations": ["named_counter", "db2_write", "proctran_audit"]
    },
    "INQACC": {
        "description": "Inquire Account (Read)",
        "complexity": "medium",
        "base_latency_ms": 10.0,
        "variance": 2.5,
        "lines_of_code": 1003,
        "operations": ["db2_read"]
    },
    "INQACCCU": {
        "description": "Inquire Accounts for Customer (Cursor)",
        "complexity": "medium",
        "base_latency_ms": 14.0,
        "variance": 3.5,
        "lines_of_code": 883,
        "operations": ["db2_cursor", "pagination"]
    },
    "UPDACC": {
        "description": "Update Account",
        "complexity": "low",
        "base_latency_ms": 18.0,
        "variance": 4.5,
        "lines_of_code": 407,
        "operations": ["db2_update"]
    },
    "DELACC": {
        "description": "Delete Account",
        "complexity": "medium",
        "base_latency_ms": 22.0,
        "variance": 5.0,
        "lines_of_code": 650,
        "operations": ["db2_delete", "proctran_audit"]
    },
    "XFRFUN": {
        "description": "Transfer Funds (Atomic)",
        "complexity": "high",
        "base_latency_ms": 55.0,
        "variance": 12.0,
        "lines_of_code": 1925,
        "operations": ["dual_account_update", "balance_validation", "rollback_handling", "proctran_audit"]
    },
    "DBCRFUN": {
        "description": "Debit/Credit Funds",
        "complexity": "medium",
        "base_latency_ms": 30.0,
        "variance": 7.0,
        "lines_of_code": 862,
        "operations": ["balance_update", "proctran_audit"]
    }
}


def generate_latency_samples(base_ms: float, variance: float, sample_count: int = 150) -> List[float]:
    """
    Generate realistic latency samples with normal distribution and outliers.
    
    Args:
        base_ms: Base latency in milliseconds (represents P50 target)
        variance: Standard deviation for normal distribution
        sample_count: Number of samples to generate (default 150)
    
    Returns:
        List of latency samples in milliseconds
    """
    samples = []
    
    for i in range(sample_count):
        if i < int(sample_count * 0.90):
            latency = random.normalvariate(base_ms, variance)
        elif i < int(sample_count * 0.95):
            latency = random.normalvariate(base_ms * 1.8, variance * 1.5)
        else:
            latency = random.normalvariate(base_ms * 2.5, variance * 2.0)
        
        latency = max(1.0, latency)
        samples.append(round(latency, 2))
    
    random.shuffle(samples)
    return samples


def calculate_percentiles(samples: List[float]) -> Tuple[float, float, float]:
    """
    Calculate P50, P95, and P99 percentiles from samples.
    
    Args:
        samples: List of latency samples
    
    Returns:
        Tuple of (p50, p95, p99) in milliseconds
    """
    sorted_samples = sorted(samples)
    n = len(sorted_samples)
    
    p50_idx = int(n * 0.50)
    p95_idx = int(n * 0.95)
    p99_idx = int(n * 0.99)
    
    p50 = sorted_samples[p50_idx]
    p95 = sorted_samples[p95_idx]
    p99 = sorted_samples[p99_idx]
    
    return (round(p50, 2), round(p95, 2), round(p99, 2))


def generate_baseline() -> Dict:
    """
    Generate complete performance baseline for all 11 COBOL programs.
    
    Returns:
        Dictionary containing metadata and metrics for all programs
    """
    random.seed(42)
    
    baseline = {
        "metadata": {
            "generated_at": datetime.utcnow().isoformat() + "Z",
            "environment": "simulated",
            "description": "Baseline COBOL/CICS performance metrics for migration validation",
            "purpose": "Establish success criteria for Java migration (Phase 1 - Setup)",
            "total_programs": 11,
            "samples_per_program": 150,
            "note": "Simulated metrics representing typical COBOL/CICS mainframe performance"
        },
        "programs": {}
    }
    
    for program_name, config in PROGRAM_CONFIGS.items():
        samples = generate_latency_samples(
            config["base_latency_ms"],
            config["variance"],
            sample_count=150
        )
        
        p50, p95, p99 = calculate_percentiles(samples)
        
        baseline["programs"][program_name] = {
            "description": config["description"],
            "complexity": config["complexity"],
            "lines_of_code": config["lines_of_code"],
            "operations": config["operations"],
            "samples": samples,
            "sample_count": len(samples),
            "statistics": {
                "p50_ms": p50,
                "p95_ms": p95,
                "p99_ms": p99,
                "mean_ms": round(statistics.mean(samples), 2),
                "median_ms": round(statistics.median(samples), 2),
                "min_ms": round(min(samples), 2),
                "max_ms": round(max(samples), 2),
                "std_dev_ms": round(statistics.stdev(samples), 2)
            }
        }
    
    return baseline


def main():
    """Generate and save performance baseline."""
    print("Generating performance baseline for 11 COBOL programs...")
    
    baseline = generate_baseline()
    
    output_file = "performance_baseline.json"
    with open(output_file, "w") as f:
        json.dump(baseline, f, indent=2)
    
    print(f"\n✅ Performance baseline generated: {output_file}")
    print(f"   Total programs: {baseline['metadata']['total_programs']}")
    print(f"   Samples per program: {baseline['metadata']['samples_per_program']}")
    print(f"   Generated at: {baseline['metadata']['generated_at']}")
    
    print("\n📊 Summary of P50 latencies:")
    for program_name in sorted(baseline["programs"].keys()):
        program = baseline["programs"][program_name]
        p50 = program["statistics"]["p50_ms"]
        complexity = program["complexity"]
        print(f"   {program_name:10s} ({complexity:6s}): {p50:6.2f}ms")


if __name__ == "__main__":
    main()
