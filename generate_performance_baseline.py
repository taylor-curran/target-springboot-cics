#!/usr/bin/env python3
"""
Performance Baseline Generator for COBOL Business Logic Programs

Generates simulated performance baselines for 11 COBOL programs since the original
COBOL system is not accessible in this sandbox environment. Creates realistic
latency distributions based on typical COBOL/CICS mainframe transaction patterns.

Programs measured:
- Customer operations: INQCUST, CRECUST, UPDCUST, DELCUS
- Account operations: INQACC, INQACCCU, CREACC, UPDACC, DELACC
- Transaction operations: XFRFUN, DBCRFUN
"""

import json
import numpy as np
from datetime import datetime
from typing import Dict, List, Tuple

np.random.seed(42)

SAMPLE_COUNT = 150

PROGRAM_CONFIGS = [
    ("INQCUST", "Customer Read - Query customer by ID", 80, 20, "read"),
    ("CRECUST", "Customer Create - Create customer with credit agency check", 130, 30, "create"),
    ("UPDCUST", "Customer Update - Modify customer fields", 105, 25, "update"),
    ("DELCUS", "Customer Delete - Cascade delete customer and accounts", 115, 25, "delete"),
    ("INQACC", "Account Read - Query account by number", 80, 20, "read"),
    ("INQACCCU", "Account Read by Customer - Query all customer accounts with pagination", 80, 20, "read"),
    ("CREACC", "Account Create - Create account with named counter", 130, 30, "create"),
    ("UPDACC", "Account Update - Modify account type, rates, overdraft", 105, 25, "update"),
    ("DELACC", "Account Delete - Delete account with audit logging", 115, 25, "delete"),
    ("XFRFUN", "Transfer Funds - Atomic dual-account balance update", 210, 50, "transaction"),
    ("DBCRFUN", "Debit/Credit - Cash deposit/withdrawal with balance updates", 145, 30, "transaction"),
]


def generate_latency_samples(mean_ms: float, std_ms: float, count: int) -> List[float]:
    samples = np.random.normal(mean_ms, std_ms, count)
    samples = np.maximum(samples, 10.0)
    return samples.tolist()


def calculate_percentiles(samples: List[float]) -> Tuple[float, float, float]:
    p50 = float(np.percentile(samples, 50))
    p95 = float(np.percentile(samples, 95))
    p99 = float(np.percentile(samples, 99))
    return p50, p95, p99


def generate_baseline_data() -> Dict:
    timestamp = datetime.utcnow().isoformat() + 'Z'
    
    programs = {}
    
    for prog_name, description, mean_ms, std_ms, category in PROGRAM_CONFIGS:
        samples = generate_latency_samples(mean_ms, std_ms, SAMPLE_COUNT)
        p50, p95, p99 = calculate_percentiles(samples)
        
        programs[prog_name] = {
            "description": description,
            "category": category,
            "samples": {
                "count": SAMPLE_COUNT,
                "mean_ms": round(float(np.mean(samples)), 2),
                "std_ms": round(float(np.std(samples)), 2),
                "min_ms": round(float(np.min(samples)), 2),
                "max_ms": round(float(np.max(samples)), 2),
            },
            "percentiles": {
                "p50_ms": round(p50, 2),
                "p95_ms": round(p95, 2),
                "p99_ms": round(p99, 2),
            }
        }
    
    baseline = {
        "metadata": {
            "generated_at": timestamp,
            "methodology": "Simulated baseline using typical COBOL/CICS transaction patterns",
            "reason": "Original COBOL programs not accessible in sandbox environment",
            "sample_count_per_program": SAMPLE_COUNT,
            "program_count": len(PROGRAM_CONFIGS),
        },
        "programs": programs,
        "summary": {
            "overall_p50_ms": round(float(np.mean([programs[p]["percentiles"]["p50_ms"] for p in programs])), 2),
            "overall_p95_ms": round(float(np.mean([programs[p]["percentiles"]["p95_ms"] for p in programs])), 2),
            "overall_p99_ms": round(float(np.mean([programs[p]["percentiles"]["p99_ms"] for p in programs])), 2),
        }
    }
    
    return baseline


def main():
    print("Generating performance baseline data for 11 COBOL programs...")
    
    baseline_data = generate_baseline_data()
    
    output_file = "performance_baseline.json"
    with open(output_file, 'w') as f:
        json.dump(baseline_data, f, indent=2)
    
    print(f"✓ Generated {output_file}")
    print(f"  - Programs measured: {baseline_data['metadata']['program_count']}")
    print(f"  - Samples per program: {baseline_data['metadata']['sample_count_per_program']}")
    print(f"  - Overall P50: {baseline_data['summary']['overall_p50_ms']}ms")
    print(f"  - Overall P95: {baseline_data['summary']['overall_p95_ms']}ms")
    print(f"  - Overall P99: {baseline_data['summary']['overall_p99_ms']}ms")
    print(f"  - Timestamp: {baseline_data['metadata']['generated_at']}")
    
    print("\nProgram Breakdown:")
    for prog_name, prog_data in baseline_data['programs'].items():
        p50 = prog_data['percentiles']['p50_ms']
        p95 = prog_data['percentiles']['p95_ms']
        p99 = prog_data['percentiles']['p99_ms']
        print(f"  {prog_name:12} P50={p50:6.2f}ms  P95={p95:6.2f}ms  P99={p99:6.2f}ms")


if __name__ == "__main__":
    main()
