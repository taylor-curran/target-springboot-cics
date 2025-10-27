#!/usr/bin/env python3
"""
Performance baseline measurement script for CICS Banking Sample Application migration.
Measures P50/P95/P99 latencies for existing endpoints and creates synthetic baselines
for not-yet-migrated COBOL programs.
"""

import requests
import time
import json
import statistics
from datetime import datetime
from typing import List, Dict, Tuple

BASE_URL = "http://localhost:8085"
NUM_SAMPLES = 100
WARMUP_REQUESTS = 10

def measure_endpoint_latency(url: str, num_samples: int = NUM_SAMPLES) -> Dict:
    """Measure latency percentiles for a given endpoint."""
    latencies = []
    
    for _ in range(WARMUP_REQUESTS):
        try:
            requests.get(url, timeout=5)
        except:
            pass
    
    successful_requests = 0
    for i in range(num_samples):
        try:
            start_time = time.time()
            response = requests.get(url, timeout=5)
            end_time = time.time()
            
            if response.status_code == 200:
                latency_ms = (end_time - start_time) * 1000
                latencies.append(latency_ms)
                successful_requests += 1
        except Exception as e:
            print(f"  Request {i+1} failed: {e}")
    
    if not latencies:
        return None
    
    latencies.sort()
    
    return {
        "samples": successful_requests,
        "p50_ms": statistics.median(latencies),
        "p95_ms": latencies[int(len(latencies) * 0.95)] if len(latencies) > 1 else latencies[0],
        "p99_ms": latencies[int(len(latencies) * 0.99)] if len(latencies) > 1 else latencies[0],
        "min_ms": min(latencies),
        "max_ms": max(latencies),
        "mean_ms": statistics.mean(latencies)
    }

def create_synthetic_baseline(program_name: str, complexity: str, operation_type: str) -> Dict:
    """Create synthetic baseline estimates based on program complexity and operation type."""
    
    base_latencies = {
        "read": {"p50": 15, "p95": 35, "p99": 50},
        "create": {"p50": 45, "p95": 95, "p99": 140},
        "update": {"p50": 35, "p95": 75, "p99": 110},
        "delete": {"p50": 40, "p95": 85, "p99": 125},
        "transfer": {"p50": 85, "p95": 175, "p99": 250},
    }
    
    complexity_multipliers = {
        "low": 0.8,
        "medium": 1.0,
        "high": 1.4
    }
    
    base = base_latencies.get(operation_type, base_latencies["read"])
    multiplier = complexity_multipliers.get(complexity, 1.0)
    
    return {
        "samples": 0,
        "p50_ms": round(base["p50"] * multiplier, 2),
        "p95_ms": round(base["p95"] * multiplier, 2),
        "p99_ms": round(base["p99"] * multiplier, 2),
        "min_ms": round(base["p50"] * multiplier * 0.6, 2),
        "max_ms": round(base["p99"] * multiplier * 1.2, 2),
        "mean_ms": round(base["p50"] * multiplier * 1.1, 2),
        "synthetic": True,
        "basis": f"Estimated based on {complexity} complexity {operation_type} operation"
    }

def main():
    print("=" * 80)
    print("CICS Banking Application - Performance Baseline Measurement")
    print("=" * 80)
    print()
    
    baseline_data = {
        "measurement_timestamp": datetime.now().isoformat(),
        "baseline_version": "1.0",
        "description": "Performance baselines for COBOL business logic programs. Includes measured data for migrated programs and synthetic estimates for not-yet-migrated programs.",
        "programs": {}
    }
    
    print("Measuring existing endpoints...")
    print()
    
    existing_programs = [
        {
            "name": "GETCOMPY",
            "endpoint": f"{BASE_URL}/api/utility/company-name",
            "operation": "read",
            "description": "Get company information"
        },
        {
            "name": "GETSCODE",
            "endpoint": f"{BASE_URL}/api/utility/sortcode",
            "operation": "read",
            "description": "Get bank sort code"
        }
    ]
    
    for prog in existing_programs:
        print(f"Testing {prog['name']} ({prog['description']})...")
        print(f"  Endpoint: {prog['endpoint']}")
        print(f"  Collecting {NUM_SAMPLES} samples...")
        
        metrics = measure_endpoint_latency(prog['endpoint'])
        
        if metrics:
            print(f"  ✓ P50: {metrics['p50_ms']:.2f}ms, P95: {metrics['p95_ms']:.2f}ms, P99: {metrics['p99_ms']:.2f}ms")
            baseline_data["programs"][prog['name']] = {
                "description": prog['description'],
                "operation_type": prog['operation'],
                "complexity": "low",
                "measured": True,
                "metrics": metrics
            }
        else:
            print(f"  ✗ Failed to measure")
        print()
    
    print("Creating synthetic baselines for unmigrated programs...")
    print()
    
    unmigrated_programs = [
        {"name": "INQCUST", "operation": "read", "complexity": "medium", "desc": "Inquiry customer"},
        {"name": "CRECUST", "operation": "create", "complexity": "high", "desc": "Create customer"},
        {"name": "UPDCUST", "operation": "update", "complexity": "low", "desc": "Update customer"},
        {"name": "DELCUS", "operation": "delete", "complexity": "medium", "desc": "Delete customer"},
        
        {"name": "INQACC", "operation": "read", "complexity": "medium", "desc": "Inquiry account"},
        {"name": "INQACCCU", "operation": "read", "complexity": "medium", "desc": "Inquiry accounts by customer"},
        {"name": "CREACC", "operation": "create", "complexity": "high", "desc": "Create account"},
        {"name": "UPDACC", "operation": "update", "complexity": "low", "desc": "Update account"},
        {"name": "DELACC", "operation": "delete", "complexity": "medium", "desc": "Delete account"},
        
        {"name": "XFRFUN", "operation": "transfer", "complexity": "high", "desc": "Transfer funds"},
        {"name": "DBCRFUN", "operation": "update", "complexity": "medium", "desc": "Debit/credit funds"},
    ]
    
    for prog in unmigrated_programs:
        metrics = create_synthetic_baseline(prog['name'], prog['complexity'], prog['operation'])
        print(f"{prog['name']}: P50={metrics['p50_ms']}ms, P95={metrics['p95_ms']}ms, P99={metrics['p99_ms']}ms (synthetic)")
        
        baseline_data["programs"][prog['name']] = {
            "description": prog['desc'],
            "operation_type": prog['operation'],
            "complexity": prog['complexity'],
            "measured": False,
            "metrics": metrics
        }
    
    print()
    
    output_file = "performance_baseline.json"
    with open(output_file, 'w') as f:
        json.dump(baseline_data, f, indent=2)
    
    print(f"✓ Performance baseline saved to {output_file}")
    print()
    print("Summary:")
    print(f"  Total programs: {len(baseline_data['programs'])}")
    measured = sum(1 for p in baseline_data['programs'].values() if p['measured'])
    print(f"  Measured: {measured}")
    print(f"  Synthetic: {len(baseline_data['programs']) - measured}")
    print()

if __name__ == "__main__":
    main()
