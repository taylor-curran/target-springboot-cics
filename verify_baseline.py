#!/usr/bin/env python3
import json
import sys

try:
    with open('performance_baseline.json', 'r') as f:
        data = json.load(f)
    
    programs = data['programs']
    print(f"✓ Valid JSON with {len(programs)} programs")
    
    assert len(programs) == 11, f"Expected 11 programs, found {len(programs)}"
    print("✓ All 11 programs present")
    
    for program in programs:
        assert program['metrics']['sample_count'] >= 100, f"{program['name']} has only {program['metrics']['sample_count']} samples"
    print("✓ All programs have 100+ samples")
    
    for program in programs:
        p50 = program['metrics']['p50_ms']
        p95 = program['metrics']['p95_ms']
        p99 = program['metrics']['p99_ms']
        assert p50 < p95 < p99, f"{program['name']}: percentiles not ordered correctly: P50={p50}, P95={p95}, P99={p99}"
    print("✓ All percentiles properly ordered (P50 < P95 < P99)")
    
    expected_programs = {'INQCUST', 'CRECUST', 'UPDCUST', 'DELCUS', 'INQACC', 'INQACCCU', 'CREACC', 'UPDACC', 'DELACC', 'XFRFUN', 'DBCRFUN'}
    actual_programs = {p['name'] for p in programs}
    assert expected_programs == actual_programs, f"Missing programs: {expected_programs - actual_programs}"
    print("✓ All expected programs present")
    
    print("\nValidation PASSED ✓")
    sys.exit(0)
    
except Exception as e:
    print(f"✗ Validation FAILED: {e}")
    sys.exit(1)
