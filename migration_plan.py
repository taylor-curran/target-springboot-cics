"""
Migration Plan Tracker for COBOL to Java Migration
Tracks status of all migration tasks and setup activities.
"""

from typing import Dict, List
from datetime import datetime

MIGRATION_TASKS = {
    "setup_001": {
        "name": "Establish Performance Baseline for Legacy COBOL Programs",
        "description": "Run performance tests against all COBOL programs. Document P50, P95, and P99 latencies.",
        "status": "completed",
        "completed_date": "2025-10-26T00:52:00Z",
        "artifacts": [
            "docs/performance/baseline_metrics.md",
            "docs/performance/baseline_metrics.json"
        ]
    },
    "setup_002": {
        "name": "Setup Continuous Monitoring and Observability",
        "description": "Implement monitoring and observability infrastructure.",
        "status": "pending",
        "dependencies": ["setup_001"]
    },
    "setup_004": {
        "name": "Extract and Document COBOL Business Rules",
        "description": "Document business rules from COBOL programs.",
        "status": "pending",
        "dependencies": []
    }
}

COBOL_PROGRAMS = {
    "migrated": [
        {"name": "GETCOMPY", "service": "CompanyInfoService", "status": "completed"},
        {"name": "GETSCODE", "service": "SortCodeService", "status": "completed"},
        {"name": "CRDTAGY1", "service": "CreditAgencyService", "status": "completed"},
        {"name": "ABNDPROC", "service": "ErrorLoggingService", "status": "completed"},
        {"name": "BANKDATA", "service": "BankDataGenerator", "status": "completed"}
    ],
    "remaining": [
        "BNK1CAC", "BNK1CCA", "BNK1CCS", "BNK1CRA", "BNK1DAC", "BNK1DCS",
        "BNK1TFN", "BNK1UAC", "BNKMENU", "CRDTAGY2", "CRDTAGY3", "CRDTAGY4",
        "CRDTAGY5", "CREACC", "CRECUST", "DBCRFUN", "DELACC", "DELCUS",
        "INQACC", "INQACCCU", "INQCUST", "UPDACC", "UPDCUST", "XFRFUN"
    ]
}

def get_task_status(task_id: str) -> str:
    """Get the status of a specific task."""
    return MIGRATION_TASKS.get(task_id, {}).get("status", "unknown")

def get_migration_progress() -> Dict[str, int]:
    """Get overall migration progress statistics."""
    total = len(COBOL_PROGRAMS["migrated"]) + len(COBOL_PROGRAMS["remaining"])
    migrated = len(COBOL_PROGRAMS["migrated"])
    return {
        "total_programs": total,
        "migrated": migrated,
        "remaining": len(COBOL_PROGRAMS["remaining"]),
        "percentage": round((migrated / total) * 100, 2)
    }

if __name__ == "__main__":
    print("=== COBOL to Java Migration Progress ===")
    progress = get_migration_progress()
    print(f"Total Programs: {progress['total_programs']}")
    print(f"Migrated: {progress['migrated']}")
    print(f"Remaining: {progress['remaining']}")
    print(f"Progress: {progress['percentage']}%")
    print("\n=== Setup Tasks ===")
    for task_id, task in MIGRATION_TASKS.items():
        print(f"{task_id}: {task['name']} - Status: {task['status']}")
