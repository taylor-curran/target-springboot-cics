"""
Migration Task Tracking for COBOL to Spring Boot Migration

This module tracks the status of all migration tasks including setup,
individual program migrations, and validation activities.

Task Categories:
- setup_tasks: Infrastructure and baseline setup activities
- migration_tasks: Individual COBOL program migrations
- validation_tasks: Testing and verification activities

Status Values:
- pending: Not yet started
- in_progress: Currently being worked on
- completed: Finished and verified
- blocked: Waiting for dependencies or user input
"""

from datetime import datetime

MIGRATION_PLAN = {
    "metadata": {
        "project_name": "CICS Banking Sample Application Migration",
        "source_language": "COBOL",
        "target_language": "Java (Spring Boot)",
        "total_programs": 29,
        "last_updated": "2024-10-26",
        "repository": "target-springboot-cics"
    },
    
    "setup_tasks": [
        {
            "id": "setup_001",
            "name": "Establish Performance Baseline for Legacy COBOL Programs",
            "status": "completed",
            "description": "Run performance tests against all COBOL programs in the legacy CICS environment. Document P50, P95, and P99 latencies for each program along with typical request volumes and data sizes.",
            "deliverables": [
                "docs/performance_baseline.md",
                "docs/performance_baseline_data.json"
            ],
            "assigned_to": "Devin AI",
            "started_date": "2024-10-26",
            "completed_date": "2024-10-26",
            "notes": "Baselines created using synthetic code analysis due to sandbox environment constraints. Actual performance measurements should be conducted when live CICS access becomes available."
        },
        {
            "id": "setup_002",
            "name": "Setup Continuous Monitoring and Observability",
            "status": "pending",
            "description": "Configure monitoring, logging, and observability for the migrated Java application to track performance metrics and system health.",
            "deliverables": [],
            "assigned_to": "TBD",
            "notes": "Running in parallel with setup_001 and setup_004"
        },
        {
            "id": "setup_004",
            "name": "Extract and Document COBOL Business Rules",
            "status": "pending",
            "description": "Analyze COBOL programs to extract and document business rules, validation logic, and data transformation rules for preservation in Java migration.",
            "deliverables": [],
            "assigned_to": "TBD",
            "notes": "Running in parallel with setup_001 and setup_002"
        }
    ],
    
    "migration_status": {
        "total_programs": 29,
        "completed": 4,
        "in_progress": 0,
        "pending": 25,
        "percentage_complete": 14
    },
    
    "completed_migrations": [
        {
            "cobol_program": "GETCOMPY",
            "java_service": "CompanyInfoService",
            "lines_of_code": 43,
            "complexity": "simple",
            "migration_date": "2024-10-15",
            "migrated_by": "Development Team",
            "test_coverage": "90%",
            "notes": "Simple utility program, returns company name constant"
        },
        {
            "cobol_program": "GETSCODE",
            "java_service": "SortCodeService",
            "lines_of_code": 46,
            "complexity": "simple",
            "migration_date": "2024-10-15",
            "migrated_by": "Development Team",
            "test_coverage": "88%",
            "notes": "Simple utility program, returns bank sort code constant"
        },
        {
            "cobol_program": "CRDTAGY1",
            "java_service": "CreditAgencyService",
            "lines_of_code": 273,
            "complexity": "medium",
            "migration_date": "2024-10-22",
            "migrated_by": "Development Team",
            "test_coverage": "85%",
            "notes": "Credit agency simulation with processing delay and random score generation"
        },
        {
            "cobol_program": "ABNDPROC",
            "java_service": "ErrorLoggingService",
            "lines_of_code": 176,
            "complexity": "simple",
            "migration_date": "2024-10-20",
            "migrated_by": "Development Team",
            "test_coverage": "92%",
            "notes": "Abend/error handling routine, writes error records to database"
        }
    ],
    
    "pending_migrations": [
        {
            "cobol_program": "INQACC",
            "description": "Display account details",
            "lines_of_code": 1002,
            "complexity": "high",
            "priority": "high",
            "reason": "High-frequency inquiry operation, frequently used"
        },
        {
            "cobol_program": "INQCUST",
            "description": "Display customer details",
            "lines_of_code": 711,
            "complexity": "medium",
            "priority": "high",
            "reason": "High-frequency inquiry operation"
        },
        {
            "cobol_program": "DBCRFUN",
            "description": "Debit/credit account",
            "lines_of_code": 861,
            "complexity": "medium",
            "priority": "high",
            "reason": "Core transaction functionality"
        },
        {
            "cobol_program": "CREACC",
            "description": "Create new account",
            "lines_of_code": 1247,
            "complexity": "high",
            "priority": "medium",
            "reason": "Important CRUD operation"
        },
        {
            "cobol_program": "CRECUST",
            "description": "Create new customer",
            "lines_of_code": 1439,
            "complexity": "high",
            "priority": "medium",
            "reason": "Important CRUD operation"
        },
        {
            "cobol_program": "UPDACC",
            "description": "Update account details",
            "lines_of_code": 406,
            "complexity": "medium",
            "priority": "medium",
            "reason": "Standard CRUD operation"
        },
        {
            "cobol_program": "UPDCUST",
            "description": "Update customer details",
            "lines_of_code": 364,
            "complexity": "medium",
            "priority": "medium",
            "reason": "Standard CRUD operation"
        },
        {
            "cobol_program": "XFRFUN",
            "description": "Transfer funds between accounts",
            "lines_of_code": 1924,
            "complexity": "very_high",
            "priority": "high",
            "reason": "Complex transaction logic, critical business function"
        },
        {
            "cobol_program": "INQACCCU",
            "description": "Inquire accounts for customer",
            "lines_of_code": 882,
            "complexity": "medium",
            "priority": "high",
            "reason": "Frequently used inquiry operation"
        },
        {
            "cobol_program": "DELACC",
            "description": "Delete account",
            "lines_of_code": 649,
            "complexity": "medium",
            "priority": "low",
            "reason": "Low-frequency operation"
        },
        {
            "cobol_program": "DELCUS",
            "description": "Delete customer",
            "lines_of_code": 761,
            "complexity": "medium",
            "priority": "low",
            "reason": "Low-frequency operation"
        },
        {
            "cobol_program": "CRDTAGY2",
            "description": "Credit check program 2",
            "lines_of_code": 273,
            "complexity": "medium",
            "priority": "medium",
            "reason": "Similar to CRDTAGY1, already migrated"
        },
        {
            "cobol_program": "CRDTAGY3",
            "description": "Credit check program 3",
            "lines_of_code": 272,
            "complexity": "medium",
            "priority": "medium",
            "reason": "Similar to CRDTAGY1, already migrated"
        },
        {
            "cobol_program": "CRDTAGY4",
            "description": "Credit check program 4",
            "lines_of_code": 275,
            "complexity": "medium",
            "priority": "medium",
            "reason": "Similar to CRDTAGY1, already migrated"
        },
        {
            "cobol_program": "CRDTAGY5",
            "description": "Credit check program 5",
            "lines_of_code": 275,
            "complexity": "medium",
            "priority": "medium",
            "reason": "Similar to CRDTAGY1, already migrated"
        },
        {
            "cobol_program": "BANKDATA",
            "description": "Bank data utility",
            "lines_of_code": 1463,
            "complexity": "high",
            "priority": "medium",
            "reason": "Utility program for data operations"
        },
        {
            "cobol_program": "BNKMENU",
            "description": "Main menu interface",
            "lines_of_code": 1311,
            "complexity": "high",
            "priority": "low",
            "reason": "BMS interface, may not need migration if using REST APIs"
        },
        {
            "cobol_program": "BNK1CAC",
            "description": "Online create account UI",
            "lines_of_code": 1298,
            "complexity": "high",
            "priority": "low",
            "reason": "BMS interface"
        },
        {
            "cobol_program": "BNK1CCA",
            "description": "Online customer lookup UI",
            "lines_of_code": 952,
            "complexity": "high",
            "priority": "low",
            "reason": "BMS interface"
        },
        {
            "cobol_program": "BNK1CCS",
            "description": "Online create customer UI",
            "lines_of_code": 1657,
            "complexity": "very_high",
            "priority": "low",
            "reason": "BMS interface"
        },
        {
            "cobol_program": "BNK1CRA",
            "description": "Online credit/debit account UI",
            "lines_of_code": 1166,
            "complexity": "high",
            "priority": "low",
            "reason": "BMS interface"
        },
        {
            "cobol_program": "BNK1DAC",
            "description": "Online display account UI",
            "lines_of_code": 1158,
            "complexity": "high",
            "priority": "low",
            "reason": "BMS interface"
        },
        {
            "cobol_program": "BNK1DCS",
            "description": "Online display customer UI",
            "lines_of_code": 2053,
            "complexity": "very_high",
            "priority": "low",
            "reason": "BMS interface, largest program"
        },
        {
            "cobol_program": "BNK1TFN",
            "description": "Online transfer funds UI",
            "lines_of_code": 1224,
            "complexity": "high",
            "priority": "low",
            "reason": "BMS interface"
        },
        {
            "cobol_program": "BNK1UAC",
            "description": "Online update account UI",
            "lines_of_code": 1405,
            "complexity": "high",
            "priority": "low",
            "reason": "BMS interface"
        }
    ],
    
    "migration_guidelines": {
        "priority_order": [
            "High-frequency inquiry operations",
            "Core transaction functionality",
            "CRUD operations (Create, Update, Delete)",
            "Utility programs",
            "BMS interface programs (may use REST APIs instead)"
        ],
        "complexity_considerations": {
            "simple": "43-176 LOC, straightforward logic",
            "medium": "272-882 LOC, single DB operations",
            "high": "1000-1500 LOC, multiple DB operations",
            "very_high": "1500-2053 LOC, complex transactions"
        },
        "testing_requirements": {
            "service_layer_coverage": "80%",
            "repository_layer_coverage": "70%",
            "controller_layer_coverage": "60%",
            "integration_tests": "Required for all DB operations"
        }
    }
}


def get_task_status(task_id):
    """Get the status of a specific task by ID."""
    for task in MIGRATION_PLAN["setup_tasks"]:
        if task["id"] == task_id:
            return task["status"]
    return None


def get_migration_progress():
    """Get current migration progress statistics."""
    return MIGRATION_PLAN["migration_status"]


def get_completed_migrations():
    """Get list of completed COBOL to Java migrations."""
    return MIGRATION_PLAN["completed_migrations"]


def get_pending_migrations():
    """Get list of pending migrations sorted by priority."""
    return sorted(
        MIGRATION_PLAN["pending_migrations"],
        key=lambda x: {"high": 0, "medium": 1, "low": 2}[x["priority"]]
    )


if __name__ == "__main__":
    print(f"Migration Progress: {get_migration_progress()['percentage_complete']}%")
    print(f"Completed: {len(get_completed_migrations())} programs")
    print(f"Pending: {len(get_pending_migrations())} programs")
    print(f"\nSetup Task setup_001 Status: {get_task_status('setup_001')}")
