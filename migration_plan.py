#!/usr/bin/env python3
"""
COBOL to Java Migration Task Tracker

Tracks progress of migration tasks for the CICS Banking Sample Application.
This module provides a structured way to manage the migration of 29 COBOL programs
to a modern Java Spring Boot architecture.

Usage:
    python migration_plan.py                    # Display migration status
    python migration_plan.py --task setup_004   # Display specific task details
"""

from enum import Enum
from dataclasses import dataclass, field
from typing import List, Optional
from datetime import datetime
import argparse
import sys


class TaskStatus(Enum):
    """Status of a migration task"""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    BLOCKED = "blocked"


@dataclass
class MigrationTask:
    """Represents a single migration task"""
    task_id: str
    title: str
    description: str
    status: TaskStatus
    assigned_to: Optional[str] = None
    completed_date: Optional[datetime] = None
    dependencies: List[str] = field(default_factory=list)
    notes: Optional[str] = None
    deliverables: List[str] = field(default_factory=list)


MIGRATION_TASKS = {
    "setup_001": MigrationTask(
        task_id="setup_001",
        title="Establish Performance Baseline for Legacy COBOL Programs",
        description="Measure and document performance metrics for all 29 COBOL programs including "
                    "response times, throughput, resource utilization, and transaction volumes. "
                    "Create baseline report for comparison with Java implementation.",
        status=TaskStatus.PENDING,
        dependencies=[],
        deliverables=[
            "Performance baseline report",
            "Metrics collection scripts",
            "Benchmark test suite"
        ]
    ),
    
    "setup_002": MigrationTask(
        task_id="setup_002",
        title="Setup Continuous Monitoring and Observability",
        description="Implement monitoring and observability infrastructure for both COBOL and Java "
                    "systems. Include metrics collection, logging, tracing, and alerting.",
        status=TaskStatus.PENDING,
        dependencies=[],
        deliverables=[
            "Monitoring dashboards",
            "Alert configuration",
            "Log aggregation setup",
            "Tracing infrastructure"
        ]
    ),
    
    "setup_004": MigrationTask(
        task_id="setup_004",
        title="Extract and Document COBOL Business Rules",
        description="Review all 29 COBOL programs in og-cics-cobol-app/src/base/cobol_src/ and document: "
                    "(1) Date validation rules (CEEDAYS minimum year 1601, maximum age 150, future date "
                    "rejection with fail codes 'O' for year/age violations and 'Y' for future dates), "
                    "(2) Composite key patterns (sort_code + customer_number), (3) Named Counter logic "
                    "for ID generation with ENQ/DEQ, (4) Transaction logging to PROCTRAN with all 18 "
                    "transaction types, (5) Credit scoring integration with 5 async agencies, "
                    "(6) Error handling patterns including centralized abend logging.",
        status=TaskStatus.COMPLETED,
        completed_date=datetime.now(),
        dependencies=[],
        notes="Comprehensive documentation created in docs/COBOL_BUSINESS_RULES.md covering all "
              "business rules from 29 COBOL programs. Each rule includes: "
              "- COBOL source reference (file name and line numbers) "
              "- Business justification explaining why the rule exists "
              "- Java translation requirements with code examples "
              "- Test scenarios for validation "
              "\n\n"
              "All 6 required categories fully documented: "
              "1. Date validation rules (CEEDAYS constraints, age limits, future date rejection) "
              "2. Composite key patterns (customer, account, transaction) "
              "3. Named Counter logic (ENQ/DEQ for atomic ID generation with rollback) "
              "4. Transaction logging to PROCTRAN (structure, types, patterns) "
              "5. Credit scoring integration (5-agency async API, aggregation, fallback) "
              "6. Error handling (centralized abend logging, fail codes, retry patterns) "
              "\n\n"
              "Additional patterns discovered and documented: "
              "- Account update restrictions (metadata only, no balance updates) "
              "- Customer deletion cascade pattern "
              "- Eye-catcher validation for data integrity "
              "- Date format standardization "
              "\n\n"
              "Documentation cross-referenced with copybooks: CUSTOMER.cpy, ACCOUNT.cpy, PROCTRAN.cpy",
        deliverables=[
            "docs/COBOL_BUSINESS_RULES.md - Comprehensive business rules documentation",
            "migration_plan.py - Task tracking system"
        ]
    ),
}


def get_task(task_id: str) -> Optional[MigrationTask]:
    """Retrieve a task by ID"""
    return MIGRATION_TASKS.get(task_id)


def update_task_status(task_id: str, status: TaskStatus, notes: Optional[str] = None) -> bool:
    """Update the status of a task"""
    task = MIGRATION_TASKS.get(task_id)
    if task:
        task.status = status
        if status == TaskStatus.COMPLETED:
            task.completed_date = datetime.now()
        if notes:
            task.notes = notes
        return True
    return False


def get_tasks_by_status(status: TaskStatus) -> List[MigrationTask]:
    """Get all tasks with a specific status"""
    return [task for task in MIGRATION_TASKS.values() if task.status == status]


def get_completion_percentage() -> float:
    """Calculate overall migration completion percentage"""
    total_tasks = len(MIGRATION_TASKS)
    completed_tasks = len(get_tasks_by_status(TaskStatus.COMPLETED))
    return (completed_tasks / total_tasks * 100) if total_tasks > 0 else 0.0


def display_summary():
    """Display migration status summary"""
    print("=" * 80)
    print("COBOL to Java Migration Status")
    print("=" * 80)
    print(f"\nOverall Completion: {get_completion_percentage():.1f}%")
    print(f"\nTotal Tasks: {len(MIGRATION_TASKS)}")
    
    for status in TaskStatus:
        tasks = get_tasks_by_status(status)
        icon = "✅" if status == TaskStatus.COMPLETED else "⏳" if status == TaskStatus.IN_PROGRESS else "🔴" if status == TaskStatus.BLOCKED else "⭕"
        print(f"{icon} {status.value.replace('_', ' ').title()}: {len(tasks)}")
    
    print("\n" + "=" * 80)
    print("Task Details")
    print("=" * 80)
    
    for task in MIGRATION_TASKS.values():
        print(f"\n[{task.task_id}] {task.title}")
        print(f"Status: {task.status.value}")
        
        if task.assigned_to:
            print(f"Assigned to: {task.assigned_to}")
        
        if task.completed_date:
            print(f"Completed: {task.completed_date.strftime('%Y-%m-%d %H:%M:%S')}")
        
        if task.dependencies:
            print(f"Dependencies: {', '.join(task.dependencies)}")
        
        if task.deliverables:
            print("Deliverables:")
            for deliverable in task.deliverables:
                print(f"  - {deliverable}")
        
        if task.notes:
            print(f"\nNotes:\n{task.notes}")


def display_task_detail(task_id: str):
    """Display detailed information about a specific task"""
    task = get_task(task_id)
    
    if not task:
        print(f"Error: Task '{task_id}' not found")
        return
    
    print("=" * 80)
    print(f"Task Details: {task_id}")
    print("=" * 80)
    print(f"\nTitle: {task.title}")
    print(f"\nStatus: {task.status.value}")
    
    if task.assigned_to:
        print(f"Assigned to: {task.assigned_to}")
    
    if task.completed_date:
        print(f"Completed: {task.completed_date.strftime('%Y-%m-%d %H:%M:%S')}")
    
    if task.dependencies:
        print(f"\nDependencies: {', '.join(task.dependencies)}")
    
    print(f"\nDescription:\n{task.description}")
    
    if task.deliverables:
        print("\nDeliverables:")
        for deliverable in task.deliverables:
            print(f"  - {deliverable}")
    
    if task.notes:
        print(f"\nNotes:\n{task.notes}")


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="COBOL to Java Migration Task Tracker",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        '--task',
        help='Display details for a specific task'
    )
    
    args = parser.parse_args()
    
    if args.task:
        display_task_detail(args.task)
    else:
        display_summary()


if __name__ == "__main__":
    main()
