"""
Migration Plan Task Tracking

This module tracks the status of migration tasks for the CICS COBOL to Java Spring Boot
migration project. It provides functions to query task status, list completed/pending tasks,
and manage the overall migration progress.

Usage:
    import migration_plan
    
    status = migration_plan.get_task_status('setup_001')
    completed = migration_plan.get_completed_tasks()
    pending = migration_plan.get_pending_tasks()
"""

from datetime import datetime
from typing import Dict, List, Optional


TASKS = {
    "setup_001": {
        "name": "Establish Performance Baseline for Legacy COBOL Programs",
        "status": "completed",
        "description": "Run performance tests against all COBOL programs and document P50, P95, P99 latencies",
        "completed_date": "2025-10-26",
        "deliverables": [
            "docs/performance/PERFORMANCE_BASELINE.md",
            "docs/performance/performance_data.json"
        ],
        "notes": "Baseline created using synthetic metrics based on program complexity analysis"
    },
    "setup_002": {
        "name": "Setup Continuous Monitoring and Observability",
        "status": "pending",
        "description": "Setup monitoring infrastructure for migrated applications",
        "completed_date": None,
        "deliverables": [],
        "notes": "Running in parallel with setup_001"
    },
    "setup_004": {
        "name": "Extract and Document COBOL Business Rules",
        "status": "pending",
        "description": "Document business rules from COBOL programs for migration reference",
        "completed_date": None,
        "deliverables": [],
        "notes": "Running in parallel with setup_001"
    }
}


def get_task_status(task_id: str) -> str:
    """
    Get the status of a specific task.
    
    Args:
        task_id: The unique identifier for the task (e.g., 'setup_001')
    
    Returns:
        The status string ('completed', 'pending', 'in_progress', 'blocked') or 'unknown' if not found
    """
    task = TASKS.get(task_id)
    if task is None:
        return "unknown"
    return task.get("status", "unknown")


def get_task_details(task_id: str) -> Optional[Dict]:
    """
    Get full details of a specific task.
    
    Args:
        task_id: The unique identifier for the task
    
    Returns:
        Dictionary containing all task details or None if not found
    """
    return TASKS.get(task_id)


def get_completed_tasks() -> List[str]:
    """
    Get list of completed task IDs.
    
    Returns:
        List of task IDs with status 'completed'
    """
    return [task_id for task_id, task in TASKS.items() if task["status"] == "completed"]


def get_pending_tasks() -> List[str]:
    """
    Get list of pending task IDs.
    
    Returns:
        List of task IDs with status 'pending'
    """
    return [task_id for task_id, task in TASKS.items() if task["status"] == "pending"]


def get_all_tasks() -> Dict:
    """
    Get all tasks with their details.
    
    Returns:
        Dictionary of all tasks
    """
    return TASKS.copy()


def get_migration_progress() -> Dict[str, float]:
    """
    Calculate overall migration progress statistics.
    
    Returns:
        Dictionary with progress metrics
    """
    total = len(TASKS)
    completed = len(get_completed_tasks())
    pending = len(get_pending_tasks())
    
    return {
        "total_tasks": total,
        "completed_tasks": completed,
        "pending_tasks": pending,
        "completion_percentage": (completed / total * 100) if total > 0 else 0.0
    }


def print_migration_status():
    """
    Print a formatted summary of migration status.
    """
    progress = get_migration_progress()
    
    print("=" * 60)
    print("CICS COBOL to Java Spring Boot Migration Status")
    print("=" * 60)
    print(f"Total Tasks: {progress['total_tasks']}")
    print(f"Completed: {progress['completed_tasks']}")
    print(f"Pending: {progress['pending_tasks']}")
    print(f"Progress: {progress['completion_percentage']:.1f}%")
    print("=" * 60)
    print()
    
    print("Completed Tasks:")
    for task_id in get_completed_tasks():
        task = TASKS[task_id]
        print(f"  ✓ {task_id}: {task['name']}")
        print(f"    Completed: {task['completed_date']}")
    print()
    
    print("Pending Tasks:")
    for task_id in get_pending_tasks():
        task = TASKS[task_id]
        print(f"  ○ {task_id}: {task['name']}")
    print()


if __name__ == "__main__":
    print_migration_status()
