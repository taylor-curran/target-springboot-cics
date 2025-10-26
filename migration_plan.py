#!/usr/bin/env python3
"""
Migration task tracking for COBOL to Spring Boot migration.
Tracks status of parallel setup and migration tasks.
"""

import json
from datetime import datetime
from typing import Dict, List

TASKS = {
    "setup_001": {
        "name": "Establish Performance Baseline for Legacy COBOL Programs",
        "status": "pending",
        "assignee": "parallel_worker_1",
        "updated": None
    },
    "setup_002": {
        "name": "Setup Continuous Monitoring and Observability",
        "status": "completed",
        "assignee": "devin",
        "updated": datetime.now().isoformat(),
        "completion_notes": "Configured Spring Boot Actuator with Micrometer Prometheus registry. Created Grafana dashboards for customer, account, and transaction domains. Set up alerting rules for performance degradation and errors."
    },
    "setup_004": {
        "name": "Extract and Document COBOL Business Rules",
        "status": "pending",
        "assignee": "parallel_worker_2",
        "updated": None
    }
}

def get_task_status(task_id: str) -> Dict:
    """Get status of a specific task."""
    return TASKS.get(task_id, {})

def update_task_status(task_id: str, status: str, notes: str = None):
    """Update task status."""
    if task_id in TASKS:
        TASKS[task_id]["status"] = status
        TASKS[task_id]["updated"] = datetime.now().isoformat()
        if notes:
            TASKS[task_id]["completion_notes"] = notes
        print(f"Updated {task_id} to status: {status}")
    else:
        print(f"Task {task_id} not found")

def list_all_tasks() -> List[Dict]:
    """List all tasks and their statuses."""
    return [{"id": k, **v} for k, v in TASKS.items()]

def get_summary():
    """Get summary of task statuses."""
    total = len(TASKS)
    completed = sum(1 for t in TASKS.values() if t["status"] == "completed")
    pending = sum(1 for t in TASKS.values() if t["status"] == "pending")
    in_progress = sum(1 for t in TASKS.values() if t["status"] == "in_progress")
    
    return {
        "total": total,
        "completed": completed,
        "pending": pending,
        "in_progress": in_progress,
        "completion_percentage": (completed / total * 100) if total > 0 else 0
    }

if __name__ == "__main__":
    print("Migration Task Status")
    print("=" * 60)
    summary = get_summary()
    print(f"Total Tasks: {summary['total']}")
    print(f"Completed: {summary['completed']}")
    print(f"In Progress: {summary['in_progress']}")
    print(f"Pending: {summary['pending']}")
    print(f"Completion: {summary['completion_percentage']:.1f}%")
    print("\nTask Details:")
    print("-" * 60)
    for task in list_all_tasks():
        status_symbol = "✅" if task["status"] == "completed" else "⏳" if task["status"] == "in_progress" else "⭕"
        print(f"{status_symbol} {task['id']}: {task['name']}")
        print(f"   Status: {task['status']}")
        if task.get("completion_notes"):
            print(f"   Notes: {task['completion_notes']}")
