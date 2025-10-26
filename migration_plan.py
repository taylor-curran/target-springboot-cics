"""
Migration Task Tracking
Tracks the status of parallel setup and migration tasks
"""

tasks = {
    "setup_000": {
        "name": "Verify and Document Schema Consistency Test",
        "status": "not-complete",
        "description": "Verify schema consistency between test and production databases"
    },
    "setup_001": {
        "name": "Establish Performance Baseline for Legacy COBOL Programs",
        "status": "not-complete",
        "description": "Collect performance metrics for COBOL programs"
    },
    "setup_002": {
        "name": "Setup Continuous Monitoring and Observability",
        "status": "complete",
        "description": "Configure application monitoring with metrics collection, dashboards, and alerting",
        "completed_by": "Devin AI",
        "completion_date": "2025-10-26",
        "deliverables": [
            "Spring Boot Actuator configured",
            "Prometheus metrics exposed at /actuator/prometheus",
            "Grafana dashboards for customer, account, transaction domains",
            "Alert rules for error rates and performance degradation",
            "Monitoring infrastructure documented"
        ]
    }
}

def get_task_status(task_id):
    """Get the status of a specific task"""
    return tasks.get(task_id, {}).get("status", "unknown")

def mark_task_complete(task_id, completed_by="Unknown", notes=""):
    """Mark a task as complete"""
    if task_id in tasks:
        tasks[task_id]["status"] = "complete"
        tasks[task_id]["completed_by"] = completed_by
        if notes:
            tasks[task_id]["notes"] = notes
        return True
    return False

def get_completion_summary():
    """Get summary of task completion"""
    total = len(tasks)
    completed = sum(1 for t in tasks.values() if t["status"] == "complete")
    return {
        "total_tasks": total,
        "completed_tasks": completed,
        "completion_percentage": (completed / total * 100) if total > 0 else 0
    }

if __name__ == "__main__":
    print("Migration Task Status")
    print("=" * 50)
    for task_id, task in tasks.items():
        status_symbol = "✓" if task["status"] == "complete" else "○"
        print(f"{status_symbol} {task_id}: {task['name']}")
        print(f"   Status: {task['status']}")
        if task.get("completed_by"):
            print(f"   Completed by: {task['completed_by']}")
        print()
    
    summary = get_completion_summary()
    print("=" * 50)
    print(f"Progress: {summary['completed_tasks']}/{summary['total_tasks']} tasks complete ({summary['completion_percentage']:.1f}%)")
