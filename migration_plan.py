"""
Migration Task Tracking for COBOL to Spring Boot Migration
Tracks the status of setup and migration tasks
"""

migration_tasks = {
    "setup_001": {
        "name": "Establish Performance Baseline for Legacy COBOL Programs",
        "status": "pending",
        "description": "Baseline performance metrics collection for COBOL programs",
        "dependencies": []
    },
    "setup_002": {
        "name": "Setup Continuous Monitoring and Observability",
        "status": "completed",
        "description": "Configure Spring Actuator, metrics collection, Grafana dashboards, and alerting",
        "dependencies": [],
        "completion_date": "2025-10-26",
        "artifacts": [
            "monitoring/grafana-dashboard-customer.json",
            "monitoring/grafana-dashboard-account.json",
            "monitoring/grafana-dashboard-transaction.json",
            "monitoring/prometheus-alerts.yml",
            "monitoring/README.md"
        ]
    },
    "setup_004": {
        "name": "Extract and Document COBOL Business Rules",
        "status": "pending",
        "description": "Document business rules from COBOL programs for validation",
        "dependencies": []
    }
}

def get_task_status(task_id):
    """Get the status of a specific task"""
    return migration_tasks.get(task_id, {}).get("status", "unknown")

def update_task_status(task_id, status, completion_date=None):
    """Update the status of a task"""
    if task_id in migration_tasks:
        migration_tasks[task_id]["status"] = status
        if completion_date:
            migration_tasks[task_id]["completion_date"] = completion_date
        return True
    return False

def get_completed_tasks():
    """Get list of completed tasks"""
    return [task_id for task_id, task in migration_tasks.items() 
            if task.get("status") == "completed"]

def get_pending_tasks():
    """Get list of pending tasks"""
    return [task_id for task_id, task in migration_tasks.items() 
            if task.get("status") == "pending"]

def get_progress():
    """Get overall migration progress"""
    total = len(migration_tasks)
    completed = len(get_completed_tasks())
    return {
        "total": total,
        "completed": completed,
        "pending": total - completed,
        "percentage": (completed / total * 100) if total > 0 else 0
    }

if __name__ == "__main__":
    print("Migration Task Tracking")
    print("=" * 50)
    progress = get_progress()
    print(f"\nProgress: {progress['completed']}/{progress['total']} tasks completed ({progress['percentage']:.1f}%)")
    print(f"\nCompleted Tasks ({len(get_completed_tasks())}):")
    for task_id in get_completed_tasks():
        task = migration_tasks[task_id]
        print(f"  - {task_id}: {task['name']}")
    
    print(f"\nPending Tasks ({len(get_pending_tasks())}):")
    for task_id in get_pending_tasks():
        task = migration_tasks[task_id]
        print(f"  - {task_id}: {task['name']}")
