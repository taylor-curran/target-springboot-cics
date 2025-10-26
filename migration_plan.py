#!/usr/bin/env python3
"""
Migration Plan Tracker for CBSA COBOL to Java Migration
Tracks status of setup and migration tasks
"""

migration_tasks = {
    "setup_001": {
        "name": "Establish Performance Baseline for Legacy COBOL Programs",
        "status": "pending",
        "description": "Establish performance baselines for COBOL programs",
        "owner": "parallel-task-1"
    },
    "setup_002": {
        "name": "Setup Continuous Monitoring and Observability",
        "status": "completed",
        "description": "Configure monitoring with metrics collection, dashboards, and alerting",
        "owner": "current-session",
        "completed_features": [
            "Spring Boot Actuator integration",
            "Prometheus metrics export",
            "Grafana dashboards for customer/account/transaction domains",
            "Alert rules for performance degradation and errors",
            "Logging aggregation configuration",
            "24-hour baseline collection documentation"
        ]
    },
    "setup_004": {
        "name": "Extract and Document COBOL Business Rules",
        "status": "pending",
        "description": "Extract and document business rules from COBOL programs",
        "owner": "parallel-task-2"
    }
}

def get_task_status(task_id):
    """Get the status of a specific task"""
    return migration_tasks.get(task_id, {}).get("status", "unknown")

def update_task_status(task_id, new_status):
    """Update the status of a specific task"""
    if task_id in migration_tasks:
        migration_tasks[task_id]["status"] = new_status
        return True
    return False

def get_completed_tasks():
    """Get list of completed tasks"""
    return [task_id for task_id, task in migration_tasks.items() 
            if task["status"] == "completed"]

def get_pending_tasks():
    """Get list of pending tasks"""
    return [task_id for task_id, task in migration_tasks.items() 
            if task["status"] == "pending"]

if __name__ == "__main__":
    print("CBSA Migration Plan Status")
    print("=" * 50)
    for task_id, task in migration_tasks.items():
        status_icon = "✓" if task["status"] == "completed" else "○"
        print(f"{status_icon} {task_id}: {task['name']} [{task['status']}]")
    
    print("\n" + "=" * 50)
    print(f"Completed: {len(get_completed_tasks())}/{len(migration_tasks)}")
    print(f"Pending: {len(get_pending_tasks())}/{len(migration_tasks)}")
