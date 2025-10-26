
TASKS = {
    "setup_001": {
        "name": "Establish Performance Baseline for Legacy COBOL Programs",
        "status": "pending",
        "description": "Measure performance baselines"
    },
    "setup_002": {
        "name": "Setup Continuous Monitoring and Observability",
        "status": "pending",
        "description": "Configure monitoring infrastructure"
    },
    "setup_004": {
        "name": "Extract and Document COBOL Business Rules",
        "status": "completed",
        "description": "Document all business rules from 29 COBOL programs",
        "documentation": "docs/COBOL_BUSINESS_RULES.md"
    }
}

def get_task_status(task_id):
    """Get the status of a specific task"""
    return TASKS.get(task_id, {}).get("status", "unknown")

def get_completed_tasks():
    """Get list of all completed tasks"""
    return [task_id for task_id, task in TASKS.items() if task["status"] == "completed"]

def get_pending_tasks():
    """Get list of all pending tasks"""
    return [task_id for task_id, task in TASKS.items() if task["status"] == "pending"]
