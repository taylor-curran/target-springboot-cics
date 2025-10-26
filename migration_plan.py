"""
Migration Task Tracking for CICS Banking Sample Application

This file tracks the status of migration tasks and their completion state.
Each task includes a name, status, and optional metadata about completion.

Status values:
- "pending": Task not yet started
- "in_progress": Task currently being worked on
- "completed": Task finished and verified
- "blocked": Task waiting on dependencies or external factors
"""

migration_tasks = {
    "setup_001": {
        "name": "Establish Performance Baseline for Legacy COBOL Programs",
        "status": "pending",
        "description": "Measure and document performance metrics for all 29 COBOL programs",
        "priority": "high",
        "assigned_to": None,
        "started_date": None,
        "completed_date": None
    },
    "setup_002": {
        "name": "Setup Continuous Monitoring and Observability",
        "status": "pending",
        "description": "Implement monitoring infrastructure for Java application",
        "priority": "high",
        "assigned_to": None,
        "started_date": None,
        "completed_date": None
    },
    "setup_004": {
        "name": "Extract and Document COBOL Business Rules",
        "status": "completed",
        "description": "Comprehensive documentation of business rules from 29 COBOL programs",
        "priority": "critical",
        "assigned_to": "devin",
        "started_date": "2024-10-26",
        "completed_date": "2024-10-26",
        "documentation": "docs/COBOL_BUSINESS_RULES.md",
        "deliverables": [
            "Date validation rules (CEEDAYS constraints, fail codes O/Y/Z)",
            "Composite key patterns (sortcode + customer/account number)",
            "Named Counter logic (ENQ/DEQ for ID generation)",
            "Transaction logging to PROCTRAN (18 transaction types)",
            "Credit scoring integration (async API, 3-second timeout)",
            "Error handling patterns (fail codes, SQLCODE, RESP codes, ABNDPROC)"
        ],
        "programs_analyzed": 29,
        "key_findings": {
            "date_validation": "CEEDAYS minimum year 1601, maximum age 150, future date rejection",
            "composite_keys": "Customer (6+10 digits), Account (6+8 digits)",
            "named_counter": "ENQ/DEQ locking with rollback on failure",
            "proctran_logging": "18 transaction types, excludes UPDCUST/UPDACC",
            "credit_scoring": "Async calls to multiple agencies, 3-second timeout, averaging",
            "error_handling": "Fail codes: 3 (ENQ), C (credit), O (range), Y (future), Z (system)"
        }
    }
}

def get_task_status(task_id):
    """Get the status of a specific task"""
    task = migration_tasks.get(task_id)
    if task:
        return task["status"]
    return None

def get_completed_tasks():
    """Get list of completed task IDs"""
    return [task_id for task_id, task in migration_tasks.items() 
            if task["status"] == "completed"]

def get_pending_tasks():
    """Get list of pending task IDs"""
    return [task_id for task_id, task in migration_tasks.items() 
            if task["status"] == "pending"]

def mark_task_completed(task_id, completed_date=None):
    """Mark a task as completed"""
    if task_id in migration_tasks:
        migration_tasks[task_id]["status"] = "completed"
        if completed_date:
            migration_tasks[task_id]["completed_date"] = completed_date
        return True
    return False

def get_task_summary():
    """Get summary statistics of all tasks"""
    total = len(migration_tasks)
    completed = len(get_completed_tasks())
    pending = len(get_pending_tasks())
    in_progress = sum(1 for task in migration_tasks.values() 
                      if task["status"] == "in_progress")
    blocked = sum(1 for task in migration_tasks.values() 
                  if task["status"] == "blocked")
    
    return {
        "total": total,
        "completed": completed,
        "pending": pending,
        "in_progress": in_progress,
        "blocked": blocked,
        "completion_percentage": (completed / total * 100) if total > 0 else 0
    }

if __name__ == "__main__":
    print("CICS Banking Application - Migration Task Status")
    print("=" * 60)
    
    summary = get_task_summary()
    print(f"\nOverall Progress: {summary['completion_percentage']:.1f}%")
    print(f"  Total Tasks: {summary['total']}")
    print(f"  Completed: {summary['completed']}")
    print(f"  In Progress: {summary['in_progress']}")
    print(f"  Pending: {summary['pending']}")
    print(f"  Blocked: {summary['blocked']}")
    
    print("\nTask Details:")
    print("-" * 60)
    for task_id, task in sorted(migration_tasks.items()):
        status_symbol = {
            "completed": "✅",
            "in_progress": "🔄",
            "pending": "⏳",
            "blocked": "🚫"
        }.get(task["status"], "❓")
        
        print(f"\n{status_symbol} {task_id}: {task['name']}")
        print(f"   Status: {task['status']}")
        print(f"   Priority: {task.get('priority', 'N/A')}")
        if task.get('completed_date'):
            print(f"   Completed: {task['completed_date']}")
        if task.get('documentation'):
            print(f"   Documentation: {task['documentation']}")
