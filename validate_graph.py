#!/usr/bin/env python3
"""
Validation script for migration plan task graphs.
Run this to check for common issues in your migration_plan.py
"""
import sys
from typing import Dict, List, Set, Any

def check_unique_ids(tasks: List[Dict]) -> List[str]:
    """Check for duplicate task IDs."""
    errors = []
    seen_ids = set()
    duplicates = set()
    
    for task in tasks:
        task_id = task.get('id', '')
        if task_id in seen_ids:
            duplicates.add(task_id)
        seen_ids.add(task_id)
    
    if duplicates:
        errors.append(f"❌ Duplicate IDs found: {duplicates}")
    else:
        print("✅ All task IDs are unique")
    
    return errors

def check_naming_conventions(tasks: List[Dict]) -> List[str]:
    """Verify tasks follow naming conventions."""
    errors = []
    valid_prefixes = ['setup_', 'migrate_', 'integrate_', 'validator_']
    
    for task in tasks:
        task_id = task.get('id', '')
        if not any(task_id.startswith(prefix) for prefix in valid_prefixes):
            errors.append(f"❌ Invalid ID prefix: {task_id} (must start with: {valid_prefixes})")
    
    if not errors:
        print("✅ All tasks follow naming conventions")
    
    return errors

def check_field_brevity(tasks: List[Dict]) -> List[str]:
    """Check that fields respect length limits."""
    errors = []
    
    for task in tasks:
        task_id = task.get('id', '')
        
        # Check title length (10 words max)
        title = task.get('title', '')
        if len(title.split()) > 10:
            errors.append(f"❌ {task_id}: title too long ({len(title.split())} words, max 10)")
        
        # Check content length (3 sentences max)
        content = task.get('content', '')
        sentences = content.count('.') + content.count('!') + content.count('?')
        if sentences > 3:
            errors.append(f"❌ {task_id}: content too long ({sentences} sentences, max 3)")
        
        # Check action if present (2 sentences max)
        if 'action' in task:
            action = task['action']
            action_sentences = action.count('.') + action.count('!') + action.count('?')
            if action_sentences > 2:
                errors.append(f"❌ {task_id}: action too long ({action_sentences} sentences, max 2)")
    
    if not errors:
        print("✅ All fields respect brevity limits")
    
    return errors

def check_dependencies_exist(tasks: List[Dict]) -> List[str]:
    """Verify all dependencies reference existing tasks."""
    errors = []
    task_ids = {task.get('id') for task in tasks}
    
    for task in tasks:
        task_id = task.get('id', '')
        deps = task.get('depends_on', [])
        
        for dep in deps:
            if dep not in task_ids:
                errors.append(f"❌ {task_id}: depends on non-existent task '{dep}'")
    
    if not errors:
        print("✅ All dependencies reference existing tasks")
    
    return errors

def detect_cycles(tasks: List[Dict]) -> List[str]:
    """Detect circular dependencies in the task graph."""
    errors = []
    
    # Build adjacency list
    graph = {}
    for task in tasks:
        task_id = task.get('id', '')
        graph[task_id] = task.get('depends_on', [])
    
    # DFS to detect cycles
    visited = set()
    rec_stack = set()
    
    def has_cycle(node: str, path: List[str] = None) -> bool:
        if path is None:
            path = []
        
        visited.add(node)
        rec_stack.add(node)
        path.append(node)
        
        for neighbor in graph.get(node, []):
            if neighbor not in visited:
                if has_cycle(neighbor, path.copy()):
                    return True
            elif neighbor in rec_stack:
                cycle_start = path.index(neighbor)
                cycle = path[cycle_start:] + [neighbor]
                errors.append(f"❌ Circular dependency detected: {' -> '.join(cycle)}")
                return True
        
        rec_stack.remove(node)
        return False
    
    for task_id in graph:
        if task_id not in visited:
            has_cycle(task_id)
    
    if not errors:
        print("✅ No circular dependencies detected")
    
    return errors

def check_time_estimates(tasks: List[Dict]) -> List[str]:
    """Check that time estimates are reasonable."""
    errors = []
    warnings = []
    
    for task in tasks:
        task_id = task.get('id', '')
        hours = task.get('estimated_hours', 0)
        
        if hours < 4:
            warnings.append(f"⚠️  {task_id}: very short estimate ({hours} hours)")
        elif hours > 20:
            errors.append(f"❌ {task_id}: task too large ({hours} hours, max 20)")
    
    if warnings:
        print("\n".join(warnings))
    
    if not errors:
        print("✅ All time estimates within bounds")
    
    return errors

def check_required_fields(tasks: List[Dict]) -> List[str]:
    """Verify all required fields are present."""
    errors = []
    required_fields = ['id', 'title', 'content', 'status']
    
    for task in tasks:
        task_id = task.get('id', '<no-id>')
        missing = []
        
        for field in required_fields:
            if field not in task or task[field] == "":
                missing.append(field)
        
        if missing:
            errors.append(f"❌ {task_id}: missing required fields {missing}")
    
    if not errors:
        print("✅ All required fields present")
    
    return errors

def check_status_values(tasks: List[Dict]) -> List[str]:
    """Check that status values are valid."""
    errors = []
    valid_statuses = ['not-complete', 'completed']
    
    for task in tasks:
        task_id = task.get('id', '')
        status = task.get('status', '')
        
        if status not in valid_statuses:
            errors.append(f"❌ {task_id}: invalid status '{status}' (must be: {valid_statuses})")
    
    if not errors:
        print("✅ All status values are valid")
    
    return errors

def validate_migration_plan(file_path: str = 'migration_plan.py') -> bool:
    """Main validation function."""
    print("\n🔍 Validating Migration Plan Graph...\n")
    
    try:
        # Import the migration plan
        import importlib.util
        spec = importlib.util.spec_from_file_location("migration_plan", file_path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        
        if not hasattr(module, 'migration_plan'):
            print("❌ migration_plan variable not found in file")
            return False
        
        tasks = module.migration_plan.get('tasks', [])
        
        if not tasks:
            print("❌ No tasks found in migration plan")
            return False
        
        print(f"📊 Found {len(tasks)} tasks\n")
        
        # Run all checks
        all_errors = []
        all_errors.extend(check_unique_ids(tasks))
        all_errors.extend(check_naming_conventions(tasks))
        all_errors.extend(check_field_brevity(tasks))
        all_errors.extend(check_required_fields(tasks))
        all_errors.extend(check_status_values(tasks))
        all_errors.extend(check_dependencies_exist(tasks))
        all_errors.extend(detect_cycles(tasks))
        all_errors.extend(check_time_estimates(tasks))
        
        # Report results
        print("\n" + "="*50)
        if all_errors:
            print("\n❌ VALIDATION FAILED\n")
            for error in all_errors:
                print(error)
            return False
        else:
            print("\n✅ VALIDATION PASSED - Graph looks good!\n")
            return True
            
    except Exception as e:
        print(f"❌ Error loading migration plan: {e}")
        return False

if __name__ == "__main__":
    file_path = sys.argv[1] if len(sys.argv) > 1 else "migration_plan.py"
    sys.exit(0 if validate_migration_plan(file_path) else 1)
