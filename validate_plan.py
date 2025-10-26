#!/usr/bin/env python3
import sys
sys.path.insert(0, '.')
from migration_plan import migration_plan

tasks = migration_plan['tasks']
print(f'Total tasks: {len(tasks)}')

completed = [t for t in tasks if t['status'] == 'completed']
pending = [t for t in tasks if t['status'] == 'pending']
setup = [t for t in tasks if t['id'].startswith('setup_')]
validators = [t for t in tasks if t['id'].startswith('validator_')]
migrations = [t for t in tasks if t['id'].startswith('migrate_')]

print(f'Completed: {len(completed)} (5 migrated + 9 UI)')
print(f'Pending: {len(pending)}')
print(f'  - Setup: {len(setup)}')
print(f'  - Validators: {len(validators)}')
print(f'  - Migrations: {len(migrations)}')
print()

all_valid = True
for task in migrations:
    deps = task.get('depends_on', [])
    has_validator = any(d.startswith('validator_') for d in deps)
    if not has_validator:
        print(f'❌ WARNING: {task["id"]} has no validator dependency!')
        all_valid = False
    else:
        print(f'✓ {task["id"]} depends on validators: {[d for d in deps if d.startswith("validator_")]}')

print()
if all_valid:
    print('✓ All migration tasks have validator dependencies')
else:
    print('❌ Some migration tasks are missing validator dependencies')
    sys.exit(1)
