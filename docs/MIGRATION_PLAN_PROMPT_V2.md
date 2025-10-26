# Migration Plan Task Graph Generation (Improved v2)

## Context & Clarifications

**IMPORTANT DISTINCTIONS:**
- **Completed Nodes** = Migrated programs + UI programs (as dependencies)
- **Tasks** ≠ **Programs**: One task can handle multiple similar programs
- **Example**: 8 migration tasks can cover 15 programs when grouping related functionality

## Migration Parameters

- **Source Repository**: `[SOURCE_REPO]` (e.g., taylor-curran/og-cics-cobol-app)
- **Target Repository**: `[TARGET_REPO]` (e.g., taylor-curran/target-springboot-cics)

## YOUR MISSION

Generate a task graph for migrating from source to target repository. Each task is a working session for an agent. Analyze both repos to understand current state and what needs migration.

**Key Counting Principle:**
```
Total Programs = Migrated + UI Programs + Remaining to Migrate
Example: 29 = 5 + 9 + 15

Total Task Nodes = Completed Nodes + Pending Tasks
Example: 32 = 14 + 18
Where: 14 completed = 5 migrated + 9 UI programs (as dependencies)
       18 pending = 2 setup + 8 validators + 8 migrations
```

## THE GOLDEN RULE: Build Validation Before Migration

**Every migration task MUST have measurable validation. If validation doesn't exist, CREATE IT FIRST.**

This means:
- **Tests come before code**: Can't migrate without tests to verify correctness
- **Metrics come before claims**: Can't claim "within 10% performance" without baseline metrics
- **Coverage comes before completion**: Can't declare done without coverage reports

The dependency chain should always look like:
```
[Setup Infrastructure] → [Create Tests] → [Migrate Code Using Tests]
                     ↗                  ↗
[Establish Baselines]                   
```

Never:
```
[Migrate Code] → [Write Tests After]  ❌ WRONG
```

## Task Node Structure

```python
{
    "id": "unique_id",  # completed_*, setup_*, validator_*, migrate_*
    "title": "Short descriptive title",
    "content": "What this working session accomplishes",
    "status": "completed" | "pending",  # Use "completed" for migrated programs AND UI programs
    "depends_on": ["task_ids"],  # Dependencies define execution order
    "action": "Brief actionable description of what to do (1-2 sentences max)",
    "definition_of_done": "Clear, measurable success criteria",
    "validation_mechanism": "How to concretely test, validate, sanity check, cross reference (tests, data quality metrics, observability metrics and data, coverage) - NOT just code review",
    "estimated_hours": 8,  # Target: 6-12 hours per task (can group similar programs)
    "deliverables": ["concrete_outputs.java"]  # Optional: specific files produced
}
```

## Completed Nodes (Two Types)

### 1. Migrated Programs
Programs already fully migrated to target with implementations and tests:
```python
{
    "id": "completed_001",
    "title": "CRDTAGY1 → CreditAgencyService",
    "content": "Credit agency program migrated...",
    "status": "completed",
    "depends_on": [],
    "action": "Already migrated - CreditAgencyService.java implements CRDTAGY1",
    ...
}
```

### 2. UI Programs (As Dependencies)
3270 terminal UI programs replaced by REST API architecture:
```python
{
    "id": "completed_006",
    "title": "BNKMENU - Main Menu UI",
    "content": "3270 terminal menu program - upstream dependency for business logic",
    "status": "completed",
    "depends_on": [],
    "action": "UI program replaced by REST API architecture - no direct migration needed",
    "definition_of_done": "Business logic extracted and identified for REST controller implementation",
    "validation_mechanism": "REST endpoints provide equivalent functionality",
    "estimated_hours": 0,
    ...
}
```

**Why include UI programs?**
- They're part of the total program count
- They serve as upstream dependencies (help understand business logic flow)
- REST API architecture replaces them
- Mark with `estimated_hours: 0` since no migration work needed

## Task Categories and Sequencing

### 1. Setup Tasks (First)
Infrastructure and baselines that everything depends on:
- Performance baseline measurement
- Monitoring/observability setup
- Development environment setup
- CI/CD pipeline configuration

### 2. Validator Tasks (Before Each Migration)
Create validation mechanisms BEFORE the code they will validate:
- Integration test suites
- Performance benchmarks
- Data comparison tools
- Coverage configuration
- Acceptance test scenarios

### 3. Migration Tasks (After Validators)
The actual migration work, which DEPENDS ON validators:
- Code translation
- API implementation
- Data migration
- Integration work

**IMPORTANT: Group Similar Programs**
- One migration task CAN handle multiple related programs
- Example: `migrate_005` covers CREACC, UPDACC, DELACC (3 programs)
- Example: `migrate_007` covers CRDTAGY2-5 (4 programs)
- Grouping criteria: similar functionality, shared service layer, similar complexity
- Target: 6-12 hours per task even when grouping multiple programs

## Dependency Rules

1. **Migration tasks MUST depend on their validators**: If `migrate_customer` needs tests, then `validator_customer_tests` must be a dependency
2. **Validators can depend on other validators**: Build incrementally (e.g., CRUD tests extend read-only tests)
3. **Setup tasks have no dependencies**: They establish the foundation
4. **No circular dependencies**: The graph must be a DAG

## Task Granularity & Grouping

- **Target: 6-12 hours** per task (even when handling multiple programs)
- **Maximum: 20 hours** (split if larger)
- **Minimum: 4 hours** (combine if smaller)

**When to Group Programs:**
- Similar functionality (e.g., all account CRUD operations)
- Shared service layer (e.g., all use CustomerService)
- Similar complexity (~300-500 lines each)
- Same data access patterns

**When to Split Programs:**
- Different complexity levels (e.g., CRECUST at 1440 lines vs INQCUST at 712 lines)
- Different validation approaches needed
- Different external dependencies (e.g., async vs sync operations)

**Example Groupings:**
```python
# Good: 3 similar programs, ~12 hours total
{
    "id": "migrate_005",
    "title": "Migrate Account CRUD Operations (CREACC, UPDACC, DELACC)",
    "content": "Implement account creation, update, and deletion services...",
    "estimated_hours": 12,
    ...
}

# Good: 4 identical dummy programs, ~8 hours total
{
    "id": "migrate_007",
    "title": "Migrate Credit Agency Programs (CRDTAGY2-5)",
    "content": "Consolidate 4 identical credit agency programs into single service...",
    "estimated_hours": 8,
    ...
}

# Bad: Too many unrelated programs
{
    "id": "migrate_all_customer_ops",  # ❌ WRONG
    "title": "Migrate All Customer Operations (INQCUST, CRECUST, UPDCUST, DELCUS, BROWCUST)",
    "estimated_hours": 40,  # Too large!
    ...
}
```

## Program Counting Example

Given 29 total COBOL programs:

**Step 1: Identify migrated programs (verify with implementations)**
- 5 programs fully migrated with tests

**Step 2: Identify UI programs (BMS/3270 terminal programs)**
- 9 UI programs (BNKMENU + BNK1xxx programs)
- These become completed nodes with `estimated_hours: 0`

**Step 3: Calculate remaining programs**
- 29 - 5 - 9 = 15 programs to migrate

**Step 4: Group similar programs into migration tasks**
- 15 programs → 8 migration tasks (by grouping similar ones)
- Task can handle 1-4 programs depending on similarity

**Step 5: Create validators for each migration task**
- 8 migration tasks → 8 validator tasks (usually 1:1)

**Final Task Count:**
```
Completed nodes: 14 (5 migrated + 9 UI)
Setup tasks: 2
Validator tasks: 8
Migration tasks: 8
─────────────────
Total: 32 tasks covering 29 programs
```

## Output Structure

Create the task graph as `migration_plan.py` at the root of `[TARGET_REPO]`:

```python
# migration_plan.py
migration_plan = {
    "tasks": [
        # Completed nodes (migrated programs)
        { "id": "completed_001", "status": "completed", ... },
        { "id": "completed_002", "status": "completed", ... },
        ...
        
        # Completed nodes (UI programs as dependencies)
        { "id": "completed_006", "status": "completed", "estimated_hours": 0, ... },
        { "id": "completed_007", "status": "completed", "estimated_hours": 0, ... },
        ...
        
        # Setup tasks
        { "id": "setup_001", "status": "pending", "depends_on": [], ... },
        { "id": "setup_002", "status": "pending", "depends_on": [], ... },
        
        # Validator tasks
        { "id": "validator_001", "status": "pending", "depends_on": ["setup_002"], ... },
        { "id": "validator_002", "status": "pending", "depends_on": ["validator_001"], ... },
        ...
        
        # Migration tasks (can group multiple programs)
        { "id": "migrate_001", "status": "pending", "depends_on": ["validator_001"], ... },
        { "id": "migrate_005", "status": "pending", "depends_on": ["validator_004"], 
          "title": "Migrate Account CRUD (CREACC, UPDACC, DELACC)", ... },
        ...
    ]
}
```

## Verification Steps

Before finalizing, verify:

1. **Program count reconciles:**
   ```
   Total Programs = Completed Migrated + UI Programs + Remaining
   29 = 5 + 9 + 15 ✓
   ```

2. **All programs accounted for:**
   - List all 29 programs from source repo
   - Mark each as: migrated, UI, or remaining
   - No program should be unaccounted for

3. **Task dependencies valid:**
   - All migration tasks depend on validator tasks
   - No circular dependencies
   - Setup tasks have no dependencies

4. **Grouping makes sense:**
   - Each migration task: 6-12 hours target
   - Similar programs grouped together
   - Complex programs get their own task

5. **Create validation script:**
   ```python
   # validate_plan.py
   # Check all migrations have validator dependencies
   # Verify program counts
   # Ensure no circular dependencies
   ```

## Common Mistakes to Avoid

❌ **Don't confuse tasks with programs**
- 32 tasks covering 29 programs is CORRECT if programs are grouped

❌ **Don't forget UI programs in completed nodes**
- They're part of the total program count
- Mark them as `status: "completed"` with `estimated_hours: 0`

❌ **Don't create 1:1 mapping of programs to tasks**
- Group similar programs to keep tasks at 6-12 hours
- CRDTAGY2-5 can be one task (all identical)

❌ **Don't count UI programs as "migrated"**
- Migrated = has Java implementation
- UI programs = marked as completed dependencies only

❌ **Don't skip grouping rationale**
- Explain WHY programs are grouped in the task content
- Make it clear one task handles multiple programs

## Summary

**Key Points:**
1. Completed nodes = Migrated programs + UI programs (as dependencies)
2. Tasks can group multiple similar programs (1 task → 2-4 programs)
3. Always verify: Total Programs = Migrated + UI + Remaining
4. UI programs get `status: "completed"` with `estimated_hours: 0`
5. Migration tasks depend on validator tasks (test-first approach)
6. Target 6-12 hours per task, grouping similar programs to achieve this

Generate a complete plan ensuring EVERY migration task has proper validation built FIRST, and EVERY program is accounted for in the task graph.
