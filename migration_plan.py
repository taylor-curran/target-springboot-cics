"""
COBOL to Java Migration Task Graph
===================================

This file defines the complete migration plan for 29 COBOL programs from 
og-cics-cobol-app to target-springboot-cics using a test-first validation approach.

Migration Status:
- Total Programs: 29
- Already Migrated: 5 (GETCOMPY, GETSCODE, CRDTAGY1, ABNDPROC, BANKDATA)
- UI Programs (Upstream Dependencies): 10 (BNK1* programs)
- Business Logic Programs to Migrate: 14

Key Principles:
1. Test-First Validation: Every migration task depends on validator tasks created BEFORE
2. Grouping: Similar programs grouped into single migration tasks where logical
3. Dependencies: Validators → Migration → Integration Testing
4. Date Validation: Preserve COBOL rules (min year 1601, max age 150, reject future dates)
"""

from dataclasses import dataclass, field
from typing import List, Set, Optional
from enum import Enum


class TaskStatus(Enum):
    """Status of migration tasks"""
    NOT_STARTED = "not_started"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    BLOCKED = "blocked"


class TaskType(Enum):
    """Type of migration task"""
    SETUP = "setup"
    VALIDATOR = "validator"
    MIGRATION = "migration"
    TESTING = "testing"
    INTEGRATION = "integration"


@dataclass
class MigrationTask:
    """Represents a single task in the migration process"""
    task_id: str
    name: str
    description: str
    task_type: TaskType
    status: TaskStatus = TaskStatus.NOT_STARTED
    cobol_programs: List[str] = field(default_factory=list)
    dependencies: List[str] = field(default_factory=list)
    estimated_complexity: str = "medium"  # low, medium, high
    notes: List[str] = field(default_factory=list)
    
    def add_dependency(self, task_id: str):
        """Add a dependency to this task"""
        if task_id not in self.dependencies:
            self.dependencies.append(task_id)
    
    def mark_completed(self):
        """Mark task as completed"""
        self.status = TaskStatus.COMPLETED
    
    def can_start(self, completed_tasks: Set[str]) -> bool:
        """Check if all dependencies are completed"""
        return all(dep in completed_tasks for dep in self.dependencies)


class MigrationPlan:
    """Complete migration plan with all tasks and dependencies"""
    
    def __init__(self):
        self.tasks: dict[str, MigrationTask] = {}
        self._initialize_tasks()
    
    def add_task(self, task: MigrationTask):
        """Add a task to the plan"""
        self.tasks[task.task_id] = task
    
    def get_task(self, task_id: str) -> Optional[MigrationTask]:
        """Get a task by ID"""
        return self.tasks.get(task_id)
    
    def get_ready_tasks(self) -> List[MigrationTask]:
        """Get all tasks that are ready to start"""
        completed = {tid for tid, task in self.tasks.items() 
                    if task.status == TaskStatus.COMPLETED}
        return [task for task in self.tasks.values() 
                if task.status == TaskStatus.NOT_STARTED and task.can_start(completed)]
    
    def get_tasks_by_type(self, task_type: TaskType) -> List[MigrationTask]:
        """Get all tasks of a specific type"""
        return [task for task in self.tasks.values() if task.task_type == task_type]
    
    def get_tasks_by_status(self, status: TaskStatus) -> List[MigrationTask]:
        """Get all tasks with a specific status"""
        return [task for task in self.tasks.values() if task.status == status]
    
    def validate_dependencies(self) -> List[str]:
        """Validate that all task dependencies exist and detect cycles"""
        errors = []
        
        for task_id, task in self.tasks.items():
            for dep_id in task.dependencies:
                if dep_id not in self.tasks:
                    errors.append(f"Task {task_id} depends on non-existent task {dep_id}")
        
        visited = set()
        rec_stack = set()
        
        def has_cycle(task_id: str) -> bool:
            visited.add(task_id)
            rec_stack.add(task_id)
            
            task = self.tasks.get(task_id)
            if task:
                for dep_id in task.dependencies:
                    if dep_id not in visited:
                        if has_cycle(dep_id):
                            return True
                    elif dep_id in rec_stack:
                        return True
            
            rec_stack.remove(task_id)
            return False
        
        for task_id in self.tasks:
            if task_id not in visited:
                if has_cycle(task_id):
                    errors.append(f"Circular dependency detected involving task {task_id}")
        
        return errors
    
    def _initialize_tasks(self):
        """Initialize all migration tasks with proper dependencies"""
        
        
        completed_programs = [
            ("GETCOMPY", "CompanyInfoService", "Returns company information (24 lines)"),
            ("GETSCODE", "SortCodeService", "Returns bank sort code (24 lines)"),
            ("CRDTAGY1", "CreditAgencyService", "Credit agency processing with delays and scoring (188 lines)"),
            ("ABNDPROC", "ErrorLoggingService", "Error logging and abend processing (161 lines)"),
            ("BANKDATA", "DataLayer", "Bank data access patterns integrated into repository layer"),
        ]
        
        for i, (program, service, desc) in enumerate(completed_programs, 1):
            task = MigrationTask(
                task_id=f"migrated_{i}",
                name=f"{program} Already Migrated",
                description=f"✅ {program}.cbl → {service}: {desc}",
                task_type=TaskType.MIGRATION,
                status=TaskStatus.COMPLETED,
                cobol_programs=[program],
                estimated_complexity="low",
                notes=["Previously completed migration"]
            )
            self.add_task(task)
        
        
        ui_programs = [
            "BNK1TFN",   # Transfer Funds UI
            "BNK1CCS",   # Customer Search UI
            "BNK1CAC",   # Create Account UI
            "BNKMENU",   # Main Menu UI
            "BNK1UAC",   # Update Account UI
            "BNK1CRA",   # Create Customer UI
            "BNK1DAC",   # Delete Account UI
            "BNK1CCA",   # Create Customer Confirm UI
            "BNK1DCS",   # Delete Customer UI
            "BNK1XXX",   # Additional UI Program (placeholder for 10th)
        ]
        
        for i, program in enumerate(ui_programs, 1):
            task = MigrationTask(
                task_id=f"ui_upstream_{i}",
                name=f"{program} UI Program",
                description=f"🖥️ {program}.cbl - UI program (upstream dependency, not requiring migration)",
                task_type=TaskType.MIGRATION,
                status=TaskStatus.COMPLETED,
                cobol_programs=[program],
                estimated_complexity="low",
                notes=["UI program marked as completed upstream dependency"]
            )
            self.add_task(task)
        
        
        setup_1 = MigrationTask(
            task_id="setup_database",
            name="Database Schema Setup",
            description="Ensure database schema matches COBOL data structures",
            task_type=TaskType.SETUP,
            estimated_complexity="low",
            notes=[
                "Verify tables: customer, account, bank_transaction, control",
                "Ensure COBOL eye-catcher patterns preserved",
                "Validate foreign key relationships"
            ]
        )
        self.add_task(setup_1)
        
        setup_2 = MigrationTask(
            task_id="setup_test_framework",
            name="Test Framework Setup",
            description="Set up JUnit 5, Mockito, and test data generation",
            task_type=TaskType.SETUP,
            estimated_complexity="low",
            notes=[
                "Configure test database (H2)",
                "Set up test data generators",
                "Configure JaCoCo for coverage"
            ]
        )
        self.add_task(setup_2)
        
        setup_3 = MigrationTask(
            task_id="setup_validation_framework",
            name="Validation Framework Setup",
            description="Create reusable validation utilities for COBOL rules",
            task_type=TaskType.SETUP,
            dependencies=["setup_test_framework"],
            estimated_complexity="medium",
            notes=[
                "Date validation: min year 1601, max age 150 years, reject future dates",
                "Fail codes: 'O' for year/age violations, 'Y' for future dates",
                "Create DateValidationService with LocalDate checks"
            ]
        )
        self.add_task(setup_3)
        
        
        validator_crdtagy = MigrationTask(
            task_id="validator_crdtagy_group",
            name="Credit Agency Group Validator",
            description="Create validators for CRDTAGY2-5 migration",
            task_type=TaskType.VALIDATOR,
            dependencies=["setup_validation_framework"],
            cobol_programs=["CRDTAGY2", "CRDTAGY3", "CRDTAGY4", "CRDTAGY5"],
            estimated_complexity="medium",
            notes=[
                "Test CRDTAGY2: Secondary credit agency",
                "Test CRDTAGY3: Tertiary credit agency",
                "Test CRDTAGY4: Quaternary credit agency",
                "Test CRDTAGY5: Quinary credit agency",
                "Validate similar patterns to CRDTAGY1",
                "Test different delay and scoring algorithms"
            ]
        )
        self.add_task(validator_crdtagy)
        
        migration_crdtagy = MigrationTask(
            task_id="migration_crdtagy_group",
            name="Migrate Credit Agency Group (CRDTAGY2-5)",
            description="Migrate CRDTAGY2-5 to CreditAgency2-5Services",
            task_type=TaskType.MIGRATION,
            dependencies=["validator_crdtagy_group", "migrated_3"],  # Depends on CRDTAGY1
            cobol_programs=["CRDTAGY2", "CRDTAGY3", "CRDTAGY4", "CRDTAGY5"],
            estimated_complexity="high",
            notes=[
                "Follow CRDTAGY1 pattern established in CreditAgencyService",
                "Implement service classes for each agency (2-5)",
                "Preserve COBOL delay simulation and random scoring",
                "Update customer credit scores in database",
                "Add REST endpoints for each agency"
            ]
        )
        self.add_task(migration_crdtagy)
        
        
        validator_account_crud = MigrationTask(
            task_id="validator_account_crud",
            name="Account CRUD Validator",
            description="Create validators for account CRUD operations",
            task_type=TaskType.VALIDATOR,
            dependencies=["setup_validation_framework"],
            cobol_programs=["CREACC", "DELACC", "UPDACC", "INQACC"],
            estimated_complexity="high",
            notes=[
                "Test CREACC: Create account with validation",
                "Test DELACC: Delete account with cascade checks",
                "Test UPDACC: Update account with concurrency control",
                "Test INQACC: Inquire account with proper data retrieval",
                "Validate 12-field COBOL account structure",
                "Test eye-catcher patterns and logical delete flags",
                "Validate account number generation/assignment",
                "Test error handling for duplicate accounts, invalid customers"
            ]
        )
        self.add_task(validator_account_crud)
        
        migration_account_crud = MigrationTask(
            task_id="migration_account_crud",
            name="Migrate Account CRUD Operations",
            description="Migrate CREACC, DELACC, UPDACC, INQACC to AccountService",
            task_type=TaskType.MIGRATION,
            dependencies=["validator_account_crud", "setup_database"],
            cobol_programs=["CREACC", "DELACC", "UPDACC", "INQACC"],
            estimated_complexity="high",
            notes=[
                "Create AccountService with CRUD methods",
                "Implement AccountRepository with JDBC",
                "Preserve COBOL account structure (12 fields)",
                "Handle VSAM key patterns in SQLite",
                "Implement logical delete (don't physically remove records)",
                "Add proper error handling and validation",
                "Create REST endpoints for each operation",
                "Ensure composite key handling (sort_code + account_number)"
            ]
        )
        self.add_task(migration_account_crud)
        
        
        validator_customer_crud = MigrationTask(
            task_id="validator_customer_crud",
            name="Customer CRUD Validator",
            description="Create validators for customer CRUD operations",
            task_type=TaskType.VALIDATOR,
            dependencies=["setup_validation_framework"],
            cobol_programs=["CRECUST", "DELCUS", "UPDCUST", "INQCUST"],
            estimated_complexity="high",
            notes=[
                "Test CRECUST: Create customer with DOB validation",
                "Test DELCUS: Delete customer with account checks",
                "Test UPDCUST: Update customer with validation",
                "Test INQCUST: Inquire customer with proper data retrieval",
                "Validate date of birth: min year 1601, max age 150, no future dates",
                "Test fail codes: 'O' for year/age violations, 'Y' for future dates",
                "Validate customer number generation/assignment",
                "Test cascade delete prevention (customer with accounts)"
            ]
        )
        self.add_task(validator_customer_crud)
        
        migration_customer_crud = MigrationTask(
            task_id="migration_customer_crud",
            name="Migrate Customer CRUD Operations",
            description="Migrate CRECUST, DELCUS, UPDCUST, INQCUST to CustomerService",
            task_type=TaskType.MIGRATION,
            dependencies=["validator_customer_crud", "setup_database"],
            cobol_programs=["CRECUST", "DELCUS", "UPDCUST", "INQCUST"],
            estimated_complexity="high",
            notes=[
                "Create CustomerService with CRUD methods",
                "Implement CustomerRepository with JDBC",
                "Preserve COBOL customer structure from CUSTOMER.cpy",
                "Implement DATE-OF-BIRTH-CHECK validation logic",
                "Use LocalDate for date handling",
                "Validate: year >= 1601, age <= 150, date not in future",
                "Return proper fail codes ('O' or 'Y')",
                "Prevent deletion if customer has accounts",
                "Create REST endpoints for each operation",
                "Ensure composite key handling (sort_code + customer_number)"
            ]
        )
        self.add_task(migration_customer_crud)
        
        
        validator_xfrfun = MigrationTask(
            task_id="validator_xfrfun",
            name="Transfer Funds Validator",
            description="Create validators for funds transfer functionality",
            task_type=TaskType.VALIDATOR,
            dependencies=["setup_validation_framework", "validator_account_crud"],
            cobol_programs=["XFRFUN"],
            estimated_complexity="high",
            notes=[
                "Test transfer between accounts",
                "Validate sufficient balance checks",
                "Test transaction recording",
                "Validate debit/credit balance updates",
                "Test error handling for invalid accounts",
                "Test overdraft prevention",
                "Validate transaction atomicity (both debit and credit succeed or fail)",
                "Test concurrent transfer scenarios"
            ]
        )
        self.add_task(validator_xfrfun)
        
        migration_xfrfun = MigrationTask(
            task_id="migration_xfrfun",
            name="Migrate Transfer Funds (XFRFUN)",
            description="Migrate XFRFUN to FundsTransferService",
            task_type=TaskType.MIGRATION,
            dependencies=["validator_xfrfun", "migration_account_crud"],
            cobol_programs=["XFRFUN"],
            estimated_complexity="high",
            notes=[
                "Create FundsTransferService",
                "Implement transfer logic: debit source, credit destination",
                "Record transaction in bank_transaction table",
                "Use @Transactional for atomicity",
                "Validate sufficient balance before transfer",
                "Handle COBOL packed decimal arithmetic in Java",
                "Create REST endpoint POST /api/transfer",
                "Return transaction confirmation with new balances"
            ]
        )
        self.add_task(migration_xfrfun)
        
        
        validator_dbcrfun = MigrationTask(
            task_id="validator_dbcrfun",
            name="Debit/Credit Funds Validator",
            description="Create validators for debit/credit functionality",
            task_type=TaskType.VALIDATOR,
            dependencies=["setup_validation_framework", "validator_account_crud"],
            cobol_programs=["DBCRFUN"],
            estimated_complexity="medium",
            notes=[
                "Test debit operation with balance check",
                "Test credit operation",
                "Validate transaction recording",
                "Test balance updates",
                "Test error handling for insufficient funds",
                "Validate transaction types (debit vs credit)"
            ]
        )
        self.add_task(validator_dbcrfun)
        
        migration_dbcrfun = MigrationTask(
            task_id="migration_dbcrfun",
            name="Migrate Debit/Credit Funds (DBCRFUN)",
            description="Migrate DBCRFUN to DebitCreditService",
            task_type=TaskType.MIGRATION,
            dependencies=["validator_dbcrfun", "migration_account_crud"],
            cobol_programs=["DBCRFUN"],
            estimated_complexity="medium",
            notes=[
                "Create DebitCreditService",
                "Implement debit method with balance validation",
                "Implement credit method",
                "Record transactions in bank_transaction table",
                "Update account balances atomically",
                "Handle COBOL packed decimal arithmetic",
                "Create REST endpoints POST /api/debit and /api/credit",
                "Return updated balance after operation"
            ]
        )
        self.add_task(migration_dbcrfun)
        
        
        validator_inqacccu = MigrationTask(
            task_id="validator_inqacccu",
            name="Account-Customer Inquiry Validator",
            description="Create validators for account-customer inquiry",
            task_type=TaskType.VALIDATOR,
            dependencies=["setup_validation_framework", "validator_account_crud", "validator_customer_crud"],
            cobol_programs=["INQACCCU"],
            estimated_complexity="medium",
            notes=[
                "Test inquiry joining account and customer data",
                "Validate proper foreign key traversal",
                "Test error handling for non-existent accounts/customers",
                "Validate complete data retrieval",
                "Test various query scenarios"
            ]
        )
        self.add_task(validator_inqacccu)
        
        migration_inqacccu = MigrationTask(
            task_id="migration_inqacccu",
            name="Migrate Account-Customer Inquiry (INQACCCU)",
            description="Migrate INQACCCU to AccountCustomerService",
            task_type=TaskType.MIGRATION,
            dependencies=["validator_inqacccu", "migration_account_crud", "migration_customer_crud"],
            cobol_programs=["INQACCCU"],
            estimated_complexity="medium",
            notes=[
                "Create AccountCustomerService",
                "Implement inquiry joining account and customer tables",
                "Return combined DTO with account and customer details",
                "Handle foreign key relationships properly",
                "Create REST endpoint GET /api/account-customer/{sortCode}/{accountNumber}",
                "Optimize query performance with proper joins"
            ]
        )
        self.add_task(migration_inqacccu)
        
        
        integration_test_1 = MigrationTask(
            task_id="integration_test_basic",
            name="Basic Integration Testing",
            description="Test basic workflows across migrated services",
            task_type=TaskType.INTEGRATION,
            dependencies=[
                "migration_account_crud",
                "migration_customer_crud"
            ],
            estimated_complexity="medium",
            notes=[
                "Test end-to-end customer creation and account creation",
                "Test customer and account inquiry",
                "Test update and delete operations",
                "Validate data consistency across operations"
            ]
        )
        self.add_task(integration_test_1)
        
        integration_test_2 = MigrationTask(
            task_id="integration_test_transactions",
            name="Transaction Integration Testing",
            description="Test transaction workflows",
            task_type=TaskType.INTEGRATION,
            dependencies=[
                "migration_xfrfun",
                "migration_dbcrfun",
                "migration_account_crud"
            ],
            estimated_complexity="high",
            notes=[
                "Test complex transfer scenarios",
                "Test debit/credit operations with balance tracking",
                "Test concurrent transaction handling",
                "Validate transaction atomicity and consistency",
                "Test error recovery scenarios"
            ]
        )
        self.add_task(integration_test_2)
        
        integration_test_3 = MigrationTask(
            task_id="integration_test_credit_agencies",
            name="Credit Agency Integration Testing",
            description="Test credit agency processing workflows",
            task_type=TaskType.INTEGRATION,
            dependencies=[
                "migration_crdtagy_group",
                "migration_customer_crud"
            ],
            estimated_complexity="medium",
            notes=[
                "Test all credit agencies (CRDTAGY1-5)",
                "Test credit score updates and review dates",
                "Test concurrent credit checks",
                "Validate proper delay simulation",
                "Test error handling for invalid customers"
            ]
        )
        self.add_task(integration_test_3)
        
        integration_test_final = MigrationTask(
            task_id="integration_test_complete",
            name="Complete System Integration Testing",
            description="Final end-to-end testing of all migrated components",
            task_type=TaskType.INTEGRATION,
            dependencies=[
                "integration_test_basic",
                "integration_test_transactions",
                "integration_test_credit_agencies",
                "migration_inqacccu"
            ],
            estimated_complexity="high",
            notes=[
                "Test complete business workflows",
                "Test all REST endpoints",
                "Validate API documentation (Swagger)",
                "Performance testing with realistic data volumes",
                "Test error handling and recovery",
                "Validate data integrity constraints",
                "Test concurrent user scenarios",
                "Final validation against COBOL behavior"
            ]
        )
        self.add_task(integration_test_final)


def create_migration_plan() -> MigrationPlan:
    """Factory function to create the migration plan"""
    return MigrationPlan()


def print_plan_summary(plan: MigrationPlan):
    """Print a summary of the migration plan"""
    print("=" * 80)
    print("COBOL TO JAVA MIGRATION PLAN - TASK SUMMARY")
    print("=" * 80)
    print()
    
    status_counts = {}
    for status in TaskStatus:
        count = len(plan.get_tasks_by_status(status))
        status_counts[status.value] = count
    
    print(f"Total Tasks: {len(plan.tasks)}")
    for status, count in status_counts.items():
        print(f"  {status}: {count}")
    print()
    
    print("Tasks by Type:")
    for task_type in TaskType:
        tasks = plan.get_tasks_by_type(task_type)
        print(f"  {task_type.value}: {len(tasks)}")
    print()
    
    print("Already Migrated Programs (5):")
    for task in plan.tasks.values():
        if task.status == TaskStatus.COMPLETED and task.cobol_programs and "UI" not in task.name:
            if not task.task_id.startswith("ui_upstream"):
                for program in task.cobol_programs:
                    if program != "BNK1XXX":  # Skip placeholder
                        print(f"  ✅ {program}")
    print()
    
    print("Business Logic Programs to Migrate (14):")
    business_programs = set()
    for task in plan.tasks.values():
        if task.task_type == TaskType.MIGRATION and task.status == TaskStatus.NOT_STARTED:
            business_programs.update(task.cobol_programs)
    
    for program in sorted(business_programs):
        print(f"  🔄 {program}")
    print()
    
    errors = plan.validate_dependencies()
    if errors:
        print("❌ Dependency Validation Errors:")
        for error in errors:
            print(f"  - {error}")
    else:
        print("✅ All dependencies validated successfully")
    print()
    print("=" * 80)


if __name__ == "__main__":
    plan = create_migration_plan()
    
    print_plan_summary(plan)
    
    print("\nTasks Ready to Start:")
    ready_tasks = plan.get_ready_tasks()
    for task in ready_tasks[:5]:  # Show first 5
        print(f"  - {task.task_id}: {task.name}")
    
    if len(ready_tasks) > 5:
        print(f"  ... and {len(ready_tasks) - 5} more")
