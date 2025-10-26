
migration_plan = {
    "tasks": [
        {
            "id": "setup_001",
            "title": "Create Performance Baselines",
            "content": "Measure current COBOL program performance for key operations. Document P50/P95/P99 latencies for customer, account, and transaction operations. Establish success criteria for migration validation.",
            "status": "not-complete",
            "depends_on": [],
            "estimated_hours": 8
        },
        {
            "id": "setup_002",
            "title": "Setup Local SQLite Database",
            "content": "Create SQLite schema matching COBOL data structures. Load test data fixtures for customers, accounts, and transactions. Configure Spring datasource for local development.",
            "status": "not-complete",
            "depends_on": [],
            "estimated_hours": 6
        },
        {
            "id": "setup_003",
            "title": "Configure Local Monitoring",
            "content": "Set up local logging and metrics collection. Configure Spring Boot Actuator for health checks. Implement basic request/response logging for debugging.",
            "status": "not-complete",
            "depends_on": [],
            "estimated_hours": 6
        },
        
        {
            "id": "migrate_001",
            "title": "Migrate Customer Read Operations",
            "content": "Port INQCUST to Spring Boot REST endpoint for customer retrieval. Map COBOL records to DTOs and implement repository with composite key support.",
            "status": "not-complete",
            "depends_on": ["setup_002"],
            "estimated_hours": 8
        },
        {
            "id": "migrate_002",
            "title": "Migrate Customer Create Operations",
            "content": "Port CRECUST to Spring Boot customer creation endpoint with async credit agency integration. Implement Named Counter with distributed locks for customer numbers. Handle DB2/VSAM writes and PROCTRAN audit logging.",
            "status": "not-complete",
            "depends_on": ["migrate_001"],
            "estimated_hours": 12
        },
        {
            "id": "migrate_003",
            "title": "Migrate Customer Update Delete",
            "content": "Port UPDCUST and DELCUS to update/delete endpoints. UPDCUST modifies limited fields without PROCTRAN. DELCUS cascades to delete all customer accounts then customer record with PROCTRAN logging.",
            "status": "not-complete",
            "depends_on": ["migrate_001", "migrate_002"],
            "estimated_hours": 10
        },
        {
            "id": "migrate_004",
            "title": "Migrate Account Read Operations",
            "content": "Port INQACC and INQACCCU to AccountService endpoints. INQACC queries by account number. INQACCCU queries all accounts for customer using cursor/pagination.",
            "status": "not-complete",
            "depends_on": ["setup_002"],
            "estimated_hours": 8
        },
        {
            "id": "migrate_005",
            "title": "Migrate Account Create Operations",
            "content": "Port CREACC to Spring Boot account creation endpoint. Implement Named Counter with enqueue/dequeue for account numbers. Handle DB2 writes, PROCTRAN logging, and rollback on failure.",
            "status": "not-complete",
            "depends_on": ["migrate_002", "migrate_004"],
            "estimated_hours": 10
        },
        {
            "id": "migrate_006",
            "title": "Migrate Account Update Delete",
            "content": "Port UPDACC and DELACC to update/delete endpoints. UPDACC modifies type, interest rate, overdraft, statement dates. DELACC deletes account with PROCTRAN logging.",
            "status": "not-complete",
            "depends_on": ["migrate_004", "migrate_005"],
            "estimated_hours": 9
        },
        {
            "id": "migrate_007",
            "title": "Migrate Transfer Funds Operations",
            "content": "Port XFRFUN to Spring Boot transfer funds endpoint. Implement atomic debit/credit operations across both accounts. Handle transaction rollback on failure and PROCTRAN audit logging.",
            "status": "not-complete",
            "depends_on": ["migrate_004", "migrate_005"],
            "estimated_hours": 12
        },
        {
            "id": "migrate_008",
            "title": "Migrate Debit Credit Operations",
            "content": "Port DBCRFUN to TransactionService for cash deposits/withdrawals. Update account balances (available and actual). Log transactions to PROCTRAN with proper transaction types.",
            "status": "not-complete",
            "depends_on": ["migrate_004", "migrate_005"],
            "estimated_hours": 8
        },
        
        {
            "id": "integrate_001",
            "title": "Integrate Customer Workflows",
            "content": "Test end-to-end customer lifecycle flows. Validate create, read, update, delete operations. Verify credit agency integration and PROCTRAN logging consistency.",
            "status": "not-complete",
            "depends_on": ["migrate_001", "migrate_002", "migrate_003"],
            "estimated_hours": 8
        },
        {
            "id": "integrate_002",
            "title": "Integrate Account Workflows",
            "content": "Test end-to-end account lifecycle flows. Validate account creation with counter management. Verify cascade delete when customer deleted and PROCTRAN audit trail.",
            "status": "not-complete",
            "depends_on": ["migrate_004", "migrate_005", "migrate_006"],
            "estimated_hours": 8
        },
        {
            "id": "integrate_003",
            "title": "Integrate Transaction Workflows",
            "content": "Test end-to-end transaction processing flows. Validate transfer funds, debit, and credit operations. Verify balance updates, rollback handling, and PROCTRAN consistency.",
            "status": "not-complete",
            "depends_on": ["migrate_007", "migrate_008"],
            "estimated_hours": 10
        }
    ],
    
    "summary": {
        "total_cobol_programs": 29,
        "completed_programs": 14,
        "completed_breakdown": {
            "ui_layer_programs": 9,  # BNK1* BMS screens replaced by REST APIs
            "migrated_utilities": 5   # CRDTAGY1-5 (1 service), GETCOMPY, GETSCODE, ABNDPROC, BANKDATA
        },
        "remaining_to_migrate": 11,  # Business logic programs
        "total_tasks": 14,
        "setup_tasks": 3,
        "migration_tasks": 8,
        "integration_tasks": 3,
        "estimated_total_hours": 117  # Sum of all task estimates
    },
    
    "completed_programs": [
        "BNKMENU",   # Main menu screen
        "BNK1TFN",   # Transfer funds screen
        "BNK1CCS",   # Customer screen
        "BNK1CCA",   # List customer accounts screen
        "BNK1CAC",   # Create account screen
        "BNK1UAC",   # Update account screen
        "BNK1CRA",   # Create account alternate screen
        "BNK1DCS",   # Delete customer screen
        "BNK1DAC",   # Delete account screen
        
        "CRDTAGY1",  # Credit agency 1 -> CreditAgencyService
        "CRDTAGY2",  # Credit agency 2 -> CreditAgencyService
        "CRDTAGY3",  # Credit agency 3 -> CreditAgencyService
        "CRDTAGY4",  # Credit agency 4 -> CreditAgencyService
        "CRDTAGY5",  # Credit agency 5 -> CreditAgencyService
        "GETCOMPY",  # Get company info -> CompanyInfoService
        "GETSCODE",  # Get sort code -> SortCodeService
        "ABNDPROC",  # Error processing -> ErrorLoggingService
        "BANKDATA"   # Data generator -> BankDataGenerator
    ],
    
    "programs_to_migrate": [
        {"program": "CRECUST", "task": "migrate_002", "lines": 1440, "complexity": "high"},
        {"program": "INQCUST", "task": "migrate_001", "lines": 712, "complexity": "medium"},
        {"program": "UPDCUST", "task": "migrate_003", "lines": 365, "complexity": "low"},
        {"program": "DELCUS",  "task": "migrate_003", "lines": 762, "complexity": "medium"},
        
        {"program": "CREACC",  "task": "migrate_005", "lines": 1248, "complexity": "high"},
        {"program": "INQACC",  "task": "migrate_004", "lines": 1003, "complexity": "medium"},
        {"program": "INQACCCU","task": "migrate_004", "lines": 883, "complexity": "medium"},
        {"program": "UPDACC",  "task": "migrate_006", "lines": 407, "complexity": "low"},
        {"program": "DELACC",  "task": "migrate_006", "lines": 650, "complexity": "medium"},
        
        {"program": "XFRFUN",  "task": "migrate_007", "lines": 1925, "complexity": "high"},
        {"program": "DBCRFUN", "task": "migrate_008", "lines": 862, "complexity": "medium"}
    ],
    
    "architecture_notes": {
        "ui_layer_strategy": "All 9 BNK1* BMS 3270 screen programs are replaced by REST APIs in Spring Boot. No direct migration needed.",
        "credit_agency_consolidation": "All 5 CRDTAGY programs are identical dummy credit agencies, consolidated into single CreditAgencyService.",
        "grouping_rationale": {
            "migrate_003": "UPDCUST (365 lines) + DELCUS (762 lines) grouped as complementary customer write operations",
            "migrate_004": "INQACC (1003 lines) + INQACCCU (883 lines) grouped as account read operations with similar cursor patterns",
            "migrate_006": "UPDACC (407 lines) + DELACC (650 lines) grouped as complementary account write operations"
        },
        "standalone_tasks": {
            "migrate_002": "CRECUST alone due to complexity (1440 lines, async credit checks, Named Counter)",
            "migrate_005": "CREACC alone due to complexity (1248 lines, Named Counter, account number generation)",
            "migrate_007": "XFRFUN alone due to high complexity (1925 lines, dual-account updates, rollback handling)"
        },
        "data_access_patterns": {
            "vsam": "Customer records use VSAM with Named Counters for customer number generation",
            "db2": "Account and Transaction records use DB2 with cursor-based pagination",
            "proctran": "Audit trail for all create/update/delete operations except UPDCUST"
        },
        "transaction_boundaries": {
            "single_entity": "CRECUST, CREACC, INQCUST, INQACC - operate on single entity",
            "cascade": "DELCUS deletes customer then all associated accounts",
            "dual_entity": "XFRFUN updates two accounts (debit source, credit target) atomically"
        }
    }
}
