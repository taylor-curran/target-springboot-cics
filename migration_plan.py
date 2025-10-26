#

migration_plan = {
    "tasks": [
        {
            "id": "setup_001",
            "title": "Create Performance Baselines",
            "content": "Measure current COBOL program performance. Document P50/P95/P99 latencies. Establish success metrics.",
            "status": "not-complete",
            "estimated_hours": 8
        },
        {
            "id": "setup_002",
            "title": "Setup Local SQLite Database",
            "content": "Verify SQLite schema matches COBOL data structures. Load test data fixtures. Configure Spring datasource.",
            "status": "not-complete",
            "estimated_hours": 6
        },
        {
            "id": "setup_003",
            "title": "Configure Local Monitoring",
            "content": "Set up observability for local development. Configure logging and metrics collection. Establish monitoring dashboards.",
            "status": "not-complete",
            "estimated_hours": 8
        },
        {
            "id": "setup_004",
            "title": "Validate Test Data Generation",
            "content": "Verify BankDataGenerator creates realistic test data. Ensure 100 customers with accounts. Validate transaction history.",
            "status": "not-complete",
            "estimated_hours": 6
        },
        {
            "id": "migrate_001",
            "title": "Migrate Credit Agency Simulators",
            "content": "Port CRDTAGY2-5 programs to Spring Boot services. All four are identical to CRDTAGY1 pattern. Implement random delay and credit scoring.",
            "status": "not-complete",
            "estimated_hours": 8
        },
        {
            "id": "migrate_002",
            "title": "Migrate Customer Inquiry Operations",
            "content": "Port INQCUST program to REST endpoint. Map COBOL customer records to DTOs. Implement repository layer with JDBC.",
            "status": "not-complete",
            "estimated_hours": 10
        },
        {
            "id": "migrate_003",
            "title": "Migrate Customer Creation",
            "content": "Port CRECUST to Spring Boot with async credit checks. Implement named counter enqueue/dequeue. Handle multi-agency credit scoring aggregation.",
            "status": "not-complete",
            "estimated_hours": 12
        },
        {
            "id": "migrate_004",
            "title": "Migrate Customer Update and Delete",
            "content": "Port UPDCUST and DELCUS programs to REST endpoints. Implement validation and cascading account deletion. Handle PROCTRAN audit records.",
            "status": "not-complete",
            "estimated_hours": 10
        },
        {
            "id": "migrate_005",
            "title": "Migrate Account Inquiry Operations",
            "content": "Port INQACC program to REST endpoint. Map COBOL account records to DTOs. Implement account lookup by sort code and number.",
            "status": "not-complete",
            "estimated_hours": 10
        },
        {
            "id": "migrate_006",
            "title": "Migrate Account List for Customer",
            "content": "Port INQACCCU program to REST endpoint. Implement customer account browsing. Return paginated account lists with balances.",
            "status": "not-complete",
            "estimated_hours": 8
        },
        {
            "id": "migrate_007",
            "title": "Migrate Account Creation",
            "content": "Port CREACC to Spring Boot with named counters. Implement account number generation and enqueue/dequeue. Write PROCTRAN audit records.",
            "status": "not-complete",
            "estimated_hours": 12
        },
        {
            "id": "migrate_008",
            "title": "Migrate Account Update and Delete",
            "content": "Port UPDACC and DELACC programs to REST endpoints. Implement balance updates and account deletion. Handle PROCTRAN audit logging.",
            "status": "not-complete",
            "estimated_hours": 10
        },
        {
            "id": "migrate_009",
            "title": "Migrate Debit Credit Operations",
            "content": "Port DBCRFUN program to Spring Boot service. Implement cash deposit/withdrawal logic. Update account balances and write PROCTRAN records.",
            "status": "not-complete",
            "estimated_hours": 10
        },
        {
            "id": "migrate_010",
            "title": "Migrate Funds Transfer",
            "content": "Port XFRFUN program to Spring Boot with @Transactional. Implement account-to-account transfers with rollback. Ensure atomicity and audit logging.",
            "status": "not-complete",
            "estimated_hours": 12
        },
        {
            "id": "migrate_011",
            "title": "Migrate Batch Data Generation",
            "content": "Port BANKDATA batch program to Spring Batch job. Generate customer and account data from parameters. Support both SQLite and H2 databases.",
            "status": "not-complete",
            "estimated_hours": 10
        },
        {
            "id": "integrate_001",
            "title": "Customer Workflow Integration",
            "content": "Test end-to-end customer operations flow. Validate create, inquiry, update, delete sequence. Verify data consistency across services.",
            "status": "not-complete",
            "estimated_hours": 8
        },
        {
            "id": "integrate_002",
            "title": "Account Workflow Integration",
            "content": "Test end-to-end account operations flow. Validate create, inquiry, update, delete sequence. Ensure customer-account relationships maintained.",
            "status": "not-complete",
            "estimated_hours": 8
        },
        {
            "id": "integrate_003",
            "title": "Transaction Processing Validation",
            "content": "Test debit/credit and transfer operations. Validate transaction atomicity and PROCTRAN audit trail. Verify balance calculations and overdraft limits.",
            "status": "not-complete",
            "estimated_hours": 10
        }
    ]
}
