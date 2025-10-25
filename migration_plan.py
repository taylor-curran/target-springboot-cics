migration_plan = {
    "metadata": {
        "source_repo": "taylor-curran/og-cics-cobol-app",
        "target_repo": "taylor-curran/target-springboot-cics",
        "total_programs": 29,
        "migrated_programs": 5,
        "remaining_programs": 24,
        "core_programs_to_migrate": 15,
        "bms_screen_programs": 9,
        "bms_migration_strategy": "BMS screen programs (BNK1TFN, BNK1CCS, BNK1CAC, BNK1UAC, BNK1CRA, BNK1DAC, BNK1CCA, BNK1DCS, BNKMENU) are UI-related CICS transactions that are being replaced by REST APIs in the Spring Boot architecture and do not require direct migration."
    },
    "tasks": [
        {
            "id": "setup_001",
            "title": "Establish Performance Baseline for Legacy COBOL Programs",
            "content": "Measure and document performance baselines for all 29 COBOL programs in the og-cics-cobol-app repository. This establishes quantitative metrics (P50/P95/P99 latencies) that migrated Java services must match or exceed. Without baselines, we cannot validate performance parity claims.",
            "depends_on": [],
            "action": "Run performance tests against all COBOL programs in the legacy CICS environment. Document P50, P95, and P99 latencies for each program along with typical request volumes and data sizes.",
            "definition_of_done": "Performance baseline document completed with specific latency measurements for all 29 COBOL programs, including migrated (5) and remaining (24) programs. Document includes methodology, test conditions, and raw data.",
            "validation_mechanism": "Baseline metrics report includes: (1) Latency measurements (P50/P95/P99) for each program, (2) Request volume data, (3) Test methodology documentation, (4) Raw performance data in JSON format for automated comparison",
            "estimated_hours": 10,
            "deliverables": ["docs/baseline_metrics.md", "data/performance_baseline.json"]
        },
        {
            "id": "setup_002",
            "title": "Setup Continuous Monitoring and Observability",
            "content": "Deploy monitoring infrastructure for the target Spring Boot application to track performance metrics, error rates, and system health in real-time. This enables continuous validation that migrations maintain performance and correctness.",
            "depends_on": [],
            "action": "Configure application monitoring with metrics collection (response times, throughput, error rates), logging aggregation, and alerting for the Spring Boot application. Integrate with existing application.properties and ensure metrics are exposed for all REST endpoints.",
            "definition_of_done": "Monitoring infrastructure operational with: (1) Metrics collection configured via Spring Actuator, (2) Dashboards created for each service domain (customer, account, transaction), (3) Alerts configured for performance degradation and errors, (4) Integration with existing logging framework",
            "validation_mechanism": "Verification includes: (1) Metrics endpoints accessible and returning data, (2) Dashboard displays live metrics for all controllers and services, (3) Alert test triggers notifications, (4) 24-hour baseline collected showing normal operations",
            "estimated_hours": 8,
            "deliverables": ["src/main/resources/application-monitoring.properties", "docs/monitoring_setup.md", "grafana/dashboards.json"]
        },
        {
            "id": "setup_003",
            "title": "Enhance CI/CD Pipeline with Coverage Enforcement",
            "content": "Strengthen the CI/CD pipeline to enforce JaCoCo coverage requirements and prevent regressions. Currently coverage is below minimums (Service 2%, overall 33%). Pipeline must fail builds that don't meet thresholds defined in TESTING.md.",
            "depends_on": [],
            "action": "Update Maven configuration and CI pipeline to enforce JaCoCo coverage minimums: Service 80% instructions/70% branches, Repository 70%, Controller 60%, Model 50%, DTO 40%, Overall 50%. Configure pipeline to fail on coverage drops.",
            "definition_of_done": "CI pipeline enforces all coverage thresholds. Test builds demonstrate: (1) Build passes when coverage meets requirements, (2) Build fails when coverage drops below thresholds, (3) Coverage reports generated and published for each build, (4) Documentation updated with CI requirements",
            "validation_mechanism": "Verification steps: (1) Trigger build with intentionally low coverage - build must fail, (2) Trigger build meeting requirements - build must pass, (3) Coverage reports published to build artifacts, (4) Pipeline configuration reviewed and documented",
            "estimated_hours": 8,
            "deliverables": ["pom.xml", ".github/workflows/maven.yml", "docs/ci_cd_setup.md"]
        },
        {
            "id": "validator_001",
            "title": "Create Comprehensive Customer CRUD Test Suite",
            "content": "Build extensive integration test suite for customer operations (CRECUST, INQCUST, UPDCUST, DELCUS) covering all CRUD operations, edge cases, and COBOL business rules. Tests must validate composite key handling (sort_code+customer_number), date validation rules (year >= 1601, age <= 150, no future dates), and credit scoring integration.",
            "depends_on": ["setup_002"],
            "action": "Create CustomerServiceTest.java with 40+ integration tests using H2 database. Cover: customer creation with credit scoring, inquiry operations, update scenarios, deletion with cascade checks, date validation (COBOL CEEDAYS rules), composite key operations, error handling, and concurrent operations.",
            "definition_of_done": "Test suite complete with: (1) 40+ integration tests covering all customer CRUD operations, (2) All COBOL date validation rules implemented and tested (min year 1601, max age 150, reject future dates with correct fail codes), (3) Tests pass against H2 test database, (4) JaCoCo configuration integrated, (5) Test fixtures and mock data generators created",
            "validation_mechanism": "Validation includes: (1) All 40+ tests pass successfully, (2) JaCoCo report shows 90%+ service layer coverage for customer operations, (3) Tests execute in under 30 seconds, (4) Code review confirms all COBOL business rules captured (date validation, composite keys, credit scoring flows), (5) Test data fixtures validated against schema.sql",
            "estimated_hours": 12,
            "deliverables": ["src/test/java/com/cbsa/migration/service/CustomerServiceTest.java", "src/test/java/com/cbsa/migration/fixtures/CustomerTestFixtures.java", "src/test/resources/test-data/customer-scenarios.json"]
        },
        {
            "id": "validator_002",
            "title": "Create Comprehensive Account CRUD Test Suite",
            "content": "Build extensive integration test suite for account operations (CREACC, INQACC, UPDACC, DELACC) covering all CRUD operations, account lifecycle, and Named Counter integration for account ID generation. Tests must validate relationships to customer records, balance calculations, and transaction logging.",
            "depends_on": ["setup_002"],
            "action": "Create AccountServiceTest.java with 35+ integration tests using H2 database. Cover: account creation with Named Counter ID generation, inquiry operations, update scenarios including balance changes, deletion with transaction checks, customer relationship validation, and concurrent account operations.",
            "definition_of_done": "Test suite complete with: (1) 35+ integration tests covering all account CRUD operations, (2) Named Counter integration tested for unique account ID generation, (3) Customer-account relationship validation, (4) Tests pass against H2 test database, (5) Repository layer tests for JDBC operations, (6) Balance calculation and constraint tests",
            "validation_mechanism": "Validation includes: (1) All 35+ tests pass successfully, (2) JaCoCo report shows 90%+ service layer coverage for account operations, (3) Repository tests achieve 70%+ coverage, (4) Named Counter ID generation verified (sequential, unique, no gaps), (5) Foreign key constraint tests validate customer relationships, (6) Tests execute in under 30 seconds",
            "estimated_hours": 12,
            "deliverables": ["src/test/java/com/cbsa/migration/service/AccountServiceTest.java", "src/test/java/com/cbsa/migration/repository/AccountRepositoryTest.java", "src/test/java/com/cbsa/migration/fixtures/AccountTestFixtures.java"]
        },
        {
            "id": "validator_003",
            "title": "Create Account Inquiry by Customer Test Suite",
            "content": "Build specialized test suite for INQACCCU (inquire accounts by customer) covering customer account listing, pagination, filtering, and empty result scenarios. This operation is commonly used by BMS screens to display customer account lists.",
            "depends_on": ["setup_002"],
            "action": "Create AccountInquiryServiceTest.java with 20+ tests covering: listing all accounts for a customer, sorting by account type and balance, pagination with various page sizes, filtering by account status, handling customers with zero accounts, handling customers with many accounts (100+), and performance with large result sets.",
            "definition_of_done": "Test suite complete with: (1) 20+ tests covering all inquiry scenarios, (2) Pagination logic validated with edge cases (empty pages, single page, many pages), (3) Sorting and filtering tests, (4) Performance tests with large datasets (100+ accounts per customer), (5) Tests pass against H2 database",
            "validation_mechanism": "Validation includes: (1) All 20+ tests pass successfully, (2) JaCoCo shows 90%+ coverage for inquiry service methods, (3) Performance tests demonstrate query execution under 100ms for 100+ accounts, (4) Pagination returns correct page counts and boundaries, (5) Empty result scenarios handled correctly without errors",
            "estimated_hours": 8,
            "deliverables": ["src/test/java/com/cbsa/migration/service/AccountInquiryServiceTest.java", "src/test/resources/test-data/account-inquiry-scenarios.json"]
        },
        {
            "id": "validator_004",
            "title": "Create Transaction Operations Test Suite",
            "content": "Build comprehensive test suite for transaction operations (XFRFUN transfer funds, DBCRFUN debit/credit) covering fund movements, transaction logging to PROCTRAN table, balance updates, transaction boundaries, rollback scenarios, and concurrent transaction handling. Critical for financial accuracy.",
            "depends_on": ["setup_002"],
            "action": "Create TransactionServiceTest.java with 30+ integration tests covering: debit operations, credit operations, transfer between accounts (same customer and different customers), insufficient funds scenarios, transaction logging to bank_transaction table, balance validation, concurrent transactions, rollback scenarios, and transaction boundary enforcement.",
            "definition_of_done": "Test suite complete with: (1) 30+ integration tests covering all transaction scenarios, (2) Transaction boundary tests validate ACID properties, (3) Concurrent transaction tests (10+ simultaneous operations), (4) Rollback scenarios tested and working correctly, (5) bank_transaction table logging validated, (6) Balance calculation accuracy tests (no rounding errors)",
            "validation_mechanism": "Validation includes: (1) All 30+ tests pass successfully, (2) JaCoCo shows 90%+ coverage for transaction service layer, (3) Transaction isolation tests demonstrate no race conditions, (4) Rollback tests show balances unchanged after failures, (5) Transaction log entries match all operations, (6) Financial accuracy verified (balances sum correctly across all accounts)",
            "estimated_hours": 12,
            "deliverables": ["src/test/java/com/cbsa/migration/service/TransactionServiceTest.java", "src/test/java/com/cbsa/migration/service/TransactionConcurrencyTest.java", "src/test/resources/test-data/transaction-scenarios.json"]
        },
        {
            "id": "validator_005",
            "title": "Create Credit Agency Services Test Suite",
            "content": "Build test suite for remaining credit agency implementations (CRDTAGY2, CRDTAGY3, CRDTAGY4, CRDTAGY5). These programs simulate external credit scoring with random delays (0-3 seconds) and random credit scores (1-999). Tests must validate async behavior, timeout handling, and score generation.",
            "depends_on": ["setup_002"],
            "action": "Create CreditAgencyServicesTest.java with 25+ tests covering: agency response simulation, random delay behavior (0-3 second range), random score generation (1-999 range), timeout scenarios (when agency doesn't respond in time), multiple agency calls (parallel credit checks), and error handling for agency failures.",
            "definition_of_done": "Test suite complete with: (1) 25+ tests covering all 4 credit agency services (CRDTAGY2-5), (2) Async operation tests with timeout validation, (3) Random score generation within bounds (1-999), (4) Delay simulation tests (0-3 seconds), (5) Mock/stub infrastructure for credit agency calls, (6) Tests validate 1-in-4 chance of timely response emulation",
            "validation_mechanism": "Validation includes: (1) All 25+ tests pass successfully, (2) JaCoCo shows 90%+ coverage for credit agency services, (3) Timeout tests demonstrate correct behavior when agencies exceed 3-second limit, (4) Score generation verified to stay within 1-999 bounds, (5) Async operation tests complete without deadlocks, (6) Tests execute in reasonable time (under 60 seconds total despite delays)",
            "estimated_hours": 10,
            "deliverables": ["src/test/java/com/cbsa/migration/service/CreditAgencyServicesTest.java", "src/test/java/com/cbsa/migration/service/CreditAgencyMockHelper.java"]
        },
        {
            "id": "validator_006",
            "title": "Create Retroactive CreditAgencyService Test Suite",
            "content": "CRITICAL: CreditAgencyService (migrated from CRDTAGY1) was implemented WITHOUT tests, violating the validator-before-migration principle. This task retroactively creates comprehensive tests for the existing service to establish baseline validation and prevent regressions.",
            "depends_on": ["setup_002"],
            "action": "Create CreditAgencyServiceTest.java with 30+ tests covering the existing CreditAgencyService implementation. Test: random delay generation (0-3 seconds), random credit score generation (1-999), container/channel operations, error handling, timeout scenarios, and async behavior. Analyze existing implementation to ensure full coverage.",
            "definition_of_done": "Test suite complete with: (1) 30+ tests covering all CreditAgencyService methods and behaviors, (2) Full coverage of existing implementation (delay logic, score generation, container operations), (3) Tests validate COBOL behavior is preserved (FUNCTION RANDOM, DELAY FOR SECONDS equivalents), (4) Tests pass against current implementation, (5) Edge cases covered (boundary values, error conditions)",
            "validation_mechanism": "Validation includes: (1) All 30+ tests pass successfully against existing CreditAgencyService, (2) JaCoCo report shows 90%+ coverage for CreditAgencyService (currently has 0% test coverage), (3) Code review confirms all methods have test coverage, (4) Regression tests capture current behavior before any modifications, (5) Tests serve as living documentation of COBOL-to-Java translation",
            "estimated_hours": 10,
            "deliverables": ["src/test/java/com/cbsa/migration/service/CreditAgencyServiceTest.java", "docs/credit_agency_service_coverage_report.md"]
        },
        {
            "id": "migrate_001",
            "title": "Migrate INQCUST - Customer Inquiry Operations",
            "content": "Migrate INQCUST COBOL program to Java, implementing customer inquiry/read operations. This is the foundation for all customer operations - read operations must work before write operations can be tested. Implements REST endpoint for retrieving customer details by sort_code and customer_number (composite key).",
            "depends_on": ["setup_001", "setup_003", "validator_001"],
            "action": "Implement CustomerInquiryService, CustomerInquiryController, and enhance CustomerRepository for inquiry operations. Map COBOL CUSTOMER VSAM read logic to JDBC queries using composite key (sort_code + customer_number). Implement REST GET endpoint. Use validator_001 tests to verify correctness.",
            "definition_of_done": "INQCUST migration complete with: (1) CustomerInquiryService implemented with composite key lookup, (2) CustomerInquiryController with GET /customers/{sortCode}/{customerNumber} endpoint, (3) All validator_001 inquiry tests passing, (4) JaCoCo shows 90%+ service coverage, (5) Manual testing confirms COBOL behavior preserved, (6) Documentation updated with API specification",
            "validation_mechanism": "Validation includes: (1) All validator_001 inquiry-related tests pass (100% pass rate), (2) JaCoCo coverage report shows 90%+ for CustomerInquiryService, (3) Integration tests demonstrate composite key queries work correctly, (4) Performance meets or exceeds baseline from setup_001, (5) REST endpoint tested via Postman/curl with sample data, (6) Error handling tests pass (customer not found scenarios)",
            "estimated_hours": 8,
            "deliverables": ["src/main/java/com/cbsa/migration/service/CustomerInquiryService.java", "src/main/java/com/cbsa/migration/controller/CustomerInquiryController.java", "docs/api/customer_inquiry_api.md"]
        },
        {
            "id": "migrate_002",
            "title": "Migrate CRECUST - Create Customer Operations",
            "content": "Migrate CRECUST COBOL program to Java, implementing customer creation with credit agency integration, Named Counter for customer number generation, and transaction logging. This is complex migration requiring async credit checks, date validation, and counter management.",
            "depends_on": ["migrate_001", "validator_001", "validator_005", "validator_006"],
            "action": "Implement CustomerCreationService with: Named Counter integration for customer_number generation, async credit agency calls (orchestrate CRDTAGY1-5), date-of-birth validation (COBOL rules: year >= 1601, age <= 150, reject future dates), transaction logging to bank_transaction table. Implement REST POST endpoint.",
            "definition_of_done": "CRECUST migration complete with: (1) CustomerCreationService with Named Counter integration, (2) Async credit agency orchestration (calls all 5 agencies), (3) Date validation implementing COBOL rules with correct fail codes ('O' for year/age, 'Y' for future dates), (4) CustomerCreationController POST endpoint, (5) All validator_001 creation tests passing, (6) Transaction logging working",
            "validation_mechanism": "Validation includes: (1) All validator_001 customer creation tests pass (100% pass rate), (2) JaCoCo shows 90%+ coverage for CustomerCreationService, (3) Named Counter generates sequential unique IDs without gaps, (4) Credit agency integration tests pass (all 5 agencies called correctly), (5) Date validation tests confirm COBOL rules preserved, (6) Performance meets baseline (considering async credit checks may timeout)",
            "estimated_hours": 12,
            "deliverables": ["src/main/java/com/cbsa/migration/service/CustomerCreationService.java", "src/main/java/com/cbsa/migration/controller/CustomerCreationController.java", "docs/api/customer_creation_api.md"]
        },
        {
            "id": "migrate_003",
            "title": "Migrate UPDCUST - Update Customer Operations",
            "content": "Migrate UPDCUST COBOL program to Java, implementing customer update operations. Must handle partial updates, preserve composite key immutability, validate date changes, and maintain audit trails. Credit score updates may trigger re-evaluation.",
            "depends_on": ["migrate_001", "migrate_002", "validator_001"],
            "action": "Implement CustomerUpdateService with: partial field updates (name, address, DOB), composite key immutability enforcement (sort_code+customer_number cannot change), date validation for DOB changes, credit score re-evaluation logic, optimistic locking for concurrent updates. Implement REST PUT/PATCH endpoints.",
            "definition_of_done": "UPDCUST migration complete with: (1) CustomerUpdateService with partial update support, (2) Composite key immutability enforced (attempts to change keys rejected), (3) Date validation on updates, (4) CustomerUpdateController PUT/PATCH endpoints, (5) All validator_001 update tests passing, (6) Optimistic locking prevents concurrent update issues, (7) Audit trail of changes captured",
            "validation_mechanism": "Validation includes: (1) All validator_001 update tests pass (100% pass rate), (2) JaCoCo shows 90%+ coverage for CustomerUpdateService, (3) Concurrent update tests demonstrate no data corruption, (4) Composite key immutability tests verify rejection of key changes, (5) Date validation tests pass on updates, (6) Performance meets baseline from setup_001",
            "estimated_hours": 10,
            "deliverables": ["src/main/java/com/cbsa/migration/service/CustomerUpdateService.java", "src/main/java/com/cbsa/migration/controller/CustomerUpdateController.java", "docs/api/customer_update_api.md"]
        },
        {
            "id": "migrate_004",
            "title": "Migrate DELCUS - Delete Customer Operations",
            "content": "Migrate DELCUS COBOL program to Java, implementing customer deletion with cascade checks. Must verify no dependent accounts exist before deletion, log deletion event, and handle soft vs hard delete scenarios. Critical for data integrity.",
            "depends_on": ["migrate_001", "migrate_002", "migrate_003", "validator_001"],
            "action": "Implement CustomerDeletionService with: cascade checks (verify customer has no accounts), soft delete option (mark inactive rather than physical delete), deletion audit logging to application_error table, transaction boundary enforcement. Implement REST DELETE endpoint with cascade validation.",
            "definition_of_done": "DELCUS migration complete with: (1) CustomerDeletionService with cascade checks, (2) Soft delete option implemented, (3) Deletion prevented if customer has accounts (foreign key constraint check), (4) CustomerDeletionController DELETE endpoint, (5) All validator_001 deletion tests passing, (6) Audit logging captures deletion events",
            "validation_mechanism": "Validation includes: (1) All validator_001 deletion tests pass (100% pass rate), (2) JaCoCo shows 90%+ coverage for CustomerDeletionService, (3) Cascade check tests verify deletion blocked when accounts exist, (4) Soft delete tests confirm data marked inactive but preserved, (5) Audit log tests verify deletion events captured, (6) Foreign key constraint tests validate referential integrity maintained",
            "estimated_hours": 8,
            "deliverables": ["src/main/java/com/cbsa/migration/service/CustomerDeletionService.java", "src/main/java/com/cbsa/migration/controller/CustomerDeletionController.java", "docs/api/customer_deletion_api.md"]
        },
        {
            "id": "migrate_005",
            "title": "Migrate INQACC - Account Inquiry Operations",
            "content": "Migrate INQACC COBOL program to Java, implementing account inquiry/read operations by account number. Foundation for all account operations. Must retrieve account details including customer reference, balance information, and account status.",
            "depends_on": ["migrate_001", "setup_001", "setup_003", "validator_002"],
            "action": "Implement AccountInquiryService and AccountInquiryController for single account lookup. Map COBOL ACCOUNT DB2 read logic to JDBC queries. Implement REST GET endpoint /accounts/{accountNumber}. Include customer reference validation (foreign key to customer table). Use validator_002 tests to verify correctness.",
            "definition_of_done": "INQACC migration complete with: (1) AccountInquiryService implemented with account number lookup, (2) Customer reference validation (account must belong to existing customer), (3) AccountInquiryController GET endpoint, (4) All validator_002 inquiry tests passing, (5) JaCoCo shows 90%+ service coverage, (6) Balance calculation logic validated",
            "validation_mechanism": "Validation includes: (1) All validator_002 inquiry-related tests pass (100% pass rate), (2) JaCoCo coverage report shows 90%+ for AccountInquiryService, (3) Integration tests validate foreign key relationships to customer table, (4) Performance meets baseline from setup_001, (5) REST endpoint tested with various account scenarios (active, inactive, different types), (6) Error handling tests pass (account not found scenarios)",
            "estimated_hours": 8,
            "deliverables": ["src/main/java/com/cbsa/migration/service/AccountInquiryService.java", "src/main/java/com/cbsa/migration/controller/AccountInquiryController.java", "docs/api/account_inquiry_api.md"]
        },
        {
            "id": "migrate_006",
            "title": "Migrate INQACCCU - Inquire Accounts by Customer",
            "content": "Migrate INQACCCU COBOL program to Java, implementing account listing by customer (sort_code + customer_number). Used by BMS screens to display all accounts for a customer. Must support pagination, sorting, and filtering for customers with many accounts.",
            "depends_on": ["migrate_001", "migrate_005", "validator_003"],
            "action": "Implement AccountListingService for customer account queries using composite customer key. Implement pagination (page size configurable, default 20), sorting by account number/type/balance, filtering by account status. Create REST GET endpoint /customers/{sortCode}/{customerNumber}/accounts with query parameters for pagination and filtering.",
            "definition_of_done": "INQACCCU migration complete with: (1) AccountListingService with composite key queries, (2) Pagination implemented with page metadata (total pages, current page, total records), (3) Sorting and filtering support, (4) AccountListingController GET endpoint with query params, (5) All validator_003 tests passing, (6) Performance optimized for large result sets",
            "validation_mechanism": "Validation includes: (1) All validator_003 tests pass (100% pass rate), (2) JaCoCo shows 90%+ coverage for AccountListingService, (3) Pagination tests verify correct page boundaries and counts, (4) Performance tests demonstrate sub-100ms query time for 100+ accounts, (5) Sorting tests validate correct ordering, (6) Empty result scenarios handled without errors, (7) REST endpoint tested with various pagination parameters",
            "estimated_hours": 8,
            "deliverables": ["src/main/java/com/cbsa/migration/service/AccountListingService.java", "src/main/java/com/cbsa/migration/controller/AccountListingController.java", "docs/api/account_listing_api.md"]
        },
        {
            "id": "migrate_007",
            "title": "Migrate CREACC - Create Account Operations",
            "content": "Migrate CREACC COBOL program to Java, implementing account creation with Named Counter for account ID generation. Must validate customer exists, enforce business rules (account types, minimum balance), and log creation to PROCTRAN. Complex migration with counter management.",
            "depends_on": ["migrate_001", "migrate_005", "validator_002"],
            "action": "Implement AccountCreationService with: Named Counter integration for account_number generation (unique, sequential), customer existence validation (foreign key), account type validation, initial balance validation, transaction logging to bank_transaction table. Implement REST POST endpoint /accounts.",
            "definition_of_done": "CREACC migration complete with: (1) AccountCreationService with Named Counter integration, (2) Customer existence validation (FK constraint), (3) Account type and balance validation rules, (4) Transaction logging working, (5) AccountCreationController POST endpoint, (6) All validator_002 creation tests passing, (7) Counter generates sequential IDs without gaps",
            "validation_mechanism": "Validation includes: (1) All validator_002 account creation tests pass (100% pass rate), (2) JaCoCo shows 90%+ coverage for AccountCreationService, (3) Named Counter generates unique sequential account numbers without gaps, (4) Customer FK validation tests verify accounts cannot be created for non-existent customers, (5) Business rule tests pass (valid account types, minimum balance), (6) Transaction log entries verified, (7) Performance meets baseline",
            "estimated_hours": 10,
            "deliverables": ["src/main/java/com/cbsa/migration/service/AccountCreationService.java", "src/main/java/com/cbsa/migration/controller/AccountCreationController.java", "docs/api/account_creation_api.md"]
        },
        {
            "id": "migrate_008",
            "title": "Migrate UPDACC - Update Account Operations",
            "content": "Migrate UPDACC COBOL program to Java, implementing account update operations. Must handle account type changes, status updates (active/inactive), balance adjustments (via transactions only, not direct updates), and maintain audit trails. Account number is immutable.",
            "depends_on": ["migrate_005", "migrate_007", "validator_002"],
            "action": "Implement AccountUpdateService with: partial field updates (type, status, metadata), account number immutability enforcement, balance update restrictions (only via transactions), optimistic locking for concurrent updates, audit logging. Implement REST PUT/PATCH endpoints /accounts/{accountNumber}.",
            "definition_of_done": "UPDACC migration complete with: (1) AccountUpdateService with partial update support, (2) Account number immutability enforced, (3) Balance update restrictions validated (direct updates rejected), (4) AccountUpdateController PUT/PATCH endpoints, (5) All validator_002 update tests passing, (6) Optimistic locking prevents concurrent issues, (7) Audit trail captures changes",
            "validation_mechanism": "Validation includes: (1) All validator_002 update tests pass (100% pass rate), (2) JaCoCo shows 90%+ coverage for AccountUpdateService, (3) Account number immutability tests verify rejection of number changes, (4) Balance restriction tests confirm direct balance updates rejected, (5) Concurrent update tests demonstrate no data corruption, (6) Audit log tests verify change tracking, (7) Performance meets baseline",
            "estimated_hours": 10,
            "deliverables": ["src/main/java/com/cbsa/migration/service/AccountUpdateService.java", "src/main/java/com/cbsa/migration/controller/AccountUpdateController.java", "docs/api/account_update_api.md"]
        },
        {
            "id": "migrate_009",
            "title": "Migrate DELACC - Delete Account Operations",
            "content": "Migrate DELACC COBOL program to Java, implementing account deletion with transaction checks. Must verify zero balance, check for pending transactions, log deletion event, and handle soft vs hard delete scenarios. Critical for financial data integrity.",
            "depends_on": ["migrate_005", "migrate_007", "migrate_008", "validator_002"],
            "action": "Implement AccountDeletionService with: zero balance validation (account must have 0.00 balance), pending transaction checks, soft delete option (mark inactive), deletion audit logging, transaction boundary enforcement. Implement REST DELETE endpoint /accounts/{accountNumber} with validation.",
            "definition_of_done": "DELACC migration complete with: (1) AccountDeletionService with balance and transaction checks, (2) Zero balance validation enforced, (3) Soft delete option implemented, (4) Deletion prevented if transactions exist or balance non-zero, (5) AccountDeletionController DELETE endpoint, (6) All validator_002 deletion tests passing, (7) Audit logging captures deletion events",
            "validation_mechanism": "Validation includes: (1) All validator_002 deletion tests pass (100% pass rate), (2) JaCoCo shows 90%+ coverage for AccountDeletionService, (3) Balance validation tests verify non-zero balance accounts cannot be deleted, (4) Transaction check tests verify accounts with transactions are protected, (5) Soft delete tests confirm data preserved but marked inactive, (6) Audit log tests verify deletion events captured, (7) Financial integrity tests validate no orphaned transactions",
            "estimated_hours": 8,
            "deliverables": ["src/main/java/com/cbsa/migration/service/AccountDeletionService.java", "src/main/java/com/cbsa/migration/controller/AccountDeletionController.java", "docs/api/account_deletion_api.md"]
        },
        {
            "id": "migrate_010",
            "title": "Migrate DBCRFUN - Debit/Credit Fund Operations",
            "content": "Migrate DBCRFUN COBOL program to Java, implementing debit and credit operations on accounts. Foundation for fund transfers. Must update balances atomically, log to PROCTRAN, enforce sufficient funds for debits, and maintain transaction boundaries. Critical for financial accuracy.",
            "depends_on": ["migrate_005", "migrate_007", "validator_004"],
            "action": "Implement TransactionService with debit and credit methods: atomic balance updates (within transaction), sufficient funds validation for debits, transaction logging to bank_transaction table, ACID transaction boundaries, concurrent operation handling. Implement REST POST endpoints /accounts/{accountNumber}/debit and /accounts/{accountNumber}/credit.",
            "definition_of_done": "DBCRFUN migration complete with: (1) TransactionService with debit/credit methods, (2) Atomic balance updates with transaction boundaries, (3) Sufficient funds validation (debits rejected if balance insufficient), (4) Transaction logging to bank_transaction table, (5) TransactionController POST endpoints, (6) All validator_004 debit/credit tests passing, (7) Concurrent operation tests pass without race conditions",
            "validation_mechanism": "Validation includes: (1) All validator_004 debit/credit tests pass (100% pass rate), (2) JaCoCo shows 90%+ coverage for TransactionService, (3) Transaction boundary tests validate ACID properties, (4) Concurrent operation tests (10+ simultaneous transactions) complete without data corruption, (5) Balance accuracy tests verify no rounding errors, (6) Transaction log entries match all operations exactly, (7) Rollback tests demonstrate balances unchanged after failures, (8) Performance meets baseline",
            "estimated_hours": 10,
            "deliverables": ["src/main/java/com/cbsa/migration/service/TransactionService.java", "src/main/java/com/cbsa/migration/controller/TransactionController.java", "docs/api/transaction_api.md"]
        },
        {
            "id": "migrate_011",
            "title": "Migrate XFRFUN - Transfer Fund Operations",
            "content": "Migrate XFRFUN COBOL program to Java, implementing fund transfers between accounts. Builds on DBCRFUN debit/credit operations. Must perform atomic debit from source and credit to destination within single transaction, validate sufficient funds, support same-customer and cross-customer transfers, and maintain audit trail.",
            "depends_on": ["migrate_010", "validator_004"],
            "action": "Implement fund transfer logic in TransactionService: atomic debit from source account and credit to destination account (single transaction boundary), sufficient funds validation, same-customer and cross-customer transfer support, transfer audit logging with source/destination tracking, rollback on any failure. Implement REST POST endpoint /transfers.",
            "definition_of_done": "XFRFUN migration complete with: (1) Transfer method in TransactionService with atomic debit+credit, (2) Single transaction boundary ensures both operations succeed or both fail, (3) Sufficient funds validation, (4) Same-customer and cross-customer transfers working, (5) Transfer audit trail with source/destination details, (6) TransferController POST endpoint, (7) All validator_004 transfer tests passing, (8) Rollback tests demonstrate atomic behavior",
            "validation_mechanism": "Validation includes: (1) All validator_004 transfer tests pass (100% pass rate), (2) JaCoCo shows 90%+ coverage for transfer logic, (3) Atomic transaction tests verify debit+credit both succeed or both fail (no partial transfers), (4) Concurrent transfer tests demonstrate no race conditions, (5) Balance sum tests verify total money in system unchanged (conservation of funds), (6) Rollback tests show balances unchanged when transfer fails, (7) Cross-customer transfer tests validate different sort codes, (8) Performance meets baseline",
            "estimated_hours": 10,
            "deliverables": ["src/main/java/com/cbsa/migration/service/TransactionService.java", "src/main/java/com/cbsa/migration/controller/TransferController.java", "docs/api/transfer_api.md"]
        },
        {
            "id": "migrate_012",
            "title": "Migrate Credit Agencies 2-5 (CRDTAGY2, CRDTAGY3, CRDTAGY4, CRDTAGY5)",
            "content": "Migrate remaining 4 credit agency COBOL programs to Java. All programs follow same pattern as CRDTAGY1 (already migrated): simulate external credit scoring with random delays (0-3 seconds) and random credit scores (1-999). These provide redundancy for credit scoring with 1-in-4 chance of timely response.",
            "depends_on": ["validator_005", "validator_006"],
            "action": "Implement CreditAgency2Service, CreditAgency3Service, CreditAgency4Service, and CreditAgency5Service following CreditAgencyService pattern. Each service: generates random delay (0-3 seconds), generates random credit score (1-999), returns score via container/channel equivalent. Implement async orchestration to call all agencies in parallel with 3-second timeout.",
            "definition_of_done": "Credit agencies migration complete with: (1) Four services implemented (CreditAgency2Service through CreditAgency5Service), (2) Each service follows same pattern as CreditAgencyService (CRDTAGY1), (3) Random delay generation (0-3 seconds), (4) Random score generation (1-999), (5) Async orchestration implemented to call all 5 agencies in parallel, (6) All validator_005 tests passing, (7) Integration with CustomerCreationService working",
            "validation_mechanism": "Validation includes: (1) All validator_005 tests pass for all 4 new services (100% pass rate), (2) JaCoCo shows 90%+ coverage for each service, (3) Delay simulation tests verify 0-3 second range, (4) Score generation tests verify 1-999 bounds, (5) Async orchestration tests demonstrate parallel calls with timeout, (6) Integration tests show CustomerCreationService correctly orchestrates all 5 agencies, (7) 1-in-4 timely response pattern validated, (8) Performance meets baseline (considering async timeouts)",
            "estimated_hours": 10,
            "deliverables": ["src/main/java/com/cbsa/migration/service/CreditAgency2Service.java", "src/main/java/com/cbsa/migration/service/CreditAgency3Service.java", "src/main/java/com/cbsa/migration/service/CreditAgency4Service.java", "src/main/java/com/cbsa/migration/service/CreditAgency5Service.java", "src/main/java/com/cbsa/migration/service/CreditAgencyOrchestrator.java", "docs/credit_agency_orchestration.md"]
        }
    ],
    "migration_summary": {
        "total_tasks": 21,
        "setup_tasks": 3,
        "validator_tasks": 6,
        "migration_tasks": 12,
        "estimated_total_hours": 204,
        "programs_migrated_by_phase": {
            "already_migrated": ["CRDTAGY1", "GETCOMPY", "GETSCODE", "ABNDPROC", "BANKDATA"],
            "customer_operations": ["INQCUST", "CRECUST", "UPDCUST", "DELCUS"],
            "account_operations": ["INQACC", "INQACCCU", "CREACC", "UPDACC", "DELACC"],
            "transaction_operations": ["DBCRFUN", "XFRFUN"],
            "credit_agencies": ["CRDTAGY2", "CRDTAGY3", "CRDTAGY4", "CRDTAGY5"],
            "not_requiring_migration": ["BNK1TFN", "BNK1CCS", "BNK1CAC", "BNK1UAC", "BNK1CRA", "BNK1DAC", "BNK1CCA", "BNK1DCS", "BNKMENU"]
        },
        "key_principles": [
            "Every migration task depends on validator tasks (tests built FIRST)",
            "Dependencies enforce correct execution order (customer -> account -> transaction)",
            "Measurable validation mechanisms (tests, coverage, metrics - not subjective reviews)",
            "Target 6-12 hours per task for optimal working sessions",
            "Retroactive tests for CreditAgencyService (migrated without tests)",
            "BMS screen programs replaced by REST APIs (no direct migration needed)",
            "Preserve COBOL business rules (date validation, composite keys, Named Counters, transaction logging)"
        ]
    }
}
