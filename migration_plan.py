migration_plan = {
    "tasks": [
        {
            "id": "setup_001",
            "title": "Establish Performance Baseline for Legacy COBOL System",
            "content": "Measure and document performance baselines for all 24 remaining COBOL programs in taylor-curran/og-cics-cobol-app. This baseline is critical for validating that migrated Java services maintain equivalent or better performance characteristics.",
            "depends_on": [],
            "prompt": """Analyze the COBOL programs in taylor-curran/og-cics-cobol-app and establish performance baselines.

COBOL Programs to Measure (24 total):
- UI Programs: BNKMENU, BNK1CAC, BNK1CCA, BNK1CCS, BNK1CRA, BNK1DAC, BNK1DCS, BNK1TFN, BNK1UAC
- Backend Logic: CREACC, CRECUST, INQACC, INQACCCU, INQCUST, UPDACC, UPDCUST, DELACC, DELCUS, DBCRFUN, XFRFUN
- Credit Agencies: CRDTAGY2, CRDTAGY3, CRDTAGY4, CRDTAGY5

For each program, measure:
1. P50/P95/P99 latency (milliseconds)
2. Throughput (transactions per second)
3. Error rates (percentage)
4. Transaction volumes (typical daily/hourly loads)
5. Resource utilization (CPU, memory, I/O patterns)

Create baseline_metrics.md with:
- Table of all programs with specific measurements
- Statistical confidence intervals
- Peak vs. average load scenarios
- Critical performance thresholds to maintain

Also create performance_data.json with raw metrics for programmatic validation.""",
            "definition_of_done": "baseline_metrics.md document covers all 24 COBOL programs with specific P50/P95/P99 latencies, throughput numbers, and error rates. performance_data.json contains structured data for all measurements.",
            "validation_mechanism": "Metrics report includes specific numbers (not ranges) for all 24 programs. At least 3 measurement runs per program. Statistical confidence >= 95%. JSON validates against schema.",
            "estimated_hours": 12,
            "deliverables": ["baseline_metrics.md", "performance_data.json", "measurement_methodology.md"]
        },
        {
            "id": "setup_002",
            "title": "Setup Monitoring and Observability Infrastructure",
            "content": "Deploy comprehensive monitoring stack for the target Spring Boot application in taylor-curran/target-springboot-cics. This infrastructure is required for all validator and migration tasks to measure coverage, performance, and correctness.",
            "depends_on": [],
            "prompt": """Setup monitoring infrastructure for taylor-curran/target-springboot-cics Spring Boot application.

Configure:
1. JaCoCo Coverage Reporting
   - Verify pom.xml has JaCoCo plugin configured
   - Set coverage thresholds: Service 80%, Repository 70%, Controller 60%, Model 50%, DTO 40%
   - Configure aggregate reports
   - Setup coverage badges/reports

2. Application Monitoring
   - Configure Spring Boot Actuator endpoints
   - Setup health checks for database, services
   - Configure metrics endpoints (Micrometer)
   - Enable request/response logging

3. Test Execution Tracking
   - Configure test result reporting
   - Setup test coverage dashboards
   - Enable parallel test execution monitoring

4. Database Monitoring
   - Track query performance
   - Monitor connection pool usage
   - Log slow queries (>100ms)

Create monitoring_config.md documenting:
- How to access coverage reports
- How to run coverage checks locally
- CI/CD integration points
- Alerting thresholds

Verify by running: mvn clean verify
Ensure coverage reports generate at target/site/jacoco/index.html""",
            "definition_of_done": "JaCoCo coverage reports generate successfully with configured thresholds. Monitoring endpoints are accessible. Test execution tracking is operational. monitoring_config.md provides clear setup instructions.",
            "validation_mechanism": "Run 'mvn clean verify' successfully. Coverage report exists at target/site/jacoco/index.html. Actuator health endpoint returns 200. All monitoring dashboards accessible.",
            "estimated_hours": 8,
            "deliverables": ["monitoring_config.md", "pom.xml updates (if needed)", "application.properties updates"]
        },
        {
            "id": "validator_001",
            "title": "Create Customer Read Operations Test Suite",
            "content": "Build comprehensive integration test suite for customer inquiry operations BEFORE migrating INQCUST.cbl. Tests will validate the migrated Java service against COBOL behavior.",
            "depends_on": ["setup_002"],
            "prompt": """Create integration tests for customer read operations in taylor-curran/target-springboot-cics.

Reference COBOL program: taylor-curran/og-cics-cobol-app/src/base/cobol_src/INQCUST.cbl

Create CustomerReadServiceTest.java with 30+ test cases:

1. Basic Read Operations (10 tests)
   - Read customer by customer_number
   - Read customer by sort_code + customer_number composite key
   - Read with valid eye-catcher (CUST)
   - Read non-existent customer (should return empty)
   - Read with invalid eye-catcher
   - Read with null parameters
   - Read with empty string parameters
   - Read with boundary values (max length fields)
   - Read with special characters in fields
   - Read multiple customers sequentially

2. Date Validation Tests (8 tests)
   - Valid date of birth (within range)
   - Date with min year 1601 (COBOL CEEDAYS limitation)
   - Date with year < 1601 (should fail with code 'O')
   - Date with age > 150 years (should fail with code 'O')
   - Date with future date (should fail with code 'Y')
   - Date with today (should succeed)
   - Date with invalid format
   - Date with leap year edge cases

3. Field Validation Tests (7 tests)
   - Customer name field length validation
   - Customer address field validation
   - Review date handling (YYYYMMDD format)
   - Credit score range validation (1-999)
   - Sort code format validation (6 digits)
   - Customer number format validation (10 chars)
   - Eye-catcher field validation

4. Database Integration Tests (5 tests)
   - Read from H2 in-memory database
   - Query with JdbcTemplate
   - Handle database connection errors
   - Handle query timeouts
   - Verify SQL query structure matches COBOL DB2 queries

Setup:
- Use @JdbcTest annotation
- Load test-schema.sql with @Sql
- Create CustomerReadTestFixtures.java for test data
- Configure JaCoCo for CustomerReadService
- Use assertJ for fluent assertions

Expected Coverage: 85%+ instruction coverage for read operations""",
            "definition_of_done": "CustomerReadServiceTest.java exists with 30+ passing test cases. Tests cover basic reads, date validation, field validation, and database integration. JaCoCo shows 85%+ coverage for customer read operations.",
            "validation_mechanism": "Run 'mvn test -Dtest=CustomerReadServiceTest'. All 30+ tests pass. JaCoCo report shows 85%+ instruction coverage. Test execution completes in <10 seconds.",
            "estimated_hours": 10,
            "deliverables": ["CustomerReadServiceTest.java", "CustomerReadTestFixtures.java", "test-data-customers.sql"]
        },
        {
            "id": "validator_002",
            "title": "Create Customer CRUD Operations Test Suite",
            "content": "Build comprehensive test suite for customer create/update/delete operations BEFORE migrating CRECUST, UPDCUST, DELCUS. Extends validator_001 with transactional CRUD tests.",
            "depends_on": ["validator_001"],
            "prompt": """Create integration tests for customer CRUD operations in taylor-curran/target-springboot-cics.

Reference COBOL programs:
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/CRECUST.cbl (Create)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/UPDCUST.cbl (Update)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/DELCUS.cbl (Delete)

Create CustomerCrudServiceTest.java with 50+ test cases:

1. Create Customer Tests (15 tests)
   - Create valid customer with all fields
   - Create customer with minimum required fields
   - Create customer triggers credit agency check
   - Create customer with generated customer_number from counter
   - Create with duplicate customer_number (should fail)
   - Create with invalid sort_code
   - Create with invalid date of birth
   - Create sets review_date to current date + 90 days
   - Create customer with credit score 0 initially
   - Create customer updates control counter
   - Create rolls back on credit check failure
   - Create validates all required fields
   - Create handles database constraint violations
   - Create with concurrent requests (race conditions)
   - Create logs to application_error on failure

2. Update Customer Tests (15 tests)
   - Update customer name
   - Update customer address
   - Update customer DOB (with validation)
   - Update credit score after review
   - Update review date
   - Update with non-existent customer (should fail)
   - Update with invalid customer_number
   - Update with invalid field values
   - Update preserves eye-catcher field
   - Update preserves sort_code and customer_number
   - Update handles optimistic locking
   - Update rolls back on validation failure
   - Update logs successful changes
   - Update with partial data
   - Update multiple fields atomically

3. Delete Customer Tests (10 tests)
   - Delete existing customer (logical delete)
   - Delete non-existent customer (should return gracefully)
   - Delete customer with existing accounts (should fail - FK constraint)
   - Delete validates customer_number format
   - Delete preserves audit trail
   - Delete is reversible (soft delete)
   - Delete handles concurrent deletion attempts
   - Delete rolls back on constraint violation
   - Delete logs to application_error on failure
   - Delete with invalid parameters

4. Transaction Management Tests (10 tests)
   - Create commits on success
   - Create rolls back on failure
   - Update commits changes atomically
   - Update rolls back on validation error
   - Delete within transaction
   - Multiple operations in single transaction
   - Transaction timeout handling
   - Nested transaction handling
   - Deadlock detection and retry
   - Transaction isolation level validation

Setup:
- Use @Transactional for rollback after each test
- Mock CreditAgencyService for create tests
- Mock ControlRepository for counter management
- Create comprehensive test fixtures
- Test against H2 in-memory database
- Configure JaCoCo for CRUD service coverage

Expected Coverage: 90%+ instruction coverage, 80%+ branch coverage""",
            "definition_of_done": "CustomerCrudServiceTest.java exists with 50+ passing tests covering create, update, delete, and transaction management. All tests use @Transactional. Credit agency and counter mocking works correctly. JaCoCo shows 90%+ instruction coverage.",
            "validation_mechanism": "Run 'mvn test -Dtest=CustomerCrudServiceTest'. All 50+ tests pass. JaCoCo report shows 90%+ instruction coverage, 80%+ branch coverage. Test execution completes in <20 seconds. No transaction leaks detected.",
            "estimated_hours": 12,
            "deliverables": ["CustomerCrudServiceTest.java", "CustomerCrudTestFixtures.java", "TransactionTestConfig.java"]
        },
        {
            "id": "validator_003",
            "title": "Create Account Read Operations Test Suite",
            "content": "Build comprehensive test suite for account inquiry operations BEFORE migrating INQACC and INQACCCU. Tests validate account queries match COBOL behavior.",
            "depends_on": ["setup_002"],
            "prompt": """Create integration tests for account read operations in taylor-curran/target-springboot-cics.

Reference COBOL programs:
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/INQACC.cbl (Inquire Account)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/INQACCCU.cbl (Inquire Account by Customer)

Create AccountReadServiceTest.java with 30+ test cases:

1. Basic Account Read Tests (10 tests)
   - Read account by sort_code + account_number
   - Read account by sort_code + customer_number
   - Read with valid eye-catcher (ACCT)
   - Read non-existent account (return empty)
   - Read with invalid eye-catcher
   - Read multiple accounts by customer_number
   - Read with null parameters
   - Read with boundary values
   - Read deleted accounts (should not return)
   - Read with special characters

2. Account Type and Status Tests (8 tests)
   - Read checking account
   - Read savings account
   - Read loan account
   - Read mortgage account
   - Read ISA account
   - Filter by account_type
   - Read accounts by opened_date range
   - Read accounts by last_statement_date

3. Balance and Financial Tests (7 tests)
   - Read actual_balance accurately
   - Read available_balance calculation
   - Read overdraft_limit
   - Read interest_rate with precision
   - Read accounts with negative balance
   - Read accounts at credit limit
   - Balance calculations match COBOL precision

4. Foreign Key Relationship Tests (5 tests)
   - Read account joins to customer correctly
   - Validate sort_code matches between account and customer
   - Read accounts for customer with multiple accounts
   - Read respects customer-account FK relationship
   - Read handles orphaned accounts gracefully

Setup:
- Use @JdbcTest with H2 database
- Load schema.sql with customer and account tables
- Create test fixtures with various account types
- Use JdbcTemplate for queries
- Configure JaCoCo for account read service

Expected Coverage: 85%+ instruction coverage""",
            "definition_of_done": "AccountReadServiceTest.java exists with 30+ passing tests. Tests cover basic reads, account types, balances, and foreign key relationships. JaCoCo shows 85%+ coverage for account read operations.",
            "validation_mechanism": "Run 'mvn test -Dtest=AccountReadServiceTest'. All 30+ tests pass. JaCoCo report shows 85%+ instruction coverage. Test execution completes in <10 seconds. All balance calculations match expected precision.",
            "estimated_hours": 10,
            "deliverables": ["AccountReadServiceTest.java", "AccountReadTestFixtures.java", "test-data-accounts.sql"]
        },
        {
            "id": "validator_004",
            "title": "Create Account CRUD Operations Test Suite",
            "content": "Build comprehensive test suite for account create/update/delete operations BEFORE migrating CREACC, UPDACC, DELACC. Includes counter management and FK validation tests.",
            "depends_on": ["validator_003"],
            "prompt": """Create integration tests for account CRUD operations in taylor-curran/target-springboot-cics.

Reference COBOL programs:
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/CREACC.cbl (Create)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/UPDACC.cbl (Update)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/DELACC.cbl (Delete)

Create AccountCrudServiceTest.java with 50+ test cases:

1. Create Account Tests (15 tests)
   - Create account with all required fields
   - Create account generates account_number from counter
   - Create validates customer exists (FK constraint)
   - Create with valid account_type (checking/savings/loan/mortgage/ISA)
   - Create sets opened_date to current date
   - Create initializes actual_balance to 0
   - Create sets available_balance = actual_balance + overdraft_limit
   - Create with overdraft_limit for checking accounts
   - Create with interest_rate for savings accounts
   - Create writes to PROCTRAN (transaction log)
   - Create updates control counter atomically
   - Create rolls back on FK violation
   - Create validates sort_code matches customer
   - Create with duplicate account_number (should fail)
   - Create logs errors on failure

2. Update Account Tests (15 tests)
   - Update overdraft_limit
   - Update interest_rate
   - Update last_statement_date
   - Update available_balance after transaction
   - Update with non-existent account (should fail)
   - Update validates FK integrity
   - Update preserves eye-catcher
   - Update preserves composite key fields
   - Update actual_balance triggers available_balance recalc
   - Update rolls back on validation failure
   - Update logs successful changes
   - Update handles concurrent updates
   - Update with partial data
   - Update multiple fields atomically
   - Update validates business rules (e.g., balance constraints)

3. Delete Account Tests (10 tests)
   - Delete existing account (logical delete)
   - Delete non-existent account (graceful return)
   - Delete account with transactions (should check business rules)
   - Delete validates account_number format
   - Delete preserves audit trail
   - Delete is reversible (soft delete)
   - Delete handles concurrent attempts
   - Delete rolls back on constraint violation
   - Delete logs to application_error
   - Delete with invalid parameters

4. Transaction and Counter Tests (10 tests)
   - Create commits on success
   - Create rolls back on failure
   - Counter increment is atomic
   - Counter handles concurrent requests
   - Counter never reuses account_numbers
   - Update commits atomically
   - Delete within transaction
   - Multiple account operations in transaction
   - Transaction timeout handling
   - Deadlock detection and recovery

Setup:
- Use @Transactional for test isolation
- Create customers in @BeforeEach for FK references
- Mock ControlRepository for counter management
- Test against H2 with full schema
- Create comprehensive fixtures for all account types
- Configure JaCoCo for account CRUD coverage

Expected Coverage: 90%+ instruction coverage, 80%+ branch coverage""",
            "definition_of_done": "AccountCrudServiceTest.java exists with 50+ passing tests covering create, update, delete, and transactions. Tests validate FK constraints, counter management, and business rules. JaCoCo shows 90%+ instruction coverage.",
            "validation_mechanism": "Run 'mvn test -Dtest=AccountCrudServiceTest'. All 50+ tests pass. JaCoCo report shows 90%+ instruction, 80%+ branch coverage. Test execution <20 seconds. FK constraints enforced. Counter operations atomic.",
            "estimated_hours": 12,
            "deliverables": ["AccountCrudServiceTest.java", "AccountCrudTestFixtures.java", "AccountTypeTestData.java"]
        },
        {
            "id": "validator_005",
            "title": "Create Transaction Operations Test Suite",
            "content": "Build comprehensive test suite for debit/credit and transfer operations BEFORE migrating DBCRFUN and XFRFUN. Tests validate balance updates and PROCTRAN logging.",
            "depends_on": ["validator_004"],
            "prompt": """Create integration tests for transaction operations in taylor-curran/target-springboot-cics.

Reference COBOL programs:
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/DBCRFUN.cbl (Debit/Credit)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/XFRFUN.cbl (Transfer)

Create TransactionServiceTest.java with 40+ test cases:

1. Debit/Credit Operation Tests (15 tests)
   - Credit account (deposit) updates actual_balance
   - Debit account (withdrawal) updates actual_balance
   - Credit updates available_balance correctly
   - Debit updates available_balance correctly
   - Debit validates sufficient funds
   - Debit allows overdraft within overdraft_limit
   - Debit rejects if exceeds available_balance
   - Credit/debit writes to PROCTRAN table
   - Transaction description stored correctly
   - Transaction timestamp in ISO format (YYYY-MM-DD HH:MM:SS.SSS)
   - Credit/debit validates account exists
   - Credit/debit handles account not found gracefully
   - Credit/debit validates amount > 0
   - Credit/debit with boundary amounts (max/min)
   - Credit/debit logs to application_error on failure

2. Transfer Operation Tests (15 tests)
   - Transfer between accounts (same customer)
   - Transfer between accounts (different customers)
   - Transfer validates source account exists
   - Transfer validates target account exists
   - Transfer validates sufficient funds in source
   - Transfer debits source account
   - Transfer credits target account
   - Transfer is atomic (both succeed or both fail)
   - Transfer writes 2 PROCTRAN records (debit + credit)
   - Transfer description includes both accounts
   - Transfer validates amount > 0
   - Transfer rejects if source = target
   - Transfer handles concurrent transfers
   - Transfer rolls back on any failure
   - Transfer with maximum amount

3. Balance Calculation Tests (5 tests)
   - Available balance = actual_balance + overdraft_limit
   - Negative actual_balance handled correctly
   - Overdraft_limit applied only to checking accounts
   - Balance precision matches COBOL (2 decimal places)
   - Balance updates are immediate and consistent

4. PROCTRAN Logging Tests (5 tests)
   - Every transaction creates PROCTRAN record
   - PROCTRAN includes all required fields
   - PROCTRAN timestamp matches transaction time
   - PROCTRAN description is descriptive
   - PROCTRAN records are never deleted (audit trail)

Setup:
- Use @Transactional for test isolation
- Create accounts with various balances in @BeforeEach
- Use H2 in-memory database
- Test both success and failure scenarios
- Verify PROCTRAN table after each transaction
- Configure JaCoCo for transaction service coverage

Expected Coverage: 85%+ instruction coverage, 75%+ branch coverage""",
            "definition_of_done": "TransactionServiceTest.java exists with 40+ passing tests covering debit, credit, transfer, balance calculations, and PROCTRAN logging. Tests validate atomicity and rollback. JaCoCo shows 85%+ coverage.",
            "validation_mechanism": "Run 'mvn test -Dtest=TransactionServiceTest'. All 40+ tests pass. JaCoCo shows 85%+ instruction, 75%+ branch coverage. Test execution <15 seconds. All transactions atomic. PROCTRAN records created correctly.",
            "estimated_hours": 10,
            "deliverables": ["TransactionServiceTest.java", "TransactionTestFixtures.java", "ProctranValidationUtils.java"]
        },
        {
            "id": "validator_006",
            "title": "Create Multi-Agency Credit Check Test Suite",
            "content": "Build test suite for multiple credit agency integrations BEFORE migrating CRDTAGY2-5. Tests validate parallel credit checks, aggregation, and timeout handling.",
            "depends_on": ["setup_002"],
            "prompt": """Create integration tests for multi-agency credit check operations in taylor-curran/target-springboot-cics.

Reference COBOL programs:
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/CRDTAGY2.cbl
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/CRDTAGY3.cbl
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/CRDTAGY4.cbl
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/CRDTAGY5.cbl

Build on existing CreditAgencyService (CRDTAGY1) in taylor-curran/target-springboot-cics.

Create MultiAgencyCreditServiceTest.java with 25+ test cases:

1. Single Agency Tests (5 tests)
   - Agency 1 returns credit score 1-999
   - Agency includes processing delay 0-3 seconds
   - Agency validates customer data
   - Agency updates customer record
   - Agency handles customer not found

2. Multi-Agency Parallel Tests (8 tests)
   - Query all 5 agencies in parallel
   - Aggregate scores from multiple agencies
   - Calculate average score across agencies
   - Handle partial responses (some agencies timeout)
   - All agencies complete within 3 second timeout
   - Fastest agency response used if others timeout
   - Parallel execution faster than sequential
   - Thread pool size configurable

3. Timeout and Failure Handling (7 tests)
   - Single agency timeout (3 seconds)
   - Multiple agency timeouts
   - All agencies timeout (fallback behavior)
   - Agency returns error response
   - Network failure simulation
   - Retry logic for transient failures
   - Circuit breaker for repeated failures

4. Score Aggregation Tests (5 tests)
   - Average score calculation correct
   - Weighted average by agency
   - Median score calculation
   - Exclude outlier scores
   - Handle agencies returning same score

Setup:
- Extend existing CreditAgencyServiceTest
- Use @Async for parallel credit checks
- Mock HTTP calls or use WireMock
- Configure thread pool for parallel execution
- Test timeout handling with CompletableFuture
- Configure JaCoCo for credit service coverage

Expected Coverage: 80%+ instruction coverage""",
            "definition_of_done": "MultiAgencyCreditServiceTest.java exists with 25+ passing tests. Tests validate parallel execution, aggregation, timeouts, and failure handling. JaCoCo shows 80%+ coverage for credit agency operations.",
            "validation_mechanism": "Run 'mvn test -Dtest=MultiAgencyCreditServiceTest'. All 25+ tests pass. JaCoCo shows 80%+ instruction coverage. Parallel tests complete faster than sequential. Timeout tests complete within expected time.",
            "estimated_hours": 8,
            "deliverables": ["MultiAgencyCreditServiceTest.java", "CreditAgencyMockUtils.java", "AsyncTestConfig.java"]
        },
        {
            "id": "migrate_001",
            "title": "Migrate Customer Inquiry Operations (INQCUST)",
            "content": "Migrate INQCUST.cbl to InquireCustomerService.java with full test coverage from validator_001. Match COBOL behavior exactly for data retrieval.",
            "depends_on": ["setup_001", "setup_002", "validator_001"],
            "prompt": """Migrate customer inquiry operations from taylor-curran/og-cics-cobol-app to taylor-curran/target-springboot-cics.

Source: taylor-curran/og-cics-cobol-app/src/base/cobol_src/INQCUST.cbl
Tests: Use validator_001 tests (CustomerReadServiceTest.java)

Create the following files:

1. InquireCustomerService.java
   - Implement inquireByCustomerNumber(sortCode, customerNumber)
   - Implement inquireByCustomerNumberAndSortCode(sortCode, customerNumber)
   - Use CustomerRepository for database queries
   - Validate eye-catcher field is "CUST"
   - Return Optional<Customer> for not found cases
   - Log all inquiry operations
   - Preserve date validation rules from COBOL:
     * Min year 1601 (CEEDAYS limitation)
     * Max age 150 years (return code 'O')
     * Reject future dates (return code 'Y')

2. InquireCustomerController.java
   - GET /api/customers/{customerNumber}
   - GET /api/customers/{sortCode}/{customerNumber}
   - Return 404 if customer not found
   - Return 200 with customer data if found
   - Include proper error handling
   - Use CustomerInquiryRequest/Response DTOs

3. CustomerInquiryRequest.java (DTO)
   - sortCode (String, 6 digits)
   - customerNumber (String, 10 chars)
   - Validation annotations

4. CustomerInquiryResponse.java (DTO)
   - Map Customer entity to response
   - Format dates as ISO 8601
   - Include all customer fields
   - Success/failure indicators

Implementation Requirements:
- Run validator_001 tests continuously during development
- Match COBOL DB2 query structure
- Use constructor injection for CustomerRepository
- Add @Service annotation
- Add @RestController for controller
- Use @GetMapping for endpoints
- Include comprehensive logging
- Handle null/empty parameters gracefully
- Match COBOL error codes and messages

Verification:
- All CustomerReadServiceTest tests pass
- JaCoCo shows 85%+ instruction coverage
- Integration tests pass with H2 database
- No regression in existing tests

Reference existing migration patterns:
- taylor-curran/target-springboot-cics/src/main/java/com/cbsa/migration/service/CreditAgencyService.java
- taylor-curran/target-springboot-cics/src/main/java/com/cbsa/migration/service/ErrorLoggingService.java""",
            "definition_of_done": "InquireCustomerService.java, InquireCustomerController.java, and DTOs created. All validator_001 tests pass. JaCoCo shows 85%+ service coverage. Customer inquiry endpoints work correctly.",
            "validation_mechanism": "Run 'mvn test -Dtest=CustomerReadServiceTest' - all tests pass. Run 'mvn verify' - JaCoCo shows 85%+ coverage. Integration tests pass. Manual test via REST endpoints returns correct data.",
            "estimated_hours": 10,
            "deliverables": ["InquireCustomerService.java", "InquireCustomerController.java", "CustomerInquiryRequest.java", "CustomerInquiryResponse.java"]
        },
        {
            "id": "migrate_002",
            "title": "Migrate Customer CRUD Operations (CRECUST, UPDCUST, DELCUS)",
            "content": "Migrate CRECUST, UPDCUST, DELCUS to CreateCustomerService, UpdateCustomerService, DeleteCustomerService with full test coverage from validator_002.",
            "depends_on": ["migrate_001", "validator_002"],
            "prompt": """Migrate customer CRUD operations from taylor-curran/og-cics-cobol-app to taylor-curran/target-springboot-cics.

Sources:
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/CRECUST.cbl (Create)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/UPDCUST.cbl (Update)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/DELCUS.cbl (Delete)

Tests: Use validator_002 tests (CustomerCrudServiceTest.java)

Create the following files:

1. CreateCustomerService.java
   - Implement createCustomer(CreateCustomerRequest)
   - Generate customer_number from ControlRepository counter
   - Validate all required fields (name, address, DOB, sort_code)
   - Apply date validation rules (min 1601, max age 150, no future)
   - Set eye-catcher to "CUST"
   - Call CreditAgencyService for initial credit check
   - Set review_date to current date + 90 days
   - Initialize credit_score to 0 (updated by credit agency)
   - Use @Transactional for atomicity
   - Roll back on any failure

2. UpdateCustomerService.java
   - Implement updateCustomer(customerNumber, UpdateCustomerRequest)
   - Validate customer exists
   - Update allowed fields (name, address, DOB, credit_score, review_date)
   - Preserve immutable fields (sort_code, customer_number, eye-catcher)
   - Apply date validation rules
   - Use @Transactional
   - Log all updates

3. DeleteCustomerService.java
   - Implement deleteCustomer(sortCode, customerNumber)
   - Logical delete (set deleted flag)
   - Validate customer exists
   - Check for FK constraints (no accounts)
   - Use @Transactional
   - Preserve audit trail
   - Log deletion

4. CustomerCrudController.java
   - POST /api/customers (create)
   - PUT /api/customers/{customerNumber} (update)
   - DELETE /api/customers/{sortCode}/{customerNumber} (delete)
   - Proper HTTP status codes (201, 200, 204, 404, 409)
   - Error handling with appropriate responses

5. DTOs
   - CreateCustomerRequest.java
   - UpdateCustomerRequest.java
   - CustomerResponse.java
   - Validation annotations (@NotNull, @Size, etc.)

Implementation Requirements:
- Run validator_002 tests continuously
- Counter management matches COBOL named counter logic
- Credit agency integration with 3-second timeout
- All operations are transactional
- FK constraint validation
- Comprehensive error handling
- Detailed logging

Verification:
- All CustomerCrudServiceTest tests pass (50+ tests)
- JaCoCo shows 90%+ instruction coverage
- Counter increments atomically
- Credit agency timeout works
- Rollback on failure works""",
            "definition_of_done": "CreateCustomerService, UpdateCustomerService, DeleteCustomerService, and CustomerCrudController created. All validator_002 tests pass. JaCoCo shows 90%+ coverage. CRUD operations work correctly with transactions.",
            "validation_mechanism": "Run 'mvn test -Dtest=CustomerCrudServiceTest' - all 50+ tests pass. Run 'mvn verify' - JaCoCo shows 90%+ instruction, 80%+ branch coverage. Integration tests pass. REST API CRUD operations verified manually.",
            "estimated_hours": 12,
            "deliverables": ["CreateCustomerService.java", "UpdateCustomerService.java", "DeleteCustomerService.java", "CustomerCrudController.java", "CreateCustomerRequest.java", "UpdateCustomerRequest.java"]
        },
        {
            "id": "migrate_003",
            "title": "Migrate Account Inquiry Operations (INQACC, INQACCCU)",
            "content": "Migrate INQACC and INQACCCU to InquireAccountService with full test coverage from validator_003. Implement account queries by account number and by customer.",
            "depends_on": ["migrate_001", "validator_003"],
            "prompt": """Migrate account inquiry operations from taylor-curran/og-cics-cobol-app to taylor-curran/target-springboot-cics.

Sources:
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/INQACC.cbl (Inquire by account)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/INQACCCU.cbl (Inquire by customer)

Tests: Use validator_003 tests (AccountReadServiceTest.java)

Create the following files:

1. InquireAccountService.java
   - Implement inquireByAccountNumber(sortCode, accountNumber)
   - Implement inquireByCustomerNumber(sortCode, customerNumber)
   - Implement inquireByAccountType(sortCode, customerNumber, accountType)
   - Use AccountRepository for database queries
   - Validate eye-catcher field is "ACCT"
   - Return Optional<Account> or List<Account>
   - Calculate available_balance = actual_balance + overdraft_limit
   - Filter out deleted accounts
   - Log all inquiry operations

2. InquireAccountController.java
   - GET /api/accounts/{accountNumber}
   - GET /api/accounts/customer/{customerNumber}
   - GET /api/accounts/customer/{customerNumber}/type/{accountType}
   - Return 404 if account not found
   - Return 200 with account data or list
   - Proper error handling

3. AccountInquiryRequest.java (DTO)
   - sortCode (String, 6 digits)
   - accountNumber (String, 8 chars) - optional
   - customerNumber (String, 10 chars) - optional
   - accountType (String) - optional
   - Validation annotations

4. AccountInquiryResponse.java (DTO)
   - Map Account entity to response
   - Include calculated available_balance
   - Format dates as ISO 8601
   - Include all account fields
   - Show associated customer info (join)

Implementation Requirements:
- Run validator_003 tests continuously
- Match COBOL DB2 join queries for customer data
- Handle FK relationships correctly
- Preserve balance precision (2 decimal places)
- Support filtering by account type
- Use constructor injection
- Comprehensive logging
- Handle not found gracefully

Verification:
- All AccountReadServiceTest tests pass (30+ tests)
- JaCoCo shows 85%+ instruction coverage
- Join queries work correctly
- Balance calculations accurate
- Integration tests pass""",
            "definition_of_done": "InquireAccountService.java, InquireAccountController.java, and DTOs created. All validator_003 tests pass. JaCoCo shows 85%+ coverage. Account inquiry endpoints work correctly.",
            "validation_mechanism": "Run 'mvn test -Dtest=AccountReadServiceTest' - all 30+ tests pass. Run 'mvn verify' - JaCoCo shows 85%+ coverage. Integration tests pass. REST endpoints return correct account data with proper joins.",
            "estimated_hours": 10,
            "deliverables": ["InquireAccountService.java", "InquireAccountController.java", "AccountInquiryRequest.java", "AccountInquiryResponse.java"]
        },
        {
            "id": "migrate_004",
            "title": "Migrate Account CRUD Operations (CREACC, UPDACC, DELACC)",
            "content": "Migrate CREACC, UPDACC, DELACC to account CRUD services with full test coverage from validator_004. Includes counter management and FK validation.",
            "depends_on": ["migrate_003", "validator_004"],
            "prompt": """Migrate account CRUD operations from taylor-curran/og-cics-cobol-app to taylor-curran/target-springboot-cics.

Sources:
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/CREACC.cbl (Create)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/UPDACC.cbl (Update)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/DELACC.cbl (Delete)

Tests: Use validator_004 tests (AccountCrudServiceTest.java)

Create the following files:

1. CreateAccountService.java
   - Implement createAccount(CreateAccountRequest)
   - Validate customer exists (FK constraint)
   - Generate account_number from ControlRepository counter
   - Validate sort_code matches customer's sort_code
   - Validate account_type (checking/savings/loan/mortgage/ISA)
   - Set eye-catcher to "ACCT"
   - Set opened_date to current date
   - Initialize actual_balance to 0
   - Set overdraft_limit (checking accounts only)
   - Set interest_rate (savings accounts)
   - Calculate available_balance = actual_balance + overdraft_limit
   - Write to PROCTRAN (transaction log)
   - Use @Transactional
   - Roll back on FK violation or validation failure

2. UpdateAccountService.java
   - Implement updateAccount(accountNumber, UpdateAccountRequest)
   - Validate account exists
   - Update allowed fields (overdraft_limit, interest_rate, last_statement_date)
   - Recalculate available_balance when actual_balance or overdraft_limit changes
   - Preserve immutable fields (sort_code, customer_number, account_number, eye-catcher)
   - Validate FK integrity maintained
   - Use @Transactional
   - Log all updates

3. DeleteAccountService.java
   - Implement deleteAccount(sortCode, accountNumber)
   - Logical delete (set deleted flag)
   - Validate account exists
   - Check business rules (e.g., balance must be 0)
   - Preserve audit trail
   - Use @Transactional
   - Log deletion

4. AccountCrudController.java
   - POST /api/accounts (create)
   - PUT /api/accounts/{accountNumber} (update)
   - DELETE /api/accounts/{sortCode}/{accountNumber} (delete)
   - HTTP status codes (201, 200, 204, 404, 409)
   - Error handling

5. DTOs
   - CreateAccountRequest.java (account_type, customer_number, overdraft_limit, interest_rate)
   - UpdateAccountRequest.java (overdraft_limit, interest_rate, last_statement_date)
   - AccountResponse.java
   - Validation annotations

Implementation Requirements:
- Run validator_004 tests continuously (50+ tests)
- Counter management for account_number
- FK validation with CustomerRepository
- PROCTRAN logging for account creation
- Balance calculations with 2 decimal precision
- All operations transactional
- Comprehensive error handling

Verification:
- All AccountCrudServiceTest tests pass (50+ tests)
- JaCoCo shows 90%+ instruction coverage
- FK constraints enforced
- Counter atomic
- PROCTRAN records created""",
            "definition_of_done": "CreateAccountService, UpdateAccountService, DeleteAccountService, and AccountCrudController created. All validator_004 tests pass. JaCoCo shows 90%+ coverage. Account CRUD operations work with FK validation.",
            "validation_mechanism": "Run 'mvn test -Dtest=AccountCrudServiceTest' - all 50+ tests pass. Run 'mvn verify' - JaCoCo shows 90%+ instruction, 80%+ branch coverage. FK constraints work. Counter atomic. REST API verified.",
            "estimated_hours": 12,
            "deliverables": ["CreateAccountService.java", "UpdateAccountService.java", "DeleteAccountService.java", "AccountCrudController.java", "CreateAccountRequest.java", "UpdateAccountRequest.java"]
        },
        {
            "id": "migrate_005",
            "title": "Migrate Transaction Operations (DBCRFUN, XFRFUN)",
            "content": "Migrate DBCRFUN and XFRFUN to DebitCreditService and TransferService with full test coverage from validator_005. Includes PROCTRAN logging and atomic balance updates.",
            "depends_on": ["migrate_004", "validator_005"],
            "prompt": """Migrate transaction operations from taylor-curran/og-cics-cobol-app to taylor-curran/target-springboot-cics.

Sources:
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/DBCRFUN.cbl (Debit/Credit)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/XFRFUN.cbl (Transfer)

Tests: Use validator_005 tests (TransactionServiceTest.java)

Create the following files:

1. DebitCreditService.java
   - Implement creditAccount(sortCode, accountNumber, amount, description)
   - Implement debitAccount(sortCode, accountNumber, amount, description)
   - Validate account exists
   - Validate amount > 0
   - For credit: actual_balance += amount
   - For debit: validate available_balance >= amount
   - For debit: allow overdraft within overdraft_limit
   - Update actual_balance atomically
   - Recalculate available_balance = actual_balance + overdraft_limit
   - Write PROCTRAN record with transaction details
   - Set transaction timestamp (ISO format: YYYY-MM-DD HH:MM:SS.SSS)
   - Use @Transactional
   - Roll back on insufficient funds or validation failure

2. TransferService.java
   - Implement transferFunds(sourceAccount, targetAccount, amount, description)
   - Validate both accounts exist
   - Validate source != target
   - Validate sufficient funds in source account
   - Debit source account
   - Credit target account
   - Both operations are atomic (single transaction)
   - Write 2 PROCTRAN records (debit + credit)
   - Link records with transfer reference
   - Use @Transactional
   - Roll back entire transfer on any failure

3. TransactionController.java
   - POST /api/transactions/credit
   - POST /api/transactions/debit
   - POST /api/transactions/transfer
   - HTTP status codes (200, 400, 404, 409)
   - Return transaction confirmation
   - Error handling with descriptive messages

4. DTOs
   - CreditDebitRequest.java (sortCode, accountNumber, amount, description)
   - TransferRequest.java (sourceAccount, targetAccount, amount, description)
   - TransactionResponse.java (transactionId, timestamp, newBalance, success)
   - Validation annotations (@Positive, @NotNull, etc.)

5. ProctranService.java (helper)
   - Implement writeProctranRecord(account, type, amount, description)
   - Generate unique transaction ID
   - Set timestamp
   - Store all transaction details
   - Support audit trail queries

Implementation Requirements:
- Run validator_005 tests continuously (40+ tests)
- Balance updates are atomic
- PROCTRAN records created for every transaction
- Transfer is all-or-nothing (atomicity)
- Precision handling for currency (2 decimals)
- Comprehensive logging
- Error messages match COBOL behavior

Verification:
- All TransactionServiceTest tests pass (40+ tests)
- JaCoCo shows 85%+ instruction coverage
- Atomicity verified in concurrent tests
- PROCTRAN records created correctly
- Balance calculations accurate""",
            "definition_of_done": "DebitCreditService, TransferService, TransactionController, ProctranService, and DTOs created. All validator_005 tests pass. JaCoCo shows 85%+ coverage. Transactions are atomic with PROCTRAN logging.",
            "validation_mechanism": "Run 'mvn test -Dtest=TransactionServiceTest' - all 40+ tests pass. Run 'mvn verify' - JaCoCo shows 85%+ instruction, 75%+ branch coverage. Integration tests verify atomicity. PROCTRAN records validated.",
            "estimated_hours": 12,
            "deliverables": ["DebitCreditService.java", "TransferService.java", "TransactionController.java", "ProctranService.java", "CreditDebitRequest.java", "TransferRequest.java", "TransactionResponse.java"]
        },
        {
            "id": "migrate_006",
            "title": "Migrate Additional Credit Agencies (CRDTAGY2-5)",
            "content": "Migrate CRDTAGY2-5 to extend CreditAgencyService with multi-agency support. Includes parallel credit checks, aggregation, and timeout handling from validator_006.",
            "depends_on": ["setup_001", "validator_006"],
            "prompt": """Migrate additional credit agencies from taylor-curran/og-cics-cobol-app to taylor-curran/target-springboot-cics.

Sources:
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/CRDTAGY2.cbl
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/CRDTAGY3.cbl
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/CRDTAGY4.cbl
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/CRDTAGY5.cbl

Existing: taylor-curran/target-springboot-cics/src/main/java/com/cbsa/migration/service/CreditAgencyService.java (CRDTAGY1)

Tests: Use validator_006 tests (MultiAgencyCreditServiceTest.java)

Extend or refactor existing files:

1. CreditAgencyService.java (extend)
   - Add processCreditCheckMultiAgency(customerId, agencies)
   - Implement parallel credit checks across multiple agencies
   - Use @Async and CompletableFuture for parallel execution
   - Configure thread pool (5 agencies = 5 threads max)
   - Apply 3-second timeout per agency
   - Aggregate scores from responding agencies
   - Calculate average score (exclude timeouts)
   - Use fastest response if some timeout
   - Update customer record with aggregated score
   - Log which agencies responded vs timed out

2. CreditAgencyConfig.java (new)
   - Configure async executor for parallel credit checks
   - Set thread pool size (configurable)
   - Set timeout duration (3 seconds)
   - Configure retry logic
   - Circuit breaker configuration

3. MultiAgencyCreditResponse.java (DTO)
   - List of agency responses
   - Individual agency scores
   - Aggregated score (average/median)
   - Response times per agency
   - Timeout indicators
   - Final credit decision

4. Update CreditAgencyController.java
   - POST /api/credit-agency/multi-check
   - Accepts list of agencies to query
   - Returns aggregated response
   - Includes timing information

Implementation Requirements:
- Run validator_006 tests continuously (25+ tests)
- Parallel execution using CompletableFuture.allOf()
- Timeout handling with orTimeout(3, TimeUnit.SECONDS)
- Score aggregation algorithms (average, median, weighted)
- Graceful degradation (partial responses OK)
- Each agency simulates 0-3 second delay
- Each agency returns score 1-999
- Comprehensive logging of all agency calls

Verification:
- All MultiAgencyCreditServiceTest tests pass (25+ tests)
- JaCoCo shows 80%+ instruction coverage
- Parallel execution faster than sequential
- Timeout tests complete within expected time
- Aggregation algorithms correct""",
            "definition_of_done": "CreditAgencyService extended with multi-agency support. CreditAgencyConfig and MultiAgencyCreditResponse created. All validator_006 tests pass. JaCoCo shows 80%+ coverage. Parallel credit checks work with timeout handling.",
            "validation_mechanism": "Run 'mvn test -Dtest=MultiAgencyCreditServiceTest' - all 25+ tests pass. Run 'mvn verify' - JaCoCo shows 80%+ coverage. Parallel tests faster than sequential. Timeout handling verified. REST endpoint tested.",
            "estimated_hours": 10,
            "deliverables": ["CreditAgencyService.java (updated)", "CreditAgencyConfig.java", "MultiAgencyCreditResponse.java", "CreditAgencyController.java (updated)"]
        },
        {
            "id": "migrate_007",
            "title": "Migrate BMS UI Programs (BNKMENU, BNK1xxx)",
            "content": "Migrate all BMS UI transaction programs to REST controllers. Maps CICS transactions to REST API endpoints with proper DTOs.",
            "depends_on": ["migrate_002", "migrate_004", "migrate_005"],
            "prompt": """Migrate BMS UI programs from taylor-curran/og-cics-cobol-app to taylor-curran/target-springboot-cics.

Sources (9 UI programs):
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/BNKMENU.cbl (Main Menu)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/BNK1CAC.cbl (Create Account UI)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/BNK1CCA.cbl (Create Customer UI)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/BNK1CCS.cbl (Customer Search UI)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/BNK1CRA.cbl (Additional UI)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/BNK1DAC.cbl (Delete Account UI)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/BNK1DCS.cbl (Delete Customer UI)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/BNK1TFN.cbl (Transfer UI)
- taylor-curran/og-cics-cobol-app/src/base/cobol_src/BNK1UAC.cbl (Update Account UI)

These programs are CICS BMS transaction handlers that call backend services (already migrated in migrate_001 through migrate_005).

Create the following files:

1. MenuController.java
   - GET /api/menu (returns available menu options)
   - Maps BNKMENU.cbl functionality
   - Returns menu structure with available operations
   - No backend service calls (just menu metadata)

2. CustomerUIController.java
   - GET /api/ui/customers/search (BNK1CCS - search customers)
   - POST /api/ui/customers/create (BNK1CCA - calls CreateCustomerService)
   - PUT /api/ui/customers/update (calls UpdateCustomerService)
   - DELETE /api/ui/customers/delete (BNK1DCS - calls DeleteCustomerService)
   - Returns UI-friendly responses with form validation errors

3. AccountUIController.java
   - POST /api/ui/accounts/create (BNK1CAC - calls CreateAccountService)
   - PUT /api/ui/accounts/update (BNK1UAC - calls UpdateAccountService)
   - DELETE /api/ui/accounts/delete (BNK1DAC - calls DeleteAccountService)
   - Returns UI-friendly responses

4. TransferUIController.java
   - POST /api/ui/transfers (BNK1TFN - calls TransferService)
   - GET /api/ui/transfers/history
   - Returns UI-friendly transfer confirmation

5. UI DTOs (separate from service DTOs)
   - UICustomerForm.java (maps BMS screen fields)
   - UIAccountForm.java (maps BMS screen fields)
   - UITransferForm.java (maps BMS screen fields)
   - UIMenuResponse.java (menu structure)
   - UIValidationError.java (field-level errors)
   - These DTOs transform backend service responses for UI consumption

Implementation Requirements:
- UI controllers call backend services (already migrated)
- Map CICS BMS screen fields to REST request/response
- Preserve validation logic from BMS programs
- Return user-friendly error messages
- HTTP status codes match CICS response codes
- Include field-level validation errors
- Comprehensive logging of UI interactions

Key CICS-to-REST Mappings:
- CICS SEND MAP → HTTP Response (JSON)
- CICS RECEIVE MAP → HTTP Request (JSON)
- CICS LINK to backend → Service method call
- CICS ABEND → HTTP 500 with error details
- CICS transaction IDs → REST endpoint paths

Verification:
- UI controllers successfully call backend services
- DTOs correctly transform data for UI
- Validation errors returned in user-friendly format
- All HTTP methods work correctly
- Integration tests pass""",
            "definition_of_done": "MenuController, CustomerUIController, AccountUIController, TransferUIController created with UI DTOs. Controllers successfully call backend services (migrate_002, migrate_004, migrate_005). UI responses are user-friendly. Integration tests pass.",
            "validation_mechanism": "Run integration tests calling UI endpoints. Verify backend services are called correctly. Test validation error responses. Verify HTTP status codes. Manual testing of all UI endpoints via Postman/curl.",
            "estimated_hours": 12,
            "deliverables": ["MenuController.java", "CustomerUIController.java", "AccountUIController.java", "TransferUIController.java", "UICustomerForm.java", "UIAccountForm.java", "UITransferForm.java", "UIMenuResponse.java", "UIValidationError.java"]
        },
        {
            "id": "validator_007",
            "title": "Create End-to-End Integration Test Suite",
            "content": "Build comprehensive E2E test suite covering complete workflows across all migrated services. Validates full user journeys and data consistency.",
            "depends_on": ["migrate_007"],
            "prompt": """Create end-to-end integration tests for the complete migration in taylor-curran/target-springboot-cics.

All 24 COBOL programs have been migrated. Now create E2E tests that validate complete workflows:

Create BankingE2ETest.java with 20+ test scenarios:

1. Customer Lifecycle Tests (5 tests)
   - Create customer → Inquire customer → Update customer → Delete customer
   - Create customer triggers credit check
   - Customer creation updates counter
   - Update customer preserves immutable fields
   - Delete customer fails if accounts exist

2. Account Lifecycle Tests (5 tests)
   - Create customer → Create account → Inquire account → Update account → Delete account
   - Account FK references customer
   - Account creation updates counter
   - Account balance calculations correct
   - Delete account requires zero balance

3. Transaction Workflow Tests (5 tests)
   - Create customer → Create account → Credit account → Debit account → Inquire balance
   - Credit/debit creates PROCTRAN records
   - Balance updates are immediate
   - Overdraft limit enforced
   - Insufficient funds rejected

4. Transfer Workflow Tests (3 tests)
   - Create 2 customers → Create 2 accounts → Transfer between accounts → Verify balances
   - Transfer is atomic (both succeed or both fail)
   - Transfer creates 2 PROCTRAN records
   - Transfer validates sufficient funds

5. Multi-Agency Credit Check Tests (2 tests)
   - Create customer triggers multi-agency check
   - Aggregated score calculated correctly
   - Timeout handling works

6. UI Workflow Tests (requires migrate_007)
   - Menu → Create customer via UI → Create account via UI → Transfer via UI
   - UI validation errors returned correctly
   - UI successfully calls backend services

Setup:
- Use @SpringBootTest for full application context
- Use TestRestTemplate for HTTP calls
- Test against H2 in-memory database
- Use @Transactional for test isolation
- Create comprehensive test fixtures
- Test data generated for realistic scenarios

Verification Steps:
- All workflows complete successfully
- Data consistency across all tables
- PROCTRAN audit trail complete
- FK constraints enforced
- Counters increment correctly
- Balance calculations accurate
- Credit checks working
- Rollback on failure works

Expected Coverage:
- Full integration coverage across all services
- Database consistency validated
- Error handling verified
- Performance acceptable (workflows complete in <5 seconds)""",
            "definition_of_done": "BankingE2ETest.java exists with 20+ workflow tests. Tests cover customer, account, transaction, transfer, and credit workflows. All tests pass. Data consistency validated across tables.",
            "validation_mechanism": "Run 'mvn test -Dtest=BankingE2ETest'. All 20+ tests pass. Test execution <60 seconds. All workflows complete successfully. Data consistency checks pass. PROCTRAN records validated.",
            "estimated_hours": 10,
            "deliverables": ["BankingE2ETest.java", "E2ETestFixtures.java", "WorkflowValidationUtils.java"]
        }
    ]
}
