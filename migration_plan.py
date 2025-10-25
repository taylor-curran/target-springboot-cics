"""
Migration Task Graph for COBOL to Spring Boot Migration
Cross-referenced between og-cics-cobol-app and target-springboot-cics

Status:
- Fully Migrated: 5 programs (CRDTAGY1, GETCOMPY, GETSCODE, ABNDPROC, BANKDATA)
- Repository Infrastructure: Customer, Account, Transaction JDBC repos exist
- Need Migration: 15 programs (4 customer, 5 account, 2 transaction, 4 credit agencies)
- No Migration Needed: 9 BMS UI programs (replaced by REST APIs)

Validator Strategy:
- Domain validators: One comprehensive test suite validates multiple related migrations
- Cross-cutting validators: Data observability and performance apply across all operations
- Complex operation validators: Async, cascade, atomicity for challenging migrations
- Flexible dependencies: Complex migrations depend on multiple validators
"""

migration_plan = {
    "tasks": [
        {
            "id": "setup_001",
            "title": "Establish Performance Baseline for Legacy COBOL System",
            "content": "Measure and document performance baselines for all 15 unmigrated COBOL programs (customer, account, transaction operations). Record P50/P95/P99 latencies, throughput, error rates, and response times from the legacy CICS/COBOL system.",
            "depends_on": [],
            "prompt": """Analyze the legacy COBOL system in og-cics-cobol-app/src/base/cobol_src/ and establish performance baselines for the following 15 programs:

Customer Operations:
- INQCUST.cbl (inquiry/read)
- CRECUST.cbl (create with async credit checks)
- UPDCUST.cbl (update)
- DELCUS.cbl (delete with cascade)

Account Operations:
- INQACC.cbl (inquiry by account)
- INQACCCU.cbl (inquiry by customer)
- CREACC.cbl (create with counter management)
- UPDACC.cbl (update)
- DELACC.cbl (delete)

Transaction Operations:
- XFRFUN.cbl (transfer funds - dual account update)
- DBCRFUN.cbl (debit/credit)

Credit Agencies:
- CRDTAGY2.cbl through CRDTAGY5.cbl (agency simulators)

For each program, document:
1. P50, P95, P99 latency metrics
2. Throughput (transactions per second)
3. Error rates and failure modes
4. Resource utilization patterns

Create baseline_metrics.md with structured data for comparison during migration validation.""",
            "definition_of_done": "baseline_metrics.md created with performance data for all 15 programs, including specific numeric targets for P50/P95/P99 latencies",
            "validation_mechanism": "Baseline document contains specific measurements (not placeholders) for all 15 programs with P50/P95/P99 data points",
            "estimated_hours": 10,
            "deliverables": ["baseline_metrics.md", "performance_measurement_scripts/"]
        },
        
        {
            "id": "setup_002",
            "title": "Setup Monitoring and Observability Infrastructure",
            "content": "Deploy comprehensive monitoring for the Spring Boot application including JaCoCo coverage tracking, application metrics, logging, and performance monitoring. Configure dashboards and alerts.",
            "depends_on": [],
            "prompt": """Setup monitoring and observability infrastructure for target-springboot-cics Spring Boot application:

1. Verify JaCoCo is properly configured in pom.xml (should already exist per TESTING.md)
2. Configure application metrics collection (Spring Boot Actuator)
3. Setup structured logging with appropriate log levels
4. Create monitoring dashboards for:
   - Service layer coverage (target: 80%)
   - Repository layer coverage (target: 70%)
   - Controller layer coverage (target: 60%)
   - Application performance metrics
   - Error rates and types
   
5. Configure alerts for:
   - Coverage drops below thresholds
   - Performance degradation
   - Error rate spikes

Reference target-springboot-cics/docs/TESTING.md for coverage requirements by layer.
Reference target-springboot-cics/MIGRATION_PLAYBOOK.md for testing patterns.""",
            "definition_of_done": "Monitoring operational with dashboards showing live metrics, JaCoCo coverage tracking enabled, alerts configured",
            "validation_mechanism": "Run `mvn verify` successfully, confirm coverage reports generate at target/site/jacoco/index.html, verify metrics endpoints are accessible",
            "estimated_hours": 8,
            "deliverables": ["monitoring_config.yaml", "dashboard_urls.txt", "pom.xml updates if needed"]
        },
        
        {
            "id": "setup_003",
            "title": "Validate Database Schema and Control Table",
            "content": "Verify database schema completeness for all migration operations. Confirm control table exists and works for counter management (needed for CREACC, CRECUST). Document any schema gaps.",
            "depends_on": [],
            "prompt": """Validate database schema in target-springboot-cics for migration readiness:

1. Review existing schemas:
   - src/main/resources/db/schema.sql (SQLite production)
   - src/test/resources/db/test-schema.sql (H2 test)

2. Verify tables exist and are complete for:
   - customer table (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score, credit_score_review_date)
   - account table (eye_catcher, customer_number, sort_code, account_number, account_type, interest_rate, opened_date, overdraft_limit, last_statement_date, next_statement_date, available_balance, actual_balance)
   - bank_transaction table (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
   - control table (id='CONTROL', customer_count, last_customer_number, account_count, last_account_number)

3. Verify ControlRepository methods exist:
   - getNextCustomerNumber() - needed for CRECUST
   - getNextAccountNumber() - needed for CREACC
   - initializeControlRecord()

4. Confirm schema consistency test exists: DatabaseSchemaConsistencyTest
5. Document any missing columns or tables needed for migrations

Reference: COBOL copybooks in og-cics-cobol-app/src/base/cobol_src/ (CUSTOMER.cpy, ACCOUNT.cpy, PROCTRAN.cpy)""",
            "definition_of_done": "Schema validation report confirms all tables and columns exist for 15 programs, control table functional, no gaps identified",
            "validation_mechanism": "Run DatabaseSchemaConsistencyTest successfully, verify ControlRepository interface methods exist, manually test getNextCustomerNumber() and getNextAccountNumber()",
            "estimated_hours": 6,
            "deliverables": ["schema_validation_report.md"]
        },

        {
            "id": "validator_001",
            "title": "Customer CRUD Test Suite (All 4 Operations)",
            "content": "Create comprehensive test suite for ALL customer operations: INQCUST (read), CRECUST (create), UPDCUST (update), DELCUS (delete). This single validator supports all 4 customer migrations. Includes unit tests with mocks and integration tests with real H2 database. 60+ test cases covering happy paths, edge cases, validation rules, error handling.",
            "depends_on": ["setup_002"],
            "prompt": """Create comprehensive customer CRUD test suite in target-springboot-cics/src/test/java/com/cbsa/migration/service/CustomerServiceTest.java

This test suite will validate ALL FOUR customer operations:
1. INQCUST (read/inquiry)
2. CRECUST (create)
3. UPDCUST (update)
4. DELCUS (delete)

Follow the pattern from ErrorLoggingServiceTest.java (nested UnitTests and IntegrationTest classes).

Test Structure:
```java
class CustomerServiceTest {
    @ExtendWith(MockitoExtension.class)
    static class UnitTests {
        @Mock private CustomerRepository customerRepository;
        @Mock private CreditAgencyService creditAgencyService;
        @InjectMocks private CustomerService customerService;
        
        // INQCUST tests (15+ cases)
        // CRECUST tests (20+ cases including credit checks)
        // UPDCUST tests (15+ cases)
        // DELCUS tests (10+ cases - basic delete only, cascade tested separately)
    }
    
    @SpringBootTest
    @ActiveProfiles("test")
    @Sql("/db/test-schema.sql")
    @Transactional
    static class IntegrationTest {
        // Integration tests with real H2 database
    }
}
```

Test Coverage Requirements (per TESTING.md):
- Service layer: 80% instructions, 70% branches
- Include date validation: min year 1601, max age 150, reject future dates (per notes)
- Test eye-catcher validation ('CUST')
- Test composite key (sort_code, customer_number)

Reference:
- JdbcCustomerRepository.java - shows available repository methods
- INQCUST.cbl, CRECUST.cbl, UPDCUST.cbl, DELCUS.cbl - COBOL business logic
- docs/TESTING.md - test patterns and coverage requirements""",
            "definition_of_done": "CustomerServiceTest.java created with 60+ test cases covering all 4 operations, tests compile and can run (will fail until services implemented)",
            "validation_mechanism": "Test suite compiles successfully, JaCoCo configured, test structure follows ErrorLoggingServiceTest pattern with nested classes",
            "estimated_hours": 12,
            "deliverables": ["src/test/java/com/cbsa/migration/service/CustomerServiceTest.java"]
        },
        
        {
            "id": "validator_002",
            "title": "Account CRUD Test Suite (All 5 Operations)",
            "content": "Create comprehensive test suite for ALL account operations: INQACC (read by account), INQACCCU (read by customer), CREACC (create), UPDACC (update), DELACC (delete). This single validator supports all 5 account migrations. 70+ test cases covering account types, balance calculations, overdraft limits, statement dates.",
            "depends_on": ["setup_002", "setup_003"],
            "prompt": """Create comprehensive account CRUD test suite in target-springboot-cics/src/test/java/com/cbsa/migration/service/AccountServiceTest.java

This test suite will validate ALL FIVE account operations:
1. INQACC (inquiry by account)
2. INQACCCU (inquiry by customer - multiple accounts)
3. CREACC (create with counter management)
4. UPDACC (update)
5. DELACC (delete)

Follow CustomerServiceTest pattern with nested UnitTests and IntegrationTest classes.

Test Structure:
```java
class AccountServiceTest {
    @ExtendWith(MockitoExtension.class)
    static class UnitTests {
        @Mock private AccountRepository accountRepository;
        @Mock private ControlRepository controlRepository;
        @InjectMocks private AccountService accountService;
        
        // INQACC tests (15+ cases)
        // INQACCCU tests (15+ cases with multiple accounts per customer)
        // CREACC tests (20+ cases including counter management)
        // UPDACC tests (15+ cases, note: cannot change balance)
        // DELACC tests (5+ cases - basic delete, cascade tested separately)
    }
    
    @SpringBootTest
    @ActiveProfiles("test")
    @Sql("/db/test-schema.sql")
    @Transactional
    static class IntegrationTest {
        // Integration tests with real H2 database
    }
}
```

Key Test Scenarios:
- Multiple account types (checking, savings, etc.)
- Balance calculations (available vs actual)
- Overdraft limits
- Statement date management
- Counter management for CREACC (getNextAccountNumber)
- Eye-catcher validation ('ACCT')
- Composite key (sort_code, account_number)

Reference:
- JdbcAccountRepository.java - available repository methods
- ControlRepository.java - getNextAccountNumber() method
- INQACC.cbl, INQACCCU.cbl, CREACC.cbl, UPDACC.cbl, DELACC.cbl - COBOL logic""",
            "definition_of_done": "AccountServiceTest.java created with 70+ test cases covering all 5 operations, tests compile and structured correctly",
            "validation_mechanism": "Test suite compiles successfully, includes integration with ControlRepository for counter management, follows established test patterns",
            "estimated_hours": 14,
            "deliverables": ["src/test/java/com/cbsa/migration/service/AccountServiceTest.java"]
        },
        
        {
            "id": "validator_003",
            "title": "Transaction Operations Test Suite (XFRFUN and DBCRFUN)",
            "content": "Create comprehensive test suite for transaction operations: XFRFUN (transfer funds with dual-account updates) and DBCRFUN (debit/credit single account). 50+ test cases covering transaction types, amount validations, balance updates, transaction logging to bank_transaction table.",
            "depends_on": ["setup_002", "validator_002"],
            "prompt": """Create transaction operations test suite in target-springboot-cics/src/test/java/com/cbsa/migration/service/TransactionServiceTest.java

This test suite validates BOTH transaction operations:
1. XFRFUN (transfer funds - updates TWO accounts)
2. DBCRFUN (debit/credit - updates ONE account)

Test Structure:
```java
class TransactionServiceTest {
    @ExtendWith(MockitoExtension.class)
    static class UnitTests {
        @Mock private AccountRepository accountRepository;
        @Mock private TransactionRepository transactionRepository;
        @InjectMocks private TransactionService transactionService;
        
        // DBCRFUN tests (20+ cases)
        // - Debit operations (withdrawals)
        // - Credit operations (deposits)
        // - Amount validation
        // - Balance updates
        // - Transaction record creation
        
        // XFRFUN tests (30+ cases)
        // - Transfer between accounts
        // - Dual account balance updates
        // - Target account validation
        // - Transaction records for both accounts
        // - Insufficient funds handling
    }
    
    @SpringBootTest
    @ActiveProfiles("test")
    @Sql("/db/test-schema.sql")
    @Transactional
    static class IntegrationTest {
        // Integration tests ensuring atomicity
    }
}
```

Key Test Scenarios:
- Transaction types: CRE (credit), DEB (debit), TFR (transfer)
- Balance calculations: available_balance and actual_balance
- Transaction record creation in bank_transaction table
- Eye-catcher validation ('PRTR')
- Composite key for transactions
- XFRFUN: Target account existence and validation
- DBCRFUN: Simple single-account operations

Reference:
- JdbcAccountRepository.java - account updates
- JdbcTransactionRepository.java - transaction logging
- Transaction.java model - transaction types and fields
- XFRFUN.cbl, DBCRFUN.cbl - COBOL business logic""",
            "definition_of_done": "TransactionServiceTest.java created with 50+ test cases covering both XFRFUN and DBCRFUN, tests compile",
            "validation_mechanism": "Test suite compiles, includes tests for dual-account updates (XFRFUN) and single-account updates (DBCRFUN)",
            "estimated_hours": 12,
            "deliverables": ["src/test/java/com/cbsa/migration/service/TransactionServiceTest.java"]
        },
        
        {
            "id": "validator_004",
            "title": "Credit Agency Extension Tests (CRDTAGY2-5)",
            "content": "Create test suite for credit agency extensions CRDTAGY2-5. These are identical in structure to CRDTAGY1 (already migrated) - they simulate multiple external credit agencies. 20+ test cases for each agency variant covering delay simulation, score generation, customer updates.",
            "depends_on": ["setup_002"],
            "prompt": """Create credit agency extension tests in target-springboot-cics/src/test/java/com/cbsa/migration/service/CreditAgencyExtensionTest.java

CRDTAGY2, CRDTAGY3, CRDTAGY4, CRDTAGY5 are structurally IDENTICAL to CRDTAGY1 (already migrated as CreditAgencyService). They simulate multiple external credit agencies for parallel credit score fetching.

From CRDTAGY2.cbl analysis:
- Random delay simulation (0-3 seconds) - same as CRDTAGY1
- Random credit score generation (1-999) - same as CRDTAGY1
- Customer record updates - same as CRDTAGY1

Test approach: Extend existing CreditAgencyService or create variants (CreditAgency2Service, etc.)

Test Structure:
```java
class CreditAgencyExtensionTest {
    @ExtendWith(MockitoExtension.class)
    static class UnitTests {
        // Tests for agency 2-5 variants
        // Similar to existing CreditAgencyService tests
        // 20+ test cases per agency covering:
        // - Score generation
        // - Delay simulation
        // - Customer updates
        // - Error handling
    }
}
```

Key Test Scenarios:
- Each agency (2-5) generates random scores independently
- Delay simulation works for each agency
- Customer credit score updates
- Parallel execution scenarios (for CRECUST async calls)

Reference:
- CreditAgencyService.java - existing implementation pattern
- CRDTAGY2.cbl through CRDTAGY5.cbl - identical COBOL structure
- CreditAgencyController.java - existing REST API pattern""",
            "definition_of_done": "CreditAgencyExtensionTest.java created with 80+ test cases (20 per agency x 4), tests compile",
            "validation_mechanism": "Test suite compiles, tests structured similarly to existing CreditAgencyService tests",
            "estimated_hours": 8,
            "deliverables": ["src/test/java/com/cbsa/migration/service/CreditAgencyExtensionTest.java"]
        },
        
        {
            "id": "validator_005",
            "title": "Async Credit Check Integration Tests",
            "content": "Create specialized tests for CRECUST async credit agency calls. Tests parallel execution, timeout handling, result aggregation, and fallback behavior when credit agencies don't respond within 3-second window. This validator specifically supports the complex CRECUST migration.",
            "depends_on": ["validator_001", "validator_004"],
            "prompt": """Create async credit check integration tests in target-springboot-cics/src/test/java/com/cbsa/migration/integration/AsyncCreditCheckTest.java

This validator specifically tests the async credit agency integration for CRECUST.

From CRECUST.cbl analysis (lines 15-22):
"It then performs a credit check on multiple credit agencies (using Async API), waits for 3 seconds and aggregates and averages the returned credit scores. If no data is returned, from the credit checks, then set the credit score to 0 and mark the credit score review date as today."

Test Structure:
```java
@SpringBootTest
@ActiveProfiles("test")
class AsyncCreditCheckTest {
    @Autowired private CustomerService customerService;
    @Autowired private CreditAgencyService creditAgency1;
    
    @Test
    void testParallelCreditChecks_allAgenciesRespond() {
        // Call customer create
        // Verify all agencies called in parallel
        // Verify score aggregation/averaging
    }
    
    @Test
    void testParallelCreditChecks_someTimeout() {
        // Simulate some agencies taking > 3 seconds
        // Verify timeout handling
        // Verify aggregation with partial results
    }
    
    @Test
    void testParallelCreditChecks_allTimeout() {
        // All agencies exceed 3-second timeout
        // Verify credit score set to 0
        // Verify review date set to today
    }
    
    // 10+ additional test cases for various async scenarios
}
```

Key Test Scenarios:
- Parallel execution of 5 credit agencies (CRDTAGY1-5)
- 3-second timeout enforcement
- Score aggregation and averaging
- Fallback behavior (score=0, review date=today) when all timeout
- Partial results when some timeout
- Error handling for individual agencies

Reference:
- CRECUST.cbl lines 15-22 - async API usage description
- CreditAgencyService.java - existing agency implementation
- CustomerService (to be created) - will contain async logic""",
            "definition_of_done": "AsyncCreditCheckTest.java created with 15+ async test cases, tests compile and test async patterns",
            "validation_mechanism": "Test suite compiles, includes timeout tests, parallel execution tests, and fallback behavior tests",
            "estimated_hours": 10,
            "deliverables": ["src/test/java/com/cbsa/migration/integration/AsyncCreditCheckTest.java"]
        },
        
        {
            "id": "validator_006",
            "title": "Cascade Delete Integration Tests",
            "content": "Create specialized tests for DELCUS cascade delete operations. Tests that customer deletion properly deletes all associated accounts first, maintains referential integrity, handles transaction logging, and rolls back on partial failure. This validator specifically supports the complex DELCUS migration.",
            "depends_on": ["validator_001", "validator_002"],
            "prompt": """Create cascade delete integration tests in target-springboot-cics/src/test/java/com/cbsa/migration/integration/CascadeDeleteTest.java

This validator specifically tests the cascade delete logic for DELCUS.

From DELCUS.cbl analysis (lines 10-24):
"This program takes an incoming customer number & retrieves the associated accounts for it and stores these in an array. Then it deletes the accounts one at a time and writes a PROCTRAN delete account record out for each deleted account. When all accounts have been deleted, it deletes the customer record (and writes out a customer delete record to PROCTRAN)."

Test Structure:
```java
@SpringBootTest
@ActiveProfiles("test")
@Sql("/db/test-schema.sql")
@Transactional
class CascadeDeleteTest {
    @Autowired private CustomerService customerService;
    @Autowired private AccountRepository accountRepository;
    @Autowired private TransactionRepository transactionRepository;
    
    @Test
    void testDeleteCustomer_withMultipleAccounts() {
        // Create customer with 3 accounts
        // Delete customer
        // Verify all 3 accounts deleted first
        // Verify customer deleted last
        // Verify transaction records created for each deletion
    }
    
    @Test
    void testDeleteCustomer_maintainsReferentialIntegrity() {
        // Verify cascade maintains database constraints
    }
    
    @Test
    void testDeleteCustomer_rollbackOnPartialFailure() {
        // Simulate failure after deleting some accounts
        // Verify full rollback
    }
    
    // 10+ additional test cases for cascade scenarios
}
```

Key Test Scenarios:
- Customer with 0, 1, multiple (5+) accounts
- Accounts deleted before customer
- Transaction records created for each account deletion
- Transaction record created for customer deletion
- Referential integrity maintained
- Rollback on partial failure
- Account already deleted scenario (should continue)

Reference:
- DELCUS.cbl lines 10-24 - cascade delete logic
- JdbcCustomerRepository.deleteById()
- JdbcAccountRepository.deleteById()
- JdbcTransactionRepository.save() - for transaction logging""",
            "definition_of_done": "CascadeDeleteTest.java created with 15+ cascade test cases, tests compile and test cascade patterns",
            "validation_mechanism": "Test suite compiles, includes referential integrity tests, rollback tests, and transaction logging verification",
            "estimated_hours": 10,
            "deliverables": ["src/test/java/com/cbsa/migration/integration/CascadeDeleteTest.java"]
        },
        
        {
            "id": "validator_007",
            "title": "Transaction Atomicity and Rollback Tests",
            "content": "Create specialized tests for XFRFUN transaction atomicity. Tests that dual-account updates are atomic, proper rollback on failure, balance consistency, and transaction logging. This validator specifically supports the complex XFRFUN migration requiring two-phase updates.",
            "depends_on": ["validator_002", "validator_003"],
            "prompt": """Create transaction atomicity tests in target-springboot-cics/src/test/java/com/cbsa/migration/integration/TransactionAtomicityTest.java

This validator specifically tests the atomicity and rollback logic for XFRFUN.

From XFRFUN.cbl analysis (lines 11-37):
"This program takes an incoming sortcode/account number (key) from the account where the money is transfered from and a sortcode/account number (key) combination for the account where the money is being transferred to... If the update to either the ACCOUNT or PROCTRAN datastore is unsuccessful, then the transaction is deemed to have failed. Should there be some kind of datastore failure or other unpredictable failure we should report it, back out any updates already made and abend the transaction."

Test Structure:
```java
@SpringBootTest
@ActiveProfiles("test")
@Sql("/db/test-schema.sql")
@Transactional
class TransactionAtomicityTest {
    @Autowired private TransactionService transactionService;
    @Autowired private AccountRepository accountRepository;
    @Autowired private TransactionRepository transactionRepository;
    
    @Test
    void testTransfer_bothAccountsUpdatedAtomically() {
        // Execute transfer
        // Verify source account debited
        // Verify target account credited
        // Verify both updates in single transaction
    }
    
    @Test
    void testTransfer_rollbackOnSourceUpdateFailure() {
        // Simulate failure updating source account
        // Verify full rollback - no accounts updated
    }
    
    @Test
    void testTransfer_rollbackOnTargetUpdateFailure() {
        // Simulate failure updating target account
        // Verify full rollback - source account not debited
    }
    
    @Test
    void testTransfer_rollbackOnTransactionLogFailure() {
        // Simulate failure writing transaction record
        // Verify full rollback - no account updates
    }
    
    // 15+ additional atomicity test cases
}
```

Key Test Scenarios:
- Successful transfer: both accounts updated + 2 transaction records
- Source account failure → full rollback
- Target account failure → full rollback
- Transaction log failure → full rollback
- Balance consistency across failures
- Concurrent transfer scenarios
- Insufficient funds handling
- Invalid target account handling

Reference:
- XFRFUN.cbl lines 11-37 - dual account update logic
- Spring @Transactional annotation for atomicity
- JdbcAccountRepository.save() - account updates
- JdbcTransactionRepository.save() - transaction logging""",
            "definition_of_done": "TransactionAtomicityTest.java created with 20+ atomicity test cases, tests compile and test rollback scenarios",
            "validation_mechanism": "Test suite compiles, includes rollback tests for each failure point, verifies no partial updates occur",
            "estimated_hours": 12,
            "deliverables": ["src/test/java/com/cbsa/migration/integration/TransactionAtomicityTest.java"]
        },
        
        {
            "id": "validator_008",
            "title": "Data Observability and Integrity Validator",
            "content": "Create cross-cutting data quality validator that checks referential integrity, data consistency, eye-catcher validity, composite key constraints across ALL operations. Includes data quality metrics, constraint validation, and integrity checks that apply to all migrations.",
            "depends_on": ["setup_002"],
            "prompt": """Create data observability validator in target-springboot-cics/src/test/java/com/cbsa/migration/validation/DataObservabilityTest.java

This is a CROSS-CUTTING validator that applies to ALL customer, account, and transaction operations.

Test Structure:
```java
@SpringBootTest
@ActiveProfiles("test")
@Sql("/db/test-schema.sql")
class DataObservabilityTest {
    @Autowired private CustomerRepository customerRepository;
    @Autowired private AccountRepository accountRepository;
    @Autowired private TransactionRepository transactionRepository;
    @Autowired private JdbcTemplate jdbcTemplate;
    
    // Referential Integrity Tests
    @Test
    void testReferentialIntegrity_accountsReferenceValidCustomers() {
        // Query all accounts
        // Verify each references existing customer
    }
    
    @Test
    void testReferentialIntegrity_transactionsReferenceValidAccounts() {
        // Query all transactions
        // Verify each references existing account
    }
    
    // Eye-Catcher Validation
    @Test
    void testEyeCatchers_customersHaveCUST() {
        // Verify all customer records have eye_catcher='CUST'
    }
    
    @Test
    void testEyeCatchers_accountsHaveACCT() {
        // Verify all account records have eye_catcher='ACCT'
    }
    
    @Test
    void testEyeCatchers_transactionsHavePRTR() {
        // Verify all transaction records have eye_catcher='PRTR'
    }
    
    // Composite Key Validation
    @Test
    void testCompositeKeys_customerUniqueness() {
        // Verify (sort_code, customer_number) unique
    }
    
    @Test
    void testCompositeKeys_accountUniqueness() {
        // Verify (sort_code, account_number) unique
    }
    
    // Data Consistency
    @Test
    void testDataConsistency_accountBalances() {
        // Verify available_balance <= actual_balance + overdraft_limit
    }
    
    @Test
    void testDataConsistency_dateValidations() {
        // Verify date_of_birth >= 1601, age <= 150, no future dates
    }
    
    // 20+ additional data quality checks
}
```

Key Validation Areas:
- Referential integrity (foreign keys)
- Eye-catcher patterns ('CUST', 'ACCT', 'PRTR')
- Composite key constraints
- Date validations (min year 1601, max age 150, no future dates per notes)
- Balance consistency
- Data type constraints
- Required field validation

Reference:
- schema.sql - database constraints
- COBOL copybooks - eye-catcher patterns
- Notes - date validation rules""",
            "definition_of_done": "DataObservabilityTest.java created with 30+ data quality test cases, tests compile",
            "validation_mechanism": "Test suite compiles, covers all major data quality dimensions (integrity, consistency, constraints)",
            "estimated_hours": 10,
            "deliverables": ["src/test/java/com/cbsa/migration/validation/DataObservabilityTest.java"]
        },
        
        {
            "id": "validator_009",
            "title": "Performance Comparison Validator",
            "content": "Create performance validator that compares Java service implementations against baseline metrics from setup_001. Measures P50/P95/P99 latencies for each migrated operation and flags if performance degrades more than 10% from COBOL baseline.",
            "depends_on": ["setup_001", "setup_002"],
            "prompt": """Create performance comparison validator in target-springboot-cics/src/test/java/com/cbsa/migration/validation/PerformanceComparisonTest.java

This validator compares Java implementation performance against COBOL baselines from setup_001.

Test Structure:
```java
@SpringBootTest
@ActiveProfiles("test")
@Sql("/db/test-schema.sql")
class PerformanceComparisonTest {
    @Autowired private CustomerService customerService;
    @Autowired private AccountService accountService;
    @Autowired private TransactionService transactionService;
    
    private BaselineMetrics baselineMetrics;
    
    @BeforeEach
    void loadBaselines() {
        // Parse baseline_metrics.md
    }
    
    @Test
    void testPerformance_customerInquiry_withinBaseline() {
        // Run INQCUST equivalent 1000 times
        // Measure P50, P95, P99
        // Compare to baseline
        // Assert within 10% of baseline
    }
    
    @Test
    void testPerformance_customerCreate_withinBaseline() {
        // Run CRECUST equivalent 1000 times
        // Measure and compare
    }
    
    // Performance tests for each of 15 operations
    // Each compares to baseline from setup_001
}
```

Performance Metrics to Measure:
- P50 (median) latency
- P95 latency
- P99 latency
- Throughput (operations per second)
- Memory usage
- CPU utilization

Acceptance Criteria:
- Java implementation within 10% of COBOL baseline for P50/P95/P99
- Document any operations that exceed 10% variance

Reference:
- baseline_metrics.md (from setup_001)
- All service implementations (customer, account, transaction)
- Spring Boot Actuator for metrics collection""",
            "definition_of_done": "PerformanceComparisonTest.java created with performance tests for all 15 operations, tests compile",
            "validation_mechanism": "Test suite compiles, includes baseline loading logic, calculates P50/P95/P99 metrics",
            "estimated_hours": 10,
            "deliverables": ["src/test/java/com/cbsa/migration/validation/PerformanceComparisonTest.java", "performance_comparison_report.md"]
        },
        
        {
            "id": "validator_010",
            "title": "Controller Integration Tests (REST API Layer)",
            "content": "Create controller integration tests using MockMvc for all REST endpoints. Tests request/response DTOs, HTTP status codes, error handling, validation, and JSON serialization. Covers customer, account, and transaction controllers.",
            "depends_on": ["setup_002"],
            "prompt": """Create controller integration tests in target-springboot-cics/src/test/java/com/cbsa/migration/controller/

This validator tests the REST API layer using MockMvc pattern.

Create separate test files:
1. CustomerControllerTest.java - tests for customer endpoints
2. AccountControllerTest.java - tests for account endpoints  
3. TransactionControllerTest.java - tests for transaction endpoints

Follow pattern from ErrorControllerTest:
```java
@WebMvcTest(CustomerController.class)
class CustomerControllerTest {
    @Autowired private MockMvc mockMvc;
    @MockBean private CustomerService customerService;
    
    @Test
    void testGetCustomer_success_returns200() throws Exception {
        // Mock service response
        // Perform GET request
        // Verify HTTP 200
        // Verify JSON response
    }
    
    @Test
    void testGetCustomer_notFound_returns404() throws Exception {
        // Mock service to return empty
        // Perform GET request
        // Verify HTTP 404
    }
    
    @Test
    void testCreateCustomer_validRequest_returns201() throws Exception {
        // POST request with JSON
        // Verify HTTP 201
        // Verify Location header
    }
    
    @Test
    void testCreateCustomer_invalidData_returns400() throws Exception {
        // POST with invalid data
        // Verify HTTP 400
        // Verify error message
    }
    
    // 15+ tests per controller
}
```

Key Test Scenarios per Controller:
- GET requests: 200 success, 404 not found, 400 bad request
- POST requests: 201 created, 400 validation errors, 500 server errors
- PUT requests: 200 updated, 404 not found, 400 validation
- DELETE requests: 204 no content, 404 not found
- JSON serialization/deserialization
- DTO validation
- Error response format

Coverage Target: 60% controller layer (per TESTING.md)

Reference:
- ErrorControllerTest.java - existing pattern
- CreditAgencyController.java - REST endpoint examples
- docs/TESTING.md - controller testing guidance""",
            "definition_of_done": "Three controller test files created (Customer, Account, Transaction) with 45+ total test cases, tests compile",
            "validation_mechanism": "Test suites compile, use @WebMvcTest annotation, mock service layers, test HTTP status codes and JSON responses",
            "estimated_hours": 12,
            "deliverables": [
                "src/test/java/com/cbsa/migration/controller/CustomerControllerTest.java",
                "src/test/java/com/cbsa/migration/controller/AccountControllerTest.java",
                "src/test/java/com/cbsa/migration/controller/TransactionControllerTest.java"
            ]
        },

        {
            "id": "migrate_001",
            "title": "Migrate INQCUST - Customer Inquiry Service",
            "content": "Migrate INQCUST.cbl to CustomerService.inquireCustomer() method. Implements customer read/inquiry by sort code and customer number. Uses existing JdbcCustomerRepository.findById(). Creates CustomerController.getCustomer() REST endpoint.",
            "depends_on": ["setup_001", "setup_002", "setup_003", "validator_001", "validator_008", "validator_009", "validator_010"],
            "prompt": """Migrate INQCUST.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/INQCUST.cbl
Target repo: target-springboot-cics

Implementation:
1. Create src/main/java/com/cbsa/migration/service/CustomerService.java
2. Create src/main/java/com/cbsa/migration/controller/CustomerController.java
3. Create DTOs: CustomerRequestDto.java, CustomerResponseDto.java

CustomerService.inquireCustomer():
- Use existing JdbcCustomerRepository.findById(sortCode, customerNumber)
- Return customer if found
- Return null/empty if not found
- Handle errors with ErrorLoggingService

CustomerController.getCustomer():
- GET /api/customers/{sortCode}/{customerNumber}
- Returns 200 + customer JSON on success
- Returns 404 if not found
- Returns 500 on error

Follow patterns from:
- CreditAgencyService.java - service structure
- CreditAgencyController.java - controller structure
- JdbcCustomerRepository.java - repository methods
- INQCUST.cbl - business logic (simple read)

Run validator_001 tests (CustomerServiceTest) to verify implementation.
Run validator_008 tests (DataObservabilityTest) to verify data quality.
Run validator_009 tests (PerformanceComparisonTest) to verify performance.
Run validator_010 tests (CustomerControllerTest) to verify REST API.

Coverage targets:
- Service: 80%
- Controller: 60%""",
            "definition_of_done": "CustomerService.inquireCustomer() and CustomerController.getCustomer() implemented, validator_001/008/009/010 tests pass for inquiry operations, coverage meets targets",
            "validation_mechanism": "Run `mvn test -Dtest=CustomerServiceTest` and verify inquiry test cases pass, run `mvn verify` and confirm service coverage ≥80% and controller coverage ≥60%",
            "estimated_hours": 8,
            "deliverables": [
                "src/main/java/com/cbsa/migration/service/CustomerService.java",
                "src/main/java/com/cbsa/migration/controller/CustomerController.java",
                "src/main/java/com/cbsa/migration/dto/CustomerRequestDto.java",
                "src/main/java/com/cbsa/migration/dto/CustomerResponseDto.java"
            ]
        },
        
        {
            "id": "migrate_002",
            "title": "Migrate CRECUST - Customer Create Service with Async Credit Checks",
            "content": "Migrate CRECUST.cbl to CustomerService.createCustomer() method. Complex implementation: gets next customer number from control table, calls 5 credit agencies asynchronously with 3-second timeout, aggregates scores, creates customer record, logs transaction. Uses existing JdbcCustomerRepository and ControlRepository.",
            "depends_on": ["migrate_001", "validator_001", "validator_004", "validator_005", "validator_008", "validator_009"],
            "prompt": """Migrate CRECUST.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/CRECUST.cbl (complex - 1440 lines)
Target repo: target-springboot-cics

Key COBOL Logic (lines 9-34):
- Get next customer number from Named Counter
- Call multiple credit agencies asynchronously (CRDTAGY1-5)
- Wait 3 seconds for responses
- Aggregate and average credit scores
- If no responses: set score=0, review date=today
- Save customer record
- Log transaction to PROCTRAN
- If any failure: rollback and restore counter

Implementation in CustomerService.createCustomer():
1. Use ControlRepository.getNextCustomerNumber() for ID
2. Validate input (date_of_birth: year≥1601, age≤150, not future)
3. Call credit agencies in parallel using CompletableFuture:
   - creditAgency1.processCredit() through creditAgency5.processCredit()
   - 3-second timeout
   - Aggregate returned scores
   - Average if multiple responses
   - Set score=0 if all timeout
4. Set eye_catcher='CUST'
5. Use JdbcCustomerRepository.save()
6. Log transaction to JdbcTransactionRepository
7. @Transactional for atomicity

CustomerController.createCustomer():
- POST /api/customers
- Request body: CustomerCreateRequestDto
- Returns 201 + customer JSON + Location header
- Returns 400 for validation errors
- Returns 500 on error

Follow patterns from:
- CreditAgencyService.java - existing credit check pattern
- ControlRepository.getNextCustomerNumber() - counter management
- CRECUST.cbl lines 9-34 - async credit check logic

Run all validator tests:
- validator_001 (CustomerServiceTest - create tests)
- validator_004 (CreditAgencyExtensionTest)
- validator_005 (AsyncCreditCheckTest - CRITICAL for this migration)
- validator_008 (DataObservabilityTest)
- validator_009 (PerformanceComparisonTest)

This is a COMPLEX migration - estimated 12-16 hours due to async logic.""",
            "definition_of_done": "CustomerService.createCustomer() implemented with async credit checks, validator_001/004/005/008/009 tests pass for create operations, coverage ≥80%",
            "validation_mechanism": "Run `mvn test -Dtest=AsyncCreditCheckTest` to verify async logic, run `mvn test -Dtest=CustomerServiceTest` to verify create tests pass, confirm 3-second timeout works",
            "estimated_hours": 16,
            "deliverables": [
                "CustomerService.java (updated with createCustomer method)",
                "CustomerController.java (updated with POST endpoint)",
                "CustomerCreateRequestDto.java"
            ]
        },
        
        {
            "id": "migrate_003",
            "title": "Migrate UPDCUST - Customer Update Service",
            "content": "Migrate UPDCUST.cbl to CustomerService.updateCustomer() method. Updates customer details (name, address allowed; balance/dates restricted per COBOL). Uses existing JdbcCustomerRepository.save(). Creates PUT endpoint in CustomerController.",
            "depends_on": ["migrate_001", "validator_001", "validator_008", "validator_009", "validator_010"],
            "prompt": """Migrate UPDCUST.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/UPDCUST.cbl (365 lines)
Target repo: target-springboot-cics

Key COBOL Logic (lines 9-23):
- Receives all customer fields
- Only limited fields can be changed (name, address)
- Cannot change: sort_code, customer_number, date_of_birth, credit_score (controlled separately)
- No PROCTRAN record needed (no financial impact)
- Presentation layer validates fields

Implementation in CustomerService.updateCustomer():
1. Find existing customer using JdbcCustomerRepository.findById()
2. Return 404 if not found
3. Validate which fields can be updated:
   - Allowed: name, address
   - Read-only: sort_code, customer_number, date_of_birth, credit_score, credit_score_review_date
4. Update allowed fields
5. Use JdbcCustomerRepository.save()
6. No transaction logging needed per COBOL

CustomerController.updateCustomer():
- PUT /api/customers/{sortCode}/{customerNumber}
- Request body: CustomerUpdateRequestDto
- Returns 200 + updated customer JSON
- Returns 404 if not found
- Returns 400 for validation errors

Follow patterns from:
- JdbcCustomerRepository.save() - upsert logic
- UPDCUST.cbl lines 9-23 - field restrictions
- CreditAgencyService.updateCustomerCreditScore() - update pattern

Run validator tests:
- validator_001 (CustomerServiceTest - update tests)
- validator_008 (DataObservabilityTest)
- validator_009 (PerformanceComparisonTest)
- validator_010 (CustomerControllerTest - PUT tests)""",
            "definition_of_done": "CustomerService.updateCustomer() implemented with field restrictions, validator_001/008/009/010 tests pass for update operations, coverage ≥80%",
            "validation_mechanism": "Run `mvn test -Dtest=CustomerServiceTest` and verify update test cases pass, verify field restrictions enforced (read-only fields rejected)",
            "estimated_hours": 8,
            "deliverables": [
                "CustomerService.java (updated with updateCustomer method)",
                "CustomerController.java (updated with PUT endpoint)",
                "CustomerUpdateRequestDto.java"
            ]
        },
        
        {
            "id": "migrate_004",
            "title": "Migrate DELCUS - Customer Delete Service with Cascade",
            "content": "Migrate DELCUS.cbl to CustomerService.deleteCustomer() method. Complex implementation: retrieves all customer accounts, deletes each account with transaction logging, then deletes customer with transaction logging. Full rollback on any failure. Uses existing repositories.",
            "depends_on": ["migrate_001", "migrate_006", "migrate_007", "migrate_008", "migrate_009", "validator_001", "validator_006", "validator_008"],
            "prompt": """Migrate DELCUS.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/DELCUS.cbl (762 lines)
Target repo: target-springboot-cics

Key COBOL Logic (lines 10-24):
- Takes customer number
- Retrieves all associated accounts (stores in array)
- Deletes accounts one at a time
- Writes PROCTRAN delete record for each account
- After all accounts deleted, deletes customer
- Writes PROCTRAN delete record for customer
- If failure after deletions start: ABEND (to rollback)
- Exception: if account already deleted, continue (don't fail)

Implementation in CustomerService.deleteCustomer():
1. Find customer using JdbcCustomerRepository.findById()
2. Return 404 if not found
3. Find all accounts using JdbcAccountRepository.findByCustomerNumber()
4. @Transactional for atomicity:
   a. For each account:
      - Delete using JdbcAccountRepository.deleteById()
      - Log transaction using JdbcTransactionRepository.save()
      - If account not found: continue (per COBOL logic)
   b. Delete customer using JdbcCustomerRepository.deleteById()
   c. Log customer transaction using JdbcTransactionRepository.save()
   d. If any operation fails: automatic rollback via @Transactional

CustomerController.deleteCustomer():
- DELETE /api/customers/{sortCode}/{customerNumber}
- Returns 204 No Content on success
- Returns 404 if customer not found
- Returns 500 on error

Follow patterns from:
- DELCUS.cbl lines 10-24 - cascade delete logic
- @Transactional annotation - automatic rollback
- JdbcAccountRepository.findByCustomerNumber() - find accounts
- JdbcAccountRepository.deleteById() - delete each account
- JdbcCustomerRepository.deleteById() - delete customer
- JdbcTransactionRepository.save() - log deletions

Run validator tests:
- validator_001 (CustomerServiceTest - delete tests)
- validator_006 (CascadeDeleteTest - CRITICAL for this migration)
- validator_008 (DataObservabilityTest - verify referential integrity)

This is a COMPLEX migration due to cascade logic - estimated 12 hours.""",
            "definition_of_done": "CustomerService.deleteCustomer() implemented with cascade delete, validator_001/006/008 tests pass, rollback verified on failure",
            "validation_mechanism": "Run `mvn test -Dtest=CascadeDeleteTest` to verify cascade logic, verify transaction rollback on partial failure, verify all accounts deleted before customer",
            "estimated_hours": 12,
            "deliverables": [
                "CustomerService.java (updated with deleteCustomer method)",
                "CustomerController.java (updated with DELETE endpoint)"
            ]
        },
        
        {
            "id": "migrate_005",
            "title": "Migrate INQACC - Account Inquiry Service",
            "content": "Migrate INQACC.cbl to AccountService.inquireAccount() method. Implements account read/inquiry by sort code and account number. Uses existing JdbcAccountRepository.findById(). Creates AccountController.getAccount() REST endpoint.",
            "depends_on": ["setup_001", "setup_002", "setup_003", "validator_002", "validator_008", "validator_009", "validator_010"],
            "prompt": """Migrate INQACC.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/INQACC.cbl (1003 lines - but logic is simple read)
Target repo: target-springboot-cics

Key COBOL Logic (lines 9-14):
- Takes account number and account type
- Accesses DB2 datastore
- Retrieves account record matching account number and type
- Returns account data or abends on error

Implementation:
1. Create src/main/java/com/cbsa/migration/service/AccountService.java
2. Create src/main/java/com/cbsa/migration/controller/AccountController.java
3. Create DTOs: AccountRequestDto.java, AccountResponseDto.java

AccountService.inquireAccount():
- Use JdbcAccountRepository.findById(sortCode, accountNumber)
- Return account if found
- Return null/empty if not found
- Handle errors with ErrorLoggingService

AccountController.getAccount():
- GET /api/accounts/{sortCode}/{accountNumber}
- Returns 200 + account JSON on success
- Returns 404 if not found
- Returns 500 on error

Follow patterns from:
- CustomerService.inquireCustomer() - similar read pattern
- JdbcAccountRepository.java - repository methods
- INQACC.cbl - simple read logic

Run validator tests:
- validator_002 (AccountServiceTest - inquiry tests)
- validator_008 (DataObservabilityTest)
- validator_009 (PerformanceComparisonTest)
- validator_010 (AccountControllerTest)

Coverage targets:
- Service: 80%
- Controller: 60%""",
            "definition_of_done": "AccountService.inquireAccount() and AccountController.getAccount() implemented, validator_002/008/009/010 tests pass for inquiry operations, coverage meets targets",
            "validation_mechanism": "Run `mvn test -Dtest=AccountServiceTest` and verify inquiry test cases pass, run `mvn verify` and confirm coverage targets met",
            "estimated_hours": 6,
            "deliverables": [
                "src/main/java/com/cbsa/migration/service/AccountService.java",
                "src/main/java/com/cbsa/migration/controller/AccountController.java",
                "src/main/java/com/cbsa/migration/dto/AccountRequestDto.java",
                "src/main/java/com/cbsa/migration/dto/AccountResponseDto.java"
            ]
        },
        
        {
            "id": "migrate_006",
            "title": "Migrate INQACCCU - Account Inquiry by Customer Service",
            "content": "Migrate INQACCCU.cbl to AccountService.inquireAccountsByCustomer() method. Retrieves all accounts for a given customer number. Uses existing JdbcAccountRepository.findByCustomerNumber(). Creates GET endpoint returning list of accounts.",
            "depends_on": ["migrate_005", "validator_002", "validator_008", "validator_009", "validator_010"],
            "prompt": """Migrate INQACCCU.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/INQACCCU.cbl (883 lines)
Target repo: target-springboot-cics

Key COBOL Logic (lines 9-12):
- Takes customer number
- Determines which accounts associated with customer
- Accesses datastore and retrieves all account records matching customer number

Implementation in AccountService.inquireAccountsByCustomer():
- Use JdbcAccountRepository.findByCustomerNumber(customerNumber)
- Return list of accounts (may be empty list)
- Handle errors with ErrorLoggingService

AccountController.getAccountsByCustomer():
- GET /api/accounts/customer/{customerNumber}
- Returns 200 + JSON array of accounts
- Returns empty array if no accounts
- Returns 500 on error

Follow patterns from:
- JdbcAccountRepository.findByCustomerNumber() - existing method
- INQACCCU.cbl - customer account lookup
- AccountService.inquireAccount() - similar read pattern

Run validator tests:
- validator_002 (AccountServiceTest - inquiry by customer tests)
- validator_008 (DataObservabilityTest)
- validator_009 (PerformanceComparisonTest)
- validator_010 (AccountControllerTest)""",
            "definition_of_done": "AccountService.inquireAccountsByCustomer() implemented, validator_002/008/009/010 tests pass, coverage ≥80%",
            "validation_mechanism": "Run `mvn test -Dtest=AccountServiceTest` and verify customer inquiry test cases pass, test with customer having 0, 1, and multiple accounts",
            "estimated_hours": 6,
            "deliverables": [
                "AccountService.java (updated with inquireAccountsByCustomer method)",
                "AccountController.java (updated with GET by customer endpoint)"
            ]
        },
        
        {
            "id": "migrate_007",
            "title": "Migrate CREACC - Account Create Service with Counter Management",
            "content": "Migrate CREACC.cbl to AccountService.createAccount() method. Gets next account number from control table, creates account record, logs transaction to PROCTRAN. Uses existing JdbcAccountRepository and ControlRepository.",
            "depends_on": ["migrate_005", "validator_002", "validator_008", "validator_009"],
            "prompt": """Migrate CREACC.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/CREACC.cbl (1248 lines)
Target repo: target-springboot-cics

Key COBOL Logic (lines 9-19):
- Takes account information from BMS application
- Enqueues Named Counter for ACCOUNT
- Increments counter and takes new account number
- Updates ACCOUNT datastore on DB2
- If successful, writes record to PROCTRAN datastore
- If successful, DEQUEUEs named counter and returns account number
- If ACCOUNT or PROCTRAN write fails: decrement counter, DEQUEUE, fail

Implementation in AccountService.createAccount():
1. Validate customer exists using CustomerRepository.findById()
2. Use ControlRepository.getNextAccountNumber() for ID
3. Set eye_catcher='ACCT'
4. Set opened_date=today
5. Calculate next_statement_date based on opened_date
6. @Transactional for atomicity:
   - Use JdbcAccountRepository.save()
   - Log transaction using JdbcTransactionRepository.save()
   - If failure: automatic rollback via @Transactional (counter handled by DB)

AccountController.createAccount():
- POST /api/accounts
- Request body: AccountCreateRequestDto
- Returns 201 + account JSON + Location header
- Returns 400 for validation errors
- Returns 404 if customer not found
- Returns 500 on error

Follow patterns from:
- CustomerService.createCustomer() - similar counter pattern
- ControlRepository.getNextAccountNumber() - counter management
- CREACC.cbl lines 9-19 - counter and transaction logic
- JdbcAccountRepository.save() - account creation

Run validator tests:
- validator_002 (AccountServiceTest - create tests including counter)
- validator_008 (DataObservabilityTest)
- validator_009 (PerformanceComparisonTest)

Complexity: Counter management and transaction logging - estimated 10 hours.""",
            "definition_of_done": "AccountService.createAccount() implemented with counter management, validator_002/008/009 tests pass, coverage ≥80%",
            "validation_mechanism": "Run `mvn test -Dtest=AccountServiceTest` and verify create tests pass including counter increment, verify rollback works on failure",
            "estimated_hours": 10,
            "deliverables": [
                "AccountService.java (updated with createAccount method)",
                "AccountController.java (updated with POST endpoint)",
                "AccountCreateRequestDto.java"
            ]
        },
        
        {
            "id": "migrate_008",
            "title": "Migrate UPDACC - Account Update Service",
            "content": "Migrate UPDACC.cbl to AccountService.updateAccount() method. Updates account details (type, interest rate, overdraft limit, statement dates allowed; balance NOT allowed per COBOL). Uses existing JdbcAccountRepository.save(). Creates PUT endpoint.",
            "depends_on": ["migrate_005", "validator_002", "validator_008", "validator_009", "validator_010"],
            "prompt": """Migrate UPDACC.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/UPDACC.cbl (407 lines)
Target repo: target-springboot-cics

Key COBOL Logic (lines 10-31):
- Updates account details (excludes balance which must be updated by crediting/debiting)
- Receives all account fields
- Only limited fields can be changed:
  * Allowed: account_type, interest_rate, overdraft_limit, last_statement_date, next_statement_date
  * Read-only: sort_code, account_number, customer_number, available_balance, actual_balance, opened_date
- No PROCTRAN record needed (balance not amended)
- Presentation layer validates fields

Implementation in AccountService.updateAccount():
1. Find existing account using JdbcAccountRepository.findById()
2. Return 404 if not found
3. Validate which fields can be updated:
   - Allowed: account_type, interest_rate, overdraft_limit, last_statement_date, next_statement_date
   - Read-only: sort_code, account_number, customer_number, opened_date, available_balance, actual_balance
4. Update allowed fields
5. Use JdbcAccountRepository.save()
6. No transaction logging needed per COBOL

AccountController.updateAccount():
- PUT /api/accounts/{sortCode}/{accountNumber}
- Request body: AccountUpdateRequestDto
- Returns 200 + updated account JSON
- Returns 404 if not found
- Returns 400 for validation errors (especially if trying to change balance)

Follow patterns from:
- CustomerService.updateCustomer() - similar field restriction pattern
- UPDACC.cbl lines 10-31 - field restrictions
- JdbcAccountRepository.save() - update logic

Run validator tests:
- validator_002 (AccountServiceTest - update tests)
- validator_008 (DataObservabilityTest)
- validator_009 (PerformanceComparisonTest)
- validator_010 (AccountControllerTest)""",
            "definition_of_done": "AccountService.updateAccount() implemented with field restrictions, validator_002/008/009/010 tests pass, coverage ≥80%",
            "validation_mechanism": "Run `mvn test -Dtest=AccountServiceTest` and verify update test cases pass, verify balance updates are rejected",
            "estimated_hours": 8,
            "deliverables": [
                "AccountService.java (updated with updateAccount method)",
                "AccountController.java (updated with PUT endpoint)",
                "AccountUpdateRequestDto.java"
            ]
        },
        
        {
            "id": "migrate_009",
            "title": "Migrate DELACC - Account Delete Service",
            "content": "Migrate DELACC.cbl to AccountService.deleteAccount() method. Deletes account and logs transaction to PROCTRAN. Uses existing JdbcAccountRepository.deleteById() and JdbcTransactionRepository.save().",
            "depends_on": ["migrate_005", "validator_002", "validator_008", "validator_009"],
            "prompt": """Migrate DELACC.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/DELACC.cbl (650 lines)
Target repo: target-springboot-cics

Key COBOL Logic (lines 9-18):
- Takes account number
- Accesses account datastore
- Retrieves account record matching customer number and account type
- Deletes it
- If no matching account found: returns error flag
- Incoming customer number assumed valid

Implementation in AccountService.deleteAccount():
1. Find account using JdbcAccountRepository.findById()
2. Return 404 if not found
3. @Transactional for atomicity:
   - Delete using JdbcAccountRepository.deleteById()
   - Log transaction using JdbcTransactionRepository.save()
   - If failure: automatic rollback

AccountController.deleteAccount():
- DELETE /api/accounts/{sortCode}/{accountNumber}
- Returns 204 No Content on success
- Returns 404 if account not found
- Returns 500 on error

Follow patterns from:
- DELACC.cbl lines 9-18 - delete logic
- @Transactional annotation - automatic rollback
- JdbcAccountRepository.deleteById() - delete account
- JdbcTransactionRepository.save() - log deletion

Run validator tests:
- validator_002 (AccountServiceTest - delete tests)
- validator_008 (DataObservabilityTest)
- validator_009 (PerformanceComparisonTest)""",
            "definition_of_done": "AccountService.deleteAccount() implemented with transaction logging, validator_002/008/009 tests pass, coverage ≥80%",
            "validation_mechanism": "Run `mvn test -Dtest=AccountServiceTest` and verify delete test cases pass, verify transaction logged",
            "estimated_hours": 8,
            "deliverables": [
                "AccountService.java (updated with deleteAccount method)",
                "AccountController.java (updated with DELETE endpoint)"
            ]
        },
        
        {
            "id": "migrate_010",
            "title": "Migrate XFRFUN - Transfer Funds Service with Dual-Account Atomicity",
            "content": "Migrate XFRFUN.cbl to TransactionService.transferFunds() method. Complex implementation: updates TWO accounts (debit source, credit target) atomically with full rollback on any failure. Logs TWO transaction records. Uses existing repositories.",
            "depends_on": ["migrate_005", "migrate_008", "validator_003", "validator_007", "validator_008", "validator_009"],
            "prompt": """Migrate XFRFUN.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/XFRFUN.cbl (1925 lines - COMPLEX)
Target repo: target-springboot-cics

Key COBOL Logic (lines 11-37):
- Takes source account (sortcode/account number) where money transferred FROM
- Takes target account (sortcode/account number) where money transferred TO
- Takes amount
- No checking on overdraft limits
- If transfer successful:
  * Updates both account balances (available and actual)
  * Updates PROCTRAN datastore with transaction records
  * Returns both updated balances
- If ACCOUNT or PROCTRAN update unsuccessful: transaction fails
- If datastore failure: back out updates and abend transaction

Implementation:
1. Create src/main/java/com/cbsa/migration/service/TransactionService.java
2. Create src/main/java/com/cbsa/migration/controller/TransactionController.java

TransactionService.transferFunds():
1. Validate source account exists using JdbcAccountRepository.findById()
2. Validate target account exists
3. @Transactional for atomicity:
   a. Update source account:
      - Debit available_balance and actual_balance by amount
      - Use JdbcAccountRepository.save()
   b. Update target account:
      - Credit available_balance and actual_balance by amount
      - Use JdbcAccountRepository.save()
   c. Log source transaction (type='TFR', amount negative):
      - Set target_sort_code and target_account_number
      - Use JdbcTransactionRepository.save()
   d. Log target transaction (type='TFR', amount positive):
      - Set target info pointing back to source
      - Use JdbcTransactionRepository.save()
   e. If ANY step fails: automatic rollback via @Transactional

TransactionController.transferFunds():
- POST /api/transactions/transfer
- Request body: TransferRequestDto (source account, target account, amount)
- Returns 200 + transfer result JSON (both balances)
- Returns 404 if either account not found
- Returns 400 for validation errors
- Returns 500 on error

Follow patterns from:
- XFRFUN.cbl lines 11-37 - dual account update logic
- @Transactional annotation - CRITICAL for atomicity
- JdbcAccountRepository.save() - account updates
- JdbcTransactionRepository.save() - transaction logging
- Transaction.java - transfer type fields (target_sort_code, target_account_number)

Run validator tests:
- validator_003 (TransactionServiceTest - transfer tests)
- validator_007 (TransactionAtomicityTest - CRITICAL for this migration)
- validator_008 (DataObservabilityTest)
- validator_009 (PerformanceComparisonTest)

This is a COMPLEX migration due to dual-account atomicity - estimated 14 hours.""",
            "definition_of_done": "TransactionService.transferFunds() implemented with atomic dual-account updates, validator_003/007/008/009 tests pass, rollback verified on failure",
            "validation_mechanism": "Run `mvn test -Dtest=TransactionAtomicityTest` to verify atomicity, verify rollback at each failure point (source update, target update, transaction log), confirm no partial updates occur",
            "estimated_hours": 14,
            "deliverables": [
                "src/main/java/com/cbsa/migration/service/TransactionService.java",
                "src/main/java/com/cbsa/migration/controller/TransactionController.java",
                "src/main/java/com/cbsa/migration/dto/TransferRequestDto.java",
                "src/main/java/com/cbsa/migration/dto/TransferResponseDto.java"
            ]
        },
        
        {
            "id": "migrate_011",
            "title": "Migrate DBCRFUN - Debit/Credit Funds Service",
            "content": "Migrate DBCRFUN.cbl to TransactionService.debitCreditFunds() method. Updates single account balance (debit or credit), logs transaction to PROCTRAN. Simpler than XFRFUN (single account). Uses existing repositories.",
            "depends_on": ["migrate_005", "migrate_008", "validator_003", "validator_008", "validator_009"],
            "prompt": """Migrate DBCRFUN.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/DBCRFUN.cbl (862 lines)
Target repo: target-springboot-cics

Key COBOL Logic (lines 10-28):
- Gets called when someone pays cash in (credit) or takes cash out (debit)
- Takes account number and amount
- Accesses DB2 datastore
- Retrieves account record
- Applies amount (credit positive, debit negative)
- Returns updated actual amount and available balance
- If update successful: writes record to PROCTRAN
- If transaction cannot be applied (account doesn't exist): sets fail flag

Implementation in TransactionService.debitCreditFunds():
1. Find account using JdbcAccountRepository.findById()
2. Return 404 if not found
3. Determine transaction type (CRE for credit/deposit, DEB for debit/withdrawal)
4. @Transactional for atomicity:
   a. Update account:
      - For credit: add amount to available_balance and actual_balance
      - For debit: subtract amount from available_balance and actual_balance
      - Use JdbcAccountRepository.save()
   b. Log transaction:
      - Set transaction_type ('CRE' or 'DEB')
      - Set amount (positive for credit, negative for debit)
      - Use JdbcTransactionRepository.save()
   c. If failure: automatic rollback

TransactionController.debitCreditFunds():
- POST /api/transactions/debit-credit
- Request body: DebitCreditRequestDto (account, amount, type)
- Returns 200 + updated account JSON
- Returns 404 if account not found
- Returns 400 for validation errors
- Returns 500 on error

Follow patterns from:
- DBCRFUN.cbl lines 10-28 - single account update logic
- TransactionService.transferFunds() - similar pattern but simpler (one account)
- @Transactional annotation - atomicity
- JdbcAccountRepository.save() - account update
- JdbcTransactionRepository.save() - transaction logging
- Transaction.TYPE_CREDIT and Transaction.TYPE_DEBIT constants

Run validator tests:
- validator_003 (TransactionServiceTest - debit/credit tests)
- validator_008 (DataObservabilityTest)
- validator_009 (PerformanceComparisonTest)""",
            "definition_of_done": "TransactionService.debitCreditFunds() implemented, validator_003/008/009 tests pass, coverage ≥80%",
            "validation_mechanism": "Run `mvn test -Dtest=TransactionServiceTest` and verify debit/credit test cases pass, test both credit and debit operations",
            "estimated_hours": 10,
            "deliverables": [
                "TransactionService.java (updated with debitCreditFunds method)",
                "TransactionController.java (updated with POST debit-credit endpoint)",
                "DebitCreditRequestDto.java",
                "DebitCreditResponseDto.java"
            ]
        },
        
        {
            "id": "migrate_012",
            "title": "Migrate CRDTAGY2 - Credit Agency 2 Service Extension",
            "content": "Extend or create variant of CreditAgencyService for CRDTAGY2. Identical logic to CRDTAGY1 (delay 0-3 seconds, generate score 1-999, update customer). Can reuse existing CreditAgencyService or create CreditAgency2Service variant.",
            "depends_on": ["validator_004"],
            "prompt": """Migrate CRDTAGY2.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/CRDTAGY2.cbl (274 lines - identical structure to CRDTAGY1)
Target repo: target-springboot-cics

CRDTAGY2 is IDENTICAL to CRDTAGY1 in structure - it simulates a second external credit agency.

From CRDTAGY2.cbl analysis:
- Lines 8-19: Identical description to CRDTAGY1
- Random delay 0-3 seconds (lines 123-124)
- Random credit score 1-999 (lines 215-216)
- Updates customer via container PUT

Implementation Options:
**Option A (Simpler):** Reuse existing CreditAgencyService as-is since logic identical
**Option B (Extensible):** Create CreditAgency2Service extending CreditAgencyService

If Option B:
1. Create CreditAgency2Service extends CreditAgencyService
2. Reuse all methods (processCredit, generateCreditScore, simulateProcessingDelay)
3. Create CreditAgency2Controller similar to CreditAgencyController
4. POST /api/credit-agency-2/score

Follow patterns from:
- CreditAgencyService.java - EXACT same logic
- CRDTAGY2.cbl - identical to CRDTAGY1

Run validator tests:
- validator_004 (CreditAgencyExtensionTest - agency 2 tests)

Since structure identical to CRDTAGY1, this is low complexity - estimated 4 hours.""",
            "definition_of_done": "CRDTAGY2 functionality available (via reused service or new variant), validator_004 tests pass for agency 2",
            "validation_mechanism": "Run `mvn test -Dtest=CreditAgencyExtensionTest` and verify agency 2 test cases pass",
            "estimated_hours": 4,
            "deliverables": [
                "CreditAgency2Service.java (if creating variant) OR documentation that CreditAgencyService handles all agencies"
            ]
        },
        
        {
            "id": "migrate_013",
            "title": "Migrate CRDTAGY3 - Credit Agency 3 Service Extension",
            "content": "Extend or create variant of CreditAgencyService for CRDTAGY3. Identical logic to CRDTAGY1/CRDTAGY2. Third credit agency simulator.",
            "depends_on": ["validator_004"],
            "prompt": """Migrate CRDTAGY3.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/CRDTAGY3.cbl
Target repo: target-springboot-cics

CRDTAGY3 is identical to CRDTAGY1 and CRDTAGY2 - third credit agency simulator.

Follow same approach as migrate_012 (CRDTAGY2):
- Reuse CreditAgencyService OR
- Create CreditAgency3Service variant

Run validator tests:
- validator_004 (CreditAgencyExtensionTest - agency 3 tests)

Estimated 4 hours (identical to CRDTAGY1/2).""",
            "definition_of_done": "CRDTAGY3 functionality available, validator_004 tests pass for agency 3",
            "validation_mechanism": "Run `mvn test -Dtest=CreditAgencyExtensionTest` and verify agency 3 test cases pass",
            "estimated_hours": 4,
            "deliverables": [
                "CreditAgency3Service.java (if creating variant)"
            ]
        },
        
        {
            "id": "migrate_014",
            "title": "Migrate CRDTAGY4 - Credit Agency 4 Service Extension",
            "content": "Extend or create variant of CreditAgencyService for CRDTAGY4. Identical logic to CRDTAGY1/2/3. Fourth credit agency simulator.",
            "depends_on": ["validator_004"],
            "prompt": """Migrate CRDTAGY4.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/CRDTAGY4.cbl
Target repo: target-springboot-cics

CRDTAGY4 is identical to CRDTAGY1/2/3 - fourth credit agency simulator.

Follow same approach as migrate_012/013.

Run validator tests:
- validator_004 (CreditAgencyExtensionTest - agency 4 tests)

Estimated 4 hours.""",
            "definition_of_done": "CRDTAGY4 functionality available, validator_004 tests pass for agency 4",
            "validation_mechanism": "Run `mvn test -Dtest=CreditAgencyExtensionTest` and verify agency 4 test cases pass",
            "estimated_hours": 4,
            "deliverables": [
                "CreditAgency4Service.java (if creating variant)"
            ]
        },
        
        {
            "id": "migrate_015",
            "title": "Migrate CRDTAGY5 - Credit Agency 5 Service Extension",
            "content": "Extend or create variant of CreditAgencyService for CRDTAGY5. Identical logic to CRDTAGY1/2/3/4. Fifth and final credit agency simulator.",
            "depends_on": ["validator_004"],
            "prompt": """Migrate CRDTAGY5.cbl to Java in target-springboot-cics.

Source: og-cics-cobol-app/src/base/cobol_src/CRDTAGY5.cbl
Target repo: target-springboot-cics

CRDTAGY5 is identical to CRDTAGY1/2/3/4 - fifth and final credit agency simulator.

Follow same approach as migrate_012/013/014.

Run validator tests:
- validator_004 (CreditAgencyExtensionTest - agency 5 tests)

Estimated 4 hours.""",
            "definition_of_done": "CRDTAGY5 functionality available, validator_004 tests pass for agency 5, all 5 credit agencies ready for CRECUST async calls",
            "validation_mechanism": "Run `mvn test -Dtest=CreditAgencyExtensionTest` and verify agency 5 test cases pass, run full test suite to confirm all agencies work",
            "estimated_hours": 4,
            "deliverables": [
                "CreditAgency5Service.java (if creating variant)"
            ]
        }
    ]
}
