# Migration Task Graph

This document explains the task graph structure in `migration_plan.py`.

## Overview

The task graph defines 23 tasks to migrate 14 remaining COBOL programs:
- **Customer Operations**: INQCUST, CRECUST, UPDCUST, DELCUS (4 programs)
- **Account Operations**: INQACC, INQACCCU, CREACC, UPDACC, DELACC (5 programs)
- **Transaction Operations**: XFRFUN, DBCRFUN (2 programs)
- **Credit Agency**: CRDTAGY2, CRDTAGY3, CRDTAGY4, CRDTAGY5 (4 programs)

Note: 10 BMS UI screen programs (BNK1*, BNKMENU) are intentionally excluded as they're replaced by REST API controllers rather than migrated 1-to-1.

## Already Migrated (5/29 Programs)

The following programs have already been migrated to Spring Boot:
1. **GETCOMPY** → CompanyInfoService (utility service)
2. **GETSCODE** → SortCodeService (utility service)
3. **ABNDPROC** → ErrorLoggingService (error handling)
4. **CRDTAGY1** → CreditAgencyService (credit scoring)
5. **BANKDATA** → BankDataGenerator (test data generation)

## Task Categories

### Setup Tasks (3 tasks, 24 hours)
Foundation tasks with no dependencies:
- **setup_001**: Performance baseline establishment for all unmigrated COBOL programs
- **setup_002**: Monitoring infrastructure (Micrometer metrics, JaCoCo automation)
- **setup_003**: Testing documentation extending TESTING.md

### Validator Tasks (7 tasks, 74 hours)
Test suites created BEFORE migration:
- **validator_001**: Customer read operations test suite (INQCUST)
- **validator_002**: Customer write operations test suite (CRECUST, UPDCUST, DELCUS)
- **validator_003**: Account read operations test suite (INQACC, INQACCCU)
- **validator_004**: Account write operations test suite (CREACC, UPDACC, DELACC)
- **validator_005**: Transaction operations test suite (XFRFUN, DBCRFUN)
- **validator_006**: Credit agency service test suite (CRDTAGY2-5)
- **validator_007**: End-to-end integration test suite (post-migration)
- **validator_008**: Performance validation and regression testing
- **validator_009**: Schema synchronization and data integrity validation

### Migration Tasks (6 tasks, 62 hours)
Actual COBOL-to-Java migration:
- **migrate_001**: Customer inquiry operations (INQCUST)
- **migrate_002**: Customer write operations (CRECUST, UPDCUST, DELCUS)
- **migrate_003**: Account inquiry operations (INQACC, INQACCCU)
- **migrate_004**: Account write operations (CREACC, UPDACC, DELACC)
- **migrate_005**: Transaction operations (XFRFUN, DBCRFUN)
- **migrate_006**: Additional credit agency services (CRDTAGY2-5)

## Dependency Rules

1. **All migration tasks depend on their validator tasks**: Tests must exist before code
2. **Setup tasks have no dependencies**: They establish the foundation
3. **Write operations depend on read operations**: Customer/account write depends on read being implemented first
4. **Account operations depend on customer operations**: Due to foreign key relationships
5. **Transaction operations depend on account operations**: Transactions operate on accounts
6. **Post-migration validators depend on completed migrations**: E2E tests require all services

## Task Dependency Graph

```
setup_001 (baseline) ────────────┬─────────────────┐
                                  │                 │
setup_002 (monitoring) ──┬───────┼─────────────────┤
                          │       │                 │
setup_003 (docs) ─────┬──┴───────┴────────┐        │
                      │                    │        │
                      ├──> validator_001   │        │
                      │    (cust read)     │        │
                      │         │          │        │
                      │         v          │        │
                      │    validator_002   │        │
                      │    (cust write)    │        │
                      │                    │        │
                      ├──> validator_003   │        │
                      │    (acct read)     │        │
                      │         │          │        │
                      │         v          │        │
                      │    validator_004   │        │
                      │    (acct write)    │        │
                      │         │          │        │
                      │         v          │        │
                      │    validator_005   │        │
                      │    (transaction)   │        │
                      │                    │        │
                      └──> validator_006   │        │
                           (credit)        │        │
                                           │        │
                      ┌────────────────────┘        │
                      │                             │
                      v                             │
migrate_001 ──────> migrate_002 ─────────┐         │
(cust read)         (cust write)         │         │
     │                   │                │         │
     │                   v                │         │
     └─────────> migrate_003 ────> migrate_004     │
                 (acct read)       (acct write)     │
                      │                 │           │
                      └────────┬────────┘           │
                               │                    │
                               v                    │
                          migrate_005               │
                          (transaction)             │
                                                    │
migrate_006 ────────────────────────────────────────┘
(credit agencies)
     │
     │
     v
validator_007 (E2E tests)
validator_008 (performance)
validator_009 (schema/data)
```

## Execution Order

The graph ensures correct execution order:
1. **Phase 1 - Setup** (24 hours): Setup tasks can run in parallel
2. **Phase 2 - Validators** (64 hours for pre-migration): Can start after setup_003
   - validator_001-006 can run largely in parallel (independent domains)
3. **Phase 3 - Migration** (62 hours): Blocked on validators
   - Must follow dependency chain (customer → account → transaction)
4. **Phase 4 - Post-Migration Validation** (24 hours): validator_007-009
   - E2E tests, performance validation, schema checks

## Coverage Requirements

Following TESTING.md guidelines:
- Service layer: 80% instructions, 70% branches
- Repository layer: 70% instructions
- Controller layer: 60% instructions
- Model layer: 50% instructions
- DTO layer: 40% instructions
- Overall minimum: 50% instructions

## COBOL Program Details

### Customer Operations
- **INQCUST**: Customer inquiry by sort code + customer number (VSAM read)
- **CRECUST**: Create customer with credit agency integration (VSAM write + credit check)
- **UPDCUST**: Update customer fields with restrictions (only certain fields modifiable)
- **DELCUS**: Delete customer with cascade checks

### Account Operations
- **INQACC**: Single account inquiry by sort code + account number (DB2 read)
- **INQACCCU**: Browse all accounts for a customer (DB2 read with filter)
- **CREACC**: Create account with customer validation and number generation
- **UPDACC**: Update account fields with restrictions
- **DELACC**: Delete account with transaction history checks

### Transaction Operations
- **XFRFUN**: Transfer funds between accounts with balance validation
- **DBCRFUN**: Debit/credit operations (deposits/withdrawals) with PROCTRAN logging

### Credit Agency Operations
- **CRDTAGY2-5**: Credit scoring services (extend CRDTAGY1 pattern)
  - Parallel async calls
  - Score aggregation/averaging
  - 3-second timeout handling
  - Fallback to zero if no responses

## Data Validation Rules

Based on COBOL patterns and user notes:
- **Date validation**: Minimum year 1601 (CEEDAYS limitation), maximum age 150 years
- **Future dates**: Reject with fail code 'Y'
- **Eye-catchers**: 'CUST' for customers, 'ACCT' for accounts, 'PROC' for transactions
- **Composite keys**: Sort code + customer number, sort code + account number
- **Control counters**: Use Control table for generating customer/account numbers

## Testing Patterns

Follow existing patterns from:
- **Unit tests**: ErrorLoggingServiceTest (mocks with @ExtendWith(MockitoExtension.class))
- **Integration tests**: @SpringBootTest with @ActiveProfiles("test") and @Sql
- **Repository tests**: @JdbcTest with @Import
- **Web tests**: MockMvc for REST endpoint testing

## Total Effort Estimate

- Setup: 24 hours (3 tasks)
- Pre-migration validators: 54 hours (6 tasks)
- Migrations: 62 hours (6 tasks)
- Post-migration validators: 24 hours (3 tasks)
- **Total: 164 hours** (~20-21 working days for a single developer)

With parallel execution:
- Phase 1 (setup): ~10 hours (critical path: setup_001)
- Phase 2 (validators): ~12 hours (many can run in parallel)
- Phase 3 (migrations): ~32 hours (critical path: customer → account → transaction)
- Phase 4 (validation): ~10 hours (E2E tests)
- **Parallel total: ~64 hours** (~8 working days with proper parallelization)

## Using This Task Graph

Each task can be assigned to a separate Devin session or developer. The `depends_on` field ensures correct execution order. Tasks with empty `depends_on` arrays can start immediately. Tasks with no downstream dependencies blocking them can run in parallel.

## BMS Screen Programs (Excluded)

The following 10 programs are BMS 3270 terminal screens and are NOT included in this migration plan as they're replaced by REST API endpoints:
- BNKMENU (main menu)
- BNK1CAC, BNK1CCA, BNK1CCS, BNK1CRA (create operations)
- BNK1DAC, BNK1DCS (delete operations)
- BNK1TFN (transfer funds)
- BNK1UAC (update account)

## References

- Source repository: taylor-curran/og-cics-cobol-app
- Target repository: taylor-curran/target-springboot-cics
- Testing guide: docs/TESTING.md
- Migration playbook: MIGRATION_PLAYBOOK.md
- Existing services: CompanyInfoService, SortCodeService, ErrorLoggingService, CreditAgencyService, BankDataGenerator
