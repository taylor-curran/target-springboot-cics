# Schema Consistency Guide

## Why Two Schemas?

We maintain **two separate schema files** for production and testing:

- **Production**: `src/main/resources/db/schema.sql` (SQLite syntax)
- **Test**: `src/test/resources/db/test-schema.sql` (H2 syntax)

### Why not just one schema?

**Testing with a real database matters.** Our integration tests use H2 (in-memory database) for speed, while production runs SQLite. Both databases have slightly different SQL syntax, so we need separate schemas.

**The `DatabaseSchemaConsistencyTest` is your safety net.** It automatically compares both schemas to ensure they stay synchronized. When it fails, it means your schemas have diverged - which is often intentional during development!

### Benefits of this approach:

✅ **Integration tests catch real issues** - No mocking database behavior  
✅ **Fast test execution** - H2 runs in memory, recreated each test run  
✅ **Production-like behavior** - Tests use actual SQL queries and constraints  
✅ **Protected against drift** - Consistency test alerts when schemas diverge  

## Schema Update Workflow (TDD Approach)

When you need to modify the database schema, follow this workflow:

### Step 1: Update Test Schema First (H2)

Start by updating `src/test/resources/db/test-schema.sql` with your changes.

**Why test schema first?** This is Test-Driven Development - write your tests (which need the schema) before implementing production code.

```bash
# Edit the H2 test schema
vim src/test/resources/db/test-schema.sql

# Add your new table, column, index, etc.
```

### Step 2: Develop and Test (Consistency Test WILL FAIL)

Write your tests and develop your feature. **The `DatabaseSchemaConsistencyTest` will fail** - this is completely normal and expected!

```bash
# Run your tests (consistency test fails - EXPECTED during development)
mvn test

# Or just check the consistency test specifically
mvn test -Dtest=DatabaseSchemaConsistencyTest
```

**Expected output during development:**
```
[ERROR] testProductionAndTestSchemasAreConsistent - Table 'your_new_table' 
        definition should match between schemas
```

**This is fine!** Keep developing and testing with your H2 schema changes.

### Step 3: Update Production Schema (SQLite)

Once your feature is complete and tests pass, update the production schema with equivalent changes.

**Important:** You must translate H2 syntax to SQLite syntax (see next section).

```bash
# Edit the SQLite production schema
vim src/main/resources/db/schema.sql

# Add the same table/column/index with SQLite syntax
```

### Step 4: Verify Synchronization

Run the consistency test to confirm both schemas are in sync:

```bash
mvn test -Dtest=DatabaseSchemaConsistencyTest
```

**Expected output when synced:**
```
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0
[INFO] BUILD SUCCESS
```

### Complete Example Workflow

```bash
# 1. Update test schema for new feature
echo "CREATE TABLE new_feature (id BIGINT AUTO_INCREMENT PRIMARY KEY, name VARCHAR(255));" \
  >> src/test/resources/db/test-schema.sql

# 2. Develop and test (consistency test fails - OK!)
mvn test  # Your feature tests pass, consistency test fails

# 3. Update production schema with SQLite syntax
echo "CREATE TABLE new_feature (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT);" \
  >> src/main/resources/db/schema.sql

# 4. Verify sync
mvn test -Dtest=DatabaseSchemaConsistencyTest  # Should pass now!
```

## SQLite vs H2 Syntax Differences

When syncing schemas, you need to translate between database-specific syntax:

### Data Types

| SQLite (Production) | H2 (Test) | Notes |
|---------------------|-----------|-------|
| `TEXT` | `VARCHAR(255)` | String data |
| `INTEGER` | `INTEGER` or `BIGINT` | Numbers |
| `REAL` | `REAL` | Floating point |

**Example:**
```sql
-- Production (SQLite)
name TEXT NOT NULL

-- Test (H2)
name VARCHAR(255) NOT NULL
```

### Auto-Increment Primary Keys

| SQLite (Production) | H2 (Test) |
|---------------------|-----------|
| `INTEGER PRIMARY KEY AUTOINCREMENT` | `BIGINT AUTO_INCREMENT PRIMARY KEY` |

**Example:**
```sql
-- Production (SQLite)
CREATE TABLE application_error (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL
);

-- Test (H2)
CREATE TABLE application_error (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    timestamp VARCHAR(255) NOT NULL
);
```

### Timestamp Handling

Both use string storage for consistency with SQLite limitations:

```sql
-- Production (SQLite)
created_at TEXT DEFAULT CURRENT_TIMESTAMP

-- Test (H2) - Use VARCHAR, not TIMESTAMP
created_at VARCHAR(255) DEFAULT CURRENT_TIMESTAMP
```

**Why VARCHAR for timestamps?** SQLite doesn't have a native TIMESTAMP type, so we store as TEXT. H2 tests use VARCHAR(255) to match production behavior exactly.

### Partial Indexes (SQLite Only)

SQLite supports `WHERE` clauses in indexes; H2 does not.

```sql
-- Production (SQLite) - OK to use WHERE
CREATE INDEX idx_transaction_transfer 
  ON bank_transaction(target_sort_code, target_account_number)
  WHERE target_sort_code IS NOT NULL;

-- Test (H2) - Remove WHERE clause
CREATE INDEX idx_transaction_transfer 
  ON bank_transaction(target_sort_code, target_account_number);
```

The consistency test's normalization logic strips WHERE clauses when comparing, so this difference is handled automatically.

### Quick Reference Table

| Feature | SQLite | H2 |
|---------|--------|-----|
| String type | `TEXT` | `VARCHAR(255)` |
| Auto-increment | `AUTOINCREMENT` | `AUTO_INCREMENT` |
| Primary key with auto | `INTEGER PRIMARY KEY AUTOINCREMENT` | `BIGINT AUTO_INCREMENT PRIMARY KEY` |
| Timestamp storage | `TEXT` | `VARCHAR(255)` |
| Partial indexes | Supported with `WHERE` | Not supported (omit `WHERE`) |
| Check constraints | Same | Same |
| Foreign keys | Same | Same |

## How to Resolve Test Failures

When `DatabaseSchemaConsistencyTest` fails, follow these steps:

### Step 1: Run the Test

```bash
mvn test -Dtest=DatabaseSchemaConsistencyTest
```

### Step 2: Read the Failure Message

The test provides detailed information about what doesn't match:

```
[ERROR] testProductionAndTestSchemasAreConsistent
  Table 'customer' definition should match between schemas
  Expected: CREATE TABLE CUSTOMER (ID BIGINT PRIMARY KEY, NAME VARCHAR(255))
  Actual:   CREATE TABLE CUSTOMER (ID BIGINT PRIMARY KEY, NAME VARCHAR(255), EMAIL VARCHAR(255))
```

This tells you:
- **Which table** differs (customer)
- **What's expected** (from production schema)
- **What was found** (from test schema)

### Step 3: Compare Schemas Side-by-Side

```bash
# View both schemas
diff src/main/resources/db/schema.sql src/test/resources/db/test-schema.sql
```

Or open both files in your editor for visual comparison.

### Step 4: Identify the Discrepancy

Common issues:
- ✗ **Added table to one schema only** - Add to both
- ✗ **Modified column in one schema** - Update both
- ✗ **Added index to one schema** - Add to both
- ✗ **Forgot syntax translation** - Apply correct SQLite ↔ H2 syntax

### Step 5: Apply the Fix

Update the schema that's missing the change, using appropriate syntax:

```bash
# If test schema is missing a change
vim src/test/resources/db/test-schema.sql

# If production schema is missing a change
vim src/main/resources/db/schema.sql
```

**Remember:** Apply syntax translations when syncing!

### Step 6: Verify the Fix

```bash
mvn test -Dtest=DatabaseSchemaConsistencyTest
```

Should now pass with `BUILD SUCCESS`.

### Understanding the Test's Normalization Logic

The test automatically handles many syntax differences. It:

1. **Strips comments** - SQL comments are ignored
2. **Normalizes whitespace** - Extra spaces don't matter
3. **Translates data types** - TEXT ↔ VARCHAR(255) automatically compared
4. **Handles auto-increment** - AUTOINCREMENT ↔ AUTO_INCREMENT normalized
5. **Compares structure** - Tables, constraints, indexes checked separately

You can see the normalization logic in `DatabaseSchemaConsistencyTest.normalizeSchema()`.

### When Intentional Differences Are OK

**Partial indexes** - SQLite can have WHERE clauses; H2 versions omit them. The test accounts for this.

**Everything else should match.** If schemas differ for any other reason, the test will fail.

## Common Scenarios

### Scenario 1: Adding a New Table

```sql
-- 1. Add to test schema (H2)
CREATE TABLE IF NOT EXISTS transaction_log (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    timestamp VARCHAR(255) NOT NULL,
    message VARCHAR(255)
);

-- 2. Develop and test your feature

-- 3. Add to production schema (SQLite)
CREATE TABLE IF NOT EXISTS transaction_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL,
    message TEXT
);

-- 4. Verify sync
mvn test -Dtest=DatabaseSchemaConsistencyTest
```

### Scenario 2: Adding a Column

```sql
-- 1. Add to test schema (H2)
ALTER TABLE customer ADD COLUMN email VARCHAR(255);

-- Actually, for our schemas, better to recreate the full CREATE TABLE
-- with the new column in the appropriate position

-- 2. Test your feature

-- 3. Add to production schema (SQLite) with correct syntax
-- Update the CREATE TABLE statement with: email TEXT

-- 4. Verify sync
mvn test -Dtest=DatabaseSchemaConsistencyTest
```

### Scenario 3: Adding an Index

```sql
-- 1. Add to test schema (H2)
CREATE INDEX IF NOT EXISTS idx_customer_email ON customer(email);

-- 2. Test performance improvement

-- 3. Add to production schema (SQLite)
CREATE INDEX IF NOT EXISTS idx_customer_email ON customer(email);

-- 4. Verify sync
mvn test -Dtest=DatabaseSchemaConsistencyTest
```

## Quick Commands Reference

```bash
# Run only the schema consistency test
mvn test -Dtest=DatabaseSchemaConsistencyTest

# Run all tests (includes consistency check)
mvn test

# View detailed test output
mvn test -Dtest=DatabaseSchemaConsistencyTest -X

# Compare schemas visually
diff src/main/resources/db/schema.sql src/test/resources/db/test-schema.sql

# Check coverage (includes all tests)
mvn verify
```

## Best Practices

### ✅ DO:

- **Update test schema first** - TDD approach, develop against tests
- **Let consistency test fail during development** - It's a reminder, not a blocker
- **Sync schemas before committing** - Both files should be updated together
- **Translate syntax carefully** - TEXT → VARCHAR(255), AUTOINCREMENT → AUTO_INCREMENT
- **Test your changes** - Run the consistency test after syncing

### ❌ DON'T:

- **Don't commit with failing consistency test** - Sync schemas first
- **Don't update only one schema** - Both must stay synchronized
- **Don't use H2-specific features** - Keep schemas compatible with both databases
- **Don't ignore test failures** - They're protecting you from schema drift
- **Don't modify the test itself** - It's working as designed

## Why This Matters

Schema drift between test and production environments is a common source of bugs:

- **Tests pass but production fails** - Schema mismatch causes runtime errors
- **Integration tests don't catch issues** - If test schema differs, you're not testing reality
- **Database migrations fail** - Schema inconsistencies cause deployment problems

The `DatabaseSchemaConsistencyTest` prevents all of these by catching drift early.

**Remember:** A failing consistency test during development is INTENTIONAL. It's reminding you to sync both schemas before you commit. This is a feature, not a bug!

## Related Documentation

- **Testing Guide**: `docs/TESTING.md` - Overall testing strategy and coverage requirements
- **Migration Playbook**: `MIGRATION_PLAYBOOK.md` - Complete COBOL migration workflow
- **Production Schema**: `src/main/resources/db/schema.sql` - SQLite schema
- **Test Schema**: `src/test/resources/db/test-schema.sql` - H2 schema
- **Consistency Test**: `src/test/java/com/cbsa/migration/DatabaseSchemaConsistencyTest.java`

---

*The consistency test failing is a FEATURE that protects you from schema drift. Use the workflow, sync your schemas, and sleep soundly knowing your tests match production.*
