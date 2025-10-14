# Developer Guide - COBOL to Java Migration

## 📋 Quick Reference

**Original System:** `og-cics-cobol-app` (COBOL/CICS)  
**New System:** `target-springboot-cics` (Java 17/Spring Boot)  
**Check Coverage:** `mvn verify`  
**Run Tests:** `mvn test`  
**Start App:** `mvn spring-boot:run`  
**Reset Data:** `rm banking.db && mvn spring-boot:run`  

---

## 🎯 Project Context

You're migrating a CICS banking application from COBOL to Java Spring Boot. 

### Migration Status
- **Source:** `og-cics-cobol-app/` - Original COBOL programs
- **Target:** `target-springboot-cics/` - New Java implementation
- **Progress:** 5/29 programs migrated (17%)

### Completed Migrations
✅ `GETCOMPY.cbl` → `CompanyInfoService.java`  
✅ `GETSCODE.cbl` → `SortCodeService.java`  
✅ `CRDTAGY1.cbl` → `CreditAgencyService.java`  
✅ `ABNDPROC.cbl` → `ErrorLoggingService.java`  
✅ `BANKDATA.cbl` → `BankDataGenerator.java` + Auto-initialization  

---

## 🏗️ Project Structure

```
target-springboot-cics/
├── src/main/java/com/cbsa/migration/
│   ├── controller/     # REST endpoints (replace CICS transactions)
│   ├── service/        # Business logic (replace COBOL programs)
│   ├── repository/     # Data access (replace VSAM/DB2)
│   ├── model/          # Entities (replace COBOL copybooks)
│   ├── dto/            # Request/Response objects
│   └── datagen/        # Test data generation (from BANKDATA.cbl)
├── src/main/resources/
│   └── db/schema.sql   # SQLite production schema
└── src/test/resources/
    └── db/test-schema.sql  # H2 test schema
```

---

## 🗄️ Database Management

### Local Development Database

#### **Current Setup**
- **Database:** SQLite file `banking.db` (created in project root)
- **Auto-creation:** Database and schema created automatically on app startup
- **Auto-population:** Test data automatically generated if DB is empty (NEW!)
- **Not in Git:** Each developer has their own local database
- **Generated data:** 100 customers, ~275 accounts, ~2500 transactions

#### **Starting Fresh**
```bash
# Delete existing database
rm banking.db

# Start the application - database auto-creates and auto-populates
mvn spring-boot:run

# Database now has 100 customers with accounts and transactions!
```

#### **Checking Database Contents**
```bash
# Count records
sqlite3 banking.db "SELECT 'Customers: ' || COUNT(*) FROM customer 
UNION ALL SELECT 'Accounts: ' || COUNT(*) FROM account 
UNION ALL SELECT 'Transactions: ' || COUNT(*) FROM bank_transaction;"

# View sample customers
sqlite3 banking.db "SELECT * FROM customer LIMIT 5;"

# View account balances
sqlite3 banking.db "SELECT account_number, actual_balance FROM account;"
```

#### **Backing Up Your Test Data**
```bash
# Backup your current database
cp banking.db banking.db.backup

# Restore from backup
cp banking.db.backup banking.db
```

### Test Data Management

#### **Current State**
- ✅ **Auto-generation** - Data automatically generated on startup if DB is empty
- ✅ **COBOL Migrated** - `BANKDATA.cbl` migrated to `BankDataGenerator.java`
- ✅ **Enhanced** - Now generates transactions (not in original COBOL)
- 🧪 **Test environment** uses H2 in-memory DB (fresh each run)
- 💾 **Local development** gets consistent test data automatically

#### **Creating Test Data via API**
```bash
# Currently available endpoints:

# Check credit score (creates test transaction)
curl -X POST http://localhost:8085/api/credit-agency/check \
  -H "Content-Type: application/json" \
  -d '{
    "customerId": "12345",
    "requestType": "SCORE_CHECK"
  }'

# Log an error (creates error record)
curl -X POST http://localhost:8085/api/error/log \
  -H "Content-Type: application/json" \
  -d '{
    "programName": "TEST_PROGRAM",
    "errorMessage": "Test error for development"
  }'

# Note: Customer and Account creation APIs not yet migrated from COBOL
# Use SQL inserts below for now
```

#### **Creating Test Data via SQL**
```bash
# Insert sample customer
sqlite3 banking.db "INSERT INTO customer VALUES (
  'CUST', '999999', 10001, 'Test User', '456 Test Ave', 
  '1985-06-15', 700, '2024-01-01'
);"

# Insert sample account
sqlite3 banking.db "INSERT INTO account VALUES (
  'ACCT', 10001, '999999', '00000001', 'CHECKING', 
  0.01, '2024-01-01', 1000, NULL, NULL, 5000.00, 5000.00
);"

# Initialize control record (required for some operations)
sqlite3 banking.db "INSERT OR REPLACE INTO control VALUES (
  'CONTROL', 0, 0, 0, 0
);"
```

#### **Quick Development Reset**
```bash
# Full reset script (save as reset-db.sh)
#!/bin/bash
echo "Resetting database..."
rm -f banking.db
mvn spring-boot:run &
sleep 5
kill %1
echo "Database reset complete. Run 'mvn spring-boot:run' to start."
```

### Automatic Data Generation Configuration

#### **Default Settings (application.properties)**
```properties
data.generation.enabled=true              # Enable auto-generation
data.generation.customer.start=1          # Starting customer number
data.generation.customer.end=100          # Ending customer number
data.generation.customer.step=1           # Customer increment
data.generation.seed=12345                # Random seed for reproducibility
data.generation.transaction.days=30       # Days of transaction history
```

#### **Test Settings (application-test.properties)**
```properties
data.generation.customer.end=10           # Only 10 customers for faster tests
data.generation.transaction.days=7        # Less transaction history
```

#### **Manual Generation Command**
```bash
# Force regenerate data with custom parameters
mvn spring-boot:run -Dspring-boot.run.arguments="generate-data --start=1 --end=50 --seed=99999"
```

### Original COBOL Data Generation

The original COBOL application has a sophisticated test data generator:

#### **BANKDATA.cbl Program**
- **Purpose:** Batch program to populate CUSTOMER (VSAM) and ACCOUNT (DB2)
- **Parameters:** `fffffff,ttttttt,ssssss,rrrrrr`
  - `fffffff` - Starting key for generation
  - `ttttttt` - Last key to generate
  - `ssssss` - Step size for generation
  - `rrrrrr` - Random seed for variations
- **Not yet migrated** to Java - potential future enhancement

#### **Future Improvements Needed**
1. **Migrate BANKDATA.cbl** to Java data generator utility
2. **Create seed SQL scripts** with standard test data
3. **Add data fixtures** for integration tests
4. **Implement reset endpoint** for development (`/api/admin/reset-data`)
5. **Create data profiles** (minimal, standard, stress-test)
WIP END

### Critical: Two Schema Files
⚠️ **ALWAYS maintain both schemas in sync:**
1. **Production:** `src/main/resources/db/schema.sql` (SQLite)
2. **Test:** `src/test/resources/db/test-schema.sql` (H2)

### Schema Change Process
```bash
# 1. FIRST update test schema (H2 compatible) for your development
edit src/test/resources/db/test-schema.sql

# 2. Continue development with your tests
mvn test
# Note: DatabaseSchemaConsistencyTest will FAIL - this is expected!

# 3. When you're confident in your changes, update production schema
edit src/main/resources/db/schema.sql

# 4. Verify schemas are now in sync
mvn test -Dtest=DatabaseSchemaConsistencyTest
# This should now PASS

# 5. Run all tests
mvn test
```

**The `DatabaseSchemaConsistencyTest` acts as a notification system:**
- ✅ **Failing test = Expected** during development (schemas intentionally out of sync)
- ❌ **Failing test = Problem** if you didn't expect it (schemas drifted unexpectedly)
- ✅ **Passing test = Good** when you're ready to commit (schemas are in sync)

### Database Structure (from COBOL)
- **Eye-catchers:** String prefixes for record identification (e.g., 'CUST', 'ACCT')
- **Composite keys:** Multiple fields form primary keys
- **Fixed-width fields:** Preserved from COBOL layouts
- **Date/Time as strings:** ISO format for compatibility

---

## 🧪 Testing Strategy

### Coverage Requirements by Layer

Run `mvn verify` to check if you meet these thresholds:

| Layer | Required | Current | What to Test |
|-------|----------|---------|--------------|
| **Service** | 80% | ❌ 2% | Business logic from COBOL programs |
| **Repository** | 70% | ❌ 55% | Database operations |
| **Controller** | 60% | ❌ 10% | REST endpoint validation |
| **Model** | 50% | ✅ 66% | Entity validation |
| **DTO** | 40% | ❌ 13% | Data transfer objects |
| **Overall** | 50% | ❌ 33% | Entire project |

### Test Types
1. **Unit Tests** - Mock dependencies, test logic
2. **Integration Tests** - Real H2 database, test data access
3. **Web Tests** - MockMvc, test REST endpoints

### Test Commands
```bash
# Run all tests with coverage check
mvn verify

# Just run tests
mvn test

# Generate coverage report
mvn clean test jacoco:report
open target/site/jacoco/index.html

# Run specific test
mvn test -Dtest=ServiceNameTest
```

---

## 🚀 Making Incremental Progress

### To Migrate a New COBOL Program

1. **Find the COBOL program** in `og-cics-cobol-app/src/`
   ```bash
   find og-cics-cobol-app -name "*.cbl" | grep -i PROGRAMNAME
   ```

2. **Create Java service** in `src/main/java/com/cbsa/migration/service/`
   ```java
   @Service
   public class NewProgramService {
       // Translate COBOL logic to Java
   }
   ```

3. **Create REST controller** if it's a CICS transaction
   ```java
   @RestController
   @RequestMapping("/api/program")
   public class NewProgramController {
       @Autowired
       private NewProgramService service;
   }
   ```

4. **Add repository** if it accesses data
   ```java
   @Repository
   public interface NewProgramRepository {
       // Data access methods
   }
   ```

5. **Write tests FIRST**
   - Unit test for service (mock repository)
   - Integration test for repository (real H2)
   - Web test for controller (MockMvc)

6. **Update schema** if needed (follow schema change process above)

7. **Check coverage**
   ```bash
   mvn verify
   ```
   Service layer needs 80% coverage!

### Working with COBOL Data Patterns

**COBOL Copybooks → Java Models:**
```cobol
01 CUSTOMER-RECORD.
   05 CUST-NUMBER    PIC 9(10).
   05 CUST-NAME      PIC X(50).
```
Becomes:
```java
@Entity
public class Customer {
    @Column(name = "customer_number")
    private Long customerNumber;
    
    @Column(name = "name", length = 50)
    private String name;
}
```

**CICS Commands → REST Endpoints:**
- `EXEC CICS LINK PROGRAM('PROG')` → `POST /api/prog`
- `EXEC CICS READ` → `GET /api/resource/{id}`
- `EXEC CICS WRITE` → `POST /api/resource`
- `EXEC CICS DELETE` → `DELETE /api/resource/{id}`

---

## 🔧 Common Tasks

### Check what needs work
```bash
# See coverage by layer
mvn verify

# Find untested services
grep -r "@Service" src/main/java | cut -d: -f1 | while read f; do
  base=$(basename $f .java)
  if ! find src/test -name "${base}Test.java" | grep -q .; then
    echo "Missing test: $base"
  fi
done

# Find COBOL programs not yet migrated
ls og-cics-cobol-app/src/*.cbl | while read f; do
  prog=$(basename $f .cbl)
  if ! grep -r "$prog" src/main/java --include="*.java" -i -q; then
    echo "Not migrated: $prog"
  fi
done
```

### Debug test failures
```bash
# Run with detailed output
mvn test -X

# Run single test with debugging
mvn -Dtest=ServiceTest#methodName test

# Check H2 console (when app is running)
# http://localhost:8080/h2-console
# JDBC URL: jdbc:h2:mem:testdb
```

---

## ⚡ Quick Wins

1. **Add missing unit tests** for services - biggest coverage gap
2. **Test error paths** - catch blocks often uncovered  
3. **Add controller tests** - quick to write with MockMvc
4. **Document COBOL → Java mappings** as you discover them

---

## 📝 Key Principles

1. **Test First** - Write tests before migrating COBOL logic
2. **Maintain Schema Sync** - Both schemas must match structurally
3. **Meet Coverage Targets** - Service layer is critical (80%)
4. **Preserve COBOL Patterns** - Eye-catchers, composite keys matter
5. **Incremental Migration** - One program at a time

---

## 🆘 Getting Help

- **Original COBOL:** Check `og-cics-cobol-app/doc/` for architecture
- **Test Guide:** See `docs/TESTING.md` for detailed testing info
- **Coverage Report:** `target/site/jacoco/index.html` after tests
- **Schema Issues:** `DatabaseSchemaConsistencyTest` output

Remember: You're not just translating code - you're modernizing a banking system while preserving its business logic and data integrity.
