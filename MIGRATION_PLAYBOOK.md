# COBOL Migration Playbook

Quick, practical guide for migrating COBOL programs to Java. No fluff.

## 🎯 Migration Status
- **Source:** `og-cics-cobol-app/` (29 COBOL programs)
- **Target:** `target-springboot-cics/` (Spring Boot)
- **Progress:** 5/29 programs migrated (17%)

## ⚡ Quick Commands
```bash
mvn verify                    # Check if coverage meets requirements
mvn test                     # Run all tests  
mvn spring-boot:run          # Start app (auto-generates data if empty)
rm banking.db && mvn spring-boot:run  # Fresh start with new data
```

---

## 📝 Migration Checklist

When migrating a COBOL program:

### 1. **Find & Understand**
```bash
# Find the COBOL program
ls og-cics-cobol-app/src/base/cobol_src/*.cbl | grep -i PROGNAME
```
Read it. Understand what it does. Look for copybooks it uses.

### 2. **Create Service**
Create `src/main/java/.../service/NewProgramService.java`
- Translate COBOL logic to Java
- Keep business rules intact
- Don't optimize yet - just make it work

### 3. **Write Tests First**
Create `src/test/java/.../service/NewProgramServiceTest.java`
- Unit tests with mocks for service layer
- Integration tests with real H2 database for repository
- Controller tests with MockMvc for REST endpoints

### 4. **Add Repository** (if needed)
If the program accesses data, create repository with JDBC
- Use the existing `JdbcTemplate` patterns
- Test with real database, not mocks

### 5. **Create Controller** (if CICS transaction)
Map CICS transactions to REST:
- `EXEC CICS LINK` → `POST /api/program`
- `EXEC CICS READ` → `GET /api/resource/{id}`
- Keep request/response DTOs simple

### 6. **Check Coverage**
```bash
mvn verify
```
Don't stress about exact percentages - focus on testing the risky parts.

---

## 🗄️ Database & Schema Management

### **The Two-Schema Reality**

We maintain two schemas because our tests use H2 (in-memory) and production uses SQLite:
- **Test:** `src/test/resources/db/test-schema.sql` (H2 syntax)
- **Prod:** `src/main/resources/db/schema.sql` (SQLite syntax)

The `DatabaseSchemaConsistencyTest` alerts you when these diverge.

### **Staging a Schema Change**

When you need to change the schema:

1. **Start with test schema** - Update H2 schema for your tests
2. **Develop and test** - The consistency test will fail (that's fine!)
3. **Update production schema** - Once happy, update SQLite schema  
4. **Verify sync** - Now consistency test should pass

```bash
# During development (schemas intentionally different)
mvn test  # Consistency test fails - EXPECTED

# After syncing schemas
mvn test -Dtest=DatabaseSchemaConsistencyTest  # Should pass
```

**Why this matters:** Integration tests catch real database issues that mocks won't. That's why we measure repository coverage separately - those tests actually hit the database.

---

## 📊 Test Data Management

### **Automatic Generation (Default)**

Data auto-generates on startup if database is empty:
- 100 customers with realistic names/addresses
- 1-5 accounts per customer (random)
- 30 days of transaction history

Configuration in `application.properties`:
```properties
data.generation.enabled=true
data.generation.customer.end=100    # Number of customers
data.generation.seed=12345         # Fixed seed for reproducibility
data.generation.transaction.days=30 # Transaction history
```

### **Manual Options**

If you need specific test data:
```bash
# Direct SQL
sqlite3 banking.db "INSERT INTO customer VALUES (...)"

# Or temporarily disable auto-generation
# Set data.generation.enabled=false in application.properties
```

But honestly, just delete the DB and let it regenerate - it's easier.

---

## 🎯 Coverage Strategy

### **What the Numbers Mean**

Coverage requirements by layer (enforced by `mvn verify`):

| Layer | Target | Why This Number |
|-------|--------|-----------------|
| **Service** | 80% | Business logic from COBOL - most critical |
| **Repository** | 70% | Database operations - integration tests matter |
| **Controller** | 60% | REST endpoints - focus on validation |
| **Model** | 50% | Entities - test business rules, not getters |
| **DTO** | 40% | Transfer objects - mostly boilerplate |

### **What Really Matters**

The percentages are guidelines, not gospel. Focus on:
- **Risk areas** - Complex business logic needs tests
- **Integration points** - Database operations, external calls
- **Edge cases** - Error handling, boundary conditions

A service at 75% coverage with good edge case tests is better than 80% that only tests happy paths.

### **Quick Coverage Wins**
- Add error case tests (catch blocks often missed)
- Test repository methods with real H2 database
- Add controller validation tests with MockMvc
- Don't waste time on getters/setters

---

## ⚠️ Common Gotchas

### **Schema Sync Issues**
- Test failures after schema change? You probably forgot to sync both schema files
- H2 and SQLite have different syntax - watch for `AUTO_INCREMENT` vs `AUTOINCREMENT`

### **Coverage Failures**
- Service below 80%? Add more business logic tests
- Repository below 70%? You're probably not testing with real database

### **Test Data Issues**
- Constraint violations? Check the COBOL eye-catchers (CUST, ACCT prefixes)
- Missing control record? The data generator creates it automatically now

### **COBOL Patterns**
- Composite keys: Customer has (sortcode, customer_number) as primary key
- Fixed-width fields: Names are padded, numbers are zero-filled
- Date formats: COBOL uses YYYYMMDD, we use ISO dates

---

## 🏁 Development Workflow

### **Daily Flow**
1. Pick a COBOL program to migrate
2. Write tests first (TDD works well here)
3. Implement until tests pass
4. Check coverage with `mvn verify`
5. Commit when layer requirements are met

### **Schema Changes**
1. Update test schema (H2)
2. Run tests, develop, iterate
3. Update prod schema (SQLite) when ready
4. Verify both schemas match
5. Commit both files together

### **Fresh Start**
```bash
rm banking.db               # Delete local database
mvn spring-boot:run         # Auto-generates test data
# New database with 100 customers ready to go
```

---

## 📚 Resources

- **Architecture:** `og-cics-cobol-app/doc/CBSA_Architecture_guide.md`
- **Testing Details:** `docs/TESTING.md`
- **Coverage Report:** `target/site/jacoco/index.html` (after `mvn test`)
- **Original COBOL:** `og-cics-cobol-app/src/base/cobol_src/*.cbl`

Remember: You're not just translating syntax - you're modernizing a banking system while preserving its proven business logic. Integration tests matter because banking systems can't afford data corruption.
