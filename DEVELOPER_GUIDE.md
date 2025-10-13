# Developer Guide - COBOL to Java Migration

## 📋 Quick Reference

**Original System:** `/og-cics-cobol-app` (COBOL/CICS)  
**New System:** `/target-springboot-cics` (Java 17/Spring Boot)  
**Check Coverage:** `mvn verify`  
**Run Tests:** `mvn test`  
**Start App:** `mvn spring-boot:run`  

---

## 🎯 Project Context

You're migrating a CICS banking application from COBOL to Java Spring Boot. 

### Migration Status
- **Source:** `og-cics-cobol-app/` - Original COBOL programs
- **Target:** `target-springboot-cics/` - New Java implementation
- **Progress:** 4/29 programs migrated (14%)

### Completed Migrations
✅ `GETCOMPY.cbl` → `CompanyInfoService.java`  
✅ `GETSCODE.cbl` → `SortCodeService.java`  
✅ `CRDTAGY1.cbl` → `CreditAgencyService.java`  
✅ `ABNDPROC.cbl` → `ErrorLoggingService.java`  

---

## 🏗️ Project Structure

```
target-springboot-cics/
├── src/main/java/com/cbsa/migration/
│   ├── controller/     # REST endpoints (replace CICS transactions)
│   ├── service/        # Business logic (replace COBOL programs)
│   ├── repository/     # Data access (replace VSAM/DB2)
│   ├── model/          # Entities (replace COBOL copybooks)
│   └── dto/            # Request/Response objects
├── src/main/resources/
│   └── db/schema.sql   # SQLite production schema
└── src/test/resources/
    └── db/test-schema.sql  # H2 test schema
```

---

## 🗄️ Database Management

### Critical: Two Schema Files
⚠️ **ALWAYS maintain both schemas in sync:**
1. **Production:** `src/main/resources/db/schema.sql` (SQLite)
2. **Test:** `src/test/resources/db/test-schema.sql` (H2)

### Schema Change Process
```bash
# 1. FIRST update test schema (H2 compatible)
edit src/test/resources/db/test-schema.sql

# 2. THEN update production schema (SQLite)
edit src/main/resources/db/schema.sql

# 3. Run consistency test - MUST PASS
mvn test -Dtest=DatabaseSchemaConsistencyTest

# 4. Run all tests
mvn test
```

**The `DatabaseSchemaConsistencyTest` will FAIL if schemas diverge - this is intentional!**

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
