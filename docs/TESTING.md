# Testing Guide for CBSA Java Migration

## 🎯 Quick Start: Check Your Coverage

```bash
# Run this command to check if your code meets coverage requirements:
mvn verify

# Or with Java 17 explicitly (if not your default):
JAVA_HOME=/opt/homebrew/Cellar/openjdk@17/17.0.16/libexec/openjdk.jdk/Contents/Home mvn verify
```

**What happens when you run `mvn verify`:**
1. Compiles your code
2. Runs all tests
3. Generates JaCoCo coverage report
4. **Checks coverage against layer-specific thresholds**
5. **Fails the build if any layer is below its required threshold**

## 📊 Layer-Based Coverage Analysis

This project uses **layer-based coverage requirements** - different parts of the application have different coverage thresholds based on their criticality. JaCoCo enforces these requirements automatically during the build.

### **Coverage Requirements by Layer**

| Layer | Required Coverage | Why This Threshold? | Current Status |
|-------|------------------|-------------------|----------------|
| **Service** | 80% instructions, 70% branches | Critical business logic - must be thoroughly tested | ❌ 2% |
| **Repository** | 70% instructions | Database operations need validation | ❌ 55% |
| **Controller** | 60% instructions | REST endpoints & validation | ❌ 10% |
| **Model** | 50% instructions | Entity validation & business rules | ✅ 66% |
| **DTO/Mapper** | 40% instructions | Mostly generated/boilerplate code | ❌ 13% |
| **Config** | 0% (optional) | Spring auto-configuration | ✅ N/A |
| **Overall Project** | 50% minimum | Safety net for entire codebase | ❌ 33% |

### **Understanding Layer Coverage**

- **Service Layer** (80% required): Contains your business logic from migrated COBOL programs. This is the most critical layer.
- **Repository Layer** (70% required): Database access code that replaced VSAM/DB2. Must be tested with real database.
- **Controller Layer** (60% required): REST endpoints that replaced CICS transactions. Focus on request/response validation.
- **Model/Entity Layer** (50% required): Data structures. Test validation logic, not just getters/setters.
- **DTO/Mapper Layer** (40% required): Data transfer objects. Lower threshold since mostly boilerplate.
- **Config Layer** (0% optional): Spring configuration. Usually doesn't need testing.

### **How to Fix Coverage Violations**

When `mvn verify` fails, it will show exactly which layers need more tests:

```
[WARNING] Rule violated for package com.cbsa.migration.service: 
         instructions covered ratio is 0.02, but expected minimum is 0.80
```

This means: Add more service tests! Focus on the layers with the biggest gaps first.

### **Viewing Detailed Coverage Reports**

After running tests, you can view detailed coverage information:

```bash
# Generate coverage report
mvn clean test jacoco:report

# View HTML report (open in browser)
open target/site/jacoco/index.html

# Check coverage without running tests (if tests were already run)
mvn jacoco:check
```

The HTML report shows:
- **Green**: Fully covered code
- **Yellow**: Partially covered (some branches missed)
- **Red**: Uncovered code that needs tests

## 🧪 Testing Overview

This project uses a comprehensive testing strategy to ensure reliability during the COBOL-to-Java migration.

### **Test Stack**
- **JUnit 5** - Core testing framework
- **Mockito** - Mocking for unit tests
- **Spring Boot Test** - Integration testing
- **H2 Database** - In-memory database for tests
- **JaCoCo** - Code coverage analysis (minimum 50%)

## 🗄️ Database Testing Strategy

### **CRITICAL: Schema Synchronization**

⚠️ **We maintain TWO separate schema files:**
- **Production**: `src/main/resources/db/schema.sql` (SQLite)
- **Test**: `src/test/resources/db/test-schema.sql` (H2)

### **Schema Consistency Test**

The `DatabaseSchemaConsistencyTest` ensures schemas stay synchronized. **This test WILL FAIL when schemas diverge** - this is intentional!

#### When the Test Fails:

1. **You've likely updated one schema without the other**
2. **Review the differences** in the test output
3. **Update BOTH schemas** to match:
   - Production schema: SQLite-specific syntax
   - Test schema: H2-compatible equivalent
4. **Re-run the test** to confirm synchronization

#### Example Schema Updates:

```sql
-- Production (SQLite)
CREATE TABLE example (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
);

-- Test (H2) - Must be equivalent
CREATE TABLE example (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

## 🏃 Running Tests

### **With Java 17 (Recommended)**
```bash
# All tests
JAVA_HOME=/opt/homebrew/Cellar/openjdk@17/17.0.16/libexec/openjdk.jdk/Contents/Home mvn test

# Specific test class
JAVA_HOME=/opt/homebrew/Cellar/openjdk@17/17.0.16/libexec/openjdk.jdk/Contents/Home mvn test -Dtest=ErrorControllerTest

# With coverage report
JAVA_HOME=/opt/homebrew/Cellar/openjdk@17/17.0.16/libexec/openjdk.jdk/Contents/Home mvn clean test jacoco:report
```

### **Coverage Reports**
After running tests with JaCoCo:
- **HTML Report**: `target/site/jacoco/index.html`
- **CSV Report**: `target/site/jacoco/jacoco.csv`
- **XML Report**: `target/site/jacoco/jacoco.xml`

## 📁 Test Structure

```
src/test/
├── java/com/cbsa/migration/
│   ├── DatabaseSchemaConsistencyTest.java  # ⚠️ CRITICAL - Schema sync check
│   ├── controller/                         # REST endpoint tests
│   ├── dto/                                # DTO validation tests
│   ├── model/                              # Entity tests
│   ├── repository/jdbc/                    # Database integration tests
│   └── service/                            # Business logic tests
└── resources/
    ├── application-test.properties         # H2 test configuration
    └── db/test-schema.sql                  # H2 test database schema
```

## 🎯 Test Categories

### **1. Unit Tests**
- Isolated component testing with mocks
- Example: `ErrorLoggingServiceTest.UnitTests`
```java
@ExtendWith(MockitoExtension.class)
class UnitTests {
    @Mock private Repository repo;
    @InjectMocks private Service service;
}
```

### **2. Integration Tests**
- Test component interactions with real database
- Example: `JdbcApplicationErrorRepositoryTest`
```java
@JdbcTest
@Sql("/db/test-schema.sql")
class RepositoryIntegrationTest {
    @Autowired JdbcTemplate jdbcTemplate;
}
```

### **3. Web Layer Tests**
- Test REST controllers without starting full server
- Example: `ErrorControllerTest`
```java
@WebMvcTest(ErrorController.class)
class ControllerTest {
    @Autowired MockMvc mockMvc;
}
```

## ⚡ Quick Reference

### **Test Data Lifecycle**
1. **Test starts** → H2 database created in memory
2. **Schema loaded** → `test-schema.sql` executed
3. **Test runs** → Isolated data operations
4. **Test ends** → Automatic cleanup/rollback

### **Common Test Patterns**

#### **Testing with Test Data**
```java
@Test
@Sql("/test-data/sample-customers.sql")  // Pre-load data
@Transactional  // Auto-rollback after test
void testWithData() {
    // Test with pre-loaded data
}
```

#### **Testing Repository Methods**
```java
@BeforeEach
void setUp() {
    jdbcTemplate.update("DELETE FROM table_name");  // Clean slate
}

@Test
void testSave() {
    // Given - Create test object
    // When - Save via repository
    // Then - Verify in database
}
```

## 🚨 Important Notes

### **Schema Changes**
1. **ALWAYS update BOTH schema files** when changing database structure
2. **Run `DatabaseSchemaConsistencyTest`** to verify synchronization
3. **Document any intentional differences** between SQLite and H2

### **Test Database vs Production**
- **Test**: H2 in-memory, recreated each run
- **Production**: SQLite file (`banking.db`)
- **Never** let tests touch production data

### **Coverage Requirements**

Coverage requirements are enforced per layer - see the **Layer-Based Coverage Analysis** section at the top of this document for details.

## 📝 Adding New Tests

### **When Adding a New Feature:**
1. Write unit tests for business logic
2. Add integration tests for database operations
3. Create controller tests for REST endpoints
4. Update schema consistency test if database changes

### **Test Naming Convention:**
```java
@Test
void methodName_condition_expectedResult() {
    // Example: saveCustomer_validData_returnsGeneratedId()
}
```

## 🔧 Troubleshooting

### **Schema Consistency Test Fails**
- **Cause**: Schemas have diverged
- **Fix**: Update both schemas to match
- **Prevention**: Always update both when changing database

### **Tests Pass Locally but Fail in CI**
- **Check**: Java version (must be 17)
- **Check**: Database initialization order
- **Check**: Test data dependencies

### **Out of Memory During Tests**
- **Increase heap**: `mvn test -Xmx1024m`
- **Check**: Cleanup in `@AfterEach` methods
- **Review**: Large dataset tests

## 🔗 Related Documentation
- [README.md](README.md) - Project overview
- [Database Schema](src/main/resources/db/schema.sql) - Production schema
- [Test Schema](src/test/resources/db/test-schema.sql) - Test schema

---
*Remember: The `DatabaseSchemaConsistencyTest` failing is a FEATURE, not a bug! It protects against schema drift.*
