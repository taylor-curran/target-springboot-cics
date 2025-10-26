# COBOL Business Rules Documentation

## Overview

This document provides comprehensive documentation of business rules extracted from all 29 COBOL programs in the CICS Banking Sample Application. Each rule includes COBOL source references, business justification, Java translation requirements, and test scenarios to guide the migration to Spring Boot.

**Source Repository:** og-cics-cobol-app/src/base/cobol_src/  
**Programs Analyzed:** 29 COBOL programs  
**Copybooks Referenced:** CUSTOMER.cpy, ACCOUNT.cpy, PROCTRAN.cpy

## Table of Contents

1. [Date Validation Rules](#1-date-validation-rules)
2. [Composite Key Patterns](#2-composite-key-patterns)
3. [Named Counter Logic](#3-named-counter-logic)
4. [Transaction Logging to PROCTRAN](#4-transaction-logging-to-proctran)
5. [Credit Scoring Integration](#5-credit-scoring-integration)
6. [Error Handling Patterns](#6-error-handling-patterns)

---

## 1. Date Validation Rules

### 1.1 CEEDAYS Minimum Year Constraint

**COBOL Source Reference:**
- File: `CRECUST.cbl`
- Lines: 1369-1379
- Section: `DATE-OF-BIRTH-CHECK`

```cobol
IF COMM-YEAR < 1601
   MOVE 'N' TO COMM-SUCCESS
   MOVE 'O' TO COMM-FAIL-CODE
   PERFORM GET-ME-OUT-OF-HERE
END-IF
```

**Business Justification:**
The CEEDAYS API in COBOL has a hard limitation that requires dates to be year 1601 or later. This is a technical constraint of the IBM Language Environment date calculation routines. Any date before 1601 cannot be processed by CEEDAYS and will cause a program failure. The fail code 'O' indicates an "Out of range" error.

**Java Translation Requirements:**
```java
// Use LocalDate validation in Java
public void validateDateOfBirth(LocalDate dateOfBirth) {
    if (dateOfBirth.getYear() < 1601) {
        throw new ValidationException("Date of birth year must be 1601 or later", "O");
    }
}
```

Implementation considerations:
- Use `java.time.LocalDate` for date handling
- Create custom `ValidationException` with fail code support
- Validate in service layer before any business logic execution
- Return consistent error response with fail code 'O'

**Test Scenarios:**
```java
@Test
void testDateOfBirthBeforeMinimumYear() {
    LocalDate dob = LocalDate.of(1600, 12, 31);
    ValidationException ex = assertThrows(ValidationException.class, 
        () -> customerService.createCustomer(customerWithDob(dob)));
    assertEquals("O", ex.getFailCode());
}

@Test
void testDateOfBirthAtMinimumYear() {
    LocalDate dob = LocalDate.of(1601, 1, 1);
    assertDoesNotThrow(() -> customerService.createCustomer(customerWithDob(dob)));
}

@Test
void testDateOfBirthValidRange() {
    LocalDate dob = LocalDate.of(1950, 6, 15);
    Customer result = customerService.createCustomer(customerWithDob(dob));
    assertNotNull(result);
    assertEquals(dob, result.getDateOfBirth());
}
```

### 1.2 Maximum Age Validation (150 Years)

**COBOL Source Reference:**
- File: `CRECUST.cbl`
- Lines: 1408-1418
- Section: `DATE-OF-BIRTH-CHECK`

```cobol
IF WS-CUSTOMER-AGE > 150
   MOVE 'N' TO COMM-SUCCESS
   MOVE 'O' TO COMM-FAIL-CODE
   PERFORM GET-ME-OUT-OF-HERE
END-IF
```

**Business Justification:**
A maximum age of 150 years is a reasonable upper bound for data quality validation. This prevents obviously incorrect dates from being entered into the system (e.g., someone born in 1800 creating an account in 2023). The fail code 'O' indicates the age is "Out of range" and unrealistic for a banking customer.

**Java Translation Requirements:**
```java
public void validateCustomerAge(LocalDate dateOfBirth) {
    LocalDate today = LocalDate.now();
    long age = ChronoUnit.YEARS.between(dateOfBirth, today);
    
    if (age > 150) {
        throw new ValidationException(
            "Customer age exceeds maximum allowed (150 years)", "O");
    }
}
```

Implementation considerations:
- Calculate age using `ChronoUnit.YEARS.between()`
- Use current system date via `LocalDate.now()`
- Apply validation before CEEDAYS-equivalent calculation
- Same fail code 'O' as year validation

**Test Scenarios:**
```java
@Test
void testDateOfBirthExceedsMaximumAge() {
    LocalDate dob = LocalDate.now().minusYears(151);
    ValidationException ex = assertThrows(ValidationException.class,
        () -> customerService.createCustomer(customerWithDob(dob)));
    assertEquals("O", ex.getFailCode());
}

@Test
void testDateOfBirthAtMaximumAge() {
    LocalDate dob = LocalDate.now().minusYears(150);
    assertDoesNotThrow(() -> customerService.createCustomer(customerWithDob(dob)));
}

@Test
void testDateOfBirthReasonableAge() {
    LocalDate dob = LocalDate.now().minusYears(30);
    Customer result = customerService.createCustomer(customerWithDob(dob));
    assertNotNull(result);
}
```

### 1.3 Future Date Rejection

**COBOL Source Reference:**
- File: `CRECUST.cbl`
- Lines: 1414-1424
- Section: `DATE-OF-BIRTH-CHECK`

```cobol
IF WS-TODAY-INT < WS-INTEGER-FORM
   MOVE 'N' TO COMM-SUCCESS
   MOVE 'Y' TO COMM-FAIL-CODE
   PERFORM GET-ME-OUT-OF-HERE
END-IF
```

**Business Justification:**
A customer cannot have a date of birth in the future. This validation ensures data integrity and prevents logical errors in the system. The fail code 'Y' specifically indicates a future date error (as opposed to 'O' for other out-of-range dates), allowing the UI to display appropriate error messages.

**Java Translation Requirements:**
```java
public void validateDateNotInFuture(LocalDate dateOfBirth) {
    LocalDate today = LocalDate.now();
    
    if (dateOfBirth.isAfter(today)) {
        throw new ValidationException(
            "Date of birth cannot be in the future", "Y");
    }
}
```

Implementation considerations:
- Use `LocalDate.isAfter()` for clear date comparison
- Different fail code 'Y' (future date) vs 'O' (out of range)
- Validate after year and age checks
- Consider timezone handling if needed

**Test Scenarios:**
```java
@Test
void testDateOfBirthInFuture() {
    LocalDate dob = LocalDate.now().plusDays(1);
    ValidationException ex = assertThrows(ValidationException.class,
        () -> customerService.createCustomer(customerWithDob(dob)));
    assertEquals("Y", ex.getFailCode());
}

@Test
void testDateOfBirthToday() {
    LocalDate dob = LocalDate.now();
    assertDoesNotThrow(() -> customerService.createCustomer(customerWithDob(dob)));
}

@Test
void testDateOfBirthInPast() {
    LocalDate dob = LocalDate.now().minusDays(1);
    Customer result = customerService.createCustomer(customerWithDob(dob));
    assertNotNull(result);
}
```

### 1.4 CEEDAYS Error Handling

**COBOL Source Reference:**
- File: `CRECUST.cbl`
- Lines: 1381-1391
- Section: `DATE-OF-BIRTH-CHECK`

```cobol
CALL 'CEEDAYS' USING COMM-YYMMDD,
                     'YYMMDD  ',
                     WS-INTEGER-FORM,
                     WS-FEEDBACK-CODE

IF WS-FEEDBACK-CODE NOT = ZEROS
   MOVE 'N' TO COMM-SUCCESS
   MOVE 'Z' TO COMM-FAIL-CODE
   PERFORM GET-ME-OUT-OF-HERE
END-IF
```

**Business Justification:**
CEEDAYS can fail for various reasons beyond simple range validation (invalid day/month combinations, leap year issues, etc.). The fail code 'Z' indicates a system-level date processing error that requires investigation. This is distinct from business validation failures ('O' and 'Y').

**Java Translation Requirements:**
```java
public void validateDateFormat(String dateString) {
    try {
        LocalDate date = LocalDate.parse(dateString, 
            DateTimeFormatter.ofPattern("yyMMdd"));
        // Additional validations...
    } catch (DateTimeParseException e) {
        throw new ValidationException(
            "Invalid date format or date calculation error", "Z");
    }
}
```

Implementation considerations:
- Catch `DateTimeParseException` for format errors
- Catch `DateTimeException` for invalid dates (e.g., Feb 30)
- Use fail code 'Z' for technical date processing errors
- Log these errors for system monitoring

**Test Scenarios:**
```java
@Test
void testInvalidDateFormat() {
    String invalidDate = "991332"; // Invalid month
    ValidationException ex = assertThrows(ValidationException.class,
        () -> customerService.parseDateOfBirth(invalidDate));
    assertEquals("Z", ex.getFailCode());
}

@Test
void testInvalidLeapYear() {
    String invalidDate = "040229"; // Feb 29, 2004 (not a leap year in all contexts)
    // Test depends on specific date parsing logic
}
```

---

## 2. Composite Key Patterns

### 2.1 Customer Composite Key

**COBOL Source Reference:**
- File: `CUSTOMER.cpy`
- Lines: 10-12

```cobol
03 CUSTOMER-KEY.
   05 CUSTOMER-SORTCODE      PIC 9(6).
   05 CUSTOMER-NUMBER        PIC 9(10).
```

**Business Justification:**
The customer key combines a 6-digit sort code (bank branch identifier) with a 10-digit customer number. This two-part key allows:
- Multiple banks/branches to share the same system
- Each branch to have independent customer numbering
- Efficient database partitioning by sort code
- Clear organizational hierarchy in data structure

**Java Translation Requirements:**
```java
@Entity
@Table(name = "customer")
@IdClass(CustomerKey.class)
public class Customer {
    @Id
    @Column(name = "customer_sortcode", length = 6)
    private String sortCode;
    
    @Id
    @Column(name = "customer_number", length = 10)
    private String customerNumber;
    
    // Other fields...
}

public class CustomerKey implements Serializable {
    private String sortCode;
    private String customerNumber;
    
    // equals() and hashCode() required
}
```

Repository pattern:
```java
public interface CustomerRepository {
    Optional<Customer> findBySortCodeAndCustomerNumber(
        String sortCode, String customerNumber);
    
    void deleteBySortCodeAndCustomerNumber(
        String sortCode, String customerNumber);
}
```

**Test Scenarios:**
```java
@Test
void testCustomerCompositeKeyUniqueness() {
    Customer c1 = createCustomer("123456", "0000000001");
    Customer c2 = createCustomer("123456", "0000000002");
    Customer c3 = createCustomer("654321", "0000000001");
    
    customerRepository.save(c1);
    customerRepository.save(c2);
    customerRepository.save(c3);
    
    assertEquals(3, customerRepository.count());
}

@Test
void testCustomerCompositeKeyRetrieval() {
    Customer saved = customerRepository.save(
        createCustomer("123456", "0000000001"));
    
    Optional<Customer> found = customerRepository
        .findBySortCodeAndCustomerNumber("123456", "0000000001");
    
    assertTrue(found.isPresent());
    assertEquals(saved.getCustomerNumber(), found.get().getCustomerNumber());
}

@Test
void testCustomerDuplicateKeyRejection() {
    Customer c1 = createCustomer("123456", "0000000001");
    Customer c2 = createCustomer("123456", "0000000001");
    
    customerRepository.save(c1);
    
    assertThrows(DataIntegrityViolationException.class,
        () -> customerRepository.save(c2));
}
```

### 2.2 Account Composite Key

**COBOL Source Reference:**
- File: `ACCOUNT.cpy`
- Lines: 11-13

```cobol
03 ACCOUNT-KEY.
   05 ACCOUNT-SORT-CODE      PIC 9(6).
   05 ACCOUNT-NUMBER         PIC 9(8).
```

**Business Justification:**
Similar to the customer key, accounts use a composite key of sort code (6 digits) and account number (8 digits). This pattern maintains consistency across the system and allows for the same organizational hierarchy. Note that account numbers are shorter (8 digits) than customer numbers (10 digits), reflecting different numbering schemes.

**Java Translation Requirements:**
```java
@Entity
@Table(name = "account")
@IdClass(AccountKey.class)
public class Account {
    @Id
    @Column(name = "account_sortcode", length = 6)
    private String sortCode;
    
    @Id
    @Column(name = "account_number", length = 8)
    private String accountNumber;
    
    @Column(name = "customer_number", length = 10)
    private String customerNumber;
    
    // Foreign key relationship
    @ManyToOne
    @JoinColumns({
        @JoinColumn(name = "account_sortcode", 
                    referencedColumnName = "customer_sortcode",
                    insertable = false, updatable = false),
        @JoinColumn(name = "customer_number",
                    referencedColumnName = "customer_number",
                    insertable = false, updatable = false)
    })
    private Customer customer;
    
    // Other fields...
}
```

**Test Scenarios:**
```java
@Test
void testAccountCompositeKeyWithCustomerRelationship() {
    Customer customer = createAndSaveCustomer("123456", "0000000001");
    Account account = createAccount("123456", "00000001", "0000000001");
    
    accountRepository.save(account);
    
    Optional<Account> found = accountRepository
        .findBySortCodeAndAccountNumber("123456", "00000001");
    
    assertTrue(found.isPresent());
    assertEquals(customer.getCustomerNumber(), 
                 found.get().getCustomerNumber());
}

@Test
void testMultipleAccountsPerCustomer() {
    Customer customer = createAndSaveCustomer("123456", "0000000001");
    
    Account checking = createAccount("123456", "00000001", "0000000001");
    Account savings = createAccount("123456", "00000002", "0000000001");
    
    accountRepository.save(checking);
    accountRepository.save(savings);
    
    List<Account> accounts = accountRepository
        .findByCustomerNumber("0000000001");
    
    assertEquals(2, accounts.size());
}
```

---

## 3. Named Counter Logic

### 3.1 ENQ (Enqueue) Resource Locking

**COBOL Source Reference:**
- File: `CRECUST.cbl`
- Lines: 441-460
- Section: `ENQ-NAMED-COUNTER`

```cobol
EXEC CICS ENQ
   RESOURCE(NCS-CUST-NO-NAME)
   LENGTH(16)
   RESP(WS-CICS-RESP)
   RESP2(WS-CICS-RESP2)
END-EXEC.

IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
   MOVE 'N' TO COMM-SUCCESS
   MOVE '3' TO COMM-FAIL-CODE
   PERFORM GET-ME-OUT-OF-HERE
END-IF
```

**Business Justification:**
The ENQ (Enqueue) operation provides exclusive locking on the Named Counter resource to ensure thread-safe ID generation. This prevents race conditions where multiple concurrent transactions might receive the same customer number. The resource name includes the sort code to allow independent counters per branch. Fail code '3' indicates ENQ failure.

**Java Translation Requirements:**
```java
@Service
public class CustomerNumberGenerator {
    private final ConcurrentHashMap<String, ReentrantLock> sortCodeLocks 
        = new ConcurrentHashMap<>();
    
    public String getNextCustomerNumber(String sortCode) {
        // Get or create lock for this sort code
        ReentrantLock lock = sortCodeLocks.computeIfAbsent(
            sortCode, k -> new ReentrantLock(true)); // fair lock
        
        boolean acquired = false;
        try {
            // Attempt to acquire lock with timeout
            acquired = lock.tryLock(5, TimeUnit.SECONDS);
            if (!acquired) {
                throw new ResourceLockException(
                    "Failed to acquire lock for customer number generation", "3");
            }
            
            // Generate next number
            return incrementAndGetCustomerNumber(sortCode);
            
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new ResourceLockException(
                "Interrupted while waiting for lock", "3");
        } finally {
            if (acquired) {
                lock.unlock();
            }
        }
    }
}
```

Alternative using database-level locking:
```java
@Repository
public class NamedCounterRepository {
    
    @Transactional
    public long getNextValue(String counterName) {
        // Use database SELECT FOR UPDATE
        String sql = "SELECT counter_value FROM named_counter " +
                     "WHERE counter_name = ? FOR UPDATE";
        
        Long currentValue = jdbcTemplate.queryForObject(
            sql, Long.class, counterName);
        
        long nextValue = currentValue + 1;
        
        jdbcTemplate.update(
            "UPDATE named_counter SET counter_value = ? WHERE counter_name = ?",
            nextValue, counterName);
        
        return nextValue;
    }
}
```

**Test Scenarios:**
```java
@Test
void testConcurrentCustomerNumberGeneration() throws Exception {
    String sortCode = "123456";
    int threadCount = 10;
    Set<String> generatedNumbers = ConcurrentHashMap.newKeySet();
    
    ExecutorService executor = Executors.newFixedThreadPool(threadCount);
    CountDownLatch latch = new CountDownLatch(threadCount);
    
    for (int i = 0; i < threadCount; i++) {
        executor.submit(() -> {
            try {
                String number = generator.getNextCustomerNumber(sortCode);
                generatedNumbers.add(number);
            } finally {
                latch.countDown();
            }
        });
    }
    
    latch.await(10, TimeUnit.SECONDS);
    executor.shutdown();
    
    // All numbers should be unique
    assertEquals(threadCount, generatedNumbers.size());
}

@Test
void testResourceLockTimeout() {
    String sortCode = "123456";
    
    // Simulate long-running transaction holding lock
    Thread blocker = new Thread(() -> {
        generator.getNextCustomerNumber(sortCode);
        // Hold lock indefinitely
        Thread.sleep(10000);
    });
    blocker.start();
    
    Thread.sleep(100); // Ensure blocker acquires lock first
    
    ResourceLockException ex = assertThrows(ResourceLockException.class,
        () -> generator.getNextCustomerNumber(sortCode));
    assertEquals("3", ex.getFailCode());
}
```

### 3.2 Named Counter Increment and Retrieval

**COBOL Source Reference:**
- File: `CRECUST.cbl`
- Lines: 461-500
- Section: `GET-NEXT-CUST-NUM`

```cobol
EXEC CICS GET COUNTER(NCS-CUST-NO-NAME)
   VALUE(NCS-CUST-NO-VALUE)
   INCREMENT(NCS-CUST-NO-INC)
   RESP(WS-CICS-RESP)
   RESP2(WS-CICS-RESP2)
END-EXEC
```

**Business Justification:**
The Named Counter provides atomic increment-and-get operation, returning the next available customer number. The counter is incremented by 1 each time. If the operation fails after ENQ but before successful write, the counter must be decremented to avoid gaps in numbering (see rollback logic).

**Java Translation Requirements:**
```java
private String incrementAndGetCustomerNumber(String sortCode) {
    String counterName = "HBNKCUST" + sortCode;
    
    // Atomic increment in database
    int rowsUpdated = jdbcTemplate.update(
        "UPDATE control SET counter_value = counter_value + 1 " +
        "WHERE counter_name = ?",
        counterName);
    
    if (rowsUpdated == 0) {
        // Counter doesn't exist, initialize it
        jdbcTemplate.update(
            "INSERT INTO control (counter_name, counter_value) VALUES (?, ?)",
            counterName, 1L);
        return String.format("%010d", 1);
    }
    
    // Retrieve the new value
    Long newValue = jdbcTemplate.queryForObject(
        "SELECT counter_value FROM control WHERE counter_name = ?",
        Long.class, counterName);
    
    return String.format("%010d", newValue);
}
```

**Test Scenarios:**
```java
@Test
void testSequentialNumberGeneration() {
    String sortCode = "123456";
    
    String num1 = generator.getNextCustomerNumber(sortCode);
    String num2 = generator.getNextCustomerNumber(sortCode);
    String num3 = generator.getNextCustomerNumber(sortCode);
    
    assertEquals("0000000001", num1);
    assertEquals("0000000002", num2);
    assertEquals("0000000003", num3);
}

@Test
void testIndependentCountersPerSortCode() {
    String num1 = generator.getNextCustomerNumber("123456");
    String num2 = generator.getNextCustomerNumber("654321");
    
    // Both should start at 1
    assertEquals("0000000001", num1);
    assertEquals("0000000001", num2);
}
```

### 3.3 DEQ (Dequeue) Resource Unlocking

**COBOL Source Reference:**
- File: `CRECUST.cbl`
- Lines: 1104
- Section: `WRITE-CUSTOMER-VSAM`

```cobol
PERFORM DEQ-NAMED-COUNTER.
```

**Business Justification:**
DEQ releases the lock acquired by ENQ, allowing other transactions to generate customer numbers. This must always be called, even in error scenarios, to prevent deadlocks. The COBOL code shows DEQ being called both in success path and error handling routines.

**Java Translation Requirements:**
```java
// Using ReentrantLock (shown in ENQ section)
// The lock.unlock() in finally block serves as DEQ

// Alternative pattern with Spring's @Transactional
@Service
public class CustomerService {
    
    @Transactional(rollbackFor = Exception.class)
    public Customer createCustomer(CustomerRequest request) {
        try {
            String customerNumber = numberGenerator
                .getNextCustomerNumber(request.getSortCode());
            
            Customer customer = buildCustomer(request, customerNumber);
            customerRepository.save(customer);
            
            // Transaction commit automatically "DEQs"
            return customer;
            
        } catch (Exception e) {
            // Transaction rollback automatically "DEQs" and
            // can be configured to rollback counter increment
            throw e;
        }
    }
}
```

**Test Scenarios:**
```java
@Test
void testLockReleasedOnSuccess() {
    String sortCode = "123456";
    
    String num1 = generator.getNextCustomerNumber(sortCode);
    // If lock not released, this would timeout
    String num2 = generator.getNextCustomerNumber(sortCode);
    
    assertNotEquals(num1, num2);
}

@Test
void testLockReleasedOnException() {
    String sortCode = "123456";
    
    // Force an exception during generation
    generator.injectFault(true);
    
    assertThrows(RuntimeException.class,
        () -> generator.getNextCustomerNumber(sortCode));
    
    generator.injectFault(false);
    
    // Lock should be released, this should succeed
    String num = generator.getNextCustomerNumber(sortCode);
    assertNotNull(num);
}
```

### 3.4 Counter Rollback on Failure

**COBOL Source Reference:**
- File: `CREACC.cbl`
- Lines: 17-19

```cobol
* If for any reason the write to the ACCOUNT or PROCTRAN
* datastore is unsuccessful, then we need to decrement the Named
* Counter (restoring it to the start position) and DEQUEUE
```

**Business Justification:**
If customer/account creation fails after incrementing the counter but before successful database write, the counter must be decremented to avoid gaps in numbering. This ensures sequential numbering is maintained even in failure scenarios.

**Java Translation Requirements:**
```java
@Transactional(rollbackFor = Exception.class)
public Customer createCustomerWithRollback(CustomerRequest request) {
    String sortCode = request.getSortCode();
    String customerNumber = null;
    
    try {
        // Increment counter
        customerNumber = numberGenerator.getNextCustomerNumber(sortCode);
        
        // Create customer
        Customer customer = buildCustomer(request, customerNumber);
        customerRepository.save(customer);
        
        // Write PROCTRAN
        proctranService.logCustomerCreation(customer);
        
        return customer;
        
    } catch (Exception e) {
        // Rollback counter if we incremented it
        if (customerNumber != null) {
            numberGenerator.decrementCounter(sortCode);
        }
        throw e;
    }
}
```

**Test Scenarios:**
```java
@Test
void testCounterRollbackOnDatabaseFailure() {
    String sortCode = "123456";
    
    // Get initial counter value
    String num1 = generator.getNextCustomerNumber(sortCode);
    
    // Simulate database failure during customer creation
    customerRepository.injectFault(true);
    
    try {
        customerService.createCustomer(request);
        fail("Expected exception");
    } catch (Exception e) {
        // Expected
    }
    
    customerRepository.injectFault(false);
    
    // Next number should be num1 + 1, not num1 + 2
    // (counter was rolled back)
    String num2 = generator.getNextCustomerNumber(sortCode);
    assertEquals(Long.parseLong(num1) + 1, Long.parseLong(num2));
}
```

---

## 4. Transaction Logging to PROCTRAN

### 4.1 PROCTRAN Structure and Transaction Types

**COBOL Source Reference:**
- File: `PROCTRAN.cpy`
- Lines: 1-104

```cobol
03 PROC-TRAN-TYPE              PIC X(3).
   88 PROC-TYPE-DEBIT-CHAPS           VALUE 'CHA'.
   88 PROC-TYPE-DEBIT-CHAPS-FOREIGN   VALUE 'CHF'.
   88 PROC-TYPE-DEBIT-CHAPS-INT       VALUE 'CHI'.
   88 PROC-TYPE-DEBIT-CHAPS-OTHER     VALUE 'CHO'.
   88 PROC-TYPE-CREDIT                VALUE 'CRE'.
   88 PROC-TYPE-DEBIT                 VALUE 'DEB'.
   88 PROC-TYPE-INQ-ACC-DB2           VALUE 'ICA'.
   88 PROC-TYPE-INQ-CUST-DB2          VALUE 'ICC'.
   88 PROC-TYPE-DEL-ACC-DB2           VALUE 'IDA'.
   88 PROC-TYPE-DEL-CUST-DB2          VALUE 'IDC'.
   88 PROC-TYPE-OPENACC-DB2           VALUE 'OCA'.
   88 PROC-TYPE-OPENCUST-DB2          VALUE 'OCC'.
   88 PROC-TYPE-DELACC-DB2            VALUE 'ODA'.
   88 PROC-TYPE-DELCUST-DB2           VALUE 'ODC'.
   88 PROC-TYPE-OPENCUST-SETUP        VALUE 'OCS'.
   88 PROC-TYPE-CREDIT-PENDING        VALUE 'PCR'.
   88 PROC-TYPE-DEBIT-PENDING         VALUE 'PDR'.
   88 PROC-TYPE-TRANSFER              VALUE 'TFR'.
```

**Business Justification:**
PROCTRAN (Processed Transaction) provides an audit trail of all banking operations. Each transaction type has a specific 3-character code that categorizes the operation. This allows for:
- Comprehensive audit logging
- Transaction reporting and analytics
- Regulatory compliance (e.g., SOX, PCI-DSS)
- Debugging and troubleshooting
- Customer transaction history

**Java Translation Requirements:**
```java
public enum TransactionType {
    CHA("DEBIT_CHAPS", "Debit via CHAPS"),
    CHF("DEBIT_CHAPS_FOREIGN", "Debit via CHAPS (Foreign)"),
    CHI("DEBIT_CHAPS_INT", "Debit via CHAPS (International)"),
    CHO("DEBIT_CHAPS_OTHER", "Debit via CHAPS (Other)"),
    CRE("CREDIT", "Credit Transaction"),
    DEB("DEBIT", "Debit Transaction"),
    ICA("INQUIRY_ACCOUNT", "Account Inquiry"),
    ICC("INQUIRY_CUSTOMER", "Customer Inquiry"),
    IDA("INQUIRY_DEL_ACCOUNT", "Account Deletion Inquiry"),
    IDC("INQUIRY_DEL_CUSTOMER", "Customer Deletion Inquiry"),
    OCA("OPEN_ACCOUNT", "Account Creation"),
    OCC("OPEN_CUSTOMER", "Customer Creation"),
    ODA("DELETE_ACCOUNT", "Account Deletion"),
    ODC("DELETE_CUSTOMER", "Customer Deletion"),
    OCS("OPEN_CUSTOMER_SETUP", "Customer Setup"),
    PCR("PENDING_CREDIT", "Pending Credit"),
    PDR("PENDING_DEBIT", "Pending Debit"),
    TFR("TRANSFER", "Transfer");
    
    private final String name;
    private final String description;
    
    TransactionType(String name, String description) {
        this.name = name;
        this.description = description;
    }
}

@Entity
@Table(name = "bank_transaction")
public class Transaction {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(name = "eyecatcher", length = 4)
    private String eyeCatcher = "PRTR";
    
    @Column(name = "sort_code", length = 6)
    private String sortCode;
    
    @Column(name = "account_number", length = 8)
    private String accountNumber;
    
    @Column(name = "transaction_date", length = 10)
    private String transactionDate;
    
    @Column(name = "transaction_time", length = 6)
    private String transactionTime;
    
    @Column(name = "transaction_ref", length = 12)
    private String transactionRef;
    
    @Enumerated(EnumType.STRING)
    @Column(name = "transaction_type", length = 3)
    private TransactionType transactionType;
    
    @Column(name = "description", length = 40)
    private String description;
    
    @Column(name = "amount", precision = 12, scale = 2)
    private BigDecimal amount;
}
```

**Test Scenarios:**
```java
@Test
void testTransactionTypeMapping() {
    assertEquals("OCC", TransactionType.OCC.toString());
    assertEquals("Customer Creation", 
                 TransactionType.OCC.getDescription());
}

@Test
void testAllTransactionTypesHaveUniqueCodesAndNames() {
    Set<String> codes = new HashSet<>();
    Set<String> names = new HashSet<>();
    
    for (TransactionType type : TransactionType.values()) {
        assertTrue(codes.add(type.toString()), 
                   "Duplicate code: " + type.toString());
        assertTrue(names.add(type.getName()), 
                   "Duplicate name: " + type.getName());
    }
    
    assertEquals(18, codes.size());
}
```

### 4.2 Customer Creation Logging (OCC)

**COBOL Source Reference:**
- File: `CRECUST.cbl`
- Lines: 1121-1193
- Section: `WRITE-PROCTRAN-DB2`

```cobol
MOVE 'PRTR' TO HV-PROCTRAN-EYECATCHER.
MOVE SORTCODE TO HV-PROCTRAN-SORT-CODE.
MOVE ZEROS TO HV-PROCTRAN-ACC-NUMBER.
MOVE EIBTASKN TO WS-EIBTASKN12.
MOVE WS-EIBTASKN12 TO HV-PROCTRAN-REF.

MOVE STORED-SORTCODE TO HV-PROCTRAN-DESC(1:6).
MOVE STORED-CUSTNO TO HV-PROCTRAN-DESC(7:10).
MOVE STORED-NAME   TO HV-PROCTRAN-DESC(17:14).
MOVE STORED-DOB    TO HV-PROCTRAN-DESC(31:10).

MOVE 'OCC' TO HV-PROCTRAN-TYPE.
MOVE ZEROS TO HV-PROCTRAN-AMOUNT.
```

**Business Justification:**
Customer creation (OCC = Open Customer) must be logged for audit purposes. The log includes the sort code, customer number, name, and date of birth in the description field. The transaction reference is the CICS task number for traceability. Amount is zero since no money is involved.

**Java Translation Requirements:**
```java
@Service
public class TransactionLoggingService {
    
    public void logCustomerCreation(Customer customer) {
        Transaction transaction = new Transaction();
        transaction.setEyeCatcher("PRTR");
        transaction.setSortCode(customer.getSortCode());
        transaction.setAccountNumber("00000000"); // Zero for customer ops
        transaction.setTransactionDate(formatDate(LocalDate.now()));
        transaction.setTransactionTime(formatTime(LocalTime.now()));
        transaction.setTransactionRef(generateTransactionRef());
        transaction.setTransactionType(TransactionType.OCC);
        transaction.setAmount(BigDecimal.ZERO);
        
        // Build description: sortcode + custno + name + dob
        String description = String.format("%-6s%-10s%-14s%-10s",
            customer.getSortCode(),
            customer.getCustomerNumber(),
            truncate(customer.getName(), 14),
            formatDate(customer.getDateOfBirth()));
        transaction.setDescription(description);
        
        transactionRepository.save(transaction);
    }
    
    private String formatDate(LocalDate date) {
        return date.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"));
    }
    
    private String formatTime(LocalTime time) {
        return time.format(DateTimeFormatter.ofPattern("HHmmss"));
    }
    
    private String generateTransactionRef() {
        // Use timestamp or UUID or sequential ID
        return String.format("%012d", System.currentTimeMillis() % 1000000000000L);
    }
}
```

**Test Scenarios:**
```java
@Test
void testCustomerCreationLogging() {
    Customer customer = createCustomer("123456", "0000000001");
    customer.setName("John Smith");
    customer.setDateOfBirth(LocalDate.of(1990, 5, 15));
    
    loggingService.logCustomerCreation(customer);
    
    List<Transaction> transactions = transactionRepository
        .findBySortCodeAndAccountNumber("123456", "00000000");
    
    assertEquals(1, transactions.size());
    Transaction tx = transactions.get(0);
    assertEquals(TransactionType.OCC, tx.getTransactionType());
    assertEquals(BigDecimal.ZERO, tx.getAmount());
    assertTrue(tx.getDescription().contains("123456"));
    assertTrue(tx.getDescription().contains("0000000001"));
}

@Test
void testTransactionReferenceUniqueness() {
    Customer c1 = createCustomer("123456", "0000000001");
    Customer c2 = createCustomer("123456", "0000000002");
    
    loggingService.logCustomerCreation(c1);
    loggingService.logCustomerCreation(c2);
    
    List<Transaction> transactions = transactionRepository
        .findBySortCode("123456");
    
    Set<String> refs = transactions.stream()
        .map(Transaction::getTransactionRef)
        .collect(Collectors.toSet());
    
    assertEquals(2, refs.size());
}
```

### 4.3 Account Operations Logging

**COBOL Source Reference:**
- File: `CREACC.cbl`, `DELACC.cbl`, `DBCRFUN.cbl`, `XFRFUN.cbl`

Account creation, deletion, debit, credit, and transfer operations all write to PROCTRAN with appropriate transaction types (OCA, ODA, DEB, CRE, TFR).

**Business Justification:**
All account operations that modify balances or account state must be logged for:
- Regulatory compliance (financial audit trail)
- Customer statement generation
- Fraud detection
- Reconciliation

**Java Translation Requirements:**
```java
public void logAccountCreation(Account account) {
    Transaction tx = buildBaseTransaction(account.getSortCode(), 
                                          account.getAccountNumber());
    tx.setTransactionType(TransactionType.OCA);
    tx.setAmount(account.getActualBalance());
    tx.setDescription(buildAccountDescription(account));
    transactionRepository.save(tx);
}

public void logDebit(Account account, BigDecimal amount, String reference) {
    Transaction tx = buildBaseTransaction(account.getSortCode(), 
                                          account.getAccountNumber());
    tx.setTransactionType(TransactionType.DEB);
    tx.setAmount(amount);
    tx.setDescription("Debit: " + reference);
    transactionRepository.save(tx);
}

public void logCredit(Account account, BigDecimal amount, String reference) {
    Transaction tx = buildBaseTransaction(account.getSortCode(), 
                                          account.getAccountNumber());
    tx.setTransactionType(TransactionType.CRE);
    tx.setAmount(amount);
    tx.setDescription("Credit: " + reference);
    transactionRepository.save(tx);
}

public void logTransfer(Account fromAccount, Account toAccount, 
                       BigDecimal amount) {
    // Log debit on source account
    Transaction debit = buildBaseTransaction(fromAccount.getSortCode(), 
                                            fromAccount.getAccountNumber());
    debit.setTransactionType(TransactionType.TFR);
    debit.setAmount(amount.negate());
    debit.setDescription("Transfer to " + toAccount.getAccountNumber());
    transactionRepository.save(debit);
    
    // Log credit on destination account
    Transaction credit = buildBaseTransaction(toAccount.getSortCode(), 
                                             toAccount.getAccountNumber());
    credit.setTransactionType(TransactionType.TFR);
    credit.setAmount(amount);
    credit.setDescription("Transfer from " + fromAccount.getAccountNumber());
    transactionRepository.save(credit);
}
```

**Test Scenarios:**
```java
@Test
void testAccountOperationsCreateTransactionLog() {
    Account account = createAndSaveAccount();
    
    accountService.debit(account, new BigDecimal("100.00"), "ATM withdrawal");
    accountService.credit(account, new BigDecimal("500.00"), "Salary deposit");
    
    List<Transaction> transactions = transactionRepository
        .findByAccountNumber(account.getAccountNumber());
    
    assertEquals(2, transactions.size());
    assertTrue(transactions.stream()
        .anyMatch(tx -> tx.getTransactionType() == TransactionType.DEB));
    assertTrue(transactions.stream()
        .anyMatch(tx -> tx.getTransactionType() == TransactionType.CRE));
}

@Test
void testTransferCreatesTransactionsOnBothAccounts() {
    Account from = createAndSaveAccount("123456", "00000001");
    Account to = createAndSaveAccount("123456", "00000002");
    
    accountService.transfer(from, to, new BigDecimal("250.00"));
    
    List<Transaction> fromTxs = transactionRepository
        .findByAccountNumber(from.getAccountNumber());
    List<Transaction> toTxs = transactionRepository
        .findByAccountNumber(to.getAccountNumber());
    
    assertEquals(1, fromTxs.size());
    assertEquals(1, toTxs.size());
    assertEquals(TransactionType.TFR, fromTxs.get(0).getTransactionType());
    assertEquals(TransactionType.TFR, toTxs.get(0).getTransactionType());
}
```

### 4.4 Operations That Do NOT Log to PROCTRAN

**COBOL Source Reference:**
- File: `UPDCUST.cbl`
- Lines: 15-17
- File: `UPDACC.cbl`
- Lines: 20-22

```cobol
* Because it is only permissible to change a limited number of
* fields on the Customer record, no record needs to be written to
* PROCTRAN.
```

**Business Justification:**
Customer and account updates that only modify non-financial fields (e.g., address, phone number, interest rate) do NOT write to PROCTRAN. This is because:
- These changes don't affect account balances
- They're not part of the financial audit trail
- Reduces transaction log volume
- PROCTRAN is specifically for financial transactions

Fields that can be updated without PROCTRAN logging:
- Customer: Address, phone number, credit score review date
- Account: Account type, interest rate, overdraft limit, statement dates

**Java Translation Requirements:**
```java
@Service
public class CustomerService {
    
    @Transactional
    public Customer updateCustomerDetails(String sortCode, 
                                         String customerNumber,
                                         CustomerUpdateRequest request) {
        Customer customer = customerRepository
            .findBySortCodeAndCustomerNumber(sortCode, customerNumber)
            .orElseThrow(() -> new CustomerNotFoundException());
        
        // Update allowed fields (no PROCTRAN logging)
        if (request.getAddress() != null) {
            customer.setAddress(request.getAddress());
        }
        if (request.getPhoneNumber() != null) {
            customer.setPhoneNumber(request.getPhoneNumber());
        }
        
        // NOTE: No transaction logging for these updates
        return customerRepository.save(customer);
    }
}

@Service
public class AccountService {
    
    @Transactional
    public Account updateAccountDetails(String sortCode,
                                       String accountNumber,
                                       AccountUpdateRequest request) {
        Account account = accountRepository
            .findBySortCodeAndAccountNumber(sortCode, accountNumber)
            .orElseThrow(() -> new AccountNotFoundException());
        
        // Update allowed fields (no PROCTRAN logging)
        if (request.getAccountType() != null) {
            account.setAccountType(request.getAccountType());
        }
        if (request.getInterestRate() != null) {
            account.setInterestRate(request.getInterestRate());
        }
        if (request.getOverdraftLimit() != null) {
            account.setOverdraftLimit(request.getOverdraftLimit());
        }
        
        // NOTE: No transaction logging for these updates
        return accountRepository.save(account);
    }
}
```

**Test Scenarios:**
```java
@Test
void testCustomerUpdateDoesNotCreateTransaction() {
    Customer customer = createAndSaveCustomer();
    
    int txCountBefore = transactionRepository.countBySortCode(
        customer.getSortCode());
    
    customerService.updateCustomerDetails(
        customer.getSortCode(),
        customer.getCustomerNumber(),
        CustomerUpdateRequest.builder()
            .address("123 New Street")
            .phoneNumber("555-0123")
            .build());
    
    int txCountAfter = transactionRepository.countBySortCode(
        customer.getSortCode());
    
    assertEquals(txCountBefore, txCountAfter);
}

@Test
void testAccountUpdateDoesNotCreateTransaction() {
    Account account = createAndSaveAccount();
    
    int txCountBefore = transactionRepository.countByAccountNumber(
        account.getAccountNumber());
    
    accountService.updateAccountDetails(
        account.getSortCode(),
        account.getAccountNumber(),
        AccountUpdateRequest.builder()
            .interestRate(new BigDecimal("2.5"))
            .overdraftLimit(1000)
            .build());
    
    int txCountAfter = transactionRepository.countByAccountNumber(
        account.getAccountNumber());
    
    assertEquals(txCountBefore, txCountAfter);
}
```

---

## 5. Credit Scoring Integration

### 5.1 Async API Credit Agency Calls

**COBOL Source Reference:**
- File: `CRECUST.cbl`
- Lines: 550-616
- Section: `CREDIT-SCORE-CHECK`

```cobol
PERFORM VARYING WS-CHILD-COUNTER FROM 1 BY 1
   UNTIL WS-CHILD-COUNTER > WS-NUM-OF-CHILDREN

   EXEC CICS RUN TRANSID(WS-CHILD-PROGRAM)
      CHANNEL(WS-CHANNEL-NAME)
      CHILD
      RESP(WS-CICS-RESP)
      RESP2(WS-CICS-RESP2)
   END-EXEC
END-PERFORM
```

**Business Justification:**
The system calls multiple credit agencies (up to 9) asynchronously to obtain credit scores for new customers. Async execution allows:
- Parallel processing of multiple agency requests
- Reduced total wait time (vs sequential calls)
- Resilience to slow-responding agencies
- Better system throughput

**Java Translation Requirements:**
```java
@Service
public class CreditScoringService {
    
    private final List<CreditAgencyClient> creditAgencies;
    private final ExecutorService executorService;
    
    @Async
    public CompletableFuture<Integer> getCreditScore(Customer customer) {
        List<CompletableFuture<CreditAgencyResponse>> futures = 
            creditAgencies.stream()
                .map(agency -> CompletableFuture.supplyAsync(
                    () -> agency.requestCreditScore(customer),
                    executorService))
                .collect(Collectors.toList());
        
        // Wait up to 3 seconds for responses
        try {
            List<CreditAgencyResponse> responses = futures.stream()
                .map(f -> {
                    try {
                        return f.get(3, TimeUnit.SECONDS);
                    } catch (TimeoutException e) {
                        return null; // Agency didn't respond in time
                    } catch (Exception e) {
                        logger.warn("Credit agency error", e);
                        return null;
                    }
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
            
            if (responses.isEmpty()) {
                throw new CreditCheckException(
                    "No credit agencies responded", "C");
            }
            
            return CompletableFuture.completedFuture(
                calculateAverageScore(responses));
            
        } catch (Exception e) {
            throw new CreditCheckException("Credit check failed", "C");
        }
    }
}
```

Alternative using Spring's RestTemplate with timeout:
```java
@Service
public class CreditAgencyClient {
    
    private final RestTemplate restTemplate;
    
    public CreditAgencyClient() {
        this.restTemplate = new RestTemplate();
        
        // Configure timeout
        HttpComponentsClientHttpRequestFactory factory = 
            new HttpComponentsClientHttpRequestFactory();
        factory.setConnectTimeout(3000);
        factory.setReadTimeout(3000);
        restTemplate.setRequestFactory(factory);
    }
    
    public CreditAgencyResponse requestCreditScore(Customer customer) {
        CreditScoreRequest request = buildRequest(customer);
        
        try {
            return restTemplate.postForObject(
                agencyUrl + "/credit-score",
                request,
                CreditAgencyResponse.class);
        } catch (RestClientException e) {
            logger.warn("Credit agency request failed", e);
            return null;
        }
    }
}
```

**Test Scenarios:**
```java
@Test
void testAsyncCreditAgencyCalls() throws Exception {
    Customer customer = createCustomer();
    
    // Mock multiple agencies
    when(agency1.requestCreditScore(any())).thenReturn(
        new CreditAgencyResponse(750));
    when(agency2.requestCreditScore(any())).thenReturn(
        new CreditAgencyResponse(780));
    when(agency3.requestCreditScore(any())).thenReturn(
        new CreditAgencyResponse(760));
    
    CompletableFuture<Integer> future = 
        creditScoringService.getCreditScore(customer);
    
    Integer score = future.get(5, TimeUnit.SECONDS);
    
    // Average of 750, 780, 760 = 763
    assertEquals(763, score.intValue());
}

@Test
void testCreditAgencyTimeout() throws Exception {
    Customer customer = createCustomer();
    
    // Mock agency that takes too long
    when(agency1.requestCreditScore(any())).thenAnswer(invocation -> {
        Thread.sleep(5000); // Exceeds 3-second timeout
        return new CreditAgencyResponse(750);
    });
    
    when(agency2.requestCreditScore(any())).thenReturn(
        new CreditAgencyResponse(780));
    
    CompletableFuture<Integer> future = 
        creditScoringService.getCreditScore(customer);
    
    Integer score = future.get(5, TimeUnit.SECONDS);
    
    // Should only use agency2's score since agency1 timed out
    assertEquals(780, score.intValue());
}
```

### 5.2 Three-Second Wait and Response Aggregation

**COBOL Source Reference:**
- File: `CRECUST.cbl`
- Lines: 619-700

```cobol
EXEC CICS DELAY
   FOR SECONDS(3)
END-EXEC.

MOVE 'N' TO WS-FINISHED-FETCHING.
MOVE 0 TO WS-RETRIEVED-CNT.
MOVE 0 TO WS-TOTAL-CS-SCR.

PERFORM UNTIL WS-FINISHED-FETCHING = 'Y'
   EXEC CICS FETCH ANY(WS-ANY-CHILD-FETCH-TKN)
      CHANNEL(WS-ANY-CHILD-FETCH-CHAN)
      NOSUSPEND
      COMPSTATUS(WS-CHILD-FETCH-COMPST)
      ABCODE(WS-ANY-CHILD-FETCH-ABCODE)
      RESP(WS-CICS-RESP)
      RESP2(WS-CICS-RESP2)
   END-EXEC
   
   * Aggregate scores
   ADD WS-CHILD-CREDIT-SCORE TO WS-TOTAL-CS-SCR
   ADD 1 TO WS-RETRIEVED-CNT
END-PERFORM

* Calculate average
COMPUTE WS-ACTUAL-CS-SCR = WS-TOTAL-CS-SCR / WS-RETRIEVED-CNT
```

**Business Justification:**
The 3-second wait represents a trade-off between:
- Gathering enough credit agency responses for accuracy
- Maintaining acceptable response times for user experience
- Not blocking system resources indefinitely

Agencies that don't respond within 3 seconds are excluded from the average, but the operation continues if at least one agency responds.

**Java Translation Requirements:**
```java
private Integer calculateAverageScore(
        List<CreditAgencyResponse> responses) {
    
    if (responses.isEmpty()) {
        return 0;
    }
    
    int total = responses.stream()
        .mapToInt(CreditAgencyResponse::getScore)
        .sum();
    
    return total / responses.size();
}

// Alternative with timeout handling
public Integer getCreditScoreWithTimeout(Customer customer) {
    List<CompletableFuture<Integer>> futures = creditAgencies.stream()
        .map(agency -> CompletableFuture.supplyAsync(
            () -> agency.requestCreditScore(customer).getScore(),
            executorService))
        .collect(Collectors.toList());
    
    // Create a combined future with timeout
    CompletableFuture<Void> allOf = CompletableFuture.allOf(
        futures.toArray(new CompletableFuture[0]));
    
    try {
        // Wait 3 seconds for all to complete
        allOf.get(3, TimeUnit.SECONDS);
    } catch (TimeoutException e) {
        // Some futures didn't complete, that's OK
        logger.info("Some credit agencies did not respond in time");
    } catch (Exception e) {
        logger.error("Error waiting for credit agencies", e);
    }
    
    // Collect completed scores
    List<Integer> scores = futures.stream()
        .filter(CompletableFuture::isDone)
        .map(f -> {
            try {
                return f.getNow(null);
            } catch (Exception e) {
                return null;
            }
        })
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
    
    if (scores.isEmpty()) {
        throw new CreditCheckException(
            "No credit agencies responded in time", "C");
    }
    
    return (int) scores.stream()
        .mapToInt(Integer::intValue)
        .average()
        .orElse(0);
}
```

**Test Scenarios:**
```java
@Test
void testAverageScoreCalculation() {
    List<CreditAgencyResponse> responses = Arrays.asList(
        new CreditAgencyResponse(700),
        new CreditAgencyResponse(750),
        new CreditAgencyResponse(800)
    );
    
    int average = creditScoringService.calculateAverageScore(responses);
    
    assertEquals(750, average);
}

@Test
void testPartialResponseHandling() throws Exception {
    Customer customer = createCustomer();
    
    // Agency 1: fast response
    when(agency1.requestCreditScore(any())).thenReturn(
        new CreditAgencyResponse(750));
    
    // Agency 2: slow response (exceeds timeout)
    when(agency2.requestCreditScore(any())).thenAnswer(invocation -> {
        Thread.sleep(5000);
        return new CreditAgencyResponse(800);
    });
    
    // Agency 3: fast response
    when(agency3.requestCreditScore(any())).thenReturn(
        new CreditAgencyResponse(780));
    
    Integer score = creditScoringService
        .getCreditScoreWithTimeout(customer);
    
    // Average of only the two fast responses
    assertEquals(765, score.intValue());
}
```

### 5.3 No Response Handling (Score = 0)

**COBOL Source Reference:**
- File: `CRECUST.cbl`
- Lines: 664-683

```cobol
IF WS-RETRIEVED-CNT = 0
   MOVE 'Y' TO WS-FINISHED-FETCHING
   MOVE 0 TO COMM-CREDIT-SCORE
   MOVE 'Y' TO WS-CREDIT-CHECK-ERROR
   
   STRING WS-ORIG-DATE-DD DELIMITED BY SIZE,
          WS-ORIG-DATE-MM DELIMITED BY SIZE,
          WS-ORIG-DATE-YYYY DELIMITED BY SIZE
          INTO COMM-CS-REVIEW-DATE
   END-STRING
   
   MOVE 'N' TO COMM-SUCCESS
   MOVE 'C' TO COMM-FAIL-CODE
```

**Business Justification:**
If no credit agencies respond within the timeout, the customer creation fails with fail code 'C' (Credit check error). This is a critical validation - the system requires at least one credit score to create a customer. Setting score to 0 and review date to today signals that manual review is required.

**Java Translation Requirements:**
```java
public Customer createCustomerWithCreditCheck(CustomerRequest request) {
    Customer customer = buildCustomer(request);
    
    try {
        Integer creditScore = creditScoringService
            .getCreditScoreWithTimeout(customer);
        
        if (creditScore == null || creditScore == 0) {
            customer.setCreditScore(0);
            customer.setCreditScoreReviewDate(LocalDate.now());
            throw new CreditCheckException(
                "No credit score available - manual review required", "C");
        }
        
        customer.setCreditScore(creditScore);
        customer.setCreditScoreReviewDate(
            LocalDate.now().plusDays(generateRandomDays(1, 21)));
        
        return customerRepository.save(customer);
        
    } catch (TimeoutException | ExecutionException e) {
        customer.setCreditScore(0);
        customer.setCreditScoreReviewDate(LocalDate.now());
        throw new CreditCheckException(
            "Credit check failed - no agencies responded", "C");
    }
}
```

**Test Scenarios:**
```java
@Test
void testNoAgencyResponseFailsCreation() {
    Customer customer = createCustomer();
    
    // All agencies timeout
    when(agency1.requestCreditScore(any())).thenAnswer(invocation -> {
        Thread.sleep(5000);
        return null;
    });
    when(agency2.requestCreditScore(any())).thenAnswer(invocation -> {
        Thread.sleep(5000);
        return null;
    });
    
    CreditCheckException ex = assertThrows(CreditCheckException.class,
        () -> customerService.createCustomerWithCreditCheck(request));
    
    assertEquals("C", ex.getFailCode());
}

@Test
void testZeroScoreHandling() {
    CustomerRequest request = new CustomerRequest();
    request.setCreditScore(0);
    
    CreditCheckException ex = assertThrows(CreditCheckException.class,
        () -> customerService.createCustomerWithCreditCheck(request));
    
    assertEquals("C", ex.getFailCode());
}
```

### 5.4 Credit Score Review Date Calculation

**COBOL Source Reference:**
- File: `CRECUST.cbl`
- Lines: 817-843

```cobol
MOVE EIBTASKN TO WS-SEED

COMPUTE WS-REVIEW-DATE-ADD = ((21 - 1)
    * FUNCTION RANDOM(WS-SEED)) + 1

COMPUTE WS-NEW-REVIEW-DATE-INT =
    WS-TODAY-INT + WS-REVIEW-DATE-ADD

COMPUTE WS-NEW-REVIEW-YYYYMMDD = FUNCTION
    DATE-OF-INTEGER (WS-NEW-REVIEW-DATE-INT)
```

**Business Justification:**
After a successful credit check, the system schedules a review date randomly within the next 21 days. This random scheduling:
- Distributes review workload evenly over time
- Prevents all reviews from clustering on the same date
- Aligns with regulatory requirements for periodic credit checks
- Allows flexibility for operational planning

**Java Translation Requirements:**
```java
public LocalDate calculateCreditScoreReviewDate() {
    Random random = new Random();
    int daysToAdd = random.nextInt(21) + 1; // 1 to 21 days
    return LocalDate.now().plusDays(daysToAdd);
}

// More sophisticated version with seed for testing
public LocalDate calculateCreditScoreReviewDate(long seed) {
    Random random = new Random(seed);
    int daysToAdd = random.nextInt(21) + 1;
    return LocalDate.now().plusDays(daysToAdd);
}

// Production usage
public void setCreditScoreReviewDate(Customer customer, Integer creditScore) {
    if (creditScore > 0) {
        customer.setCreditScoreReviewDate(
            calculateCreditScoreReviewDate());
    } else {
        // Manual review required immediately
        customer.setCreditScoreReviewDate(LocalDate.now());
    }
}
```

**Test Scenarios:**
```java
@Test
void testReviewDateWithinRange() {
    LocalDate today = LocalDate.now();
    LocalDate reviewDate = creditScoringService
        .calculateCreditScoreReviewDate();
    
    assertTrue(reviewDate.isAfter(today));
    assertTrue(reviewDate.isBefore(today.plusDays(22)));
    assertTrue(reviewDate.isAfter(today.minusDays(1)));
}

@Test
void testReviewDateDistribution() {
    Set<LocalDate> dates = new HashSet<>();
    
    // Generate 100 review dates
    for (int i = 0; i < 100; i++) {
        dates.add(creditScoringService
            .calculateCreditScoreReviewDate());
    }
    
    // Should have multiple unique dates (not all the same)
    assertTrue(dates.size() > 1);
}

@Test
void testDeterministicReviewDateWithSeed() {
    long seed = 12345L;
    
    LocalDate date1 = creditScoringService
        .calculateCreditScoreReviewDate(seed);
    LocalDate date2 = creditScoringService
        .calculateCreditScoreReviewDate(seed);
    
    assertEquals(date1, date2);
}
```

### 5.5 Credit Agency Simulation (Random Scores and Delays)

**COBOL Source Reference:**
- File: `CRDTAGY1.cbl`
- Lines: 114-217

```cobol
* Generate random delay between 0 and 3 seconds
COMPUTE WS-DELAY-AMT = ((3 - 1)
    * FUNCTION RANDOM(WS-SEED)) + 1.

EXEC CICS DELAY
   FOR SECONDS(WS-DELAY-AMT)
END-EXEC.

* Generate random credit score between 1 and 999
COMPUTE WS-NEW-CREDSCORE = ((999 - 1)
    * FUNCTION RANDOM) + 1.
```

**Business Justification:**
The credit agency programs (CRDTAGY1-5) are simulations for testing purposes. They:
- Generate random delays (0-3 seconds) to simulate real-world API latency
- Generate random scores (1-999) to simulate varying credit worthiness
- Allow testing of timeout handling without external dependencies
- Provide 1-in-4 chance of timeout (3-second delay with 3-second parent wait)

**Java Translation Requirements:**
```java
@Service
public class CreditAgencySimulator {
    
    private final Random random = new Random();
    
    public CreditAgencyResponse simulateCreditCheck(Customer customer) {
        // Random delay 0-3 seconds
        int delaySeconds = random.nextInt(4);
        
        try {
            Thread.sleep(delaySeconds * 1000L);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException("Credit check interrupted", e);
        }
        
        // Random score 1-999
        int score = random.nextInt(999) + 1;
        
        return new CreditAgencyResponse(score);
    }
}

// For testing with controllable behavior
@Service
@Profile("test")
public class MockCreditAgencyService {
    
    private Integer fixedScore;
    private Long fixedDelay;
    
    public void setFixedScore(Integer score) {
        this.fixedScore = score;
    }
    
    public void setFixedDelay(Long delayMs) {
        this.fixedDelay = delayMs;
    }
    
    public CreditAgencyResponse requestCreditScore(Customer customer) {
        if (fixedDelay != null) {
            try {
                Thread.sleep(fixedDelay);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
        
        int score = (fixedScore != null) ? fixedScore : 750;
        return new CreditAgencyResponse(score);
    }
}
```

**Test Scenarios:**
```java
@Test
void testCreditAgencySimulatorGeneratesValidScores() {
    Customer customer = createCustomer();
    
    for (int i = 0; i < 100; i++) {
        CreditAgencyResponse response = simulator.simulateCreditCheck(customer);
        assertTrue(response.getScore() >= 1);
        assertTrue(response.getScore() <= 999);
    }
}

@Test
void testCreditAgencySimulatorIntroducesDelay() {
    Customer customer = createCustomer();
    
    long start = System.currentTimeMillis();
    simulator.simulateCreditCheck(customer);
    long duration = System.currentTimeMillis() - start;
    
    // Should take between 0 and 3 seconds
    assertTrue(duration <= 3500); // Allow some overhead
}

@Test
void testMockCreditAgencyWithFixedBehavior() {
    mockAgency.setFixedScore(850);
    mockAgency.setFixedDelay(100L);
    
    Customer customer = createCustomer();
    
    long start = System.currentTimeMillis();
    CreditAgencyResponse response = mockAgency.requestCreditScore(customer);
    long duration = System.currentTimeMillis() - start;
    
    assertEquals(850, response.getScore());
    assertTrue(duration >= 100);
    assertTrue(duration < 200);
}
```

---

## 6. Error Handling Patterns

### 6.1 Fail Code Taxonomy

**COBOL Source Reference:**
Multiple files use consistent fail codes throughout the codebase.

**Fail Code Reference:**
- `'3'` - ENQ (resource lock) failure
- `'C'` - Credit check error
- `'O'` - Out of range (year/age validation failure)
- `'Y'` - Future date error (date of birth in future)
- `'Z'` - System error (CEEDAYS or other technical failure)

**Business Justification:**
Standardized fail codes allow:
- Consistent error handling across the application
- Clear error messaging to users
- Categorization of errors for monitoring and analytics
- Differentiation between user errors (O, Y) and system errors (Z, 3, C)

**Java Translation Requirements:**
```java
public enum FailCode {
    ENQ_FAILURE("3", "Resource lock failure", true),
    CREDIT_CHECK_ERROR("C", "Credit check failed", true),
    OUT_OF_RANGE("O", "Date out of valid range", false),
    FUTURE_DATE("Y", "Date cannot be in future", false),
    SYSTEM_ERROR("Z", "System processing error", true);
    
    private final String code;
    private final String message;
    private final boolean isSystemError;
    
    FailCode(String code, String message, boolean isSystemError) {
        this.code = code;
        this.message = message;
        this.isSystemError = isSystemError;
    }
    
    public String getCode() { return code; }
    public String getMessage() { return message; }
    public boolean isSystemError() { return isSystemError; }
}

@ResponseStatus(HttpStatus.BAD_REQUEST)
public class ValidationException extends RuntimeException {
    private final FailCode failCode;
    
    public ValidationException(String message, String failCodeStr) {
        super(message);
        this.failCode = FailCode.fromCode(failCodeStr);
    }
    
    public FailCode getFailCode() {
        return failCode;
    }
}

@RestControllerAdvice
public class GlobalExceptionHandler {
    
    @ExceptionHandler(ValidationException.class)
    public ResponseEntity<ErrorResponse> handleValidationException(
            ValidationException ex) {
        ErrorResponse response = ErrorResponse.builder()
            .success(false)
            .failCode(ex.getFailCode().getCode())
            .message(ex.getMessage())
            .timestamp(LocalDateTime.now())
            .build();
        
        return ResponseEntity
            .status(HttpStatus.BAD_REQUEST)
            .body(response);
    }
}
```

**Test Scenarios:**
```java
@Test
void testFailCodeCategorizationUserVsSystem() {
    assertTrue(FailCode.OUT_OF_RANGE.isUserError());
    assertTrue(FailCode.FUTURE_DATE.isUserError());
    assertTrue(FailCode.SYSTEM_ERROR.isSystemError());
    assertTrue(FailCode.ENQ_FAILURE.isSystemError());
    assertTrue(FailCode.CREDIT_CHECK_ERROR.isSystemError());
}

@Test
void testValidationExceptionWithFailCode() {
    ValidationException ex = new ValidationException(
        "Date is too old", "O");
    
    assertEquals(FailCode.OUT_OF_RANGE, ex.getFailCode());
    assertEquals("O", ex.getFailCode().getCode());
}

@Test
void testGlobalExceptionHandlerMapsFailCodes() throws Exception {
    when(customerService.createCustomer(any()))
        .thenThrow(new ValidationException("Future date", "Y"));
    
    mockMvc.perform(post("/api/customers")
        .contentType(MediaType.APPLICATION_JSON)
        .content(customerJson))
        .andExpect(status().isBadRequest())
        .andExpect(jsonPath("$.success").value(false))
        .andExpect(jsonPath("$.failCode").value("Y"));
}
```

### 6.2 SQLCODE Error Handling

**COBOL Source Reference:**
- Multiple files check SQLCODE after DB2 operations

```cobol
IF SQLCODE NOT = 0
   MOVE SQLCODE TO SQLCODE-DISPLAY
   * Handle error or abend
END-IF
```

**Business Justification:**
SQLCODE provides detailed information about database operation success or failure. Key SQLCODE values:
- 0 = Success
- +100 = No data found (not an error in many contexts)
- Negative values = Errors (deadlock, constraint violation, etc.)

**Java Translation Requirements:**
```java
@Repository
public class JdbcCustomerRepository {
    
    private final JdbcTemplate jdbcTemplate;
    
    public Customer save(Customer customer) {
        try {
            String sql = "INSERT INTO customer (...) VALUES (...)";
            
            int rowsAffected = jdbcTemplate.update(sql, 
                /* parameters */);
            
            if (rowsAffected == 0) {
                throw new DatabaseException(
                    "Failed to insert customer record");
            }
            
            return customer;
            
        } catch (DataIntegrityViolationException e) {
            // SQL constraint violation (e.g., duplicate key)
            throw new DatabaseException(
                "Customer already exists: " + 
                customer.getCustomerNumber(), e);
                
        } catch (DataAccessException e) {
            // Other database errors
            throw new DatabaseException(
                "Database error: " + e.getMessage(), e);
        }
    }
    
    public Optional<Customer> findBySortCodeAndCustomerNumber(
            String sortCode, String customerNumber) {
        try {
            String sql = "SELECT * FROM customer " +
                        "WHERE sort_code = ? AND customer_number = ?";
            
            Customer customer = jdbcTemplate.queryForObject(
                sql,
                new CustomerRowMapper(),
                sortCode, customerNumber);
            
            return Optional.of(customer);
            
        } catch (EmptyResultDataAccessException e) {
            // SQLCODE +100 equivalent
            return Optional.empty();
            
        } catch (DataAccessException e) {
            throw new DatabaseException(
                "Database error: " + e.getMessage(), e);
        }
    }
}
```

**Test Scenarios:**
```java
@Test
void testDuplicateKeyViolation() {
    Customer customer = createCustomer("123456", "0000000001");
    customerRepository.save(customer);
    
    DatabaseException ex = assertThrows(DatabaseException.class,
        () -> customerRepository.save(customer));
    
    assertTrue(ex.getMessage().contains("already exists"));
}

@Test
void testRecordNotFoundReturnsEmpty() {
    Optional<Customer> result = customerRepository
        .findBySortCodeAndCustomerNumber("999999", "9999999999");
    
    assertFalse(result.isPresent());
}

@Test
void testDatabaseConnectionFailure() {
    // Simulate connection failure
    dataSource.shutdown();
    
    assertThrows(DatabaseException.class,
        () -> customerRepository.findAll());
}
```

### 6.3 CICS RESP/RESP2 Error Handling

**COBOL Source Reference:**
- All CICS commands check RESP and RESP2

```cobol
EXEC CICS READ ...
   RESP(WS-CICS-RESP)
   RESP2(WS-CICS-RESP2)
END-EXEC.

IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
   * Handle specific error conditions
END-IF
```

**Business Justification:**
CICS RESP codes indicate the success or failure of CICS API calls (file I/O, locking, task management, etc.). RESP2 provides additional detail. Common patterns:
- NORMAL = Success
- NOTFND = Record not found
- DUPKEY = Duplicate key
- NOTFINISHED = Async operation incomplete

**Java Translation Requirements:**
```java
// CICS operations translate to various Java operations
// File operations → Database/File I/O
// ENQ/DEQ → Locking mechanisms
// LINK → Method calls
// Async operations → CompletableFuture/ExecutorService

public class CicsOperationException extends RuntimeException {
    private final String respCode;
    private final String resp2Code;
    
    public CicsOperationException(String message, 
                                  String respCode, 
                                  String resp2Code) {
        super(message);
        this.respCode = respCode;
        this.resp2Code = resp2Code;
    }
}

// Example: File operation
public Customer readCustomer(String sortCode, String customerNumber) {
    try {
        return customerRepository
            .findBySortCodeAndCustomerNumber(sortCode, customerNumber)
            .orElseThrow(() -> new CicsOperationException(
                "Customer not found",
                "NOTFND",
                ""));
    } catch (DataAccessException e) {
        throw new CicsOperationException(
            "File operation failed",
            "ERROR",
            e.getMessage());
    }
}
```

**Test Scenarios:**
```java
@Test
void testRecordNotFoundThrowsNotfndException() {
    CicsOperationException ex = assertThrows(
        CicsOperationException.class,
        () -> service.readCustomer("999999", "9999999999"));
    
    assertEquals("NOTFND", ex.getRespCode());
}

@Test
void testDuplicateKeyThrowsDupkeyException() {
    Customer customer = createCustomer("123456", "0000000001");
    service.createCustomer(customer);
    
    CicsOperationException ex = assertThrows(
        CicsOperationException.class,
        () -> service.createCustomer(customer));
    
    assertEquals("DUPKEY", ex.getRespCode());
}
```

### 6.4 Centralized Abend Processing (ABNDPROC)

**COBOL Source Reference:**
- File: `ABNDPROC.cbl`
- Lines: 1-177

```cobol
EXEC CICS WRITE
   FILE('ABNDFILE')
   FROM(WS-ABND-AREA)
   RIDFLD(ABND-VSAM-KEY)
   RESP(WS-CICS-RESP)
   RESP2(WS-CICS-RESP2)
END-EXEC.
```

**Business Justification:**
ABNDPROC provides centralized error logging for application abends (abnormal terminations). This allows:
- Single location for error investigation
- Consistent error record format
- Historical error tracking
- System health monitoring
- Root cause analysis

The abend record includes: task number, transaction ID, program name, date/time, RESP codes, SQLCODE, and freeform error description.

**Java Translation Requirements:**
```java
@Entity
@Table(name = "application_error")
public class ApplicationError {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(name = "task_number")
    private String taskNumber;
    
    @Column(name = "applid")
    private String applId;
    
    @Column(name = "transaction_id")
    private String transactionId;
    
    @Column(name = "error_date")
    private LocalDate errorDate;
    
    @Column(name = "error_time")
    private LocalTime errorTime;
    
    @Column(name = "error_code")
    private String errorCode;
    
    @Column(name = "program_name")
    private String programName;
    
    @Column(name = "resp_code")
    private Integer respCode;
    
    @Column(name = "resp2_code")
    private Integer resp2Code;
    
    @Column(name = "sql_code")
    private Integer sqlCode;
    
    @Column(name = "error_description", length = 600)
    private String errorDescription;
}

@Service
public class ErrorLoggingService {
    
    private final ApplicationErrorRepository errorRepository;
    
    public void logApplicationError(Exception e, String context) {
        ApplicationError error = new ApplicationError();
        error.setTaskNumber(generateTaskNumber());
        error.setApplId(applicationId);
        error.setTransactionId(getTransactionId());
        error.setErrorDate(LocalDate.now());
        error.setErrorTime(LocalTime.now());
        error.setErrorCode(extractErrorCode(e));
        error.setProgram Name(extractProgramName());
        error.setErrorDescription(buildErrorDescription(e, context));
        
        // Extract RESP, RESP2, SQLCODE if available
        if (e instanceof CicsOperationException) {
            CicsOperationException cics = (CicsOperationException) e;
            error.setRespCode(parseRespCode(cics.getRespCode()));
            error.setResp2Code(parseRespCode(cics.getResp2Code()));
        }
        
        if (e instanceof DatabaseException) {
            DatabaseException db = (DatabaseException) e;
            error.setSqlCode(db.getSqlCode());
        }
        
        errorRepository.save(error);
        
        logger.error("Application error logged: {}", error, e);
    }
}

@RestControllerAdvice
public class GlobalExceptionHandler {
    
    private final ErrorLoggingService errorLoggingService;
    
    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> handleGenericException(
            Exception ex, WebRequest request) {
        
        // Log to centralized error table
        errorLoggingService.logApplicationError(ex, request.getDescription(false));
        
        ErrorResponse response = ErrorResponse.builder()
            .success(false)
            .message("An unexpected error occurred")
            .timestamp(LocalDateTime.now())
            .build();
        
        return ResponseEntity
            .status(HttpStatus.INTERNAL_SERVER_ERROR)
            .body(response);
    }
}
```

**Test Scenarios:**
```java
@Test
void testErrorLoggingCreatesRecord() {
    Exception ex = new RuntimeException("Test error");
    
    errorLoggingService.logApplicationError(ex, "Customer creation");
    
    List<ApplicationError> errors = errorRepository.findAll();
    assertEquals(1, errors.size());
    
    ApplicationError error = errors.get(0);
    assertNotNull(error.getTaskNumber());
    assertNotNull(error.getErrorDate());
    assertTrue(error.getErrorDescription().contains("Test error"));
}

@Test
void testCicsExceptionIncludesRespCodes() {
    CicsOperationException ex = new CicsOperationException(
        "File operation failed", "ERROR", "SYSIDERR");
    
    errorLoggingService.logApplicationError(ex, "File read");
    
    ApplicationError error = errorRepository.findAll().get(0);
    assertNotNull(error.getRespCode());
    assertNotNull(error.getResp2Code());
}

@Test
void testDatabaseExceptionIncludesSqlCode() {
    DatabaseException ex = new DatabaseException("Constraint violation", -803);
    
    errorLoggingService.logApplicationError(ex, "Customer insert");
    
    ApplicationError error = errorRepository.findAll().get(0);
    assertEquals(-803, error.getSqlCode().intValue());
}
```

---

## Summary

This document provides comprehensive documentation of business rules extracted from 29 COBOL programs in the CICS Banking Sample Application. Each rule has been documented with:

✅ **COBOL Source References** - Specific files and line numbers  
✅ **Business Justifications** - Why the rule exists and its purpose  
✅ **Java Translation Requirements** - How to implement in Spring Boot  
✅ **Test Scenarios** - Comprehensive test cases for validation

### Key Business Rules Summary:

1. **Date Validation**: CEEDAYS constraints (year ≥ 1601, age ≤ 150), future date rejection, fail codes O/Y/Z
2. **Composite Keys**: Sort code + customer/account number patterns for multi-branch support
3. **Named Counter**: ENQ/DEQ locking for thread-safe ID generation with rollback on failure
4. **PROCTRAN Logging**: 18 transaction types, audit trail for financial operations (excluding non-financial updates)
5. **Credit Scoring**: Async API calls, 3-second timeout, score aggregation, random review dates
6. **Error Handling**: Standardized fail codes (3/C/O/Y/Z), SQLCODE/RESP checking, centralized abend logging

### Migration Priorities:

**High Priority** (Core functionality):
- Date validation rules (critical for data integrity)
- Composite key patterns (database design foundation)
- Named Counter logic (prevents duplicate IDs)
- PROCTRAN logging (regulatory requirement)

**Medium Priority** (Important features):
- Credit scoring integration (business requirement)
- Error handling patterns (system reliability)

**Low Priority** (Nice to have):
- Credit agency simulation (testing infrastructure)

### Next Steps:

1. Implement validation framework with fail code support
2. Set up composite key entities and repositories
3. Implement Named Counter service with locking
4. Create transaction logging service
5. Integrate credit scoring API (or simulator)
6. Establish centralized error logging

---

**Document Version:** 1.0  
**Last Updated:** 2024-10-26  
**Total Programs Analyzed:** 29  
**Total Business Rules Documented:** 30+
