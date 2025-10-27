# Business Processes Requirements

**Document Version:** 1.0  
**Last Updated:** 2025-10-27  
**Target System:** CBSA Java Migration (Spring Boot)  
**Legacy System:** CICS Banking Sample Application (COBOL)

## Executive Summary

This document captures the business processes implemented in the CICS Banking Sample Application (CBSA) legacy system, tracing each process back to its originating COBOL programs and BMS screen definitions. These requirements form the foundation for the Java-based modernization effort.

## Table of Contents

1. [Customer Management Processes](#customer-management-processes)
2. [Account Management Processes](#account-management-processes)
3. [Transaction Processing](#transaction-processing)
4. [System Utilities](#system-utilities)
5. [Acceptance Criteria](#acceptance-criteria)
6. [Modernization Considerations](#modernization-considerations)

---

## 1. Customer Management Processes

### 1.1 Customer Creation (CRECUST)

**Legacy Program:** `CRECUST.cbl` (1440 lines)  
**BMS Screen:** `BNK1CCS` (Customer Services Screen)  
**Complexity:** High

#### Business Process Flow

1. **Input Collection**
   - User enters customer name (60 characters)
   - User enters customer address (160 characters) 
   - User enters date of birth (YYYYMMDD format)
   - System retrieves bank sort code via GETSCODE

2. **Customer Number Generation**
   - System enqueues Named Counter for customer numbers (`HBNKCUST`)
   - Increments counter atomically using CICS Named Counter API
   - Assigns unique 10-digit customer number
   - Composite key: `sortcode (6 digits) + customer_number (10 digits)`

3. **Credit Score Assessment**
   - System invokes async credit checks to 5 credit agencies (CRDTAGY1-5)
   - Waits 3 seconds for responses
   - Aggregates and averages returned scores
   - If no responses received, sets credit score to 0 and marks review date as current date
   - Credit score range: 0-999

4. **Data Persistence**
   - Writes customer record to VSAM CUSTOMER file with eyecatcher 'CUST'
   - Records transaction in PROCTRAN (Db2) with type 'OCC' (branch create customer) or 'ICC' (web create customer)
   - Transaction includes: sortcode, customer number, name excerpt, DOB

5. **Resource Cleanup**
   - Dequeues Named Counter on success
   - On failure: decrements counter and dequeues to restore state

#### Date Validation Rules

**Legacy COBOL:** Lines implementing DATE-OF-BIRTH-CHECK
- Minimum year: 1601 (COBOL CEEDAYS limitation)
- Maximum age: 150 years from current date
- Reject future dates
- Fail code 'O' for year/age violations
- Fail code 'Y' for future dates

#### Acceptance Criteria

- AC1.1: Customer numbers must be unique and sequential per sortcode
- AC1.2: Credit score aggregation must average all responses within 3 second timeout
- AC1.3: PROCTRAN audit record must be created for every successful customer creation
- AC1.4: Date validation must reject DOB before 1601, age > 150 years, or future dates
- AC1.5: Transaction must be atomic - rollback on any failure during persistence
- AC1.6: Named Counter must be decremented and dequeued on rollback

**Traced to:** CRECUST.cbl, CUSTOMER.cpy, PROCTRAN.cpy, NEWCUSNO.cpy

---

### 1.2 Customer Inquiry (INQCUST)

**Legacy Program:** `INQCUST.cbl` (712 lines)  
**BMS Screen:** `BNK1CCS` (Customer Services Screen)  
**Complexity:** Medium

#### Business Process Flow

1. **Input Validation**
   - Accepts sortcode (6 digits) and customer number (10 digits) as composite key
   - Validates format and range

2. **Data Retrieval**
   - Queries VSAM CUSTOMER file using composite key
   - Returns complete customer record if found
   - Returns low-values record if not found
   - No PROCTRAN logging (read-only operation)

3. **Response Formatting**
   - Customer name (60 characters)
   - Customer address (160 characters)
   - Date of birth (YYYYMMDD)
   - Credit score (0-999)
   - Credit score review date

#### Acceptance Criteria

- AC1.7: Must support composite key lookup (sortcode + customer_number)
- AC1.8: Must return low-values record (not error) when customer not found
- AC1.9: Response time must be < 100ms for 95th percentile queries
- AC1.10: No audit logging required for read operations

**Traced to:** INQCUST.cbl, CUSTOMER.cpy

---

### 1.3 Customer Update (UPDCUST)

**Legacy Program:** `UPDCUST.cbl` (365 lines)  
**BMS Screen:** `BNK1CCS` (Customer Services Screen)  
**Complexity:** Low

#### Business Process Flow

1. **Input Validation**
   - Accepts composite key (sortcode + customer_number)
   - Accepts updated fields (name, address only - DOB immutable)

2. **Update Processing**
   - Reads existing customer record from VSAM
   - Updates only modifiable fields: name, address
   - Credit score and DOB are immutable (business rule)
   - Writes updated record back to VSAM

3. **Audit Trail**
   - **No PROCTRAN logging** for customer updates (business rule exception)
   - This is intentional - only creates and deletes are audited

#### Field Update Rules

- **Modifiable:** name (60 chars), address (160 chars)
- **Immutable:** customer_number, sortcode, date_of_birth, credit_score, cs_review_date

#### Acceptance Criteria

- AC1.11: Only name and address fields may be updated
- AC1.12: Attempts to modify DOB or credit score must be rejected
- AC1.13: No PROCTRAN audit record is created (intentional business rule)
- AC1.14: Update operation must use optimistic locking via VSAM version check

**Traced to:** UPDCUST.cbl, CUSTOMER.cpy

---

### 1.4 Customer Deletion (DELCUS)

**Legacy Program:** `DELCUS.cbl` (762 lines)  
**BMS Screen:** `BNK1DCS` (Delete Customer Screen)  
**Complexity:** Medium

#### Business Process Flow

1. **Pre-Delete Validation**
   - Accepts composite key (sortcode + customer_number)
   - Retrieves customer record to confirm existence

2. **Cascade Delete Logic**
   - Queries Db2 ACCOUNT table for all customer accounts
   - Deletes each associated account record
   - Deletes customer record from VSAM

3. **Audit Trail**
   - Creates PROCTRAN record with type 'ODC' (branch delete customer) or 'IDC' (web delete customer)
   - Records: sortcode, customer_number, name excerpt (14 chars), DOB

4. **Transaction Integrity**
   - Must be atomic - if account deletion fails, rollback customer deletion
   - Verify no orphaned accounts remain after customer deletion

#### Acceptance Criteria

- AC1.15: Must delete all associated accounts before deleting customer
- AC1.16: Must be atomic transaction - rollback all deletes on any failure
- AC1.17: PROCTRAN audit must record customer details before deletion
- AC1.18: Database query after deletion must confirm zero orphaned accounts
- AC1.19: Cascade delete must handle customers with 0 to N accounts

**Traced to:** DELCUS.cbl, CUSTOMER.cpy, ACCOUNT.cpy, PROCTRAN.cpy

---

## 2. Account Management Processes

### 2.1 Account Creation (CREACC)

**Legacy Program:** `CREACC.cbl` (1248 lines)  
**BMS Screen:** `BNK1CAC` (Create Account Screen)  
**Complexity:** High

#### Business Process Flow

1. **Input Validation**
   - Customer number (must exist in CUSTOMER file)
   - Account type (8 characters: ISA, MORTGAGE, SAVING, LOAN, CURRENT)
   - Interest rate (9(4)V99 format, e.g., 0450 = 4.50%)
   - Overdraft limit (up to 99,999,999)

2. **Account Number Generation**
   - Enqueues Named Counter for account numbers (`HBNKACC`)
   - Increments counter atomically
   - Assigns unique 8-digit account number
   - Account key: `sortcode (6 digits) + account_number (8 digits)`

3. **Initial Account Setup**
   - Sets opening date to current date (YYYYMMDD)
   - Initializes last statement date and next statement date
   - Sets both available and actual balance to 0.00
   - Assigns eyecatcher 'ACCT'

4. **Data Persistence**
   - Writes account record to Db2 ACCOUNT table
   - Records transaction in PROCTRAN with type 'OCA' (branch create account) or 'ICA' (web create account)
   - PROCTRAN details: customer_number, account_type, last_stmt_date, next_stmt_date

5. **Resource Cleanup**
   - Dequeues Named Counter on success
   - On failure: decrements counter and dequeues

#### Acceptance Criteria

- AC2.1: Account numbers must be unique and sequential per sortcode
- AC2.2: Customer must exist before account creation (foreign key integrity)
- AC2.3: Opening date must be set to current system date
- AC2.4: Initial balances (available and actual) must be 0.00
- AC2.5: PROCTRAN audit record required with 'CREATE' footer
- AC2.6: Transaction must be atomic with Named Counter rollback on failure

**Traced to:** CREACC.cbl, ACCOUNT.cpy, PROCTRAN.cpy, NEWACCNO.cpy

---

### 2.2 Account Inquiry - Single (INQACC)

**Legacy Program:** `INQACC.cbl` (1003 lines)  
**BMS Screen:** `BNK1CAC` (Account Screen)  
**Complexity:** Medium

#### Business Process Flow

1. **Input Validation**
   - Accepts sortcode (6 digits) and account number (8 digits)
   - Validates format

2. **Data Retrieval**
   - Queries Db2 ACCOUNT table using composite key
   - Returns complete account record if found
   - Returns low-values if not found

3. **Response Data**
   - Account details: type, customer number, balances, dates
   - Interest rate, overdraft limit
   - Statement dates (last and next)

#### Acceptance Criteria

- AC2.7: Must support composite key lookup (sortcode + account_number)
- AC2.8: Must return account with both available and actual balances
- AC2.9: Response time < 100ms for 95th percentile queries
- AC2.10: No audit logging for read operations

**Traced to:** INQACC.cbl, ACCOUNT.cpy

---

### 2.3 Account Inquiry - Customer List (INQACCCU)

**Legacy Program:** `INQACCCU.cbl` (883 lines)  
**BMS Screen:** `BNK1CCA` (List Customer Accounts)  
**Complexity:** Medium

#### Business Process Flow

1. **Input Validation**
   - Accepts customer number (10 digits)
   - Optional: pagination parameters

2. **Cursor-Based Query**
   - Opens Db2 cursor for customer's accounts
   - Uses cursor to handle pagination
   - Retrieves accounts in batches

3. **Response Handling**
   - Returns list of all accounts for customer
   - Supports pagination for customers with many accounts
   - Closes cursor after retrieval

#### Acceptance Criteria

- AC2.11: Must retrieve all accounts for a given customer number
- AC2.12: Must support cursor-based pagination for large result sets
- AC2.13: Must handle customers with 0 to N accounts gracefully
- AC2.14: Cursor must be properly closed after query completion

**Traced to:** INQACCCU.cbl, ACCOUNT.cpy

---

### 2.4 Account Update (UPDACC)

**Legacy Program:** `UPDACC.cbl` (407 lines)  
**BMS Screen:** `BNK1UAC` (Update Account Screen)  
**Complexity:** Low

#### Business Process Flow

1. **Input Validation**
   - Accepts composite key (sortcode + account_number)
   - Validates updated field values

2. **Update Processing**
   - Reads existing account from Db2
   - Updates modifiable fields only
   - Validates business rules
   - Writes updated record

3. **Field Update Rules**
   - **Modifiable:** account_type, interest_rate, overdraft_limit, last_stmt_date, next_stmt_date
   - **Immutable:** account_number, sortcode, customer_number, opened_date, balances

4. **Audit Trail**
   - Creates PROCTRAN record for account update
   - Records updated field values

#### Acceptance Criteria

- AC2.15: Only designated fields may be updated (type, rate, overdraft, dates)
- AC2.16: Balances and opening date must remain immutable
- AC2.17: Interest rate must be validated: 0.00% to 99.99%
- AC2.18: Overdraft limit must be non-negative
- AC2.19: PROCTRAN audit record required

**Traced to:** UPDACC.cbl, ACCOUNT.cpy, PROCTRAN.cpy

---

### 2.5 Account Deletion (DELACC)

**Legacy Program:** `DELACC.cbl` (650 lines)  
**BMS Screen:** `BNK1DAC` (Delete Account Screen)  
**Complexity:** Medium

#### Business Process Flow

1. **Pre-Delete Validation**
   - Accepts composite key (sortcode + account_number)
   - Retrieves account to confirm existence
   - Optional: Check for zero balance before deletion

2. **Deletion Processing**
   - Deletes account record from Db2 ACCOUNT table
   - Maintains referential integrity with customer

3. **Audit Trail**
   - Creates PROCTRAN record with type 'ODA' (branch delete account) or 'IDA' (web delete account)
   - Records: customer_number, account_type, last_stmt_date, next_stmt_date, 'DELETE' footer

#### Acceptance Criteria

- AC2.20: Account must exist before deletion
- AC2.21: PROCTRAN audit must record account details before deletion
- AC2.22: Deletion must not orphan the parent customer record
- AC2.23: Business rule validation for balance check (if required)

**Traced to:** DELACC.cbl, ACCOUNT.cpy, PROCTRAN.cpy

---

## 3. Transaction Processing

### 3.1 Transfer Funds (XFRFUN)

**Legacy Program:** `XFRFUN.cbl` (1925 lines)  
**BMS Screen:** `BNK1TFN` (Transfer Funds Screen)  
**Complexity:** High

#### Business Process Flow

1. **Input Validation**
   - Source account (sortcode + account_number)
   - Target account (sortcode + account_number)
   - Transfer amount (up to 9,999,999,999.99)
   - Optional: transfer description

2. **Pre-Transfer Validation**
   - Verify source account exists and is active
   - Verify target account exists and is active
   - Check source account has sufficient available balance
   - Validate overdraft limit not exceeded

3. **Atomic Dual-Account Update**
   - **Phase 1:** Debit source account
     - Decrease available balance by transfer amount
     - Decrease actual balance by transfer amount
   - **Phase 2:** Credit target account
     - Increase available balance by transfer amount
     - Increase actual balance by transfer amount
   - **Both updates must succeed or both rollback**

4. **Audit Trail**
   - Create PROCTRAN record with type 'TFR' (transfer)
   - Description includes: "TRANSFER" + target_sortcode + target_account_number
   - Amount recorded as positive value
   - Separate PROCTRAN records may be created for source and target accounts

5. **Rollback Handling**
   - On any failure: rollback both account updates
   - Restore original balances
   - Log failure details

#### Balance Update Rules

- **Available Balance:** Immediately reflects transfer (real-time)
- **Actual Balance:** Also updated immediately (posted transaction)
- **Overdraft Logic:** Available balance may go negative up to overdraft_limit

#### Acceptance Criteria

- AC3.1: Must be atomic - both accounts updated or both rollback
- AC3.2: Source account available balance must be >= transfer amount (considering overdraft)
- AC3.3: Both available and actual balances must be updated on both accounts
- AC3.4: PROCTRAN audit record must reference both source and target accounts
- AC3.5: Transfer to same account (source = target) must be rejected
- AC3.6: Rollback must restore exact original balances on failure
- AC3.7: Response time < 500ms for 95th percentile transfers

**Traced to:** XFRFUN.cbl, ACCOUNT.cpy, PROCTRAN.cpy

---

### 3.2 Debit/Credit Operations (DBCRFUN)

**Legacy Program:** `DBCRFUN.cbl` (862 lines)  
**BMS Screen:** Multiple (Account screens)  
**Complexity:** Medium

#### Business Process Flow

1. **Operation Type Determination**
   - **Debit (Withdrawal):** Transaction type 'DEB'
   - **Credit (Deposit):** Transaction type 'CRE'

2. **Input Validation**
   - Account composite key (sortcode + account_number)
   - Transaction amount (up to 9,999,999,999.99)
   - Transaction type code

3. **Balance Updates**
   - **Credit Operation:**
     - Increase available balance by amount
     - Increase actual balance by amount
   - **Debit Operation:**
     - Decrease available balance by amount
     - Decrease actual balance by amount
     - Validate sufficient funds (including overdraft)

4. **Audit Trail**
   - Create PROCTRAN record with type 'CRE' or 'DEB'
   - Record transaction amount
   - Capture transaction timestamp

#### Transaction Type Codes

- **CRE:** Credit/Deposit
- **DEB:** Debit/Withdrawal
- **CHI:** Cheque Paid In
- **CHO:** Cheque Paid Out
- **CHA:** Cheque Acknowledged
- **CHF:** Cheque Failure

#### Acceptance Criteria

- AC3.8: Must update both available and actual balances for both operations
- AC3.9: Debit must validate sufficient funds (actual_balance + overdraft_limit)
- AC3.10: PROCTRAN record must have correct transaction type code
- AC3.11: Timestamp must be captured at transaction processing time
- AC3.12: Credit operations must always succeed (no balance validation)
- AC3.13: Response time < 200ms for 95th percentile operations

**Traced to:** DBCRFUN.cbl, ACCOUNT.cpy, PROCTRAN.cpy

---

## 4. System Utilities

### 4.1 Get Company Information (GETCOMPY)

**Legacy Program:** `GETCOMPY.cbl`  
**Complexity:** Low

#### Business Process

- Returns company name: "CICS Bank Sample Application"
- Read-only utility function
- No database access
- No audit logging

#### Acceptance Criteria

- AC4.1: Must return consistent company name across all invocations
- AC4.2: Response time < 10ms

**Traced to:** GETCOMPY.cbl

---

### 4.2 Get Sort Code (GETSCODE)

**Legacy Program:** `GETSCODE.cbl`  
**Complexity:** Low

#### Business Process

- Returns bank sort code: "987654"
- Read-only utility function
- May read from system configuration
- No audit logging

#### Acceptance Criteria

- AC4.3: Must return consistent sort code across all invocations
- AC4.4: Sort code must be valid 6-digit format
- AC4.5: Response time < 10ms

**Traced to:** GETSCODE.cbl, SORTCODE.cpy

---

### 4.3 Error Processing (ABNDPROC)

**Legacy Program:** `ABNDPROC.cbl`  
**Complexity:** Low

#### Business Process

- Centralized error handling and logging
- Writes error details to ABNDFILE (VSAM)
- Provides error context for debugging
- Abends transaction with appropriate code

#### Acceptance Criteria

- AC4.6: Must log all program abends with context
- AC4.7: Must capture: program name, timestamp, error code, error message
- AC4.8: ABNDFILE must be accessible for error analysis

**Traced to:** ABNDPROC.cbl, ABNDINFO.cpy

---

## 5. Acceptance Criteria Summary

### 5.1 Functional Acceptance

All functional acceptance criteria (AC1.1 through AC4.8) listed above must be met for each business process.

### 5.2 Cross-Functional Acceptance

- **CFA1:** All PROCTRAN audit records must be queryable by date range
- **CFA2:** All operations must support both web-initiated and branch-initiated transactions
- **CFA3:** Named Counter operations must be thread-safe and prevent duplicate numbers
- **CFA4:** All date fields must use ISO 8601 format in Java implementation (YYYY-MM-DD)
- **CFA5:** All monetary amounts must use decimal precision without rounding errors

### 5.3 Migration Acceptance

- **MA1:** All business logic must produce identical results to COBOL programs
- **MA2:** All data validations from COBOL must be preserved in Java
- **MA3:** All audit trail patterns must be maintained in Java implementation
- **MA4:** Performance must be within 20% of COBOL baseline (see performance requirements)

---

## 6. Modernization Considerations

### 6.1 Transaction Management

**Legacy:** CICS manages transactions automatically with SYNCPOINT  
**Modern:** Spring @Transactional annotations for declarative transaction management
- Use propagation level REQUIRED for standard operations
- Use REQUIRES_NEW for Named Counter operations to prevent deadlocks
- Implement compensation logic for multi-phase operations (e.g., XFRFUN)

### 6.2 Concurrency Control

**Legacy:** CICS Named Counter API with ENQUEUE/DEQUEUE  
**Modern:** Database-level optimistic locking with version columns or pessimistic locking with SELECT FOR UPDATE
- Implement distributed counter service for customer/account number generation
- Use @Version annotation for optimistic locking on entity updates
- Consider Redis for high-performance distributed counters

### 6.3 Async Processing

**Legacy:** CICS Async API for credit agency calls  
**Modern:** Spring @Async with CompletableFuture
- Use thread pool executor for credit agency calls
- Implement timeout handling (3 seconds)
- Aggregate results using CompletableFuture.allOf()

### 6.4 Data Access Patterns

**Legacy:** VSAM sequential/random access, Db2 SQL with cursors  
**Modern:** Spring Data JDBC with repository pattern
- Use composite key classes for Customer and Account entities
- Implement pagination with Spring Pageable for account lists
- Use JDBC Template for cursor-based queries if needed

### 6.5 Audit Trail

**Legacy:** PROCTRAN records in Db2 with specific transaction type codes  
**Modern:** Maintain PROCTRAN table structure but use Spring AOP for audit interceptors
- Create @Audited annotation for automatic audit logging
- Aspect captures transaction details before and after method execution
- Preserve all legacy transaction type codes

### 6.6 Error Handling

**Legacy:** ABNDPROC with ABNDFILE logging and CICS ABEND  
**Modern:** Spring exception handling with @ControllerAdvice
- Custom exceptions map to HTTP status codes
- Centralized error logging to database and/or log aggregation service
- Preserve error context for debugging

### 6.7 Date Validation

**Legacy:** COBOL CEEDAYS with year 1601 limitation  
**Modern:** Java LocalDate with equivalent validation
- Minimum date: 1601-01-01 (preserve COBOL limitation)
- Maximum age: 150 years
- Reject future dates
- Use same fail codes: 'O' for year/age, 'Y' for future

### 6.8 BMS Screen Migration

**Legacy:** BMS 3270 screens (BNK1* programs)  
**Modern:** RESTful APIs with JSON request/response
- Map each BMS screen to REST endpoint
- Maintain same validation rules in API layer
- Provide OpenAPI/Swagger documentation for all endpoints

### 6.9 Testing Strategy

- Unit tests for service layer business logic (80% coverage target)
- Integration tests with real database (70% coverage target)
- End-to-end tests for complete workflows
- Performance tests against COBOL baseline
- Legacy comparison tests to validate identical behavior

---

## Appendix A: COBOL Program Inventory

### Business Logic Programs (11 programs to migrate)

| Program | Lines | Complexity | Purpose |
|---------|-------|------------|---------|
| CRECUST | 1440 | High | Create customer with credit checks |
| INQCUST | 712 | Medium | Inquire customer details |
| UPDCUST | 365 | Low | Update customer information |
| DELCUS | 762 | Medium | Delete customer with cascade |
| CREACC | 1248 | High | Create account with counter |
| INQACC | 1003 | Medium | Inquire single account |
| INQACCCU | 883 | Medium | Inquire customer accounts |
| UPDACC | 407 | Low | Update account details |
| DELACC | 650 | Medium | Delete account |
| XFRFUN | 1925 | High | Transfer funds atomically |
| DBCRFUN | 862 | Medium | Debit/credit operations |

### UI Layer Programs (9 programs - replaced by REST APIs)

| Program | Purpose |
|---------|---------|
| BNKMENU | Main menu screen |
| BNK1TFN | Transfer funds screen |
| BNK1CCS | Customer screen |
| BNK1CCA | List customer accounts screen |
| BNK1CAC | Create account screen |
| BNK1UAC | Update account screen |
| BNK1CRA | Create account alternate screen |
| BNK1DCS | Delete customer screen |
| BNK1DAC | Delete account screen |

### Utility Programs (5 programs - already migrated)

| Program | Purpose | Java Service |
|---------|---------|--------------|
| GETCOMPY | Get company info | CompanyInfoService |
| GETSCODE | Get sort code | SortCodeService |
| ABNDPROC | Error processing | ErrorLoggingService |
| CRDTAGY1-5 | Credit agencies (5 identical) | CreditAgencyService |
| BANKDATA | Data generator | BankDataGenerator |

---

## Document Control

**Approvals Required:**
- Business Analyst: Review business process accuracy
- Technical Architect: Review modernization approach
- QA Lead: Review acceptance criteria completeness

**Related Documents:**
- 02_Data_Structures.md
- 03_Functional_Requirements.md
- 04_Integration_Points.md
- 05_Non_Functional_Requirements.md
