# Functional Requirements

**Document Version:** 1.0  
**Last Updated:** 2025-10-27  
**Target System:** CBSA Java Migration (Spring Boot)  
**Legacy System:** CICS Banking Sample Application (COBOL)

## Executive Summary

This document defines all functional requirements for the CBSA modernization effort, tracing each requirement back to specific COBOL programs, line numbers, and business logic. These requirements form the contract for ensuring the Java implementation maintains functional equivalence with the legacy system.

## Table of Contents

1. [Customer Management Requirements](#customer-management-requirements)
2. [Account Management Requirements](#account-management-requirements)
3. [Transaction Processing Requirements](#transaction-processing-requirements)
4. [System Utility Requirements](#system-utility-requirements)
5. [Cross-Cutting Requirements](#cross-cutting-requirements)
6. [Acceptance Criteria](#acceptance-criteria)

---

## 1. Customer Management Requirements

### FR-CUST-001: Create Customer with Credit Check

**Priority:** High  
**Source Program:** CRECUST.cbl (lines 1-1440)  
**Complexity:** High

#### Functional Description

The system shall create a new customer record with the following process:

1. Generate unique customer number using Named Counter
2. Perform async credit checks with 5 agencies
3. Store customer details in CUSTOMER file
4. Record audit trail in PROCTRAN
5. Handle rollback on any failure

#### Detailed Requirements

**FR-CUST-001.1: Customer Number Generation**
- SHALL enqueue Named Counter resource 'HBNKCUST' before incrementing
- SHALL increment counter atomically
- SHALL generate 10-digit customer number (zero-padded)
- SHALL combine with 6-digit sortcode for composite key
- SHALL dequeue counter after successful creation or on rollback

**Acceptance Test:** Create 100 customers concurrently; verify all customer numbers are unique and sequential

**FR-CUST-001.2: Credit Score Aggregation**
- SHALL invoke 5 credit agency services asynchronously (CRDTAGY1-5)
- SHALL wait maximum 3 seconds for all responses
- SHALL average all received credit scores
- SHALL set credit score to 0 if no responses received within timeout
- SHALL set cs_review_date to current date if score is 0

**Acceptance Test:** Mock credit agencies with varying response times; verify timeout handling and score averaging

**FR-CUST-001.3: Date of Birth Validation**
- SHALL reject DOB with year before 1601 (fail code 'O')
- SHALL reject DOB with age greater than 150 years (fail code 'O')
- SHALL reject DOB in the future (fail code 'Y')
- SHALL accept DOB between 1601 and (current_year - 150)

**Acceptance Test:** Submit customers with DOB in 1600, 1601, current date + 1 day, 150 years ago, 151 years ago; verify correct acceptance/rejection

**FR-CUST-001.4: Customer Data Persistence**
- SHALL write customer record to VSAM CUSTOMER file with eyecatcher 'CUST'
- SHALL store name (60 characters, left-justified, space-padded)
- SHALL store address (160 characters, left-justified, space-padded)
- SHALL store DOB in YYYYMMDD format (8 digits)
- SHALL store credit score (0-999, 3 digits)
- SHALL store cs_review_date in YYYYMMDD format

**Acceptance Test:** Create customer and verify all fields stored correctly with proper padding and formats

**FR-CUST-001.5: Audit Trail**
- SHALL create PROCTRAN record with type 'OCC' (branch) or 'ICC' (web)
- SHALL record sortcode (6 digits)
- SHALL record customer_number (10 digits)
- SHALL record name excerpt (first 14 characters)
- SHALL record DOB in format YYYY-MM-DD within description
- SHALL record current date and time

**Acceptance Test:** Create customer via branch and web interfaces; verify PROCTRAN records with correct type codes

**FR-CUST-001.6: Transaction Rollback**
- SHALL decrement Named Counter if customer write fails
- SHALL dequeue Named Counter on any failure
- SHALL NOT create PROCTRAN record if customer write fails
- SHALL rollback all changes atomically

**Acceptance Test:** Simulate database failure during customer write; verify counter is decremented and dequeued

**Traced to:** CRECUST.cbl, CUSTOMER.cpy, PROCTRAN.cpy, NEWCUSNO.cpy

---

### FR-CUST-002: Inquire Customer

**Priority:** High  
**Source Program:** INQCUST.cbl (lines 1-712)  
**Complexity:** Medium

#### Functional Description

The system shall retrieve customer information by composite key (sortcode + customer_number).

#### Detailed Requirements

**FR-CUST-002.1: Customer Lookup**
- SHALL accept sortcode (6 digits) and customer_number (10 digits)
- SHALL query VSAM CUSTOMER file using composite key
- SHALL return complete customer record if found
- SHALL return record with all fields set to low-values if not found (not an error)
- SHALL NOT create PROCTRAN record (read-only operation)

**Acceptance Test:** Query existing customer by valid key; verify all fields returned. Query non-existent customer; verify low-values record returned

**FR-CUST-002.2: Response Data**
- SHALL return: sortcode, customer_number, name, address, date_of_birth, credit_score, cs_review_date
- SHALL preserve field lengths and padding from VSAM record
- SHALL NOT modify or trim trailing spaces

**Acceptance Test:** Verify response contains all fields with correct types and lengths

**FR-CUST-002.3: Performance**
- SHALL complete read operation in < 100ms (95th percentile)
- SHALL support concurrent read operations without blocking

**Acceptance Test:** Execute 1000 concurrent customer queries; verify 95th percentile < 100ms

**Traced to:** INQCUST.cbl, CUSTOMER.cpy

---

### FR-CUST-003: Update Customer

**Priority:** Medium  
**Source Program:** UPDCUST.cbl (lines 1-365)  
**Complexity:** Low

#### Functional Description

The system shall update limited customer fields while preventing modification of immutable data.

#### Detailed Requirements

**FR-CUST-003.1: Field Update Rules**
- SHALL allow update of: name (60 chars), address (160 chars)
- SHALL NOT allow update of: sortcode, customer_number, date_of_birth, credit_score, cs_review_date
- SHALL reject requests attempting to modify immutable fields

**Acceptance Test:** Submit update with modified name and address; verify success. Submit update attempting to modify DOB; verify rejection

**FR-CUST-003.2: Update Process**
- SHALL read existing customer record using composite key
- SHALL validate customer exists before update
- SHALL update only specified modifiable fields
- SHALL write updated record back to VSAM
- SHALL use optimistic locking to prevent concurrent update conflicts

**Acceptance Test:** Update customer name; verify only name changed. Attempt concurrent updates; verify one succeeds and one is rejected

**FR-CUST-003.3: No Audit Trail**
- SHALL NOT create PROCTRAN record for customer updates (business rule exception)
- This is intentional - only creates and deletes are audited

**Acceptance Test:** Update customer and verify no PROCTRAN record created

**Traced to:** UPDCUST.cbl, CUSTOMER.cpy

---

### FR-CUST-004: Delete Customer with Cascade

**Priority:** High  
**Source Program:** DELCUS.cbl (lines 1-762)  
**Complexity:** Medium

#### Functional Description

The system shall delete a customer and all associated accounts atomically.

#### Detailed Requirements

**FR-CUST-004.1: Pre-Delete Validation**
- SHALL verify customer exists using composite key (sortcode + customer_number)
- SHALL retrieve customer details for audit logging
- SHALL query Db2 ACCOUNT table for all customer accounts

**Acceptance Test:** Attempt to delete non-existent customer; verify appropriate error. Delete customer with 0 accounts; verify success

**FR-CUST-004.2: Cascade Delete Logic**
- SHALL delete all associated account records from Db2 ACCOUNT table FIRST
- SHALL delete customer record from VSAM CUSTOMER file SECOND
- SHALL be atomic - rollback all deletes if any fails
- SHALL NOT leave orphaned accounts after customer deletion

**Acceptance Test:** Create customer with 3 accounts; delete customer; verify all 3 accounts and customer deleted. Simulate failure during account deletion; verify customer not deleted

**FR-CUST-004.3: Audit Trail**
- SHALL create PROCTRAN record with type 'ODC' (branch) or 'IDC' (web)
- SHALL record sortcode, customer_number
- SHALL record name excerpt (first 14 characters)
- SHALL record DOB in format YYYY-MM-DD
- SHALL record deletion before performing delete operation

**Acceptance Test:** Delete customer; verify PROCTRAN record created with correct type and customer details

**FR-CUST-004.4: Referential Integrity**
- SHALL verify no orphaned accounts remain after deletion
- SHALL execute post-delete query: `SELECT COUNT(*) FROM account WHERE customer_number = ?`
- SHALL assert count = 0

**Acceptance Test:** After customer deletion, query for orphaned accounts; verify count is zero

**Traced to:** DELCUS.cbl, CUSTOMER.cpy, ACCOUNT.cpy, PROCTRAN.cpy

---

## 2. Account Management Requirements

### FR-ACCT-001: Create Account

**Priority:** High  
**Source Program:** CREACC.cbl (lines 1-1248)  
**Complexity:** High

#### Functional Description

The system shall create a new bank account for an existing customer.

#### Detailed Requirements

**FR-ACCT-001.1: Customer Validation**
- SHALL verify customer exists in CUSTOMER file before creating account
- SHALL use composite key (sortcode + customer_number) for lookup
- SHALL reject account creation if customer not found

**Acceptance Test:** Attempt to create account for non-existent customer; verify rejection

**FR-ACCT-001.2: Account Number Generation**
- SHALL enqueue Named Counter resource 'HBNKACC' before incrementing
- SHALL increment counter atomically
- SHALL generate 8-digit account number (zero-padded)
- SHALL combine with 6-digit sortcode for composite key
- SHALL dequeue counter after successful creation or on rollback

**Acceptance Test:** Create 100 accounts concurrently; verify all account numbers are unique and sequential

**FR-ACCT-001.3: Account Type Validation**
- SHALL accept account types: ISA, MORTGAGE, SAVING, LOAN, CURRENT
- SHALL store as 8-character field, left-justified, space-padded
- SHALL reject invalid account types

**Acceptance Test:** Create accounts with each valid type; verify success. Attempt invalid type 'INVALID'; verify rejection

**FR-ACCT-001.4: Interest Rate Validation**
- SHALL accept interest rate in format 9(4)V99 (e.g., 450 = 4.50%)
- SHALL validate range: 0.00% to 9999.99%
- SHALL store with 2 decimal places precision

**Acceptance Test:** Create account with rate 4.50%; verify stored correctly. Attempt rate 10000.00%; verify rejection

**FR-ACCT-001.5: Initial Account Setup**
- SHALL set opened_date to current system date (YYYYMMDD)
- SHALL initialize available_balance to 0.00
- SHALL initialize actual_balance to 0.00
- SHALL set last_statement_date based on business rules
- SHALL set next_statement_date based on statement frequency
- SHALL assign eyecatcher 'ACCT'

**Acceptance Test:** Create account; verify opening date is current date and balances are 0.00

**FR-ACCT-001.6: Overdraft Limit**
- SHALL accept overdraft_limit from 0 to 99,999,999
- SHALL default to 0 if not specified
- SHALL validate non-negative value

**Acceptance Test:** Create account with overdraft 5000; verify stored. Attempt negative overdraft; verify rejection

**FR-ACCT-001.7: Data Persistence**
- SHALL write account record to Db2 ACCOUNT table
- SHALL enforce foreign key relationship to CUSTOMER
- SHALL use composite primary key (sortcode + account_number)

**Acceptance Test:** Create account and query database; verify record exists with correct composite key

**FR-ACCT-001.8: Audit Trail**
- SHALL create PROCTRAN record with type 'OCA' (branch) or 'ICA' (web)
- SHALL record customer_number, account_type
- SHALL record last_statement_date and next_statement_date
- SHALL include 'CREATE' footer in description

**Acceptance Test:** Create account via branch; verify PROCTRAN record with type 'OCA' and correct details

**FR-ACCT-001.9: Transaction Rollback**
- SHALL decrement Named Counter if account write fails
- SHALL dequeue Named Counter on any failure
- SHALL NOT create PROCTRAN record if account write fails
- SHALL rollback all changes atomically

**Acceptance Test:** Simulate database failure during account write; verify counter decremented and dequeued

**Traced to:** CREACC.cbl, ACCOUNT.cpy, PROCTRAN.cpy, NEWACCNO.cpy

---

### FR-ACCT-002: Inquire Single Account

**Priority:** High  
**Source Program:** INQACC.cbl (lines 1-1003)  
**Complexity:** Medium

#### Functional Description

The system shall retrieve account information by composite key.

#### Detailed Requirements

**FR-ACCT-002.1: Account Lookup**
- SHALL accept sortcode (6 digits) and account_number (8 digits)
- SHALL query Db2 ACCOUNT table using composite key
- SHALL return complete account record if found
- SHALL return record with all fields set to low-values if not found
- SHALL NOT create PROCTRAN record (read-only operation)

**Acceptance Test:** Query existing account; verify all fields returned. Query non-existent account; verify low-values record

**FR-ACCT-002.2: Response Data**
- SHALL return all account fields: type, customer_number, interest_rate, opened_date
- SHALL return balance fields: available_balance, actual_balance
- SHALL return statement dates: last_statement_date, next_statement_date
- SHALL return overdraft_limit

**Acceptance Test:** Verify response contains all 12 account fields with correct types

**FR-ACCT-002.3: Performance**
- SHALL complete read operation in < 100ms (95th percentile)
- SHALL support concurrent read operations without blocking

**Acceptance Test:** Execute 1000 concurrent account queries; verify 95th percentile < 100ms

**Traced to:** INQACC.cbl, ACCOUNT.cpy

---

### FR-ACCT-003: Inquire Customer Accounts

**Priority:** High  
**Source Program:** INQACCCU.cbl (lines 1-883)  
**Complexity:** Medium

#### Functional Description

The system shall retrieve all accounts for a given customer using cursor-based pagination.

#### Detailed Requirements

**FR-ACCT-003.1: Customer Account Query**
- SHALL accept customer_number (10 digits)
- SHALL query Db2 ACCOUNT table for all accounts with matching customer_number
- SHALL use Db2 cursor for result set iteration
- SHALL support pagination for customers with many accounts

**Acceptance Test:** Query customer with 5 accounts; verify all 5 returned

**FR-ACCT-003.2: Cursor Management**
- SHALL open Db2 cursor at beginning of query
- SHALL fetch accounts in batches for efficiency
- SHALL close cursor after all results retrieved or on error
- SHALL NOT leave cursors open (resource leak prevention)

**Acceptance Test:** Query account list; verify cursor properly closed. Simulate error mid-query; verify cursor closed

**FR-ACCT-003.3: Pagination Support**
- SHALL support retrieving accounts in pages (e.g., 10 per page)
- SHALL maintain cursor position between page requests
- SHALL handle customers with 0 to N accounts gracefully

**Acceptance Test:** Query customer with 25 accounts using page size 10; verify 3 pages returned correctly

**FR-ACCT-003.4: Empty Result Handling**
- SHALL return empty list (not error) if customer has no accounts
- SHALL NOT fail if customer_number not found

**Acceptance Test:** Query customer with zero accounts; verify empty list returned

**Traced to:** INQACCCU.cbl, ACCOUNT.cpy

---

### FR-ACCT-004: Update Account

**Priority:** Medium  
**Source Program:** UPDACC.cbl (lines 1-407)  
**Complexity:** Low

#### Functional Description

The system shall update specific account fields while preventing modification of immutable data.

#### Detailed Requirements

**FR-ACCT-004.1: Field Update Rules**
- SHALL allow update of: account_type, interest_rate, overdraft_limit, last_statement_date, next_statement_date
- SHALL NOT allow update of: sortcode, account_number, customer_number, opened_date, available_balance, actual_balance
- SHALL reject requests attempting to modify immutable fields

**Acceptance Test:** Update account type and interest rate; verify success. Attempt to modify opened_date; verify rejection

**FR-ACCT-004.2: Business Rule Validation**
- SHALL validate interest_rate: 0.00% to 9999.99%
- SHALL validate overdraft_limit: non-negative integer
- SHALL validate account_type: ISA, MORTGAGE, SAVING, LOAN, CURRENT

**Acceptance Test:** Update interest rate to negative value; verify rejection. Update to valid value; verify success

**FR-ACCT-004.3: Update Process**
- SHALL read existing account record using composite key
- SHALL validate account exists before update
- SHALL update only specified modifiable fields
- SHALL write updated record back to Db2

**Acceptance Test:** Update account overdraft limit; verify only overdraft changed

**FR-ACCT-004.4: Audit Trail**
- SHALL create PROCTRAN record for account updates
- SHALL record updated field values in description

**Acceptance Test:** Update account; verify PROCTRAN record created with update details

**Traced to:** UPDACC.cbl, ACCOUNT.cpy, PROCTRAN.cpy

---

### FR-ACCT-005: Delete Account

**Priority:** Medium  
**Source Program:** DELACC.cbl (lines 1-650)  
**Complexity:** Medium

#### Functional Description

The system shall delete an account and record the deletion in audit trail.

#### Detailed Requirements

**FR-ACCT-005.1: Pre-Delete Validation**
- SHALL verify account exists using composite key
- SHALL retrieve account details for audit logging
- OPTIONAL: Verify zero balance before deletion (business rule decision)

**Acceptance Test:** Attempt to delete non-existent account; verify error. Delete account with zero balance; verify success

**FR-ACCT-005.2: Deletion Process**
- SHALL delete account record from Db2 ACCOUNT table
- SHALL maintain referential integrity with CUSTOMER
- SHALL NOT orphan the parent customer record

**Acceptance Test:** Delete account; verify account removed but customer remains

**FR-ACCT-005.3: Audit Trail**
- SHALL create PROCTRAN record with type 'ODA' (branch) or 'IDA' (web)
- SHALL record customer_number, account_type
- SHALL record last_statement_date, next_statement_date
- SHALL include 'DELETE' footer in description

**Acceptance Test:** Delete account; verify PROCTRAN record with type 'ODA' and account details before deletion

**FR-ACCT-005.4: Transaction Integrity**
- SHALL execute deletion within transaction
- SHALL rollback on any failure

**Acceptance Test:** Simulate failure during PROCTRAN write; verify account not deleted

**Traced to:** DELACC.cbl, ACCOUNT.cpy, PROCTRAN.cpy

---

## 3. Transaction Processing Requirements

### FR-TRAN-001: Transfer Funds

**Priority:** Critical  
**Source Program:** XFRFUN.cbl (lines 1-1925)  
**Complexity:** High

#### Functional Description

The system shall transfer funds between two accounts atomically, updating both balances and creating audit trail.

#### Detailed Requirements

**FR-TRAN-001.1: Input Validation**
- SHALL accept source account (sortcode + account_number)
- SHALL accept target account (sortcode + account_number)
- SHALL accept transfer amount (up to 9,999,999,999.99)
- SHALL accept optional transfer description
- SHALL reject if source = target (same account)

**Acceptance Test:** Attempt transfer to same account; verify rejection. Transfer between different accounts; verify success

**FR-TRAN-001.2: Pre-Transfer Validation**
- SHALL verify source account exists and is active
- SHALL verify target account exists and is active
- SHALL verify source account has sufficient available balance
- SHALL calculate: available_balance + overdraft_limit >= transfer_amount
- SHALL reject if insufficient funds

**Acceptance Test:** Attempt transfer with insufficient balance; verify rejection with descriptive error

**FR-TRAN-001.3: Atomic Dual-Account Update**
- SHALL execute as single atomic transaction
- SHALL update source account:
  - Decrease available_balance by transfer_amount
  - Decrease actual_balance by transfer_amount
- SHALL update target account:
  - Increase available_balance by transfer_amount
  - Increase actual_balance by transfer_amount
- SHALL commit both updates or rollback both on any failure

**Acceptance Test:** Execute transfer; verify both balances updated. Simulate failure after first update; verify both balances unchanged (rollback)

**FR-TRAN-001.4: Balance Consistency**
- SHALL update both available and actual balances simultaneously
- SHALL maintain balance integrity: actual_balance + pending_transactions = available_balance
- SHALL allow available_balance to go negative up to overdraft_limit
- SHALL NOT allow actual_balance below (0 - overdraft_limit)

**Acceptance Test:** Transfer amount that exceeds balance but within overdraft; verify success with negative available_balance

**FR-TRAN-001.5: Audit Trail**
- SHALL create PROCTRAN record with type 'TFR'
- SHALL set description format: "TRANSFER" + target_sortcode (6) + target_account_number (8)
- SHALL record transfer amount as positive value
- SHALL capture transaction date and time
- SHALL may create separate PROCTRAN records for source and target accounts

**Acceptance Test:** Execute transfer; verify PROCTRAN record(s) with type 'TFR' and correct description format

**FR-TRAN-001.6: Rollback Handling**
- SHALL rollback both account updates on any failure
- SHALL restore exact original balances on rollback
- SHALL NOT create PROCTRAN record on rollback
- SHALL log rollback reason for troubleshooting

**Acceptance Test:** Simulate database failure during second account update; verify first account balance restored to original value

**FR-TRAN-001.7: Overdraft Logic**
- SHALL allow available_balance to become negative if within overdraft_limit
- SHALL calculate: final_available_balance >= (0 - overdraft_limit)
- SHALL reject transfer if would exceed overdraft limit

**Acceptance Test:** Account with balance 100 and overdraft 500; transfer 400; verify available_balance = -300 (within overdraft)

**FR-TRAN-001.8: Performance**
- SHALL complete transfer in < 500ms (95th percentile)
- SHALL support concurrent transfers without deadlocks
- SHALL use appropriate database isolation level

**Acceptance Test:** Execute 100 concurrent transfers; verify all succeed and 95th percentile < 500ms

**Traced to:** XFRFUN.cbl, ACCOUNT.cpy, PROCTRAN.cpy

---

### FR-TRAN-002: Debit/Credit Operations

**Priority:** High  
**Source Program:** DBCRFUN.cbl (lines 1-862)  
**Complexity:** Medium

#### Functional Description

The system shall process deposits (credits) and withdrawals (debits) on individual accounts.

#### Detailed Requirements

**FR-TRAN-002.1: Operation Types**
- SHALL support transaction types:
  - CRE: Credit/Deposit
  - DEB: Debit/Withdrawal
  - CHI: Cheque Paid In
  - CHO: Cheque Paid Out
  - CHA: Cheque Acknowledged
  - CHF: Cheque Failure

**Acceptance Test:** Execute each transaction type; verify correct processing

**FR-TRAN-002.2: Credit Operation (Deposit)**
- SHALL accept account composite key and amount
- SHALL increase available_balance by amount
- SHALL increase actual_balance by amount
- SHALL always succeed (no balance validation required)
- SHALL create PROCTRAN record with type 'CRE'

**Acceptance Test:** Deposit 100 to account with balance 500; verify balances updated to 600

**FR-TRAN-002.3: Debit Operation (Withdrawal)**
- SHALL accept account composite key and amount
- SHALL validate sufficient funds: available_balance + overdraft_limit >= amount
- SHALL decrease available_balance by amount
- SHALL decrease actual_balance by amount
- SHALL reject if insufficient funds
- SHALL create PROCTRAN record with type 'DEB'

**Acceptance Test:** Withdraw 100 from account with balance 500; verify balances updated to 400. Attempt withdraw 700 with no overdraft; verify rejection

**FR-TRAN-002.4: Balance Updates**
- SHALL update both available_balance and actual_balance atomically
- SHALL maintain balance consistency
- SHALL respect overdraft limits for debits

**Acceptance Test:** Execute debit; verify both balances decreased by same amount

**FR-TRAN-002.5: Audit Trail**
- SHALL create PROCTRAN record for each operation
- SHALL set transaction_type to operation code (CRE, DEB, etc.)
- SHALL record transaction amount
- SHALL capture timestamp (date and time separately)
- SHALL record transaction reference number

**Acceptance Test:** Execute credit and debit; verify two PROCTRAN records with correct types and amounts

**FR-TRAN-002.6: Timestamp Capture**
- SHALL capture transaction_date in YYYYMMDD format
- SHALL capture transaction_time in HHMMSS format
- SHALL use system clock at time of processing

**Acceptance Test:** Execute transaction; verify PROCTRAN has current date and time

**FR-TRAN-002.7: Performance**
- SHALL complete debit/credit in < 200ms (95th percentile)
- SHALL support concurrent operations on different accounts

**Acceptance Test:** Execute 1000 concurrent debits/credits; verify 95th percentile < 200ms

**Traced to:** DBCRFUN.cbl, ACCOUNT.cpy, PROCTRAN.cpy

---

## 4. System Utility Requirements

### FR-UTIL-001: Get Company Information

**Priority:** Low  
**Source Program:** GETCOMPY.cbl  
**Complexity:** Low

#### Functional Description

The system shall return the company name for display purposes.

#### Detailed Requirements

**FR-UTIL-001.1: Company Name**
- SHALL return constant string: "CICS Bank Sample Application"
- SHALL be consistent across all invocations
- SHALL NOT require database access

**Acceptance Test:** Call utility 100 times; verify same company name returned each time

**FR-UTIL-001.2: Performance**
- SHALL complete in < 10ms

**Acceptance Test:** Verify response time < 10ms

**Traced to:** GETCOMPY.cbl

---

### FR-UTIL-002: Get Sort Code

**Priority:** Low  
**Source Program:** GETSCODE.cbl  
**Complexity:** Low

#### Functional Description

The system shall return the bank sort code for transactions.

#### Detailed Requirements

**FR-UTIL-002.1: Sort Code Value**
- SHALL return constant string: "987654"
- SHALL be consistent across all invocations
- SHALL be 6-digit format

**Acceptance Test:** Call utility; verify sort code "987654" returned

**FR-UTIL-002.2: Performance**
- SHALL complete in < 10ms

**Acceptance Test:** Verify response time < 10ms

**Traced to:** GETSCODE.cbl, SORTCODE.cpy

---

### FR-UTIL-003: Error Processing

**Priority:** Medium  
**Source Program:** ABNDPROC.cbl  
**Complexity:** Low

#### Functional Description

The system shall provide centralized error handling and logging.

#### Detailed Requirements

**FR-UTIL-003.1: Error Logging**
- SHALL log all program abends with context
- SHALL capture: program_name, timestamp, error_code, error_message, stack_trace
- SHALL write to error log (database or file)

**Acceptance Test:** Trigger error in program; verify error logged with all required fields

**FR-UTIL-003.2: Error Context**
- SHALL provide sufficient context for debugging
- SHALL include request parameters if available
- SHALL include user/session information if applicable

**Acceptance Test:** Verify error log includes request details and user context

**FR-UTIL-003.3: Error Handling**
- SHALL return appropriate error codes to caller
- SHALL NOT expose internal error details to external clients
- SHALL log full error details for internal diagnostics

**Acceptance Test:** Trigger error; verify client receives generic error message but log contains detailed diagnostics

**Traced to:** ABNDPROC.cbl, ABNDINFO.cpy

---

## 5. Cross-Cutting Requirements

### FR-CROSS-001: PROCTRAN Audit Logging

**Priority:** High  
**Applies to:** All create, delete, transfer, and debit/credit operations

#### Functional Description

The system shall maintain comprehensive audit trail of all data modifications.

#### Detailed Requirements

**FR-CROSS-001.1: Audit Scope**
- SHALL create PROCTRAN record for: customer create, customer delete, account create, account delete, fund transfer, debit, credit
- SHALL NOT create PROCTRAN record for: customer update, account update, read operations

**Acceptance Test:** Execute each operation type; verify PROCTRAN records created only for specified operations

**FR-CROSS-001.2: Audit Data**
- SHALL record eyecatcher 'PRTR'
- SHALL record sortcode and transaction_number
- SHALL record transaction_date and transaction_time
- SHALL record transaction_type code
- SHALL record descriptive text (40 characters)
- SHALL record amount (if applicable)

**Acceptance Test:** Query PROCTRAN after operation; verify all required fields populated

**FR-CROSS-001.3: Transaction Types**
- SHALL use correct type codes per operation:
  - OCC/ICC: Customer create
  - ODC/IDC: Customer delete
  - OCA/ICA: Account create
  - ODA/IDA: Account delete
  - TFR: Transfer funds
  - DEB: Debit
  - CRE: Credit

**Acceptance Test:** Verify type codes match COBOL 88-level values exactly

**FR-CROSS-001.4: Logical Delete**
- SHALL support logical delete via logically_deleted flag
- SHALL NOT physically delete PROCTRAN records
- SHALL filter out logically deleted records in queries by default

**Acceptance Test:** Mark record as logically deleted; verify not returned in standard queries

---

### FR-CROSS-002: Named Counter Management

**Priority:** Critical  
**Applies to:** Customer create, account create

#### Functional Description

The system shall provide thread-safe, atomic counter increment with rollback support.

#### Detailed Requirements

**FR-CROSS-002.1: Counter Types**
- SHALL maintain counter 'HBNKCUST' for customer numbers
- SHALL maintain counter 'HBNKACC' for account numbers

**Acceptance Test:** Verify both counters exist in CONTROL table

**FR-CROSS-002.2: Atomic Increment**
- SHALL increment counter atomically (thread-safe)
- SHALL prevent duplicate counter values
- SHALL use database-level locking or equivalent mechanism

**Acceptance Test:** Increment counter 1000 times concurrently; verify all values unique and sequential

**FR-CROSS-002.3: Rollback Support**
- SHALL decrement counter if transaction rolled back
- SHALL restore counter to pre-increment value
- SHALL handle rollback within same database transaction

**Acceptance Test:** Increment counter then rollback transaction; verify counter value unchanged

**FR-CROSS-002.4: Counter Initialization**
- SHALL initialize customer counter to 1 on first use
- SHALL initialize account counter to 1 on first use
- SHALL persist counter values across system restarts

**Acceptance Test:** Initialize database; verify counter values set to 1. Restart system; verify counter values preserved

---

### FR-CROSS-003: Date Handling

**Priority:** High  
**Applies to:** All date fields

#### Functional Description

The system shall handle dates consistently between COBOL and Java.

#### Detailed Requirements

**FR-CROSS-003.1: Date Format Conversion**
- SHALL convert COBOL YYYYMMDD (8 digits) to Java LocalDate
- SHALL convert Java LocalDate to COBOL YYYYMMDD format
- SHALL preserve date values exactly during conversion

**Acceptance Test:** Convert date "20251027" to LocalDate and back; verify value unchanged

**FR-CROSS-003.2: Date Validation**
- SHALL validate minimum year 1601 (COBOL CEEDAYS limitation)
- SHALL validate maximum age 150 years from current date
- SHALL reject future dates for date_of_birth fields
- SHALL use fail codes: 'O' for year/age violations, 'Y' for future dates

**Acceptance Test:** Validate dates with years 1600, 1601, future date; verify correct acceptance/rejection

**FR-CROSS-003.3: Current Date**
- SHALL use system clock for current date
- SHALL format as YYYYMMDD for COBOL compatibility
- SHALL use LocalDate.now() in Java

**Acceptance Test:** Capture current date; verify format matches YYYYMMDD

---

### FR-CROSS-004: Decimal Precision

**Priority:** High  
**Applies to:** All monetary amounts and interest rates

#### Functional Description

The system shall handle decimal values without rounding errors.

#### Detailed Requirements

**FR-CROSS-004.1: BigDecimal Usage**
- SHALL use java.math.BigDecimal for all monetary amounts
- SHALL use java.math.BigDecimal for interest rates
- SHALL NOT use float or double for money

**Acceptance Test:** Verify all money fields use BigDecimal type

**FR-CROSS-004.2: Precision**
- SHALL maintain 2 decimal places for amounts (e.g., 123.45)
- SHALL maintain 2 decimal places for interest rates (e.g., 4.50%)
- SHALL use RoundingMode.HALF_UP for any required rounding

**Acceptance Test:** Store value 123.456; verify rounded to 123.46

**FR-CROSS-004.3: COBOL Conversion**
- SHALL convert COBOL PIC S9(10)V99 to BigDecimal with scale 2
- SHALL convert COBOL PIC 9(4)V99 (interest rate) to BigDecimal with scale 2
- SHALL multiply COBOL integer interest by 0.01 (e.g., 450 -> 4.50)

**Acceptance Test:** Convert COBOL value 450 to BigDecimal; verify 4.50

---

## 6. Acceptance Criteria

### 6.1 Functional Acceptance

**FA1:** All functional requirements (FR-CUST-001 through FR-CROSS-004) SHALL be implemented and tested

**FA2:** All legacy COBOL business rules SHALL be preserved in Java implementation

**FA3:** All data validations SHALL produce same results as COBOL programs

**FA4:** All audit trail patterns SHALL match COBOL behavior exactly

### 6.2 Integration Acceptance

**IA1:** All COBOL programs SHALL have equivalent Java services or REST endpoints

**IA2:** All copybook structures SHALL have equivalent Java entities

**IA3:** All transaction boundaries SHALL be preserved in Java

### 6.3 Performance Acceptance

**PA1:** Read operations SHALL complete in < 100ms (95th percentile)

**PA2:** Write operations SHALL complete in < 200ms (95th percentile)

**PA3:** Transfer operations SHALL complete in < 500ms (95th percentile)

**PA4:** Performance SHALL be within 20% of COBOL baseline (after optimization)

### 6.4 Testing Acceptance

**TA1:** Unit tests SHALL cover 80%+ of service layer code

**TA2:** Integration tests SHALL cover 70%+ of repository layer code

**TA3:** End-to-end tests SHALL validate complete workflows

**TA4:** Legacy comparison tests SHALL validate identical behavior with COBOL

### 6.5 Migration Acceptance

**MA1:** All 11 business logic programs SHALL be migrated

**MA2:** All 14 completed programs (9 UI + 5 utilities) SHALL be accounted for

**MA3:** All test cases SHALL pass before production deployment

**MA4:** All audit trail data SHALL be validated against legacy system

---

## Appendix A: Functional Requirement Traceability Matrix

| Requirement | COBOL Program | Lines | Priority | Complexity |
|-------------|---------------|-------|----------|------------|
| FR-CUST-001 | CRECUST.cbl | 1-1440 | High | High |
| FR-CUST-002 | INQCUST.cbl | 1-712 | High | Medium |
| FR-CUST-003 | UPDCUST.cbl | 1-365 | Medium | Low |
| FR-CUST-004 | DELCUS.cbl | 1-762 | High | Medium |
| FR-ACCT-001 | CREACC.cbl | 1-1248 | High | High |
| FR-ACCT-002 | INQACC.cbl | 1-1003 | High | Medium |
| FR-ACCT-003 | INQACCCU.cbl | 1-883 | High | Medium |
| FR-ACCT-004 | UPDACC.cbl | 1-407 | Medium | Low |
| FR-ACCT-005 | DELACC.cbl | 1-650 | Medium | Medium |
| FR-TRAN-001 | XFRFUN.cbl | 1-1925 | Critical | High |
| FR-TRAN-002 | DBCRFUN.cbl | 1-862 | High | Medium |
| FR-UTIL-001 | GETCOMPY.cbl | - | Low | Low |
| FR-UTIL-002 | GETSCODE.cbl | - | Low | Low |
| FR-UTIL-003 | ABNDPROC.cbl | - | Medium | Low |

---

## Document Control

**Approvals Required:**
- Business Analyst: Review functional requirements accuracy
- QA Lead: Review acceptance criteria and test coverage
- Technical Lead: Review traceability to COBOL programs

**Related Documents:**
- 01_Business_Processes.md
- 02_Data_Structures.md
- 04_Integration_Points.md
- 05_Non_Functional_Requirements.md
