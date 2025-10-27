# Data Structures Requirements

**Document Version:** 1.0  
**Last Updated:** 2025-10-27  
**Target System:** CBSA Java Migration (Spring Boot + SQLite)  
**Legacy System:** CICS Banking Sample Application (COBOL + VSAM + Db2)

## Executive Summary

This document defines all data structures used in the CICS Banking Sample Application, mapping legacy COBOL copybooks and data stores to modern Java entities and database schemas. Each structure includes field-level specifications, validation rules, and modernization mappings.

## Table of Contents

1. [Data Store Overview](#data-store-overview)
2. [Customer Data Structure](#customer-data-structure)
3. [Account Data Structure](#account-data-structure)
4. [Transaction Audit Structure](#transaction-audit-structure)
5. [Control Data Structure](#control-data-structure)
6. [Acceptance Criteria](#acceptance-criteria)
7. [Modernization Mappings](#modernization-mappings)

---

## 1. Data Store Overview

### 1.1 Legacy Data Stores

| Data Store | Type | Purpose | Records |
|------------|------|---------|---------|
| CUSTOMER | VSAM KSDS | Customer master data | ~100-10,000 |
| ACCOUNT | Db2 Table | Account master data | ~200-50,000 |
| PROCTRAN | Db2 Table | Transaction audit trail | ~1,000-1,000,000 |
| CONTROL | Db2 Table | System control records | 1-10 |
| ABNDFILE | VSAM KSDS | Error logging | Variable |

### 1.2 Migration Data Stores

| Data Store | Type | Purpose |
|------------|------|---------|
| customer | SQLite Table | Customer master data |
| account | SQLite Table | Account master data |
| bank_transaction (PROCTRAN) | SQLite Table | Transaction audit trail |
| control | SQLite Table | System control records |

**Rationale:** SQLite chosen for portability and simplicity in Java migration. Schema preserves COBOL data patterns including eye-catchers, composite keys, and date formats.

---

## 2. Customer Data Structure

### 2.1 COBOL Copybook Definition

**Source:** `CUSTOMER.cpy` (35 lines)  
**VSAM File:** CUSTOMER (KSDS - Key Sequenced Data Set)

```cobol
03 CUSTOMER-RECORD.
   05 CUSTOMER-EYECATCHER                 PIC X(4).
      88 CUSTOMER-EYECATCHER-VALUE        VALUE 'CUST'.
   05 CUSTOMER-KEY.
      07 CUSTOMER-SORTCODE                PIC 9(6) DISPLAY.
      07 CUSTOMER-NUMBER                  PIC 9(10) DISPLAY.
   05 CUSTOMER-NAME                       PIC X(60).
   05 CUSTOMER-ADDRESS                    PIC X(160).
   05 CUSTOMER-DATE-OF-BIRTH              PIC 9(8).
   05 CUSTOMER-DOB-GROUP REDEFINES CUSTOMER-DATE-OF-BIRTH.
      07 CUSTOMER-BIRTH-DAY               PIC 99.
      07 CUSTOMER-BIRTH-MONTH             PIC 99.
      07 CUSTOMER-BIRTH-YEAR              PIC 9999.
   05 CUSTOMER-CREDIT-SCORE               PIC 999.
   05 CUSTOMER-CS-REVIEW-DATE             PIC 9(8).
   05 CUSTOMER-CS-GROUP REDEFINES CUSTOMER-CS-REVIEW-DATE.
      07 CUSTOMER-CS-REVIEW-DAY           PIC 99.
      07 CUSTOMER-CS-REVIEW-MONTH         PIC 99.
      07 CUSTOMER-CS-REVIEW-YEAR          PIC 9999.
```

### 2.2 Field Specifications

| Field Name | COBOL Type | Length | Java Type | Constraints | Description |
|------------|------------|--------|-----------|-------------|-------------|
| eyecatcher | PIC X(4) | 4 | String | CHECK = 'CUST' | Record type identifier |
| sortcode | PIC 9(6) | 6 | String | PRIMARY KEY (part 1) | Bank sort code |
| customer_number | PIC 9(10) | 10 | String | PRIMARY KEY (part 2) | Unique customer ID |
| name | PIC X(60) | 60 | String | NOT NULL | Customer full name |
| address | PIC X(160) | 160 | String | NOT NULL | Customer full address |
| date_of_birth | PIC 9(8) | 8 | LocalDate | NOT NULL | Format: YYYYMMDD |
| credit_score | PIC 999 | 3 | Integer | 0-999 | Credit score (0-999) |
| cs_review_date | PIC 9(8) | 8 | LocalDate | NOT NULL | Next credit score review |

### 2.3 Composite Primary Key

**Key Structure:** `CUSTOMER-SORTCODE (6) + CUSTOMER-NUMBER (10) = 16 digits`

**Example:** `987654` + `0000000001` = `9876540000000001`

### 2.4 Data Validation Rules

#### Eyecatcher Validation
- Must be exactly 'CUST' (uppercase, 4 characters)
- Validates record type when reading from VSAM
- CHECK constraint in SQLite: `eyecatcher = 'CUST'`

#### Date of Birth Validation
- Format: YYYYMMDD (8 digits, numeric)
- Minimum year: 1601 (COBOL CEEDAYS limitation)
- Maximum age: 150 years from current date
- Must not be future date
- Validation fail codes:
  - 'O': Year before 1601 or age > 150 years
  - 'Y': Future date

#### Credit Score Validation
- Range: 0-999 (3 digits)
- 0 indicates no credit check performed or failed
- Non-zero indicates average of credit agency responses

#### Name and Address
- Name: 60 characters, left-justified, space-padded
- Address: 160 characters, left-justified, space-padded
- No special character restrictions in legacy system

### 2.5 SQLite Schema Definition

```sql
CREATE TABLE IF NOT EXISTS customer (
    eyecatcher TEXT NOT NULL CHECK(eyecatcher = 'CUST'),
    sortcode TEXT NOT NULL,
    customer_number TEXT NOT NULL,
    name TEXT NOT NULL,
    address TEXT NOT NULL,
    date_of_birth TEXT NOT NULL,
    credit_score INTEGER NOT NULL CHECK(credit_score >= 0 AND credit_score <= 999),
    cs_review_date TEXT NOT NULL,
    PRIMARY KEY (sortcode, customer_number)
);

CREATE INDEX IF NOT EXISTS idx_customer_number ON customer(customer_number);
CREATE INDEX IF NOT EXISTS idx_customer_name ON customer(name);
```

### 2.6 Java Entity Mapping

```java
@Data
@Table(name = "customer")
public class Customer {
    @Column(name = "eyecatcher", nullable = false)
    private String eyecatcher = "CUST";
    
    @Column(name = "sortcode", nullable = false)
    private String sortcode;
    
    @Column(name = "customer_number", nullable = false)
    private String customerNumber;
    
    @Column(name = "name", nullable = false, length = 60)
    private String name;
    
    @Column(name = "address", nullable = false, length = 160)
    private String address;
    
    @Column(name = "date_of_birth", nullable = false)
    private LocalDate dateOfBirth;
    
    @Column(name = "credit_score", nullable = false)
    private Integer creditScore;
    
    @Column(name = "cs_review_date", nullable = false)
    private LocalDate csReviewDate;
}
```

### 2.7 Acceptance Criteria

- **AC2.1:** Composite primary key must enforce uniqueness across sortcode + customer_number
- **AC2.2:** Eyecatcher must always be 'CUST' for valid records
- **AC2.3:** Date of birth must reject years before 1601, ages > 150, and future dates
- **AC2.4:** Credit score must be in range 0-999
- **AC2.5:** Name and address must support full 60 and 160 character lengths
- **AC2.6:** All text fields must preserve trailing spaces from COBOL

**Traced to:** CUSTOMER.cpy, CRECUST.cbl, INQCUST.cbl, UPDCUST.cbl, DELCUS.cbl

---

## 3. Account Data Structure

### 3.1 COBOL Copybook Definition

**Source:** `ACCOUNT.cpy` (36 lines)  
**Db2 Table:** ACCOUNT

```cobol
03 ACCOUNT-DATA.
   05 ACCOUNT-EYE-CATCHER        PIC X(4).
   88 ACCOUNT-EYECATCHER-VALUE        VALUE 'ACCT'.
   05 ACCOUNT-CUST-NO            PIC 9(10).
   05 ACCOUNT-KEY.
      07 ACCOUNT-SORT-CODE       PIC 9(6).
      07 ACCOUNT-NUMBER          PIC 9(8).
   05 ACCOUNT-TYPE               PIC X(8).
   05 ACCOUNT-INTEREST-RATE      PIC 9(4)V99.
   05 ACCOUNT-OPENED             PIC 9(8).
   05 ACCOUNT-OPENED-GROUP REDEFINES ACCOUNT-OPENED.
      07 ACCOUNT-OPENED-DAY       PIC 99.
      07 ACCOUNT-OPENED-MONTH     PIC 99.
      07 ACCOUNT-OPENED-YEAR      PIC 9999.
   05 ACCOUNT-OVERDRAFT-LIMIT    PIC 9(8).
   05 ACCOUNT-LAST-STMT-DATE     PIC 9(8).
   05 ACCOUNT-LAST-STMT-GROUP REDEFINES ACCOUNT-LAST-STMT-DATE.
      07 ACCOUNT-LAST-STMT-DAY   PIC 99.
      07 ACCOUNT-LAST-STMT-MONTH PIC 99.
      07 ACCOUNT-LAST-STMT-YEAR  PIC 9999.
   05 ACCOUNT-NEXT-STMT-DATE     PIC 9(8).
   05 ACCOUNT-NEXT-STMT-GROUP REDEFINES ACCOUNT-NEXT-STMT-DATE.
      07 ACCOUNT-NEXT-STMT-DAY   PIC 99.
      07 ACCOUNT-NEXT-STMT-MONTH PIC 99.
      07 ACCOUNT-NEXT-STMT-YEAR  PIC 9999.
   05 ACCOUNT-AVAILABLE-BALANCE  PIC S9(10)V99.
   05 ACCOUNT-ACTUAL-BALANCE     PIC S9(10)V99.
```

### 3.2 Field Specifications

| Field Name | COBOL Type | Length | Java Type | Constraints | Description |
|------------|------------|--------|-----------|-------------|-------------|
| eyecatcher | PIC X(4) | 4 | String | CHECK = 'ACCT' | Record type identifier |
| sortcode | PIC 9(6) | 6 | String | PRIMARY KEY (part 1) | Bank sort code |
| account_number | PIC 9(8) | 8 | String | PRIMARY KEY (part 2) | Unique account ID |
| customer_number | PIC 9(10) | 10 | String | FOREIGN KEY | Links to customer |
| account_type | PIC X(8) | 8 | String | NOT NULL | ISA, MORTGAGE, SAVING, LOAN, CURRENT |
| interest_rate | PIC 9(4)V99 | 6 | BigDecimal | NOT NULL | Interest rate (e.g., 450 = 4.50%) |
| opened_date | PIC 9(8) | 8 | LocalDate | NOT NULL | Account opening date |
| overdraft_limit | PIC 9(8) | 8 | Integer | NOT NULL | Maximum overdraft allowed |
| last_statement_date | PIC 9(8) | 8 | LocalDate | NOT NULL | Last statement date |
| next_statement_date | PIC 9(8) | 8 | LocalDate | NOT NULL | Next statement date |
| available_balance | PIC S9(10)V99 | 12 | BigDecimal | NOT NULL | Available balance |
| actual_balance | PIC S9(10)V99 | 12 | BigDecimal | NOT NULL | Actual posted balance |

### 3.3 Composite Primary Key

**Key Structure:** `ACCOUNT-SORT-CODE (6) + ACCOUNT-NUMBER (8) = 14 digits`

**Example:** `987654` + `00000001` = `98765400000001`

### 3.4 Data Validation Rules

#### Eyecatcher Validation
- Must be exactly 'ACCT' (uppercase, 4 characters)
- CHECK constraint in SQLite: `eyecatcher = 'ACCT'`

#### Account Type Validation
- Valid types: ISA, MORTGAGE, SAVING, LOAN, CURRENT
- 8 characters, left-justified, space-padded if shorter
- Case-sensitive in legacy system

#### Interest Rate
- Format: 9(4)V99 = 6 digits with 2 decimal places
- Range: 0.00% to 9999.99%
- Stored as integer in COBOL (e.g., 450 = 4.50%)
- Converted to BigDecimal in Java for precision

#### Overdraft Limit
- Range: 0 to 99,999,999
- Zero indicates no overdraft allowed
- Negative values not permitted

#### Balance Fields
- Format: S9(10)V99 = signed 12 digits with 2 decimal places
- Range: -9,999,999,999.99 to 9,999,999,999.99
- Available balance: Real-time balance (includes pending)
- Actual balance: Posted transactions only
- Both updated together in most operations

#### Date Fields
- All dates in YYYYMMDD format
- opened_date: Set to current date on creation, immutable
- last_statement_date: Updated when statement generated
- next_statement_date: Calculated based on statement frequency

### 3.5 Foreign Key Relationships

**Customer Relationship:**
- `account.customer_number` references `customer.customer_number`
- Enforces referential integrity
- Cascade delete: When customer deleted, all accounts must be deleted first

### 3.6 SQLite Schema Definition

```sql
CREATE TABLE IF NOT EXISTS account (
    eyecatcher TEXT NOT NULL CHECK(eyecatcher = 'ACCT'),
    sortcode TEXT NOT NULL,
    account_number TEXT NOT NULL,
    customer_number TEXT NOT NULL,
    account_type TEXT NOT NULL,
    interest_rate REAL NOT NULL,
    opened_date TEXT NOT NULL,
    overdraft_limit INTEGER NOT NULL CHECK(overdraft_limit >= 0),
    last_statement_date TEXT NOT NULL,
    next_statement_date TEXT NOT NULL,
    available_balance REAL NOT NULL,
    actual_balance REAL NOT NULL,
    PRIMARY KEY (sortcode, account_number),
    FOREIGN KEY (sortcode, customer_number) REFERENCES customer(sortcode, customer_number)
);

CREATE INDEX IF NOT EXISTS idx_account_customer ON account(customer_number);
CREATE INDEX IF NOT EXISTS idx_account_type ON account(account_type);
```

### 3.7 Java Entity Mapping

```java
@Data
@Table(name = "account")
public class Account {
    @Column(name = "eyecatcher", nullable = false)
    private String eyecatcher = "ACCT";
    
    @Column(name = "sortcode", nullable = false)
    private String sortcode;
    
    @Column(name = "account_number", nullable = false)
    private String accountNumber;
    
    @Column(name = "customer_number", nullable = false)
    private String customerNumber;
    
    @Column(name = "account_type", nullable = false, length = 8)
    private String accountType;
    
    @Column(name = "interest_rate", nullable = false, precision = 6, scale = 2)
    private BigDecimal interestRate;
    
    @Column(name = "opened_date", nullable = false)
    private LocalDate openedDate;
    
    @Column(name = "overdraft_limit", nullable = false)
    private Integer overdraftLimit;
    
    @Column(name = "last_statement_date", nullable = false)
    private LocalDate lastStatementDate;
    
    @Column(name = "next_statement_date", nullable = false)
    private LocalDate nextStatementDate;
    
    @Column(name = "available_balance", nullable = false, precision = 12, scale = 2)
    private BigDecimal availableBalance;
    
    @Column(name = "actual_balance", nullable = false, precision = 12, scale = 2)
    private BigDecimal actualBalance;
}
```

### 3.8 Acceptance Criteria

- **AC3.1:** Composite primary key must enforce uniqueness across sortcode + account_number
- **AC3.2:** Eyecatcher must always be 'ACCT' for valid records
- **AC3.3:** Foreign key must reference valid customer (sortcode + customer_number)
- **AC3.4:** Account type must be one of: ISA, MORTGAGE, SAVING, LOAN, CURRENT
- **AC3.5:** Interest rate must use BigDecimal to prevent rounding errors
- **AC3.6:** Both available and actual balances must be updated together
- **AC3.7:** Overdraft limit must be non-negative
- **AC3.8:** Opening date must be immutable after account creation

**Traced to:** ACCOUNT.cpy, CREACC.cbl, INQACC.cbl, INQACCCU.cbl, UPDACC.cbl, DELACC.cbl, XFRFUN.cbl, DBCRFUN.cbl

---

## 4. Transaction Audit Structure (PROCTRAN)

### 4.1 COBOL Copybook Definition

**Source:** `PROCTRAN.cpy` (104 lines)  
**Db2 Table:** PROCTRAN (Process Transactions)

```cobol
03 PROC-TRAN-DATA.
   05 PROC-TRAN-EYE-CATCHER        PIC X(4).
   88 PROC-TRAN-VALID VALUE 'PRTR'.
   05 PROC-TRAN-LOGICAL-DELETE-AREA REDEFINES PROC-TRAN-EYE-CATCHER.
      07 PROC-TRAN-LOGICAL-DELETE-FLAG PIC X.
      88 PROC-TRAN-LOGICALLY-DELETED VALUE X'FF'.
      07 FILLER PIC X(3).
   05 PROC-TRAN-ID.
      07 PROC-TRAN-SORT-CODE       PIC 9(6).
      07 PROC-TRAN-NUMBER          PIC 9(8).
   05 PROC-TRAN-DATE               PIC 9(8).
   05 PROC-TRAN-TIME               PIC 9(6).
   05 PROC-TRAN-REF                PIC 9(12).
   05 PROC-TRAN-TYPE               PIC X(3).
   05 PROC-TRAN-DESC               PIC X(40).
   05 PROC-TRAN-AMOUNT             PIC S9(10)V99.
```

### 4.2 Transaction Type Codes

| Code | 88-Level Name | Description |
|------|---------------|-------------|
| CHA | PROC-TY-CHEQUE-ACKNOWLEDGED | Cheque acknowledged |
| CHF | PROC-TY-CHEQUE-FAILURE | Cheque failed |
| CHI | PROC-TY-CHEQUE-PAID-IN | Cheque paid in |
| CHO | PROC-TY-CHEQUE-PAID-OUT | Cheque paid out |
| CRE | PROC-TY-CREDIT | Credit/deposit |
| DEB | PROC-TY-DEBIT | Debit/withdrawal |
| ICA | PROC-TY-WEB-CREATE-ACCOUNT | Web create account |
| ICC | PROC-TY-WEB-CREATE-CUSTOMER | Web create customer |
| IDA | PROC-TY-WEB-DELETE-ACCOUNT | Web delete account |
| IDC | PROC-TY-WEB-DELETE-CUSTOMER | Web delete customer |
| OCA | PROC-TY-BRANCH-CREATE-ACCOUNT | Branch create account |
| OCC | PROC-TY-BRANCH-CREATE-CUSTOMER | Branch create customer |
| ODA | PROC-TY-BRANCH-DELETE-ACCOUNT | Branch delete account |
| ODC | PROC-TY-BRANCH-DELETE-CUSTOMER | Branch delete customer |
| OCS | PROC-TY-CREATE-SODD | Create standing order |
| PCR | PROC-TY-PAYMENT-CREDIT | Payment credit |
| PDR | PROC-TY-PAYMENT-DEBIT | Payment debit |
| TFR | PROC-TY-TRANSFER | Transfer funds |

### 4.3 Field Specifications

| Field Name | COBOL Type | Length | Java Type | Constraints | Description |
|------------|------------|--------|-----------|-------------|-------------|
| eyecatcher | PIC X(4) | 4 | String | CHECK = 'PRTR' | Record type identifier |
| sortcode | PIC 9(6) | 6 | String | NOT NULL | Bank sort code |
| transaction_number | PIC 9(8) | 8 | String | PRIMARY KEY | Unique transaction ID |
| transaction_date | PIC 9(8) | 8 | LocalDate | NOT NULL | Transaction date (YYYYMMDD) |
| transaction_time | PIC 9(6) | 6 | LocalTime | NOT NULL | Transaction time (HHMMSS) |
| reference | PIC 9(12) | 12 | String | NOT NULL | Transaction reference |
| transaction_type | PIC X(3) | 3 | String | NOT NULL | Transaction type code |
| description | PIC X(40) | 40 | String | NOT NULL | Transaction description |
| amount | PIC S9(10)V99 | 12 | BigDecimal | NOT NULL | Transaction amount |
| logical_delete_flag | PIC X | 1 | Boolean | NOT NULL | Soft delete flag (X'FF' = deleted) |

### 4.4 Special Description Formats

#### Transfer Description (PROC-TRAN-DESC-XFR)
```cobol
07 PROC-TRAN-DESC-XFR-HEADER PIC X(26).
   88 PROC-TRAN-DESC-XFR-FLAG VALUE 'TRANSFER'.
07 PROC-TRAN-DESC-XFR-SORTCODE PIC 9(6).
07 PROC-TRAN-DESC-XFR-ACCOUNT PIC 9(8).
```
Format: "TRANSFER" + target_sortcode (6) + target_account (8)

#### Create Account Description (PROC-DESC-CREACC)
```cobol
07 PROC-DESC-CREACC-CUSTOMER PIC 9(10).
07 PROC-DESC-CREACC-ACCTYPE PIC X(8).
07 PROC-DESC-CREACC-LAST-DD PIC 99.
07 PROC-DESC-CREACC-LAST-MM PIC 99.
07 PROC-DESC-CREACC-LAST-YYYY PIC 9999.
07 PROC-DESC-CREACC-NEXT-DD PIC 99.
07 PROC-DESC-CREACC-NEXT-MM PIC 99.
07 PROC-DESC-CREACC-NEXT-YYYY PIC 9999.
07 PROC-DESC-CREACC-FOOTER PIC X(6).
   88 PROC-DESC-CREACC-FLAG VALUE 'CREATE'.
```

#### Delete Customer Description (PROC-DESC-DELCUS)
```cobol
07 PROC-DESC-DELCUS-SORTCODE PIC 9(6).
07 PROC-DESC-DELCUS-CUSTOMER PIC 9(10).
07 PROC-DESC-DELCUS-NAME PIC X(14).
07 PROC-DESC-DELCUS-DOB-YYYY PIC 9999.
07 PROC-DESC-DELCUS-FILLER PIC X.
   88 PROC-DESC-DELCUS-FILLER-SET VALUE '-'.
07 PROC-DESC-DELCUS-DOB-MM PIC 99.
07 PROC-DESC-DELCUS-FILLER2 PIC X.
   88 PROC-DESC-DELCUS-FILLER2-SET VALUE '-'.
07 PROC-DESC-DELCUS-DOB-DD PIC 99.
```

### 4.5 Logical Delete Pattern

**COBOL Implementation:**
- Eyecatcher and logical delete flag occupy same 4 bytes (REDEFINES)
- Active record: eyecatcher = 'PRTR', delete flag = space
- Deleted record: eyecatcher first byte = X'FF', delete flag = X'FF'

**Java Implementation:**
- Separate boolean field: `logicallyDeleted`
- Query filters: `WHERE logically_deleted = false`
- Soft delete: Update `logically_deleted = true` instead of physical delete

### 4.6 SQLite Schema Definition

```sql
CREATE TABLE IF NOT EXISTS bank_transaction (
    eyecatcher TEXT NOT NULL CHECK(eyecatcher = 'PRTR'),
    sortcode TEXT NOT NULL,
    transaction_number TEXT NOT NULL,
    transaction_date TEXT NOT NULL,
    transaction_time TEXT NOT NULL,
    reference TEXT NOT NULL,
    transaction_type TEXT NOT NULL,
    description TEXT NOT NULL,
    amount REAL NOT NULL,
    logically_deleted INTEGER NOT NULL DEFAULT 0,
    PRIMARY KEY (sortcode, transaction_number)
);

CREATE INDEX IF NOT EXISTS idx_transaction_date ON bank_transaction(transaction_date);
CREATE INDEX IF NOT EXISTS idx_transaction_type ON bank_transaction(transaction_type);
CREATE INDEX IF NOT EXISTS idx_transaction_ref ON bank_transaction(reference);
CREATE INDEX IF NOT EXISTS idx_not_deleted ON bank_transaction(logically_deleted) WHERE logically_deleted = 0;
```

### 4.7 Java Entity Mapping

```java
@Data
@Table(name = "bank_transaction")
public class BankTransaction {
    @Column(name = "eyecatcher", nullable = false)
    private String eyecatcher = "PRTR";
    
    @Column(name = "sortcode", nullable = false)
    private String sortcode;
    
    @Column(name = "transaction_number", nullable = false)
    private String transactionNumber;
    
    @Column(name = "transaction_date", nullable = false)
    private LocalDate transactionDate;
    
    @Column(name = "transaction_time", nullable = false)
    private LocalTime transactionTime;
    
    @Column(name = "reference", nullable = false)
    private String reference;
    
    @Column(name = "transaction_type", nullable = false, length = 3)
    private String transactionType;
    
    @Column(name = "description", nullable = false, length = 40)
    private String description;
    
    @Column(name = "amount", nullable = false, precision = 12, scale = 2)
    private BigDecimal amount;
    
    @Column(name = "logically_deleted", nullable = false)
    private Boolean logicallyDeleted = false;
}
```

### 4.8 Acceptance Criteria

- **AC4.1:** All create/delete operations must create PROCTRAN record
- **AC4.2:** Update operations (UPDCUST, UPDACC) must NOT create PROCTRAN record
- **AC4.3:** Transaction type codes must match legacy codes exactly
- **AC4.4:** Description format must match COBOL structure for transfers, creates, deletes
- **AC4.5:** Logical delete flag must be used instead of physical deletion
- **AC4.6:** Amount must use BigDecimal with 2 decimal places
- **AC4.7:** Timestamp must capture date and time separately
- **AC4.8:** All PROCTRAN queries must filter out logically deleted records by default

**Traced to:** PROCTRAN.cpy, CRECUST.cbl, DELCUS.cbl, CREACC.cbl, DELACC.cbl, XFRFUN.cbl, DBCRFUN.cbl

---

## 5. Control Data Structure

### 5.1 COBOL Copybook Definition

**Source:** `CONTROLI.cpy`, `CONTDB2.cpy`  
**Db2 Table:** CONTROL

```cobol
03 CONTROL-DATA.
   05 CONTROL-NAME               PIC X(30).
   05 CONTROL-VALUE-STR          PIC X(30).
   05 CONTROL-VALUE-NUM          PIC 9(18) COMP.
```

### 5.2 Purpose

The CONTROL table stores system-wide configuration and counter values:
- Named Counter values (customer numbers, account numbers)
- System configuration parameters
- Application constants

### 5.3 Field Specifications

| Field Name | COBOL Type | Length | Java Type | Constraints | Description |
|------------|------------|--------|-----------|-------------|-------------|
| control_name | PIC X(30) | 30 | String | PRIMARY KEY | Control parameter name |
| control_value_str | PIC X(30) | 30 | String | NULL | String value (if applicable) |
| control_value_num | PIC 9(18) COMP | 18 | Long | NULL | Numeric value (if applicable) |

### 5.4 Named Counter Records

| Control Name | Purpose | Initial Value |
|--------------|---------|---------------|
| HBNKCUST | Customer number counter | 1 |
| HBNKACC | Account number counter | 1 |

### 5.5 SQLite Schema Definition

```sql
CREATE TABLE IF NOT EXISTS control (
    control_name TEXT NOT NULL PRIMARY KEY,
    control_value_str TEXT,
    control_value_num INTEGER
);

INSERT OR IGNORE INTO control VALUES ('HBNKCUST', NULL, 1);
INSERT OR IGNORE INTO control VALUES ('HBNKACC', NULL, 1);
```

### 5.6 Java Entity Mapping

```java
@Data
@Table(name = "control")
public class Control {
    @Column(name = "control_name", nullable = false)
    private String controlName;
    
    @Column(name = "control_value_str")
    private String controlValueStr;
    
    @Column(name = "control_value_num")
    private Long controlValueNum;
}
```

### 5.7 Acceptance Criteria

- **AC5.1:** Named Counter values must be atomically incremented (thread-safe)
- **AC5.2:** Counter increment must support rollback on transaction failure
- **AC5.3:** Initial counter values must be seeded during database initialization
- **AC5.4:** Control table must support both string and numeric values

**Traced to:** CONTROLI.cpy, CONTDB2.cpy, CRECUST.cbl, CREACC.cbl

---

## 6. Acceptance Criteria Summary

### 6.1 Data Integrity

- **DI1:** All composite primary keys must enforce uniqueness
- **DI2:** All foreign keys must enforce referential integrity
- **DI3:** All eyecatcher fields must be validated with CHECK constraints
- **DI4:** All date fields must validate against business rules (min 1601, max age 150, no future)
- **DI5:** All numeric fields must use appropriate precision (BigDecimal for money)

### 6.2 Data Migration

- **DM1:** All COBOL copybook structures must have exact Java entity equivalents
- **DM2:** All field lengths must be preserved in Java (e.g., name = 60 chars)
- **DM3:** All COBOL PIC clauses must map to appropriate Java types
- **DM4:** All REDEFINES clauses must be handled with separate fields or computed properties
- **DM5:** All 88-level condition names must be replaced with enums or constants

### 6.3 Backward Compatibility

- **BC1:** SQLite schema must preserve COBOL data patterns (eyecatchers, composite keys)
- **BC2:** Transaction type codes must match COBOL exactly (CRE, DEB, TFR, etc.)
- **BC3:** PROCTRAN description formats must match COBOL structures
- **BC4:** Date formats must convert between COBOL YYYYMMDD and Java LocalDate
- **BC5:** Decimal precision must prevent rounding errors (use BigDecimal, not float/double)

---

## 7. Modernization Mappings

### 7.1 COBOL to Java Type Mappings

| COBOL Type | Example | Java Type | Notes |
|------------|---------|-----------|-------|
| PIC X(n) | PIC X(60) | String | Character string |
| PIC 9(n) DISPLAY | PIC 9(10) | String | Numeric string (preserve leading zeros) |
| PIC 9(n) COMP | PIC 9(8) COMP | Integer/Long | Binary integer |
| PIC S9(n)V99 | PIC S9(10)V99 | BigDecimal | Signed decimal with 2 places |
| PIC 9(4)V99 | PIC 9(4)V99 | BigDecimal | Unsigned decimal (e.g., interest rate) |
| PIC 9(8) (date) | PIC 9(8) | LocalDate | Date in YYYYMMDD format |
| PIC 9(6) (time) | PIC 9(6) | LocalTime | Time in HHMMSS format |
| 88 level | 88 VALUE 'X' | enum/constant | Condition name |

### 7.2 VSAM to SQLite Mappings

| VSAM Feature | SQLite Equivalent |
|--------------|-------------------|
| KSDS (Key Sequenced) | PRIMARY KEY constraint |
| Alternate Index | CREATE INDEX statement |
| REDEFINES | Separate columns or computed fields |
| Eye-catcher | CHECK constraint |
| Logical delete | Boolean column with index |

### 7.3 Db2 to SQLite Mappings

| Db2 Feature | SQLite Equivalent |
|-------------|-------------------|
| VARCHAR(n) | TEXT |
| DECIMAL(p,s) | REAL (with application-level precision) |
| INTEGER | INTEGER |
| DATE | TEXT (ISO 8601 format YYYY-MM-DD) |
| TIME | TEXT (HH:MM:SS format) |
| TIMESTAMP | TEXT (ISO 8601 datetime) |

### 7.4 Named Counter Migration

**Legacy CICS:**
```cobol
EXEC CICS ENQ RESOURCE('HBNKCUST') LENGTH(16) END-EXEC.
EXEC CICS GET COUNTER('HBNKCUST') VALUE(WS-COUNTER) INCREMENT(1) END-EXEC.
EXEC CICS DEQ RESOURCE('HBNKCUST') END-EXEC.
```

**Modern Java (Option 1 - Database):**
```java
@Transactional(propagation = Propagation.REQUIRES_NEW)
public Long getNextCustomerNumber() {
    String sql = "UPDATE control SET control_value_num = control_value_num + 1 " +
                 "WHERE control_name = 'HBNKCUST' RETURNING control_value_num";
    return jdbcTemplate.queryForObject(sql, Long.class);
}
```

**Modern Java (Option 2 - Redis):**
```java
public Long getNextCustomerNumber() {
    return redisTemplate.opsForValue().increment("counter:customer", 1);
}
```

### 7.5 Date Format Conversion

**COBOL to Java:**
```java
// COBOL: PIC 9(8) = "20251027"
String cobolDate = "20251027";
LocalDate javaDate = LocalDate.parse(cobolDate, 
    DateTimeFormatter.ofPattern("yyyyMMdd"));
```

**Java to COBOL:**
```java
LocalDate javaDate = LocalDate.now();
String cobolDate = javaDate.format(
    DateTimeFormatter.ofPattern("yyyyMMdd"));
// Result: "20251027"
```

### 7.6 Decimal Precision Handling

**COBOL Interest Rate:**
```cobol
05 ACCOUNT-INTEREST-RATE PIC 9(4)V99.
* Value 450 = 4.50%
```

**Java Conversion:**
```java
// COBOL integer 450 -> Java BigDecimal 4.50
BigDecimal interestRate = new BigDecimal(cobolValue)
    .divide(new BigDecimal("100"), 2, RoundingMode.HALF_UP);

// Java BigDecimal 4.50 -> COBOL integer 450
int cobolValue = interestRate
    .multiply(new BigDecimal("100"))
    .intValue();
```

---

## Appendix A: Complete Schema DDL

```sql
-- Customer Table
CREATE TABLE IF NOT EXISTS customer (
    eyecatcher TEXT NOT NULL CHECK(eyecatcher = 'CUST'),
    sortcode TEXT NOT NULL,
    customer_number TEXT NOT NULL,
    name TEXT NOT NULL,
    address TEXT NOT NULL,
    date_of_birth TEXT NOT NULL,
    credit_score INTEGER NOT NULL CHECK(credit_score >= 0 AND credit_score <= 999),
    cs_review_date TEXT NOT NULL,
    PRIMARY KEY (sortcode, customer_number)
);

CREATE INDEX IF NOT EXISTS idx_customer_number ON customer(customer_number);
CREATE INDEX IF NOT EXISTS idx_customer_name ON customer(name);

-- Account Table
CREATE TABLE IF NOT EXISTS account (
    eyecatcher TEXT NOT NULL CHECK(eyecatcher = 'ACCT'),
    sortcode TEXT NOT NULL,
    account_number TEXT NOT NULL,
    customer_number TEXT NOT NULL,
    account_type TEXT NOT NULL,
    interest_rate REAL NOT NULL,
    opened_date TEXT NOT NULL,
    overdraft_limit INTEGER NOT NULL CHECK(overdraft_limit >= 0),
    last_statement_date TEXT NOT NULL,
    next_statement_date TEXT NOT NULL,
    available_balance REAL NOT NULL,
    actual_balance REAL NOT NULL,
    PRIMARY KEY (sortcode, account_number),
    FOREIGN KEY (sortcode, customer_number) REFERENCES customer(sortcode, customer_number)
);

CREATE INDEX IF NOT EXISTS idx_account_customer ON account(customer_number);
CREATE INDEX IF NOT EXISTS idx_account_type ON account(account_type);

-- Transaction Table (PROCTRAN)
CREATE TABLE IF NOT EXISTS bank_transaction (
    eyecatcher TEXT NOT NULL CHECK(eyecatcher = 'PRTR'),
    sortcode TEXT NOT NULL,
    transaction_number TEXT NOT NULL,
    transaction_date TEXT NOT NULL,
    transaction_time TEXT NOT NULL,
    reference TEXT NOT NULL,
    transaction_type TEXT NOT NULL,
    description TEXT NOT NULL,
    amount REAL NOT NULL,
    logically_deleted INTEGER NOT NULL DEFAULT 0,
    PRIMARY KEY (sortcode, transaction_number)
);

CREATE INDEX IF NOT EXISTS idx_transaction_date ON bank_transaction(transaction_date);
CREATE INDEX IF NOT EXISTS idx_transaction_type ON bank_transaction(transaction_type);
CREATE INDEX IF NOT EXISTS idx_transaction_ref ON bank_transaction(reference);
CREATE INDEX IF NOT EXISTS idx_not_deleted ON bank_transaction(logically_deleted) WHERE logically_deleted = 0;

-- Control Table
CREATE TABLE IF NOT EXISTS control (
    control_name TEXT NOT NULL PRIMARY KEY,
    control_value_str TEXT,
    control_value_num INTEGER
);

INSERT OR IGNORE INTO control VALUES ('HBNKCUST', NULL, 1);
INSERT OR IGNORE INTO control VALUES ('HBNKACC', NULL, 1);
```

---

## Document Control

**Approvals Required:**
- Data Architect: Review schema design and mappings
- Database Administrator: Review SQLite implementation
- Technical Lead: Review Java entity mappings

**Related Documents:**
- 01_Business_Processes.md
- 03_Functional_Requirements.md
- 04_Integration_Points.md
- 05_Non_Functional_Requirements.md
