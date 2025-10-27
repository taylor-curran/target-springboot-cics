# Integration Points Requirements

**Document Version:** 1.0  
**Last Updated:** 2025-10-27  
**Target System:** CBSA Java Migration (Spring Boot)  
**Legacy System:** CICS Banking Sample Application (COBOL)

## Executive Summary

This document defines all integration points for the CBSA modernization effort, covering external systems, APIs, messaging patterns, and inter-service communication. Each integration point includes legacy patterns, modern equivalents, and technical specifications.

## Table of Contents

1. [Integration Architecture Overview](#integration-architecture-overview)
2. [Credit Agency Integration](#credit-agency-integration)
3. [REST API Integration](#rest-api-integration)
4. [Database Integration](#database-integration)
5. [BMS Screen Replacement](#bms-screen-replacement)
6. [System Utility Integration](#system-utility-integration)
7. [Acceptance Criteria](#acceptance-criteria)

---

## 1. Integration Architecture Overview

### 1.1 Legacy Integration Architecture

**CICS Transaction Server:**
- BMS 3270 terminals for user interface
- CICS LINK commands for program-to-program calls
- CICS Named Counter API for sequence generation
- CICS Async API for credit agency calls
- VSAM for customer data persistence
- Db2 for account and transaction data
- z/OS Connect for RESTful API exposure

### 1.2 Modern Integration Architecture

**Spring Boot Microservices:**
- REST APIs replace BMS screens
- Service layer methods replace CICS LINK calls
- Database sequences or Redis replace Named Counters
- Spring @Async with CompletableFuture for async calls
- SQLite for all data persistence (development/test)
- OpenAPI/Swagger for API documentation

### 1.3 Integration Layers

| Layer | Legacy | Modern |
|-------|--------|--------|
| Presentation | BMS 3270 screens | REST APIs (JSON) |
| Application | CICS programs (EXEC CICS LINK) | Spring Services (@Service) |
| Integration | z/OS Connect, CICS Async | Spring @Async, RestTemplate |
| Data | VSAM + Db2 | SQLite (unified) |
| Security | RACF, CICS security | Spring Security |

---

## 2. Credit Agency Integration

### 2.1 Legacy Credit Agency Integration

**Source Programs:** CRDTAGY1.cbl, CRDTAGY2.cbl, CRDTAGY3.cbl, CRDTAGY4.cbl, CRDTAGY5.cbl  
**Called From:** CRECUST.cbl  
**Integration Pattern:** CICS Async API

#### Legacy Implementation

```cobol
* Invoke 5 credit agencies asynchronously
EXEC CICS LINK PROGRAM('CRDTAGY1') 
     COMMAREA(CREDIT-CHECK-REQUEST) 
     CHANNEL('CREDIT') 
     ASYNCHRONOUS
END-EXEC.

* Wait for responses with 3 second timeout
EXEC CICS WAITCICS 
     TIMEOUT(3) 
     CHANNEL('CREDIT')
END-EXEC.

* Aggregate and average credit scores
PERFORM AGGREGATE-CREDIT-SCORES.
```

#### Legacy Behavior

- **5 identical credit agency programs** (CRDTAGY1-5)
- **Async invocation:** All 5 called simultaneously
- **3-second timeout:** Wait maximum 3 seconds for all responses
- **Score aggregation:** Average all received scores
- **No response handling:** If no agency responds within 3 seconds, set credit_score = 0 and cs_review_date = today

### 2.2 Modern Credit Agency Integration

**Target Service:** CreditAgencyService  
**Integration Pattern:** Spring @Async with CompletableFuture

#### Modern Implementation

```java
@Service
public class CreditAgencyService {
    
    @Async("creditCheckExecutor")
    public CompletableFuture<Integer> checkCreditAgency1(String customerInfo) {
        // Simulate credit agency 1 call
        return CompletableFuture.completedFuture(generateCreditScore());
    }
    
    // Similar methods for agencies 2-5
    
    public int aggregateCreditScores(String customerInfo) {
        // Create 5 async tasks
        CompletableFuture<Integer> agency1 = checkCreditAgency1(customerInfo);
        CompletableFuture<Integer> agency2 = checkCreditAgency2(customerInfo);
        CompletableFuture<Integer> agency3 = checkCreditAgency3(customerInfo);
        CompletableFuture<Integer> agency4 = checkCreditAgency4(customerInfo);
        CompletableFuture<Integer> agency5 = checkCreditAgency5(customerInfo);
        
        // Wait for all with 3-second timeout
        CompletableFuture<Void> allFutures = CompletableFuture.allOf(
            agency1, agency2, agency3, agency4, agency5
        );
        
        try {
            allFutures.get(3, TimeUnit.SECONDS);
            
            // Collect and average responses
            List<Integer> scores = new ArrayList<>();
            if (agency1.isDone() && !agency1.isCompletedExceptionally()) {
                scores.add(agency1.get());
            }
            // ... repeat for agencies 2-5
            
            if (scores.isEmpty()) {
                return 0; // No responses received
            }
            
            return (int) scores.stream()
                .mapToInt(Integer::intValue)
                .average()
                .orElse(0);
                
        } catch (TimeoutException e) {
            // Timeout after 3 seconds
            return 0;
        }
    }
}
```

#### Thread Pool Configuration

```java
@Configuration
@EnableAsync
public class AsyncConfig {
    
    @Bean(name = "creditCheckExecutor")
    public Executor creditCheckExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(5);
        executor.setMaxPoolSize(10);
        executor.setQueueCapacity(25);
        executor.setThreadNamePrefix("CreditCheck-");
        executor.initialize();
        return executor;
    }
}
```

### 2.3 Requirements

**INT-CREDIT-001: Async Invocation**
- SHALL invoke all 5 credit agency services asynchronously
- SHALL use separate threads for each agency call
- SHALL not block on individual agency response

**Acceptance Test:** Verify all 5 agency calls execute in parallel, not sequentially

**INT-CREDIT-002: Timeout Handling**
- SHALL wait maximum 3 seconds for all responses
- SHALL proceed after 3 seconds regardless of response count
- SHALL average only responses received within timeout

**Acceptance Test:** Mock one agency with 5-second delay; verify timeout at 3 seconds and score calculated from 4 responses

**INT-CREDIT-003: Score Aggregation**
- SHALL average all received credit scores (integer division)
- SHALL return 0 if no responses received within timeout
- SHALL set cs_review_date = current date if score = 0

**Acceptance Test:** Mock agencies returning scores 600, 650, 700; verify average = 650

**INT-CREDIT-004: Error Handling**
- SHALL handle agency failures gracefully (don't crash)
- SHALL exclude failed agencies from average calculation
- SHALL log agency errors for monitoring

**Acceptance Test:** Mock one agency throwing exception; verify other 4 scores averaged correctly

**INT-CREDIT-005: Response Format**
- SHALL accept credit score as integer (0-999)
- SHALL validate score range before averaging
- SHALL reject scores outside 0-999 range

**Acceptance Test:** Mock agency returning score 1200; verify score rejected and excluded from average

**Traced to:** CRECUST.cbl (lines 200-400), CRDTAGY1.cbl, CRDTAGY2.cbl, CRDTAGY3.cbl, CRDTAGY4.cbl, CRDTAGY5.cbl

---

## 3. REST API Integration

### 3.1 Legacy z/OS Connect Integration

**Architecture:** z/OS Connect EE Server exposes CICS programs as REST APIs

**Components:**
- **z/OS Connect Server:** API gateway layer
- **CICS Service Provider:** Bridges z/OS Connect to CICS programs
- **API Definitions:** OpenAPI specs for CICS program interfaces

**Example REST Call (Legacy):**
```
POST /banking/customers
Content-Type: application/json

{
  "sortcode": "987654",
  "name": "John Smith",
  "address": "123 Main St, London",
  "dateOfBirth": "19850515"
}

Response:
{
  "sortcode": "987654",
  "customerNumber": "0000000123",
  "creditScore": 675,
  "csReviewDate": "20251027"
}
```

### 3.2 Modern Spring Boot REST APIs

**Architecture:** Spring Boot application with embedded Tomcat exposes REST APIs directly

#### API Endpoints

| Endpoint | Method | Legacy Program | Purpose |
|----------|--------|----------------|---------|
| /api/customers | POST | CRECUST | Create customer |
| /api/customers/{sortcode}/{customerNumber} | GET | INQCUST | Get customer |
| /api/customers/{sortcode}/{customerNumber} | PUT | UPDCUST | Update customer |
| /api/customers/{sortcode}/{customerNumber} | DELETE | DELCUS | Delete customer |
| /api/accounts | POST | CREACC | Create account |
| /api/accounts/{sortcode}/{accountNumber} | GET | INQACC | Get account |
| /api/accounts/customer/{customerNumber} | GET | INQACCCU | List customer accounts |
| /api/accounts/{sortcode}/{accountNumber} | PUT | UPDACC | Update account |
| /api/accounts/{sortcode}/{accountNumber} | DELETE | DELACC | Delete account |
| /api/transactions/transfer | POST | XFRFUN | Transfer funds |
| /api/transactions/debit | POST | DBCRFUN | Debit account |
| /api/transactions/credit | POST | DBCRFUN | Credit account |
| /api/utility/company-name | GET | GETCOMPY | Get company info |
| /api/utility/sortcode | GET | GETSCODE | Get sort code |

#### REST API Implementation Example

```java
@RestController
@RequestMapping("/api/customers")
public class CustomerController {
    
    @Autowired
    private CustomerService customerService;
    
    @PostMapping
    public ResponseEntity<CustomerResponse> createCustomer(
            @RequestBody @Valid CustomerCreateRequest request) {
        
        CustomerResponse response = customerService.createCustomer(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
    
    @GetMapping("/{sortcode}/{customerNumber}")
    public ResponseEntity<CustomerResponse> getCustomer(
            @PathVariable String sortcode,
            @PathVariable String customerNumber) {
        
        CustomerResponse response = customerService.getCustomer(sortcode, customerNumber);
        return ResponseEntity.ok(response);
    }
    
    @PutMapping("/{sortcode}/{customerNumber}")
    public ResponseEntity<CustomerResponse> updateCustomer(
            @PathVariable String sortcode,
            @PathVariable String customerNumber,
            @RequestBody @Valid CustomerUpdateRequest request) {
        
        CustomerResponse response = customerService.updateCustomer(sortcode, customerNumber, request);
        return ResponseEntity.ok(response);
    }
    
    @DeleteMapping("/{sortcode}/{customerNumber}")
    public ResponseEntity<Void> deleteCustomer(
            @PathVariable String sortcode,
            @PathVariable String customerNumber) {
        
        customerService.deleteCustomer(sortcode, customerNumber);
        return ResponseEntity.noContent().build();
    }
}
```

### 3.3 Requirements

**INT-API-001: RESTful Design**
- SHALL follow REST principles (resource-based URLs, HTTP verbs)
- SHALL use appropriate HTTP status codes (200, 201, 400, 404, 500)
- SHALL use JSON for request/response bodies
- SHALL provide meaningful error messages

**Acceptance Test:** Verify all endpoints follow REST conventions and return correct status codes

**INT-API-002: Request Validation**
- SHALL validate all input parameters (path, query, body)
- SHALL return 400 Bad Request for invalid input
- SHALL provide validation error details in response

**Acceptance Test:** Submit invalid request (e.g., missing required field); verify 400 status with error details

**INT-API-003: Response Format**
- SHALL return JSON responses with consistent structure
- SHALL include all relevant data fields from COBOL response
- SHALL use camelCase for JSON property names
- SHALL format dates as ISO 8601 (YYYY-MM-DD)

**Acceptance Test:** Compare JSON response structure with COBOL COMMAREA; verify all fields mapped

**INT-API-004: Error Handling**
- SHALL return 404 Not Found when resource doesn't exist
- SHALL return 409 Conflict for business rule violations
- SHALL return 500 Internal Server Error for system failures
- SHALL NOT expose internal error details to clients

**Acceptance Test:** Trigger various error scenarios; verify correct HTTP status codes returned

**INT-API-005: API Documentation**
- SHALL provide OpenAPI/Swagger documentation for all endpoints
- SHALL include request/response examples
- SHALL document all error codes and messages

**Acceptance Test:** Access /swagger-ui/index.html; verify all endpoints documented

**INT-API-006: Transaction Context**
- SHALL distinguish between branch-initiated and web-initiated transactions
- SHALL use different PROCTRAN type codes (OCC vs ICC, OCA vs ICA, etc.)
- SHALL accept source indicator in request or header

**Acceptance Test:** Create customer via API with source='WEB'; verify PROCTRAN type = 'ICC'

**Traced to:** All COBOL programs, z/OS Connect configuration

---

## 4. Database Integration

### 4.1 Legacy Database Architecture

**Data Stores:**
- **VSAM KSDS:** CUSTOMER file (key-sequenced data set)
- **Db2 Tables:** ACCOUNT, PROCTRAN, CONTROL
- **Named Counters:** CICS Named Counter API for HBNKCUST, HBNKACC

**Access Patterns:**
- **VSAM:** Sequential and random access using composite keys
- **Db2:** SQL queries with cursors for pagination
- **Named Counters:** ENQ/DEQUEUE with atomic increment

### 4.2 Modern Database Architecture

**Data Store:** SQLite (unified database)

**Tables:**
- customer (replaces VSAM CUSTOMER)
- account (replaces Db2 ACCOUNT)
- bank_transaction (replaces Db2 PROCTRAN)
- control (replaces Db2 CONTROL and Named Counters)

**Access Patterns:**
- Spring Data JDBC repositories
- Composite key support via @Id annotations
- Pagination via Spring Pageable
- Atomic counter increments via SQL UPDATE...RETURNING

### 4.3 Requirements

**INT-DB-001: Connection Management**
- SHALL use connection pooling for efficiency
- SHALL configure appropriate pool size (min 5, max 20)
- SHALL handle connection failures gracefully
- SHALL implement connection health checks

**Acceptance Test:** Execute 100 concurrent database operations; verify connection pool manages connections efficiently

**INT-DB-002: Transaction Management**
- SHALL use Spring @Transactional for declarative transactions
- SHALL configure appropriate isolation level (READ_COMMITTED)
- SHALL implement rollback on any exception
- SHALL use REQUIRES_NEW propagation for Named Counter operations

**Acceptance Test:** Execute operation that fails mid-transaction; verify all changes rolled back

**INT-DB-003: Named Counter Implementation**
- SHALL implement atomic counter increment using database
- SHALL support rollback on transaction failure
- SHALL prevent duplicate counter values
- SHALL use SELECT FOR UPDATE or UPDATE...RETURNING

**Acceptance Test:** Increment counter 1000 times concurrently; verify all values unique and sequential

**INT-DB-004: Composite Key Support**
- SHALL support composite primary keys (sortcode + customer_number, sortcode + account_number)
- SHALL implement composite key classes with @Embeddable
- SHALL query using full composite key

**Acceptance Test:** Query customer using composite key; verify correct record returned

**INT-DB-005: Cursor/Pagination Support**
- SHALL implement pagination for large result sets
- SHALL use Spring Pageable interface
- SHALL support page size and page number parameters
- SHALL return total count and page metadata

**Acceptance Test:** Query customer with 25 accounts using page size 10; verify 3 pages returned with correct counts

**INT-DB-006: Schema Migration**
- SHALL preserve COBOL data patterns (eyecatchers, composite keys)
- SHALL enforce CHECK constraints for data validation
- SHALL maintain referential integrity with foreign keys
- SHALL create indexes for performance

**Acceptance Test:** Verify schema matches COBOL copybook structures; verify constraints enforced

**Traced to:** All COBOL programs, VSAM file definitions, Db2 table definitions

---

## 5. BMS Screen Replacement

### 5.1 Legacy BMS Screens

**Screen Programs (9 total):**
- BNKMENU - Main menu
- BNK1TFN - Transfer funds screen
- BNK1CCS - Customer services screen
- BNK1CCA - List customer accounts screen
- BNK1CAC - Create account screen
- BNK1UAC - Update account screen
- BNK1CRA - Create account alternate screen
- BNK1DCS - Delete customer screen
- BNK1DAC - Delete account screen

**Integration Pattern:** 3270 terminal emulation, BMS mapsets, CICS SEND MAP/RECEIVE MAP

### 5.2 Modern REST API Replacement

**Strategy:** Replace all BMS screens with REST APIs

| BMS Screen | Replaced By | HTTP Method | Endpoint |
|------------|-------------|-------------|----------|
| BNKMENU | Status API | GET | /api/status |
| BNK1TFN | Transfer API | POST | /api/transactions/transfer |
| BNK1CCS | Customer CRUD | POST/GET/PUT/DELETE | /api/customers/* |
| BNK1CCA | Account List | GET | /api/accounts/customer/{customerNumber} |
| BNK1CAC | Account Create | POST | /api/accounts |
| BNK1UAC | Account Update | PUT | /api/accounts/{sortcode}/{accountNumber} |
| BNK1CRA | Account Create Alt | POST | /api/accounts |
| BNK1DCS | Customer Delete | DELETE | /api/customers/{sortcode}/{customerNumber} |
| BNK1DAC | Account Delete | DELETE | /api/accounts/{sortcode}/{accountNumber} |

### 5.3 Requirements

**INT-BMS-001: Screen Function Mapping**
- SHALL provide equivalent functionality for all 9 BMS screens via REST APIs
- SHALL maintain same business logic and validation rules
- SHALL support both branch and web transaction sources

**Acceptance Test:** Execute each BMS screen function via REST API; verify identical results

**INT-BMS-002: User Experience**
- SHALL provide OpenAPI/Swagger UI as development interface
- SHALL support web frontend (Carbon React UI or similar) for production
- SHALL maintain same workflow patterns as BMS screens

**Acceptance Test:** Compare BMS screen workflow with REST API workflow; verify equivalent user experience

**INT-BMS-003: Data Format Conversion**
- SHALL convert between COBOL COMMAREA format and JSON
- SHALL preserve all data fields from BMS screens
- SHALL use appropriate JSON data types (string, number, boolean)

**Acceptance Test:** Compare BMS COMMAREA structure with JSON request/response; verify all fields mapped

**Traced to:** BNKMENU.cbl, BNK1TFN.cbl, BNK1CCS.cbl, BNK1CCA.cbl, BNK1CAC.cbl, BNK1UAC.cbl, BNK1CRA.cbl, BNK1DCS.cbl, BNK1DAC.cbl

---

## 6. System Utility Integration

### 6.1 Company Information Service

**Legacy Program:** GETCOMPY.cbl  
**Modern Service:** CompanyInfoService

**Integration Pattern:** Direct service invocation (no external integration)

#### Requirements

**INT-UTIL-001: Company Name**
- SHALL return constant: "CICS Bank Sample Application"
- SHALL be callable from any service layer
- SHALL not require external dependencies

**Acceptance Test:** Call service from multiple contexts; verify consistent response

### 6.2 Sort Code Service

**Legacy Program:** GETSCODE.cbl  
**Modern Service:** SortCodeService

**Integration Pattern:** Direct service invocation or configuration property

#### Requirements

**INT-UTIL-002: Sort Code**
- SHALL return constant: "987654"
- SHALL support configuration override via application.properties
- SHALL validate 6-digit format

**Acceptance Test:** Verify sort code returned matches configuration

### 6.3 Error Logging Service

**Legacy Program:** ABNDPROC.cbl  
**Modern Service:** ErrorLoggingService

**Integration Pattern:** Spring AOP interceptor or exception handler

#### Requirements

**INT-UTIL-003: Error Context**
- SHALL capture program/service name, timestamp, error code, message
- SHALL log to database and/or centralized logging system
- SHALL provide correlation IDs for request tracing

**Acceptance Test:** Trigger error; verify full context logged

---

## 7. Acceptance Criteria

### 7.1 Integration Acceptance

**IA1:** All credit agency calls SHALL complete asynchronously within 3-second timeout

**IA2:** All REST APIs SHALL follow RESTful conventions and return appropriate HTTP status codes

**IA3:** All database operations SHALL use connection pooling and transaction management

**IA4:** All BMS screen functions SHALL have equivalent REST API endpoints

**IA5:** All Named Counter operations SHALL be thread-safe and support rollback

### 7.2 Performance Acceptance

**PA1:** Credit agency aggregation SHALL complete within 3.5 seconds (including processing)

**PA2:** REST API response times SHALL meet requirements specified in non-functional requirements document

**PA3:** Database connection pool SHALL handle 100 concurrent operations without exhaustion

**PA4:** Named Counter increment SHALL support 1000+ concurrent operations without duplicate values

### 7.3 Compatibility Acceptance

**CA1:** REST API JSON format SHALL be compatible with legacy COBOL COMMAREA structure

**CA2:** Transaction type codes SHALL match legacy codes exactly (OCC, ICC, TFR, etc.)

**CA3:** Date formats SHALL convert correctly between COBOL YYYYMMDD and ISO 8601

**CA4:** Decimal precision SHALL be maintained for all monetary values (BigDecimal)

### 7.4 Testing Acceptance

**TA1:** Credit agency timeout handling SHALL be verified with mock delays

**TA2:** REST API validation SHALL be tested with invalid inputs

**TA3:** Database transaction rollback SHALL be verified with intentional failures

**TA4:** BMS screen equivalence SHALL be verified by comparing results

---

## Appendix A: Integration Endpoint Reference

### Credit Agency Endpoints (Internal)

| Service Method | Timeout | Return Type | Purpose |
|----------------|---------|-------------|---------|
| checkCreditAgency1() | 3s | CompletableFuture<Integer> | Agency 1 credit check |
| checkCreditAgency2() | 3s | CompletableFuture<Integer> | Agency 2 credit check |
| checkCreditAgency3() | 3s | CompletableFuture<Integer> | Agency 3 credit check |
| checkCreditAgency4() | 3s | CompletableFuture<Integer> | Agency 4 credit check |
| checkCreditAgency5() | 3s | CompletableFuture<Integer> | Agency 5 credit check |
| aggregateCreditScores() | 3s | int | Average all responses |

### REST API Endpoints (External)

| Endpoint | Method | Request Body | Response Body | Status Codes |
|----------|--------|--------------|---------------|--------------|
| /api/customers | POST | CustomerCreateRequest | CustomerResponse | 201, 400, 500 |
| /api/customers/{sortcode}/{customerNumber} | GET | - | CustomerResponse | 200, 404, 500 |
| /api/customers/{sortcode}/{customerNumber} | PUT | CustomerUpdateRequest | CustomerResponse | 200, 400, 404, 500 |
| /api/customers/{sortcode}/{customerNumber} | DELETE | - | - | 204, 404, 500 |
| /api/accounts | POST | AccountCreateRequest | AccountResponse | 201, 400, 500 |
| /api/accounts/{sortcode}/{accountNumber} | GET | - | AccountResponse | 200, 404, 500 |
| /api/accounts/customer/{customerNumber} | GET | - | AccountListResponse | 200, 500 |
| /api/accounts/{sortcode}/{accountNumber} | PUT | AccountUpdateRequest | AccountResponse | 200, 400, 404, 500 |
| /api/accounts/{sortcode}/{accountNumber} | DELETE | - | - | 204, 404, 500 |
| /api/transactions/transfer | POST | TransferRequest | TransferResponse | 200, 400, 409, 500 |
| /api/transactions/debit | POST | DebitRequest | TransactionResponse | 200, 400, 409, 500 |
| /api/transactions/credit | POST | CreditRequest | TransactionResponse | 200, 400, 500 |

### Database Integration (Internal)

| Repository Method | SQL Operation | Transaction | Purpose |
|-------------------|---------------|-------------|---------|
| customerRepository.save() | INSERT/UPDATE | Required | Create/update customer |
| customerRepository.findByKey() | SELECT | ReadOnly | Query customer |
| accountRepository.save() | INSERT/UPDATE | Required | Create/update account |
| accountRepository.findByKey() | SELECT | ReadOnly | Query account |
| accountRepository.findByCustomerNumber() | SELECT | ReadOnly | List customer accounts |
| transactionRepository.save() | INSERT | Required | Create PROCTRAN record |
| controlRepository.incrementCounter() | UPDATE...RETURNING | RequiresNew | Increment Named Counter |

---

## Appendix B: Integration Sequence Diagrams

### Customer Creation with Credit Check

```
Client -> REST API: POST /api/customers
REST API -> CustomerService: createCustomer()
CustomerService -> CreditAgencyService: aggregateCreditScores()
CreditAgencyService -> [5 Agencies]: async credit checks
CreditAgencyService <- [5 Agencies]: credit scores (within 3s)
CreditAgencyService -> CreditAgencyService: average scores
CustomerService <- CreditAgencyService: aggregated score
CustomerService -> CounterService: getNextCustomerNumber()
CounterService -> Database: UPDATE control SET value=value+1 RETURNING value
CounterService <- Database: new customer number
CustomerService <- CounterService: customer number
CustomerService -> CustomerRepository: save(customer)
CustomerRepository -> Database: INSERT INTO customer
CustomerService -> TransactionRepository: save(PROCTRAN)
TransactionRepository -> Database: INSERT INTO bank_transaction
REST API <- CustomerService: CustomerResponse
Client <- REST API: 201 Created + customer details
```

### Fund Transfer

```
Client -> REST API: POST /api/transactions/transfer
REST API -> TransactionService: transferFunds()
TransactionService -> Database: BEGIN TRANSACTION
TransactionService -> AccountRepository: findByKey(source)
TransactionService -> AccountRepository: findByKey(target)
TransactionService -> TransactionService: validate balances
TransactionService -> AccountRepository: updateBalance(source, -amount)
TransactionService -> AccountRepository: updateBalance(target, +amount)
TransactionService -> TransactionRepository: save(PROCTRAN)
TransactionService -> Database: COMMIT TRANSACTION
REST API <- TransactionService: TransferResponse
Client <- REST API: 200 OK + transfer confirmation

[On any failure]
TransactionService -> Database: ROLLBACK TRANSACTION
```

---

## Document Control

**Approvals Required:**
- Integration Architect: Review integration patterns and APIs
- Technical Lead: Review service boundaries and dependencies
- API Designer: Review REST API design and documentation

**Related Documents:**
- 01_Business_Processes.md
- 02_Data_Structures.md
- 03_Functional_Requirements.md
- 05_Non_Functional_Requirements.md
