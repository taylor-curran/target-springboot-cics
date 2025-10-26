# COBOL Program Performance Baseline

**Created:** October 26, 2024  
**Environment:** Sandbox (Code Analysis Based)  
**Programs Analyzed:** 29 COBOL programs  
**Methodology:** Synthetic baseline using code complexity analysis

---

## Executive Summary

This document establishes performance baselines for all 29 COBOL programs in the CICS Banking Sample Application. **Critical Note:** These baselines are synthetic estimates based on code complexity analysis, as no live CICS environment exists for actual performance testing. This is a sandbox migration project with only source code available.

### Key Findings
- **Program Count:** 29 total COBOL programs
  - 2 migrated (GETCOMPY, GETSCODE)
  - 9 BMS/UI programs (replaced by REST APIs)
  - 5 Credit check dummy programs (CRDTAGY1-5)
  - 13 Business logic programs requiring migration
- **Complexity Range:** 43 to 2,053 lines of code
- **Estimated Latency Range:** P50: 5-100ms, P95: 15-250ms, P99: 30-500ms
- **Request Volume Estimates:** Based on program type and typical banking operations

---

## Methodology

### Baseline Approach
Since this is a sandbox migration environment without a running CICS system, performance baselines are estimated using:

1. **Static Code Analysis**
   - Lines of code (LOC) as complexity indicator
   - Database operation counts (SQL/VSAM)
   - CICS command counts (LINK, READ, WRITE, etc.)
   - BMS screen interactions

2. **Complexity Tiers**
   - **Simple** (<300 LOC, 0-1 DB ops): Utility/lookup programs
   - **Medium** (300-1000 LOC, 1-3 DB ops): CRUD operations
   - **Complex** (>1000 LOC, 3+ DB ops): Multi-step transactions, UI programs

3. **Latency Estimation Model**
   - Base latency by complexity tier
   - Add 10ms per database operation
   - Add 5ms per CICS LINK command
   - Add 50ms for BMS screen rendering (UI programs)
   - Scale to P50/P95/P99 using standard distribution (P95=2.5x P50, P99=6x P50)

4. **Request Volume Estimation**
   - UI programs: Low (10-50 req/min) - user-driven
   - API/Business logic: Medium (100-500 req/min) - application-driven
   - Utility programs: High (1000+ req/min) - called frequently

### Limitations
- **No actual measurements:** Estimates based on code analysis only
- **No network latency:** Sandbox environment assumptions
- **No contention modeling:** Assumes no resource conflicts
- **Simplified database timing:** Actual DB performance varies
- **No real user behavior:** Volume estimates are theoretical

### Future Validation
When a live CICS environment becomes available or Spring Boot implementation is deployed:
1. Capture actual P50/P95/P99 latencies using APM tools
2. Monitor real request volumes and patterns
3. Compare actual vs. estimated baseline
4. Update this document with real measurements

---

## Program Categorization

### Migrated Programs (2)
| Program | LOC | Type | Status |
|---------|-----|------|--------|
| GETCOMPY | 43 | Utility | ✅ Migrated to CompanyInfoService |
| GETSCODE | 46 | Utility | ✅ Migrated to SortCodeService |

### BMS/UI Programs (9) - Replaced by REST APIs
| Program | LOC | Description | Replacement |
|---------|-----|-------------|-------------|
| BNKMENU | 1,311 | Main Menu | REST API menu endpoints |
| BNK1CAC | 1,298 | Create Account UI | POST /api/accounts |
| BNK1CCA | 952 | Customer Lookup UI | GET /api/customers |
| BNK1CCS | 1,657 | Create Customer UI | POST /api/customers |
| BNK1CRA | 1,166 | Credit/Debit UI | POST /api/transactions |
| BNK1DAC | 1,158 | Display Account UI | GET /api/accounts/{id} |
| BNK1DCS | 2,053 | Display Customer UI | GET /api/customers/{id} |
| BNK1TFN | 1,224 | Transfer Funds UI | POST /api/transfers |
| BNK1UAC | 1,405 | Update Account UI | PUT /api/accounts/{id} |

### Credit Check Programs (5) - Dummy/Simulation
| Program | LOC | Description | Complexity |
|---------|-----|-------------|------------|
| CRDTAGY1 | 273 | Credit Check #1 | Simple (Dummy) |
| CRDTAGY2 | 273 | Credit Check #2 | Simple (Dummy) |
| CRDTAGY3 | 272 | Credit Check #3 | Simple (Dummy) |
| CRDTAGY4 | 275 | Credit Check #4 | Simple (Dummy) |
| CRDTAGY5 | 275 | Credit Check #5 | Simple (Dummy) |

### Business Logic Programs (13) - Requiring Migration
| Program | LOC | Description | Complexity | Priority |
|---------|-----|-------------|------------|----------|
| ABNDPROC | 176 | Abend Handler | Simple | High |
| UPDCUST | 364 | Update Customer | Medium | High |
| UPDACC | 406 | Update Account | Medium | High |
| DELACC | 649 | Delete Account | Medium | Medium |
| INQCUST | 711 | Inquire Customer | Medium | High |
| DELCUS | 761 | Delete Customer | Medium | Medium |
| DBCRFUN | 861 | Debit/Credit Function | Medium | High |
| INQACCCU | 882 | Inquire Accounts for Customer | Medium | High |
| INQACC | 1,002 | Inquire Account | Complex | High |
| CREACC | 1,247 | Create Account | Complex | High |
| CRECUST | 1,439 | Create Customer | Complex | High |
| BANKDATA | 1,463 | Data Generation | Complex | Low |
| XFRFUN | 1,924 | Transfer Function | Complex | High |

---

## Performance Baseline Data

### Simple Programs (<300 LOC)
**Characteristics:** Utility lookups, dummy programs, minimal processing  
**Estimated Latency:** P50: 5ms | P95: 15ms | P99: 30ms  
**Request Volume:** High (1000+ req/min for utilities, Low for dummies)

| Program | P50 (ms) | P95 (ms) | P99 (ms) | Est. Volume (req/min) | Data Size (bytes) |
|---------|----------|----------|----------|-----------------------|-------------------|
| GETCOMPY | 5 | 10 | 20 | 2000 | 256 (company name) |
| GETSCODE | 5 | 10 | 20 | 2000 | 128 (sort code) |
| ABNDPROC | 10 | 25 | 50 | 10 | 681 (abend record) |
| CRDTAGY1 | 8 | 20 | 40 | 50 | 512 (credit score) |
| CRDTAGY2 | 8 | 20 | 40 | 50 | 512 (credit score) |
| CRDTAGY3 | 8 | 20 | 40 | 50 | 512 (credit score) |
| CRDTAGY4 | 8 | 20 | 40 | 50 | 512 (credit score) |
| CRDTAGY5 | 8 | 20 | 40 | 50 | 512 (credit score) |

### Medium Programs (300-1000 LOC)
**Characteristics:** CRUD operations, 1-3 database interactions  
**Estimated Latency:** P50: 20ms | P95: 50ms | P99: 100ms  
**Request Volume:** Medium (100-500 req/min)

| Program | P50 (ms) | P95 (ms) | P99 (ms) | Est. Volume (req/min) | Data Size (bytes) |
|---------|----------|----------|----------|-----------------------|-------------------|
| UPDCUST | 25 | 60 | 120 | 200 | 259 (customer record) |
| UPDACC | 25 | 60 | 120 | 300 | 200 (account record) |
| DELACC | 30 | 75 | 150 | 100 | 200 (account record) |
| INQCUST | 20 | 50 | 100 | 500 | 259 (customer record) |
| DELCUS | 30 | 75 | 150 | 50 | 259 (customer record) |
| DBCRFUN | 35 | 85 | 170 | 400 | 300 (transaction) |
| INQACCCU | 35 | 85 | 170 | 400 | 2000 (multi-account) |
| BNK1CCA | 30 | 75 | 150 | 100 | 259 (customer + UI) |

### Complex Programs (>1000 LOC)
**Characteristics:** Multi-step transactions, 3+ DB operations, complex logic  
**Estimated Latency:** P50: 50ms | P95: 150ms | P99: 300ms  
**Request Volume:** Medium (100-500 req/min for business logic, Low for UI)

| Program | P50 (ms) | P95 (ms) | P99 (ms) | Est. Volume (req/min) | Data Size (bytes) |
|---------|----------|----------|----------|-----------------------|-------------------|
| INQACC | 45 | 110 | 220 | 500 | 200 (account record) |
| CREACC | 60 | 150 | 300 | 200 | 200 (account record) |
| CRECUST | 60 | 150 | 300 | 150 | 259 (customer record) |
| BANKDATA | 80 | 200 | 400 | 10 | 10000 (bulk data) |
| XFRFUN | 70 | 175 | 350 | 300 | 400 (2 accounts + txn) |

### BMS/UI Programs (Complex with screen rendering)
**Characteristics:** User interface programs with screen I/O  
**Estimated Latency:** P50: 100ms | P95: 250ms | P99: 500ms  
**Request Volume:** Low (10-50 req/min, user-driven)

| Program | P50 (ms) | P95 (ms) | P99 (ms) | Est. Volume (req/min) | Data Size (bytes) |
|---------|----------|----------|----------|-----------------------|-------------------|
| BNKMENU | 100 | 250 | 500 | 50 | 1024 (menu screen) |
| BNK1CAC | 120 | 300 | 600 | 20 | 1280 (create form) |
| BNK1CCS | 120 | 300 | 600 | 15 | 1536 (create form) |
| BNK1CRA | 110 | 275 | 550 | 30 | 1152 (txn form) |
| BNK1DAC | 100 | 250 | 500 | 40 | 1280 (display) |
| BNK1DCS | 110 | 275 | 550 | 35 | 1536 (display) |
| BNK1TFN | 120 | 300 | 600 | 25 | 1280 (transfer form) |
| BNK1UAC | 110 | 275 | 550 | 20 | 1280 (update form) |

---

## Test Data Specifications

Based on configured test data generation in `src/main/resources/application.properties`:

- **Customers:** 100 (configurable: `data.generation.customer.end=100`)
- **Accounts per Customer:** 1-5 (random distribution)
- **Transaction History:** 30 days (`data.generation.transaction.days=30`)
- **Seed:** 12345 (deterministic generation: `data.generation.seed=12345`)

### Typical Data Volumes
- **Customer Records:** ~100 active
- **Account Records:** ~250-300 (avg 2.5 per customer)
- **Daily Transactions:** ~500-1000
- **Total Transaction History:** ~15,000-30,000 records

### Record Sizes
- **Customer:** 259 bytes (from CUSTOMER copybook)
- **Account:** ~200 bytes (from ACCOUNT copybook)
- **Transaction:** ~100 bytes (from PROCTRAN copybook)
- **Control:** 40 bytes (system counters)

---

## Migration Recommendations

### Performance-Critical Programs
Focus migration effort on high-volume programs:
1. **INQACC** (500 req/min) - Account inquiries
2. **INQCUST** (500 req/min) - Customer inquiries
3. **DBCRFUN** (400 req/min) - Debit/Credit operations
4. **INQACCCU** (400 req/min) - Customer account listing
5. **XFRFUN** (300 req/min) - Fund transfers

### Testing Strategy
1. **Load Testing:** Use JMeter/Gatling with synthetic baseline as target
2. **Comparison:** Measure Spring Boot performance against baseline
3. **Acceptance Criteria:** Within 20% of estimated latencies
4. **Regression:** Monitor for performance degradation

### Monitoring Requirements
- APM tool (e.g., New Relic, DataDog) for latency tracking
- Database query performance monitoring
- Request rate and volume tracking
- Error rate monitoring

---

## Appendix: Raw Data

See `docs/performance_baseline.json` for machine-readable baseline data.
