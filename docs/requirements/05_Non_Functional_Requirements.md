# Non-Functional Requirements

**Document Version:** 1.0  
**Last Updated:** 2025-10-27  
**Target System:** CBSA Java Migration (Spring Boot)  
**Legacy System:** CICS Banking Sample Application (COBOL)

## Executive Summary

This document defines all non-functional requirements (NFRs) for the CBSA modernization effort, covering performance, scalability, security, reliability, maintainability, and operational characteristics. These requirements ensure the Java implementation meets enterprise-grade standards while maintaining or improving upon legacy system characteristics.

## Table of Contents

1. [Performance Requirements](#performance-requirements)
2. [Scalability Requirements](#scalability-requirements)
3. [Security Requirements](#security-requirements)
4. [Reliability & Availability](#reliability--availability)
5. [Transaction Management](#transaction-management)
6. [Audit & Compliance](#audit--compliance)
7. [Maintainability Requirements](#maintainability-requirements)
8. [Operational Requirements](#operational-requirements)
9. [Acceptance Criteria](#acceptance-criteria)

---

## 1. Performance Requirements

### 1.1 Response Time Requirements

**NFR-PERF-001: Read Operations**
- SHALL complete customer inquiry (INQCUST) in < 100ms (95th percentile)
- SHALL complete account inquiry (INQACC) in < 100ms (95th percentile)
- SHALL complete account list inquiry (INQACCCU) in < 150ms (95th percentile) for up to 50 accounts
- SHALL complete utility operations (GETCOMPY, GETSCODE) in < 10ms (95th percentile)

**Baseline:** Legacy CICS read operations: 50-80ms average  
**Target:** Within 20% of legacy baseline after optimization

**Acceptance Test:** Execute 1000 read operations under normal load; verify 95th percentile meets requirements

**Traced to:** INQCUST.cbl, INQACC.cbl, INQACCCU.cbl, GETCOMPY.cbl, GETSCODE.cbl

---

**NFR-PERF-002: Write Operations**
- SHALL complete customer creation (CRECUST) in < 5000ms (95th percentile) including 3-second credit check timeout
- SHALL complete account creation (CREACC) in < 300ms (95th percentile)
- SHALL complete customer update (UPDCUST) in < 200ms (95th percentile)
- SHALL complete account update (UPDACC) in < 200ms (95th percentile)
- SHALL complete debit/credit (DBCRFUN) in < 200ms (95th percentile)

**Baseline:** Legacy CICS write operations: 100-200ms average  
**Target:** Within 20% of legacy baseline

**Acceptance Test:** Execute 1000 write operations under normal load; verify 95th percentile meets requirements

**Traced to:** CRECUST.cbl, CREACC.cbl, UPDCUST.cbl, UPDACC.cbl, DBCRFUN.cbl

---

**NFR-PERF-003: Complex Operations**
- SHALL complete fund transfer (XFRFUN) in < 500ms (95th percentile)
- SHALL complete customer deletion with cascade (DELCUS) in < 1000ms (95th percentile) for up to 50 accounts
- SHALL complete account deletion (DELACC) in < 250ms (95th percentile)

**Baseline:** Legacy CICS complex operations: 200-400ms average  
**Target:** Within 20% of legacy baseline

**Acceptance Test:** Execute 1000 complex operations under normal load; verify 95th percentile meets requirements

**Traced to:** XFRFUN.cbl, DELCUS.cbl, DELACC.cbl

---

### 1.2 Throughput Requirements

**NFR-PERF-004: Transaction Throughput**
- SHALL support minimum 100 transactions per second (TPS) sustained load
- SHALL support peak 200 TPS for 5-minute bursts
- SHALL maintain response time requirements under sustained load

**Baseline:** Legacy CICS: 80-120 TPS average  
**Target:** Match or exceed legacy throughput

**Acceptance Test:** Execute load test with 100 TPS sustained for 30 minutes; verify response times maintained

---

**NFR-PERF-005: Concurrent Users**
- SHALL support minimum 50 concurrent users
- SHALL support peak 100 concurrent users
- SHALL not degrade performance below requirements under concurrent load

**Acceptance Test:** Simulate 50 concurrent users performing mixed operations for 30 minutes; verify performance maintained

---

### 1.3 Database Performance

**NFR-PERF-006: Query Performance**
- SHALL use appropriate indexes for all frequent query patterns
- SHALL optimize queries using composite keys efficiently
- SHALL implement pagination for large result sets (> 100 records)
- SHALL use connection pooling with minimum 5, maximum 20 connections

**Acceptance Test:** Analyze query execution plans; verify all queries use indexes. Execute paginated query for 1000 records; verify reasonable performance

---

**NFR-PERF-007: Named Counter Performance**
- SHALL support atomic counter increment in < 50ms (95th percentile)
- SHALL support 1000+ concurrent counter increments without duplicates
- SHALL not create bottleneck for customer/account creation

**Acceptance Test:** Execute 1000 concurrent counter increments; verify all unique and 95th percentile < 50ms

---

### 1.4 Credit Agency Integration Performance

**NFR-PERF-008: Async Credit Checks**
- SHALL invoke all 5 credit agencies in parallel (not sequential)
- SHALL timeout after exactly 3 seconds
- SHALL complete aggregation within 100ms after responses received
- SHALL not block other operations during credit checks

**Acceptance Test:** Mock agencies with 2-second delays; verify all invoked in parallel and aggregation completes in ~2.1 seconds total

**Traced to:** CRECUST.cbl, CRDTAGY1-5.cbl

---

## 2. Scalability Requirements

### 2.1 Vertical Scalability

**NFR-SCALE-001: CPU Utilization**
- SHALL utilize available CPU cores efficiently via thread pools
- SHALL maintain < 70% CPU utilization under sustained load
- SHALL support increasing CPU allocation for higher throughput

**Acceptance Test:** Monitor CPU usage during load test; verify < 70% utilization at 100 TPS

---

**NFR-SCALE-002: Memory Utilization**
- SHALL maintain stable memory usage (no memory leaks)
- SHALL use < 2GB heap memory for 100 concurrent users
- SHALL implement efficient object pooling where appropriate

**Acceptance Test:** Run 8-hour load test; verify memory usage stable (no upward trend)

---

### 2.2 Horizontal Scalability

**NFR-SCALE-003: Stateless Design**
- SHALL implement stateless services (no in-memory session state)
- SHALL support deployment of multiple instances behind load balancer
- SHALL share state only via database

**Acceptance Test:** Deploy 2 instances behind load balancer; verify requests distributed and function correctly

---

**NFR-SCALE-004: Database Scalability**
- SHALL support read replicas for query operations (if needed)
- SHALL support database connection pooling across instances
- SHALL handle database connection failures gracefully

**Acceptance Test:** Configure read replica; verify read operations distributed. Simulate database failure; verify graceful handling

---

### 2.3 Data Volume Scalability

**NFR-SCALE-005: Large Datasets**
- SHALL handle 100,000+ customer records efficiently
- SHALL handle 500,000+ account records efficiently
- SHALL handle 10,000,000+ transaction records efficiently
- SHALL implement database archival strategy for old transactions

**Acceptance Test:** Populate database with target record counts; verify query performance maintained

---

## 3. Security Requirements

### 3.1 Authentication & Authorization

**NFR-SEC-001: API Authentication**
- SHALL require authentication for all API endpoints (except health checks)
- SHALL support JWT bearer token authentication
- SHALL validate token signature and expiration
- SHALL return 401 Unauthorized for missing/invalid tokens

**Acceptance Test:** Call API without token; verify 401. Call with expired token; verify 401. Call with valid token; verify 200

---

**NFR-SEC-002: Authorization**
- SHALL implement role-based access control (RBAC)
- SHALL support roles: ADMIN, TELLER, READ_ONLY
- SHALL enforce least privilege principle
- SHALL return 403 Forbidden for insufficient permissions

**Acceptance Test:** Authenticate as READ_ONLY user; attempt DELETE operation; verify 403

---

**NFR-SEC-003: Session Management**
- SHALL use stateless JWT tokens (no server-side sessions)
- SHALL implement token refresh mechanism
- SHALL support token revocation via blacklist (if needed)

**Acceptance Test:** Verify tokens contain no server-side session reference

---

### 3.2 Data Security

**NFR-SEC-004: Data Encryption in Transit**
- SHALL use TLS 1.2 or higher for all external API communication
- SHALL enforce HTTPS only (redirect HTTP to HTTPS)
- SHALL use strong cipher suites (no weak ciphers)

**Acceptance Test:** Attempt HTTP connection; verify redirect to HTTPS. Verify TLS version >= 1.2

---

**NFR-SEC-005: Data Encryption at Rest**
- SHALL encrypt sensitive customer data (name, address, DOB) at rest
- SHALL use AES-256 encryption for sensitive fields
- SHALL manage encryption keys securely (external key management service)

**Acceptance Test:** Examine database file; verify sensitive fields encrypted, not plaintext

---

**NFR-SEC-006: Sensitive Data Handling**
- SHALL not log sensitive customer information (DOB, credit score)
- SHALL mask sensitive data in logs (e.g., DOB -> ****-**-**)
- SHALL not expose sensitive data in error messages

**Acceptance Test:** Review application logs; verify no plaintext sensitive data

---

### 3.3 Input Validation & Injection Prevention

**NFR-SEC-007: Input Validation**
- SHALL validate all input parameters (path, query, body)
- SHALL enforce strict type and format validation
- SHALL reject malformed or suspicious inputs
- SHALL use whitelist validation where possible

**Acceptance Test:** Submit malformed inputs; verify rejection with 400 error

---

**NFR-SEC-008: SQL Injection Prevention**
- SHALL use parameterized queries exclusively (no string concatenation)
- SHALL use Spring Data JDBC prepared statements
- SHALL validate and sanitize all dynamic query parameters

**Acceptance Test:** Attempt SQL injection attack in input fields; verify query not executed

---

**NFR-SEC-009: XSS Prevention**
- SHALL sanitize all output to prevent cross-site scripting
- SHALL use appropriate Content-Type headers
- SHALL implement Content Security Policy (CSP) headers

**Acceptance Test:** Submit XSS payload in input; verify output sanitized and not executed

---

### 3.4 Audit & Logging for Security

**NFR-SEC-010: Security Event Logging**
- SHALL log all authentication attempts (success and failure)
- SHALL log all authorization failures
- SHALL log all data modification operations (via PROCTRAN)
- SHALL include user identity, timestamp, action, and result in logs

**Acceptance Test:** Attempt failed login; verify security event logged with details

---

## 4. Reliability & Availability

### 4.1 Availability Requirements

**NFR-REL-001: System Availability**
- SHALL target 99.5% availability (SLA: 3.65 hours downtime per month)
- SHALL support planned maintenance windows
- SHALL implement health checks for monitoring

**Acceptance Test:** Monitor uptime over 30 days; calculate availability percentage

---

**NFR-REL-002: Health Checks**
- SHALL provide /actuator/health endpoint
- SHALL check database connectivity
- SHALL check external service dependencies
- SHALL return 200 OK if healthy, 503 Service Unavailable if unhealthy

**Acceptance Test:** Call health endpoint; verify returns correct status based on dependencies

---

### 4.2 Error Handling

**NFR-REL-003: Graceful Degradation**
- SHALL handle credit agency timeouts gracefully (continue with score=0)
- SHALL handle database connection failures with retry logic
- SHALL provide meaningful error messages to clients

**Acceptance Test:** Simulate credit agency timeout; verify customer creation continues with score=0

---

**NFR-REL-004: Error Recovery**
- SHALL implement retry logic for transient failures (with exponential backoff)
- SHALL implement circuit breaker pattern for external service calls
- SHALL log all errors with full context for diagnostics

**Acceptance Test:** Simulate transient database error; verify retry attempted and succeeds

---

### 4.3 Data Integrity

**NFR-REL-005: Transaction Integrity**
- SHALL ensure ACID properties for all database transactions
- SHALL implement proper transaction boundaries
- SHALL rollback all changes on any failure within transaction

**Acceptance Test:** Simulate failure mid-transaction; verify all changes rolled back

---

**NFR-REL-006: Data Consistency**
- SHALL prevent duplicate customer/account numbers via Named Counter
- SHALL maintain referential integrity (foreign keys)
- SHALL prevent orphaned records (e.g., accounts without customers)

**Acceptance Test:** Create customer and account; delete customer; verify account also deleted

---

**NFR-REL-007: Backup & Recovery**
- SHALL support database backup and restore procedures
- SHALL implement point-in-time recovery capability
- SHALL test backup/restore procedures regularly

**Acceptance Test:** Perform database backup; restore to new environment; verify data integrity

---

## 5. Transaction Management

### 5.1 ACID Properties

**NFR-TRAN-001: Atomicity**
- SHALL ensure all operations within transaction succeed or all rollback
- SHALL use Spring @Transactional annotation for declarative transactions
- SHALL handle nested transactions appropriately

**Acceptance Test:** Execute transfer operation; simulate failure after first account update; verify both accounts unchanged

**Traced to:** XFRFUN.cbl, DELCUS.cbl, CRECUST.cbl, CREACC.cbl

---

**NFR-TRAN-002: Consistency**
- SHALL maintain database consistency constraints at all times
- SHALL validate business rules before committing
- SHALL prevent invalid state transitions

**Acceptance Test:** Attempt overdraft exceeding limit; verify transaction rejected and database consistent

---

**NFR-TRAN-003: Isolation**
- SHALL use READ_COMMITTED isolation level by default
- SHALL prevent dirty reads, non-repeatable reads
- SHALL use pessimistic locking (SELECT FOR UPDATE) where necessary

**Acceptance Test:** Execute concurrent updates to same account; verify isolation maintained

---

**NFR-TRAN-004: Durability**
- SHALL persist committed transactions to durable storage
- SHALL survive application restarts
- SHALL use database WAL (Write-Ahead Logging) or equivalent

**Acceptance Test:** Commit transaction; kill application; restart; verify transaction persisted

---

### 5.2 Transaction Boundaries

**NFR-TRAN-005: Service Layer Transactions**
- SHALL define transaction boundaries at service layer (not controller or repository)
- SHALL use appropriate propagation levels (REQUIRED, REQUIRES_NEW, etc.)
- SHALL keep transactions as short as possible

**Acceptance Test:** Review service methods; verify @Transactional annotations at appropriate level

---

**NFR-TRAN-006: Named Counter Transactions**
- SHALL use REQUIRES_NEW propagation for Named Counter operations
- SHALL allow counter increment to commit even if parent transaction rolls back
- SHALL decrement counter explicitly on rollback (compensation logic)

**Acceptance Test:** Create customer; simulate failure after counter increment; verify counter incremented but customer not created

**Traced to:** CRECUST.cbl, CREACC.cbl

---

### 5.3 Concurrency Control

**NFR-TRAN-007: Optimistic Locking**
- SHALL use version columns for optimistic locking on updates
- SHALL handle OptimisticLockException gracefully
- SHALL retry on lock conflicts (with exponential backoff)

**Acceptance Test:** Execute concurrent updates to same record; verify one succeeds and one retries

---

**NFR-TRAN-008: Pessimistic Locking**
- SHALL use SELECT FOR UPDATE for critical operations (e.g., balance updates)
- SHALL minimize lock duration
- SHALL handle lock timeout exceptions

**Acceptance Test:** Lock account record; attempt concurrent update; verify second request waits or fails appropriately

---

## 6. Audit & Compliance

### 6.1 Audit Trail Requirements

**NFR-AUDIT-001: PROCTRAN Audit Logging**
- SHALL create PROCTRAN record for all data modifications (create, delete)
- SHALL NOT create PROCTRAN record for updates (UPDCUST, UPDACC) per business rules
- SHALL NOT create PROCTRAN record for read operations
- SHALL capture: transaction type, timestamp, user, amount, description

**Acceptance Test:** Execute each operation type; verify PROCTRAN records created only for specified operations

**Traced to:** PROCTRAN.cpy, all COBOL programs

---

**NFR-AUDIT-002: Audit Data Retention**
- SHALL retain PROCTRAN records for minimum 7 years
- SHALL implement logical delete (not physical delete) for PROCTRAN
- SHALL support querying audit trail by date range, transaction type, user

**Acceptance Test:** Query PROCTRAN records older than 7 years; verify accessible

---

**NFR-AUDIT-003: Audit Integrity**
- SHALL prevent modification or deletion of PROCTRAN records (append-only)
- SHALL protect audit logs from tampering
- SHALL include checksums or signatures for audit records (optional)

**Acceptance Test:** Attempt to update PROCTRAN record; verify operation rejected

---

### 6.2 Compliance Requirements

**NFR-COMP-001: Data Privacy**
- SHALL comply with GDPR requirements for customer data
- SHALL support right to access (data export)
- SHALL support right to erasure (account deletion)
- SHALL support right to rectification (data correction)

**Acceptance Test:** Request customer data export; verify all data provided in standard format

---

**NFR-COMP-002: Financial Regulations**
- SHALL maintain audit trail for all financial transactions
- SHALL support regulatory reporting requirements
- SHALL prevent unauthorized fund transfers

**Acceptance Test:** Generate regulatory report; verify all required data available

---

**NFR-COMP-003: Data Residency**
- SHALL store all customer data in specified geographic region
- SHALL not transfer data outside region without consent
- SHALL document data storage locations

**Acceptance Test:** Verify database server location; verify no cross-border data transfers

---

## 7. Maintainability Requirements

### 7.1 Code Quality

**NFR-MAINT-001: Code Coverage**
- SHALL maintain minimum 80% unit test coverage for service layer
- SHALL maintain minimum 70% integration test coverage for repository layer
- SHALL maintain minimum 60% coverage for controller layer

**Acceptance Test:** Run code coverage report; verify percentages meet requirements

---

**NFR-MAINT-002: Code Standards**
- SHALL follow Java coding conventions (Google Java Style Guide or similar)
- SHALL use consistent naming conventions
- SHALL document public APIs with Javadoc
- SHALL pass static analysis tools (SonarQube, Checkstyle)

**Acceptance Test:** Run static analysis; verify no critical violations

---

**NFR-MAINT-003: Technical Debt**
- SHALL limit technical debt to < 5% of total codebase (per SonarQube)
- SHALL address critical code smells within 2 sprints
- SHALL refactor duplicate code (DRY principle)

**Acceptance Test:** Monitor technical debt metrics; verify within limits

---

### 7.2 Documentation

**NFR-MAINT-004: API Documentation**
- SHALL provide OpenAPI/Swagger documentation for all REST endpoints
- SHALL include request/response examples
- SHALL document all error codes and messages
- SHALL keep documentation synchronized with code

**Acceptance Test:** Access /swagger-ui/index.html; verify all endpoints documented

---

**NFR-MAINT-005: Code Documentation**
- SHALL document all public classes and methods with Javadoc
- SHALL document complex business logic with inline comments
- SHALL maintain README with getting started guide
- SHALL document database schema and migrations

**Acceptance Test:** Review codebase; verify public APIs documented

---

**NFR-MAINT-006: Architecture Documentation**
- SHALL maintain architecture decision records (ADRs)
- SHALL document system architecture and design patterns
- SHALL maintain deployment diagrams
- SHALL document integration points and dependencies

**Acceptance Test:** Review documentation directory; verify architecture documented

---

### 7.3 Modularity

**NFR-MAINT-007: Layer Separation**
- SHALL maintain clear separation: Controller -> Service -> Repository -> Entity
- SHALL not allow controller to call repository directly
- SHALL not allow business logic in controller or repository layers

**Acceptance Test:** Analyze dependencies; verify layering rules enforced

---

**NFR-MAINT-008: Dependency Management**
- SHALL use Maven for dependency management
- SHALL keep dependencies up to date (patch versions monthly)
- SHALL avoid deprecated libraries
- SHALL minimize dependency count (avoid bloat)

**Acceptance Test:** Run dependency analysis; verify no critical vulnerabilities

---

## 8. Operational Requirements

### 8.1 Monitoring & Observability

**NFR-OPS-001: Application Metrics**
- SHALL expose metrics via Spring Actuator (/actuator/metrics)
- SHALL track: request count, response times, error rates, JVM metrics
- SHALL support integration with Prometheus or similar

**Acceptance Test:** Access metrics endpoint; verify key metrics available

---

**NFR-OPS-002: Logging**
- SHALL use SLF4J/Logback for application logging
- SHALL implement structured logging (JSON format)
- SHALL include correlation IDs for request tracing
- SHALL configure appropriate log levels (INFO for production)

**Acceptance Test:** Review application logs; verify structured format with correlation IDs

---

**NFR-OPS-003: Alerting**
- SHALL alert on high error rates (> 5% of requests)
- SHALL alert on slow response times (> 2x baseline)
- SHALL alert on database connection failures
- SHALL alert on low disk space

**Acceptance Test:** Simulate high error rate; verify alert triggered

---

### 8.2 Deployment & Configuration

**NFR-OPS-004: Configuration Management**
- SHALL externalize all configuration (application.properties / YAML)
- SHALL support environment-specific configurations (dev, test, prod)
- SHALL not hardcode credentials or secrets in code
- SHALL support configuration via environment variables

**Acceptance Test:** Deploy to multiple environments; verify correct configuration loaded

---

**NFR-OPS-005: Deployment Packaging**
- SHALL package as executable JAR with embedded Tomcat
- SHALL support Docker containerization
- SHALL include health checks in container definition
- SHALL minimize container image size

**Acceptance Test:** Build Docker image; verify runs correctly in container

---

**NFR-OPS-006: Zero-Downtime Deployment**
- SHALL support rolling deployments
- SHALL implement graceful shutdown (finish in-flight requests)
- SHALL use health checks for deployment verification

**Acceptance Test:** Deploy new version while load testing; verify no failed requests

---

### 8.3 Database Operations

**NFR-OPS-007: Schema Migration**
- SHALL use Flyway or Liquibase for database migrations
- SHALL version all schema changes
- SHALL support rollback for failed migrations
- SHALL test migrations in lower environments first

**Acceptance Test:** Execute schema migration; verify applied successfully. Rollback; verify reverted

---

**NFR-OPS-008: Database Maintenance**
- SHALL support database backup procedures
- SHALL support database optimization (vacuum, analyze, reindex)
- SHALL monitor database size and growth
- SHALL implement data archival for old transactions

**Acceptance Test:** Execute database backup; verify backup file created and restorable

---

## 9. Acceptance Criteria

### 9.1 Performance Acceptance

- **PA1:** All read operations SHALL meet < 100ms (95th percentile) requirement
- **PA2:** All write operations SHALL meet < 200ms (95th percentile) requirement  
- **PA3:** Fund transfers SHALL complete in < 500ms (95th percentile)
- **PA4:** System SHALL support 100 TPS sustained load
- **PA5:** Credit agency aggregation SHALL complete in < 3.5 seconds total

### 9.2 Security Acceptance

- **SA1:** All APIs SHALL require authentication (401 without token)
- **SA2:** All APIs SHALL enforce authorization (403 for insufficient permissions)
- **SA3:** All communication SHALL use TLS 1.2+ (HTTPS only)
- **SA4:** Sensitive data SHALL be encrypted at rest (AES-256)
- **SA5:** No SQL injection vulnerabilities SHALL exist

### 9.3 Reliability Acceptance

- **RA1:** System SHALL achieve 99.5% availability over 30-day period
- **RA2:** All transactions SHALL maintain ACID properties
- **RA3:** Credit agency timeouts SHALL not crash customer creation
- **RA4:** Database connection failures SHALL trigger retry logic
- **RA5:** All committed transactions SHALL survive application restart

### 9.4 Audit Acceptance

- **AA1:** All data modifications (create, delete) SHALL create PROCTRAN records
- **AA2:** PROCTRAN records SHALL be queryable by date, type, user
- **AA3:** PROCTRAN records SHALL not be modifiable (append-only)
- **AA4:** Audit trail SHALL be retained for minimum 7 years

### 9.5 Maintainability Acceptance

- **MA1:** Service layer SHALL have >= 80% test coverage
- **MA2:** Repository layer SHALL have >= 70% test coverage
- **MA3:** All public APIs SHALL have Javadoc documentation
- **MA4:** Technical debt SHALL be < 5% of codebase (SonarQube)
- **MA5:** OpenAPI documentation SHALL be complete and accurate

### 9.6 Operational Acceptance

- **OA1:** Application metrics SHALL be exposed via /actuator/metrics
- **OA2:** Health checks SHALL accurately reflect system status
- **OA3:** Configuration SHALL be externalized (no hardcoded values)
- **OA4:** Docker image SHALL build and run successfully
- **OA5:** Database migrations SHALL execute successfully in all environments

---

## Appendix A: Performance Baseline Comparison

### Legacy CICS Performance (Baseline)

| Operation | Avg Response Time | 95th Percentile | Max TPS |
|-----------|-------------------|-----------------|---------|
| INQCUST | 50ms | 80ms | 500 |
| INQACC | 45ms | 75ms | 600 |
| CRECUST | 3500ms | 4500ms | 50 |
| CREACC | 150ms | 250ms | 200 |
| UPDCUST | 100ms | 180ms | 300 |
| UPDACC | 120ms | 200ms | 250 |
| XFRFUN | 250ms | 400ms | 150 |
| DBCRFUN | 100ms | 180ms | 400 |
| Overall | - | - | 100 TPS |

### Target Java Performance

| Operation | Target 95th Percentile | Acceptance Threshold |
|-----------|------------------------|----------------------|
| INQCUST | < 100ms | Within 20% of baseline (96ms) |
| INQACC | < 100ms | Within 20% of baseline (90ms) |
| CRECUST | < 5000ms | Within 20% of baseline (5400ms) |
| CREACC | < 300ms | Within 20% of baseline (300ms) |
| UPDCUST | < 200ms | Within 20% of baseline (216ms) |
| UPDACC | < 200ms | Within 20% of baseline (240ms) |
| XFRFUN | < 500ms | Within 20% of baseline (480ms) |
| DBCRFUN | < 200ms | Within 20% of baseline (216ms) |
| Overall | >= 100 TPS | Match or exceed baseline |

---

## Appendix B: Security Controls Matrix

| Security Control | Requirement | Implementation | Testing |
|------------------|-------------|----------------|---------|
| Authentication | JWT bearer tokens | Spring Security | Token validation tests |
| Authorization | Role-based (RBAC) | @PreAuthorize annotations | Permission tests |
| TLS/HTTPS | TLS 1.2+ | Spring Boot SSL config | SSL verification |
| Encryption at Rest | AES-256 | JPA AttributeConverter | Database inspection |
| Input Validation | Strict validation | @Valid annotations | Invalid input tests |
| SQL Injection | Parameterized queries | Spring Data JDBC | Injection attempt tests |
| XSS Prevention | Output sanitization | Content-Type headers | XSS payload tests |
| Audit Logging | All auth/authz events | Spring AOP | Log verification |
| Secrets Management | External key store | Spring Cloud Config | No hardcoded secrets |
| API Rate Limiting | 1000 req/min per user | Spring rate limiter | Rate limit tests |

---

## Appendix C: Monitoring & Alerting Configuration

### Key Metrics to Monitor

| Metric | Threshold | Alert Level | Action |
|--------|-----------|-------------|--------|
| Error Rate | > 5% | Critical | Page on-call engineer |
| Response Time (p95) | > 2x baseline | Warning | Investigate performance |
| CPU Utilization | > 80% | Warning | Scale up resources |
| Memory Usage | > 90% | Critical | Check for memory leaks |
| Database Connections | > 18 of 20 | Warning | Check connection leaks |
| Disk Space | < 10% free | Critical | Archive/cleanup data |
| Failed Logins | > 10 in 5 min | Warning | Possible attack |
| Transaction Rollback Rate | > 2% | Warning | Investigate failures |

### Health Check Configuration

```yaml
management:
  health:
    defaults:
      enabled: true
    db:
      enabled: true
    diskspace:
      enabled: true
      threshold: 10GB
  endpoint:
    health:
      show-details: when-authorized
```

---

## Document Control

**Approvals Required:**
- Performance Engineer: Review performance requirements and baselines
- Security Architect: Review security controls and compliance
- Operations Lead: Review operational requirements and monitoring
- QA Lead: Review acceptance criteria and testing strategy

**Related Documents:**
- 01_Business_Processes.md
- 02_Data_Structures.md
- 03_Functional_Requirements.md
- 04_Integration_Points.md

---

**Document History:**

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2025-10-27 | Devin AI | Initial version - comprehensive NFRs for CBSA migration |
