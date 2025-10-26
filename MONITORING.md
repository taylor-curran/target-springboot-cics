# Monitoring and Observability Guide

This document describes the monitoring infrastructure for the CBSA Banking Application Spring Boot migration.

## Overview

The application uses Spring Boot Actuator with Micrometer Prometheus registry to expose comprehensive metrics for monitoring application health, performance, and business operations.

## Quick Access

### Actuator Endpoints

Once the application is running on `http://localhost:8085`, the following endpoints are available:

- **Health Check**: `http://localhost:8085/actuator/health`
  - Shows application health status and component details
  
- **Metrics List**: `http://localhost:8085/actuator/metrics`
  - Lists all available metrics
  
- **Prometheus Metrics**: `http://localhost:8085/actuator/prometheus`
  - Prometheus-formatted metrics for scraping
  
- **Application Info**: `http://localhost:8085/actuator/info`
  - Application metadata and version information

### Example Usage

```bash
# Check application health
curl http://localhost:8085/actuator/health

# View all available metrics
curl http://localhost:8085/actuator/metrics

# Get Prometheus-formatted metrics
curl http://localhost:8085/actuator/prometheus

# View specific metric (e.g., JVM memory)
curl http://localhost:8085/actuator/metrics/jvm.memory.used
```

## Custom Business Metrics

### Entity Count Gauges

- `banking.customers.count` - Total number of customers in the system
- `banking.accounts.count` - Total number of accounts in the system
- `banking.transactions.count` - Total number of transactions in the system

### Credit Score Processing Metrics

- `banking.credit.score.requests_total{status="success"}` - Successful credit score requests
- `banking.credit.score.requests_total{status="failure"}` - Failed credit score requests
- `banking.credit.score.processing.time` - Credit score processing duration (timer)

### HTTP Metrics (Automatic)

Spring Boot Actuator automatically exposes:
- `http.server.requests` - HTTP request counts and durations by endpoint, method, and status
- Response time percentiles (P50, P95, P99)
- Error rates by endpoint

### JVM Metrics (Automatic)

- `jvm.memory.used` / `jvm.memory.max` - Heap and non-heap memory usage
- `jvm.gc.pause` - Garbage collection pause times
- `jvm.threads.live` / `jvm.threads.peak` - Thread counts
- `process.cpu.usage` - CPU usage

## Prometheus Setup

### Configuration

A Prometheus configuration file is provided at `monitoring/prometheus.yml`:

```yaml
scrape_configs:
  - job_name: 'spring-boot-banking-app'
    metrics_path: '/actuator/prometheus'
    scrape_interval: 10s
    static_configs:
      - targets: ['localhost:8085']
```

### Running Prometheus

1. **Download Prometheus**: https://prometheus.io/download/

2. **Start Prometheus** with the provided configuration:
   ```bash
   prometheus --config.file=monitoring/prometheus.yml
   ```

3. **Access Prometheus UI**: http://localhost:9090

4. **Verify Scraping**: Check Status → Targets to ensure the Spring Boot app is being scraped

## Grafana Dashboards

Pre-configured dashboard JSON files are available in the `monitoring/` directory:

### Customer Domain Dashboard
**File**: `grafana-dashboard-customer.json`

Visualizes:
- Customer count over time
- Customer operation rates
- Customer query response times (P95)
- Customer operation error rates

### Account Domain Dashboard
**File**: `grafana-dashboard-account.json`

Visualizes:
- Account count over time
- Account operation rates
- Account query response times (P95)
- Account operation error rates

### Transaction Domain Dashboard
**File**: `grafana-dashboard-transaction.json`

Visualizes:
- Transaction count over time
- Transaction rate per second
- Transaction operation response times (P95)
- Transaction operation error rates

### System Metrics Dashboard
**File**: `grafana-dashboard-system.json`

Visualizes:
- JVM heap memory usage
- HTTP request rates
- HTTP response time percentiles (P50, P95, P99)
- CPU usage
- Overall error rates
- Credit score processing metrics

### Importing Dashboards

1. **Install Grafana**: https://grafana.com/grafana/download

2. **Start Grafana**: 
   ```bash
   grafana-server
   ```
   Default URL: http://localhost:3000 (admin/admin)

3. **Add Prometheus Data Source**:
   - Configuration → Data Sources → Add data source
   - Select Prometheus
   - URL: http://localhost:9090
   - Save & Test

4. **Import Dashboards**:
   - Dashboards → Import
   - Upload JSON file or paste JSON content
   - Select Prometheus data source
   - Import

## Alerting

### Alert Rules

Alert rules are defined in `monitoring/prometheus-alerts.yml`:

#### High Error Rate Alert
- **Condition**: Error rate > 5% for 2 minutes
- **Severity**: Warning
- **Description**: Triggers when 5xx HTTP errors exceed threshold

#### Slow Response Time Alert
- **Condition**: P95 response time > 2 seconds for 3 minutes
- **Severity**: Warning
- **Description**: Triggers when API response times degrade

#### Credit Score Processing Failures
- **Condition**: Failure rate > 0.1/sec for 2 minutes
- **Severity**: Critical
- **Description**: Triggers on high credit processing failure rate

#### Customer Count Dropped
- **Condition**: Customer count < 50 for 5 minutes
- **Severity**: Critical
- **Description**: Potential data loss detection

#### High Memory Usage
- **Condition**: Heap usage > 85% for 5 minutes
- **Severity**: Warning
- **Description**: JVM memory pressure alert

### Configuring Alertmanager

1. **Download Alertmanager**: https://prometheus.io/download/

2. **Create alertmanager.yml**:
   ```yaml
   route:
     receiver: 'email-alerts'
   
   receivers:
     - name: 'email-alerts'
       email_configs:
         - to: 'team@example.com'
           from: 'alertmanager@example.com'
           smarthost: 'smtp.example.com:587'
   ```

3. **Update Prometheus config** to enable alerting (already configured in prometheus.yml)

4. **Start Alertmanager**:
   ```bash
   alertmanager --config.file=alertmanager.yml
   ```

## 24-Hour Baseline Collection

To establish a baseline of normal operations:

### Procedure

1. **Start the application**:
   ```bash
   mvn spring-boot:run
   ```

2. **Let it run for 24 hours** with typical usage patterns

3. **Monitor metrics** via Prometheus/Grafana during this period

4. **Record baseline values** for:
   - Average response times per endpoint
   - Typical request rates
   - Normal memory usage patterns
   - Average CPU utilization
   - Error rates (should be near zero)

5. **Export baseline data**:
   - Use Prometheus snapshot: `curl -XPOST http://localhost:9090/api/v1/admin/tsdb/snapshot`
   - Or export from Grafana dashboards

### Baseline Metrics to Capture

| Metric | Expected Baseline |
|--------|-------------------|
| Response Time (P95) | < 500ms for most endpoints |
| Error Rate | < 0.1% |
| CPU Usage | < 50% average |
| Memory Usage | Steady state without leaks |
| Customer Operations/sec | Based on actual usage |
| Account Operations/sec | Based on actual usage |
| Transaction Operations/sec | Based on actual usage |

## Troubleshooting

### Metrics Not Appearing

1. **Check Actuator is enabled**:
   ```bash
   curl http://localhost:8085/actuator
   ```
   Should list exposed endpoints

2. **Verify application.properties** has correct configuration

3. **Check application logs** for Actuator startup messages

### Prometheus Not Scraping

1. **Verify target is UP** in Prometheus UI (Status → Targets)

2. **Check connectivity**:
   ```bash
   curl http://localhost:8085/actuator/prometheus
   ```

3. **Review prometheus.yml** configuration

### High Memory Usage

1. **Check JVM heap settings** in application startup

2. **Review metrics**:
   ```bash
   curl http://localhost:8085/actuator/metrics/jvm.memory.used
   ```

3. **Adjust heap size** if needed:
   ```bash
   mvn spring-boot:run -Dspring-boot.run.jvmArguments="-Xmx1024m"
   ```

## Integration with Existing Logging

The monitoring setup integrates seamlessly with the existing logging framework:

- **Log Levels**: Configured in `application.properties` (DEBUG for com.cbsa.migration)
- **Metrics complement logs**: Metrics provide quantitative data while logs provide context
- **Correlation**: Use trace IDs (if implemented) to correlate metrics with log entries
- **Error tracking**: Both ERROR logs and error metrics capture failures

### Best Practices

1. **Use structured logging** for better correlation with metrics
2. **Log correlation IDs** for request tracing
3. **Monitor both metrics and logs** for complete observability
4. **Set up log aggregation** (e.g., ELK stack) alongside metrics

## Next Steps

1. **Production Deployment**: Update prometheus.yml targets for production instances
2. **Alert Channels**: Configure Slack/PagerDuty integration in Alertmanager
3. **Dashboard Refinement**: Customize dashboards based on actual usage patterns
4. **SLO Definition**: Establish Service Level Objectives based on baseline data
5. **Distributed Tracing**: Consider adding Spring Cloud Sleuth for request tracing

## Support

For questions about monitoring setup:
- Review Spring Boot Actuator documentation: https://docs.spring.io/spring-boot/docs/current/reference/html/actuator.html
- Review Micrometer documentation: https://micrometer.io/docs
- Check application logs for monitoring-related issues
