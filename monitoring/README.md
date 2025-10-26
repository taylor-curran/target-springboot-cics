# Monitoring and Observability

This directory contains the monitoring and observability configuration for the CBSA Java Migration application.

## Overview

The application uses Spring Boot Actuator with Micrometer to expose metrics in Prometheus format. These metrics can be scraped by Prometheus and visualized using Grafana dashboards.

## Architecture

```
Application (Spring Boot) 
    ↓ (exposes metrics)
/actuator/prometheus endpoint
    ↓ (scraped by)
Prometheus Server
    ↓ (queries)
Grafana Dashboards
    ↓ (alerts via)
Alert Manager → Notifications
```

## Quick Start

### 1. Access Metrics Endpoints

Once the application is running, the following endpoints are available:

- **Health Check**: http://localhost:8085/actuator/health
  - Shows application health status and details
  
- **Metrics List**: http://localhost:8085/actuator/metrics
  - Lists all available metrics
  
- **Prometheus Format**: http://localhost:8085/actuator/prometheus
  - Metrics in Prometheus exposition format for scraping

### 2. View Specific Metrics

```bash
# View all available metrics
curl http://localhost:8085/actuator/metrics

# View a specific metric (e.g., JVM memory)
curl http://localhost:8085/actuator/metrics/jvm.memory.used

# View HTTP request metrics
curl http://localhost:8085/actuator/metrics/http.server.requests
```

## Dashboards

Pre-configured Grafana dashboards are available in the `dashboards/` directory:

### 1. Customer Operations Dashboard
**File**: `customer-operations-dashboard.json`

Monitors customer data operations including:
- Customer repository operation rates
- Response time percentiles (95th)
- Error rates for customer endpoints
- Database query performance for customer table
- Current customer record count

**Import Instructions**:
1. Open Grafana UI
2. Navigate to Dashboards → Import
3. Upload `customer-operations-dashboard.json`
4. Select your Prometheus data source
5. Click Import

### 2. Account Operations Dashboard
**File**: `account-operations-dashboard.json`

Monitors account data operations including:
- Account repository operation rates
- Response time percentiles (95th)
- Error rates for account endpoints
- Database query performance for account table
- Current account record count

### 3. Transaction Operations Dashboard
**File**: `transaction-operations-dashboard.json`

Monitors transaction data operations including:
- Transaction repository operation rates
- Response time percentiles (95th)
- Error rates for transaction endpoints
- Database query performance for bank_transaction table
- Current transaction record count

### 4. Application Overview Dashboard
**File**: `application-overview-dashboard.json`

High-level application monitoring including:
- Request rate by endpoint
- Response time distribution (95th and 99th percentiles)
- HTTP status codes breakdown
- JVM heap memory usage
- Garbage collection activity
- Active thread count
- Database connection pool status
- Application uptime

## Alerting

Prometheus alert rules are configured in `alerts/prometheus-alerts.yml`.

### Alert Categories

#### Critical Alerts (Immediate Action Required)
- **HighErrorRate**: Error rate > 5% for 2 minutes
- **VerySlowResponseTime**: 95th percentile > 2 seconds for 3 minutes
- **ApplicationDown**: Application unreachable for 1 minute
- **CriticalMemoryUsage**: Heap usage > 95% for 2 minutes

#### Warning Alerts (Investigation Needed)
- **SlowResponseTime**: 95th percentile > 1 second for 5 minutes
- **HighMemoryUsage**: Heap usage > 90% for 5 minutes
- **HighGCActivity**: GC time > 10% for 5 minutes
- **DatabaseConnectionPoolExhaustion**: > 90% connections in use
- **High*OperationLatency**: Repository operations > 500ms at p95

### Configuring Alert Manager

To enable alert notifications:

1. **Install Alert Manager**:
   ```bash
   # Download and install Prometheus Alert Manager
   wget https://github.com/prometheus/alertmanager/releases/download/v0.26.0/alertmanager-0.26.0.linux-amd64.tar.gz
   tar xvfz alertmanager-0.26.0.linux-amd64.tar.gz
   cd alertmanager-0.26.0.linux-amd64
   ```

2. **Configure Alert Manager** (`alertmanager.yml`):
   ```yaml
   route:
     group_by: ['alertname', 'component']
     group_wait: 10s
     group_interval: 10s
     repeat_interval: 12h
     receiver: 'email-notifications'
   
   receivers:
   - name: 'email-notifications'
     email_configs:
     - to: 'ops-team@example.com'
       from: 'alertmanager@example.com'
       smarthost: 'smtp.example.com:587'
       auth_username: 'alertmanager@example.com'
       auth_password: 'your-password'
   ```

3. **Link Prometheus to Alert Manager**:
   Add to your `prometheus.yml`:
   ```yaml
   alerting:
     alertmanagers:
     - static_configs:
       - targets: ['localhost:9093']
   
   rule_files:
     - '/path/to/monitoring/alerts/prometheus-alerts.yml'
   ```

## Setting Up Prometheus

### 1. Install Prometheus

```bash
# Download Prometheus
wget https://github.com/prometheus/prometheus/releases/download/v2.47.0/prometheus-2.47.0.linux-amd64.tar.gz
tar xvfz prometheus-2.47.0.linux-amd64.tar.gz
cd prometheus-2.47.0.linux-amd64
```

### 2. Configure Prometheus

Create or update `prometheus.yml`:

```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'banking-app'
    metrics_path: '/actuator/prometheus'
    static_configs:
      - targets: ['localhost:8085']
        labels:
          application: 'cbsa-migration'
          environment: 'production'
```

### 3. Start Prometheus

```bash
./prometheus --config.file=prometheus.yml
```

Access Prometheus UI at: http://localhost:9090

## 24-Hour Baseline Collection

To establish a performance baseline:

### 1. Deploy the Application

Ensure the application is running in a production or staging environment with realistic load.

### 2. Configure Prometheus Scraping

Verify Prometheus is successfully scraping metrics:
```bash
# Check if target is up in Prometheus UI
# Navigate to: http://localhost:9090/targets
```

### 3. Run Under Normal Load

Allow the application to run for 24 hours under typical usage patterns. Consider:
- Running automated API tests to simulate user traffic
- Using tools like JMeter or Gatling for load simulation
- Monitoring actual production traffic if in staging

### 4. Collect Baseline Metrics

After 24 hours, query Prometheus to establish baselines:

```promql
# Average request rate
rate(http_server_requests_seconds_count[24h])

# 95th percentile response time
histogram_quantile(0.95, rate(http_server_requests_seconds_bucket[24h]))

# Error rate
rate(http_server_requests_seconds_count{status=~"5.."}[24h]) / rate(http_server_requests_seconds_count[24h])

# Memory usage patterns
avg_over_time(jvm_memory_used_bytes{area="heap"}[24h])

# GC frequency
rate(jvm_gc_pause_seconds_count[24h])
```

### 5. Adjust Alert Thresholds

Based on baseline data, fine-tune alert thresholds in `prometheus-alerts.yml` to match your application's normal behavior.

## Metrics Available

### HTTP Request Metrics
- `http_server_requests_seconds_count`: Total number of HTTP requests
- `http_server_requests_seconds_sum`: Total time spent handling requests
- `http_server_requests_seconds_max`: Maximum request duration
- `http_server_requests_seconds_bucket`: Request duration histogram buckets

**Labels**: `uri`, `method`, `status`, `exception`, `outcome`

### JVM Metrics
- `jvm_memory_used_bytes`: Current memory usage
- `jvm_memory_max_bytes`: Maximum memory available
- `jvm_gc_pause_seconds_*`: GC pause time and count
- `jvm_threads_live_threads`: Number of live threads
- `jvm_classes_loaded_classes`: Number of loaded classes

### Database Metrics
- `hikaricp_connections_active`: Active database connections
- `hikaricp_connections_idle`: Idle database connections
- `hikaricp_connections_pending`: Pending connection requests
- `hikaricp_connections_max`: Maximum pool size

### Application Metrics
- `process_uptime_seconds`: Application uptime
- `process_cpu_usage`: CPU usage percentage
- `system_cpu_usage`: System CPU usage

## Testing the Monitoring Setup

### 1. Verify Actuator Endpoints

```bash
# Health check
curl http://localhost:8085/actuator/health

# List all metrics
curl http://localhost:8085/actuator/metrics | jq '.names[]'

# Get Prometheus format metrics
curl http://localhost:8085/actuator/prometheus | head -50
```

### 2. Generate Test Traffic

```bash
# Generate requests to create metrics
for i in {1..100}; do
  curl http://localhost:8085/api/status
  curl http://localhost:8085/api/utility/sortcode
  curl http://localhost:8085/api/utility/company-name
  sleep 0.1
done
```

### 3. Verify Metrics Collection

```bash
# Check HTTP request metrics are being recorded
curl http://localhost:8085/actuator/prometheus | grep http_server_requests_seconds_count
```

### 4. Test Alert Rules (Optional)

To test alert firing:

```bash
# Simulate high load to trigger slow response time alerts
# Use a load testing tool like Apache Bench
ab -n 10000 -c 100 http://localhost:8085/api/status

# Check Prometheus UI for firing alerts
# Navigate to: http://localhost:9090/alerts
```

## Troubleshooting

### Metrics Not Appearing

1. **Check Actuator is enabled**:
   ```bash
   curl http://localhost:8085/actuator
   ```
   Should return a list of available endpoints.

2. **Verify configuration**:
   Check `application.properties` for:
   ```properties
   management.endpoints.web.exposure.include=health,info,metrics,prometheus
   ```

3. **Check logs**:
   Look for errors in application logs related to Actuator or Micrometer.

### Prometheus Not Scraping

1. **Check target status** in Prometheus UI (http://localhost:9090/targets)
2. **Verify network connectivity** between Prometheus and application
3. **Check firewall rules** if running in separate environments

### Dashboard Not Showing Data

1. **Verify Prometheus data source** is configured in Grafana
2. **Check time range** in dashboard (adjust to recent time)
3. **Verify metrics exist** by querying Prometheus directly
4. **Check dashboard queries** match your actual metric names

## Best Practices

1. **Metric Naming**: Follow Prometheus naming conventions (snake_case, descriptive)
2. **Label Cardinality**: Avoid high-cardinality labels (e.g., user IDs, timestamps)
3. **Retention**: Configure appropriate data retention in Prometheus (default: 15 days)
4. **Backup**: Regularly backup Prometheus data and Grafana dashboards
5. **Security**: Secure actuator endpoints in production (consider authentication)
6. **Performance**: Monitor the monitoring - ensure metrics collection doesn't impact application performance

## Additional Resources

- [Spring Boot Actuator Documentation](https://docs.spring.io/spring-boot/docs/current/reference/html/actuator.html)
- [Micrometer Documentation](https://micrometer.io/docs)
- [Prometheus Documentation](https://prometheus.io/docs/introduction/overview/)
- [Grafana Documentation](https://grafana.com/docs/)
