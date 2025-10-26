# Monitoring and Observability Guide

## Overview
This document describes the monitoring and observability setup for the CBSA Java Migration application.

## Metrics Endpoints

### Health Check
```bash
curl http://localhost:8085/actuator/health
```
Returns application health status, database connectivity, and disk space.

### Metrics
```bash
curl http://localhost:8085/actuator/metrics
```
Lists all available metrics. To view a specific metric:
```bash
curl http://localhost:8085/actuator/metrics/http.server.requests
```

### Prometheus Format
```bash
curl http://localhost:8085/actuator/prometheus
```
Returns all metrics in Prometheus format for scraping.

## Key Metrics

### Response Time Metrics
- `http_server_requests_seconds_sum` / `http_server_requests_seconds_count` = Average response time
- `http_server_requests_seconds_bucket` = Response time histogram (for percentiles)

### Throughput Metrics
- `http_server_requests_seconds_count` = Total request count
- Use `rate()` function to calculate requests per second

### Error Rate Metrics
- `http_server_requests_seconds_count{status="5xx"}` = Server errors
- `http_server_requests_seconds_count{status="4xx"}` = Client errors

### Service Domain Metrics
All HTTP metrics are tagged with:
- `uri` = Request URI (e.g., `/api/status`, `/api/credit-agency/score`)
- `method` = HTTP method (GET, POST, etc.)
- `status` = HTTP status code
- `application` = cbsa-migration

## Dashboards

Three Grafana dashboards are provided in the `/dashboards` directory:

1. **customer-domain-dashboard.json** - Monitors customer-related endpoints
2. **account-domain-dashboard.json** - Monitors account-related endpoints  
3. **transaction-domain-dashboard.json** - Monitors transaction-related endpoints and credit agency service

### Importing Dashboards
1. Open Grafana UI
2. Navigate to Dashboards → Import
3. Upload the JSON file
4. Select your Prometheus data source
5. Click Import

## Alerting

Alert rules are defined in `/monitoring/alert-rules.yml` and cover:

### Performance Alerts
- **HighResponseTime**: Triggers when p95 response time > 1 second
- **LowThroughput**: Triggers when request rate < 10 requests/minute

### Error Alerts
- **HighErrorRate**: Triggers when error rate > 5%
- **ApplicationDown**: Triggers when application is unreachable

### Service-Specific Alerts
- **CreditAgencySlowResponse**: Triggers when credit agency p95 > 5 seconds

### Configuring Alert Thresholds
Edit thresholds in `application.properties`:
```properties
monitoring.alerts.response-time.threshold-ms=1000
monitoring.alerts.error-rate.threshold-percent=5.0
monitoring.alerts.throughput.min-requests-per-minute=10
```

## 24-Hour Baseline Collection

To collect a 24-hour baseline showing normal operations:

### Using Prometheus
1. Configure Prometheus to scrape metrics:
```yaml
scrape_configs:
  - job_name: 'cbsa-migration'
    scrape_interval: 15s
    static_configs:
      - targets: ['localhost:8085']
    metrics_path: '/actuator/prometheus'
```

2. Run application for 24 hours: `mvn spring-boot:run`

3. Query baseline metrics in Prometheus:
```promql
# Average response time over 24h
avg_over_time(http_server_requests_seconds_sum[24h]) / avg_over_time(http_server_requests_seconds_count[24h])

# Request rate over 24h  
rate(http_server_requests_seconds_count[24h])

# Error rate over 24h
rate(http_server_requests_seconds_count{status=~"5.."}[24h]) / rate(http_server_requests_seconds_count[24h])
```

### Using Log Files
Structured logs capture request metrics:
```bash
# Tail logs with duration tracking
tail -f logs/application.log | grep duration=

# Calculate average response time from logs
grep 'duration=' logs/application.log | awk -F'duration=' '{print $2}' | awk '{sum+=$1; count++} END {print sum/count}'
```

## Integration with Existing Logging

The application uses SLF4J with Logback. Metrics complement logs by:

1. **Logs**: Provide detailed context for individual requests/errors
2. **Metrics**: Provide aggregate statistics and trends over time

Both use common tags (uri, method, status) for correlation.

## Testing Alert Configuration

### Trigger High Response Time Alert
```bash
# Credit agency service has configurable delay
curl -X POST http://localhost:8085/api/credit-agency/score \
  -H "Content-Type: application/json" \
  -d '{"sortCode":"987654","customerNumber":1,"name":"Test","address":"Test","dateOfBirth":"1990-01-01","currentCreditScore":500}'
```

### Trigger Error Rate Alert
```bash
# Make requests to non-existent endpoint
for i in {1..100}; do curl http://localhost:8085/api/nonexistent; done
```

### Verify Alert Triggers
Check metrics to confirm alert conditions:
```bash
# Check response time
curl http://localhost:8085/actuator/metrics/http.server.requests | jq '.measurements[] | select(.statistic=="TOTAL_TIME")'

# Check error count
curl http://localhost:8085/actuator/metrics/http.server.requests | jq '.availableTags[] | select(.tag=="status")'
```
