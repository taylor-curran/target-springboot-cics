# Monitoring and Observability Setup

This directory contains the monitoring infrastructure configuration for the CBSA Spring Boot migration.

## Overview

The monitoring setup includes:
- **Spring Boot Actuator** for metrics collection
- **Micrometer** with Prometheus registry for metrics export
- **Grafana dashboards** for visualization (3 domain-specific dashboards)
- **Prometheus alerting rules** for operational alerts

## Quick Start

### 1. Start the Application

```bash
cd /path/to/target-springboot-cics
mvn spring-boot:run
```

### 2. Access Metrics Endpoints

Once the application is running, the following endpoints are available:

- **Health Check**: `http://localhost:8085/actuator/health`
  - Shows application health status and database connectivity
  
- **Metrics List**: `http://localhost:8085/actuator/metrics`
  - Lists all available metrics that can be queried
  
- **Prometheus Metrics**: `http://localhost:8085/actuator/prometheus`
  - Prometheus-formatted metrics for scraping

### 3. Test Metrics Collection

```bash
# Check application health
curl http://localhost:8085/actuator/health

# View all available metrics
curl http://localhost:8085/actuator/metrics

# View specific metric (e.g., JVM memory)
curl http://localhost:8085/actuator/metrics/jvm.memory.used

# View Prometheus-formatted metrics
curl http://localhost:8085/actuator/prometheus | head -50
```

## Dashboards

Three domain-specific Grafana dashboards are provided:

### 1. Customer Domain (`grafana-dashboard-customer.json`)

Monitors customer-related operations:
- Customer operation request rates
- Customer endpoint response times (p50, p95)
- Customer repository database connections
- Customer service error rates (4xx, 5xx)
- Customer count statistics
- JVM heap memory usage

### 2. Account Domain (`grafana-dashboard-account.json`)

Monitors account-related operations:
- Account operation request rates
- Account endpoint response times (p50, p95)
- Account repository query performance
- Account service error rates
- Account count statistics
- System CPU usage

### 3. Transaction Domain (`grafana-dashboard-transaction.json`)

Monitors transaction-related operations:
- Transaction operation request rates
- Transaction endpoint response times (p50, p95, p99)
- Transaction repository throughput
- Transaction service error rates
- Transaction count statistics
- Credit agency simulation latency
- Garbage collection activity

## Importing Dashboards into Grafana

1. Access your Grafana instance (typically `http://localhost:3000`)
2. Navigate to **Dashboards** → **Import**
3. Click **Upload JSON file** or paste the JSON contents
4. Select your Prometheus data source
5. Click **Import**

## Alerting Rules

The `prometheus-alerts.yml` file contains alerting rules for:

### Response Time Alerts
- **HighResponseTime**: Warning when p95 > 1s
- **CriticalResponseTime**: Critical when p95 > 2s

### Error Rate Alerts
- **HighErrorRate**: Warning when error rate > 5%
- **CriticalErrorRate**: Critical when error rate > 10%

### Resource Alerts
- **DatabaseConnectionPoolExhaustion**: Warning at 80%, Critical at 95%
- **HighJVMMemoryUsage**: Warning at 85%, Critical at 95%
- **HighGCActivity**: Warning when GC runs > 10 times/sec

### Availability Alerts
- **ApplicationDown**: Critical when application is unreachable
- **CreditAgencyHighLatency**: Warning when credit agency p95 > 3s

## Prometheus Configuration

Add the Spring Boot application as a scrape target in your `prometheus.yml`:

```yaml
scrape_configs:
  - job_name: 'cbsa-banking-app'
    metrics_path: '/actuator/prometheus'
    scrape_interval: 15s
    static_configs:
      - targets: ['localhost:8085']
        labels:
          application: 'cbsa-migration'
          environment: 'development'
```

Then load the alerting rules:

```yaml
rule_files:
  - 'prometheus-alerts.yml'
```

## Available Metrics

### HTTP Metrics
- `http_server_requests_seconds_count` - Total HTTP requests
- `http_server_requests_seconds_sum` - Total request duration
- `http_server_requests_seconds_bucket` - Request duration histogram

### JVM Metrics
- `jvm_memory_used_bytes` - JVM memory usage
- `jvm_memory_max_bytes` - JVM memory limits
- `jvm_gc_pause_seconds_count` - Garbage collection count
- `jvm_threads_live` - Active thread count

### Database Metrics
- `hikaricp_connections_active` - Active database connections
- `hikaricp_connections_idle` - Idle database connections
- `hikaricp_connections_max` - Maximum connection pool size
- `hikaricp_connections_pending` - Pending connection requests

### System Metrics
- `system_cpu_usage` - System CPU usage
- `process_cpu_usage` - Process CPU usage
- `system_load_average_1m` - System load average

## 24-Hour Baseline Collection

To establish a performance baseline:

1. Ensure the application is running with monitoring enabled
2. Generate realistic load:
   ```bash
   # The application auto-generates 100 customers with transaction history
   mvn spring-boot:run
   ```
3. Let Prometheus scrape metrics for 24 hours
4. Review the Grafana dashboards to establish normal operation baselines
5. Adjust alert thresholds in `prometheus-alerts.yml` based on observed performance

### Recommended Baseline Metrics to Capture

- **Response Times**: p50, p95, p99 for each endpoint
- **Request Rates**: Average requests per second per endpoint
- **Error Rates**: Baseline error rate for normal operations
- **Resource Usage**: Normal memory, CPU, and connection pool usage
- **GC Activity**: Baseline garbage collection frequency

## Troubleshooting

### Metrics Not Appearing

1. Verify actuator endpoints are accessible:
   ```bash
   curl http://localhost:8085/actuator
   ```

2. Check application logs for actuator startup:
   ```bash
   # Look for lines mentioning "Exposing" endpoints
   mvn spring-boot:run | grep -i actuator
   ```

3. Verify Prometheus configuration points to correct endpoint

### High Memory Usage

- Check JVM heap settings in application startup
- Review Grafana dashboard for memory trends
- Consider adjusting `-Xmx` JVM parameter if needed

### Connection Pool Exhaustion

- Review database query patterns in logs
- Check for connection leaks in repository code
- Consider increasing pool size in configuration

## Integration with Existing Logging

The monitoring setup integrates with the existing logging configuration:

- Log level: `logging.level.com.cbsa.migration=DEBUG`
- Actuator uses the same logging framework
- Metrics are collected independently but complement log analysis

## Next Steps

1. **Deploy Prometheus** - Set up Prometheus to scrape metrics
2. **Deploy Grafana** - Import the provided dashboards
3. **Configure Alertmanager** - Set up alert notifications (email, Slack, PagerDuty)
4. **Baseline Collection** - Run for 24 hours to establish baselines
5. **Tune Alerts** - Adjust thresholds based on baseline data

## Support

For issues or questions about monitoring setup:
- Review application logs: `mvn spring-boot:run`
- Check actuator endpoints: `curl http://localhost:8085/actuator`
- Verify metrics collection: `curl http://localhost:8085/actuator/prometheus`

## References

- [Spring Boot Actuator Documentation](https://docs.spring.io/spring-boot/docs/current/reference/html/actuator.html)
- [Micrometer Documentation](https://micrometer.io/docs)
- [Prometheus Documentation](https://prometheus.io/docs/introduction/overview/)
- [Grafana Documentation](https://grafana.com/docs/)
