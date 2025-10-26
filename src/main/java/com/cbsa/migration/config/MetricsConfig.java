package com.cbsa.migration.config;

import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.binder.MeterBinder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.TransactionRepository;

@Configuration
public class MetricsConfig {

    @Bean
    public MeterBinder entityCountMetrics(CustomerRepository customerRepository,
                                         AccountRepository accountRepository,
                                         TransactionRepository transactionRepository) {
        return registry -> {
            registry.gauge("banking.customers.count", customerRepository, 
                          CustomerRepository::count);
            
            registry.gauge("banking.accounts.count", accountRepository,
                          AccountRepository::count);
            
            registry.gauge("banking.transactions.count", transactionRepository,
                          TransactionRepository::count);
        };
    }
}
