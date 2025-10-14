package com.cbsa.migration.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.core.env.Environment;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ScriptUtils;
import org.springframework.beans.factory.annotation.Autowired;

import javax.annotation.PostConstruct;
import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Arrays;

/**
 * Database configuration class for the banking application
 */
@Configuration
public class DatabaseConfig {

    @Value("${spring.datasource.url}")
    private String databaseUrl;

    @Value("${spring.datasource.driver-class-name}")
    private String driverClassName;
    
    @Autowired
    private Environment environment;
    
    private DataSource dataSource;

    /**
     * Initialize the database with our schema
     * Test environment uses H2 and test-schema.sql (configured in application-test.properties)
     * Production environment uses SQLite and schema.sql
     */
    @PostConstruct
    public void initializeDatabase() {
        // Skip initialization for test profile - Spring Boot handles it via application-test.properties
        if (Arrays.asList(environment.getActiveProfiles()).contains("test")) {
            return;
        }
        
        // Create datasource first
        if (dataSource == null) {
            DriverManagerDataSource ds = new DriverManagerDataSource();
            ds.setDriverClassName(driverClassName);
            ds.setUrl(databaseUrl);
            dataSource = ds;
        }
        
        try (Connection connection = dataSource.getConnection()) {
            ScriptUtils.executeSqlScript(connection, new ClassPathResource("db/schema.sql"));
        } catch (SQLException e) {
            throw new RuntimeException("Failed to initialize database", e);
        }
    }

    /**
     * Configure JdbcTemplate with our datasource
     */
    @Bean
    public JdbcTemplate jdbcTemplate() {
        return new JdbcTemplate(dataSource());
    }
    
    /**
     * Configure DataSource using injected properties
     */
    @Bean
    public DataSource dataSource() {
        if (dataSource == null) {
            DriverManagerDataSource ds = new DriverManagerDataSource();
            ds.setDriverClassName(driverClassName);
            ds.setUrl(databaseUrl);
            dataSource = ds;
        }
        return dataSource;
    }
}
