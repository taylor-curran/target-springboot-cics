package com.cbsa.migration.config;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.test.util.ReflectionTestUtils;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * Unit tests for DatabaseConfig
 * Tests database configuration methods and bean creation
 */
class DatabaseConfigTest {

    private DatabaseConfig databaseConfig;

    @BeforeEach
    void setUp() {
        databaseConfig = new DatabaseConfig();
        ReflectionTestUtils.setField(databaseConfig, "databaseUrl", "jdbc:h2:mem:testdb;DB_CLOSE_DELAY=-1");
        ReflectionTestUtils.setField(databaseConfig, "driverClassName", "org.h2.Driver");
    }

    @Test
    @DisplayName("Should create DataSource bean with correct properties")
    void shouldCreateDataSourceBeanWithCorrectProperties() {
        DataSource dataSource = databaseConfig.dataSource();
        
        assertThat(dataSource).isNotNull();
        assertThat(dataSource).isInstanceOf(DriverManagerDataSource.class);
    }

    @Test
    @DisplayName("Should create JdbcTemplate bean with DataSource")
    void shouldCreateJdbcTemplateBeanWithDataSource() {
        JdbcTemplate jdbcTemplate = databaseConfig.jdbcTemplate();
        
        assertThat(jdbcTemplate).isNotNull();
        assertThat(jdbcTemplate.getDataSource()).isNotNull();
    }

    @Test
    @DisplayName("Should reuse same DataSource instance")
    void shouldReuseSameDataSourceInstance() {
        DataSource dataSource1 = databaseConfig.dataSource();
        DataSource dataSource2 = databaseConfig.dataSource();
        
        assertThat(dataSource1).isSameAs(dataSource2);
    }

    @Test
    @DisplayName("Should establish database connection")
    void shouldEstablishDatabaseConnection() throws SQLException {
        DataSource dataSource = databaseConfig.dataSource();
        
        try (Connection connection = dataSource.getConnection()) {
            assertThat(connection).isNotNull();
            assertThat(connection.isClosed()).isFalse();
        }
    }

    @Test
    @DisplayName("Should support basic database operations")
    void shouldSupportBasicDatabaseOperations() {
        JdbcTemplate jdbcTemplate = databaseConfig.jdbcTemplate();
        
        Integer result = jdbcTemplate.queryForObject("SELECT 1", Integer.class);
        assertThat(result).isEqualTo(1);
    }

    @Test
    @DisplayName("Should handle database initialization errors")
    void shouldHandleDatabaseInitializationErrors() {
        assertThatThrownBy(() -> databaseConfig.initializeDatabase())
            .isInstanceOf(Exception.class);
    }
}
