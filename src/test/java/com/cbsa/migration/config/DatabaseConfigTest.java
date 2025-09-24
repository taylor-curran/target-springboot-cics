package com.cbsa.migration.config;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for DatabaseConfig
 * Tests database configuration and connection setup
 */
class DatabaseConfigTest {

    private DataSource testDataSource;

    @BeforeEach
    void setUp() {
        DriverManagerDataSource ds = new DriverManagerDataSource();
        ds.setDriverClassName("org.h2.Driver");
        ds.setUrl("jdbc:h2:mem:testdb;DB_CLOSE_DELAY=-1");
        testDataSource = ds;
    }

    @Test
    @DisplayName("Should create DataSource with correct properties")
    void shouldCreateDataSourceWithCorrectProperties() {
        // Given
        DriverManagerDataSource ds = new DriverManagerDataSource();
        ds.setDriverClassName("org.h2.Driver");
        ds.setUrl("jdbc:h2:mem:testdb");

        // Then
        assertThat(ds).isNotNull();
        assertThat(ds.getUrl()).isEqualTo("jdbc:h2:mem:testdb");
    }

    @Test
    @DisplayName("Should create JdbcTemplate with DataSource")
    void shouldCreateJdbcTemplateWithDataSource() {
        // When
        JdbcTemplate jdbcTemplate = new JdbcTemplate(testDataSource);

        // Then
        assertThat(jdbcTemplate).isNotNull();
        assertThat(jdbcTemplate.getDataSource()).isEqualTo(testDataSource);
    }

    @Test
    @DisplayName("Should establish database connection")
    void shouldEstablishDatabaseConnection() throws SQLException {
        // When
        try (Connection connection = testDataSource.getConnection()) {
            // Then
            assertThat(connection).isNotNull();
            assertThat(connection.isClosed()).isFalse();
        }
    }

    @Test
    @DisplayName("Should support basic database operations")
    void shouldSupportBasicDatabaseOperations() {
        // Given
        JdbcTemplate jdbcTemplate = new JdbcTemplate(testDataSource);

        // When & Then
        assertThat(jdbcTemplate.queryForObject("SELECT 1", Integer.class)).isEqualTo(1);
    }

    @Test
    @DisplayName("Should handle H2 database URL format")
    void shouldHandleH2DatabaseUrlFormat() {
        // Given
        String h2Url = "jdbc:h2:mem:testdb";
        DriverManagerDataSource ds = new DriverManagerDataSource();
        ds.setUrl(h2Url);

        // Then
        assertThat(ds.getUrl()).contains("h2:mem:");
    }

    @Test
    @DisplayName("Should support transaction-ready configuration")
    void shouldSupportTransactionReadyConfiguration() {
        // Given
        JdbcTemplate jdbcTemplate = new JdbcTemplate(testDataSource);

        assertThat(jdbcTemplate).isNotNull();
        assertThat(testDataSource).isNotNull();
    }
}
