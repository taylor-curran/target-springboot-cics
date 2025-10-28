package com.cbsa.migration.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for OpenApiConfig
 * Tests OpenAPI configuration and documentation setup
 */
@SpringBootTest
class OpenApiConfigTest {

    @Autowired
    private OpenAPI openAPI;

    @Test
    @DisplayName("Should create OpenAPI bean")
    void shouldCreateOpenApiBean() {
        // Then
        assertThat(openAPI).isNotNull();
    }

    @Test
    @DisplayName("Should configure API info correctly")
    void shouldConfigureApiInfoCorrectly() {
        // When
        Info info = openAPI.getInfo();

        // Then
        assertThat(info).isNotNull();
        assertThat(info.getTitle()).isEqualTo("CBSA Banking Application API");
        assertThat(info.getVersion()).isEqualTo("v0.0.1");
        assertThat(info.getDescription()).contains("API documentation for the CICS Banking Sample Application Java migration");
    }

    @Test
    @DisplayName("Should have proper API documentation structure")
    void shouldHaveProperApiDocumentationStructure() {
        // When
        Info info = openAPI.getInfo();

        // Then
        assertThat(info.getTitle()).isNotEmpty();
        assertThat(info.getVersion()).isNotEmpty();
        assertThat(info.getDescription()).isNotEmpty();
        assertThat(info.getDescription()).contains("CICS Banking Sample Application");
    }

    @Test
    @DisplayName("Should support API versioning")
    void shouldSupportApiVersioning() {
        // When
        String version = openAPI.getInfo().getVersion();

        // Then
        assertThat(version).matches("v\\d+\\.\\d+\\.\\d+");
    }

    @Test
    @DisplayName("Should have comprehensive API description")
    void shouldHaveComprehensiveApiDescription() {
        // When
        String description = openAPI.getInfo().getDescription();

        // Then
        assertThat(description).contains("CICS Banking Sample Application");
        assertThat(description).contains("Java migration");
    }
}
