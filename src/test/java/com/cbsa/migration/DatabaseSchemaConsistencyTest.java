package com.cbsa.migration;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Pattern;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Critical test to ensure production (SQLite) and test (H2) database schemas remain synchronized.
 * 
 * This test WILL FAIL when schemas diverge, which is intentional.
 * When this test fails:
 * 1. You've likely updated one schema without updating the other
 * 2. Update BOTH schemas to match (or understand why they differ)
 * 3. Test schemas should be H2-compatible versions of production schemas
 * 
 * Schema files:
 * - Production: src/main/resources/db/schema.sql (SQLite)
 * - Test: src/test/resources/db/test-schema.sql (H2)
 */
public class DatabaseSchemaConsistencyTest {

    private static final String PROD_SCHEMA_PATH = "src/main/resources/db/schema.sql";
    private static final String TEST_SCHEMA_PATH = "src/test/resources/db/test-schema.sql";

    @Test
    void testProductionAndTestSchemasAreConsistent() throws IOException {
        // Read both schema files
        String prodSchema = Files.readString(Paths.get(PROD_SCHEMA_PATH));
        String testSchema = Files.readString(Paths.get(TEST_SCHEMA_PATH));

        // Normalize schemas for comparison
        String normalizedProd = normalizeSchema(prodSchema, "sqlite");
        String normalizedTest = normalizeSchema(testSchema, "h2");

        // Compare table definitions
        assertTableDefinitionsMatch(normalizedProd, normalizedTest);
        
        // Compare constraints
        assertConstraintsMatch(normalizedProd, normalizedTest);
        
        // Compare indexes
        assertIndexesMatch(normalizedProd, normalizedTest);
    }

    /**
     * Normalizes schema SQL for comparison by removing database-specific syntax
     */
    private String normalizeSchema(String schema, String dbType) {
        // Remove comments
        schema = schema.replaceAll("--.*?\n", "\n");
        
        // Normalize whitespace
        schema = schema.replaceAll("\\s+", " ");
        
        // Normalize data types based on database
        if ("sqlite".equalsIgnoreCase(dbType)) {
            // SQLite TEXT → generic VARCHAR for comparison
            schema = schema.replaceAll("\\bTEXT\\b", "VARCHAR(255)");
            // SQLite INTEGER PRIMARY KEY is actually BIGINT in behavior
            schema = schema.replaceAll("INTEGER PRIMARY KEY AUTOINCREMENT", "BIGINT AUTO_INCREMENT PRIMARY KEY");
            schema = schema.replaceAll("INTEGER PRIMARY KEY", "BIGINT PRIMARY KEY");
        } else if ("h2".equalsIgnoreCase(dbType)) {
            // H2 normalization
            schema = schema.replaceAll("\\bTEXT\\b", "VARCHAR(255)");
            // Only replace TIMESTAMP when it's a data type, not in DEFAULT CURRENT_TIMESTAMP
            schema = schema.replaceAll("(\\s)TIMESTAMP(\\s)", "$1VARCHAR(255)$2");
            schema = schema.replaceAll("BIGINT AUTO_INCREMENT", "BIGINT");
        }
        
        // Normalize other common differences
        schema = schema.toUpperCase();
        schema = schema.replaceAll("VARCHAR\\(\\d+\\)", "VARCHAR(255)");
        
        // Remove database-specific clauses
        schema = schema.replaceAll("AUTO_INCREMENT", "");
        schema = schema.replaceAll("AUTOINCREMENT", "");
        
        return schema.trim();
    }

    /**
     * Compares table definitions between schemas
     */
    private void assertTableDefinitionsMatch(String prodSchema, String testSchema) {
        // Extract all CREATE TABLE statements
        Pattern tablePattern = Pattern.compile("CREATE TABLE[^;]+;", Pattern.CASE_INSENSITIVE);
        
        var prodTables = tablePattern.matcher(prodSchema).results()
            .map(mr -> normalizeTableDefinition(mr.group()))
            .sorted()
            .toList();
            
        var testTables = tablePattern.matcher(testSchema).results()
            .map(mr -> normalizeTableDefinition(mr.group()))
            .sorted()
            .toList();
        
        // Check same number of tables
        assertThat(testTables.size())
            .as("Number of tables should match between production and test schemas")
            .isEqualTo(prodTables.size());
        
        // Check each table matches
        for (int i = 0; i < prodTables.size(); i++) {
            String prodTable = prodTables.get(i);
            String testTable = testTables.get(i);
            
            // Extract table name for better error messages
            String tableName = extractTableName(prodTable);
            
            assertThat(testTable)
                .as("Table '%s' definition should match between schemas", tableName)
                .isEqualTo(prodTable);
        }
    }

    /**
     * Compares constraint definitions
     */
    private void assertConstraintsMatch(String prodSchema, String testSchema) {
        // Extract CHECK constraints
        Pattern checkPattern = Pattern.compile("CHECK\\s*\\([^)]+\\)", Pattern.CASE_INSENSITIVE);
        
        var prodChecks = checkPattern.matcher(prodSchema).results()
            .map(mr -> mr.group().toUpperCase())
            .sorted()
            .toList();
            
        var testChecks = checkPattern.matcher(testSchema).results()
            .map(mr -> mr.group().toUpperCase())
            .sorted()
            .toList();
        
        assertThat(testChecks)
            .as("CHECK constraints should match between schemas")
            .containsExactlyElementsOf(prodChecks);
        
        // Extract FOREIGN KEY constraints
        Pattern fkPattern = Pattern.compile("FOREIGN KEY[^;]+?REFERENCES[^)]+\\)", Pattern.CASE_INSENSITIVE);
        
        var prodFKs = fkPattern.matcher(prodSchema).results()
            .map(mr -> mr.group().toUpperCase().replaceAll("\\s+", " "))
            .sorted()
            .toList();
            
        var testFKs = fkPattern.matcher(testSchema).results()
            .map(mr -> mr.group().toUpperCase().replaceAll("\\s+", " "))
            .sorted()
            .toList();
        
        assertThat(testFKs)
            .as("FOREIGN KEY constraints should match between schemas")
            .containsExactlyElementsOf(prodFKs);
    }

    /**
     * Compares index definitions
     */
    private void assertIndexesMatch(String prodSchema, String testSchema) {
        Pattern indexPattern = Pattern.compile("CREATE\\s+INDEX[^;]+;", Pattern.CASE_INSENSITIVE);
        
        var prodIndexes = indexPattern.matcher(prodSchema).results()
            .map(mr -> normalizeIndexDefinition(mr.group()))
            .sorted()
            .toList();
            
        var testIndexes = indexPattern.matcher(testSchema).results()
            .map(mr -> normalizeIndexDefinition(mr.group()))
            .sorted()
            .toList();
        
        // Note: Test schema may have fewer indexes (H2 doesn't support partial indexes)
        // So we check that test indexes are a subset of production
        for (String testIndex : testIndexes) {
            String indexName = extractIndexName(testIndex);
            boolean found = prodIndexes.stream()
                .anyMatch(prod -> prod.contains(indexName));
            
            assertThat(found)
                .as("Index '%s' from test schema should exist in production schema", indexName)
                .isTrue();
        }
    }

    /**
     * Normalizes table definition for comparison
     */
    private String normalizeTableDefinition(String tableDef) {
        return tableDef
            .toUpperCase()
            .replaceAll("\\s+", " ")
            .replaceAll("IF NOT EXISTS", "")
            .replaceAll("VARCHAR\\(\\d+\\)", "VARCHAR(255)")
            .trim();
    }

    /**
     * Normalizes index definition for comparison
     */
    private String normalizeIndexDefinition(String indexDef) {
        return indexDef
            .toUpperCase()
            .replaceAll("\\s+", " ")
            .replaceAll("IF NOT EXISTS", "")
            .replaceAll("WHERE[^;]+", "") // Remove partial index clauses
            .trim();
    }

    /**
     * Extracts table name from CREATE TABLE statement
     */
    private String extractTableName(String createTable) {
        Pattern pattern = Pattern.compile("CREATE\\s+TABLE\\s+(?:IF\\s+NOT\\s+EXISTS\\s+)?(\\w+)", 
                                         Pattern.CASE_INSENSITIVE);
        var matcher = pattern.matcher(createTable);
        if (matcher.find()) {
            return matcher.group(1);
        }
        return "UNKNOWN";
    }

    /**
     * Extracts index name from CREATE INDEX statement
     */
    private String extractIndexName(String createIndex) {
        Pattern pattern = Pattern.compile("CREATE\\s+INDEX\\s+(?:IF\\s+NOT\\s+EXISTS\\s+)?(\\w+)", 
                                         Pattern.CASE_INSENSITIVE);
        var matcher = pattern.matcher(createIndex);
        if (matcher.find()) {
            return matcher.group(1);
        }
        return "UNKNOWN";
    }
}
