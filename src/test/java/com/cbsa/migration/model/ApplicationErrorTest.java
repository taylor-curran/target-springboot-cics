package com.cbsa.migration.model;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ApplicationErrorTest {

    @Test
    @DisplayName("Should create valid object with minimal fields")
    void testConstructor_withMinimalFields_createsValidObject() {
        // Given
        String applicationId = "TESTAPP";
        String errorCode = "ABND";
        
        // When
        ApplicationError error = new ApplicationError();
        error.setApplicationId(applicationId);
        error.setErrorCode(errorCode);
        
        // Then
        assertThat(error).isNotNull();
        assertThat(error.getApplicationId()).isEqualTo(applicationId);
        assertThat(error.getErrorCode()).isEqualTo(errorCode);
        assertThat(error.getId()).isNull();
    }

    @Test
    @DisplayName("Should set all properties with all-args constructor")
    void testAllArgsConstructor_setsAllProperties() {
        // Given
        Long id = 1L;
        String timestamp = "2023-12-25T10:30:00";
        String applicationId = "BNKAPP";
        String transactionId = "TRAN001";
        String errorCode = "ABND";
        String programName = "ABNDPROC";
        String errorMessage = "Test error message";
        String stackTrace = "Stack trace here";
        String createdAt = "2023-12-25T10:30:00";
        String responseCode = "12";
        String response2Code = "34";
        String sqlCode = "-803";
        String freeformText = "Database constraint violation";
        
        // When
        ApplicationError error = new ApplicationError(id, timestamp, applicationId, transactionId,
            errorCode, programName, errorMessage, stackTrace, createdAt,
            responseCode, response2Code, sqlCode, freeformText);
        
        // Then
        assertThat(error.getId()).isEqualTo(id);
        assertThat(error.getTimestamp()).isEqualTo(timestamp);
        assertThat(error.getApplicationId()).isEqualTo(applicationId);
        assertThat(error.getTransactionId()).isEqualTo(transactionId);
        assertThat(error.getErrorCode()).isEqualTo(errorCode);
        assertThat(error.getProgramName()).isEqualTo(programName);
        assertThat(error.getErrorMessage()).isEqualTo(errorMessage);
        assertThat(error.getStackTrace()).isEqualTo(stackTrace);
        assertThat(error.getCreatedAt()).isEqualTo(createdAt);
        assertThat(error.getResponseCode()).isEqualTo(responseCode);
        assertThat(error.getResponse2Code()).isEqualTo(response2Code);
        assertThat(error.getSqlCode()).isEqualTo(sqlCode);
        assertThat(error.getFreeformText()).isEqualTo(freeformText);
    }

    @Test
    @DisplayName("Should set all properties with 11-parameter constructor")
    void testElevenParameterConstructor_setsAllPropertiesExceptId() {
        // Given
        String timestamp = "2023-12-25T10:30:00";
        String applicationId = "BNKAPP";
        String transactionId = "TRAN001";
        String errorCode = "ABND";
        String programName = "ABNDPROC";
        String errorMessage = "Test error message";
        String stackTrace = "Stack trace here";
        String responseCode = "12";
        String response2Code = "34";
        String sqlCode = "-803";
        String freeformText = "Database constraint violation";
        
        // When
        ApplicationError error = new ApplicationError(timestamp, applicationId, transactionId,
            errorCode, programName, errorMessage, stackTrace, responseCode, 
            response2Code, sqlCode, freeformText);
        
        // Then
        assertThat(error.getId()).isNull();
        assertThat(error.getTimestamp()).isEqualTo(timestamp);
        assertThat(error.getApplicationId()).isEqualTo(applicationId);
        assertThat(error.getTransactionId()).isEqualTo(transactionId);
        assertThat(error.getErrorCode()).isEqualTo(errorCode);
        assertThat(error.getProgramName()).isEqualTo(programName);
        assertThat(error.getErrorMessage()).isEqualTo(errorMessage);
        assertThat(error.getStackTrace()).isEqualTo(stackTrace);
        assertThat(error.getResponseCode()).isEqualTo(responseCode);
        assertThat(error.getResponse2Code()).isEqualTo(response2Code);
        assertThat(error.getSqlCode()).isEqualTo(sqlCode);
        assertThat(error.getFreeformText()).isEqualTo(freeformText);
    }

    @Test
    @DisplayName("Should return true for critical error with ABND code")
    void testIsCritical_withAbendCode_returnsTrue() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setErrorCode("ABND");
        
        // When
        boolean result = error.isCritical();
        
        // Then
        assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Should return true for critical error with ABN prefix")
    void testIsCritical_withAbnPrefix_returnsTrue() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setErrorCode("ABN123");
        
        // When
        boolean result = error.isCritical();
        
        // Then
        assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Should return true for critical error with ASRA code")
    void testIsCritical_withAsraCode_returnsTrue() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setErrorCode("ASRA");
        
        // When
        boolean result = error.isCritical();
        
        // Then
        assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Should return true for critical error with ASRB code")
    void testIsCritical_withAsrbCode_returnsTrue() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setErrorCode("ASRB");
        
        // When
        boolean result = error.isCritical();
        
        // Then
        assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Should return false for non-critical error code")
    void testIsCritical_withNormalCode_returnsFalse() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setErrorCode("INFO");
        
        // When
        boolean result = error.isCritical();
        
        // Then
        assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Should return false when error code is null")
    void testIsCritical_withNullErrorCode_returnsFalse() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setErrorCode(null);
        
        // When
        boolean result = error.isCritical();
        
        // Then
        assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Should return false for empty error code")
    void testIsCritical_withEmptyErrorCode_returnsFalse() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setErrorCode("");
        
        // When
        boolean result = error.isCritical();
        
        // Then
        assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Should contain essential fields in error summary")
    void testGetErrorSummary_containsEssentialFields() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setApplicationId("TESTAPP");
        error.setTransactionId("TRAN123");
        error.setErrorCode("ABND");
        error.setProgramName("TESTPROG");
        error.setErrorMessage("Test error message");
        
        // When
        String result = error.getErrorSummary();
        
        // Then
        assertThat(result).contains("ABND");
        assertThat(result).contains("TESTPROG");
        assertThat(result).contains("TRAN123");
        assertThat(result).contains("Test error message");
    }

    @Test
    @DisplayName("Should handle null fields in error summary")
    void testGetErrorSummary_withNullFields_usesDefaults() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setErrorCode(null);
        error.setProgramName(null);
        error.setTransactionId(null);
        error.setErrorMessage(null);
        
        // When
        String result = error.getErrorSummary();
        
        // Then
        assertThat(result).contains("UNKNOWN");
        assertThat(result).contains("UNKNOWN_PROGRAM");
        assertThat(result).contains("UNKNOWN_TRANS");
        assertThat(result).contains("No message");
    }

    @Test
    @DisplayName("Should truncate long error messages in summary")
    void testGetErrorSummary_withLongMessage_truncatesMessage() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setErrorCode("ABND");
        error.setProgramName("TESTPROG");
        error.setTransactionId("TRAN123");
        String longMessage = "This is a very long error message that exceeds 100 characters and should be truncated by the getErrorSummary method to prevent overly long summaries";
        error.setErrorMessage(longMessage);
        
        // When
        String result = error.getErrorSummary();
        
        // Then
        assertThat(result).contains("ABND");
        assertThat(result).contains("TESTPROG");
        assertThat(result).contains("TRAN123");
        assertThat(result).contains(longMessage.substring(0, 100));
        assertThat(result).doesNotContain(longMessage.substring(100));
    }

    @Test
    @DisplayName("Should handle short error messages without truncation")
    void testGetErrorSummary_withShortMessage_doesNotTruncate() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setErrorCode("ABND");
        error.setProgramName("TESTPROG");
        error.setTransactionId("TRAN123");
        String shortMessage = "Short message";
        error.setErrorMessage(shortMessage);
        
        // When
        String result = error.getErrorSummary();
        
        // Then
        assertThat(result).contains(shortMessage);
    }

    @Test
    @DisplayName("Should test equals and hashCode consistency")
    void testEqualsAndHashCode_consistency() {
        // Given
        ApplicationError error1 = new ApplicationError();
        error1.setId(1L);
        error1.setErrorCode("ABND");
        error1.setApplicationId("TESTAPP");
        
        ApplicationError error2 = new ApplicationError();
        error2.setId(1L);
        error2.setErrorCode("ABND");
        error2.setApplicationId("TESTAPP");
        
        ApplicationError error3 = new ApplicationError();
        error3.setId(2L);
        error3.setErrorCode("INFO");
        error3.setApplicationId("OTHERAPP");
        
        // Then
        assertThat(error1).isEqualTo(error2);
        assertThat(error1).isNotEqualTo(error3);
        assertThat(error1.hashCode()).isEqualTo(error2.hashCode());
        assertThat(error1.hashCode()).isNotEqualTo(error3.hashCode());
    }

    @Test
    @DisplayName("Should test toString contains key information")
    void testToString_containsKeyInformation() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setId(1L);
        error.setErrorCode("ABND");
        error.setApplicationId("TESTAPP");
        error.setProgramName("TESTPROG");
        error.setErrorMessage("Test error");
        
        // When
        String result = error.toString();
        
        // Then
        assertThat(result).contains("ApplicationError");
        assertThat(result).contains("id=1");
        assertThat(result).contains("errorCode=ABND");
        assertThat(result).contains("applicationId=TESTAPP");
    }

    @Test
    @DisplayName("Should handle all field setters and getters")
    void testAllFieldsSettersAndGetters() {
        // Given
        ApplicationError error = new ApplicationError();
        
        // When
        error.setId(123L);
        error.setTimestamp("2023-12-25T10:30:00");
        error.setApplicationId("BANKAPP");
        error.setTransactionId("TX001");
        error.setErrorCode("ABND");
        error.setProgramName("PROGRAM1");
        error.setErrorMessage("Error occurred");
        error.setStackTrace("Stack trace content");
        error.setCreatedAt("2023-12-25T10:30:01");
        error.setResponseCode("RC01");
        error.setResponse2Code("RC02");
        error.setSqlCode("-803");
        error.setFreeformText("Additional info");
        
        // Then
        assertThat(error.getId()).isEqualTo(123L);
        assertThat(error.getTimestamp()).isEqualTo("2023-12-25T10:30:00");
        assertThat(error.getApplicationId()).isEqualTo("BANKAPP");
        assertThat(error.getTransactionId()).isEqualTo("TX001");
        assertThat(error.getErrorCode()).isEqualTo("ABND");
        assertThat(error.getProgramName()).isEqualTo("PROGRAM1");
        assertThat(error.getErrorMessage()).isEqualTo("Error occurred");
        assertThat(error.getStackTrace()).isEqualTo("Stack trace content");
        assertThat(error.getCreatedAt()).isEqualTo("2023-12-25T10:30:01");
        assertThat(error.getResponseCode()).isEqualTo("RC01");
        assertThat(error.getResponse2Code()).isEqualTo("RC02");
        assertThat(error.getSqlCode()).isEqualTo("-803");
        assertThat(error.getFreeformText()).isEqualTo("Additional info");
    }

    @Test
    @DisplayName("Should handle edge cases in isCritical method")
    void testIsCritical_edgeCases() {
        ApplicationError error = new ApplicationError();
        
        error.setErrorCode("asra");
        assertThat(error.isCritical()).isFalse();
        
        error.setErrorCode("abn");
        assertThat(error.isCritical()).isFalse();
        
        error.setErrorCode("ABNORMAL");
        assertThat(error.isCritical()).isTrue();
        
        error.setErrorCode("XABN");
        assertThat(error.isCritical()).isFalse();
    }

    @Test
    @DisplayName("Should handle edge cases in getErrorSummary method")
    void testGetErrorSummary_edgeCases() {
        ApplicationError error = new ApplicationError();
        
        String exactMessage = "1234567890".repeat(10);
        error.setErrorMessage(exactMessage);
        error.setErrorCode("TEST");
        error.setProgramName("PROG");
        error.setTransactionId("TX");
        
        String result = error.getErrorSummary();
        assertThat(result).contains(exactMessage);
        
        error.setErrorCode("");
        error.setProgramName("");
        error.setTransactionId("");
        error.setErrorMessage("");
        
        result = error.getErrorSummary();
        assertThat(result).contains("[]");
        assertThat(result).contains("in");
        assertThat(result).doesNotContain("UNKNOWN");
    }
}
