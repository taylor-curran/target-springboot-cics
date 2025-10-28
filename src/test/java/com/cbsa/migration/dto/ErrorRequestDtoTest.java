package com.cbsa.migration.dto;

import org.junit.jupiter.api.Test;

import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.ValidatorFactory;
import javax.validation.ConstraintViolation;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for ErrorRequestDto validation and functionality.
 */
class ErrorRequestDtoTest {

    private final ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
    private final Validator validator = factory.getValidator();

    @Test
    void testValidErrorRequest_passesValidation() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TESTPROG");
        dto.setApplicationId("TESTAPP");
        dto.setTransactionId("T001");
        dto.setErrorCode("ABND");
        dto.setFreeformText("Test error message");

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).isEmpty();
    }

    @Test
    void testMissingProgramName_failsValidation() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setApplicationId("TESTAPP");
        dto.setFreeformText("Test error");

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Program name is required");
    }

    @Test
    void testBlankProgramName_failsValidation() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("");
        dto.setApplicationId("TESTAPP");
        dto.setFreeformText("Test error");

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Program name is required");
    }

    @Test
    void testProgramNameTooLong_failsValidation() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TOOLONGPROGRAMNAME"); // 18 characters, max is 8
        dto.setApplicationId("TESTAPP");
        dto.setFreeformText("Test error");

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Program name must not exceed 8 characters");
    }

    @Test
    void testTransactionIdTooLong_failsValidation() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TESTPROG");
        dto.setTransactionId("TOOLONG"); // 7 characters, max is 4
        dto.setFreeformText("Test error");

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Transaction ID must not exceed 4 characters");
    }

    @Test
    void testFreeformTextTooLong_failsValidation() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TESTPROG");
        dto.setFreeformText("A".repeat(601)); // 601 characters, max is 600

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Freeform text must not exceed 600 characters");
    }

    @Test
    void testFromException_createsValidDto() {
        // Given
        String programName = "TESTPROG";
        Exception exception = new RuntimeException("Test exception message");

        // When
        ErrorRequestDto dto = ErrorRequestDto.fromException(programName, exception);

        // Then
        assertThat(dto.getProgramName()).isEqualTo("TESTPROG");
        assertThat(dto.getErrorCode()).isEqualTo("JAVA");
        assertThat(dto.getErrorMessage()).isEqualTo("Test exception message");
        assertThat(dto.getStackTrace()).isNotNull();
        assertThat(dto.getStackTrace()).contains("RuntimeException: Test exception message");
    }

    @Test
    void testFromExceptionWithContext_includesAllFields() {
        // Given
        String programName = "TESTPROG";
        String applicationId = "TESTAPP";
        String transactionId = "T001";
        Exception exception = new RuntimeException("Test exception message");

        // When
        ErrorRequestDto dto = ErrorRequestDto.fromException(programName, applicationId, transactionId, exception);

        // Then
        assertThat(dto.getProgramName()).isEqualTo("TESTPROG");
        assertThat(dto.getApplicationId()).isEqualTo("TESTAPP");
        assertThat(dto.getTransactionId()).isEqualTo("T001");
        assertThat(dto.getErrorCode()).isEqualTo("JAVA");
        assertThat(dto.getErrorMessage()).isEqualTo("Test exception message");
        assertThat(dto.getStackTrace()).isNotNull();
    }

    @Test
    void testFromExceptionWithNullMessage_handlesGracefully() {
        // Given
        String programName = "TESTPROG";
        Exception exception = new RuntimeException((String) null);

        // When
        ErrorRequestDto dto = ErrorRequestDto.fromException(programName, exception);

        // Then
        assertThat(dto.getProgramName()).isEqualTo("TESTPROG");
        assertThat(dto.getErrorCode()).isEqualTo("JAVA");
        assertThat(dto.getErrorMessage()).isNull();
        assertThat(dto.getStackTrace()).isNotNull();
    }

    @Test
    void testLombokMethods_workCorrectly() {
        // Given
        ErrorRequestDto dto1 = new ErrorRequestDto();
        dto1.setProgramName("TESTPROG");
        dto1.setApplicationId("TESTAPP");

        ErrorRequestDto dto2 = new ErrorRequestDto();
        dto2.setProgramName("TESTPROG");
        dto2.setApplicationId("TESTAPP");

        // When & Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
        assertThat(dto1.toString()).contains("TESTPROG");
        assertThat(dto1.toString()).contains("TESTAPP");
    }

    @Test
    void testEquals_comprehensiveScenarios() {
        ErrorRequestDto base = new ErrorRequestDto();
        base.setProgramName("TESTPROG");
        base.setApplicationId("TESTAPP");
        base.setTransactionId("T001");
        base.setDate("2023-01-01");
        base.setTime("12:00:00");
        base.setErrorCode("ABND");
        base.setErrorMessage("Test error");
        base.setStackTrace("Stack trace");
        base.setResponseCode("RC01");
        base.setResponse2Code("RC02");
        base.setSqlCode("SQL001");
        base.setFreeformText("Freeform text");

        assertThat(base).isEqualTo(base);
        assertThat(base.equals(base)).isTrue();

        assertThat(base.equals(null)).isFalse();

        assertThat(base.equals("not an ErrorRequestDto")).isFalse();
        assertThat(base.equals(new Object())).isFalse();

        ErrorRequestDto identical = new ErrorRequestDto();
        identical.setProgramName("TESTPROG");
        identical.setApplicationId("TESTAPP");
        identical.setTransactionId("T001");
        identical.setDate("2023-01-01");
        identical.setTime("12:00:00");
        identical.setErrorCode("ABND");
        identical.setErrorMessage("Test error");
        identical.setStackTrace("Stack trace");
        identical.setResponseCode("RC01");
        identical.setResponse2Code("RC02");
        identical.setSqlCode("SQL001");
        identical.setFreeformText("Freeform text");
        
        assertThat(base).isEqualTo(identical);
        assertThat(base.hashCode()).isEqualTo(identical.hashCode());

        ErrorRequestDto diffProgramName = new ErrorRequestDto();
        diffProgramName.setProgramName("DIFFERENT");
        diffProgramName.setApplicationId("TESTAPP");
        assertThat(base).isNotEqualTo(diffProgramName);

        ErrorRequestDto diffApplicationId = new ErrorRequestDto();
        diffApplicationId.setProgramName("TESTPROG");
        diffApplicationId.setApplicationId("DIFFERENT");
        assertThat(base).isNotEqualTo(diffApplicationId);

        ErrorRequestDto diffTransactionId = new ErrorRequestDto();
        diffTransactionId.setProgramName("TESTPROG");
        diffTransactionId.setApplicationId("TESTAPP");
        diffTransactionId.setTransactionId("DIFF");
        assertThat(base).isNotEqualTo(diffTransactionId);

        ErrorRequestDto diffDate = new ErrorRequestDto();
        diffDate.setProgramName("TESTPROG");
        diffDate.setApplicationId("TESTAPP");
        diffDate.setTransactionId("T001");
        diffDate.setDate("2023-12-31");
        assertThat(base).isNotEqualTo(diffDate);

        ErrorRequestDto diffTime = new ErrorRequestDto();
        diffTime.setProgramName("TESTPROG");
        diffTime.setApplicationId("TESTAPP");
        diffTime.setTransactionId("T001");
        diffTime.setDate("2023-01-01");
        diffTime.setTime("23:59:59");
        assertThat(base).isNotEqualTo(diffTime);

        ErrorRequestDto diffErrorCode = new ErrorRequestDto();
        diffErrorCode.setProgramName("TESTPROG");
        diffErrorCode.setApplicationId("TESTAPP");
        diffErrorCode.setTransactionId("T001");
        diffErrorCode.setDate("2023-01-01");
        diffErrorCode.setTime("12:00:00");
        diffErrorCode.setErrorCode("DIFF");
        assertThat(base).isNotEqualTo(diffErrorCode);

        ErrorRequestDto diffErrorMessage = new ErrorRequestDto();
        diffErrorMessage.setProgramName("TESTPROG");
        diffErrorMessage.setApplicationId("TESTAPP");
        diffErrorMessage.setTransactionId("T001");
        diffErrorMessage.setDate("2023-01-01");
        diffErrorMessage.setTime("12:00:00");
        diffErrorMessage.setErrorCode("ABND");
        diffErrorMessage.setErrorMessage("Different error");
        assertThat(base).isNotEqualTo(diffErrorMessage);

        ErrorRequestDto diffStackTrace = new ErrorRequestDto();
        diffStackTrace.setProgramName("TESTPROG");
        diffStackTrace.setApplicationId("TESTAPP");
        diffStackTrace.setTransactionId("T001");
        diffStackTrace.setDate("2023-01-01");
        diffStackTrace.setTime("12:00:00");
        diffStackTrace.setErrorCode("ABND");
        diffStackTrace.setErrorMessage("Test error");
        diffStackTrace.setStackTrace("Different stack trace");
        assertThat(base).isNotEqualTo(diffStackTrace);

        ErrorRequestDto diffResponseCode = new ErrorRequestDto();
        diffResponseCode.setProgramName("TESTPROG");
        diffResponseCode.setApplicationId("TESTAPP");
        diffResponseCode.setTransactionId("T001");
        diffResponseCode.setDate("2023-01-01");
        diffResponseCode.setTime("12:00:00");
        diffResponseCode.setErrorCode("ABND");
        diffResponseCode.setErrorMessage("Test error");
        diffResponseCode.setStackTrace("Stack trace");
        diffResponseCode.setResponseCode("DIFF");
        assertThat(base).isNotEqualTo(diffResponseCode);

        ErrorRequestDto diffResponse2Code = new ErrorRequestDto();
        diffResponse2Code.setProgramName("TESTPROG");
        diffResponse2Code.setApplicationId("TESTAPP");
        diffResponse2Code.setTransactionId("T001");
        diffResponse2Code.setDate("2023-01-01");
        diffResponse2Code.setTime("12:00:00");
        diffResponse2Code.setErrorCode("ABND");
        diffResponse2Code.setErrorMessage("Test error");
        diffResponse2Code.setStackTrace("Stack trace");
        diffResponse2Code.setResponseCode("RC01");
        diffResponse2Code.setResponse2Code("DIFF");
        assertThat(base).isNotEqualTo(diffResponse2Code);

        ErrorRequestDto diffSqlCode = new ErrorRequestDto();
        diffSqlCode.setProgramName("TESTPROG");
        diffSqlCode.setApplicationId("TESTAPP");
        diffSqlCode.setTransactionId("T001");
        diffSqlCode.setDate("2023-01-01");
        diffSqlCode.setTime("12:00:00");
        diffSqlCode.setErrorCode("ABND");
        diffSqlCode.setErrorMessage("Test error");
        diffSqlCode.setStackTrace("Stack trace");
        diffSqlCode.setResponseCode("RC01");
        diffSqlCode.setResponse2Code("RC02");
        diffSqlCode.setSqlCode("DIFF");
        assertThat(base).isNotEqualTo(diffSqlCode);

        ErrorRequestDto diffFreeformText = new ErrorRequestDto();
        diffFreeformText.setProgramName("TESTPROG");
        diffFreeformText.setApplicationId("TESTAPP");
        diffFreeformText.setTransactionId("T001");
        diffFreeformText.setDate("2023-01-01");
        diffFreeformText.setTime("12:00:00");
        diffFreeformText.setErrorCode("ABND");
        diffFreeformText.setErrorMessage("Test error");
        diffFreeformText.setStackTrace("Stack trace");
        diffFreeformText.setResponseCode("RC01");
        diffFreeformText.setResponse2Code("RC02");
        diffFreeformText.setSqlCode("SQL001");
        diffFreeformText.setFreeformText("Different freeform text");
        assertThat(base).isNotEqualTo(diffFreeformText);
    }

    @Test
    void testEquals_withNullFields() {
        ErrorRequestDto dto1 = new ErrorRequestDto();
        dto1.setProgramName("TESTPROG");
        dto1.setApplicationId(null);
        dto1.setTransactionId(null);

        ErrorRequestDto dto2 = new ErrorRequestDto();
        dto2.setProgramName("TESTPROG");
        dto2.setApplicationId(null);
        dto2.setTransactionId(null);

        ErrorRequestDto dto3 = new ErrorRequestDto();
        dto3.setProgramName("TESTPROG");
        dto3.setApplicationId("TESTAPP");
        dto3.setTransactionId(null);

        // When & Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
        assertThat(dto1).isNotEqualTo(dto3);
    }

    @Test
    void testHashCode_consistency() {
        // Given
        ErrorRequestDto dto1 = new ErrorRequestDto();
        dto1.setProgramName("TESTPROG");
        dto1.setApplicationId("TESTAPP");
        dto1.setErrorCode("ABND");

        ErrorRequestDto dto2 = new ErrorRequestDto();
        dto2.setProgramName("TESTPROG");
        dto2.setApplicationId("TESTAPP");
        dto2.setErrorCode("ABND");

        ErrorRequestDto dto3 = new ErrorRequestDto();
        dto3.setProgramName("DIFFERENT");
        dto3.setApplicationId("TESTAPP");
        dto3.setErrorCode("ABND");

        // When & Then - Equal objects must have equal hash codes
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
        
        assertThat(dto1.hashCode()).isNotEqualTo(dto3.hashCode());

        int hashCode1 = dto1.hashCode();
        int hashCode2 = dto1.hashCode();
        assertThat(hashCode1).isEqualTo(hashCode2);
    }

    @Test
    void testHashCode_withNullFields() {
        // Given
        ErrorRequestDto dtoWithNulls = new ErrorRequestDto();
        dtoWithNulls.setProgramName("TESTPROG");
        dtoWithNulls.setApplicationId(null);
        dtoWithNulls.setTransactionId(null);
        dtoWithNulls.setErrorMessage(null);

        ErrorRequestDto dtoWithValues = new ErrorRequestDto();
        dtoWithValues.setProgramName("TESTPROG");
        dtoWithValues.setApplicationId("TESTAPP");
        dtoWithValues.setTransactionId("T001");
        dtoWithValues.setErrorMessage("Error message");

        // When & Then - Should not throw NPE and should be consistent
        int hashWithNulls = dtoWithNulls.hashCode();
        int hashWithValues = dtoWithValues.hashCode();
        
        assertThat(hashWithNulls).isNotEqualTo(hashWithValues);
        
        assertThat(dtoWithNulls.hashCode()).isEqualTo(hashWithNulls);
        assertThat(dtoWithValues.hashCode()).isEqualTo(hashWithValues);
    }

    @Test
    void testAllArgsConstructor_createsValidDto() {
        ErrorRequestDto dto = new ErrorRequestDto(
            "TESTPROG", "TESTAPP", "T001", "2023-01-01", "12:00:00",
            "ABND", "Test error", "Stack trace", "RC01", "RC02", "SQL001", "Freeform text"
        );

        // Then
        assertThat(dto.getProgramName()).isEqualTo("TESTPROG");
        assertThat(dto.getApplicationId()).isEqualTo("TESTAPP");
        assertThat(dto.getTransactionId()).isEqualTo("T001");
        assertThat(dto.getDate()).isEqualTo("2023-01-01");
        assertThat(dto.getTime()).isEqualTo("12:00:00");
        assertThat(dto.getErrorCode()).isEqualTo("ABND");
        assertThat(dto.getErrorMessage()).isEqualTo("Test error");
        assertThat(dto.getStackTrace()).isEqualTo("Stack trace");
        assertThat(dto.getResponseCode()).isEqualTo("RC01");
        assertThat(dto.getResponse2Code()).isEqualTo("RC02");
        assertThat(dto.getSqlCode()).isEqualTo("SQL001");
        assertThat(dto.getFreeformText()).isEqualTo("Freeform text");
    }

    @Test
    void testDateValidation_failsWhenTooLong() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TESTPROG");
        dto.setDate("2023-01-01-TOOLONG"); // 17 characters, max is 10

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Date must not exceed 10 characters");
    }

    @Test
    void testTimeValidation_failsWhenTooLong() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TESTPROG");
        dto.setTime("12:00:00:TOOLONG"); // 15 characters, max is 8

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Time must not exceed 8 characters");
    }

    @Test
    void testErrorCodeValidation_failsWhenTooLong() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TESTPROG");
        dto.setErrorCode("TOOLONG"); // 7 characters, max is 4

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Error code must not exceed 4 characters");
    }

    @Test
    void testErrorMessageValidation_failsWhenTooLong() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TESTPROG");
        dto.setErrorMessage("A".repeat(501)); // 501 characters, max is 500

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Error message must not exceed 500 characters");
    }

    @Test
    void testStackTraceValidation_failsWhenTooLong() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TESTPROG");
        dto.setStackTrace("A".repeat(2001)); // 2001 characters, max is 2000

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Stack trace must not exceed 2000 characters");
    }

    @Test
    void testApplicationIdValidation_failsWhenTooLong() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TESTPROG");
        dto.setApplicationId("TOOLONGAPP"); // 10 characters, max is 8

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Application ID must not exceed 8 characters");
    }

    @Test
    void testFromExceptionWithExceptionMessage_includesMessage() {
        // Given
        String programName = "TESTPROG";
        Exception exception = new IllegalArgumentException("Invalid argument provided");

        // When
        ErrorRequestDto dto = ErrorRequestDto.fromException(programName, exception);

        // Then
        assertThat(dto.getProgramName()).isEqualTo("TESTPROG");
        assertThat(dto.getErrorCode()).isEqualTo("JAVA");
        assertThat(dto.getErrorMessage()).isEqualTo("Invalid argument provided");
        assertThat(dto.getStackTrace()).contains("IllegalArgumentException: Invalid argument provided");
    }

    @Test
    void testFromExceptionWithLongStackTrace_truncatesCorrectly() {
        // Given
        String programName = "TESTPROG";
        Exception deepException = createDeepStackTrace(15);

        // When
        ErrorRequestDto dto = ErrorRequestDto.fromException(programName, deepException);

        // Then
        assertThat(dto.getProgramName()).isEqualTo("TESTPROG");
        assertThat(dto.getErrorCode()).isEqualTo("JAVA");
        assertThat(dto.getStackTrace()).contains("RuntimeException:");
        assertThat(dto.getStackTrace()).contains("... 72 more"); // Based on actual output
    }

    @Test
    void testGettersAndSetters_workCorrectly() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();

        // When
        dto.setDate("2023-01-01");
        dto.setTime("12:00:00");
        dto.setResponseCode("RC01");
        dto.setResponse2Code("RC02");
        dto.setSqlCode("SQL001");

        // Then
        assertThat(dto.getDate()).isEqualTo("2023-01-01");
        assertThat(dto.getTime()).isEqualTo("12:00:00");
        assertThat(dto.getResponseCode()).isEqualTo("RC01");
        assertThat(dto.getResponse2Code()).isEqualTo("RC02");
        assertThat(dto.getSqlCode()).isEqualTo("SQL001");
    }

    @Test
    void testCompleteValidDto_passesAllValidation() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TESTPROG");
        dto.setApplicationId("TESTAPP");
        dto.setTransactionId("T001");
        dto.setDate("2023-01-01");
        dto.setTime("12:00:00");
        dto.setErrorCode("ABND");
        dto.setErrorMessage("Test error message");
        dto.setStackTrace("Stack trace");
        dto.setResponseCode("RC01");
        dto.setResponse2Code("RC02");
        dto.setSqlCode("SQL001");
        dto.setFreeformText("Additional information");

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).isEmpty();
    }

    private Exception createDeepStackTrace(int depth) {
        if (depth <= 0) {
            return new RuntimeException("Deep stack trace test");
        }
        try {
            return createDeepStackTrace(depth - 1);
        } catch (Exception e) {
            throw new RuntimeException("Level " + depth, e);
        }
    }
}
