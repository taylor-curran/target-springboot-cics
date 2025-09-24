package com.cbsa.migration.service;

import com.cbsa.migration.dto.ErrorRequestDto;
import com.cbsa.migration.dto.ErrorResponseDto;
import com.cbsa.migration.model.ApplicationError;
import com.cbsa.migration.repository.ApplicationErrorRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Unit tests for ErrorLoggingService
 * Tests error logging and retrieval functionality
 */
@ExtendWith(MockitoExtension.class)
class ErrorLoggingServiceTest {

    @Mock
    private ApplicationErrorRepository applicationErrorRepository;

    @InjectMocks
    private ErrorLoggingService errorLoggingService;

    @Test
    @DisplayName("Should log error with valid request")
    void shouldLogErrorWithValidRequest() {
        // Given
        ErrorRequestDto request = new ErrorRequestDto();
        request.setApplicationId("TESTAPP");
        request.setTransactionId("TRAN001");
        request.setErrorCode("ABND");
        request.setProgramName("TESTPROG");
        request.setResponseCode("12");
        request.setResponse2Code("34");
        request.setSqlCode("-803");
        request.setFreeformText("Test error message");

        when(applicationErrorRepository.save(any(ApplicationError.class))).thenReturn(1L);

        // When
        ErrorResponseDto response = errorLoggingService.logError(request);

        // Then
        verify(applicationErrorRepository).save(any(ApplicationError.class));
        assertThat(response).isNotNull();
        assertThat(response.getErrorId()).isEqualTo(1L);
        assertThat(response.getStatus()).isEqualTo("SUCCESS");
        assertThat(response.getMessage()).isNotNull();
        assertThat(response.getTimestamp()).isNotNull();
    }

    @Test
    @DisplayName("Should return validation error for null request")
    void shouldReturnValidationErrorForNullRequest() {
        // When
        ErrorResponseDto response = errorLoggingService.logError((ErrorRequestDto) null);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getStatus()).isEqualTo("VALIDATION_ERROR");
        assertThat(response.getMessage()).contains("Error request cannot be null");
        assertThat(response.getErrorId()).isNull();
    }

    @Test
    @DisplayName("Should return validation error for missing program name")
    void shouldReturnValidationErrorForMissingProgramName() {
        // Given
        ErrorRequestDto request = new ErrorRequestDto();
        request.setApplicationId("TESTAPP");
        request.setErrorCode("ABND");

        // When
        ErrorResponseDto response = errorLoggingService.logError(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getStatus()).isEqualTo("VALIDATION_ERROR");
        assertThat(response.getMessage()).contains("Program name is required");
        assertThat(response.getErrorId()).isNull();
    }

    @Test
    @DisplayName("Should return validation error for program name too long")
    void shouldReturnValidationErrorForProgramNameTooLong() {
        // Given
        ErrorRequestDto request = new ErrorRequestDto();
        request.setApplicationId("TESTAPP");
        request.setErrorCode("ABND");
        request.setProgramName("VERYLONGPROGRAMNAME"); // More than 8 characters

        // When
        ErrorResponseDto response = errorLoggingService.logError(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getStatus()).isEqualTo("VALIDATION_ERROR");
        assertThat(response.getMessage()).contains("Program name must not exceed 8 characters");
        assertThat(response.getErrorId()).isNull();
    }

    @Test
    @DisplayName("Should log error from exception")
    void shouldLogErrorFromException() {
        // Given
        String programName = "TESTPROG";
        Exception exception = new RuntimeException("Test exception");

        when(applicationErrorRepository.save(any(ApplicationError.class))).thenReturn(2L);

        // When
        ErrorResponseDto response = errorLoggingService.logError(programName, exception);

        // Then
        verify(applicationErrorRepository).save(any(ApplicationError.class));
        assertThat(response).isNotNull();
        assertThat(response.getErrorId()).isEqualTo(2L);
        assertThat(response.getStatus()).isEqualTo("SUCCESS");
    }

    @Test
    @DisplayName("Should log error from exception with transaction context")
    void shouldLogErrorFromExceptionWithTransactionContext() {
        // Given
        String programName = "TESTPROG";
        String applicationId = "TESTAPP";
        String transactionId = "TRAN001";
        Exception exception = new RuntimeException("Test exception");

        when(applicationErrorRepository.save(any(ApplicationError.class))).thenReturn(3L);

        // When
        ErrorResponseDto response = errorLoggingService.logError(programName, applicationId, transactionId, exception);

        // Then
        verify(applicationErrorRepository).save(any(ApplicationError.class));
        assertThat(response).isNotNull();
        assertThat(response.getErrorId()).isEqualTo(3L);
        assertThat(response.getStatus()).isEqualTo("SUCCESS");
    }

    @Test
    @DisplayName("Should get error count by program name")
    void shouldGetErrorCountByProgramName() {
        // Given
        String programName = "TESTPROG";
        when(applicationErrorRepository.countByProgramName(programName)).thenReturn(5L);

        // When
        long errorCount = errorLoggingService.getErrorCount(programName);

        // Then
        assertThat(errorCount).isEqualTo(5L);
        verify(applicationErrorRepository).countByProgramName(programName);
    }

    @Test
    @DisplayName("Should get recent errors")
    void shouldGetRecentErrors() {
        // Given
        int limit = 10;
        ApplicationError error1 = new ApplicationError();
        ApplicationError error2 = new ApplicationError();
        List<ApplicationError> mockErrors = Arrays.asList(error1, error2);

        when(applicationErrorRepository.findRecentErrors(limit)).thenReturn(mockErrors);

        // When
        List<ApplicationError> recentErrors = errorLoggingService.getRecentErrors(limit);

        // Then
        assertThat(recentErrors).hasSize(2);
        assertThat(recentErrors).containsExactly(error1, error2);
        verify(applicationErrorRepository).findRecentErrors(limit);
    }

    @Test
    @DisplayName("Should get errors by program name")
    void shouldGetErrorsByProgramName() {
        // Given
        String programName = "TESTPROG";
        ApplicationError error1 = new ApplicationError();
        ApplicationError error2 = new ApplicationError();
        List<ApplicationError> mockErrors = Arrays.asList(error1, error2);

        when(applicationErrorRepository.findByProgramName(programName)).thenReturn(mockErrors);

        // When
        List<ApplicationError> programErrors = errorLoggingService.getErrorsByProgram(programName);

        // Then
        assertThat(programErrors).hasSize(2);
        assertThat(programErrors).containsExactly(error1, error2);
        verify(applicationErrorRepository).findByProgramName(programName);
    }

    @Test
    @DisplayName("Should handle repository save failure gracefully")
    void shouldHandleRepositorySaveFailureGracefully() {
        // Given
        ErrorRequestDto request = new ErrorRequestDto();
        request.setApplicationId("TESTAPP");
        request.setProgramName("TESTPROG");
        request.setErrorCode("ABND");

        when(applicationErrorRepository.save(any(ApplicationError.class)))
                .thenThrow(new RuntimeException("Database connection failed"));

        // When
        ErrorResponseDto response = errorLoggingService.logError(request);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getStatus()).isEqualTo("FAILURE");
        assertThat(response.getMessage()).contains("Failed to log error for program TESTPROG");
        assertThat(response.getErrorId()).isNull();
    }

    @Test
    @DisplayName("Should create application error with correct timestamp")
    void shouldCreateApplicationErrorWithCorrectTimestamp() {
        // Given
        ErrorRequestDto request = new ErrorRequestDto();
        request.setApplicationId("TESTAPP");
        request.setProgramName("TESTPROG");
        request.setErrorCode("ABND");
        request.setDate("2023-12-01");
        request.setTime("14:30:00");

        when(applicationErrorRepository.save(any(ApplicationError.class))).thenReturn(4L);

        // When
        ErrorResponseDto response = errorLoggingService.logError(request);

        // Then
        ArgumentCaptor<ApplicationError> errorCaptor = ArgumentCaptor.forClass(ApplicationError.class);
        verify(applicationErrorRepository).save(errorCaptor.capture());

        ApplicationError capturedError = errorCaptor.getValue();
        assertThat(capturedError.getTimestamp()).isEqualTo("2023-12-01 14:30:00");
        assertThat(response.getStatus()).isEqualTo("SUCCESS");
    }
}
