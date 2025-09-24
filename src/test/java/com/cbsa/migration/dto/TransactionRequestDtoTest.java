package com.cbsa.migration.dto;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.ValidatorFactory;
import java.math.BigDecimal;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for TransactionRequestDto
 * Tests the Transaction request DTO structure, validation, and functionality
 */
class TransactionRequestDtoTest {

    private final ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
    private final Validator validator = factory.getValidator();

    @Test
    @DisplayName("Should create TransactionRequestDto with no-args constructor")
    void testNoArgsConstructor_createsValidObject() {
        // When
        TransactionRequestDto dto = new TransactionRequestDto();
        
        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getSortCode()).isNull();
        assertThat(dto.getAccountNumber()).isNull();
        assertThat(dto.getTransactionType()).isNull();
        assertThat(dto.getDescription()).isNull();
        assertThat(dto.getAmount()).isNull();
        assertThat(dto.getTargetSortCode()).isNull();
        assertThat(dto.getTargetAccountNumber()).isNull();
    }

    @Test
    @DisplayName("Should create TransactionRequestDto with all-args constructor")
    void testAllArgsConstructor_setsAllProperties() {
        // Given
        String sortCode = "123456";
        String accountNumber = "12345678";
        String transactionType = "TFR";
        String description = "Transfer to savings";
        BigDecimal amount = new BigDecimal("100.00");
        String targetSortCode = "654321";
        String targetAccountNumber = "87654321";
        
        // When
        TransactionRequestDto dto = new TransactionRequestDto(
            sortCode, accountNumber, transactionType, description, amount,
            targetSortCode, targetAccountNumber
        );
        
        // Then
        assertThat(dto.getSortCode()).isEqualTo(sortCode);
        assertThat(dto.getAccountNumber()).isEqualTo(accountNumber);
        assertThat(dto.getTransactionType()).isEqualTo(transactionType);
        assertThat(dto.getDescription()).isEqualTo(description);
        assertThat(dto.getAmount()).isEqualTo(amount);
        assertThat(dto.getTargetSortCode()).isEqualTo(targetSortCode);
        assertThat(dto.getTargetAccountNumber()).isEqualTo(targetAccountNumber);
    }

    @Test
    @DisplayName("Should create TransactionRequestDto with builder pattern")
    void testBuilder_setsAllProperties() {
        // Given
        String sortCode = "111111";
        String accountNumber = "11111111";
        String transactionType = "CRE";
        String description = "Salary deposit";
        BigDecimal amount = new BigDecimal("2500.50");
        
        // When
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode(sortCode)
            .accountNumber(accountNumber)
            .transactionType(transactionType)
            .description(description)
            .amount(amount)
            .build();
        
        // Then
        assertThat(dto.getSortCode()).isEqualTo(sortCode);
        assertThat(dto.getAccountNumber()).isEqualTo(accountNumber);
        assertThat(dto.getTransactionType()).isEqualTo(transactionType);
        assertThat(dto.getDescription()).isEqualTo(description);
        assertThat(dto.getAmount()).isEqualTo(amount);
        assertThat(dto.getTargetSortCode()).isNull();
        assertThat(dto.getTargetAccountNumber()).isNull();
    }

    @Test
    @DisplayName("Should test all field setters and getters")
    void testAllFieldsSettersAndGetters() {
        // Given
        TransactionRequestDto dto = new TransactionRequestDto();
        String sortCode = "222222";
        String accountNumber = "22222222";
        String transactionType = "DEB";
        String description = "ATM withdrawal";
        BigDecimal amount = new BigDecimal("50.00");
        String targetSortCode = "333333";
        String targetAccountNumber = "33333333";
        
        // When
        dto.setSortCode(sortCode);
        dto.setAccountNumber(accountNumber);
        dto.setTransactionType(transactionType);
        dto.setDescription(description);
        dto.setAmount(amount);
        dto.setTargetSortCode(targetSortCode);
        dto.setTargetAccountNumber(targetAccountNumber);
        
        // Then
        assertThat(dto.getSortCode()).isEqualTo(sortCode);
        assertThat(dto.getAccountNumber()).isEqualTo(accountNumber);
        assertThat(dto.getTransactionType()).isEqualTo(transactionType);
        assertThat(dto.getDescription()).isEqualTo(description);
        assertThat(dto.getAmount()).isEqualTo(amount);
        assertThat(dto.getTargetSortCode()).isEqualTo(targetSortCode);
        assertThat(dto.getTargetAccountNumber()).isEqualTo(targetAccountNumber);
    }

    @Test
    @DisplayName("Should validate required fields - valid request")
    void testValidation_validRequest_passesValidation() {
        // Given
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("CRE")
            .description("Valid transaction")
            .amount(new BigDecimal("100.00"))
            .build();
        
        // When
        Set<ConstraintViolation<TransactionRequestDto>> violations = validator.validate(dto);
        
        // Then
        assertThat(violations).isEmpty();
    }

    @Test
    @DisplayName("Should validate sort code - blank sort code fails validation")
    void testValidation_blankSortCode_failsValidation() {
        // Given
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("")
            .accountNumber("12345678")
            .transactionType("CRE")
            .description("Valid transaction")
            .amount(new BigDecimal("100.00"))
            .build();
        
        // When
        Set<ConstraintViolation<TransactionRequestDto>> violations = validator.validate(dto);
        
        // Then
        assertThat(violations).hasSize(2);
        assertThat(violations).anyMatch(v -> v.getMessage().contains("Sort code is required"));
        assertThat(violations).anyMatch(v -> v.getMessage().contains("Sort code must be exactly 6 characters"));
    }

    @Test
    @DisplayName("Should validate sort code - invalid length fails validation")
    void testValidation_invalidSortCodeLength_failsValidation() {
        // Given
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("12345")
            .accountNumber("12345678")
            .transactionType("CRE")
            .description("Valid transaction")
            .amount(new BigDecimal("100.00"))
            .build();
        
        // When
        Set<ConstraintViolation<TransactionRequestDto>> violations = validator.validate(dto);
        
        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations).anyMatch(v -> v.getMessage().contains("Sort code must be exactly 6 characters"));
    }

    @Test
    @DisplayName("Should validate account number - blank account number fails validation")
    void testValidation_blankAccountNumber_failsValidation() {
        // Given
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("")
            .transactionType("CRE")
            .description("Valid transaction")
            .amount(new BigDecimal("100.00"))
            .build();
        
        // When
        Set<ConstraintViolation<TransactionRequestDto>> violations = validator.validate(dto);
        
        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations).anyMatch(v -> v.getMessage().contains("Account number is required"));
    }

    @Test
    @DisplayName("Should validate account number - too long account number fails validation")
    void testValidation_tooLongAccountNumber_failsValidation() {
        // Given
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("123456789")
            .transactionType("CRE")
            .description("Valid transaction")
            .amount(new BigDecimal("100.00"))
            .build();
        
        // When
        Set<ConstraintViolation<TransactionRequestDto>> violations = validator.validate(dto);
        
        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations).anyMatch(v -> v.getMessage().contains("Account number must not exceed 8 characters"));
    }

    @Test
    @DisplayName("Should validate transaction type - blank transaction type fails validation")
    void testValidation_blankTransactionType_failsValidation() {
        // Given
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("")
            .description("Valid transaction")
            .amount(new BigDecimal("100.00"))
            .build();
        
        // When
        Set<ConstraintViolation<TransactionRequestDto>> violations = validator.validate(dto);
        
        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations).anyMatch(v -> v.getMessage().contains("Transaction type is required"));
    }

    @Test
    @DisplayName("Should validate transaction type - too long transaction type fails validation")
    void testValidation_tooLongTransactionType_failsValidation() {
        // Given
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("CREDIT")
            .description("Valid transaction")
            .amount(new BigDecimal("100.00"))
            .build();
        
        // When
        Set<ConstraintViolation<TransactionRequestDto>> violations = validator.validate(dto);
        
        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations).anyMatch(v -> v.getMessage().contains("Transaction type must not exceed 3 characters"));
    }

    @Test
    @DisplayName("Should validate description - blank description fails validation")
    void testValidation_blankDescription_failsValidation() {
        // Given
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("CRE")
            .description("")
            .amount(new BigDecimal("100.00"))
            .build();
        
        // When
        Set<ConstraintViolation<TransactionRequestDto>> violations = validator.validate(dto);
        
        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations).anyMatch(v -> v.getMessage().contains("Transaction description is required"));
    }

    @Test
    @DisplayName("Should validate description - too long description fails validation")
    void testValidation_tooLongDescription_failsValidation() {
        // Given
        String longDescription = "This is a very long transaction description that exceeds the maximum allowed length";
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("CRE")
            .description(longDescription)
            .amount(new BigDecimal("100.00"))
            .build();
        
        // When
        Set<ConstraintViolation<TransactionRequestDto>> violations = validator.validate(dto);
        
        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations).anyMatch(v -> v.getMessage().contains("Transaction description must not exceed 35 characters"));
    }

    @Test
    @DisplayName("Should validate amount - null amount fails validation")
    void testValidation_nullAmount_failsValidation() {
        // Given
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("CRE")
            .description("Valid transaction")
            .amount(null)
            .build();
        
        // When
        Set<ConstraintViolation<TransactionRequestDto>> violations = validator.validate(dto);
        
        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations).anyMatch(v -> v.getMessage().contains("Transaction amount is required"));
    }

    @Test
    @DisplayName("Should validate amount - zero amount fails validation")
    void testValidation_zeroAmount_failsValidation() {
        // Given
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("CRE")
            .description("Valid transaction")
            .amount(BigDecimal.ZERO)
            .build();
        
        // When
        Set<ConstraintViolation<TransactionRequestDto>> violations = validator.validate(dto);
        
        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations).anyMatch(v -> v.getMessage().contains("Transaction amount must be greater than 0"));
    }

    @Test
    @DisplayName("Should validate target sort code - invalid length fails validation")
    void testValidation_invalidTargetSortCodeLength_failsValidation() {
        // Given
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("TFR")
            .description("Transfer transaction")
            .amount(new BigDecimal("100.00"))
            .targetSortCode("12345")
            .targetAccountNumber("87654321")
            .build();
        
        // When
        Set<ConstraintViolation<TransactionRequestDto>> violations = validator.validate(dto);
        
        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations).anyMatch(v -> v.getMessage().contains("Target sort code must be exactly 6 characters"));
    }

    @Test
    @DisplayName("Should validate target account number - too long target account number fails validation")
    void testValidation_tooLongTargetAccountNumber_failsValidation() {
        // Given
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("TFR")
            .description("Transfer transaction")
            .amount(new BigDecimal("100.00"))
            .targetSortCode("654321")
            .targetAccountNumber("876543210")
            .build();
        
        // When
        Set<ConstraintViolation<TransactionRequestDto>> violations = validator.validate(dto);
        
        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations).anyMatch(v -> v.getMessage().contains("Target account number must not exceed 8 characters"));
    }

    @Test
    @DisplayName("Should test equals and hashCode consistency")
    void testEqualsAndHashCode_consistency() {
        // Given
        TransactionRequestDto dto1 = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("CRE")
            .description("Test transaction")
            .amount(new BigDecimal("100.00"))
            .build();
        
        TransactionRequestDto dto2 = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("CRE")
            .description("Test transaction")
            .amount(new BigDecimal("100.00"))
            .build();
        
        TransactionRequestDto dto3 = TransactionRequestDto.builder()
            .sortCode("654321")
            .accountNumber("87654321")
            .transactionType("DEB")
            .description("Different transaction")
            .amount(new BigDecimal("200.00"))
            .build();
        
        // Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1).isNotEqualTo(dto3);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
        assertThat(dto1.hashCode()).isNotEqualTo(dto3.hashCode());
    }

    @Test
    @DisplayName("Should test toString contains key information")
    void testToString_containsKeyInformation() {
        // Given
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("CRE")
            .description("Test transaction")
            .amount(new BigDecimal("100.00"))
            .targetSortCode("654321")
            .targetAccountNumber("87654321")
            .build();
        
        // When
        String result = dto.toString();
        
        // Then
        assertThat(result).contains("TransactionRequestDto");
        assertThat(result).contains("sortCode=123456");
        assertThat(result).contains("accountNumber=12345678");
        assertThat(result).contains("transactionType=CRE");
        assertThat(result).contains("description=Test transaction");
        assertThat(result).contains("amount=100.00");
        assertThat(result).contains("targetSortCode=654321");
        assertThat(result).contains("targetAccountNumber=87654321");
    }

    @Test
    @DisplayName("Should handle various transaction types")
    void testVariousTransactionTypes_handledCorrectly() {
        // Given
        TransactionRequestDto dto = new TransactionRequestDto();
        
        dto.setTransactionType("CRE");
        assertThat(dto.getTransactionType()).isEqualTo("CRE");
        
        dto.setTransactionType("DEB");
        assertThat(dto.getTransactionType()).isEqualTo("DEB");
        
        dto.setTransactionType("TFR");
        assertThat(dto.getTransactionType()).isEqualTo("TFR");
        
        dto.setTransactionType("FEE");
        assertThat(dto.getTransactionType()).isEqualTo("FEE");
        
        dto.setTransactionType("INT");
        assertThat(dto.getTransactionType()).isEqualTo("INT");
    }

    @Test
    @DisplayName("Should handle edge case amounts")
    void testEdgeCaseAmounts_handledCorrectly() {
        // Given
        TransactionRequestDto dto = new TransactionRequestDto();
        BigDecimal minAmount = new BigDecimal("0.01");
        BigDecimal maxAmount = new BigDecimal("999999999.99");
        BigDecimal preciseAmount = new BigDecimal("123.456789");
        
        // When
        dto.setAmount(minAmount);
        assertThat(dto.getAmount()).isEqualTo(minAmount);
        
        dto.setAmount(maxAmount);
        assertThat(dto.getAmount()).isEqualTo(maxAmount);
        
        dto.setAmount(preciseAmount);
        assertThat(dto.getAmount()).isEqualTo(preciseAmount);
    }

    @Test
    @DisplayName("Should test builder with partial fields for transfer")
    void testBuilder_withPartialFieldsForTransfer() {
        // When
        TransactionRequestDto dto = TransactionRequestDto.builder()
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("TFR")
            .description("Transfer transaction")
            .amount(new BigDecimal("500.00"))
            .targetSortCode("654321")
            .build();
        
        // Then
        assertThat(dto.getSortCode()).isEqualTo("123456");
        assertThat(dto.getAccountNumber()).isEqualTo("12345678");
        assertThat(dto.getTransactionType()).isEqualTo("TFR");
        assertThat(dto.getDescription()).isEqualTo("Transfer transaction");
        assertThat(dto.getAmount()).isEqualTo(new BigDecimal("500.00"));
        assertThat(dto.getTargetSortCode()).isEqualTo("654321");
        assertThat(dto.getTargetAccountNumber()).isNull();
    }
}
