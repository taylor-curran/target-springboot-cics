package com.cbsa.migration.dto;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for TransactionResponseDto
 * Tests the Transaction response DTO structure and functionality
 */
class TransactionResponseDtoTest {

    @Test
    @DisplayName("Should create TransactionResponseDto with no-args constructor")
    void testNoArgsConstructor_createsValidObject() {
        // When
        TransactionResponseDto dto = new TransactionResponseDto();
        
        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getReferenceNumber()).isNull();
        assertThat(dto.getSortCode()).isNull();
        assertThat(dto.getAccountNumber()).isNull();
        assertThat(dto.getTransactionDate()).isNull();
        assertThat(dto.getTransactionTime()).isNull();
        assertThat(dto.getTransactionType()).isNull();
        assertThat(dto.getDescription()).isNull();
        assertThat(dto.getAmount()).isNull();
        assertThat(dto.getTargetSortCode()).isNull();
        assertThat(dto.getTargetAccountNumber()).isNull();
        assertThat(dto.getIsDebit()).isNull();
        assertThat(dto.getStatus()).isNull();
        assertThat(dto.getRunningBalance()).isNull();
    }

    @Test
    @DisplayName("Should create TransactionResponseDto with all-args constructor")
    void testAllArgsConstructor_setsAllProperties() {
        // Given
        Long referenceNumber = 1000001L;
        String sortCode = "123456";
        String accountNumber = "12345678";
        LocalDate transactionDate = LocalDate.of(2024, 9, 24);
        LocalTime transactionTime = LocalTime.of(14, 30, 45);
        String transactionType = "CRE";
        String description = "Salary deposit";
        BigDecimal amount = new BigDecimal("2500.00");
        String targetSortCode = "654321";
        String targetAccountNumber = "87654321";
        Boolean isDebit = false;
        String status = "COMPLETED";
        BigDecimal runningBalance = new BigDecimal("3500.00");
        
        // When
        TransactionResponseDto dto = new TransactionResponseDto(
            referenceNumber, sortCode, accountNumber, transactionDate, transactionTime,
            transactionType, description, amount, targetSortCode, targetAccountNumber,
            isDebit, status, runningBalance
        );
        
        // Then
        assertThat(dto.getReferenceNumber()).isEqualTo(referenceNumber);
        assertThat(dto.getSortCode()).isEqualTo(sortCode);
        assertThat(dto.getAccountNumber()).isEqualTo(accountNumber);
        assertThat(dto.getTransactionDate()).isEqualTo(transactionDate);
        assertThat(dto.getTransactionTime()).isEqualTo(transactionTime);
        assertThat(dto.getTransactionType()).isEqualTo(transactionType);
        assertThat(dto.getDescription()).isEqualTo(description);
        assertThat(dto.getAmount()).isEqualTo(amount);
        assertThat(dto.getTargetSortCode()).isEqualTo(targetSortCode);
        assertThat(dto.getTargetAccountNumber()).isEqualTo(targetAccountNumber);
        assertThat(dto.getIsDebit()).isEqualTo(isDebit);
        assertThat(dto.getStatus()).isEqualTo(status);
        assertThat(dto.getRunningBalance()).isEqualTo(runningBalance);
    }

    @Test
    @DisplayName("Should create TransactionResponseDto with builder pattern")
    void testBuilder_setsAllProperties() {
        // Given
        Long referenceNumber = 2000002L;
        String sortCode = "111111";
        String accountNumber = "11111111";
        LocalDate transactionDate = LocalDate.of(2024, 9, 23);
        LocalTime transactionTime = LocalTime.of(10, 15, 30);
        String transactionType = "DEB";
        String description = "ATM withdrawal";
        BigDecimal amount = new BigDecimal("100.00");
        Boolean isDebit = true;
        String status = "COMPLETED";
        BigDecimal runningBalance = new BigDecimal("1400.00");
        
        // When
        TransactionResponseDto dto = TransactionResponseDto.builder()
            .referenceNumber(referenceNumber)
            .sortCode(sortCode)
            .accountNumber(accountNumber)
            .transactionDate(transactionDate)
            .transactionTime(transactionTime)
            .transactionType(transactionType)
            .description(description)
            .amount(amount)
            .isDebit(isDebit)
            .status(status)
            .runningBalance(runningBalance)
            .build();
        
        // Then
        assertThat(dto.getReferenceNumber()).isEqualTo(referenceNumber);
        assertThat(dto.getSortCode()).isEqualTo(sortCode);
        assertThat(dto.getAccountNumber()).isEqualTo(accountNumber);
        assertThat(dto.getTransactionDate()).isEqualTo(transactionDate);
        assertThat(dto.getTransactionTime()).isEqualTo(transactionTime);
        assertThat(dto.getTransactionType()).isEqualTo(transactionType);
        assertThat(dto.getDescription()).isEqualTo(description);
        assertThat(dto.getAmount()).isEqualTo(amount);
        assertThat(dto.getTargetSortCode()).isNull();
        assertThat(dto.getTargetAccountNumber()).isNull();
        assertThat(dto.getIsDebit()).isEqualTo(isDebit);
        assertThat(dto.getStatus()).isEqualTo(status);
        assertThat(dto.getRunningBalance()).isEqualTo(runningBalance);
    }

    @Test
    @DisplayName("Should test all field setters and getters")
    void testAllFieldsSettersAndGetters() {
        // Given
        TransactionResponseDto dto = new TransactionResponseDto();
        Long referenceNumber = 3000003L;
        String sortCode = "222222";
        String accountNumber = "22222222";
        LocalDate transactionDate = LocalDate.of(2024, 9, 22);
        LocalTime transactionTime = LocalTime.of(16, 45, 15);
        String transactionType = "TFR";
        String description = "Transfer to savings";
        BigDecimal amount = new BigDecimal("500.00");
        String targetSortCode = "333333";
        String targetAccountNumber = "33333333";
        Boolean isDebit = true;
        String status = "PENDING";
        BigDecimal runningBalance = new BigDecimal("2000.00");
        
        // When
        dto.setReferenceNumber(referenceNumber);
        dto.setSortCode(sortCode);
        dto.setAccountNumber(accountNumber);
        dto.setTransactionDate(transactionDate);
        dto.setTransactionTime(transactionTime);
        dto.setTransactionType(transactionType);
        dto.setDescription(description);
        dto.setAmount(amount);
        dto.setTargetSortCode(targetSortCode);
        dto.setTargetAccountNumber(targetAccountNumber);
        dto.setIsDebit(isDebit);
        dto.setStatus(status);
        dto.setRunningBalance(runningBalance);
        
        // Then
        assertThat(dto.getReferenceNumber()).isEqualTo(referenceNumber);
        assertThat(dto.getSortCode()).isEqualTo(sortCode);
        assertThat(dto.getAccountNumber()).isEqualTo(accountNumber);
        assertThat(dto.getTransactionDate()).isEqualTo(transactionDate);
        assertThat(dto.getTransactionTime()).isEqualTo(transactionTime);
        assertThat(dto.getTransactionType()).isEqualTo(transactionType);
        assertThat(dto.getDescription()).isEqualTo(description);
        assertThat(dto.getAmount()).isEqualTo(amount);
        assertThat(dto.getTargetSortCode()).isEqualTo(targetSortCode);
        assertThat(dto.getTargetAccountNumber()).isEqualTo(targetAccountNumber);
        assertThat(dto.getIsDebit()).isEqualTo(isDebit);
        assertThat(dto.getStatus()).isEqualTo(status);
        assertThat(dto.getRunningBalance()).isEqualTo(runningBalance);
    }

    @Test
    @DisplayName("Should test equals and hashCode consistency")
    void testEqualsAndHashCode_consistency() {
        // Given
        TransactionResponseDto dto1 = TransactionResponseDto.builder()
            .referenceNumber(1000001L)
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionDate(LocalDate.of(2024, 9, 24))
            .transactionTime(LocalTime.of(14, 30, 45))
            .transactionType("CRE")
            .description("Test transaction")
            .amount(new BigDecimal("100.00"))
            .isDebit(false)
            .status("COMPLETED")
            .runningBalance(new BigDecimal("1500.00"))
            .build();
        
        TransactionResponseDto dto2 = TransactionResponseDto.builder()
            .referenceNumber(1000001L)
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionDate(LocalDate.of(2024, 9, 24))
            .transactionTime(LocalTime.of(14, 30, 45))
            .transactionType("CRE")
            .description("Test transaction")
            .amount(new BigDecimal("100.00"))
            .isDebit(false)
            .status("COMPLETED")
            .runningBalance(new BigDecimal("1500.00"))
            .build();
        
        TransactionResponseDto dto3 = TransactionResponseDto.builder()
            .referenceNumber(2000002L)
            .sortCode("654321")
            .accountNumber("87654321")
            .transactionDate(LocalDate.of(2024, 9, 23))
            .transactionTime(LocalTime.of(10, 15, 30))
            .transactionType("DEB")
            .description("Different transaction")
            .amount(new BigDecimal("200.00"))
            .isDebit(true)
            .status("PENDING")
            .runningBalance(new BigDecimal("800.00"))
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
        TransactionResponseDto dto = TransactionResponseDto.builder()
            .referenceNumber(1000001L)
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionDate(LocalDate.of(2024, 9, 24))
            .transactionTime(LocalTime.of(14, 30, 45))
            .transactionType("CRE")
            .description("Test transaction")
            .amount(new BigDecimal("100.00"))
            .targetSortCode("654321")
            .targetAccountNumber("87654321")
            .isDebit(false)
            .status("COMPLETED")
            .runningBalance(new BigDecimal("1500.00"))
            .build();
        
        // When
        String result = dto.toString();
        
        // Then
        assertThat(result).contains("TransactionResponseDto");
        assertThat(result).contains("referenceNumber=1000001");
        assertThat(result).contains("sortCode=123456");
        assertThat(result).contains("accountNumber=12345678");
        assertThat(result).contains("transactionDate=2024-09-24");
        assertThat(result).contains("transactionTime=14:30:45");
        assertThat(result).contains("transactionType=CRE");
        assertThat(result).contains("description=Test transaction");
        assertThat(result).contains("amount=100.00");
        assertThat(result).contains("targetSortCode=654321");
        assertThat(result).contains("targetAccountNumber=87654321");
        assertThat(result).contains("isDebit=false");
        assertThat(result).contains("status=COMPLETED");
        assertThat(result).contains("runningBalance=1500.00");
    }

    @Test
    @DisplayName("Should handle null values in fields")
    void testNullValues_handledCorrectly() {
        // Given
        TransactionResponseDto dto = new TransactionResponseDto();
        
        // When
        dto.setReferenceNumber(null);
        dto.setSortCode(null);
        dto.setAccountNumber(null);
        dto.setTransactionDate(null);
        dto.setTransactionTime(null);
        dto.setTransactionType(null);
        dto.setDescription(null);
        dto.setAmount(null);
        dto.setTargetSortCode(null);
        dto.setTargetAccountNumber(null);
        dto.setIsDebit(null);
        dto.setStatus(null);
        dto.setRunningBalance(null);
        
        // Then
        assertThat(dto.getReferenceNumber()).isNull();
        assertThat(dto.getSortCode()).isNull();
        assertThat(dto.getAccountNumber()).isNull();
        assertThat(dto.getTransactionDate()).isNull();
        assertThat(dto.getTransactionTime()).isNull();
        assertThat(dto.getTransactionType()).isNull();
        assertThat(dto.getDescription()).isNull();
        assertThat(dto.getAmount()).isNull();
        assertThat(dto.getTargetSortCode()).isNull();
        assertThat(dto.getTargetAccountNumber()).isNull();
        assertThat(dto.getIsDebit()).isNull();
        assertThat(dto.getStatus()).isNull();
        assertThat(dto.getRunningBalance()).isNull();
    }

    @Test
    @DisplayName("Should handle edge case values")
    void testEdgeCaseValues_handledCorrectly() {
        // Given
        TransactionResponseDto dto = new TransactionResponseDto();
        Long maxReferenceNumber = Long.MAX_VALUE;
        Long minReferenceNumber = 1L;
        BigDecimal maxAmount = new BigDecimal("999999999.99");
        BigDecimal minAmount = new BigDecimal("0.01");
        BigDecimal negativeAmount = new BigDecimal("-500.00");
        LocalDate pastDate = LocalDate.of(1900, 1, 1);
        LocalDate futureDate = LocalDate.of(2100, 12, 31);
        LocalTime earlyTime = LocalTime.of(0, 0, 0);
        LocalTime lateTime = LocalTime.of(23, 59, 59);
        
        // When
        dto.setReferenceNumber(maxReferenceNumber);
        dto.setAmount(maxAmount);
        dto.setRunningBalance(negativeAmount);
        dto.setTransactionDate(pastDate);
        dto.setTransactionTime(earlyTime);
        
        // Then
        assertThat(dto.getReferenceNumber()).isEqualTo(maxReferenceNumber);
        assertThat(dto.getAmount()).isEqualTo(maxAmount);
        assertThat(dto.getRunningBalance()).isEqualTo(negativeAmount);
        assertThat(dto.getTransactionDate()).isEqualTo(pastDate);
        assertThat(dto.getTransactionTime()).isEqualTo(earlyTime);
        
        // When
        dto.setReferenceNumber(minReferenceNumber);
        dto.setAmount(minAmount);
        dto.setTransactionDate(futureDate);
        dto.setTransactionTime(lateTime);
        
        // Then
        assertThat(dto.getReferenceNumber()).isEqualTo(minReferenceNumber);
        assertThat(dto.getAmount()).isEqualTo(minAmount);
        assertThat(dto.getTransactionDate()).isEqualTo(futureDate);
        assertThat(dto.getTransactionTime()).isEqualTo(lateTime);
    }

    @Test
    @DisplayName("Should test builder with partial fields")
    void testBuilder_withPartialFields() {
        // When
        TransactionResponseDto dto1 = TransactionResponseDto.builder()
            .referenceNumber(1000001L)
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("CRE")
            .amount(new BigDecimal("100.00"))
            .build();
        
        TransactionResponseDto dto2 = TransactionResponseDto.builder()
            .transactionDate(LocalDate.of(2024, 9, 24))
            .transactionTime(LocalTime.of(14, 30, 45))
            .description("Test transaction")
            .isDebit(false)
            .status("COMPLETED")
            .runningBalance(new BigDecimal("1500.00"))
            .build();
        
        // Then
        assertThat(dto1.getReferenceNumber()).isEqualTo(1000001L);
        assertThat(dto1.getSortCode()).isEqualTo("123456");
        assertThat(dto1.getAccountNumber()).isEqualTo("12345678");
        assertThat(dto1.getTransactionType()).isEqualTo("CRE");
        assertThat(dto1.getAmount()).isEqualTo(new BigDecimal("100.00"));
        assertThat(dto1.getTransactionDate()).isNull();
        assertThat(dto1.getTransactionTime()).isNull();
        assertThat(dto1.getDescription()).isNull();
        
        assertThat(dto2.getReferenceNumber()).isNull();
        assertThat(dto2.getSortCode()).isNull();
        assertThat(dto2.getAccountNumber()).isNull();
        assertThat(dto2.getTransactionDate()).isEqualTo(LocalDate.of(2024, 9, 24));
        assertThat(dto2.getTransactionTime()).isEqualTo(LocalTime.of(14, 30, 45));
        assertThat(dto2.getDescription()).isEqualTo("Test transaction");
        assertThat(dto2.getIsDebit()).isEqualTo(false);
        assertThat(dto2.getStatus()).isEqualTo("COMPLETED");
        assertThat(dto2.getRunningBalance()).isEqualTo(new BigDecimal("1500.00"));
    }

    @Test
    @DisplayName("Should handle various transaction types and statuses")
    void testVariousTransactionTypesAndStatuses_handledCorrectly() {
        // Given
        TransactionResponseDto dto = new TransactionResponseDto();
        
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
        
        dto.setStatus("COMPLETED");
        assertThat(dto.getStatus()).isEqualTo("COMPLETED");
        
        dto.setStatus("PENDING");
        assertThat(dto.getStatus()).isEqualTo("PENDING");
        
        dto.setStatus("FAILED");
        assertThat(dto.getStatus()).isEqualTo("FAILED");
        
        dto.setStatus("CANCELLED");
        assertThat(dto.getStatus()).isEqualTo("CANCELLED");
        
        dto.setStatus("PROCESSING");
        assertThat(dto.getStatus()).isEqualTo("PROCESSING");
    }

    @Test
    @DisplayName("Should handle debit and credit transactions")
    void testDebitAndCreditTransactions_handledCorrectly() {
        // Given
        TransactionResponseDto creditDto = new TransactionResponseDto();
        TransactionResponseDto debitDto = new TransactionResponseDto();
        
        // When
        creditDto.setIsDebit(false);
        creditDto.setTransactionType("CRE");
        creditDto.setAmount(new BigDecimal("500.00"));
        
        debitDto.setIsDebit(true);
        debitDto.setTransactionType("DEB");
        debitDto.setAmount(new BigDecimal("100.00"));
        
        // Then
        assertThat(creditDto.getIsDebit()).isFalse();
        assertThat(creditDto.getTransactionType()).isEqualTo("CRE");
        assertThat(creditDto.getAmount()).isEqualTo(new BigDecimal("500.00"));
        
        assertThat(debitDto.getIsDebit()).isTrue();
        assertThat(debitDto.getTransactionType()).isEqualTo("DEB");
        assertThat(debitDto.getAmount()).isEqualTo(new BigDecimal("100.00"));
    }

    @Test
    @DisplayName("Should handle transfer transactions with target details")
    void testTransferTransactions_withTargetDetails() {
        // Given
        TransactionResponseDto dto = TransactionResponseDto.builder()
            .referenceNumber(1000001L)
            .sortCode("123456")
            .accountNumber("12345678")
            .transactionType("TFR")
            .description("Transfer to savings")
            .amount(new BigDecimal("500.00"))
            .targetSortCode("654321")
            .targetAccountNumber("87654321")
            .isDebit(true)
            .status("COMPLETED")
            .runningBalance(new BigDecimal("1500.00"))
            .build();
        
        // Then
        assertThat(dto.getTransactionType()).isEqualTo("TFR");
        assertThat(dto.getTargetSortCode()).isEqualTo("654321");
        assertThat(dto.getTargetAccountNumber()).isEqualTo("87654321");
        assertThat(dto.getIsDebit()).isTrue();
        assertThat(dto.getDescription()).contains("Transfer");
    }

    @Test
    @DisplayName("Should handle running balance calculations")
    void testRunningBalanceCalculations_handledCorrectly() {
        // Given
        TransactionResponseDto dto = new TransactionResponseDto();
        BigDecimal initialBalance = new BigDecimal("1000.00");
        BigDecimal positiveBalance = new BigDecimal("1500.00");
        BigDecimal negativeBalance = new BigDecimal("-200.00");
        BigDecimal zeroBalance = BigDecimal.ZERO;
        
        // When & Then
        dto.setRunningBalance(initialBalance);
        assertThat(dto.getRunningBalance()).isEqualTo(initialBalance);
        
        dto.setRunningBalance(positiveBalance);
        assertThat(dto.getRunningBalance()).isEqualTo(positiveBalance);
        
        dto.setRunningBalance(negativeBalance);
        assertThat(dto.getRunningBalance()).isEqualTo(negativeBalance);
        
        dto.setRunningBalance(zeroBalance);
        assertThat(dto.getRunningBalance()).isEqualTo(zeroBalance);
    }
}
