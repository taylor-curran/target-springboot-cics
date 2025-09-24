package com.cbsa.migration.dto;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for AccountResponseDto
 * Tests the Account response DTO structure and functionality
 */
class AccountResponseDtoTest {

    @Test
    @DisplayName("Should create AccountResponseDto with no-args constructor")
    void testNoArgsConstructor_createsValidObject() {
        // When
        AccountResponseDto dto = new AccountResponseDto();
        
        // Then
        assertThat(dto).isNotNull();
        assertThat(dto.getAccountNumber()).isNull();
        assertThat(dto.getSortCode()).isNull();
        assertThat(dto.getAccountType()).isNull();
        assertThat(dto.getCustomerNumber()).isNull();
        assertThat(dto.getCustomerName()).isNull();
        assertThat(dto.getAvailableBalance()).isNull();
        assertThat(dto.getActualBalance()).isNull();
        assertThat(dto.getInterestRate()).isNull();
        assertThat(dto.getOpenedDate()).isNull();
        assertThat(dto.getOverdraftLimit()).isNull();
        assertThat(dto.getLastStatementDate()).isNull();
        assertThat(dto.getNextStatementDate()).isNull();
        assertThat(dto.getStatus()).isNull();
    }

    @Test
    @DisplayName("Should create AccountResponseDto with all-args constructor")
    void testAllArgsConstructor_setsAllProperties() {
        // Given
        String accountNumber = "12345678";
        String sortCode = "123456";
        String accountType = "CURRENT";
        Long customerNumber = 1000001L;
        String customerName = "John Doe";
        BigDecimal availableBalance = new BigDecimal("1500.00");
        BigDecimal actualBalance = new BigDecimal("1500.00");
        BigDecimal interestRate = new BigDecimal("2.5");
        LocalDate openedDate = LocalDate.of(2023, 1, 15);
        Integer overdraftLimit = 500;
        LocalDate lastStatementDate = LocalDate.of(2024, 8, 31);
        LocalDate nextStatementDate = LocalDate.of(2024, 9, 30);
        String status = "ACTIVE";
        
        // When
        AccountResponseDto dto = new AccountResponseDto(
            accountNumber, sortCode, accountType, customerNumber, customerName,
            availableBalance, actualBalance, interestRate, openedDate, overdraftLimit,
            lastStatementDate, nextStatementDate, status
        );
        
        // Then
        assertThat(dto.getAccountNumber()).isEqualTo(accountNumber);
        assertThat(dto.getSortCode()).isEqualTo(sortCode);
        assertThat(dto.getAccountType()).isEqualTo(accountType);
        assertThat(dto.getCustomerNumber()).isEqualTo(customerNumber);
        assertThat(dto.getCustomerName()).isEqualTo(customerName);
        assertThat(dto.getAvailableBalance()).isEqualTo(availableBalance);
        assertThat(dto.getActualBalance()).isEqualTo(actualBalance);
        assertThat(dto.getInterestRate()).isEqualTo(interestRate);
        assertThat(dto.getOpenedDate()).isEqualTo(openedDate);
        assertThat(dto.getOverdraftLimit()).isEqualTo(overdraftLimit);
        assertThat(dto.getLastStatementDate()).isEqualTo(lastStatementDate);
        assertThat(dto.getNextStatementDate()).isEqualTo(nextStatementDate);
        assertThat(dto.getStatus()).isEqualTo(status);
    }

    @Test
    @DisplayName("Should create AccountResponseDto with builder pattern")
    void testBuilder_setsAllProperties() {
        // Given
        String accountNumber = "87654321";
        String sortCode = "654321";
        String accountType = "SAVINGS";
        Long customerNumber = 2000002L;
        String customerName = "Jane Smith";
        BigDecimal availableBalance = new BigDecimal("2500.75");
        BigDecimal actualBalance = new BigDecimal("2500.75");
        BigDecimal interestRate = new BigDecimal("3.0");
        LocalDate openedDate = LocalDate.of(2022, 6, 10);
        Integer overdraftLimit = 1000;
        LocalDate lastStatementDate = LocalDate.of(2024, 8, 31);
        LocalDate nextStatementDate = LocalDate.of(2024, 9, 30);
        String status = "ACTIVE";
        
        // When
        AccountResponseDto dto = AccountResponseDto.builder()
            .accountNumber(accountNumber)
            .sortCode(sortCode)
            .accountType(accountType)
            .customerNumber(customerNumber)
            .customerName(customerName)
            .availableBalance(availableBalance)
            .actualBalance(actualBalance)
            .interestRate(interestRate)
            .openedDate(openedDate)
            .overdraftLimit(overdraftLimit)
            .lastStatementDate(lastStatementDate)
            .nextStatementDate(nextStatementDate)
            .status(status)
            .build();
        
        // Then
        assertThat(dto.getAccountNumber()).isEqualTo(accountNumber);
        assertThat(dto.getSortCode()).isEqualTo(sortCode);
        assertThat(dto.getAccountType()).isEqualTo(accountType);
        assertThat(dto.getCustomerNumber()).isEqualTo(customerNumber);
        assertThat(dto.getCustomerName()).isEqualTo(customerName);
        assertThat(dto.getAvailableBalance()).isEqualTo(availableBalance);
        assertThat(dto.getActualBalance()).isEqualTo(actualBalance);
        assertThat(dto.getInterestRate()).isEqualTo(interestRate);
        assertThat(dto.getOpenedDate()).isEqualTo(openedDate);
        assertThat(dto.getOverdraftLimit()).isEqualTo(overdraftLimit);
        assertThat(dto.getLastStatementDate()).isEqualTo(lastStatementDate);
        assertThat(dto.getNextStatementDate()).isEqualTo(nextStatementDate);
        assertThat(dto.getStatus()).isEqualTo(status);
    }

    @Test
    @DisplayName("Should test all field setters and getters")
    void testAllFieldsSettersAndGetters() {
        // Given
        AccountResponseDto dto = new AccountResponseDto();
        String accountNumber = "11111111";
        String sortCode = "111111";
        String accountType = "BUSINESS";
        Long customerNumber = 3000003L;
        String customerName = "Business Corp";
        BigDecimal availableBalance = new BigDecimal("10000.00");
        BigDecimal actualBalance = new BigDecimal("9500.00");
        BigDecimal interestRate = new BigDecimal("1.5");
        LocalDate openedDate = LocalDate.of(2021, 3, 20);
        Integer overdraftLimit = 2000;
        LocalDate lastStatementDate = LocalDate.of(2024, 7, 31);
        LocalDate nextStatementDate = LocalDate.of(2024, 8, 31);
        String status = "OVERDRAWN";
        
        // When
        dto.setAccountNumber(accountNumber);
        dto.setSortCode(sortCode);
        dto.setAccountType(accountType);
        dto.setCustomerNumber(customerNumber);
        dto.setCustomerName(customerName);
        dto.setAvailableBalance(availableBalance);
        dto.setActualBalance(actualBalance);
        dto.setInterestRate(interestRate);
        dto.setOpenedDate(openedDate);
        dto.setOverdraftLimit(overdraftLimit);
        dto.setLastStatementDate(lastStatementDate);
        dto.setNextStatementDate(nextStatementDate);
        dto.setStatus(status);
        
        // Then
        assertThat(dto.getAccountNumber()).isEqualTo(accountNumber);
        assertThat(dto.getSortCode()).isEqualTo(sortCode);
        assertThat(dto.getAccountType()).isEqualTo(accountType);
        assertThat(dto.getCustomerNumber()).isEqualTo(customerNumber);
        assertThat(dto.getCustomerName()).isEqualTo(customerName);
        assertThat(dto.getAvailableBalance()).isEqualTo(availableBalance);
        assertThat(dto.getActualBalance()).isEqualTo(actualBalance);
        assertThat(dto.getInterestRate()).isEqualTo(interestRate);
        assertThat(dto.getOpenedDate()).isEqualTo(openedDate);
        assertThat(dto.getOverdraftLimit()).isEqualTo(overdraftLimit);
        assertThat(dto.getLastStatementDate()).isEqualTo(lastStatementDate);
        assertThat(dto.getNextStatementDate()).isEqualTo(nextStatementDate);
        assertThat(dto.getStatus()).isEqualTo(status);
    }

    @Test
    @DisplayName("Should test equals and hashCode consistency")
    void testEqualsAndHashCode_consistency() {
        // Given
        AccountResponseDto dto1 = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode("123456")
            .accountType("CURRENT")
            .customerNumber(1000001L)
            .customerName("John Doe")
            .availableBalance(new BigDecimal("1500.00"))
            .actualBalance(new BigDecimal("1500.00"))
            .interestRate(new BigDecimal("2.5"))
            .openedDate(LocalDate.of(2023, 1, 15))
            .overdraftLimit(500)
            .status("ACTIVE")
            .build();
        
        AccountResponseDto dto2 = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode("123456")
            .accountType("CURRENT")
            .customerNumber(1000001L)
            .customerName("John Doe")
            .availableBalance(new BigDecimal("1500.00"))
            .actualBalance(new BigDecimal("1500.00"))
            .interestRate(new BigDecimal("2.5"))
            .openedDate(LocalDate.of(2023, 1, 15))
            .overdraftLimit(500)
            .status("ACTIVE")
            .build();
        
        AccountResponseDto dto3 = AccountResponseDto.builder()
            .accountNumber("87654321")
            .sortCode("654321")
            .accountType("SAVINGS")
            .customerNumber(2000002L)
            .customerName("Jane Smith")
            .availableBalance(new BigDecimal("2500.75"))
            .actualBalance(new BigDecimal("2500.75"))
            .interestRate(new BigDecimal("3.0"))
            .openedDate(LocalDate.of(2022, 6, 10))
            .overdraftLimit(1000)
            .status("ACTIVE")
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
        AccountResponseDto dto = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode("123456")
            .accountType("CURRENT")
            .customerNumber(1000001L)
            .customerName("John Doe")
            .availableBalance(new BigDecimal("1500.00"))
            .actualBalance(new BigDecimal("1500.00"))
            .interestRate(new BigDecimal("2.5"))
            .openedDate(LocalDate.of(2023, 1, 15))
            .overdraftLimit(500)
            .status("ACTIVE")
            .build();
        
        // When
        String result = dto.toString();
        
        // Then
        assertThat(result).contains("AccountResponseDto");
        assertThat(result).contains("accountNumber=12345678");
        assertThat(result).contains("sortCode=123456");
        assertThat(result).contains("accountType=CURRENT");
        assertThat(result).contains("customerNumber=1000001");
        assertThat(result).contains("customerName=John Doe");
        assertThat(result).contains("availableBalance=1500.00");
        assertThat(result).contains("actualBalance=1500.00");
        assertThat(result).contains("interestRate=2.5");
        assertThat(result).contains("overdraftLimit=500");
        assertThat(result).contains("status=ACTIVE");
    }

    @Test
    @DisplayName("Should handle null values in fields")
    void testNullValues_handledCorrectly() {
        // Given
        AccountResponseDto dto = new AccountResponseDto();
        
        // When
        dto.setAccountNumber(null);
        dto.setSortCode(null);
        dto.setAccountType(null);
        dto.setCustomerNumber(null);
        dto.setCustomerName(null);
        dto.setAvailableBalance(null);
        dto.setActualBalance(null);
        dto.setInterestRate(null);
        dto.setOpenedDate(null);
        dto.setOverdraftLimit(null);
        dto.setLastStatementDate(null);
        dto.setNextStatementDate(null);
        dto.setStatus(null);
        
        // Then
        assertThat(dto.getAccountNumber()).isNull();
        assertThat(dto.getSortCode()).isNull();
        assertThat(dto.getAccountType()).isNull();
        assertThat(dto.getCustomerNumber()).isNull();
        assertThat(dto.getCustomerName()).isNull();
        assertThat(dto.getAvailableBalance()).isNull();
        assertThat(dto.getActualBalance()).isNull();
        assertThat(dto.getInterestRate()).isNull();
        assertThat(dto.getOpenedDate()).isNull();
        assertThat(dto.getOverdraftLimit()).isNull();
        assertThat(dto.getLastStatementDate()).isNull();
        assertThat(dto.getNextStatementDate()).isNull();
        assertThat(dto.getStatus()).isNull();
    }

    @Test
    @DisplayName("Should handle edge case values")
    void testEdgeCaseValues_handledCorrectly() {
        // Given
        AccountResponseDto dto = new AccountResponseDto();
        BigDecimal maxBalance = new BigDecimal("999999999.99");
        BigDecimal minBalance = new BigDecimal("0.01");
        BigDecimal zeroBalance = BigDecimal.ZERO;
        Integer maxOverdraft = Integer.MAX_VALUE;
        Integer zeroOverdraft = 0;
        LocalDate pastDate = LocalDate.of(1900, 1, 1);
        LocalDate futureDate = LocalDate.of(2100, 12, 31);
        
        // When
        dto.setAvailableBalance(maxBalance);
        dto.setActualBalance(minBalance);
        dto.setInterestRate(zeroBalance);
        dto.setOverdraftLimit(maxOverdraft);
        dto.setOpenedDate(pastDate);
        dto.setLastStatementDate(futureDate);
        
        // Then
        assertThat(dto.getAvailableBalance()).isEqualTo(maxBalance);
        assertThat(dto.getActualBalance()).isEqualTo(minBalance);
        assertThat(dto.getInterestRate()).isEqualTo(zeroBalance);
        assertThat(dto.getOverdraftLimit()).isEqualTo(maxOverdraft);
        assertThat(dto.getOpenedDate()).isEqualTo(pastDate);
        assertThat(dto.getLastStatementDate()).isEqualTo(futureDate);
    }

    @Test
    @DisplayName("Should test builder with partial fields")
    void testBuilder_withPartialFields() {
        // When
        AccountResponseDto dto1 = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode("123456")
            .accountType("CURRENT")
            .build();
        
        AccountResponseDto dto2 = AccountResponseDto.builder()
            .customerNumber(1000001L)
            .customerName("John Doe")
            .availableBalance(new BigDecimal("1500.00"))
            .build();
        
        // Then
        assertThat(dto1.getAccountNumber()).isEqualTo("12345678");
        assertThat(dto1.getSortCode()).isEqualTo("123456");
        assertThat(dto1.getAccountType()).isEqualTo("CURRENT");
        assertThat(dto1.getCustomerNumber()).isNull();
        assertThat(dto1.getCustomerName()).isNull();
        assertThat(dto1.getAvailableBalance()).isNull();
        
        assertThat(dto2.getAccountNumber()).isNull();
        assertThat(dto2.getSortCode()).isNull();
        assertThat(dto2.getAccountType()).isNull();
        assertThat(dto2.getCustomerNumber()).isEqualTo(1000001L);
        assertThat(dto2.getCustomerName()).isEqualTo("John Doe");
        assertThat(dto2.getAvailableBalance()).isEqualTo(new BigDecimal("1500.00"));
    }

    @Test
    @DisplayName("Should handle various account types and statuses")
    void testVariousAccountTypesAndStatuses_handledCorrectly() {
        // Given
        AccountResponseDto dto = new AccountResponseDto();
        
        dto.setAccountType("CURRENT");
        assertThat(dto.getAccountType()).isEqualTo("CURRENT");
        
        dto.setAccountType("SAVINGS");
        assertThat(dto.getAccountType()).isEqualTo("SAVINGS");
        
        dto.setAccountType("BUSINESS");
        assertThat(dto.getAccountType()).isEqualTo("BUSINESS");
        
        dto.setAccountType("LOAN");
        assertThat(dto.getAccountType()).isEqualTo("LOAN");
        
        dto.setStatus("ACTIVE");
        assertThat(dto.getStatus()).isEqualTo("ACTIVE");
        
        dto.setStatus("INACTIVE");
        assertThat(dto.getStatus()).isEqualTo("INACTIVE");
        
        dto.setStatus("CLOSED");
        assertThat(dto.getStatus()).isEqualTo("CLOSED");
        
        dto.setStatus("OVERDRAWN");
        assertThat(dto.getStatus()).isEqualTo("OVERDRAWN");
        
        dto.setStatus("SUSPENDED");
        assertThat(dto.getStatus()).isEqualTo("SUSPENDED");
    }

    @Test
    @DisplayName("Should handle negative balances and overdrafts")
    void testNegativeBalancesAndOverdrafts_handledCorrectly() {
        // Given
        AccountResponseDto dto = new AccountResponseDto();
        BigDecimal negativeBalance = new BigDecimal("-500.00");
        BigDecimal negativeActualBalance = new BigDecimal("-750.25");
        Integer negativeOverdraft = -100;
        
        // When
        dto.setAvailableBalance(negativeBalance);
        dto.setActualBalance(negativeActualBalance);
        dto.setOverdraftLimit(negativeOverdraft);
        
        // Then
        assertThat(dto.getAvailableBalance()).isEqualTo(negativeBalance);
        assertThat(dto.getActualBalance()).isEqualTo(negativeActualBalance);
        assertThat(dto.getOverdraftLimit()).isEqualTo(negativeOverdraft);
    }

    @Test
    @DisplayName("Should test equals with null object")
    void testEquals_withNullObject() {
        // Given
        AccountResponseDto dto = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode("123456")
            .build();
        
        // Then
        assertThat(dto.equals(null)).isFalse();
    }

    @Test
    @DisplayName("Should test equals with same object")
    void testEquals_withSameObject() {
        // Given
        AccountResponseDto dto = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode("123456")
            .build();
        
        // Then
        assertThat(dto.equals(dto)).isTrue();
    }

    @Test
    @DisplayName("Should test equals with different class")
    void testEquals_withDifferentClass() {
        // Given
        AccountResponseDto dto = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode("123456")
            .build();
        
        // Then
        assertThat(dto.equals("not an AccountResponseDto")).isFalse();
    }

    @Test
    @DisplayName("Should test equals with different field values")
    void testEquals_withDifferentFieldValues() {
        // Given
        AccountResponseDto dto1 = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode("123456")
            .accountType("CURRENT")
            .customerNumber(1000001L)
            .customerName("John Doe")
            .availableBalance(new BigDecimal("1500.00"))
            .actualBalance(new BigDecimal("1500.00"))
            .interestRate(new BigDecimal("2.5"))
            .openedDate(LocalDate.of(2023, 1, 15))
            .overdraftLimit(500)
            .lastStatementDate(LocalDate.of(2024, 8, 31))
            .nextStatementDate(LocalDate.of(2024, 9, 30))
            .status("ACTIVE")
            .build();
        
        AccountResponseDto dto2 = AccountResponseDto.builder()
            .accountNumber("87654321")
            .sortCode("123456")
            .accountType("CURRENT")
            .customerNumber(1000001L)
            .customerName("John Doe")
            .availableBalance(new BigDecimal("1500.00"))
            .actualBalance(new BigDecimal("1500.00"))
            .interestRate(new BigDecimal("2.5"))
            .openedDate(LocalDate.of(2023, 1, 15))
            .overdraftLimit(500)
            .lastStatementDate(LocalDate.of(2024, 8, 31))
            .nextStatementDate(LocalDate.of(2024, 9, 30))
            .status("ACTIVE")
            .build();
        
        AccountResponseDto dto3 = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode("654321")
            .accountType("CURRENT")
            .customerNumber(1000001L)
            .customerName("John Doe")
            .availableBalance(new BigDecimal("1500.00"))
            .actualBalance(new BigDecimal("1500.00"))
            .interestRate(new BigDecimal("2.5"))
            .openedDate(LocalDate.of(2023, 1, 15))
            .overdraftLimit(500)
            .lastStatementDate(LocalDate.of(2024, 8, 31))
            .nextStatementDate(LocalDate.of(2024, 9, 30))
            .status("ACTIVE")
            .build();
        
        AccountResponseDto dto4 = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode("123456")
            .accountType("CURRENT")
            .customerNumber(2000002L)
            .customerName("John Doe")
            .availableBalance(new BigDecimal("1500.00"))
            .actualBalance(new BigDecimal("1500.00"))
            .interestRate(new BigDecimal("2.5"))
            .openedDate(LocalDate.of(2023, 1, 15))
            .overdraftLimit(500)
            .lastStatementDate(LocalDate.of(2024, 8, 31))
            .nextStatementDate(LocalDate.of(2024, 9, 30))
            .status("ACTIVE")
            .build();
        
        // Then
        assertThat(dto1).isNotEqualTo(dto2);
        assertThat(dto1).isNotEqualTo(dto3);
        assertThat(dto1).isNotEqualTo(dto4);
        assertThat(dto1.hashCode()).isNotEqualTo(dto2.hashCode());
        assertThat(dto1.hashCode()).isNotEqualTo(dto3.hashCode());
        assertThat(dto1.hashCode()).isNotEqualTo(dto4.hashCode());
    }

    @Test
    @DisplayName("Should test equals with null fields")
    void testEquals_withNullFields() {
        // Given
        AccountResponseDto dto1 = new AccountResponseDto();
        AccountResponseDto dto2 = new AccountResponseDto();
        
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
        
        dto1.setAccountNumber("12345678");
        assertThat(dto1).isNotEqualTo(dto2);
        assertThat(dto1.hashCode()).isNotEqualTo(dto2.hashCode());
        
        dto2.setAccountNumber("12345678");
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
    }

    @Test
    @DisplayName("Should test equals with mixed null and non-null fields")
    void testEquals_withMixedNullAndNonNullFields() {
        // Given
        AccountResponseDto dto1 = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode(null)
            .accountType("CURRENT")
            .customerNumber(null)
            .customerName("John Doe")
            .availableBalance(null)
            .actualBalance(new BigDecimal("1500.00"))
            .interestRate(null)
            .openedDate(LocalDate.of(2023, 1, 15))
            .overdraftLimit(null)
            .lastStatementDate(null)
            .nextStatementDate(LocalDate.of(2024, 9, 30))
            .status(null)
            .build();
        
        AccountResponseDto dto2 = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode(null)
            .accountType("CURRENT")
            .customerNumber(null)
            .customerName("John Doe")
            .availableBalance(null)
            .actualBalance(new BigDecimal("1500.00"))
            .interestRate(null)
            .openedDate(LocalDate.of(2023, 1, 15))
            .overdraftLimit(null)
            .lastStatementDate(null)
            .nextStatementDate(LocalDate.of(2024, 9, 30))
            .status(null)
            .build();
        
        AccountResponseDto dto3 = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode("123456")  // Different: non-null vs null
            .accountType("CURRENT")
            .customerNumber(null)
            .customerName("John Doe")
            .availableBalance(null)
            .actualBalance(new BigDecimal("1500.00"))
            .interestRate(null)
            .openedDate(LocalDate.of(2023, 1, 15))
            .overdraftLimit(null)
            .lastStatementDate(null)
            .nextStatementDate(LocalDate.of(2024, 9, 30))
            .status(null)
            .build();
        
        // Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
        assertThat(dto1).isNotEqualTo(dto3);
        assertThat(dto1.hashCode()).isNotEqualTo(dto3.hashCode());
    }

    @Test
    @DisplayName("Should test hashCode consistency")
    void testHashCode_consistency() {
        // Given
        AccountResponseDto dto = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode("123456")
            .accountType("CURRENT")
            .customerNumber(1000001L)
            .customerName("John Doe")
            .availableBalance(new BigDecimal("1500.00"))
            .actualBalance(new BigDecimal("1500.00"))
            .interestRate(new BigDecimal("2.5"))
            .openedDate(LocalDate.of(2023, 1, 15))
            .overdraftLimit(500)
            .lastStatementDate(LocalDate.of(2024, 8, 31))
            .nextStatementDate(LocalDate.of(2024, 9, 30))
            .status("ACTIVE")
            .build();
        
        // When
        int hashCode1 = dto.hashCode();
        int hashCode2 = dto.hashCode();
        
        // Then
        assertThat(hashCode1).isEqualTo(hashCode2);
    }

    @Test
    @DisplayName("Should test toString with null fields")
    void testToString_withNullFields() {
        // Given
        AccountResponseDto dto = AccountResponseDto.builder()
            .accountNumber("12345678")
            .sortCode(null)
            .accountType("CURRENT")
            .customerNumber(null)
            .customerName("John Doe")
            .build();
        
        // When
        String result = dto.toString();
        
        // Then
        assertThat(result).contains("AccountResponseDto");
        assertThat(result).contains("accountNumber=12345678");
        assertThat(result).contains("sortCode=null");
        assertThat(result).contains("accountType=CURRENT");
        assertThat(result).contains("customerNumber=null");
        assertThat(result).contains("customerName=John Doe");
    }
}
