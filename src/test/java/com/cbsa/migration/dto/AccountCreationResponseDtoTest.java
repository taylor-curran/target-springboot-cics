package com.cbsa.migration.dto;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

class AccountCreationResponseDtoTest {

    @Test
    void testSuccessfulResponse() {
        AccountCreationResponseDto dto = new AccountCreationResponseDto();
        dto.setSuccess(true);
        dto.setEyeCatcher("ACCT");
        dto.setCustomerNumber(1234567890L);
        dto.setSortCode("123456");
        dto.setAccountNumber("00000001");
        dto.setAccountType("CURRENT");
        dto.setInterestRate(new BigDecimal("2.50"));
        dto.setOpenedDate(LocalDate.of(2023, 1, 15));
        dto.setOverdraftLimit(1000);
        dto.setAvailableBalance(BigDecimal.ZERO);
        dto.setActualBalance(BigDecimal.ZERO);

        assertThat(dto.isSuccess()).isTrue();
        assertThat(dto.getEyeCatcher()).isEqualTo("ACCT");
        assertThat(dto.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(dto.getSortCode()).isEqualTo("123456");
        assertThat(dto.getAccountNumber()).isEqualTo("00000001");
        assertThat(dto.getAccountType()).isEqualTo("CURRENT");
        assertThat(dto.getInterestRate()).isEqualTo(new BigDecimal("2.50"));
        assertThat(dto.getOpenedDate()).isEqualTo(LocalDate.of(2023, 1, 15));
        assertThat(dto.getOverdraftLimit()).isEqualTo(1000);
        assertThat(dto.getAvailableBalance()).isEqualTo(BigDecimal.ZERO);
        assertThat(dto.getActualBalance()).isEqualTo(BigDecimal.ZERO);
        assertThat(dto.getFailCode()).isNull();
    }

    @Test
    void testFailureResponse() {
        AccountCreationResponseDto dto = new AccountCreationResponseDto();
        dto.setSuccess(false);
        dto.setFailCode("1");

        assertThat(dto.isSuccess()).isFalse();
        assertThat(dto.getFailCode()).isEqualTo("1");
        assertThat(dto.getCustomerNumber()).isNull();
        assertThat(dto.getSortCode()).isNull();
        assertThat(dto.getAccountNumber()).isNull();
    }

    @Test
    void testConstructorWithSuccessAndFailCode() {
        AccountCreationResponseDto successDto = new AccountCreationResponseDto(true, null);
        successDto.setEyeCatcher("ACCT");
        successDto.setCustomerNumber(1234567890L);
        successDto.setSortCode("123456");
        successDto.setAccountNumber("00000001");
        successDto.setAccountType("CURRENT");
        successDto.setInterestRate(new BigDecimal("2.50"));
        successDto.setOpenedDate(LocalDate.now());
        successDto.setOverdraftLimit(1000);
        successDto.setAvailableBalance(BigDecimal.ZERO);
        successDto.setActualBalance(BigDecimal.ZERO);

        assertThat(successDto.isSuccess()).isTrue();
        assertThat(successDto.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(successDto.getAccountType()).isEqualTo("CURRENT");

        AccountCreationResponseDto failureDto = new AccountCreationResponseDto(false, "2");

        assertThat(failureDto.isSuccess()).isFalse();
        assertThat(failureDto.getFailCode()).isEqualTo("2");
    }

    @Test
    void testAllSettersAndGetters() {
        AccountCreationResponseDto dto = new AccountCreationResponseDto();
        
        dto.setSuccess(true);
        dto.setEyeCatcher("TEST");
        dto.setCustomerNumber(9999999999L);
        dto.setSortCode("654321");
        dto.setAccountNumber("99999999");
        dto.setAccountType("SAVINGS");
        dto.setInterestRate(new BigDecimal("4.25"));
        dto.setOpenedDate(LocalDate.of(2024, 6, 1));
        dto.setOverdraftLimit(5000);
        dto.setLastStatementDate(LocalDate.of(2024, 5, 31));
        dto.setNextStatementDate(LocalDate.of(2024, 6, 30));
        dto.setAvailableBalance(new BigDecimal("1500.75"));
        dto.setActualBalance(new BigDecimal("1500.75"));
        dto.setFailCode("0");

        assertThat(dto.isSuccess()).isTrue();
        assertThat(dto.getEyeCatcher()).isEqualTo("TEST");
        assertThat(dto.getCustomerNumber()).isEqualTo(9999999999L);
        assertThat(dto.getSortCode()).isEqualTo("654321");
        assertThat(dto.getAccountNumber()).isEqualTo("99999999");
        assertThat(dto.getAccountType()).isEqualTo("SAVINGS");
        assertThat(dto.getInterestRate()).isEqualTo(new BigDecimal("4.25"));
        assertThat(dto.getOpenedDate()).isEqualTo(LocalDate.of(2024, 6, 1));
        assertThat(dto.getOverdraftLimit()).isEqualTo(5000);
        assertThat(dto.getLastStatementDate()).isEqualTo(LocalDate.of(2024, 5, 31));
        assertThat(dto.getNextStatementDate()).isEqualTo(LocalDate.of(2024, 6, 30));
        assertThat(dto.getAvailableBalance()).isEqualTo(new BigDecimal("1500.75"));
        assertThat(dto.getActualBalance()).isEqualTo(new BigDecimal("1500.75"));
        assertThat(dto.getFailCode()).isEqualTo("0");
    }
}
