package com.cbsa.migration.dto;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

class AccountInquiryResponseDtoTest {

    @Test
    void testSuccessfulResponse() {
        AccountInquiryResponseDto dto = new AccountInquiryResponseDto();
        dto.setSuccess(true);
        dto.setEyeCatcher("ACCT");
        dto.setCustomerNumber(1234567890L);
        dto.setSortCode("123456");
        dto.setAccountNumber("00000001");
        dto.setAccountType("CURRENT");
        dto.setInterestRate(new BigDecimal("2.50"));
        dto.setOpenedDate(LocalDate.of(2023, 1, 15));
        dto.setOverdraftLimit(1000);
        dto.setLastStatementDate(LocalDate.of(2023, 1, 1));
        dto.setNextStatementDate(LocalDate.of(2023, 2, 1));
        dto.setAvailableBalance(new BigDecimal("500.00"));
        dto.setActualBalance(new BigDecimal("500.00"));

        assertThat(dto.isSuccess()).isTrue();
        assertThat(dto.getEyeCatcher()).isEqualTo("ACCT");
        assertThat(dto.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(dto.getSortCode()).isEqualTo("123456");
        assertThat(dto.getAccountNumber()).isEqualTo("00000001");
        assertThat(dto.getAccountType()).isEqualTo("CURRENT");
        assertThat(dto.getInterestRate()).isEqualTo(new BigDecimal("2.50"));
        assertThat(dto.getOpenedDate()).isEqualTo(LocalDate.of(2023, 1, 15));
        assertThat(dto.getOverdraftLimit()).isEqualTo(1000);
        assertThat(dto.getLastStatementDate()).isEqualTo(LocalDate.of(2023, 1, 1));
        assertThat(dto.getNextStatementDate()).isEqualTo(LocalDate.of(2023, 2, 1));
        assertThat(dto.getAvailableBalance()).isEqualTo(new BigDecimal("500.00"));
        assertThat(dto.getActualBalance()).isEqualTo(new BigDecimal("500.00"));
    }

    @Test
    void testFailureResponse() {
        AccountInquiryResponseDto dto = new AccountInquiryResponseDto();
        dto.setSuccess(false);

        assertThat(dto.isSuccess()).isFalse();
        assertThat(dto.getCustomerNumber()).isNull();
        assertThat(dto.getSortCode()).isNull();
        assertThat(dto.getAccountNumber()).isNull();
    }

    @Test
    void testConstructorWithSuccess() {
        AccountInquiryResponseDto successDto = new AccountInquiryResponseDto(true);
        assertThat(successDto.isSuccess()).isTrue();

        AccountInquiryResponseDto failureDto = new AccountInquiryResponseDto(false);
        assertThat(failureDto.isSuccess()).isFalse();
    }

    @Test
    void testAllSettersAndGetters() {
        AccountInquiryResponseDto dto = new AccountInquiryResponseDto();
        
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
    }

    @Test
    void testNullValues() {
        AccountInquiryResponseDto dto = new AccountInquiryResponseDto();
        
        assertThat(dto.isSuccess()).isFalse();
        assertThat(dto.getEyeCatcher()).isNull();
        assertThat(dto.getCustomerNumber()).isNull();
        assertThat(dto.getSortCode()).isNull();
        assertThat(dto.getAccountNumber()).isNull();
        assertThat(dto.getAccountType()).isNull();
        assertThat(dto.getInterestRate()).isNull();
        assertThat(dto.getOpenedDate()).isNull();
        assertThat(dto.getOverdraftLimit()).isNull();
        assertThat(dto.getLastStatementDate()).isNull();
        assertThat(dto.getNextStatementDate()).isNull();
        assertThat(dto.getAvailableBalance()).isNull();
        assertThat(dto.getActualBalance()).isNull();
    }
}
