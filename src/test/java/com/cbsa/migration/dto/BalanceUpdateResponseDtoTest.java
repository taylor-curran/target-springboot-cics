package com.cbsa.migration.dto;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

class BalanceUpdateResponseDtoTest {

    @Test
    void testSuccessfulResponse() {
        BalanceUpdateResponseDto dto = new BalanceUpdateResponseDto();
        dto.setSuccess(true);
        dto.setAvailableBalance(new BigDecimal("750.00"));
        dto.setActualBalance(new BigDecimal("750.00"));

        assertThat(dto.isSuccess()).isTrue();
        assertThat(dto.getAvailableBalance()).isEqualTo(new BigDecimal("750.00"));
        assertThat(dto.getActualBalance()).isEqualTo(new BigDecimal("750.00"));
        assertThat(dto.getFailCode()).isNull();
    }

    @Test
    void testFailureResponse() {
        BalanceUpdateResponseDto dto = new BalanceUpdateResponseDto();
        dto.setSuccess(false);
        dto.setFailCode("3");

        assertThat(dto.isSuccess()).isFalse();
        assertThat(dto.getFailCode()).isEqualTo("3");
        assertThat(dto.getAvailableBalance()).isNull();
        assertThat(dto.getActualBalance()).isNull();
    }

    @Test
    void testConstructorWithSuccessAndFailCode() {
        BalanceUpdateResponseDto successDto = new BalanceUpdateResponseDto(true, null);
        successDto.setAvailableBalance(new BigDecimal("1250.50"));
        successDto.setActualBalance(new BigDecimal("1250.50"));

        assertThat(successDto.isSuccess()).isTrue();
        assertThat(successDto.getAvailableBalance()).isEqualTo(new BigDecimal("1250.50"));
        assertThat(successDto.getActualBalance()).isEqualTo(new BigDecimal("1250.50"));
        assertThat(successDto.getFailCode()).isNull();

        BalanceUpdateResponseDto failureDto = new BalanceUpdateResponseDto(false, "1");

        assertThat(failureDto.isSuccess()).isFalse();
        assertThat(failureDto.getFailCode()).isEqualTo("1");
        assertThat(failureDto.getAvailableBalance()).isNull();
        assertThat(failureDto.getActualBalance()).isNull();
    }

    @Test
    void testAllSettersAndGetters() {
        BalanceUpdateResponseDto dto = new BalanceUpdateResponseDto();
        
        dto.setSuccess(true);
        dto.setAvailableBalance(new BigDecimal("2500.25"));
        dto.setActualBalance(new BigDecimal("2500.25"));
        dto.setFailCode("0");

        assertThat(dto.isSuccess()).isTrue();
        assertThat(dto.getAvailableBalance()).isEqualTo(new BigDecimal("2500.25"));
        assertThat(dto.getActualBalance()).isEqualTo(new BigDecimal("2500.25"));
        assertThat(dto.getFailCode()).isEqualTo("0");
    }

    @Test
    void testNullValues() {
        BalanceUpdateResponseDto dto = new BalanceUpdateResponseDto();
        
        assertThat(dto.isSuccess()).isFalse();
        assertThat(dto.getAvailableBalance()).isNull();
        assertThat(dto.getActualBalance()).isNull();
        assertThat(dto.getFailCode()).isNull();
    }

    @Test
    void testZeroBalances() {
        BalanceUpdateResponseDto dto = new BalanceUpdateResponseDto();
        dto.setSuccess(true);
        dto.setAvailableBalance(BigDecimal.ZERO);
        dto.setActualBalance(BigDecimal.ZERO);

        assertThat(dto.isSuccess()).isTrue();
        assertThat(dto.getAvailableBalance()).isEqualTo(BigDecimal.ZERO);
        assertThat(dto.getActualBalance()).isEqualTo(BigDecimal.ZERO);
    }

    @Test
    void testNegativeBalances() {
        BalanceUpdateResponseDto dto = new BalanceUpdateResponseDto();
        dto.setSuccess(true);
        dto.setAvailableBalance(new BigDecimal("-500.00"));
        dto.setActualBalance(new BigDecimal("-500.00"));

        assertThat(dto.isSuccess()).isTrue();
        assertThat(dto.getAvailableBalance()).isEqualTo(new BigDecimal("-500.00"));
        assertThat(dto.getActualBalance()).isEqualTo(new BigDecimal("-500.00"));
    }

    @Test
    void testDifferentBalances() {
        BalanceUpdateResponseDto dto = new BalanceUpdateResponseDto();
        dto.setSuccess(true);
        dto.setAvailableBalance(new BigDecimal("1000.00"));
        dto.setActualBalance(new BigDecimal("1200.00"));

        assertThat(dto.isSuccess()).isTrue();
        assertThat(dto.getAvailableBalance()).isEqualTo(new BigDecimal("1000.00"));
        assertThat(dto.getActualBalance()).isEqualTo(new BigDecimal("1200.00"));
    }
}
