package com.cbsa.migration.dto;

import org.junit.jupiter.api.Test;

import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.ValidatorFactory;
import java.math.BigDecimal;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

class BalanceUpdateRequestDtoTest {

    private final ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
    private final Validator validator = factory.getValidator();

    @Test
    void testValidBalanceUpdateRequest() {
        BalanceUpdateRequestDto dto = new BalanceUpdateRequestDto(new BigDecimal("250.00"));

        Set<ConstraintViolation<BalanceUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).isEmpty();
        
        assertThat(dto.getAmount()).isEqualTo(new BigDecimal("250.00"));
        assertThat(dto.getFacilityType()).isNull();
    }

    @Test
    void testValidBalanceUpdateRequestWithFacilityType() {
        BalanceUpdateRequestDto dto = new BalanceUpdateRequestDto(new BigDecimal("-100.00"));
        dto.setFacilityType(496);

        Set<ConstraintViolation<BalanceUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).isEmpty();
        
        assertThat(dto.getAmount()).isEqualTo(new BigDecimal("-100.00"));
        assertThat(dto.getFacilityType()).isEqualTo(496);
    }

    @Test
    void testNullAmount() {
        BalanceUpdateRequestDto dto = new BalanceUpdateRequestDto(null);

        Set<ConstraintViolation<BalanceUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).contains("Amount is required");
    }

    @Test
    void testSettersAndGetters() {
        BalanceUpdateRequestDto dto = new BalanceUpdateRequestDto();
        
        dto.setAmount(new BigDecimal("500.75"));
        dto.setFacilityType(123);

        assertThat(dto.getAmount()).isEqualTo(new BigDecimal("500.75"));
        assertThat(dto.getFacilityType()).isEqualTo(123);
    }

    @Test
    void testZeroAmount() {
        BalanceUpdateRequestDto dto = new BalanceUpdateRequestDto(BigDecimal.ZERO);

        Set<ConstraintViolation<BalanceUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).isEmpty();
        assertThat(dto.getAmount()).isEqualTo(BigDecimal.ZERO);
    }

    @Test
    void testNegativeAmount() {
        BalanceUpdateRequestDto dto = new BalanceUpdateRequestDto(new BigDecimal("-1000.00"));

        Set<ConstraintViolation<BalanceUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).isEmpty();
        assertThat(dto.getAmount()).isEqualTo(new BigDecimal("-1000.00"));
    }

    @Test
    void testLargeAmount() {
        BalanceUpdateRequestDto dto = new BalanceUpdateRequestDto(new BigDecimal("999999.99"));

        Set<ConstraintViolation<BalanceUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).isEmpty();
        assertThat(dto.getAmount()).isEqualTo(new BigDecimal("999999.99"));
    }

    @Test
    void testDefaultConstructor() {
        BalanceUpdateRequestDto dto = new BalanceUpdateRequestDto();
        
        assertThat(dto.getAmount()).isNull();
        assertThat(dto.getFacilityType()).isNull();
    }
}
