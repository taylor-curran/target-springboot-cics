package com.cbsa.migration.dto;

import org.junit.jupiter.api.Test;

import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.ValidatorFactory;
import java.math.BigDecimal;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

class AccountUpdateRequestDtoTest {

    private final ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
    private final Validator validator = factory.getValidator();

    @Test
    void testValidAccountUpdateRequest() {
        AccountUpdateRequestDto dto = new AccountUpdateRequestDto(
            "SAVINGS",
            new BigDecimal("3.25"),
            1500
        );

        Set<ConstraintViolation<AccountUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).isEmpty();
        
        assertThat(dto.getAccountType()).isEqualTo("SAVINGS");
        assertThat(dto.getInterestRate()).isEqualTo(new BigDecimal("3.25"));
        assertThat(dto.getOverdraftLimit()).isEqualTo(1500);
    }

    @Test
    void testNullAccountType() {
        AccountUpdateRequestDto dto = new AccountUpdateRequestDto(
            null,
            new BigDecimal("3.25"),
            1500
        );

        Set<ConstraintViolation<AccountUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).contains("Account type is required");
    }

    @Test
    void testInvalidAccountTypePattern() {
        AccountUpdateRequestDto dto = new AccountUpdateRequestDto(
            "savings",
            new BigDecimal("3.25"),
            1500
        );

        Set<ConstraintViolation<AccountUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).contains("Account type must start with a letter");
    }

    @Test
    void testAccountTypeTooLong() {
        AccountUpdateRequestDto dto = new AccountUpdateRequestDto(
            "VERYLONGTYPE",
            new BigDecimal("3.25"),
            1500
        );

        Set<ConstraintViolation<AccountUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).contains("Account type must be between 1 and 8 characters");
    }

    @Test
    void testNullInterestRate() {
        AccountUpdateRequestDto dto = new AccountUpdateRequestDto(
            "SAVINGS",
            null,
            1500
        );

        Set<ConstraintViolation<AccountUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).contains("Interest rate is required");
    }

    @Test
    void testNullOverdraftLimit() {
        AccountUpdateRequestDto dto = new AccountUpdateRequestDto(
            "SAVINGS",
            new BigDecimal("3.25"),
            null
        );

        Set<ConstraintViolation<AccountUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).contains("Overdraft limit is required");
    }

    @Test
    void testSettersAndGetters() {
        AccountUpdateRequestDto dto = new AccountUpdateRequestDto();
        
        dto.setAccountType("MORTGAGE");
        dto.setInterestRate(new BigDecimal("4.75"));
        dto.setOverdraftLimit(0);

        assertThat(dto.getAccountType()).isEqualTo("MORTGAGE");
        assertThat(dto.getInterestRate()).isEqualTo(new BigDecimal("4.75"));
        assertThat(dto.getOverdraftLimit()).isEqualTo(0);
    }

    @Test
    void testValidAccountTypeWithSpaces() {
        AccountUpdateRequestDto dto = new AccountUpdateRequestDto(
            "LOAN ACC",
            new BigDecimal("5.50"),
            2000
        );

        Set<ConstraintViolation<AccountUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).isEmpty();
        assertThat(dto.getAccountType()).isEqualTo("LOAN ACC");
    }

    @Test
    void testValidAccountTypeWithNumbers() {
        AccountUpdateRequestDto dto = new AccountUpdateRequestDto(
            "TERM90",
            new BigDecimal("6.00"),
            0
        );

        Set<ConstraintViolation<AccountUpdateRequestDto>> violations = validator.validate(dto);
        assertThat(violations).isEmpty();
        assertThat(dto.getAccountType()).isEqualTo("TERM90");
    }
}
