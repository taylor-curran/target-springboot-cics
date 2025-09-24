package com.cbsa.migration.dto;

import org.junit.jupiter.api.Test;

import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.ValidatorFactory;
import java.math.BigDecimal;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

class AccountCreationRequestDtoTest {

    private final ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
    private final Validator validator = factory.getValidator();

    @Test
    void testValidAccountCreationRequest() {
        AccountCreationRequestDto dto = new AccountCreationRequestDto(
            1234567890L,
            "CURRENT",
            new BigDecimal("2.50"),
            1000
        );

        Set<ConstraintViolation<AccountCreationRequestDto>> violations = validator.validate(dto);
        assertThat(violations).isEmpty();
        
        assertThat(dto.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(dto.getAccountType()).isEqualTo("CURRENT");
        assertThat(dto.getInterestRate()).isEqualTo(new BigDecimal("2.50"));
        assertThat(dto.getOverdraftLimit()).isEqualTo(1000);
    }

    @Test
    void testNullCustomerNumber() {
        AccountCreationRequestDto dto = new AccountCreationRequestDto(
            null,
            "CURRENT",
            new BigDecimal("2.50"),
            1000
        );

        Set<ConstraintViolation<AccountCreationRequestDto>> violations = validator.validate(dto);
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).contains("Customer number is required");
    }

    @Test
    void testNegativeCustomerNumber() {
        AccountCreationRequestDto dto = new AccountCreationRequestDto(
            -123L,
            "CURRENT",
            new BigDecimal("2.50"),
            1000
        );

        Set<ConstraintViolation<AccountCreationRequestDto>> violations = validator.validate(dto);
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).contains("Customer number must be positive");
    }

    @Test
    void testNullAccountType() {
        AccountCreationRequestDto dto = new AccountCreationRequestDto(
            1234567890L,
            null,
            new BigDecimal("2.50"),
            1000
        );

        Set<ConstraintViolation<AccountCreationRequestDto>> violations = validator.validate(dto);
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).contains("Account type is required");
    }

    @Test
    void testInvalidAccountTypePattern() {
        AccountCreationRequestDto dto = new AccountCreationRequestDto(
            1234567890L,
            "current",
            new BigDecimal("2.50"),
            1000
        );

        Set<ConstraintViolation<AccountCreationRequestDto>> violations = validator.validate(dto);
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).contains("Account type must start with a letter");
    }

    @Test
    void testAccountTypeTooLong() {
        AccountCreationRequestDto dto = new AccountCreationRequestDto(
            1234567890L,
            "VERYLONGACCOUNTTYPE",
            new BigDecimal("2.50"),
            1000
        );

        Set<ConstraintViolation<AccountCreationRequestDto>> violations = validator.validate(dto);
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).contains("Account type must be between 1 and 8 characters");
    }

    @Test
    void testNullInterestRate() {
        AccountCreationRequestDto dto = new AccountCreationRequestDto(
            1234567890L,
            "CURRENT",
            null,
            1000
        );

        Set<ConstraintViolation<AccountCreationRequestDto>> violations = validator.validate(dto);
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).contains("Interest rate is required");
    }

    @Test
    void testNullOverdraftLimit() {
        AccountCreationRequestDto dto = new AccountCreationRequestDto(
            1234567890L,
            "CURRENT",
            new BigDecimal("2.50"),
            null
        );

        Set<ConstraintViolation<AccountCreationRequestDto>> violations = validator.validate(dto);
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).contains("Overdraft limit is required");
    }

    @Test
    void testSettersAndGetters() {
        AccountCreationRequestDto dto = new AccountCreationRequestDto();
        
        dto.setCustomerNumber(9876543210L);
        dto.setAccountType("SAVINGS");
        dto.setInterestRate(new BigDecimal("3.75"));
        dto.setOverdraftLimit(2000);

        assertThat(dto.getCustomerNumber()).isEqualTo(9876543210L);
        assertThat(dto.getAccountType()).isEqualTo("SAVINGS");
        assertThat(dto.getInterestRate()).isEqualTo(new BigDecimal("3.75"));
        assertThat(dto.getOverdraftLimit()).isEqualTo(2000);
    }
}
