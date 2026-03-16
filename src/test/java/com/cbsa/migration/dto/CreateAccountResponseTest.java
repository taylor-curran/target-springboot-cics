package com.cbsa.migration.dto;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.junit.jupiter.api.Assertions.*;

class CreateAccountResponseTest {

    @Test
    void successFactory_setsAllFields() {
        LocalDate today = LocalDate.of(2026, 3, 16);
        LocalDate nextStmt = today.plusDays(30);

        CreateAccountResponse response = CreateAccountResponse.success(
                "10000001", "987654", 1000001L, "CURRENT",
                new BigDecimal("2.50"), today, 500,
                today, nextStmt, BigDecimal.ZERO, BigDecimal.ZERO
        );

        assertTrue(response.isSuccess());
        assertNull(response.getFailCode());
        assertNull(response.getFailMessage());
        assertEquals("10000001", response.getAccountNumber());
        assertEquals("987654", response.getSortCode());
        assertEquals(1000001L, response.getCustomerNumber());
        assertEquals("CURRENT", response.getAccountType());
        assertEquals(new BigDecimal("2.50"), response.getInterestRate());
        assertEquals(today, response.getOpenedDate());
        assertEquals(500, response.getOverdraftLimit());
        assertEquals(today, response.getLastStatementDate());
        assertEquals(nextStmt, response.getNextStatementDate());
        assertEquals(BigDecimal.ZERO, response.getAvailableBalance());
        assertEquals(BigDecimal.ZERO, response.getActualBalance());
    }

    @Test
    void failureFactory_setsFailFields() {
        CreateAccountResponse response = CreateAccountResponse.failure("1", "Customer not found");

        assertFalse(response.isSuccess());
        assertEquals("1", response.getFailCode());
        assertEquals("Customer not found", response.getFailMessage());
        assertNull(response.getAccountNumber());
        assertNull(response.getSortCode());
    }

    @Test
    void noArgsConstructor_andSetters() {
        CreateAccountResponse response = new CreateAccountResponse();
        response.setAccountNumber("99999999");
        response.setSortCode("123456");
        response.setCustomerNumber(42L);
        response.setAccountType("ISA");
        response.setInterestRate(new BigDecimal("3.00"));
        response.setOpenedDate(LocalDate.of(2026, 1, 1));
        response.setOverdraftLimit(100);
        response.setLastStatementDate(LocalDate.of(2026, 1, 1));
        response.setNextStatementDate(LocalDate.of(2026, 1, 31));
        response.setAvailableBalance(new BigDecimal("1000.00"));
        response.setActualBalance(new BigDecimal("1000.00"));
        response.setSuccess(true);
        response.setFailCode(null);
        response.setFailMessage(null);

        assertEquals("99999999", response.getAccountNumber());
        assertEquals("123456", response.getSortCode());
        assertEquals(42L, response.getCustomerNumber());
        assertEquals("ISA", response.getAccountType());
        assertEquals(new BigDecimal("3.00"), response.getInterestRate());
        assertEquals(LocalDate.of(2026, 1, 1), response.getOpenedDate());
        assertEquals(100, response.getOverdraftLimit());
        assertTrue(response.isSuccess());
    }

    @Test
    void allArgsConstructor() {
        LocalDate d = LocalDate.of(2026, 6, 15);
        CreateAccountResponse response = new CreateAccountResponse(
                "00000001", "987654", 1L, "LOAN",
                new BigDecimal("5.00"), d, 0,
                d, d.plusDays(30),
                BigDecimal.TEN, BigDecimal.TEN,
                true, null, null
        );

        assertEquals("00000001", response.getAccountNumber());
        assertEquals("LOAN", response.getAccountType());
        assertTrue(response.isSuccess());
    }

    @Test
    void equalsAndHashCode() {
        CreateAccountResponse r1 = CreateAccountResponse.failure("A", "Invalid type");
        CreateAccountResponse r2 = CreateAccountResponse.failure("A", "Invalid type");

        assertEquals(r1, r2);
        assertEquals(r1.hashCode(), r2.hashCode());
    }

    @Test
    void toString_containsFields() {
        CreateAccountResponse response = CreateAccountResponse.failure("8", "Too many accounts");
        String str = response.toString();

        assertTrue(str.contains("8"));
        assertTrue(str.contains("Too many accounts"));
    }

    @Test
    void builder_setsFields() {
        CreateAccountResponse response = CreateAccountResponse.builder()
                .accountNumber("12345678")
                .sortCode("987654")
                .customerNumber(100L)
                .accountType("SAVING")
                .interestRate(new BigDecimal("1.50"))
                .openedDate(LocalDate.now())
                .overdraftLimit(0)
                .lastStatementDate(LocalDate.now())
                .nextStatementDate(LocalDate.now().plusDays(30))
                .availableBalance(BigDecimal.ZERO)
                .actualBalance(BigDecimal.ZERO)
                .success(true)
                .failCode(null)
                .failMessage(null)
                .build();

        assertEquals("12345678", response.getAccountNumber());
        assertEquals("SAVING", response.getAccountType());
        assertTrue(response.isSuccess());
    }
}
