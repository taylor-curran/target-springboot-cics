package com.cbsa.migration.dto;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.*;

class CreateAccountRequestTest {

    @Test
    void builder_setsAllFields() {
        CreateAccountRequest request = CreateAccountRequest.builder()
                .customerNumber(1000001L)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("2.50"))
                .overdraftLimit(500)
                .build();

        assertEquals(1000001L, request.getCustomerNumber());
        assertEquals("CURRENT", request.getAccountType());
        assertEquals(new BigDecimal("2.50"), request.getInterestRate());
        assertEquals(500, request.getOverdraftLimit());
    }

    @Test
    void noArgsConstructor_andSetters() {
        CreateAccountRequest request = new CreateAccountRequest();
        request.setCustomerNumber(2000002L);
        request.setAccountType("ISA");
        request.setInterestRate(new BigDecimal("1.25"));
        request.setOverdraftLimit(0);

        assertEquals(2000002L, request.getCustomerNumber());
        assertEquals("ISA", request.getAccountType());
        assertEquals(new BigDecimal("1.25"), request.getInterestRate());
        assertEquals(0, request.getOverdraftLimit());
    }

    @Test
    void allArgsConstructor() {
        CreateAccountRequest request = new CreateAccountRequest(
                3000003L, "LOAN", new BigDecimal("5.00"), 1000);

        assertEquals(3000003L, request.getCustomerNumber());
        assertEquals("LOAN", request.getAccountType());
        assertEquals(new BigDecimal("5.00"), request.getInterestRate());
        assertEquals(1000, request.getOverdraftLimit());
    }

    @Test
    void equalsAndHashCode() {
        CreateAccountRequest r1 = CreateAccountRequest.builder()
                .customerNumber(1L).accountType("SAVING")
                .interestRate(BigDecimal.ONE).overdraftLimit(0).build();
        CreateAccountRequest r2 = CreateAccountRequest.builder()
                .customerNumber(1L).accountType("SAVING")
                .interestRate(BigDecimal.ONE).overdraftLimit(0).build();

        assertEquals(r1, r2);
        assertEquals(r1.hashCode(), r2.hashCode());
    }

    @Test
    void toString_containsFields() {
        CreateAccountRequest request = CreateAccountRequest.builder()
                .customerNumber(1L).accountType("MORTGAGE").build();
        String str = request.toString();

        assertTrue(str.contains("MORTGAGE"));
        assertTrue(str.contains("1"));
    }
}
