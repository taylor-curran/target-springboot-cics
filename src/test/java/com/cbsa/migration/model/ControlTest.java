package com.cbsa.migration.model;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for Control model
 * Tests the COBOL CONTROL copybook structure migration
 */
class ControlTest {

    @Test
    @DisplayName("Should create Control with no-args constructor")
    void testNoArgsConstructor_createsValidObject() {
        // When
        Control control = new Control();
        
        // Then
        assertThat(control).isNotNull();
        assertThat(control.getCustomerCount()).isNull();
        assertThat(control.getLastCustomerNumber()).isNull();
        assertThat(control.getAccountCount()).isNull();
        assertThat(control.getLastAccountNumber()).isNull();
    }

    @Test
    @DisplayName("Should create Control with all-args constructor")
    void testAllArgsConstructor_setsAllProperties() {
        // Given
        Long customerCount = 1000L;
        Long lastCustomerNumber = 999999L;
        Integer accountCount = 2500;
        Integer lastAccountNumber = 99999;
        
        // When
        Control control = new Control(customerCount, lastCustomerNumber, accountCount, lastAccountNumber);
        
        // Then
        assertThat(control.getCustomerCount()).isEqualTo(customerCount);
        assertThat(control.getLastCustomerNumber()).isEqualTo(lastCustomerNumber);
        assertThat(control.getAccountCount()).isEqualTo(accountCount);
        assertThat(control.getLastAccountNumber()).isEqualTo(lastAccountNumber);
    }

    @Test
    @DisplayName("Should create Control with builder pattern")
    void testBuilder_setsAllProperties() {
        // Given
        Long customerCount = 1500L;
        Long lastCustomerNumber = 888888L;
        Integer accountCount = 3000;
        Integer lastAccountNumber = 77777;
        
        // When
        Control control = Control.builder()
                .customerCount(customerCount)
                .lastCustomerNumber(lastCustomerNumber)
                .accountCount(accountCount)
                .lastAccountNumber(lastAccountNumber)
                .build();
        
        // Then
        assertThat(control.getCustomerCount()).isEqualTo(customerCount);
        assertThat(control.getLastCustomerNumber()).isEqualTo(lastCustomerNumber);
        assertThat(control.getAccountCount()).isEqualTo(accountCount);
        assertThat(control.getLastAccountNumber()).isEqualTo(lastAccountNumber);
    }

    @Test
    @DisplayName("Should return static CONTROL_ID from getId method")
    void testGetId_returnsStaticControlId() {
        // Given
        Control control = new Control();
        
        // When
        String id = control.getId();
        
        // Then
        assertThat(id).isEqualTo("CONTROL");
        assertThat(id).isEqualTo(Control.CONTROL_ID);
    }

    @Test
    @DisplayName("Should have consistent CONTROL_ID constant")
    void testControlIdConstant_isConsistent() {
        // Then
        assertThat(Control.CONTROL_ID).isEqualTo("CONTROL");
        assertThat(Control.CONTROL_ID).isNotNull();
        assertThat(Control.CONTROL_ID).isNotEmpty();
    }

    @Test
    @DisplayName("Should test all field setters and getters")
    void testAllFieldsSettersAndGetters() {
        // Given
        Control control = new Control();
        Long customerCount = 2000L;
        Long lastCustomerNumber = 777777L;
        Integer accountCount = 4000;
        Integer lastAccountNumber = 55555;
        
        // When
        control.setCustomerCount(customerCount);
        control.setLastCustomerNumber(lastCustomerNumber);
        control.setAccountCount(accountCount);
        control.setLastAccountNumber(lastAccountNumber);
        
        // Then
        assertThat(control.getCustomerCount()).isEqualTo(customerCount);
        assertThat(control.getLastCustomerNumber()).isEqualTo(lastCustomerNumber);
        assertThat(control.getAccountCount()).isEqualTo(accountCount);
        assertThat(control.getLastAccountNumber()).isEqualTo(lastAccountNumber);
    }

    @Test
    @DisplayName("Should test equals and hashCode consistency")
    void testEqualsAndHashCode_consistency() {
        // Given
        Control control1 = Control.builder()
                .customerCount(1000L)
                .lastCustomerNumber(999999L)
                .accountCount(2500)
                .lastAccountNumber(99999)
                .build();
        
        Control control2 = Control.builder()
                .customerCount(1000L)
                .lastCustomerNumber(999999L)
                .accountCount(2500)
                .lastAccountNumber(99999)
                .build();
        
        Control control3 = Control.builder()
                .customerCount(2000L)
                .lastCustomerNumber(888888L)
                .accountCount(3000)
                .lastAccountNumber(77777)
                .build();
        
        // Then
        assertThat(control1).isEqualTo(control2);
        assertThat(control1).isNotEqualTo(control3);
        assertThat(control1.hashCode()).isEqualTo(control2.hashCode());
        assertThat(control1.hashCode()).isNotEqualTo(control3.hashCode());
    }

    @Test
    @DisplayName("Should test toString contains key information")
    void testToString_containsKeyInformation() {
        // Given
        Control control = Control.builder()
                .customerCount(1000L)
                .lastCustomerNumber(999999L)
                .accountCount(2500)
                .lastAccountNumber(99999)
                .build();
        
        // When
        String result = control.toString();
        
        // Then
        assertThat(result).contains("Control");
        assertThat(result).contains("customerCount=1000");
        assertThat(result).contains("lastCustomerNumber=999999");
        assertThat(result).contains("accountCount=2500");
        assertThat(result).contains("lastAccountNumber=99999");
    }

    @Test
    @DisplayName("Should handle null values in fields")
    void testNullValues_handledCorrectly() {
        // Given
        Control control = new Control();
        
        // When
        control.setCustomerCount(null);
        control.setLastCustomerNumber(null);
        control.setAccountCount(null);
        control.setLastAccountNumber(null);
        
        // Then
        assertThat(control.getCustomerCount()).isNull();
        assertThat(control.getLastCustomerNumber()).isNull();
        assertThat(control.getAccountCount()).isNull();
        assertThat(control.getLastAccountNumber()).isNull();
        assertThat(control.getId()).isEqualTo("CONTROL");
    }

    @Test
    @DisplayName("Should handle edge case values")
    void testEdgeCaseValues_handledCorrectly() {
        // Given
        Control control = new Control();
        Long maxLong = Long.MAX_VALUE;
        Integer maxInt = Integer.MAX_VALUE;
        Long minLong = 0L;
        Integer minInt = 0;
        
        // When
        control.setCustomerCount(maxLong);
        control.setLastCustomerNumber(minLong);
        control.setAccountCount(maxInt);
        control.setLastAccountNumber(minInt);
        
        // Then
        assertThat(control.getCustomerCount()).isEqualTo(maxLong);
        assertThat(control.getLastCustomerNumber()).isEqualTo(minLong);
        assertThat(control.getAccountCount()).isEqualTo(maxInt);
        assertThat(control.getLastAccountNumber()).isEqualTo(minInt);
    }

    @Test
    @DisplayName("Should test builder with partial fields")
    void testBuilder_withPartialFields() {
        // When
        Control control1 = Control.builder()
                .customerCount(1000L)
                .accountCount(2500)
                .build();
        
        Control control2 = Control.builder()
                .lastCustomerNumber(999999L)
                .lastAccountNumber(99999)
                .build();
        
        // Then
        assertThat(control1.getCustomerCount()).isEqualTo(1000L);
        assertThat(control1.getLastCustomerNumber()).isNull();
        assertThat(control1.getAccountCount()).isEqualTo(2500);
        assertThat(control1.getLastAccountNumber()).isNull();
        
        assertThat(control2.getCustomerCount()).isNull();
        assertThat(control2.getLastCustomerNumber()).isEqualTo(999999L);
        assertThat(control2.getAccountCount()).isNull();
        assertThat(control2.getLastAccountNumber()).isEqualTo(99999);
    }

    @Test
    @DisplayName("Should test getId method consistency across instances")
    void testGetId_consistencyAcrossInstances() {
        // Given
        Control control1 = new Control();
        Control control2 = Control.builder().customerCount(1000L).build();
        Control control3 = new Control(2000L, 888888L, 3000, 77777);
        
        // When & Then
        assertThat(control1.getId()).isEqualTo("CONTROL");
        assertThat(control2.getId()).isEqualTo("CONTROL");
        assertThat(control3.getId()).isEqualTo("CONTROL");
        assertThat(control1.getId()).isEqualTo(control2.getId());
        assertThat(control2.getId()).isEqualTo(control3.getId());
    }
}
