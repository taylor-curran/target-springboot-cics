package com.cbsa.migration;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.boot.SpringApplication;

import java.lang.reflect.Method;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

/**
 * Unit tests for BankingApplication
 * Tests Spring Boot application class structure and main method
 */
class BankingApplicationTest {

    @Test
    @DisplayName("Should have SpringBootApplication annotation")
    void shouldHaveSpringBootApplicationAnnotation() {
        // Given
        Class<?> applicationClass = BankingApplication.class;
        
        // When & Then
        assertThat(applicationClass).isNotNull();
        assertThat(applicationClass.getAnnotation(org.springframework.boot.autoconfigure.SpringBootApplication.class))
            .isNotNull();
    }

    @Test
    @DisplayName("Should have main method with correct signature")
    void shouldHaveMainMethodWithCorrectSignature() {
        // Given
        Class<?> applicationClass = BankingApplication.class;
        
        // When
        Method[] methods = applicationClass.getDeclaredMethods();
        
        // Then
        assertThat(methods)
            .anyMatch(method -> 
                method.getName().equals("main") &&
                method.getParameterTypes().length == 1 &&
                method.getParameterTypes()[0].equals(String[].class) &&
                java.lang.reflect.Modifier.isStatic(method.getModifiers()) &&
                java.lang.reflect.Modifier.isPublic(method.getModifiers())
            );
    }

    @Test
    @DisplayName("Should be a public class")
    void shouldBePublicClass() {
        // Given
        Class<?> applicationClass = BankingApplication.class;
        
        // When & Then
        assertThat(java.lang.reflect.Modifier.isPublic(applicationClass.getModifiers()))
            .isTrue();
    }

    @Test
    @DisplayName("Should have default constructor")
    void shouldHaveDefaultConstructor() {
        // Given & When & Then
        assertThat(BankingApplication.class.getDeclaredConstructors())
            .anyMatch(constructor -> constructor.getParameterCount() == 0);
    }

    @Test
    @DisplayName("Should be able to instantiate BankingApplication")
    void shouldBeAbleToInstantiateBankingApplication() {
        // Given & When & Then
        assertThat(new BankingApplication()).isNotNull();
    }
}
