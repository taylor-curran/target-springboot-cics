package com.cbsa.migration.util;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Security tests for SecurityUtils class.
 * Tests protection against XSS, SQL injection, and path traversal attacks.
 */
class SecurityUtilsTest {
    
    @Test
    void testSanitizeForHtml_XSSProtection() {
        assertEquals("&lt;script&gt;alert(&#39;XSS&#39;)&lt;/script&gt;", 
            SecurityUtils.sanitizeForHtml("<script>alert('XSS')</script>"));
        
        assertEquals("&lt;img src=x onerror=alert(1)&gt;", 
            SecurityUtils.sanitizeForHtml("<img src=x onerror=alert(1)>"));
        
        assertEquals("&lt;a href=&quot;javascript:alert(1)&quot;&gt;Click&lt;/a&gt;", 
            SecurityUtils.sanitizeForHtml("<a href=\"javascript:alert(1)\">Click</a>"));
        
        assertNull(SecurityUtils.sanitizeForHtml(null));
        
        assertEquals("Normal text", SecurityUtils.sanitizeForHtml("Normal text"));
    }
    
    @Test
    void testIsAlphanumeric() {
        assertTrue(SecurityUtils.isAlphanumeric("abc123"));
        assertTrue(SecurityUtils.isAlphanumeric("test_name"));
        assertTrue(SecurityUtils.isAlphanumeric("test-name"));
        
        assertFalse(SecurityUtils.isAlphanumeric("test name"));
        assertFalse(SecurityUtils.isAlphanumeric("test@name"));
        assertFalse(SecurityUtils.isAlphanumeric("test/name"));
        assertFalse(SecurityUtils.isAlphanumeric(null));
        assertFalse(SecurityUtils.isAlphanumeric(""));
    }
    
    @Test
    void testIsValidProgramName() {
        assertTrue(SecurityUtils.isValidProgramName("CRECUST"));
        assertTrue(SecurityUtils.isValidProgramName("INQCUST"));
        assertTrue(SecurityUtils.isValidProgramName("TEST123"));
        assertTrue(SecurityUtils.isValidProgramName("A"));
        
        assertFalse(SecurityUtils.isValidProgramName("toolong12"));
        assertFalse(SecurityUtils.isValidProgramName("lower"));
        assertFalse(SecurityUtils.isValidProgramName("TEST_123"));
        assertFalse(SecurityUtils.isValidProgramName("TEST-123"));
        assertFalse(SecurityUtils.isValidProgramName(null));
        assertFalse(SecurityUtils.isValidProgramName(""));
    }
    
    @Test
    void testValidateFilePath_PathTraversalProtection(@TempDir Path tempDir) {
        Path validPath = SecurityUtils.validateFilePath(tempDir.toString(), "test.txt");
        assertNotNull(validPath);
        assertTrue(validPath.startsWith(tempDir));
        
        assertThrows(SecurityException.class, () -> 
            SecurityUtils.validateFilePath(tempDir.toString(), "../etc/passwd"));
        
        assertThrows(SecurityException.class, () -> 
            SecurityUtils.validateFilePath(tempDir.toString(), "..\\windows\\system32"));
        
        assertThrows(SecurityException.class, () -> 
            SecurityUtils.validateFilePath(tempDir.toString(), "/etc/passwd"));
        
        assertThrows(SecurityException.class, () -> 
            SecurityUtils.validateFilePath(tempDir.toString(), "test/subdir/file.txt"));
        
        assertThrows(SecurityException.class, () -> 
            SecurityUtils.validateFilePath(tempDir.toString(), "test<script>.txt"));
        
        assertThrows(SecurityException.class, () -> 
            SecurityUtils.validateFilePath(tempDir.toString(), null));
        
        assertThrows(SecurityException.class, () -> 
            SecurityUtils.validateFilePath(tempDir.toString(), ""));
    }
    
    @Test
    void testIsValidLength() {
        assertTrue(SecurityUtils.isValidLength("short"));
        assertTrue(SecurityUtils.isValidLength(null));
        assertTrue(SecurityUtils.isValidLength("", 10));
        
        assertFalse(SecurityUtils.isValidLength("x".repeat(1001)));
        assertFalse(SecurityUtils.isValidLength("x".repeat(11), 10));
        
        assertTrue(SecurityUtils.isValidLength("x".repeat(10), 10));
    }
    
    @Test
    void testSanitizeSqlLikePattern_SQLInjectionProtection() {
        assertEquals("test\\%value", SecurityUtils.sanitizeSqlLikePattern("test%value"));
        assertEquals("test\\_value", SecurityUtils.sanitizeSqlLikePattern("test_value"));
        assertEquals("test\\\\value", SecurityUtils.sanitizeSqlLikePattern("test\\value"));
        
        assertEquals("\\%\\%\\%", SecurityUtils.sanitizeSqlLikePattern("%%%"));
        assertEquals("\\_\\_\\_", SecurityUtils.sanitizeSqlLikePattern("___"));
        
        assertNull(SecurityUtils.sanitizeSqlLikePattern(null));
        
        assertEquals("normal", SecurityUtils.sanitizeSqlLikePattern("normal"));
    }
    
    @Test
    void testIsValidSortCode() {
        assertTrue(SecurityUtils.isValidSortCode("123456"));
        assertTrue(SecurityUtils.isValidSortCode("987654"));
        
        assertFalse(SecurityUtils.isValidSortCode("12345"));
        assertFalse(SecurityUtils.isValidSortCode("1234567"));
        assertFalse(SecurityUtils.isValidSortCode("abcdef"));
        assertFalse(SecurityUtils.isValidSortCode("12-34-56"));
        assertFalse(SecurityUtils.isValidSortCode(null));
    }
    
    @Test
    void testIsValidAccountNumber() {
        assertTrue(SecurityUtils.isValidAccountNumber("12345678"));
        assertTrue(SecurityUtils.isValidAccountNumber("98765432"));
        
        assertFalse(SecurityUtils.isValidAccountNumber("1234567"));
        assertFalse(SecurityUtils.isValidAccountNumber("123456789"));
        assertFalse(SecurityUtils.isValidAccountNumber("abcdefgh"));
        assertFalse(SecurityUtils.isValidAccountNumber("1234-5678"));
        assertFalse(SecurityUtils.isValidAccountNumber(null));
    }
}
