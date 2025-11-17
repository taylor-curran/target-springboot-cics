package com.cbsa.migration.util;

import org.springframework.web.util.HtmlUtils;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Pattern;

/**
 * Security utility class for input validation and output encoding.
 * Provides protection against XSS, SQL injection, and path traversal attacks.
 */
public class SecurityUtils {
    
    private static final Pattern ALPHANUMERIC_PATTERN = Pattern.compile("^[a-zA-Z0-9_-]+$");
    private static final Pattern PROGRAM_NAME_PATTERN = Pattern.compile("^[A-Z0-9]{1,8}$");
    private static final int MAX_STRING_LENGTH = 1000;
    
    /**
     * Sanitize HTML output to prevent XSS attacks.
     * Encodes special HTML characters to their entity equivalents.
     * 
     * @param input the input string to sanitize
     * @return sanitized string safe for HTML output
     */
    public static String sanitizeForHtml(String input) {
        if (input == null) {
            return null;
        }
        return HtmlUtils.htmlEscape(input);
    }
    
    /**
     * Validate that a string contains only alphanumeric characters, underscores, and hyphens.
     * 
     * @param input the input string to validate
     * @return true if valid, false otherwise
     */
    public static boolean isAlphanumeric(String input) {
        if (input == null || input.isEmpty()) {
            return false;
        }
        return ALPHANUMERIC_PATTERN.matcher(input).matches();
    }
    
    /**
     * Validate program name format (COBOL naming convention).
     * Must be 1-8 uppercase alphanumeric characters.
     * 
     * @param programName the program name to validate
     * @return true if valid, false otherwise
     */
    public static boolean isValidProgramName(String programName) {
        if (programName == null || programName.isEmpty()) {
            return false;
        }
        return PROGRAM_NAME_PATTERN.matcher(programName).matches();
    }
    
    /**
     * Validate and sanitize file path to prevent path traversal attacks.
     * Ensures the path does not contain directory traversal sequences.
     * 
     * @param basePath the base directory path
     * @param fileName the file name to validate
     * @return sanitized absolute path
     * @throws SecurityException if path traversal is detected
     */
    public static Path validateFilePath(String basePath, String fileName) throws SecurityException {
        if (fileName == null || fileName.isEmpty()) {
            throw new SecurityException("File name cannot be null or empty");
        }
        
        if (fileName.contains("..") || fileName.contains("/") || fileName.contains("\\")) {
            throw new SecurityException("Path traversal detected in file name: " + fileName);
        }
        
        if (!fileName.matches("^[a-zA-Z0-9_.-]+$")) {
            throw new SecurityException("Invalid characters in file name: " + fileName);
        }
        
        try {
            Path base = Paths.get(basePath).toAbsolutePath().normalize();
            Path resolved = base.resolve(fileName).normalize();
            
            if (!resolved.startsWith(base)) {
                throw new SecurityException("Path traversal detected: resolved path is outside base directory");
            }
            
            return resolved;
        } catch (Exception e) {
            throw new SecurityException("Invalid file path: " + e.getMessage(), e);
        }
    }
    
    /**
     * Validate string length to prevent buffer overflow or DoS attacks.
     * 
     * @param input the input string to validate
     * @param maxLength maximum allowed length
     * @return true if valid, false otherwise
     */
    public static boolean isValidLength(String input, int maxLength) {
        if (input == null) {
            return true;
        }
        return input.length() <= maxLength;
    }
    
    /**
     * Validate string length with default maximum.
     * 
     * @param input the input string to validate
     * @return true if valid, false otherwise
     */
    public static boolean isValidLength(String input) {
        return isValidLength(input, MAX_STRING_LENGTH);
    }
    
    /**
     * Sanitize SQL LIKE pattern wildcards to prevent SQL injection.
     * Escapes special characters used in LIKE patterns (%, _, \).
     * 
     * @param input the input string to sanitize
     * @return sanitized string safe for SQL LIKE patterns
     */
    public static String sanitizeSqlLikePattern(String input) {
        if (input == null) {
            return null;
        }
        return input.replace("\\", "\\\\")
                    .replace("%", "\\%")
                    .replace("_", "\\_");
    }
    
    /**
     * Validate sort code format (6 digits).
     * 
     * @param sortCode the sort code to validate
     * @return true if valid, false otherwise
     */
    public static boolean isValidSortCode(String sortCode) {
        if (sortCode == null) {
            return false;
        }
        return sortCode.matches("^\\d{6}$");
    }
    
    /**
     * Validate account number format (8 digits).
     * 
     * @param accountNumber the account number to validate
     * @return true if valid, false otherwise
     */
    public static boolean isValidAccountNumber(String accountNumber) {
        if (accountNumber == null) {
            return false;
        }
        return accountNumber.matches("^\\d{8}$");
    }
}
