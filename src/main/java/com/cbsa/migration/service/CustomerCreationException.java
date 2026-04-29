package com.cbsa.migration.service;

/**
 * Exception thrown during customer creation that carries COBOL-equivalent fail codes.
 * Fail codes: O (too old/year<1601), Y (future DOB), Z (invalid date),
 * 1 (VSAM write failure), 3 (lock failure), 4 (PROCTRAN write failure),
 * 5 (DEQ failure), A-H (credit check infrastructure errors), G (credit check error).
 */
public class CustomerCreationException extends RuntimeException {

    private final String failCode;

    public CustomerCreationException(String failCode, String message) {
        super(message);
        this.failCode = failCode;
    }

    public CustomerCreationException(String failCode, String message, Throwable cause) {
        super(message, cause);
        this.failCode = failCode;
    }

    public String getFailCode() {
        return failCode;
    }
}
