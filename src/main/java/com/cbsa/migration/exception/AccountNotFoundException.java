package com.cbsa.migration.exception;

public class AccountNotFoundException extends RuntimeException {
    private final String errorCode;
    
    public AccountNotFoundException(String message) {
        super(message);
        this.errorCode = "1";
    }
    
    public AccountNotFoundException(String message, String errorCode) {
        super(message);
        this.errorCode = errorCode;
    }
    
    public String getErrorCode() {
        return errorCode;
    }
}
