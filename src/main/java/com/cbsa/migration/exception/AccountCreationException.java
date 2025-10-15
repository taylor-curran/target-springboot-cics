package com.cbsa.migration.exception;

public class AccountCreationException extends RuntimeException {
    private final String errorCode;
    
    public AccountCreationException(String message, String errorCode) {
        super(message);
        this.errorCode = errorCode;
    }
    
    public String getErrorCode() {
        return errorCode;
    }
}
