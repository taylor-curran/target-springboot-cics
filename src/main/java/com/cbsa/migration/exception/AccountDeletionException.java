package com.cbsa.migration.exception;

public class AccountDeletionException extends RuntimeException {
    private final String errorCode;
    
    public AccountDeletionException(String message, String errorCode) {
        super(message);
        this.errorCode = errorCode;
    }
    
    public String getErrorCode() {
        return errorCode;
    }
}
