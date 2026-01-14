package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * Response DTO for Transfer Funds operations.
 * Migrated from COBOL XFRFUN program.
 * 
 * Returns the result of the transfer including:
 * - Success/failure status
 * - Error code and message if failed
 * - New balances for both accounts after successful transfer
 * - Transaction reference for audit trail
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TransferResponseDto {
    
    /**
     * Whether the transfer was successful
     */
    private boolean success;
    
    /**
     * Error code if transfer failed:
     * - '1': FROM account not found
     * - '2': TO account not found
     * - '3': Unexpected error during transfer
     * - '4': Amount must be greater than zero (or invalid amount)
     */
    private String errorCode;
    
    /**
     * Human-readable error message if transfer failed
     */
    private String errorMessage;
    
    /**
     * Source account sort code
     */
    private String fromSortCode;
    
    /**
     * Source account number
     */
    private String fromAccountNumber;
    
    /**
     * New available balance of the FROM account after transfer
     */
    private BigDecimal fromAccountNewBalance;
    
    /**
     * Target account sort code
     */
    private String toSortCode;
    
    /**
     * Target account number
     */
    private String toAccountNumber;
    
    /**
     * New available balance of the TO account after transfer
     */
    private BigDecimal toAccountNewBalance;
    
    /**
     * Transfer amount
     */
    private BigDecimal amount;
    
    /**
     * Transaction reference number for audit trail
     */
    private Long transactionReference;
    
    /**
     * Timestamp of the transfer
     */
    private LocalDateTime transferTimestamp;
    
    /**
     * Factory method for successful transfer response
     */
    public static TransferResponseDto success(
            String fromSortCode, String fromAccountNumber, BigDecimal fromNewBalance,
            String toSortCode, String toAccountNumber, BigDecimal toNewBalance,
            BigDecimal amount, Long transactionReference) {
        return TransferResponseDto.builder()
                .success(true)
                .fromSortCode(fromSortCode)
                .fromAccountNumber(fromAccountNumber)
                .fromAccountNewBalance(fromNewBalance)
                .toSortCode(toSortCode)
                .toAccountNumber(toAccountNumber)
                .toAccountNewBalance(toNewBalance)
                .amount(amount)
                .transactionReference(transactionReference)
                .transferTimestamp(LocalDateTime.now())
                .build();
    }
    
    /**
     * Factory method for failed transfer response
     */
    public static TransferResponseDto failure(String errorCode, String errorMessage) {
        return TransferResponseDto.builder()
                .success(false)
                .errorCode(errorCode)
                .errorMessage(errorMessage)
                .transferTimestamp(LocalDateTime.now())
                .build();
    }
}
