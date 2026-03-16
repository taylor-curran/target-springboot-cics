package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Response DTO for the Create Account (CREACC) operation.
 * Returns the newly created account details or failure information.
 * Preserves the COBOL CREACC failure code semantics.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CreateAccountResponse {

    /**
     * The generated account number (8-digit zero-padded string).
     */
    private String accountNumber;

    /**
     * Branch sort code.
     */
    private String sortCode;

    /**
     * Customer number who owns the account.
     */
    private Long customerNumber;

    /**
     * Account type (ISA, MORTGAGE, SAVING, CURRENT, LOAN).
     */
    private String accountType;

    /**
     * Interest rate for the account.
     */
    private BigDecimal interestRate;

    /**
     * Date account was opened.
     */
    private LocalDate openedDate;

    /**
     * Overdraft limit.
     */
    private Integer overdraftLimit;

    /**
     * Date of last statement.
     */
    private LocalDate lastStatementDate;

    /**
     * Date of next statement.
     */
    private LocalDate nextStatementDate;

    /**
     * Available balance.
     */
    private BigDecimal availableBalance;

    /**
     * Actual balance.
     */
    private BigDecimal actualBalance;

    /**
     * Whether the account creation succeeded.
     */
    private boolean success;

    /**
     * Failure code preserving COBOL CREACC semantics:
     * '1' = customer not found,
     * '3' = enqueue/lock failed,
     * '5' = dequeue failed,
     * '7' = account insert failed,
     * '8' = 10+ accounts already exist,
     * '9' = error counting accounts,
     * 'A' = invalid account type.
     * Null on success.
     */
    private String failCode;

    /**
     * Human-readable failure message.
     */
    private String failMessage;

    /**
     * Factory method for a successful response.
     */
    public static CreateAccountResponse success(String accountNumber, String sortCode,
                                                 Long customerNumber, String accountType,
                                                 BigDecimal interestRate, LocalDate openedDate,
                                                 Integer overdraftLimit, LocalDate lastStatementDate,
                                                 LocalDate nextStatementDate, BigDecimal availableBalance,
                                                 BigDecimal actualBalance) {
        return CreateAccountResponse.builder()
                .accountNumber(accountNumber)
                .sortCode(sortCode)
                .customerNumber(customerNumber)
                .accountType(accountType)
                .interestRate(interestRate)
                .openedDate(openedDate)
                .overdraftLimit(overdraftLimit)
                .lastStatementDate(lastStatementDate)
                .nextStatementDate(nextStatementDate)
                .availableBalance(availableBalance)
                .actualBalance(actualBalance)
                .success(true)
                .build();
    }

    /**
     * Factory method for a failure response.
     */
    public static CreateAccountResponse failure(String failCode, String failMessage) {
        return CreateAccountResponse.builder()
                .success(false)
                .failCode(failCode)
                .failMessage(failMessage)
                .build();
    }
}
