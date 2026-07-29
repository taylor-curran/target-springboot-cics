package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * Account read DTO mirroring the INQACC / INQACCCU COMMAREA layout.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccountDTO {

    /** ACCOUNT-EYE-CATCHER PIC X(4) */
    private String eyeCatcher;

    /** ACCOUNT-CUST-NO PIC 9(10) */
    private Long customerNumber;

    /** ACCOUNT-SORT-CODE PIC 9(6) */
    private String sortCode;

    /** ACCOUNT-NUMBER PIC 9(8) */
    private String accountNumber;

    /** ACCOUNT-TYPE PIC X(8) */
    private String accountType;

    /** ACCOUNT-INTEREST-RATE PIC 9(4)V99 */
    private BigDecimal interestRate;

    /** ACCOUNT-OPENED PIC 9(8) */
    private LocalDate openedDate;

    /** ACCOUNT-OVERDRAFT-LIMIT PIC 9(8) */
    private Integer overdraftLimit;

    /** ACCOUNT-LAST-STMT-DATE PIC 9(8) */
    private LocalDate lastStatementDate;

    /** ACCOUNT-NEXT-STMT-DATE PIC 9(8) */
    private LocalDate nextStatementDate;

    /** ACCOUNT-AVAILABLE-BALANCE PIC S9(10)V99 */
    private BigDecimal availableBalance;

    /** ACCOUNT-ACTUAL-BALANCE PIC S9(10)V99 */
    private BigDecimal actualBalance;

    /**
     * Cursor-based page of accounts for one customer, equivalent to the
     * INQACCCU COMMAREA (ACCOUNT-DETAILS OCCURS 1 TO 20).
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AccountListDTO {

        /** CUSTOMER-NUMBER PIC 9(10) */
        private Long customerNumber;

        /** NUMBER-OF-ACCOUNTS PIC S9(8) BINARY - accounts in this page */
        private int numberOfAccounts;

        private List<AccountDTO> accounts;

        /**
         * Account number of the last entry in this page; pass it back as the
         * cursor to continue the browse. Null once the cursor is exhausted.
         */
        private String nextCursor;

        /** False once the legacy cursor terminates (no more rows, or cap reached) */
        private boolean hasMore;

        /** Legacy cap of 20 accounts per customer retrieval */
        private int maxAccountsPerCustomer;
    }
}
