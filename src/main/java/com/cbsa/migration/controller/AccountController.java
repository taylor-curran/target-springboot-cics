package com.cbsa.migration.controller;

import com.cbsa.migration.dto.AccountDTO;
import com.cbsa.migration.dto.ErrorResponseDto;
import com.cbsa.migration.service.AccountService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.Instant;

/**
 * REST Controller for read-only account retrieval.
 * Migrated from the INQACC.cbl and INQACCCU.cbl COBOL programs.
 */
@RestController
@RequestMapping("/api/accounts")
@Tag(name = "Accounts", description = "Read-only account retrieval (INQACC, INQACCCU)")
public class AccountController {

    private final AccountService accountService;

    public AccountController(AccountService accountService) {
        this.accountService = accountService;
    }

    /**
     * Retrieve a single account. Corresponds to INQACC.cbl.
     */
    @GetMapping("/{accountNumber}")
    @Operation(
        summary = "Retrieve an account by account number",
        description = "Composite key lookup on sort code + account number. Account number 99999999 " +
                     "returns the highest-numbered account for the sort code, as in INQACC."
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Account found"),
        @ApiResponse(responseCode = "404", description = "Account not found")
    })
    public ResponseEntity<Object> getAccount(
            @PathVariable String accountNumber,
            @RequestParam(required = false) String sortCode) {

        return accountService.getAccount(sortCode, accountNumber)
                .<ResponseEntity<Object>>map(ResponseEntity::ok)
                .orElseGet(() -> notFound("Account not found: " + accountNumber));
    }

    /**
     * Retrieve the accounts of a customer with cursor-based retrieval.
     * Corresponds to INQACCCU.cbl.
     */
    @GetMapping("/customer/{customerNumber}")
    @Operation(
        summary = "Retrieve the accounts of a customer",
        description = "Cursor-based retrieval ordered by account number, capped at 20 accounts per " +
                     "customer as in INQACCCU. Pass nextCursor from the previous page to continue."
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Accounts retrieved"),
        @ApiResponse(responseCode = "404", description = "Customer not found")
    })
    public ResponseEntity<Object> getAccountsForCustomer(
            @PathVariable Long customerNumber,
            @RequestParam(required = false) String sortCode,
            @RequestParam(required = false) String cursor,
            @RequestParam(required = false) Integer limit) {

        return accountService.getAccountsForCustomer(customerNumber, sortCode, cursor, limit)
                .<ResponseEntity<Object>>map(ResponseEntity::ok)
                .orElseGet(() -> notFound("Customer not found: " + customerNumber));
    }

    private ResponseEntity<Object> notFound(String message) {
        return ResponseEntity.status(HttpStatus.NOT_FOUND)
                .body(ErrorResponseDto.failure(message, Instant.now().toString()));
    }
}
