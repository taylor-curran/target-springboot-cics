package com.cbsa.migration.controller;

import com.cbsa.migration.dto.AccountRequestDto;
import com.cbsa.migration.dto.AccountResponseDto;
import com.cbsa.migration.service.AccountService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * REST Controller for Account Management Operations
 * Migrated from COBOL programs handling account creation, maintenance, balance inquiries, and overdraft processing
 */
@Tag(name = "Account Management", description = "API endpoints for account operations migrated from COBOL programs")
@RestController
@RequestMapping("/api/accounts")
public class AccountController {

    private final AccountService accountService;

    public AccountController(AccountService accountService) {
        this.accountService = accountService;
    }

    /**
     * Create a new account
     * Equivalent to COBOL account creation program
     */
    @Operation(summary = "Create new account", description = "Create a new bank account for an existing customer")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "201", description = "Account created successfully", 
                    content = {@Content(mediaType = "application/json", schema = @Schema(implementation = AccountResponseDto.class))}),
        @ApiResponse(responseCode = "400", description = "Invalid request data"),
        @ApiResponse(responseCode = "404", description = "Customer not found")
    })
    @PostMapping
    public ResponseEntity<AccountResponseDto> createAccount(@Valid @RequestBody AccountRequestDto request) {
        try {
            AccountResponseDto response = accountService.createAccount(request);
            return ResponseEntity.status(HttpStatus.CREATED).body(response);
        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest().build();
        }
    }

    /**
     * Update an existing account
     * Equivalent to COBOL account maintenance program
     */
    @Operation(summary = "Update account", description = "Update account details such as type, interest rate, or overdraft limit")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Account updated successfully", 
                    content = {@Content(mediaType = "application/json", schema = @Schema(implementation = AccountResponseDto.class))}),
        @ApiResponse(responseCode = "400", description = "Invalid request data"),
        @ApiResponse(responseCode = "404", description = "Account not found")
    })
    @PutMapping("/{sortCode}/{accountNumber}")
    public ResponseEntity<AccountResponseDto> updateAccount(
            @Parameter(description = "Bank sort code") @PathVariable String sortCode,
            @Parameter(description = "Account number") @PathVariable String accountNumber,
            @Valid @RequestBody AccountRequestDto request) {
        try {
            AccountResponseDto response = accountService.updateAccount(sortCode, accountNumber, request);
            return ResponseEntity.ok(response);
        } catch (IllegalArgumentException e) {
            return ResponseEntity.notFound().build();
        }
    }

    /**
     * Get account balance information
     * Equivalent to COBOL balance inquiry program
     */
    @Operation(summary = "Get account balance", description = "Retrieve current available and actual balance for an account")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Balance retrieved successfully", 
                    content = {@Content(mediaType = "application/json", schema = @Schema(implementation = AccountResponseDto.class))}),
        @ApiResponse(responseCode = "404", description = "Account not found")
    })
    @GetMapping("/{sortCode}/{accountNumber}/balance")
    public ResponseEntity<AccountResponseDto> getAccountBalance(
            @Parameter(description = "Bank sort code") @PathVariable String sortCode,
            @Parameter(description = "Account number") @PathVariable String accountNumber) {
        try {
            AccountResponseDto response = accountService.getAccountBalance(sortCode, accountNumber);
            return ResponseEntity.ok(response);
        } catch (IllegalArgumentException e) {
            return ResponseEntity.notFound().build();
        }
    }

    /**
     * Process overdraft transaction
     * Equivalent to COBOL overdraft processing program
     */
    @Operation(summary = "Process overdraft", description = "Process an overdraft transaction within the account's overdraft limit")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Overdraft processed successfully", 
                    content = {@Content(mediaType = "application/json", schema = @Schema(implementation = AccountResponseDto.class))}),
        @ApiResponse(responseCode = "400", description = "Invalid amount or overdraft exceeds limit"),
        @ApiResponse(responseCode = "404", description = "Account not found")
    })
    @PostMapping("/{sortCode}/{accountNumber}/overdraft")
    public ResponseEntity<AccountResponseDto> processOverdraft(
            @Parameter(description = "Bank sort code") @PathVariable String sortCode,
            @Parameter(description = "Account number") @PathVariable String accountNumber,
            @RequestBody Map<String, BigDecimal> request) {
        try {
            BigDecimal amount = request.get("amount");
            if (amount == null) {
                return ResponseEntity.badRequest().build();
            }
            AccountResponseDto response = accountService.processOverdraft(sortCode, accountNumber, amount);
            return ResponseEntity.ok(response);
        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest().build();
        }
    }

    /**
     * Get full account details
     * Equivalent to COBOL account inquiry program
     */
    @Operation(summary = "Get account details", description = "Retrieve complete account information including balances and settings")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Account details retrieved successfully", 
                    content = {@Content(mediaType = "application/json", schema = @Schema(implementation = AccountResponseDto.class))}),
        @ApiResponse(responseCode = "404", description = "Account not found")
    })
    @GetMapping("/{sortCode}/{accountNumber}")
    public ResponseEntity<AccountResponseDto> getAccountDetails(
            @Parameter(description = "Bank sort code") @PathVariable String sortCode,
            @Parameter(description = "Account number") @PathVariable String accountNumber) {
        try {
            AccountResponseDto response = accountService.getAccountDetails(sortCode, accountNumber);
            return ResponseEntity.ok(response);
        } catch (IllegalArgumentException e) {
            return ResponseEntity.notFound().build();
        }
    }

    /**
     * Get all accounts for a customer
     * Equivalent to COBOL customer account listing program
     */
    @Operation(summary = "Get customer accounts", description = "Retrieve all accounts owned by a specific customer")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Customer accounts retrieved successfully", 
                    content = {@Content(mediaType = "application/json", schema = @Schema(implementation = List.class))})
    })
    @GetMapping("/customer/{customerNumber}")
    public ResponseEntity<List<AccountResponseDto>> getAccountsByCustomer(
            @Parameter(description = "Customer number") @PathVariable Long customerNumber) {
        List<AccountResponseDto> response = accountService.getAccountsByCustomer(customerNumber);
        return ResponseEntity.ok(response);
    }
}
