package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CreateAccountRequest;
import com.cbsa.migration.dto.CreateAccountResponse;
import com.cbsa.migration.service.AccountCreationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

/**
 * REST Controller for Account operations.
 * Migrated from COBOL CREACC program.
 */
@RestController
@RequestMapping("/api/accounts")
@Tag(name = "Accounts", description = "Bank account management API")
public class AccountController {

    private final AccountCreationService accountCreationService;

    @Autowired
    public AccountController(AccountCreationService accountCreationService) {
        this.accountCreationService = accountCreationService;
    }

    /**
     * Create a new bank account for an existing customer.
     * Equivalent to COBOL CREACC program functionality.
     *
     * @param request the account creation request
     * @return 201 Created with account details on success, or appropriate error status with fail code
     */
    @PostMapping
    @Operation(summary = "Create bank account",
               description = "Create a new bank account for an existing customer (migrated from CREACC COBOL program)")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "201", description = "Account successfully created"),
        @ApiResponse(responseCode = "400", description = "Invalid request (bad account type, missing fields)"),
        @ApiResponse(responseCode = "404", description = "Customer not found"),
        @ApiResponse(responseCode = "409", description = "Business rule violation (too many accounts)"),
        @ApiResponse(responseCode = "500", description = "Internal error (insert failed, lock failed)")
    })
    public ResponseEntity<CreateAccountResponse> createAccount(
            @Valid @RequestBody CreateAccountRequest request) {
        CreateAccountResponse response = accountCreationService.createAccount(request);

        if (response.isSuccess()) {
            return ResponseEntity.status(HttpStatus.CREATED).body(response);
        }

        // Map COBOL fail codes to appropriate HTTP status codes
        String failCode = response.getFailCode();
        if (failCode != null) {
            switch (failCode) {
                case "1":
                    // Customer not found
                    return ResponseEntity.status(HttpStatus.NOT_FOUND).body(response);
                case "A":
                    // Invalid account type
                    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
                case "8":
                    // Too many accounts
                    return ResponseEntity.status(HttpStatus.CONFLICT).body(response);
                case "3":
                case "5":
                case "7":
                case "9":
                    // Lock/insert/count failures
                    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(response);
                default:
                    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(response);
            }
        }

        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(response);
    }
}
