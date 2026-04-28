package com.cbsa.migration.controller;

import com.cbsa.migration.dto.DebitCreditRequestDto;
import com.cbsa.migration.dto.DebitCreditResponseDto;
import com.cbsa.migration.service.CreditDebitService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

/**
 * Credit/Debit Controller - REST API for account credit and debit operations.
 * Migrated from COBOL programs BNK1CRA.cbl (presentation) and DBCRFUN.cbl (business logic).
 *
 * Provides REST endpoint equivalent to CICS OCRA transaction.
 */
@RestController
@RequestMapping("/api/accounts")
@Validated
@Tag(name = "Credit/Debit", description = "Account credit and debit operations (OCRA transaction)")
public class CreditDebitController {

    private static final Logger logger = LoggerFactory.getLogger(CreditDebitController.class);

    private final CreditDebitService creditDebitService;

    public CreditDebitController(CreditDebitService creditDebitService) {
        this.creditDebitService = creditDebitService;
    }

    /**
     * Process a credit or debit operation on an account.
     * Equivalent to COBOL BNK1CRA PROCESS-MAP → DBCRFUN UPDATE-ACCOUNT-DB2.
     *
     * @param request the debit/credit request with account number, sign, and amount
     * @return response with updated balances or failure details
     */
    @PostMapping("/credit-debit")
    @Operation(
        summary = "Process credit or debit on an account",
        description = "Applies a credit (+) or debit (-) to the specified account. " +
                     "Validates input, checks business rules (MORTGAGE/LOAN restrictions, " +
                     "insufficient funds for payment channel), updates balances atomically, " +
                     "and writes a PROCTRAN audit record. " +
                     "Equivalent to COBOL OCRA transaction (BNK1CRA + DBCRFUN)."
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Credit/debit applied successfully"),
        @ApiResponse(responseCode = "400", description = "Invalid request data or business rule violation"),
        @ApiResponse(responseCode = "404", description = "Account not found"),
        @ApiResponse(responseCode = "409", description = "Insufficient funds (payment channel only)"),
        @ApiResponse(responseCode = "500", description = "Internal processing error")
    })
    public ResponseEntity<DebitCreditResponseDto> creditDebit(
            @Parameter(description = "Credit/debit request with account number, sign, and amount", required = true)
            @Valid @RequestBody DebitCreditRequestDto request) {

        logger.info("Received credit/debit request: account={}, sign={}, amount={}, channel={}",
                request.getAccountNumber(), request.getSign(), request.getAmount(), request.getChannelType());

        // Additional validation: account number must not be all zeros
        if ("00000000".equals(request.getAccountNumber())) {
            DebitCreditResponseDto errorResponse = DebitCreditResponseDto.builder()
                    .success(false)
                    .failCode("V")
                    .message("Please enter a non-zero account number")
                    .build();
            return ResponseEntity.badRequest().body(errorResponse);
        }

        try {
            DebitCreditResponseDto response = creditDebitService.debitCreditAccount(request);

            if (response.isSuccess()) {
                return ResponseEntity.ok(response);
            }

            HttpStatus status = mapFailCodeToHttpStatus(response.getFailCode());
            return ResponseEntity.status(status).body(response);

        } catch (Exception e) {
            logger.error("Unexpected error processing credit/debit for account {}: {}",
                    request.getAccountNumber(), e.getMessage(), e);

            DebitCreditResponseDto errorResponse = DebitCreditResponseDto.builder()
                    .success(false)
                    .failCode("2")
                    .message("Amount could not be applied due to an unexpected error")
                    .build();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorResponse);
        }
    }

    /**
     * Map COBOL fail codes to HTTP status codes.
     */
    private HttpStatus mapFailCodeToHttpStatus(String failCode) {
        if (failCode == null) {
            return HttpStatus.INTERNAL_SERVER_ERROR;
        }
        switch (failCode) {
            case "1":
                return HttpStatus.NOT_FOUND;
            case "2":
                return HttpStatus.INTERNAL_SERVER_ERROR;
            case "3":
                return HttpStatus.CONFLICT;
            case "4":
                return HttpStatus.BAD_REQUEST;
            default:
                return HttpStatus.INTERNAL_SERVER_ERROR;
        }
    }
}
