package com.cbsa.migration.controller;

import com.cbsa.migration.dto.TransferRequestDto;
import com.cbsa.migration.dto.TransferResponseDto;
import com.cbsa.migration.service.TransferService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

/**
 * REST Controller for Transfer Funds operations.
 * Migrated from COBOL XFRFUN program (BNK1TFN.cbl interface).
 * 
 * This controller provides endpoints for transferring funds between accounts
 * within the same bank. The accounts can belong to the same or different customers.
 * 
 * Error Codes returned in response:
 * - '1': FROM account not found
 * - '2': TO account not found
 * - '3': Unexpected error during transfer
 * - '4': Amount must be greater than zero (or invalid amount)
 * - '5': Insufficient funds in FROM account
 * - '6': FROM and TO accounts must be different
 * - '7': Invalid account number (00000000)
 */
@Tag(name = "Transfer", description = "API endpoints for fund transfer operations (migrated from XFRFUN COBOL program)")
@RestController
@RequestMapping("/api/transfer")
public class TransferController {

    private final TransferService transferService;

    public TransferController(TransferService transferService) {
        this.transferService = transferService;
    }

    /**
     * Transfer funds from one account to another.
     * 
     * This endpoint implements the functionality previously provided by the
     * XFRFUN COBOL program. It transfers funds between two accounts within
     * the same bank, updating both account balances atomically.
     * 
     * Input Validation Rules:
     * - FROM account number: Must be numeric and not '00000000'
     * - TO account number: Must be numeric and not '00000000'
     * - Different accounts: FROM and TO accounts must be different
     * - Amount: Must be greater than zero
     * 
     * Business Rules:
     * - Both FROM and TO accounts must exist
     * - FROM account must have sufficient available balance (including overdraft)
     * - Both account updates are atomic (succeed or fail together)
     * - All successful transfers are recorded for audit purposes
     * 
     * @param request the transfer request containing source account, target account, and amount
     * @return ResponseEntity with the transfer result including new balances or error details
     */
    @Operation(
        summary = "Transfer funds between accounts",
        description = "Transfer funds from one account to another within the same bank. " +
                     "Migrated from COBOL XFRFUN program. Both account updates are atomic."
    )
    @ApiResponses(value = {
        @ApiResponse(
            responseCode = "200",
            description = "Transfer completed successfully",
            content = @Content(mediaType = "application/json", 
                             schema = @Schema(implementation = TransferResponseDto.class))
        ),
        @ApiResponse(
            responseCode = "400",
            description = "Invalid request - validation failed or business rule violation",
            content = @Content(mediaType = "application/json", 
                             schema = @Schema(implementation = TransferResponseDto.class))
        ),
        @ApiResponse(
            responseCode = "404",
            description = "Account not found",
            content = @Content(mediaType = "application/json", 
                             schema = @Schema(implementation = TransferResponseDto.class))
        ),
        @ApiResponse(
            responseCode = "500",
            description = "Unexpected error during transfer",
            content = @Content(mediaType = "application/json", 
                             schema = @Schema(implementation = TransferResponseDto.class))
        )
    })
    @PostMapping
    public ResponseEntity<TransferResponseDto> transferFunds(@Valid @RequestBody TransferRequestDto request) {
        TransferResponseDto response = transferService.transferFunds(request);
        
        if (response.isSuccess()) {
            return ResponseEntity.ok(response);
        }
        
        // Map error codes to appropriate HTTP status codes
        HttpStatus status = mapErrorCodeToHttpStatus(response.getErrorCode());
        return ResponseEntity.status(status).body(response);
    }

    /**
     * Map error codes to appropriate HTTP status codes.
     * 
     * @param errorCode the error code from the transfer service
     * @return the appropriate HTTP status code
     */
    private HttpStatus mapErrorCodeToHttpStatus(String errorCode) {
        if (errorCode == null) {
            return HttpStatus.INTERNAL_SERVER_ERROR;
        }
        
        switch (errorCode) {
            case "1": // FROM account not found
            case "2": // TO account not found
                return HttpStatus.NOT_FOUND;
            case "3": // Unexpected error
                return HttpStatus.INTERNAL_SERVER_ERROR;
            case "4": // Invalid amount
            case "5": // Insufficient funds
            case "6": // Same account
            case "7": // Invalid account number (00000000)
                return HttpStatus.BAD_REQUEST;
            default:
                return HttpStatus.INTERNAL_SERVER_ERROR;
        }
    }
}
