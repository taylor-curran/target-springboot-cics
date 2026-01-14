package com.cbsa.migration.controller;

import com.cbsa.migration.dto.TransferRequestDto;
import com.cbsa.migration.dto.TransferResponseDto;
import com.cbsa.migration.service.TransferFundsService;
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

@RestController
@RequestMapping("/api/transfer")
@Validated
@Tag(name = "Transfer Funds", description = "Fund transfer operations between accounts")
public class TransferFundsController {

    private static final Logger logger = LoggerFactory.getLogger(TransferFundsController.class);

    private final TransferFundsService transferFundsService;

    public TransferFundsController(TransferFundsService transferFundsService) {
        this.transferFundsService = transferFundsService;
    }

    @PostMapping
    @Operation(
        summary = "Transfer funds between accounts",
        description = "Transfers funds from one account to another within the same bank. " +
                     "Both accounts must exist and the FROM account must have sufficient balance. " +
                     "Migrated from COBOL programs XFRFUN.cbl and BNK1TFN.cbl."
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Transfer completed successfully"),
        @ApiResponse(responseCode = "400", description = "Invalid request - validation failed or business rule violation"),
        @ApiResponse(responseCode = "404", description = "Account not found"),
        @ApiResponse(responseCode = "500", description = "Internal server error during transfer")
    })
    public ResponseEntity<TransferResponseDto> transferFunds(
            @Parameter(description = "Transfer request with FROM/TO accounts and amount", required = true)
            @Valid @RequestBody TransferRequestDto request) {
        
        logger.info("Received transfer request: {} {} -> {} {} amount={}", 
                   request.getFromSortCode(), request.getFromAccountNumber(),
                   request.getToSortCode(), request.getToAccountNumber(),
                   request.getAmount());

        TransferResponseDto response = transferFundsService.transferFunds(request);

        if (response.isSuccess()) {
            logger.info("Transfer successful: {} {} -> {} {}", 
                       request.getFromSortCode(), request.getFromAccountNumber(),
                       request.getToSortCode(), request.getToAccountNumber());
            return ResponseEntity.ok(response);
        } else {
            HttpStatus status = determineHttpStatus(response.getFailCode());
            logger.warn("Transfer failed: {} - {} (HTTP {})", 
                       response.getFailCode(), response.getErrorMessage(), status.value());
            return ResponseEntity.status(status).body(response);
        }
    }

    private HttpStatus determineHttpStatus(String failCode) {
        if (failCode == null) {
            return HttpStatus.INTERNAL_SERVER_ERROR;
        }
        
        switch (failCode) {
            case "1":
            case "2":
                return HttpStatus.NOT_FOUND;
            case "4":
                return HttpStatus.BAD_REQUEST;
            case "3":
            default:
                return HttpStatus.INTERNAL_SERVER_ERROR;
        }
    }
}
