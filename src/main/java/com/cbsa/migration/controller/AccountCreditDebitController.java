package com.cbsa.migration.controller;

import com.cbsa.migration.dto.DebitCreditRequest;
import com.cbsa.migration.dto.DebitCreditResponse;
import com.cbsa.migration.service.DebitCreditService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

/**
 * REST Controller replacing BNK1CRA.cbl (the BMS screen handler for the OCRA transaction).
 *
 * BNK1CRA was the CICS BMS program that:
 * <ul>
 *   <li>Received input from the 3270 map (account number, sign +/-, amount)</li>
 *   <li>Validated numeric fields and sign indicator</li>
 *   <li>Populated the PAYDBCR COMMAREA</li>
 *   <li>Called DBCRFUN via EXEC CICS LINK</li>
 *   <li>Displayed results back on the BMS screen</li>
 * </ul>
 *
 * This controller replaces the BMS screen interaction with REST endpoints.
 * The business logic (DBCRFUN) is delegated to {@link DebitCreditService}.
 */
@RestController
@RequestMapping("/api/accounts")
public class AccountCreditDebitController {

    private static final Logger log = LoggerFactory.getLogger(AccountCreditDebitController.class);

    private final DebitCreditService debitCreditService;

    public AccountCreditDebitController(DebitCreditService debitCreditService) {
        this.debitCreditService = debitCreditService;
    }

    /**
     * Credit an account with the specified amount.
     *
     * Replaces BNK1CRA's handling of sign '+' combined with DBCRFUN's credit path.
     * The amount in the request body is always positive; it is applied as a credit
     * (positive adjustment to actual and available balances).
     *
     * @param accountNumber 8-digit account number (path variable)
     * @param request       the credit request containing amount and origin info
     * @return response with updated balances or error information
     */
    @PutMapping("/{accountNumber}/credit")
    public ResponseEntity<DebitCreditResponse> creditAccount(
            @PathVariable String accountNumber,
            @Valid @RequestBody DebitCreditRequest request) {

        log.info("Credit request: account={}, amount={}, facilityType={}",
                accountNumber, request.getAmount(), request.getFacilityType());

        DebitCreditResponse response = debitCreditService.creditAccount(accountNumber, request);

        if (response.isSuccess()) {
            return ResponseEntity.ok(response);
        } else {
            return mapFailCodeToResponse(response);
        }
    }

    /**
     * Debit an account by the specified amount.
     *
     * Replaces BNK1CRA's handling of sign '-' combined with DBCRFUN's debit path.
     * The amount in the request body is always positive; the service negates it
     * internally to apply as a debit (negative adjustment to balances).
     *
     * @param accountNumber 8-digit account number (path variable)
     * @param request       the debit request containing amount and origin info
     * @return response with updated balances or error information
     */
    @PutMapping("/{accountNumber}/debit")
    public ResponseEntity<DebitCreditResponse> debitAccount(
            @PathVariable String accountNumber,
            @Valid @RequestBody DebitCreditRequest request) {

        log.info("Debit request: account={}, amount={}, facilityType={}",
                accountNumber, request.getAmount(), request.getFacilityType());

        DebitCreditResponse response = debitCreditService.debitAccount(accountNumber, request);

        if (response.isSuccess()) {
            return ResponseEntity.ok(response);
        } else {
            return mapFailCodeToResponse(response);
        }
    }

    /**
     * Maps DBCRFUN fail codes to appropriate HTTP status codes.
     *
     * Fail codes from COBOL:
     * <pre>
     *   '1' -> 404 Not Found (account does not exist)
     *   '2' -> 500 Internal Server Error (unexpected DB error)
     *   '3' -> 400 Bad Request (insufficient funds)
     *   '4' -> 400 Bad Request (MORTGAGE/LOAN restriction)
     * </pre>
     */
    private ResponseEntity<DebitCreditResponse> mapFailCodeToResponse(DebitCreditResponse response) {
        switch (response.getFailCode()) {
            case "1":
                return ResponseEntity.status(404).body(response);
            case "2":
                return ResponseEntity.status(500).body(response);
            case "3":
            case "4":
                return ResponseEntity.badRequest().body(response);
            default:
                return ResponseEntity.status(500).body(response);
        }
    }
}
