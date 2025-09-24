package com.cbsa.migration.controller;

import com.cbsa.migration.model.AccountCreationRequest;
import com.cbsa.migration.model.AccountCreationResponse;
import com.cbsa.migration.model.DebitCreditRequest;
import com.cbsa.migration.model.DebitCreditResponse;
import com.cbsa.migration.service.AccountCreationService;
import com.cbsa.migration.service.DebitCreditService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

/**
 * REST Controller for account operations migrated from COBOL.
 * This controller provides endpoints for functionality previously
 * implemented in CREACC.cbl and DBCRFUN.cbl COBOL programs.
 */
@RestController
@RequestMapping("/api/account")
public class AccountController {

    private final AccountCreationService accountCreationService;
    private final DebitCreditService debitCreditService;

    @Autowired
    public AccountController(AccountCreationService accountCreationService, DebitCreditService debitCreditService) {
        this.accountCreationService = accountCreationService;
        this.debitCreditService = debitCreditService;
    }

    /**
     * Endpoint to create a new account.
     * Corresponds to CREACC.cbl COBOL program.
     *
     * @param request The account creation request
     * @return ResponseEntity with account creation result
     */
    @PostMapping("/create")
    public ResponseEntity<AccountCreationResponse> createAccount(@Valid @RequestBody AccountCreationRequest request) {
        AccountCreationResponse response = accountCreationService.createAccount(request);
        
        if (response.isSuccess()) {
            return ResponseEntity.ok(response);
        } else {
            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * Endpoint to process debit/credit transactions.
     * Corresponds to DBCRFUN.cbl COBOL program.
     *
     * @param request The debit/credit request
     * @return ResponseEntity with transaction result and updated balances
     */
    @PostMapping("/debit-credit")
    public ResponseEntity<DebitCreditResponse> processDebitCredit(@Valid @RequestBody DebitCreditRequest request) {
        DebitCreditResponse response = debitCreditService.processTransaction(request);
        
        if (response.isSuccess()) {
            return ResponseEntity.ok(response);
        } else {
            return ResponseEntity.badRequest().body(response);
        }
    }
}
