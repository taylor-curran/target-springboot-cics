package com.cbsa.migration.controller;

import com.cbsa.migration.service.AccountUpdateService;
import com.cbsa.migration.service.AccountDisplayService;
import com.cbsa.migration.dto.AccountUpdateRequestDto;
import com.cbsa.migration.dto.AccountResponseDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * REST controller for Account operations.
 * Provides endpoints for account updates and display operations.
 * Migrated from UPDACC and BNK1DAC COBOL programs.
 */
@RestController
@RequestMapping("/api/accounts")
public class AccountController {

    private final AccountUpdateService accountUpdateService;
    private final AccountDisplayService accountDisplayService;

    @Autowired
    public AccountController(AccountUpdateService accountUpdateService, AccountDisplayService accountDisplayService) {
        this.accountUpdateService = accountUpdateService;
        this.accountDisplayService = accountDisplayService;
    }

    /**
     * Update account information.
     * Migrated from UPDACC COBOL program.
     * 
     * @param sortCode the sort code
     * @param accountNumber the account number
     * @param updateRequest the update request data
     * @return success/failure response
     */
    @PutMapping("/{sortCode}/{accountNumber}")
    public ResponseEntity<String> updateAccount(
            @PathVariable String sortCode,
            @PathVariable String accountNumber,
            @Valid @RequestBody AccountUpdateRequestDto updateRequest) {
        
        boolean success = accountUpdateService.updateAccount(sortCode, accountNumber, updateRequest);
        
        if (success) {
            return ResponseEntity.ok("Account updated successfully");
        } else {
            return ResponseEntity.badRequest().body("Failed to update account");
        }
    }

    /**
     * Get account information.
     * Migrated from BNK1DAC COBOL program.
     * 
     * @param sortCode the sort code
     * @param accountNumber the account number
     * @return account information or not found
     */
    @GetMapping("/{sortCode}/{accountNumber}")
    public ResponseEntity<AccountResponseDto> getAccount(
            @PathVariable String sortCode,
            @PathVariable String accountNumber) {
        
        AccountResponseDto account = accountDisplayService.displayAccount(sortCode, accountNumber);
        
        if (account != null) {
            return ResponseEntity.ok(account);
        } else {
            return ResponseEntity.notFound().build();
        }
    }
}
