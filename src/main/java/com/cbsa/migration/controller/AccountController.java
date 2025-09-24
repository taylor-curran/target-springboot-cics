package com.cbsa.migration.controller;

import com.cbsa.migration.dto.AccountCreationRequestDto;
import com.cbsa.migration.dto.AccountCreationResponseDto;
import com.cbsa.migration.dto.AccountInquiryResponseDto;
import com.cbsa.migration.dto.AccountUpdateRequestDto;
import com.cbsa.migration.dto.BalanceUpdateRequestDto;
import com.cbsa.migration.dto.BalanceUpdateResponseDto;
import com.cbsa.migration.service.AccountCreationService;
import com.cbsa.migration.service.AccountInquiryService;
import com.cbsa.migration.service.AccountMaintenanceService;
import com.cbsa.migration.service.BalanceManagementService;
import javax.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/accounts")
public class AccountController {

    private final AccountCreationService accountCreationService;
    private final AccountInquiryService accountInquiryService;
    private final AccountMaintenanceService accountMaintenanceService;
    private final BalanceManagementService balanceManagementService;

    @Autowired
    public AccountController(AccountCreationService accountCreationService,
                           AccountInquiryService accountInquiryService,
                           AccountMaintenanceService accountMaintenanceService,
                           BalanceManagementService balanceManagementService) {
        this.accountCreationService = accountCreationService;
        this.accountInquiryService = accountInquiryService;
        this.accountMaintenanceService = accountMaintenanceService;
        this.balanceManagementService = balanceManagementService;
    }

    @PostMapping
    public ResponseEntity<AccountCreationResponseDto> createAccount(@Valid @RequestBody AccountCreationRequestDto request) {
        AccountCreationResponseDto response = accountCreationService.createAccount(request);
        
        if (response.isSuccess()) {
            return ResponseEntity.status(HttpStatus.CREATED).body(response);
        } else {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

    @GetMapping("/{sortCode}/{accountNumber}")
    public ResponseEntity<AccountInquiryResponseDto> inquireAccount(@PathVariable String sortCode, 
                                                                   @PathVariable String accountNumber) {
        AccountInquiryResponseDto response = accountInquiryService.inquireAccount(sortCode, accountNumber);
        
        if (response.isSuccess()) {
            return ResponseEntity.ok(response);
        } else {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(response);
        }
    }

    @PutMapping("/{sortCode}/{accountNumber}")
    public ResponseEntity<AccountInquiryResponseDto> updateAccount(@PathVariable String sortCode,
                                                                  @PathVariable String accountNumber,
                                                                  @Valid @RequestBody AccountUpdateRequestDto request) {
        AccountInquiryResponseDto response = accountMaintenanceService.updateAccount(sortCode, accountNumber, request);
        
        if (response.isSuccess()) {
            return ResponseEntity.ok(response);
        } else {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

    @PostMapping("/{sortCode}/{accountNumber}/balance")
    public ResponseEntity<BalanceUpdateResponseDto> updateBalance(@PathVariable String sortCode,
                                                                 @PathVariable String accountNumber,
                                                                 @Valid @RequestBody BalanceUpdateRequestDto request) {
        BalanceUpdateResponseDto response = balanceManagementService.updateBalance(sortCode, accountNumber, request);
        
        if (response.isSuccess()) {
            return ResponseEntity.ok(response);
        } else {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }
}
