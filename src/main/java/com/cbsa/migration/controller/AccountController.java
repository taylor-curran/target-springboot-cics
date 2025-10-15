package com.cbsa.migration.controller;

import com.cbsa.migration.dto.AccountRequestDto;
import com.cbsa.migration.dto.AccountResponseDto;
import com.cbsa.migration.service.AccountService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

@RestController
@RequestMapping("/api/account")
public class AccountController {
    
    private final AccountService accountService;
    
    @Autowired
    public AccountController(AccountService accountService) {
        this.accountService = accountService;
    }
    
    @PostMapping
    public ResponseEntity<AccountResponseDto> createAccount(@Valid @RequestBody AccountRequestDto request) {
        AccountResponseDto response = accountService.createAccount(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
    
    @GetMapping("/{sortCode}/{accountNumber}")
    public ResponseEntity<AccountResponseDto> getAccount(
            @PathVariable String sortCode,
            @PathVariable String accountNumber) {
        AccountResponseDto response = accountService.getAccount(sortCode, accountNumber);
        return ResponseEntity.ok(response);
    }
    
    @PutMapping("/{sortCode}/{accountNumber}")
    public ResponseEntity<AccountResponseDto> updateAccount(
            @PathVariable String sortCode,
            @PathVariable String accountNumber,
            @Valid @RequestBody AccountRequestDto request) {
        AccountResponseDto response = accountService.updateAccount(sortCode, accountNumber, request);
        return ResponseEntity.ok(response);
    }
    
    @DeleteMapping("/{sortCode}/{accountNumber}")
    public ResponseEntity<Void> deleteAccount(
            @PathVariable String sortCode,
            @PathVariable String accountNumber) {
        accountService.deleteAccount(sortCode, accountNumber);
        return ResponseEntity.noContent().build();
    }
    
    @GetMapping("/customer/{sortCode}/{customerNumber}")
    public ResponseEntity<List<AccountResponseDto>> getAccountsByCustomer(
            @PathVariable String sortCode,
            @PathVariable Long customerNumber) {
        List<AccountResponseDto> response = accountService.getAccountsByCustomer(sortCode, customerNumber);
        return ResponseEntity.ok(response);
    }
}
