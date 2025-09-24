package com.cbsa.migration.service;

import com.cbsa.migration.dto.AccountRequestDto;
import com.cbsa.migration.dto.AccountResponseDto;
import com.cbsa.migration.dto.mapper.DtoMapper;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Control;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Service for Account Management Operations
 * Migrated from COBOL programs handling account creation, maintenance, balance inquiries, and overdraft processing
 */
@Service
public class AccountService {

    private final AccountRepository accountRepository;
    private final CustomerRepository customerRepository;
    private final ControlRepository controlRepository;
    private final DtoMapper dtoMapper;

    public AccountService(AccountRepository accountRepository, 
                         CustomerRepository customerRepository,
                         ControlRepository controlRepository,
                         DtoMapper dtoMapper) {
        this.accountRepository = accountRepository;
        this.customerRepository = customerRepository;
        this.controlRepository = controlRepository;
        this.dtoMapper = dtoMapper;
    }

    /**
     * Create a new account
     * Equivalent to COBOL account creation program
     */
    public AccountResponseDto createAccount(AccountRequestDto request) {
        if (!customerRepository.findById(request.getSortCode(), request.getCustomerNumber()).isPresent()) {
            throw new IllegalArgumentException("Customer not found: " + request.getCustomerNumber());
        }

        Control control = controlRepository.initializeControlRecord();
        String accountNumber = String.format("%08d", control.getLastAccountNumber() + 1);

        Account account = dtoMapper.toAccount(request);
        account.setEyeCatcher("ACCT");
        account.setAccountNumber(accountNumber);
        account.setOpenedDate(LocalDate.now());
        
        if (account.getInterestRate() == null) {
            account.setInterestRate(getDefaultInterestRate(request.getAccountType()));
        }
        if (account.getOverdraftLimit() == null) {
            account.setOverdraftLimit(0);
        }

        BigDecimal initialDeposit = request.getInitialDeposit() != null ? request.getInitialDeposit() : BigDecimal.ZERO;
        account.setAvailableBalance(initialDeposit);
        account.setActualBalance(initialDeposit);

        Account savedAccount = accountRepository.save(account);

        control.setLastAccountNumber(Integer.parseInt(accountNumber));
        control.setAccountCount(control.getAccountCount() + 1);
        controlRepository.save(control);

        return dtoMapper.toAccountResponseDto(savedAccount);
    }

    /**
     * Update an existing account
     * Equivalent to COBOL account maintenance program
     */
    public AccountResponseDto updateAccount(String sortCode, String accountNumber, AccountRequestDto request) {
        Account account = accountRepository.findById(sortCode, accountNumber)
            .orElseThrow(() -> new IllegalArgumentException("Account not found: " + sortCode + accountNumber));

        if (request.getAccountType() != null) {
            account.setAccountType(request.getAccountType());
        }
        if (request.getInterestRate() != null) {
            account.setInterestRate(request.getInterestRate());
        }
        if (request.getOverdraftLimit() != null) {
            account.setOverdraftLimit(request.getOverdraftLimit());
        }

        Account updatedAccount = accountRepository.save(account);
        return dtoMapper.toAccountResponseDto(updatedAccount);
    }

    /**
     * Get account balance information
     * Equivalent to COBOL balance inquiry program
     */
    public AccountResponseDto getAccountBalance(String sortCode, String accountNumber) {
        Account account = accountRepository.findById(sortCode, accountNumber)
            .orElseThrow(() -> new IllegalArgumentException("Account not found: " + sortCode + accountNumber));

        return dtoMapper.toAccountResponseDto(account);
    }

    /**
     * Process overdraft transaction
     * Equivalent to COBOL overdraft processing program
     */
    public AccountResponseDto processOverdraft(String sortCode, String accountNumber, BigDecimal amount) {
        Account account = accountRepository.findById(sortCode, accountNumber)
            .orElseThrow(() -> new IllegalArgumentException("Account not found: " + sortCode + accountNumber));

        if (amount.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("Overdraft amount must be positive");
        }

        BigDecimal newAvailableBalance = account.getAvailableBalance().subtract(amount);
        BigDecimal overdraftLimit = BigDecimal.valueOf(account.getOverdraftLimit());

        if (newAvailableBalance.compareTo(overdraftLimit.negate()) < 0) {
            throw new IllegalArgumentException("Overdraft exceeds limit. Available: " + 
                account.getAvailableBalance() + ", Limit: " + overdraftLimit);
        }

        account.setAvailableBalance(newAvailableBalance);
        account.setActualBalance(account.getActualBalance().subtract(amount));

        Account updatedAccount = accountRepository.save(account);
        return dtoMapper.toAccountResponseDto(updatedAccount);
    }

    /**
     * Get full account details
     * Equivalent to COBOL account inquiry program
     */
    public AccountResponseDto getAccountDetails(String sortCode, String accountNumber) {
        Account account = accountRepository.findById(sortCode, accountNumber)
            .orElseThrow(() -> new IllegalArgumentException("Account not found: " + sortCode + accountNumber));

        return dtoMapper.toAccountResponseDto(account);
    }

    /**
     * Get all accounts for a customer
     * Equivalent to COBOL customer account listing program
     */
    public List<AccountResponseDto> getAccountsByCustomer(Long customerNumber) {
        List<Account> accounts = accountRepository.findByCustomerNumber(customerNumber);
        return accounts.stream()
            .map(dtoMapper::toAccountResponseDto)
            .collect(Collectors.toList());
    }

    /**
     * Get default interest rate based on account type
     */
    private BigDecimal getDefaultInterestRate(String accountType) {
        switch (accountType.toUpperCase()) {
            case "SAVINGS":
                return new BigDecimal("2.50");
            case "CURRENT":
                return new BigDecimal("0.10");
            default:
                return new BigDecimal("1.00");
        }
    }
}
