package com.cbsa.migration.service;

import com.cbsa.migration.dto.AccountRequestDto;
import com.cbsa.migration.dto.AccountResponseDto;
import com.cbsa.migration.exception.AccountCreationException;
import com.cbsa.migration.exception.AccountDeletionException;
import com.cbsa.migration.exception.AccountNotFoundException;
import com.cbsa.migration.exception.CustomerNotFoundException;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class AccountService {
    
    private final AccountRepository accountRepository;
    private final CustomerRepository customerRepository;
    private final TransactionRepository transactionRepository;
    
    private static final String ERROR_CUSTOMER_NOT_FOUND = "1";
    private static final String ERROR_TOO_MANY_ACCOUNTS = "8";
    private static final String ERROR_ACCOUNT_NOT_FOUND = "1";
    private static final String ERROR_DELETE_FAILED = "3";
    private static final int MAX_ACCOUNTS_PER_CUSTOMER = 10;
    
    private static final Set<String> VALID_ACCOUNT_TYPES = Set.of(
        "CURRENT", "SAVINGS", "LOAN", "ISA", "MORTGAGE", "CHECKING"
    );
    
    @Autowired
    public AccountService(AccountRepository accountRepository,
                         CustomerRepository customerRepository,
                         TransactionRepository transactionRepository) {
        this.accountRepository = accountRepository;
        this.customerRepository = customerRepository;
        this.transactionRepository = transactionRepository;
    }
    
    public AccountResponseDto createAccount(AccountRequestDto request) {
        Optional<Customer> customer = customerRepository.findById(
            request.getSortCode(), 
            request.getCustomerNumber()
        );
        if (customer.isEmpty()) {
            throw new AccountCreationException("Customer not found", ERROR_CUSTOMER_NOT_FOUND);
        }
        
        int accountCount = accountRepository.findByCustomerNumber(request.getCustomerNumber()).size();
        if (accountCount >= MAX_ACCOUNTS_PER_CUSTOMER) {
            throw new AccountCreationException("Customer already has maximum number of accounts", ERROR_TOO_MANY_ACCOUNTS);
        }
        
        if (!VALID_ACCOUNT_TYPES.contains(request.getAccountType().toUpperCase())) {
            throw new IllegalArgumentException("Invalid account type");
        }
        
        String accountNumber = generateNextAccountNumber(request.getSortCode());
        
        Account account = Account.builder()
            .eyeCatcher(Account.VALID_EYECATCHER)
            .customerNumber(request.getCustomerNumber())
            .sortCode(request.getSortCode())
            .accountNumber(accountNumber)
            .accountType(request.getAccountType().toUpperCase())
            .interestRate(request.getInterestRate() != null ? request.getInterestRate() : BigDecimal.ZERO)
            .openedDate(LocalDate.now())
            .overdraftLimit(request.getOverdraftLimit() != null ? request.getOverdraftLimit() : 0)
            .lastStatementDate(null)
            .nextStatementDate(null)
            .availableBalance(request.getInitialDeposit() != null ? request.getInitialDeposit() : BigDecimal.ZERO)
            .actualBalance(request.getInitialDeposit() != null ? request.getInitialDeposit() : BigDecimal.ZERO)
            .build();
        
        Account savedAccount = accountRepository.save(account);
        
        return mapToResponseDto(savedAccount, customer.get());
    }
    
    public AccountResponseDto getAccount(String sortCode, String accountNumber) {
        Account account = accountRepository.findById(sortCode, accountNumber)
            .orElseThrow(() -> new AccountNotFoundException("Account not found"));
        
        Customer customer = customerRepository.findById(sortCode, account.getCustomerNumber())
            .orElseThrow(() -> new CustomerNotFoundException("Customer not found"));
        
        return mapToResponseDto(account, customer);
    }
    
    public AccountResponseDto updateAccount(String sortCode, String accountNumber, AccountRequestDto request) {
        Account account = accountRepository.findById(sortCode, accountNumber)
            .orElseThrow(() -> new AccountNotFoundException("Account not found"));
        
        if (request.getAccountType() == null || request.getAccountType().trim().isEmpty()) {
            throw new IllegalArgumentException("Account type cannot be empty");
        }
        
        if (!VALID_ACCOUNT_TYPES.contains(request.getAccountType().toUpperCase())) {
            throw new IllegalArgumentException("Invalid account type");
        }
        
        account.setAccountType(request.getAccountType().toUpperCase());
        account.setInterestRate(request.getInterestRate() != null ? request.getInterestRate() : account.getInterestRate());
        account.setOverdraftLimit(request.getOverdraftLimit() != null ? request.getOverdraftLimit() : account.getOverdraftLimit());
        
        Account updatedAccount = accountRepository.save(account);
        
        Customer customer = customerRepository.findById(sortCode, account.getCustomerNumber())
            .orElseThrow(() -> new CustomerNotFoundException("Customer not found"));
        
        return mapToResponseDto(updatedAccount, customer);
    }
    
    public void deleteAccount(String sortCode, String accountNumber) {
        Account account = accountRepository.findById(sortCode, accountNumber)
            .orElseThrow(() -> new AccountNotFoundException("Account not found", ERROR_ACCOUNT_NOT_FOUND));
        
        boolean deleted = accountRepository.deleteById(sortCode, accountNumber);
        if (!deleted) {
            throw new AccountDeletionException("Failed to delete account", ERROR_DELETE_FAILED);
        }
        
        writeDeleteTransaction(account);
    }
    
    public List<AccountResponseDto> getAccountsByCustomer(String sortCode, Long customerNumber) {
        Customer customer = customerRepository.findById(sortCode, customerNumber)
            .orElseThrow(() -> new CustomerNotFoundException("Customer not found"));
        
        List<Account> accounts = accountRepository.findByCustomerNumber(customerNumber);
        
        return accounts.stream()
            .map(account -> mapToResponseDto(account, customer))
            .collect(Collectors.toList());
    }
    
    private String generateNextAccountNumber(String sortCode) {
        return String.format("%08d", System.currentTimeMillis() % 100000000);
    }
    
    private void writeDeleteTransaction(Account account) {
        Transaction transaction = Transaction.builder()
            .eyeCatcher("PRTR")
            .sortCode(account.getSortCode())
            .accountNumber(account.getAccountNumber())
            .transactionDate(LocalDate.now())
            .transactionTime(LocalTime.now())
            .referenceNumber(generateTransactionReference())
            .transactionType("DAC")
            .description("Account deleted: " + account.getAccountType())
            .amount(account.getActualBalance())
            .logicallyDeleted(false)
            .build();
        
        transactionRepository.save(transaction);
    }
    
    private Long generateTransactionReference() {
        return System.currentTimeMillis() % 1000000000000L;
    }
    
    private AccountResponseDto mapToResponseDto(Account account, Customer customer) {
        return AccountResponseDto.builder()
            .accountNumber(account.getAccountNumber())
            .sortCode(account.getSortCode())
            .accountType(account.getAccountType())
            .customerNumber(account.getCustomerNumber())
            .customerName(customer.getName())
            .availableBalance(account.getAvailableBalance())
            .actualBalance(account.getActualBalance())
            .interestRate(account.getInterestRate())
            .openedDate(account.getOpenedDate())
            .overdraftLimit(account.getOverdraftLimit())
            .lastStatementDate(account.getLastStatementDate())
            .nextStatementDate(account.getNextStatementDate())
            .status("ACTIVE")
            .build();
    }
}
