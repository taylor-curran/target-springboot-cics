package com.cbsa.migration.service;

import com.cbsa.migration.dto.CreateAccountRequest;
import com.cbsa.migration.dto.CreateAccountResponse;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Set;

/**
 * Service implementing the CREACC (Create Account) COBOL program business logic.
 * <p>
 * Creates a new bank account for an existing customer with the following rules:
 * <ul>
 *   <li>Customer must exist (fail code '1')</li>
 *   <li>Customer must have fewer than 10 accounts (fail code '8')</li>
 *   <li>Account type must be one of: ISA, MORTGAGE, SAVING, CURRENT, LOAN (fail code 'A')</li>
 *   <li>A sequential account number is generated from the CONTROL table with pessimistic locking (fail code '3')</li>
 *   <li>The new ACCOUNT row is inserted (fail code '7')</li>
 *   <li>An audit record (PROCTRAN) is inserted with type 'OCA' (branch-create-account)</li>
 * </ul>
 */
@Service
public class AccountCreationService {

    private static final Logger log = LoggerFactory.getLogger(AccountCreationService.class);

    private static final Set<String> VALID_ACCOUNT_TYPES = Set.of(
            "ISA", "MORTGAGE", "SAVING", "CURRENT", "LOAN"
    );
    private static final int MAX_ACCOUNTS_PER_CUSTOMER = 10;
    private static final String PROGRAM_NAME = "CREACC";

    private final CustomerRepository customerRepository;
    private final AccountRepository accountRepository;
    private final ControlRepository controlRepository;
    private final TransactionRepository transactionRepository;
    private final SortCodeService sortCodeService;
    private final ErrorLoggingService errorLoggingService;

    @Autowired
    public AccountCreationService(CustomerRepository customerRepository,
                                  AccountRepository accountRepository,
                                  ControlRepository controlRepository,
                                  TransactionRepository transactionRepository,
                                  SortCodeService sortCodeService,
                                  ErrorLoggingService errorLoggingService) {
        this.customerRepository = customerRepository;
        this.accountRepository = accountRepository;
        this.controlRepository = controlRepository;
        this.transactionRepository = transactionRepository;
        this.sortCodeService = sortCodeService;
        this.errorLoggingService = errorLoggingService;
    }

    /**
     * Create a new bank account for an existing customer.
     * Mirrors the CREACC COBOL program logic with all validation and audit steps.
     *
     * @param request the account creation request
     * @return response with account details on success, or failure code on error
     */
    @Transactional
    public CreateAccountResponse createAccount(CreateAccountRequest request) {
        String sortCode = sortCodeService.getSortCode();

        // Step 1: Validate account type against whitelist
        String accountType = request.getAccountType().toUpperCase().trim();
        if (!VALID_ACCOUNT_TYPES.contains(accountType)) {
            log.warn("CREACC: Invalid account type '{}' for customer {}",
                    request.getAccountType(), request.getCustomerNumber());
            return CreateAccountResponse.failure("A",
                    "Invalid account type. Must be one of: ISA, MORTGAGE, SAVING, CURRENT, LOAN");
        }

        // Step 2: Validate customer exists (equivalent of INQCUST call)
        boolean customerExists = customerRepository
                .findById(sortCode, request.getCustomerNumber())
                .isPresent();
        if (!customerExists) {
            log.warn("CREACC: Customer {} not found", request.getCustomerNumber());
            return CreateAccountResponse.failure("1",
                    "Customer " + request.getCustomerNumber() + " not found");
        }

        // Step 3: Count existing accounts (equivalent of INQACCCU call)
        int existingAccountCount;
        try {
            existingAccountCount = accountRepository.countByCustomerNumber(request.getCustomerNumber());
        } catch (Exception e) {
            log.error("CREACC: Error counting accounts for customer {}", request.getCustomerNumber(), e);
            errorLoggingService.logError(PROGRAM_NAME, e);
            return CreateAccountResponse.failure("9",
                    "Error counting existing accounts for customer " + request.getCustomerNumber());
        }

        if (existingAccountCount >= MAX_ACCOUNTS_PER_CUSTOMER) {
            log.warn("CREACC: Customer {} already has {} accounts (max {})",
                    request.getCustomerNumber(), existingAccountCount, MAX_ACCOUNTS_PER_CUSTOMER);
            return CreateAccountResponse.failure("8",
                    "Customer already has " + existingAccountCount + " accounts (maximum is " + MAX_ACCOUNTS_PER_CUSTOMER + ")");
        }

        // Step 4: Generate next sequential account number from CONTROL table
        // The JdbcControlRepository.getNextAccountNumber() increments both
        // last_account_number and account_count atomically within the @Transactional boundary.
        // In a multi-instance deployment, the SQLite serialized writes provide the
        // same concurrency guarantee as the COBOL CICS ENQ/DEQ Named Counter pattern.
        Integer nextAccountNumber;
        try {
            nextAccountNumber = controlRepository.getNextAccountNumber();
        } catch (Exception e) {
            log.error("CREACC: Failed to acquire next account number (ENQ/lock failure)", e);
            errorLoggingService.logError(PROGRAM_NAME, e);
            return CreateAccountResponse.failure("3",
                    "Failed to generate account number (lock acquisition failed)");
        }

        String accountNumberStr = String.format("%08d", nextAccountNumber);

        // Step 5: Calculate dates
        LocalDate today = LocalDate.now();
        LocalDate nextStatementDate = today.plusDays(30);

        // Step 6: Build and insert the new Account record
        Account newAccount = Account.builder()
                .eyeCatcher(Account.VALID_EYECATCHER)
                .customerNumber(request.getCustomerNumber())
                .sortCode(sortCode)
                .accountNumber(accountNumberStr)
                .accountType(accountType)
                .interestRate(request.getInterestRate())
                .openedDate(today)
                .overdraftLimit(request.getOverdraftLimit())
                .lastStatementDate(today)
                .nextStatementDate(nextStatementDate)
                .availableBalance(BigDecimal.ZERO)
                .actualBalance(BigDecimal.ZERO)
                .build();

        try {
            accountRepository.save(newAccount);
        } catch (Exception e) {
            log.error("CREACC: Failed to insert account {} for customer {}",
                    accountNumberStr, request.getCustomerNumber(), e);
            errorLoggingService.logError(PROGRAM_NAME, e);
            return CreateAccountResponse.failure("7",
                    "Failed to insert account record");
        }

        // Step 7: Insert PROCTRAN audit record with type 'OCA' (branch-create-account)
        try {
            Transaction auditRecord = Transaction.builder()
                    .eyeCatcher(Transaction.VALID_EYECATCHER)
                    .logicallyDeleted(false)
                    .sortCode(sortCode)
                    .accountNumber(accountNumberStr)
                    .transactionDate(today)
                    .transactionTime(LocalTime.now())
                    .referenceNumber(0L)
                    .transactionType(Transaction.TYPE_BRANCH_CREATE_ACCOUNT)
                    .description("Account " + accountNumberStr + " created for customer " + request.getCustomerNumber())
                    .amount(BigDecimal.ZERO)
                    .build();

            transactionRepository.save(auditRecord);
        } catch (Exception e) {
            // PROCTRAN insert failure should roll back the entire transaction
            log.error("CREACC: Failed to insert PROCTRAN audit record for account {}",
                    accountNumberStr, e);
            errorLoggingService.logError(PROGRAM_NAME, e);
            throw new RuntimeException("Failed to insert PROCTRAN audit record for account " + accountNumberStr, e);
        }

        log.info("CREACC: Successfully created account {} (type={}) for customer {}",
                accountNumberStr, accountType, request.getCustomerNumber());

        // Step 8: Return success response with full account details
        return CreateAccountResponse.success(
                accountNumberStr,
                sortCode,
                request.getCustomerNumber(),
                accountType,
                request.getInterestRate(),
                today,
                request.getOverdraftLimit(),
                today,
                nextStatementDate,
                BigDecimal.ZERO,
                BigDecimal.ZERO
        );
    }
}
