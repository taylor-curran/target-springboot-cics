package com.cbsa.migration.service;

import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.AccountCreationRequest;
import com.cbsa.migration.model.AccountCreationResponse;
import com.cbsa.migration.model.CustomerInquiryResponse;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

/**
 * This service replicates the functionality of the CREACC COBOL program.
 * It handles account creation including customer validation, account number generation,
 * and initial transaction creation.
 */
@Service
public class AccountCreationService {

    private final CustomerInquiryService customerInquiryService;
    private final AccountRepository accountRepository;
    private final TransactionRepository transactionRepository;
    private final ControlRepository controlRepository;
    private final SortCodeService sortCodeService;

    @Autowired
    public AccountCreationService(CustomerInquiryService customerInquiryService,
                                AccountRepository accountRepository,
                                TransactionRepository transactionRepository,
                                ControlRepository controlRepository,
                                SortCodeService sortCodeService) {
        this.customerInquiryService = customerInquiryService;
        this.accountRepository = accountRepository;
        this.transactionRepository = transactionRepository;
        this.controlRepository = controlRepository;
        this.sortCodeService = sortCodeService;
    }

    /**
     * Creates a new account for a customer
     *
     * @param request The account creation request
     * @return AccountCreationResponse with success/failure information
     */
    @Transactional
    public AccountCreationResponse createAccount(AccountCreationRequest request) {
        try {
            CustomerInquiryResponse customerResponse = customerInquiryService.inquireCustomer(request.getCustomerNumber());
            
            if (!customerResponse.isSuccess()) {
                return AccountCreationResponse.builder()
                        .success(false)
                        .failureCode("1")
                        .build();
            }

            int accountCount = accountRepository.countByCustomerNumber(request.getCustomerNumber());
            if (accountCount >= 9) {
                return AccountCreationResponse.builder()
                        .success(false)
                        .failureCode("8")
                        .build();
            }

            if (!isValidAccountType(request.getAccountType())) {
                return AccountCreationResponse.builder()
                        .success(false)
                        .failureCode("7")
                        .build();
            }

            String sortCode = sortCodeService.getSortCode();
            String accountNumber = generateAccountNumber();

            Account account = Account.builder()
                    .eyeCatcher("ACCT")
                    .customerNumber(request.getCustomerNumber())
                    .sortCode(sortCode)
                    .accountNumber(accountNumber)
                    .accountType(request.getAccountType())
                    .interestRate(request.getInterestRate())
                    .openedDate(LocalDate.now())
                    .overdraftLimit(request.getOverdraftLimit())
                    .availableBalance(request.getInitialBalance())
                    .actualBalance(request.getInitialBalance())
                    .build();

            accountRepository.save(account);

            if (request.getInitialBalance().compareTo(BigDecimal.ZERO) > 0) {
                createInitialTransaction(sortCode, accountNumber, request.getInitialBalance());
            }

            return AccountCreationResponse.builder()
                    .success(true)
                    .failureCode("0")
                    .accountNumber(accountNumber)
                    .customerNumber(customerResponse.getCustomerNumber())
                    .name(customerResponse.getName())
                    .address(customerResponse.getAddress())
                    .dateOfBirth(customerResponse.getDateOfBirth().format(DateTimeFormatter.ofPattern("ddMMyyyy")))
                    .build();

        } catch (Exception e) {
            return AccountCreationResponse.builder()
                    .success(false)
                    .failureCode("9")
                    .build();
        }
    }

    private boolean isValidAccountType(String accountType) {
        return "CURRENT".equals(accountType) || 
               "SAVINGS".equals(accountType) || 
               "MORTGAGE".equals(accountType) || 
               "LOAN".equals(accountType);
    }

    private String generateAccountNumber() {
        String sortCode = sortCodeService.getSortCode();
        String controlName = sortCode + "-ACCOUNT-LAST";
        
        Integer lastAccountNumber = controlRepository.getControlValueNum(controlName);
        if (lastAccountNumber == null) {
            lastAccountNumber = 0;
        }
        
        int newAccountNumber = lastAccountNumber + 1;
        controlRepository.updateControlValueNum(controlName, newAccountNumber);
        
        String accountCountControlName = sortCode + "-ACCOUNT-COUNT";
        Integer accountCount = controlRepository.getControlValueNum(accountCountControlName);
        if (accountCount == null) {
            accountCount = 0;
        }
        controlRepository.updateControlValueNum(accountCountControlName, accountCount + 1);
        
        return String.format("%08d", newAccountNumber);
    }

    private void createInitialTransaction(String sortCode, String accountNumber, BigDecimal amount) {
        Transaction transaction = Transaction.builder()
                .eyeCatcher("PRTR")
                .logicallyDeleted(false)
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .transactionDate(LocalDate.now())
                .transactionTime(LocalTime.now())
                .referenceNumber(System.currentTimeMillis())
                .transactionType("CRE")
                .description("INITIAL DEPOSIT")
                .amount(amount)
                .build();

        transactionRepository.save(transaction);
    }
}
