package com.cbsa.migration.service;

import com.cbsa.migration.dto.CreateAccountRequest;
import com.cbsa.migration.dto.CreateAccountResponse;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for AccountCreationService covering all CREACC business logic branches.
 */
@ExtendWith(MockitoExtension.class)
class AccountCreationServiceTest {

    @Mock
    private CustomerRepository customerRepository;

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private ControlRepository controlRepository;

    @Mock
    private TransactionRepository transactionRepository;

    @Mock
    private SortCodeService sortCodeService;

    @Mock
    private ErrorLoggingService errorLoggingService;

    @InjectMocks
    private AccountCreationService accountCreationService;

    private static final String SORT_CODE = "987654";

    @BeforeEach
    void setUp() {
        lenient().when(sortCodeService.getSortCode()).thenReturn(SORT_CODE);
    }

    private CreateAccountRequest buildValidRequest() {
        return CreateAccountRequest.builder()
                .customerNumber(1000001L)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("2.50"))
                .overdraftLimit(500)
                .build();
    }

    private Customer buildCustomer() {
        return Customer.builder()
                .eyeCatcher("CUST")
                .sortCode(SORT_CODE)
                .customerNumber(1000001L)
                .name("Test Customer")
                .address("123 Test St")
                .dateOfBirth(LocalDate.of(1990, 1, 1))
                .creditScore(750)
                .build();
    }

    // --- Success path ---

    @Test
    void createAccount_success() {
        when(customerRepository.findById(SORT_CODE, 1000001L))
                .thenReturn(Optional.of(buildCustomer()));
        when(accountRepository.countByCustomerNumber(1000001L)).thenReturn(2);
        when(controlRepository.getNextAccountNumber()).thenReturn(10000003);
        when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
        when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

        CreateAccountResponse response = accountCreationService.createAccount(buildValidRequest());

        assertTrue(response.isSuccess());
        assertNull(response.getFailCode());
        assertEquals("10000003", response.getAccountNumber());
        assertEquals(SORT_CODE, response.getSortCode());
        assertEquals(1000001L, response.getCustomerNumber());
        assertEquals("CURRENT", response.getAccountType());
        assertEquals(new BigDecimal("2.50"), response.getInterestRate());
        assertEquals(500, response.getOverdraftLimit());
        assertEquals(LocalDate.now(), response.getOpenedDate());
        assertEquals(LocalDate.now(), response.getLastStatementDate());
        assertEquals(LocalDate.now().plusDays(30), response.getNextStatementDate());
        assertEquals(BigDecimal.ZERO, response.getAvailableBalance());
        assertEquals(BigDecimal.ZERO, response.getActualBalance());

        // Verify account was saved
        ArgumentCaptor<Account> accountCaptor = ArgumentCaptor.forClass(Account.class);
        verify(accountRepository).save(accountCaptor.capture());
        Account savedAccount = accountCaptor.getValue();
        assertEquals("ACCT", savedAccount.getEyeCatcher());
        assertEquals("10000003", savedAccount.getAccountNumber());

        // Verify PROCTRAN audit record was saved with type OCA
        ArgumentCaptor<Transaction> txnCaptor = ArgumentCaptor.forClass(Transaction.class);
        verify(transactionRepository).save(txnCaptor.capture());
        Transaction savedTxn = txnCaptor.getValue();
        assertEquals("OCA", savedTxn.getTransactionType());
        assertEquals("PRTR", savedTxn.getEyeCatcher());
        assertFalse(savedTxn.isLogicallyDeleted());
    }

    // --- Fail code 'A': Invalid account type ---

    @Test
    void createAccount_invalidAccountType_returnsFailCodeA() {
        CreateAccountRequest request = buildValidRequest();
        request.setAccountType("CHECKING");

        CreateAccountResponse response = accountCreationService.createAccount(request);

        assertFalse(response.isSuccess());
        assertEquals("A", response.getFailCode());
        assertNotNull(response.getFailMessage());
        verifyNoInteractions(customerRepository, accountRepository, controlRepository, transactionRepository);
    }

    @Test
    void createAccount_validAccountTypes() {
        for (String type : new String[]{"ISA", "MORTGAGE", "SAVING", "CURRENT", "LOAN"}) {
            when(customerRepository.findById(SORT_CODE, 1000001L))
                    .thenReturn(Optional.of(buildCustomer()));
            when(accountRepository.countByCustomerNumber(1000001L)).thenReturn(0);
            when(controlRepository.getNextAccountNumber()).thenReturn(10000001);
            when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
            when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

            CreateAccountRequest request = buildValidRequest();
            request.setAccountType(type);
            CreateAccountResponse response = accountCreationService.createAccount(request);

            assertTrue(response.isSuccess(), "Expected success for account type: " + type);
            assertEquals(type, response.getAccountType());
        }
    }

    @Test
    void createAccount_accountTypeCaseInsensitive() {
        when(customerRepository.findById(SORT_CODE, 1000001L))
                .thenReturn(Optional.of(buildCustomer()));
        when(accountRepository.countByCustomerNumber(1000001L)).thenReturn(0);
        when(controlRepository.getNextAccountNumber()).thenReturn(10000001);
        when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
        when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

        CreateAccountRequest request = buildValidRequest();
        request.setAccountType("current");
        CreateAccountResponse response = accountCreationService.createAccount(request);

        assertTrue(response.isSuccess());
        assertEquals("CURRENT", response.getAccountType());
    }

    // --- Fail code '1': Customer not found ---

    @Test
    void createAccount_customerNotFound_returnsFailCode1() {
        when(customerRepository.findById(SORT_CODE, 1000001L))
                .thenReturn(Optional.empty());

        CreateAccountResponse response = accountCreationService.createAccount(buildValidRequest());

        assertFalse(response.isSuccess());
        assertEquals("1", response.getFailCode());
        assertNotNull(response.getFailMessage());
        verify(customerRepository).findById(SORT_CODE, 1000001L);
        verifyNoInteractions(controlRepository);
    }

    // --- Fail code '9': Error counting accounts ---

    @Test
    void createAccount_errorCountingAccounts_returnsFailCode9() {
        when(customerRepository.findById(SORT_CODE, 1000001L))
                .thenReturn(Optional.of(buildCustomer()));
        when(accountRepository.countByCustomerNumber(1000001L))
                .thenThrow(new RuntimeException("DB error"));

        CreateAccountResponse response = accountCreationService.createAccount(buildValidRequest());

        assertFalse(response.isSuccess());
        assertEquals("9", response.getFailCode());
        verify(errorLoggingService).logError(eq("CREACC"), any(RuntimeException.class));
    }

    // --- Fail code '8': Too many accounts ---

    @Test
    void createAccount_tooManyAccounts_returnsFailCode8() {
        when(customerRepository.findById(SORT_CODE, 1000001L))
                .thenReturn(Optional.of(buildCustomer()));
        when(accountRepository.countByCustomerNumber(1000001L)).thenReturn(10);

        CreateAccountResponse response = accountCreationService.createAccount(buildValidRequest());

        assertFalse(response.isSuccess());
        assertEquals("8", response.getFailCode());
        verifyNoInteractions(controlRepository);
    }

    @Test
    void createAccount_exactlyTenAccounts_returnsFailCode8() {
        when(customerRepository.findById(SORT_CODE, 1000001L))
                .thenReturn(Optional.of(buildCustomer()));
        when(accountRepository.countByCustomerNumber(1000001L)).thenReturn(10);

        CreateAccountResponse response = accountCreationService.createAccount(buildValidRequest());

        assertFalse(response.isSuccess());
        assertEquals("8", response.getFailCode());
    }

    @Test
    void createAccount_nineAccounts_succeeds() {
        when(customerRepository.findById(SORT_CODE, 1000001L))
                .thenReturn(Optional.of(buildCustomer()));
        when(accountRepository.countByCustomerNumber(1000001L)).thenReturn(9);
        when(controlRepository.getNextAccountNumber()).thenReturn(10000010);
        when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
        when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

        CreateAccountResponse response = accountCreationService.createAccount(buildValidRequest());

        assertTrue(response.isSuccess());
    }

    // --- Fail code '3': Lock/counter acquisition failed ---

    @Test
    void createAccount_lockFailed_returnsFailCode3() {
        when(customerRepository.findById(SORT_CODE, 1000001L))
                .thenReturn(Optional.of(buildCustomer()));
        when(accountRepository.countByCustomerNumber(1000001L)).thenReturn(0);
        when(controlRepository.getNextAccountNumber())
                .thenThrow(new RuntimeException("Lock acquisition failed"));

        CreateAccountResponse response = accountCreationService.createAccount(buildValidRequest());

        assertFalse(response.isSuccess());
        assertEquals("3", response.getFailCode());
        verify(errorLoggingService).logError(eq("CREACC"), any(RuntimeException.class));
    }

    // --- Fail code '7': Account insert failed ---

    @Test
    void createAccount_accountInsertFailed_returnsFailCode7() {
        when(customerRepository.findById(SORT_CODE, 1000001L))
                .thenReturn(Optional.of(buildCustomer()));
        when(accountRepository.countByCustomerNumber(1000001L)).thenReturn(0);
        when(controlRepository.getNextAccountNumber()).thenReturn(10000001);
        when(accountRepository.save(any(Account.class)))
                .thenThrow(new RuntimeException("Insert failed"));

        CreateAccountResponse response = accountCreationService.createAccount(buildValidRequest());

        assertFalse(response.isSuccess());
        assertEquals("7", response.getFailCode());
        verify(errorLoggingService).logError(eq("CREACC"), any(RuntimeException.class));
    }

    // --- PROCTRAN insert failure triggers rollback ---

    @Test
    void createAccount_proctranInsertFailed_throwsForRollback() {
        when(customerRepository.findById(SORT_CODE, 1000001L))
                .thenReturn(Optional.of(buildCustomer()));
        when(accountRepository.countByCustomerNumber(1000001L)).thenReturn(0);
        when(controlRepository.getNextAccountNumber()).thenReturn(10000001);
        when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
        when(transactionRepository.save(any(Transaction.class)))
                .thenThrow(new RuntimeException("PROCTRAN insert failed"));

        assertThrows(RuntimeException.class,
                () -> accountCreationService.createAccount(buildValidRequest()));
        verify(errorLoggingService).logError(eq("CREACC"), any(RuntimeException.class));
    }

    // --- Account number formatting ---

    @Test
    void createAccount_accountNumberPaddedToEightDigits() {
        when(customerRepository.findById(SORT_CODE, 1000001L))
                .thenReturn(Optional.of(buildCustomer()));
        when(accountRepository.countByCustomerNumber(1000001L)).thenReturn(0);
        when(controlRepository.getNextAccountNumber()).thenReturn(42);
        when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
        when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

        CreateAccountResponse response = accountCreationService.createAccount(buildValidRequest());

        assertTrue(response.isSuccess());
        assertEquals("00000042", response.getAccountNumber());
    }

    // --- Date calculations ---

    @Test
    void createAccount_dateCalculationsCorrect() {
        when(customerRepository.findById(SORT_CODE, 1000001L))
                .thenReturn(Optional.of(buildCustomer()));
        when(accountRepository.countByCustomerNumber(1000001L)).thenReturn(0);
        when(controlRepository.getNextAccountNumber()).thenReturn(10000001);
        when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
        when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

        CreateAccountResponse response = accountCreationService.createAccount(buildValidRequest());

        assertTrue(response.isSuccess());
        LocalDate today = LocalDate.now();
        assertEquals(today, response.getOpenedDate());
        assertEquals(today, response.getLastStatementDate());
        assertEquals(today.plusDays(30), response.getNextStatementDate());
    }

    // --- Account type with whitespace trimming ---

    @Test
    void createAccount_accountTypeWithWhitespace_trimmed() {
        when(customerRepository.findById(SORT_CODE, 1000001L))
                .thenReturn(Optional.of(buildCustomer()));
        when(accountRepository.countByCustomerNumber(1000001L)).thenReturn(0);
        when(controlRepository.getNextAccountNumber()).thenReturn(10000001);
        when(accountRepository.save(any(Account.class))).thenAnswer(i -> i.getArgument(0));
        when(transactionRepository.save(any(Transaction.class))).thenAnswer(i -> i.getArgument(0));

        CreateAccountRequest request = buildValidRequest();
        request.setAccountType("  LOAN  ");
        CreateAccountResponse response = accountCreationService.createAccount(request);

        assertTrue(response.isSuccess());
        assertEquals("LOAN", response.getAccountType());
    }
}
