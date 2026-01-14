package com.cbsa.migration.service;

import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.service.CustomerService.DeleteResult;
import com.cbsa.migration.service.CustomerService.UpdateResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CustomerServiceTest {

    @Mock
    private CustomerRepository customerRepository;

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private SortCodeService sortCodeService;

    private CustomerService customerService;

    private static final String SORT_CODE = "987654";

    @BeforeEach
    void setUp() {
        customerService = new CustomerService(customerRepository, accountRepository, sortCodeService);
        lenient().when(sortCodeService.getSortCode()).thenReturn(SORT_CODE);
    }

    private Customer createTestCustomer(Long customerNumber) {
        Customer customer = new Customer();
        customer.setSortCode(SORT_CODE);
        customer.setCustomerNumber(customerNumber);
        customer.setName("Mr John Smith");
        customer.setAddress("123 Main Street, London");
        customer.setDateOfBirth(LocalDate.of(1980, 1, 15));
        customer.setCreditScore(750);
        customer.setCreditScoreReviewDate(LocalDate.of(2025, 6, 1));
        return customer;
    }

    private Account createTestAccount(Long customerNumber, String accountNumber) {
        Account account = new Account();
        account.setSortCode(SORT_CODE);
        account.setCustomerNumber(customerNumber);
        account.setAccountNumber(accountNumber);
        account.setAccountType("SAVINGS");
        return account;
    }

    @Test
    void getCustomer_validCustomerNumber_returnsCustomer() {
        // Given
        Long customerNumber = 1000001L;
        Customer expectedCustomer = createTestCustomer(customerNumber);
        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(expectedCustomer));

        // When
        Optional<Customer> result = customerService.getCustomer(customerNumber);

        // Then
        assertThat(result).isPresent();
        assertThat(result.get().getCustomerNumber()).isEqualTo(customerNumber);
        assertThat(result.get().getName()).isEqualTo("Mr John Smith");
        verify(customerRepository).findById(SORT_CODE, customerNumber);
    }

    @Test
    void getCustomer_customerNotFound_returnsEmpty() {
        // Given
        Long customerNumber = 9999999L;
        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.empty());

        // When
        Optional<Customer> result = customerService.getCustomer(customerNumber);

        // Then
        assertThat(result).isEmpty();
        verify(customerRepository).findById(SORT_CODE, customerNumber);
    }

    @Test
    void getCustomer_nullCustomerNumber_returnsEmpty() {
        // When
        Optional<Customer> result = customerService.getCustomer((Long) null);

        // Then
        assertThat(result).isEmpty();
        verify(customerRepository, never()).findById(anyString(), anyLong());
    }

    @Test
    void getCustomer_zeroCustomerNumber_returnsEmpty() {
        // When
        Optional<Customer> result = customerService.getCustomer(0L);

        // Then
        assertThat(result).isEmpty();
        verify(customerRepository, never()).findById(anyString(), anyLong());
    }

    @Test
    void getCustomer_negativeCustomerNumber_returnsEmpty() {
        // When
        Optional<Customer> result = customerService.getCustomer(-1L);

        // Then
        assertThat(result).isEmpty();
        verify(customerRepository, never()).findById(anyString(), anyLong());
    }

    @Test
    void getCustomer_withSortCode_returnsCustomer() {
        // Given
        Long customerNumber = 1000001L;
        Customer expectedCustomer = createTestCustomer(customerNumber);
        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(expectedCustomer));

        // When
        Optional<Customer> result = customerService.getCustomer(SORT_CODE, customerNumber);

        // Then
        assertThat(result).isPresent();
        assertThat(result.get().getCustomerNumber()).isEqualTo(customerNumber);
    }

    @Test
    void getCustomer_withBlankSortCode_usesDefaultSortCode() {
        // Given
        Long customerNumber = 1000001L;
        Customer expectedCustomer = createTestCustomer(customerNumber);
        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(expectedCustomer));

        // When
        Optional<Customer> result = customerService.getCustomer("", customerNumber);

        // Then
        assertThat(result).isPresent();
        verify(sortCodeService).getSortCode();
    }

    @Test
    void deleteCustomer_validCustomer_deletesSuccessfully() {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);
        Account account1 = createTestAccount(customerNumber, "10000001");
        Account account2 = createTestAccount(customerNumber, "10000002");
        List<Account> accounts = List.of(account1, account2);

        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(customer));
        when(accountRepository.findByCustomerNumber(customerNumber)).thenReturn(accounts);
        when(accountRepository.deleteById(SORT_CODE, "10000001")).thenReturn(true);
        when(accountRepository.deleteById(SORT_CODE, "10000002")).thenReturn(true);
        when(customerRepository.deleteById(SORT_CODE, customerNumber)).thenReturn(true);

        // When
        DeleteResult result = customerService.deleteCustomer(customerNumber);

        // Then
        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getFailCode()).isNull();
        assertThat(result.getDeletedAccountCount()).isEqualTo(2);
        assertThat(result.getDeletedCustomer()).isNotNull();
        verify(accountRepository, times(2)).deleteById(anyString(), anyString());
        verify(customerRepository).deleteById(SORT_CODE, customerNumber);
    }

    @Test
    void deleteCustomer_customerNotFound_returnsFailCode1() {
        // Given
        Long customerNumber = 9999999L;
        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.empty());

        // When
        DeleteResult result = customerService.deleteCustomer(customerNumber);

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("1");
        assertThat(result.getErrorMessage()).isEqualTo("Customer not found");
    }

    @Test
    void deleteCustomer_nullCustomerNumber_returnsFailCode1() {
        // When
        DeleteResult result = customerService.deleteCustomer(null);

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("1");
        assertThat(result.getErrorMessage()).isEqualTo("Invalid customer number");
    }

    @Test
    void deleteCustomer_zeroCustomerNumber_returnsFailCode1() {
        // When
        DeleteResult result = customerService.deleteCustomer(0L);

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("1");
    }

    @Test
    void deleteCustomer_accountDeleteFails_returnsFailCode3() {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);
        Account account = createTestAccount(customerNumber, "10000001");

        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(customer));
        when(accountRepository.findByCustomerNumber(customerNumber)).thenReturn(List.of(account));
        when(accountRepository.deleteById(SORT_CODE, "10000001")).thenReturn(false);

        // When
        DeleteResult result = customerService.deleteCustomer(customerNumber);

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("3");
        assertThat(result.getErrorMessage()).contains("Failed to delete account");
    }

    @Test
    void deleteCustomer_customerDeleteFails_returnsFailCode3() {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);

        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(customer));
        when(accountRepository.findByCustomerNumber(customerNumber)).thenReturn(Collections.emptyList());
        when(customerRepository.deleteById(SORT_CODE, customerNumber)).thenReturn(false);

        // When
        DeleteResult result = customerService.deleteCustomer(customerNumber);

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("3");
        assertThat(result.getErrorMessage()).isEqualTo("Failed to delete customer record");
    }

    @Test
    void deleteCustomer_datastoreException_returnsFailCode2() {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);

        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(customer));
        when(accountRepository.findByCustomerNumber(customerNumber)).thenThrow(new RuntimeException("Database error"));

        // When
        DeleteResult result = customerService.deleteCustomer(customerNumber);

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("2");
        assertThat(result.getErrorMessage()).contains("Datastore error");
    }

    @Test
    void deleteCustomer_noAccounts_deletesSuccessfully() {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);

        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(customer));
        when(accountRepository.findByCustomerNumber(customerNumber)).thenReturn(Collections.emptyList());
        when(customerRepository.deleteById(SORT_CODE, customerNumber)).thenReturn(true);

        // When
        DeleteResult result = customerService.deleteCustomer(customerNumber);

        // Then
        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getDeletedAccountCount()).isEqualTo(0);
    }

    @Test
    void updateCustomer_validNameAndAddress_updatesSuccessfully() {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);
        Customer updatedCustomer = createTestCustomer(customerNumber);
        updatedCustomer.setName("Dr Jane Doe");
        updatedCustomer.setAddress("456 New Street, Manchester");

        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class))).thenReturn(updatedCustomer);

        // When
        UpdateResult result = customerService.updateCustomer(customerNumber, "Dr Jane Doe", "456 New Street, Manchester");

        // Then
        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getFailCode()).isNull();
        assertThat(result.getUpdatedCustomer()).isNotNull();
        verify(customerRepository).save(any(Customer.class));
    }

    @Test
    void updateCustomer_validNameOnly_updatesSuccessfully() {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);

        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class))).thenReturn(customer);

        // When
        UpdateResult result = customerService.updateCustomer(customerNumber, "Mrs Sarah Connor", null);

        // Then
        assertThat(result.isSuccess()).isTrue();
    }

    @Test
    void updateCustomer_validAddressOnly_updatesSuccessfully() {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);

        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class))).thenReturn(customer);

        // When
        UpdateResult result = customerService.updateCustomer(customerNumber, null, "789 Another Road, Birmingham");

        // Then
        assertThat(result.isSuccess()).isTrue();
    }

    @Test
    void updateCustomer_customerNotFound_returnsFailCode1() {
        // Given
        Long customerNumber = 9999999L;
        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.empty());

        // When
        UpdateResult result = customerService.updateCustomer(customerNumber, "Mr Test User", "Test Address");

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("1");
        assertThat(result.getErrorMessage()).isEqualTo("Customer not found");
    }

    @Test
    void updateCustomer_nullCustomerNumber_returnsFailCode1() {
        // When
        UpdateResult result = customerService.updateCustomer(null, "Mr Test User", "Test Address");

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("1");
        assertThat(result.getErrorMessage()).isEqualTo("Invalid customer number");
    }

    @Test
    void updateCustomer_bothFieldsEmpty_returnsFailCode4() {
        // When
        UpdateResult result = customerService.updateCustomer(1000001L, null, null);

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("4");
        assertThat(result.getErrorMessage()).isEqualTo("Both name and address cannot be empty");
    }

    @Test
    void updateCustomer_bothFieldsBlank_returnsFailCode4() {
        // When
        UpdateResult result = customerService.updateCustomer(1000001L, "   ", "   ");

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("4");
    }

    @Test
    void updateCustomer_invalidTitle_returnsFailCodeT() {
        // When
        UpdateResult result = customerService.updateCustomer(1000001L, "InvalidTitle John Smith", "123 Main Street");

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("T");
        assertThat(result.getErrorMessage()).contains("Invalid title");
    }

    @Test
    void updateCustomer_validTitleProfessor_updatesSuccessfully() {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);

        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class))).thenReturn(customer);

        // When
        UpdateResult result = customerService.updateCustomer(customerNumber, "Professor Albert Einstein", null);

        // Then
        assertThat(result.isSuccess()).isTrue();
    }

    @Test
    void updateCustomer_validTitleDrs_updatesSuccessfully() {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);

        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class))).thenReturn(customer);

        // When
        UpdateResult result = customerService.updateCustomer(customerNumber, "Drs Van Der Berg", null);

        // Then
        assertThat(result.isSuccess()).isTrue();
    }

    @Test
    void updateCustomer_validTitleLord_updatesSuccessfully() {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);

        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class))).thenReturn(customer);

        // When
        UpdateResult result = customerService.updateCustomer(customerNumber, "Lord Byron", null);

        // Then
        assertThat(result.isSuccess()).isTrue();
    }

    @Test
    void updateCustomer_validTitleLady_updatesSuccessfully() {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);

        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class))).thenReturn(customer);

        // When
        UpdateResult result = customerService.updateCustomer(customerNumber, "Lady Diana", null);

        // Then
        assertThat(result.isSuccess()).isTrue();
    }

    @Test
    void updateCustomer_datastoreException_returnsFailCode2() {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);

        when(customerRepository.findById(SORT_CODE, customerNumber)).thenReturn(Optional.of(customer));
        when(customerRepository.save(any(Customer.class))).thenThrow(new RuntimeException("Database error"));

        // When
        UpdateResult result = customerService.updateCustomer(customerNumber, "Mr Test User", "Test Address");

        // Then
        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getFailCode()).isEqualTo("2");
        assertThat(result.getErrorMessage()).contains("Datastore error");
    }
}
