package com.cbsa.migration.service;

import com.cbsa.migration.dto.AccountResponseDto;
import com.cbsa.migration.dto.mapper.DtoMapper;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.repository.AccountRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class AccountInquiryServiceTest {

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private DtoMapper dtoMapper;

    @InjectMocks
    private AccountInquiryService accountInquiryService;

    @Test
    void testGetAccount_whenAccountExists_returnsAccountWithAll12Fields() {
        String sortCode = "987654";
        String accountNumber = "00000001";
        
        Account mockAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1L)
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("1.50"))
                .openedDate(LocalDate.of(2020, 1, 15))
                .overdraftLimit(1000)
                .lastStatementDate(LocalDate.of(2024, 10, 1))
                .nextStatementDate(LocalDate.of(2024, 11, 1))
                .availableBalance(new BigDecimal("5000.00"))
                .actualBalance(new BigDecimal("5000.00"))
                .build();
        
        AccountResponseDto expectedDto = AccountResponseDto.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1L)
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .accountType("CURRENT")
                .interestRate(new BigDecimal("1.50"))
                .openedDate(LocalDate.of(2020, 1, 15))
                .overdraftLimit(1000)
                .lastStatementDate(LocalDate.of(2024, 10, 1))
                .nextStatementDate(LocalDate.of(2024, 11, 1))
                .availableBalance(new BigDecimal("5000.00"))
                .actualBalance(new BigDecimal("5000.00"))
                .status("ACTIVE")
                .build();
        
        when(accountRepository.findById(sortCode, accountNumber)).thenReturn(Optional.of(mockAccount));
        when(dtoMapper.toAccountResponseDto(mockAccount)).thenReturn(expectedDto);
        
        Optional<AccountResponseDto> result = accountInquiryService.getAccount(sortCode, accountNumber);
        
        assertThat(result).isPresent();
        AccountResponseDto dto = result.get();
        
        assertThat(dto.getEyeCatcher()).isEqualTo("ACCT");
        assertThat(dto.getCustomerNumber()).isEqualTo(1L);
        assertThat(dto.getSortCode()).isEqualTo(sortCode);
        assertThat(dto.getAccountNumber()).isEqualTo(accountNumber);
        assertThat(dto.getAccountType()).isEqualTo("CURRENT");
        assertThat(dto.getInterestRate()).isEqualByComparingTo(new BigDecimal("1.50"));
        assertThat(dto.getOpenedDate()).isEqualTo(LocalDate.of(2020, 1, 15));
        assertThat(dto.getOverdraftLimit()).isEqualTo(1000);
        assertThat(dto.getLastStatementDate()).isEqualTo(LocalDate.of(2024, 10, 1));
        assertThat(dto.getNextStatementDate()).isEqualTo(LocalDate.of(2024, 11, 1));
        assertThat(dto.getAvailableBalance()).isEqualByComparingTo(new BigDecimal("5000.00"));
        assertThat(dto.getActualBalance()).isEqualByComparingTo(new BigDecimal("5000.00"));
        
        verify(accountRepository).findById(sortCode, accountNumber);
        verify(dtoMapper).toAccountResponseDto(mockAccount);
    }

    @Test
    void testGetAccount_whenAccountNotFound_returnsEmpty() {
        String sortCode = "987654";
        String accountNumber = "99999999";
        
        when(accountRepository.findById(sortCode, accountNumber)).thenReturn(Optional.empty());
        
        Optional<AccountResponseDto> result = accountInquiryService.getAccount(sortCode, accountNumber);
        
        assertThat(result).isEmpty();
        verify(accountRepository).findById(sortCode, accountNumber);
    }

    @Test
    void testGetAccount_withDifferentSortCode_callsRepositoryWithCorrectParameters() {
        String sortCode = "123456";
        String accountNumber = "87654321";
        
        when(accountRepository.findById(sortCode, accountNumber)).thenReturn(Optional.empty());
        
        accountInquiryService.getAccount(sortCode, accountNumber);
        
        verify(accountRepository).findById(sortCode, accountNumber);
    }

    @Test
    void testGetAccount_withNullableFields_handlesCorrectly() {
        String sortCode = "987654";
        String accountNumber = "00000002";
        
        Account mockAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(2L)
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("2.00"))
                .openedDate(LocalDate.of(2021, 5, 10))
                .overdraftLimit(0)
                .lastStatementDate(null)
                .nextStatementDate(null)
                .availableBalance(new BigDecimal("10000.00"))
                .actualBalance(new BigDecimal("10000.00"))
                .build();
        
        AccountResponseDto expectedDto = AccountResponseDto.builder()
                .eyeCatcher("ACCT")
                .customerNumber(2L)
                .sortCode(sortCode)
                .accountNumber(accountNumber)
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("2.00"))
                .openedDate(LocalDate.of(2021, 5, 10))
                .overdraftLimit(0)
                .lastStatementDate(null)
                .nextStatementDate(null)
                .availableBalance(new BigDecimal("10000.00"))
                .actualBalance(new BigDecimal("10000.00"))
                .status("ACTIVE")
                .build();
        
        when(accountRepository.findById(sortCode, accountNumber)).thenReturn(Optional.of(mockAccount));
        when(dtoMapper.toAccountResponseDto(mockAccount)).thenReturn(expectedDto);
        
        Optional<AccountResponseDto> result = accountInquiryService.getAccount(sortCode, accountNumber);
        
        assertThat(result).isPresent();
        AccountResponseDto dto = result.get();
        assertThat(dto.getLastStatementDate()).isNull();
        assertThat(dto.getNextStatementDate()).isNull();
    }
}
