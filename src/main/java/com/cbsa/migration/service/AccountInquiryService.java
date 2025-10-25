package com.cbsa.migration.service;

import com.cbsa.migration.dto.AccountResponseDto;
import com.cbsa.migration.dto.mapper.DtoMapper;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.repository.AccountRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class AccountInquiryService {

    private final AccountRepository accountRepository;
    private final DtoMapper dtoMapper;

    @Autowired
    public AccountInquiryService(AccountRepository accountRepository, DtoMapper dtoMapper) {
        this.accountRepository = accountRepository;
        this.dtoMapper = dtoMapper;
    }

    public Optional<AccountResponseDto> getAccount(String sortCode, String accountNumber) {
        Optional<Account> accountOpt = accountRepository.findById(sortCode, accountNumber);
        
        return accountOpt.map(dtoMapper::toAccountResponseDto);
    }
}
