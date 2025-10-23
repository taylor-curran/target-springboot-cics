package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DebitCreditResponseDto {
    
    private BigDecimal availableBalance;
    
    private BigDecimal actualBalance;
    
    private Character success;
    
    private Character failCode;
    
    public static DebitCreditResponseDto success(BigDecimal availableBalance, BigDecimal actualBalance) {
        return DebitCreditResponseDto.builder()
            .availableBalance(availableBalance)
            .actualBalance(actualBalance)
            .success('Y')
            .failCode('0')
            .build();
    }
    
    public static DebitCreditResponseDto failure(Character failCode) {
        return DebitCreditResponseDto.builder()
            .success('N')
            .failCode(failCode)
            .build();
    }
}
