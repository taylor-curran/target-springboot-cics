package com.cbsa.migration.controller;

import com.cbsa.migration.dto.TransferRequestDto;
import com.cbsa.migration.dto.TransferResponseDto;
import com.cbsa.migration.service.TransferService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/transfer")
public class TransferController {

    private final TransferService transferService;

    public TransferController(TransferService transferService) {
        this.transferService = transferService;
    }

    @PostMapping("/local")
    public ResponseEntity<TransferResponseDto> transferLocal(@Valid @RequestBody TransferRequestDto request) {
        TransferResponseDto response = transferService.transferLocal(
                request.getFromSortCode(),
                request.getFromAccountNumber(),
                request.getToSortCode(),
                request.getToAccountNumber(),
                request.getAmount()
        );

        if (response.isSuccess()) {
            return ResponseEntity.ok(response);
        } else {
            return ResponseEntity.badRequest().body(response);
        }
    }
}
