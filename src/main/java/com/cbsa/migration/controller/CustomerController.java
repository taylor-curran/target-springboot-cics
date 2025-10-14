package com.cbsa.migration.controller;

import com.cbsa.migration.dto.InquiryCustomerResponseDto;
import com.cbsa.migration.service.CustomerService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/customers")
@Tag(name = "Customer", description = "Customer inquiry and management services")
public class CustomerController {

    private static final Logger logger = LoggerFactory.getLogger(CustomerController.class);

    private final CustomerService customerService;

    public CustomerController(CustomerService customerService) {
        this.customerService = customerService;
    }

    @GetMapping("/{sortCode}/{customerNumber}")
    @Operation(
        summary = "Inquire customer information",
        description = "Retrieves customer details by sort code and customer number. " +
                     "Equivalent to COBOL INQCUST program functionality."
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Customer found successfully"),
        @ApiResponse(responseCode = "404", description = "Customer not found"),
        @ApiResponse(responseCode = "500", description = "Internal system error")
    })
    public ResponseEntity<InquiryCustomerResponseDto> inquireCustomer(
            @Parameter(description = "Bank sort code (6 digits)", required = true, example = "987654")
            @PathVariable String sortCode,
            @Parameter(description = "Customer number (10 digits)", required = true, example = "100001")
            @PathVariable Long customerNumber) {

        logger.info("Received customer inquiry request: sortCode={}, customerNumber={}", 
                   sortCode, customerNumber);

        InquiryCustomerResponseDto response = customerService.findCustomer(sortCode, customerNumber);

        if (response.getSuccess()) {
            logger.info("Customer inquiry successful: {}-{}", sortCode, customerNumber);
            return ResponseEntity.ok(response);
        } else {
            HttpStatus status = determineErrorStatus(response.getFailureCode());
            logger.warn("Customer inquiry failed: {}-{}, failureCode={}, status={}", 
                       sortCode, customerNumber, response.getFailureCode(), status.value());
            return ResponseEntity.status(status).body(response);
        }
    }

    private HttpStatus determineErrorStatus(String failureCode) {
        if (failureCode == null) {
            return HttpStatus.INTERNAL_SERVER_ERROR;
        }

        switch (failureCode) {
            case "0":
                return HttpStatus.OK;
            case "1":
                return HttpStatus.NOT_FOUND;
            case "2":
            default:
                return HttpStatus.INTERNAL_SERVER_ERROR;
        }
    }
}
