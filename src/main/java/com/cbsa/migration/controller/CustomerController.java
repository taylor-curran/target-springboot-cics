package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerInquiryResponseDto;
import com.cbsa.migration.service.CustomerInquiryService;
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

import javax.validation.constraints.Pattern;

@RestController
@RequestMapping("/api/customers")
@Tag(name = "Customer", description = "Customer inquiry and lookup services")
public class CustomerController {

    private static final Logger logger = LoggerFactory.getLogger(CustomerController.class);

    private final CustomerInquiryService customerInquiryService;

    public CustomerController(CustomerInquiryService customerInquiryService) {
        this.customerInquiryService = customerInquiryService;
    }

    @GetMapping("/{customerNumber}")
    @Operation(
        summary = "Inquire customer information",
        description = "Retrieve customer details by customer number and sort code. " +
                     "Special values: customerNumber=0 returns random customer, " +
                     "customerNumber=9999999999 returns last customer. " +
                     "Equivalent to COBOL INQCUST program functionality."
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Customer inquiry successful"),
        @ApiResponse(responseCode = "404", description = "Customer not found"),
        @ApiResponse(responseCode = "500", description = "Internal system error")
    })
    public ResponseEntity<CustomerInquiryResponseDto> inquireCustomer(
            @Parameter(description = "Customer number (0=random, 9999999999=last)", required = true)
            @PathVariable Long customerNumber,
            
            @Parameter(description = "6-digit bank sort code", required = true)
            @RequestParam 
            @Pattern(regexp = "\\d{6}", message = "Sort code must be exactly 6 digits")
            String sortCode) {
        
        logger.info("Received customer inquiry: customerNumber={}, sortCode={}", 
                   customerNumber, sortCode);

        CustomerInquiryResponseDto response = customerInquiryService.inquireCustomer(sortCode, customerNumber);
        
        if (response.getSuccess()) {
            logger.info("Customer inquiry successful: {}-{}", sortCode, customerNumber);
            return ResponseEntity.ok(response);
        } else {
            HttpStatus status = mapFailureCodeToHttpStatus(response.getFailureCode());
            
            logger.warn("Customer inquiry failed: {}-{}, failureCode={}, message={}", 
                       sortCode, customerNumber, response.getFailureCode(), response.getErrorMessage());
            
            return ResponseEntity.status(status).body(response);
        }
    }

    private HttpStatus mapFailureCodeToHttpStatus(String failureCode) {
        if (failureCode == null) {
            return HttpStatus.INTERNAL_SERVER_ERROR;
        }
        
        switch (failureCode) {
            case "1":
                return HttpStatus.NOT_FOUND;
            case "0":
                return HttpStatus.OK;
            default:
                return HttpStatus.INTERNAL_SERVER_ERROR;
        }
    }
}
