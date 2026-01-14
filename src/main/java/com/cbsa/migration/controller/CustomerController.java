package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerRequestDto;
import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.dto.mapper.DtoMapper;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.service.CustomerService;
import com.cbsa.migration.service.CustomerService.DeleteResult;
import com.cbsa.migration.service.CustomerService.UpdateResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * REST Controller for Customer operations migrated from COBOL BNK1DCS program.
 * 
 * This controller provides endpoints for:
 * - Customer inquiry (GET) - migrated from INQCUST
 * - Customer deletion (DELETE) - migrated from DELCUS
 * - Customer update (PUT) - migrated from UPDCUST
 * 
 * The original BNK1DCS.cbl was a BMS 3270 interface program that delegated
 * business operations to INQCUST, DELCUS, and UPDCUST programs.
 */
@RestController
@RequestMapping("/api/customers")
@Tag(name = "Customer", description = "Customer management operations migrated from BNK1DCS COBOL program")
public class CustomerController {

    private static final Logger logger = LoggerFactory.getLogger(CustomerController.class);

    private final CustomerService customerService;
    private final DtoMapper dtoMapper;

    public CustomerController(CustomerService customerService, DtoMapper dtoMapper) {
        this.customerService = customerService;
        this.dtoMapper = dtoMapper;
    }

    /**
     * Retrieves a customer by customer number.
     * Corresponds to INQCUST COBOL program functionality.
     * 
     * @param customerNumber the customer number to look up
     * @return the customer details if found, or 404 if not found
     */
    @GetMapping("/{customerNumber}")
    @Operation(summary = "Get customer by number", 
               description = "Retrieves customer details by customer number. Migrated from INQCUST COBOL program.")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Customer found",
                     content = @Content(schema = @Schema(implementation = CustomerResponseDto.class))),
        @ApiResponse(responseCode = "400", description = "Invalid customer number"),
        @ApiResponse(responseCode = "404", description = "Customer not found")
    })
    public ResponseEntity<?> getCustomer(
            @Parameter(description = "Customer number (10-digit identifier)")
            @PathVariable Long customerNumber) {
        
        logger.info("GET /api/customers/{} - Customer inquiry request", customerNumber);
        
        if (customerNumber == null || customerNumber <= 0) {
            logger.warn("Invalid customer number: {}", customerNumber);
            return ResponseEntity.badRequest()
                    .body(createErrorResponse("Invalid customer number"));
        }
        
        Optional<Customer> customer = customerService.getCustomer(customerNumber);
        
        if (customer.isEmpty()) {
            logger.info("Customer {} not found", customerNumber);
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(createErrorResponse("Customer " + customerNumber + " not found"));
        }
        
        CustomerResponseDto response = dtoMapper.toCustomerResponseDto(customer.get());
        logger.info("Customer {} found successfully", customerNumber);
        return ResponseEntity.ok(response);
    }

    /**
     * Deletes a customer and all associated accounts.
     * Corresponds to DELCUS COBOL program functionality (triggered by PF5 in BNK1DCS).
     * 
     * This operation performs cascade deletion:
     * 1. Deletes all accounts associated with the customer
     * 2. Deletes the customer record
     * 
     * @param customerNumber the customer number to delete
     * @return success message with deleted customer details, or error response
     */
    @DeleteMapping("/{customerNumber}")
    @Operation(summary = "Delete customer", 
               description = "Deletes a customer and all associated accounts. Migrated from DELCUS COBOL program (PF5 action in BNK1DCS).")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Customer deleted successfully"),
        @ApiResponse(responseCode = "400", description = "Invalid customer number"),
        @ApiResponse(responseCode = "404", description = "Customer not found"),
        @ApiResponse(responseCode = "500", description = "Datastore or delete operation error")
    })
    public ResponseEntity<?> deleteCustomer(
            @Parameter(description = "Customer number to delete")
            @PathVariable Long customerNumber) {
        
        logger.info("DELETE /api/customers/{} - Customer deletion request", customerNumber);
        
        if (customerNumber == null || customerNumber <= 0) {
            logger.warn("Invalid customer number for deletion: {}", customerNumber);
            return ResponseEntity.badRequest()
                    .body(createErrorResponse("Invalid customer number"));
        }
        
        DeleteResult result = customerService.deleteCustomer(customerNumber);
        
        if (!result.isSuccess()) {
            HttpStatus status = mapDeleteFailCodeToStatus(result.getFailCode());
            logger.warn("Customer deletion failed for {}: {} (code: {})", 
                    customerNumber, result.getErrorMessage(), result.getFailCode());
            return ResponseEntity.status(status)
                    .body(createErrorResponse(result.getErrorMessage()));
        }
        
        Map<String, Object> response = new HashMap<>();
        response.put("message", "Customer " + customerNumber + " and " + 
                result.getDeletedAccountCount() + " associated accounts were successfully deleted");
        response.put("customerNumber", customerNumber);
        response.put("deletedAccountCount", result.getDeletedAccountCount());
        
        if (result.getDeletedCustomer() != null) {
            response.put("deletedCustomer", dtoMapper.toCustomerResponseDto(result.getDeletedCustomer()));
        }
        
        logger.info("Customer {} deleted successfully with {} accounts", 
                customerNumber, result.getDeletedAccountCount());
        return ResponseEntity.ok(response);
    }

    /**
     * Updates a customer's name and/or address.
     * Corresponds to UPDCUST COBOL program functionality (triggered by PF10 + ENTER in BNK1DCS).
     * 
     * Validation rules:
     * - Name must start with a valid title (Professor, Mr, Mrs, Miss, Ms, Dr, Drs, Lord, Sir, Lady)
     * - At least one of name or address must be provided
     * - Neither name nor address can be all spaces
     * 
     * @param customerNumber the customer number to update
     * @param request the update request containing name and/or address
     * @return updated customer details, or error response
     */
    @PutMapping("/{customerNumber}")
    @Operation(summary = "Update customer", 
               description = "Updates customer name and/or address. Migrated from UPDCUST COBOL program (PF10 action in BNK1DCS).")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Customer updated successfully",
                     content = @Content(schema = @Schema(implementation = CustomerResponseDto.class))),
        @ApiResponse(responseCode = "400", description = "Invalid input (empty fields or invalid title)"),
        @ApiResponse(responseCode = "404", description = "Customer not found"),
        @ApiResponse(responseCode = "500", description = "Datastore or update operation error")
    })
    public ResponseEntity<?> updateCustomer(
            @Parameter(description = "Customer number to update")
            @PathVariable Long customerNumber,
            @RequestBody CustomerUpdateRequest request) {
        
        logger.info("PUT /api/customers/{} - Customer update request", customerNumber);
        
        if (customerNumber == null || customerNumber <= 0) {
            logger.warn("Invalid customer number for update: {}", customerNumber);
            return ResponseEntity.badRequest()
                    .body(createErrorResponse("Invalid customer number"));
        }
        
        UpdateResult result = customerService.updateCustomer(
                customerNumber, 
                request.getName(), 
                request.getAddress()
        );
        
        if (!result.isSuccess()) {
            HttpStatus status = mapUpdateFailCodeToStatus(result.getFailCode());
            logger.warn("Customer update failed for {}: {} (code: {})", 
                    customerNumber, result.getErrorMessage(), result.getFailCode());
            return ResponseEntity.status(status)
                    .body(createErrorResponse(result.getErrorMessage()));
        }
        
        CustomerResponseDto response = dtoMapper.toCustomerResponseDto(result.getUpdatedCustomer());
        logger.info("Customer {} updated successfully", customerNumber);
        return ResponseEntity.ok(response);
    }

    /**
     * Maps COBOL-style delete fail codes to HTTP status codes.
     */
    private HttpStatus mapDeleteFailCodeToStatus(String failCode) {
        if (failCode == null) {
            return HttpStatus.INTERNAL_SERVER_ERROR;
        }
        switch (failCode) {
            case "1":
                return HttpStatus.NOT_FOUND;
            case "2":
            case "3":
                return HttpStatus.INTERNAL_SERVER_ERROR;
            default:
                return HttpStatus.INTERNAL_SERVER_ERROR;
        }
    }

    /**
     * Maps COBOL-style update fail codes to HTTP status codes.
     */
    private HttpStatus mapUpdateFailCodeToStatus(String failCode) {
        if (failCode == null) {
            return HttpStatus.INTERNAL_SERVER_ERROR;
        }
        switch (failCode) {
            case "1":
                return HttpStatus.NOT_FOUND;
            case "4":
            case "T":
                return HttpStatus.BAD_REQUEST;
            case "2":
            case "3":
                return HttpStatus.INTERNAL_SERVER_ERROR;
            default:
                return HttpStatus.INTERNAL_SERVER_ERROR;
        }
    }

    /**
     * Creates a standardized error response.
     */
    private Map<String, String> createErrorResponse(String message) {
        Map<String, String> error = new HashMap<>();
        error.put("errorMessage", message);
        return error;
    }

    /**
     * Request DTO for customer update operations.
     * Allows partial updates - either name or address can be provided.
     */
    public static class CustomerUpdateRequest {
        private String name;
        private String address;

        public CustomerUpdateRequest() {}

        public CustomerUpdateRequest(String name, String address) {
            this.name = name;
            this.address = address;
        }

        public String getName() { return name; }
        public void setName(String name) { this.name = name; }
        public String getAddress() { return address; }
        public void setAddress(String address) { this.address = address; }
    }
}
