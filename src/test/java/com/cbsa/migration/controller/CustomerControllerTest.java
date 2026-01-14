package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.dto.mapper.DtoMapper;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.service.CustomerService;
import com.cbsa.migration.service.CustomerService.DeleteResult;
import com.cbsa.migration.service.CustomerService.UpdateResult;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.time.LocalDate;
import java.util.Optional;

import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(CustomerController.class)
class CustomerControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @MockBean
    private CustomerService customerService;

    @MockBean
    private DtoMapper dtoMapper;

    private static final String SORT_CODE = "987654";

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

    private CustomerResponseDto createTestCustomerResponseDto(Long customerNumber) {
        return CustomerResponseDto.builder()
                .customerNumber(customerNumber)
                .sortCode(SORT_CODE)
                .name("Mr John Smith")
                .address("123 Main Street, London")
                .dateOfBirth(LocalDate.of(1980, 1, 15))
                .creditScore(750)
                .creditScoreReviewDate(LocalDate.of(2025, 6, 1))
                .status("ACTIVE")
                .build();
    }

    @Test
    void getCustomer_validCustomerNumber_returnsCustomer() throws Exception {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);
        CustomerResponseDto responseDto = createTestCustomerResponseDto(customerNumber);

        when(customerService.getCustomer(customerNumber)).thenReturn(Optional.of(customer));
        when(dtoMapper.toCustomerResponseDto(customer)).thenReturn(responseDto);

        // When & Then
        mockMvc.perform(get("/api/customers/{customerNumber}", customerNumber))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.customerNumber", is(customerNumber.intValue())))
                .andExpect(jsonPath("$.name", is("Mr John Smith")))
                .andExpect(jsonPath("$.creditScore", is(750)));
    }

    @Test
    void getCustomer_customerNotFound_returns404() throws Exception {
        // Given
        Long customerNumber = 9999999L;
        when(customerService.getCustomer(customerNumber)).thenReturn(Optional.empty());

        // When & Then
        mockMvc.perform(get("/api/customers/{customerNumber}", customerNumber))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.errorMessage").exists());
    }

    @Test
    void getCustomer_invalidCustomerNumber_returns400() throws Exception {
        // When & Then
        mockMvc.perform(get("/api/customers/{customerNumber}", -1))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage", is("Invalid customer number")));
    }

    @Test
    void getCustomer_zeroCustomerNumber_returns400() throws Exception {
        // When & Then
        mockMvc.perform(get("/api/customers/{customerNumber}", 0))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage", is("Invalid customer number")));
    }

    @Test
    void deleteCustomer_validCustomer_returnsSuccess() throws Exception {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);
        CustomerResponseDto responseDto = createTestCustomerResponseDto(customerNumber);
        DeleteResult deleteResult = new DeleteResult(true, null, null, customer, 2);

        when(customerService.deleteCustomer(customerNumber)).thenReturn(deleteResult);
        when(dtoMapper.toCustomerResponseDto(customer)).thenReturn(responseDto);

        // When & Then
        mockMvc.perform(delete("/api/customers/{customerNumber}", customerNumber))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message").exists())
                .andExpect(jsonPath("$.customerNumber", is(customerNumber.intValue())))
                .andExpect(jsonPath("$.deletedAccountCount", is(2)));
    }

    @Test
    void deleteCustomer_customerNotFound_returns404() throws Exception {
        // Given
        Long customerNumber = 9999999L;
        DeleteResult deleteResult = new DeleteResult(false, "1", "Customer not found");

        when(customerService.deleteCustomer(customerNumber)).thenReturn(deleteResult);

        // When & Then
        mockMvc.perform(delete("/api/customers/{customerNumber}", customerNumber))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.errorMessage", is("Customer not found")));
    }

    @Test
    void deleteCustomer_datastoreError_returns500() throws Exception {
        // Given
        Long customerNumber = 1000001L;
        DeleteResult deleteResult = new DeleteResult(false, "2", "Datastore error");

        when(customerService.deleteCustomer(customerNumber)).thenReturn(deleteResult);

        // When & Then
        mockMvc.perform(delete("/api/customers/{customerNumber}", customerNumber))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.errorMessage", is("Datastore error")));
    }

    @Test
    void deleteCustomer_deleteOperationFails_returns500() throws Exception {
        // Given
        Long customerNumber = 1000001L;
        DeleteResult deleteResult = new DeleteResult(false, "3", "Delete operation failed");

        when(customerService.deleteCustomer(customerNumber)).thenReturn(deleteResult);

        // When & Then
        mockMvc.perform(delete("/api/customers/{customerNumber}", customerNumber))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.errorMessage", is("Delete operation failed")));
    }

    @Test
    void deleteCustomer_invalidCustomerNumber_returns400() throws Exception {
        // When & Then
        mockMvc.perform(delete("/api/customers/{customerNumber}", -1))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage", is("Invalid customer number")));
    }

    @Test
    void updateCustomer_validUpdate_returnsSuccess() throws Exception {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);
        customer.setName("Dr Jane Doe");
        customer.setAddress("456 New Street, Manchester");
        CustomerResponseDto responseDto = createTestCustomerResponseDto(customerNumber);
        responseDto = CustomerResponseDto.builder()
                .customerNumber(customerNumber)
                .sortCode(SORT_CODE)
                .name("Dr Jane Doe")
                .address("456 New Street, Manchester")
                .dateOfBirth(LocalDate.of(1980, 1, 15))
                .creditScore(750)
                .creditScoreReviewDate(LocalDate.of(2025, 6, 1))
                .status("ACTIVE")
                .build();
        UpdateResult updateResult = new UpdateResult(true, null, null, customer);

        when(customerService.updateCustomer(eq(customerNumber), anyString(), anyString())).thenReturn(updateResult);
        when(dtoMapper.toCustomerResponseDto(customer)).thenReturn(responseDto);

        String requestBody = "{\"name\": \"Dr Jane Doe\", \"address\": \"456 New Street, Manchester\"}";

        // When & Then
        mockMvc.perform(put("/api/customers/{customerNumber}", customerNumber)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(requestBody))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name", is("Dr Jane Doe")))
                .andExpect(jsonPath("$.address", is("456 New Street, Manchester")));
    }

    @Test
    void updateCustomer_customerNotFound_returns404() throws Exception {
        // Given
        Long customerNumber = 9999999L;
        UpdateResult updateResult = new UpdateResult(false, "1", "Customer not found");

        when(customerService.updateCustomer(eq(customerNumber), anyString(), anyString())).thenReturn(updateResult);

        String requestBody = "{\"name\": \"Mr Test User\", \"address\": \"Test Address\"}";

        // When & Then
        mockMvc.perform(put("/api/customers/{customerNumber}", customerNumber)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(requestBody))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.errorMessage", is("Customer not found")));
    }

    @Test
    void updateCustomer_bothFieldsEmpty_returns400() throws Exception {
        // Given
        Long customerNumber = 1000001L;
        UpdateResult updateResult = new UpdateResult(false, "4", "Both name and address cannot be empty");

        when(customerService.updateCustomer(eq(customerNumber), isNull(), isNull())).thenReturn(updateResult);

        String requestBody = "{}";

        // When & Then
        mockMvc.perform(put("/api/customers/{customerNumber}", customerNumber)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(requestBody))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage", is("Both name and address cannot be empty")));
    }

    @Test
    void updateCustomer_invalidTitle_returns400() throws Exception {
        // Given
        Long customerNumber = 1000001L;
        UpdateResult updateResult = new UpdateResult(false, "T", "Invalid title");

        when(customerService.updateCustomer(eq(customerNumber), anyString(), anyString())).thenReturn(updateResult);

        String requestBody = "{\"name\": \"InvalidTitle John Smith\", \"address\": \"123 Main Street\"}";

        // When & Then
        mockMvc.perform(put("/api/customers/{customerNumber}", customerNumber)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(requestBody))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage", is("Invalid title")));
    }

    @Test
    void updateCustomer_datastoreError_returns500() throws Exception {
        // Given
        Long customerNumber = 1000001L;
        UpdateResult updateResult = new UpdateResult(false, "2", "Datastore error");

        when(customerService.updateCustomer(eq(customerNumber), anyString(), anyString())).thenReturn(updateResult);

        String requestBody = "{\"name\": \"Mr Test User\", \"address\": \"Test Address\"}";

        // When & Then
        mockMvc.perform(put("/api/customers/{customerNumber}", customerNumber)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(requestBody))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.errorMessage", is("Datastore error")));
    }

    @Test
    void updateCustomer_updateOperationFails_returns500() throws Exception {
        // Given
        Long customerNumber = 1000001L;
        UpdateResult updateResult = new UpdateResult(false, "3", "Update operation failed");

        when(customerService.updateCustomer(eq(customerNumber), anyString(), anyString())).thenReturn(updateResult);

        String requestBody = "{\"name\": \"Mr Test User\", \"address\": \"Test Address\"}";

        // When & Then
        mockMvc.perform(put("/api/customers/{customerNumber}", customerNumber)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(requestBody))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.errorMessage", is("Update operation failed")));
    }

    @Test
    void updateCustomer_invalidCustomerNumber_returns400() throws Exception {
        // When & Then
        String requestBody = "{\"name\": \"Mr Test User\", \"address\": \"Test Address\"}";

        mockMvc.perform(put("/api/customers/{customerNumber}", -1)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(requestBody))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage", is("Invalid customer number")));
    }

    @Test
    void updateCustomer_nameOnlyUpdate_returnsSuccess() throws Exception {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);
        customer.setName("Mrs Sarah Connor");
        CustomerResponseDto responseDto = createTestCustomerResponseDto(customerNumber);
        UpdateResult updateResult = new UpdateResult(true, null, null, customer);

        when(customerService.updateCustomer(eq(customerNumber), anyString(), isNull())).thenReturn(updateResult);
        when(dtoMapper.toCustomerResponseDto(customer)).thenReturn(responseDto);

        String requestBody = "{\"name\": \"Mrs Sarah Connor\"}";

        // When & Then
        mockMvc.perform(put("/api/customers/{customerNumber}", customerNumber)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(requestBody))
                .andExpect(status().isOk());
    }

    @Test
    void updateCustomer_addressOnlyUpdate_returnsSuccess() throws Exception {
        // Given
        Long customerNumber = 1000001L;
        Customer customer = createTestCustomer(customerNumber);
        customer.setAddress("789 Another Road, Birmingham");
        CustomerResponseDto responseDto = createTestCustomerResponseDto(customerNumber);
        UpdateResult updateResult = new UpdateResult(true, null, null, customer);

        when(customerService.updateCustomer(eq(customerNumber), isNull(), anyString())).thenReturn(updateResult);
        when(dtoMapper.toCustomerResponseDto(customer)).thenReturn(responseDto);

        String requestBody = "{\"address\": \"789 Another Road, Birmingham\"}";

        // When & Then
        mockMvc.perform(put("/api/customers/{customerNumber}", customerNumber)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(requestBody))
                .andExpect(status().isOk());
    }
}
