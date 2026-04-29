package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CustomerRequestDto;
import com.cbsa.migration.dto.CustomerResponseDto;
import com.cbsa.migration.service.CustomerCreationException;
import com.cbsa.migration.service.CustomerService;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Collections;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@ExtendWith(MockitoExtension.class)
class CustomerControllerTest {

    @Mock
    private CustomerService customerService;

    private MockMvc mockMvc;
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        CustomerController controller = new CustomerController(customerService);
        mockMvc = MockMvcBuilders.standaloneSetup(controller).build();
        objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
    }

    // --- POST /api/customers ---

    @Test
    void createCustomer_success_returns201() throws Exception {
        CustomerRequestDto request = buildValidRequest();
        CustomerResponseDto response = CustomerResponseDto.builder()
                .sortCode("987654")
                .customerNumber(100001L)
                .name("John Smith")
                .status("CREATED")
                .success(true)
                .creditScore(750)
                .build();

        when(customerService.createCustomer(any(CustomerRequestDto.class))).thenReturn(response);

        mockMvc.perform(post("/api/customers")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.customerNumber").value(100001))
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.status").value("CREATED"));
    }

    @Test
    void createCustomer_dobValidationFails_returns400() throws Exception {
        CustomerRequestDto request = buildValidRequest();
        when(customerService.createCustomer(any(CustomerRequestDto.class)))
                .thenThrow(new CustomerCreationException("O", "Customer too old"));

        mockMvc.perform(post("/api/customers")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.failCode").value("O"))
                .andExpect(jsonPath("$.success").value(false));
    }

    @Test
    void createCustomer_futureDob_returns400() throws Exception {
        CustomerRequestDto request = buildValidRequest();
        when(customerService.createCustomer(any(CustomerRequestDto.class)))
                .thenThrow(new CustomerCreationException("Y", "DOB in future"));

        mockMvc.perform(post("/api/customers")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.failCode").value("Y"));
    }

    @Test
    void createCustomer_lockFailure_returns503() throws Exception {
        CustomerRequestDto request = buildValidRequest();
        when(customerService.createCustomer(any(CustomerRequestDto.class)))
                .thenThrow(new CustomerCreationException("3", "Lock failure"));

        mockMvc.perform(post("/api/customers")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isServiceUnavailable())
                .andExpect(jsonPath("$.failCode").value("3"));
    }

    @Test
    void createCustomer_vsamWriteFailure_returns500() throws Exception {
        CustomerRequestDto request = buildValidRequest();
        when(customerService.createCustomer(any(CustomerRequestDto.class)))
                .thenThrow(new CustomerCreationException("1", "VSAM write error"));

        mockMvc.perform(post("/api/customers")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.failCode").value("1"));
    }

    @Test
    void createCustomer_unexpectedException_returns500() throws Exception {
        CustomerRequestDto request = buildValidRequest();
        when(customerService.createCustomer(any(CustomerRequestDto.class)))
                .thenThrow(new RuntimeException("Unexpected"));

        mockMvc.perform(post("/api/customers")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.success").value(false));
    }

    // --- GET /api/customers/{sortCode}/{customerNumber} ---

    @Test
    void getCustomer_found_returns200() throws Exception {
        CustomerResponseDto response = CustomerResponseDto.builder()
                .sortCode("987654")
                .customerNumber(100001L)
                .name("John Smith")
                .creditScore(750)
                .build();

        when(customerService.getCustomer("987654", 100001L)).thenReturn(response);

        mockMvc.perform(get("/api/customers/987654/100001"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value("John Smith"))
                .andExpect(jsonPath("$.customerNumber").value(100001))
                .andExpect(jsonPath("$.creditScore").value(750));
    }

    @Test
    void getCustomer_notFound_returns404() throws Exception {
        when(customerService.getCustomer("987654", 999999L))
                .thenThrow(new CustomerService.CustomerNotFoundException("Not found"));

        mockMvc.perform(get("/api/customers/987654/999999"))
                .andExpect(status().isNotFound());
    }

    // --- GET /api/customers?name= ---

    @Test
    void getCustomerByName_found_returnsList() throws Exception {
        CustomerResponseDto dto = CustomerResponseDto.builder()
                .name("John Smith")
                .sortCode("987654")
                .customerNumber(100001L)
                .build();

        when(customerService.getCustomerByName("Smith")).thenReturn(Arrays.asList(dto));

        mockMvc.perform(get("/api/customers").param("name", "Smith"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$[0].name").value("John Smith"));
    }

    @Test
    void getCustomerByName_notFound_returnsEmptyList() throws Exception {
        when(customerService.getCustomerByName("Nobody")).thenReturn(Collections.emptyList());

        mockMvc.perform(get("/api/customers").param("name", "Nobody"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").isEmpty());
    }

    // --- Helpers ---

    private CustomerRequestDto buildValidRequest() {
        return CustomerRequestDto.builder()
                .name("John Smith")
                .address("123 Main St, London")
                .dateOfBirth(LocalDate.of(1985, 6, 15))
                .sortCode("987654")
                .build();
    }
}
