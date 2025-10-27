package com.cbsa.migration.monitoring;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;

import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.isEmptyString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureMockMvc
class CorrelationIdIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void statusEndpointReturnsCorrelationIdHeader() throws Exception {
        mockMvc.perform(get("/api/status"))
                .andExpect(status().isOk())
                .andExpect(header().exists("X-Correlation-Id"))
                .andExpect(header().string("X-Correlation-Id", not(isEmptyString())));
    }
}
