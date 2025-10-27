package com.cbsa.migration.config;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@SpringBootTest
@AutoConfigureMockMvc
class CorrelationIdFilterTest {

    @Autowired
    private MockMvc mockMvc;

    private static final String CORRELATION_ID_HEADER = "X-Correlation-ID";

    @Test
    void testCorrelationId_generatedWhenNotProvided() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/status"))
            .andExpect(status().isOk())
            .andExpect(header().exists(CORRELATION_ID_HEADER))
            .andReturn();
        
        String correlationId = result.getResponse().getHeader(CORRELATION_ID_HEADER);
        assertThat(correlationId).isNotNull();
        assertThat(correlationId).isNotEmpty();
        assertThat(correlationId).matches("[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}");
    }

    @Test
    void testCorrelationId_preservedWhenProvided() throws Exception {
        String providedCorrelationId = "test-correlation-id-12345";
        
        MvcResult result = mockMvc.perform(get("/api/status")
                .header(CORRELATION_ID_HEADER, providedCorrelationId))
            .andExpect(status().isOk())
            .andExpect(header().string(CORRELATION_ID_HEADER, providedCorrelationId))
            .andReturn();
        
        String returnedCorrelationId = result.getResponse().getHeader(CORRELATION_ID_HEADER);
        assertThat(returnedCorrelationId).isEqualTo(providedCorrelationId);
    }

    @Test
    void testCorrelationId_addedToAllEndpoints() throws Exception {
        mockMvc.perform(get("/actuator/health"))
            .andExpect(status().isOk())
            .andExpect(header().exists(CORRELATION_ID_HEADER));
        
        mockMvc.perform(get("/actuator/metrics"))
            .andExpect(status().isOk())
            .andExpect(header().exists(CORRELATION_ID_HEADER));
    }

    @Test
    void testCorrelationId_uniquePerRequest() throws Exception {
        MvcResult result1 = mockMvc.perform(get("/api/status"))
            .andExpect(status().isOk())
            .andReturn();
        
        MvcResult result2 = mockMvc.perform(get("/api/status"))
            .andExpect(status().isOk())
            .andReturn();
        
        String correlationId1 = result1.getResponse().getHeader(CORRELATION_ID_HEADER);
        String correlationId2 = result2.getResponse().getHeader(CORRELATION_ID_HEADER);
        
        assertThat(correlationId1).isNotEqualTo(correlationId2);
    }
}
