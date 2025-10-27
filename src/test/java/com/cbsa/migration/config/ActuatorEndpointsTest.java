package com.cbsa.migration.config;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;

import static org.hamcrest.Matchers.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@SpringBootTest
@AutoConfigureMockMvc
class ActuatorEndpointsTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void testHealthEndpoint_returnsUpStatus() throws Exception {
        mockMvc.perform(get("/actuator/health"))
            .andExpect(status().isOk())
            .andExpect(content().contentType("application/vnd.spring-boot.actuator.v3+json"))
            .andExpect(jsonPath("$.status", is("UP")));
    }

    @Test
    void testHealthEndpoint_includesComponents() throws Exception {
        mockMvc.perform(get("/actuator/health"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.components").exists())
            .andExpect(jsonPath("$.components.db").exists())
            .andExpect(jsonPath("$.components.db.status", is("UP")));
    }

    @Test
    void testMetricsEndpoint_isAccessible() throws Exception {
        mockMvc.perform(get("/actuator/metrics"))
            .andExpect(status().isOk())
            .andExpect(content().contentType("application/vnd.spring-boot.actuator.v3+json"))
            .andExpect(jsonPath("$.names").isArray())
            .andExpect(jsonPath("$.names", hasItem("jvm.memory.used")))
            .andExpect(jsonPath("$.names", hasItem("jvm.threads.live")));
    }

    @Test
    void testSpecificMetric_jvmMemoryUsed() throws Exception {
        mockMvc.perform(get("/actuator/metrics/jvm.memory.used"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.name", is("jvm.memory.used")))
            .andExpect(jsonPath("$.measurements").isArray())
            .andExpect(jsonPath("$.measurements[0].value").isNumber());
    }

    @Test
    void testSpecificMetric_httpServerRequests() throws Exception {
        mockMvc.perform(get("/api/status"));
        
        mockMvc.perform(get("/actuator/metrics/http.server.requests"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.name", is("http.server.requests")))
            .andExpect(jsonPath("$.measurements").isArray());
    }
}
