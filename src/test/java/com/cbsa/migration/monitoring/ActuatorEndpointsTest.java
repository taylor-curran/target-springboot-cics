package com.cbsa.migration.monitoring;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;

import static org.hamcrest.Matchers.hasItems;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureMockMvc
class ActuatorEndpointsTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void healthReturnsUp() throws Exception {
        mockMvc.perform(get("/actuator/health"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.status").value("UP"));
    }

    @Test
    void metricsExposeJvmAndHttp() throws Exception {
        mockMvc.perform(get("/actuator/metrics"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.names", hasItems("jvm.memory.used", "http.server.requests")));
    }
}
