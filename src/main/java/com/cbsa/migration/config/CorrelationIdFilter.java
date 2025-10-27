package com.cbsa.migration.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.UUID;

@Component
public class CorrelationIdFilter extends OncePerRequestFilter {

    private static final Logger logger = LoggerFactory.getLogger(CorrelationIdFilter.class);
    private static final String CORRELATION_ID_HEADER = "X-Correlation-ID";
    private static final String CORRELATION_ID_MDC_KEY = "correlationId";

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {
        
        String correlationId = request.getHeader(CORRELATION_ID_HEADER);
        if (correlationId == null || correlationId.isEmpty()) {
            correlationId = UUID.randomUUID().toString();
        }
        
        MDC.put(CORRELATION_ID_MDC_KEY, correlationId);
        response.setHeader(CORRELATION_ID_HEADER, correlationId);
        
        long startTime = System.currentTimeMillis();
        
        try {
            logger.info("Request: method={}, uri={}, correlationId={}", 
                    request.getMethod(), request.getRequestURI(), correlationId);
            
            filterChain.doFilter(request, response);
            
            long duration = System.currentTimeMillis() - startTime;
            logger.info("Response: method={}, uri={}, status={}, duration={}ms, correlationId={}", 
                    request.getMethod(), request.getRequestURI(), response.getStatus(), duration, correlationId);
        } finally {
            MDC.remove(CORRELATION_ID_MDC_KEY);
        }
    }
}
