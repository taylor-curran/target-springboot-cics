package com.cbsa.migration.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.UUID;

@Component
@Order(Ordered.HIGHEST_PRECEDENCE)
public class CorrelationIdFilter implements Filter {
    
    private static final Logger logger = LoggerFactory.getLogger(CorrelationIdFilter.class);
    private static final String CORRELATION_ID_HEADER = "X-Correlation-ID";
    private static final String CORRELATION_ID_MDC_KEY = "correlationId";
    
    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
    }
    
    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;
        
        try {
            String correlationId = httpRequest.getHeader(CORRELATION_ID_HEADER);
            if (correlationId == null || correlationId.trim().isEmpty()) {
                correlationId = generateCorrelationId();
            }
            
            MDC.put(CORRELATION_ID_MDC_KEY, correlationId);
            
            httpResponse.setHeader(CORRELATION_ID_HEADER, correlationId);
            
            logger.info("Request: {} {} - Correlation ID: {}", 
                    httpRequest.getMethod(), 
                    httpRequest.getRequestURI(), 
                    correlationId);
            
            long startTime = System.currentTimeMillis();
            
            try {
                chain.doFilter(request, response);
            } finally {
                long duration = System.currentTimeMillis() - startTime;
                logger.info("Response: {} {} - Status: {} - Duration: {}ms", 
                        httpRequest.getMethod(), 
                        httpRequest.getRequestURI(), 
                        httpResponse.getStatus(),
                        duration);
            }
            
        } finally {
            MDC.clear();
        }
    }
    
    @Override
    public void destroy() {
    }
    
    private String generateCorrelationId() {
        return UUID.randomUUID().toString();
    }
}
