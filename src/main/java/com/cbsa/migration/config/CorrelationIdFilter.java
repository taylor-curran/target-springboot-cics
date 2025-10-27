package com.cbsa.migration.config;

import org.slf4j.MDC;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.UUID;

@Component
@Order(1)
public class CorrelationIdFilter implements Filter {
    
    private static final String CORRELATION_ID_HEADER = "X-Correlation-ID";
    private static final String CORRELATION_ID_MDC_KEY = "correlationId";
    
    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;
        
        try {
            String correlationId = httpRequest.getHeader(CORRELATION_ID_HEADER);
            if (correlationId == null || correlationId.trim().isEmpty()) {
                correlationId = UUID.randomUUID().toString();
            }
            
            MDC.put(CORRELATION_ID_MDC_KEY, correlationId);
            
            httpResponse.setHeader(CORRELATION_ID_HEADER, correlationId);
            
            logRequest(httpRequest, correlationId);
            
            chain.doFilter(request, response);
            
            logResponse(httpResponse, correlationId);
            
        } finally {
            MDC.remove(CORRELATION_ID_MDC_KEY);
        }
    }
    
    private void logRequest(HttpServletRequest request, String correlationId) {
        org.slf4j.Logger logger = org.slf4j.LoggerFactory.getLogger(CorrelationIdFilter.class);
        logger.info("Incoming request: method={}, uri={}, correlationId={}", 
                    request.getMethod(), 
                    request.getRequestURI(), 
                    correlationId);
    }
    
    private void logResponse(HttpServletResponse response, String correlationId) {
        org.slf4j.Logger logger = org.slf4j.LoggerFactory.getLogger(CorrelationIdFilter.class);
        logger.info("Outgoing response: status={}, correlationId={}", 
                    response.getStatus(), 
                    correlationId);
    }
    
    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
    }
    
    @Override
    public void destroy() {
    }
}
