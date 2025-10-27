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
    private static final Logger log = LoggerFactory.getLogger(CorrelationIdFilter.class);

    public static final String CORRELATION_ID_HEADER = "X-Correlation-Id";
    public static final String MDC_KEY = "correlationId";

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {
        String correlationId = request.getHeader(CORRELATION_ID_HEADER);
        if (correlationId == null || correlationId.isBlank()) {
            correlationId = UUID.randomUUID().toString();
        }

        long start = System.currentTimeMillis();
        MDC.put(MDC_KEY, correlationId);
        response.setHeader(CORRELATION_ID_HEADER, correlationId);

        try {
            log.info("Incoming request method={} path={} remote={} ua={}",
                    request.getMethod(), request.getRequestURI(), request.getRemoteAddr(),
                    request.getHeader("User-Agent"));
            filterChain.doFilter(request, response);
        } finally {
            long duration = System.currentTimeMillis() - start;
            log.info("Outgoing response method={} path={} status={} durationMs={}",
                    request.getMethod(), request.getRequestURI(), response.getStatus(), duration);
            MDC.remove(MDC_KEY);
        }
    }
}
