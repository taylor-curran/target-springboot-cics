# Multi-stage Dockerfile for Spring Boot Banking Application
# Stage 1: Build the application using Maven
FROM maven:3.9-eclipse-temurin-17 AS build
WORKDIR /build

# Copy pom.xml first for dependency caching
COPY pom.xml .
RUN mvn dependency:go-offline -B

# Copy source code
COPY src ./src

# Build the application (skip tests for faster builds)
RUN mvn clean package -DskipTests

# Stage 2: Create the runtime image
FROM eclipse-temurin:17-jre
WORKDIR /app

# Create directory for SQLite database with proper permissions
RUN mkdir -p /app/data && chmod 777 /app/data

# Copy the JAR from build stage
COPY --from=build /build/target/banking-app-migration-0.0.1-SNAPSHOT.jar /app/banking-app.jar

# Set environment variable to use the data directory for database
ENV SPRING_DATASOURCE_URL=jdbc:sqlite:/app/data/banking.db

# Expose port 8085
EXPOSE 8085

# Health check using Spring Boot Actuator
HEALTHCHECK --interval=30s --timeout=3s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:8085/actuator/health || exit 1

# Run the application
ENTRYPOINT ["java", "-jar", "/app/banking-app.jar"]
