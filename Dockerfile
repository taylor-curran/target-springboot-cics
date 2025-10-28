# Multi-stage build for Spring Boot application

# Stage 1: Build stage using Maven
FROM maven:3.9-eclipse-temurin-17 AS build

WORKDIR /app

COPY pom.xml .
RUN mvn dependency:go-offline -B

COPY src ./src
RUN mvn clean package -DskipTests

# Stage 2: Runtime stage using JRE
FROM eclipse-temurin:17-jre

WORKDIR /app

COPY --from=build /app/target/banking-app-migration-0.0.1-SNAPSHOT.jar app.jar

COPY banking.db .

EXPOSE 8085

ENTRYPOINT ["java", "-jar", "app.jar"]
