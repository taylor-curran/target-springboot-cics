package com.cbsa.migration.repository.jdbc;

import com.cbsa.migration.repository.ControlRepository;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

/**
 * JDBC implementation of ControlRepository using Spring's JdbcTemplate
 */
@Repository
public class JdbcControlRepository implements ControlRepository {

    private final JdbcTemplate jdbcTemplate;

    public JdbcControlRepository(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public Integer getControlValueNum(String controlName) {
        try {
            return jdbcTemplate.queryForObject(
                "SELECT control_value_num FROM control WHERE control_name = ?",
                Integer.class,
                controlName
            );
        } catch (EmptyResultDataAccessException e) {
            return null;
        }
    }

    @Override
    public void updateControlValueNum(String controlName, Integer value) {
        int rowsUpdated = jdbcTemplate.update(
            "UPDATE control SET control_value_num = ? WHERE control_name = ?",
            value,
            controlName
        );
        
        if (rowsUpdated == 0) {
            jdbcTemplate.update(
                "INSERT INTO control (control_name, control_value_num, control_value_str) VALUES (?, ?, ?)",
                controlName,
                value,
                ""
            );
        }
    }

    @Override
    public String getControlValueStr(String controlName) {
        try {
            return jdbcTemplate.queryForObject(
                "SELECT control_value_str FROM control WHERE control_name = ?",
                String.class,
                controlName
            );
        } catch (EmptyResultDataAccessException e) {
            return null;
        }
    }

    @Override
    public void updateControlValueStr(String controlName, String value) {
        int rowsUpdated = jdbcTemplate.update(
            "UPDATE control SET control_value_str = ? WHERE control_name = ?",
            value,
            controlName
        );
        
        if (rowsUpdated == 0) {
            jdbcTemplate.update(
                "INSERT INTO control (control_name, control_value_num, control_value_str) VALUES (?, ?, ?)",
                controlName,
                0,
                value
            );
        }
    }
}
