package com.cbsa.migration.repository;

/**
 * Repository interface for Control table operations
 */
public interface ControlRepository {

    /**
     * Get a numeric control value by name
     * 
     * @param controlName the control name
     * @return the numeric value, or null if not found
     */
    Integer getControlValueNum(String controlName);
    
    /**
     * Update a numeric control value by name
     * 
     * @param controlName the control name
     * @param value the new numeric value
     */
    void updateControlValueNum(String controlName, Integer value);
    
    /**
     * Get a string control value by name
     * 
     * @param controlName the control name
     * @return the string value, or null if not found
     */
    String getControlValueStr(String controlName);
    
    /**
     * Update a string control value by name
     * 
     * @param controlName the control name
     * @param value the new string value
     */
    void updateControlValueStr(String controlName, String value);
}
