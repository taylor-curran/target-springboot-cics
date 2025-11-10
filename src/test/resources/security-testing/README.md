# Security Testing Resources

## EICAR Test File

### Overview
This directory contains the EICAR (European Institute for Computer Antivirus Research) test file, a standard tool used by security professionals for testing antivirus and malware detection systems.

### What is EICAR?
The EICAR test file is a legitimate, safe file that is recognized by antivirus software as if it were malware. It contains a specific 68-byte string that antivirus programs are designed to detect. **It is NOT actual malware** and poses no threat to systems.

### File Details
- **Filename**: `eicar.txt`
- **Size**: 68 bytes
- **Content**: Standard EICAR test string
- **Purpose**: Penetration testing for malicious file upload detection

### Use Cases for Penetration Testing

#### 1. File Upload Security Testing
Test whether your application properly detects and blocks malicious file uploads:

```java
@Test
public void testMaliciousFileUploadDetection() {
    File eicarFile = new File("src/test/resources/security-testing/eicar.txt");
    // Attempt to upload the EICAR file
    // Verify that the application blocks or quarantines it
}
```

#### 2. Antivirus Integration Testing
Verify that your antivirus scanning integration is working correctly:
- Upload the EICAR file through your application's file upload endpoint
- Confirm that the antivirus scanner detects it
- Verify proper error handling and user notification

#### 3. Security Policy Validation
Test that security policies are enforced:
- Verify file scanning occurs before storage
- Confirm infected files are not persisted to disk
- Test that appropriate alerts are generated

### Expected Behavior
When properly configured, your application should:
1. Scan uploaded files before accepting them
2. Detect the EICAR file as a threat
3. Reject the upload with an appropriate error message
4. Log the security event
5. NOT store the file in the application's file system

### Safety Information
- **Safe to use**: The EICAR file is completely safe and contains no executable code
- **Antivirus detection**: Most antivirus software will flag this file - this is expected and desired behavior
- **Not actual malware**: This is a test file only, designed specifically for security testing

### Testing Endpoints
Use this file to test file upload endpoints in the application:

```bash
# Example: Test file upload with EICAR file
curl -X POST http://localhost:8085/api/upload \
  -F "file=@src/test/resources/security-testing/eicar.txt"

# Expected response: 400 Bad Request or 403 Forbidden
# Expected message: "Malicious file detected" or similar
```

### References
- [EICAR Official Website](https://www.eicar.org/)
- [EICAR Test File Information](https://en.wikipedia.org/wiki/EICAR_test_file)

### Important Notes
- Do NOT use this file in production environments
- Only use for authorized penetration testing
- Ensure proper approval before conducting security tests
- Document all security testing activities

---
*This file is part of the security testing resources for the CICS Banking Sample Application migration project.*
