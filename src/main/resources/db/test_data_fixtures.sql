

INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number)
VALUES ('CONTROL', 5, 5, 10, 10);

INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score, credit_score_review_date)
VALUES 
    ('CUST', '987654', 1, 'Mr John A Smith', '42 Oak Street, London', '1985-03-15', 750, '2025-11-15'),
    ('CUST', '987654', 2, 'Mrs Sarah B Johnson', '17 Elm Road, Manchester', '1978-07-22', 820, '2025-11-20'),
    ('CUST', '987654', 3, 'Ms Emily C Williams', '89 Pine Avenue, Birmingham', '1992-11-08', 680, '2025-11-10'),
    ('CUST', '987654', 4, 'Dr Michael D Brown', '23 Maple Drive, Leeds', '1970-05-30', 890, '2025-11-25'),
    ('CUST', '987654', 5, 'Miss Lisa E Davis', '56 Cedar Lane, Bristol', '1988-09-12', 710, '2025-11-18');

INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, interest_rate, opened_date, overdraft_limit, last_statement_date, next_statement_date, available_balance, actual_balance)
VALUES 
    ('ACCT', 1, '987654', '00000001', 'CURRENT', 0.01, '2024-01-15', 500, '2024-10-15', '2024-11-15', 1250.00, 1250.00),
    ('ACCT', 1, '987654', '00000002', 'SAVING', 2.50, '2024-02-20', 0, '2024-10-20', '2024-11-20', 5000.00, 5000.00),
    ('ACCT', 2, '987654', '00000003', 'CURRENT', 0.01, '2023-06-10', 1000, '2024-10-10', '2024-11-10', 2340.50, 2340.50),
    ('ACCT', 2, '987654', '00000004', 'ISA', 3.00, '2023-04-05', 0, '2024-10-05', '2024-11-05', 15000.00, 15000.00),
    ('ACCT', 3, '987654', '00000005', 'CURRENT', 0.01, '2024-03-12', 750, NULL, '2024-11-12', -125.00, -125.00),
    ('ACCT', 3, '987654', '00000006', 'LOAN', 5.50, '2024-01-08', 0, '2024-10-08', '2024-11-08', -12500.00, -12500.00),
    ('ACCT', 4, '987654', '00000007', 'CURRENT', 0.01, '2022-09-22', 2000, '2024-10-22', '2024-11-22', 8750.25, 8750.25),
    ('ACCT', 4, '987654', '00000008', 'MORTGAGE', 3.75, '2020-05-15', 0, '2024-10-15', '2024-11-15', -250000.00, -250000.00),
    ('ACCT', 5, '987654', '00000009', 'SAVING', 2.50, '2024-07-30', 0, NULL, '2024-11-30', 3500.00, 3500.00),
    ('ACCT', 5, '987654', '00000010', 'CURRENT', 0.01, '2024-08-01', 500, NULL, '2024-12-01', 890.75, 890.75);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES 
    ('TXN', 0, '987654', '00000001', '2024-10-01', '09:15:30', 1001, 'CREDIT', 'Salary Payment', NULL, NULL, 2500.00),
    ('TXN', 0, '987654', '00000001', '2024-10-05', '14:22:10', 1002, 'DEBIT', 'ATM Withdrawal', NULL, NULL, 100.00),
    ('TXN', 0, '987654', '00000001', '2024-10-10', '11:45:00', 1003, 'DEBIT', 'Online Purchase', NULL, NULL, 45.50),
    ('TXN', 0, '987654', '00000001', '2024-10-15', '10:30:15', 1004, 'TRANSFER', 'Transfer to Savings', '987654', '00000002', 500.00),
    ('TXN', 0, '987654', '00000002', '2024-10-15', '10:30:15', 1005, 'CREDIT', 'Transfer from Current', '987654', '00000001', 500.00),
    ('TXN', 0, '987654', '00000003', '2024-10-02', '08:20:45', 1006, 'CREDIT', 'Salary Payment', NULL, NULL, 3200.00),
    ('TXN', 0, '987654', '00000003', '2024-10-08', '16:55:30', 1007, 'DEBIT', 'Rent Payment', NULL, NULL, 1200.00),
    ('TXN', 0, '987654', '00000003', '2024-10-12', '13:10:00', 1008, 'DEBIT', 'Utility Bill', NULL, NULL, 150.00),
    ('TXN', 0, '987654', '00000004', '2024-10-01', '09:00:00', 1009, 'CREDIT', 'ISA Deposit', NULL, NULL, 1000.00),
    ('TXN', 0, '987654', '00000005', '2024-10-03', '15:30:20', 1010, 'DEBIT', 'Grocery Shopping', NULL, NULL, 85.50),
    ('TXN', 0, '987654', '00000005', '2024-10-18', '12:00:00', 1011, 'DEBIT', 'Restaurant Bill', NULL, NULL, 65.00),
    ('TXN', 0, '987654', '00000006', '2024-10-01', '09:30:00', 1012, 'DEBIT', 'Loan Payment', NULL, NULL, 250.00),
    ('TXN', 0, '987654', '00000007', '2024-10-01', '09:05:15', 1013, 'CREDIT', 'Salary Payment', NULL, NULL, 4500.00),
    ('TXN', 0, '987654', '00000007', '2024-10-06', '10:15:30', 1014, 'DEBIT', 'Car Payment', NULL, NULL, 350.00),
    ('TXN', 0, '987654', '00000007', '2024-10-20', '14:25:00', 1015, 'TRANSFER', 'Transfer to ISA', '987654', '00000004', 1000.00),
    ('TXN', 0, '987654', '00000008', '2024-10-01', '09:00:00', 1016, 'DEBIT', 'Mortgage Payment', NULL, NULL, 1250.00),
    ('TXN', 0, '987654', '00000009', '2024-10-15', '11:20:00', 1017, 'CREDIT', 'Savings Deposit', NULL, NULL, 500.00),
    ('TXN', 0, '987654', '00000010', '2024-10-08', '13:45:00', 1018, 'CREDIT', 'Refund', NULL, NULL, 25.50),
    ('TXN', 0, '987654', '00000010', '2024-10-22', '16:30:00', 1019, 'DEBIT', 'Phone Bill', NULL, NULL, 45.00),
    ('TXN', 0, '987654', '00000001', '2024-10-25', '10:00:00', 1020, 'CREDIT', 'Bonus Payment', NULL, NULL, 500.00);

INSERT INTO application_error (timestamp, application_id, transaction_id, error_code, program_name, error_message, stack_trace, response_code, response2_code, sql_code, freeform_text)
VALUES 
    ('2024-10-20T14:30:00', 'BANKING-APP', 'TXN-12345', 'ERR001', 'INQCUST', 'Customer not found', 'com.cbsa.migration.service.CustomerService.findCustomer(CustomerService.java:45)', '0013', '0001', NULL, 'Attempted to retrieve customer 99999 which does not exist in database'),
    ('2024-10-21T09:15:00', 'BANKING-APP', 'TXN-12346', 'ERR002', 'XFRFUN', 'Insufficient funds', 'com.cbsa.migration.service.TransactionService.transfer(TransactionService.java:78)', NULL, NULL, NULL, 'Transfer of 5000.00 failed - account 00000005 has balance -125.00 with overdraft 750'),
    ('2024-10-22T16:45:00', 'BANKING-APP', 'TXN-12347', 'ERR003', 'CRECUST', 'Credit agency timeout', 'com.cbsa.migration.service.CreditAgencyService.checkCredit(CreditAgencyService.java:92)', NULL, NULL, NULL, 'Credit agency service did not respond within 5 second timeout period');
