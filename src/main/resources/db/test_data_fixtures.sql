
INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number)
VALUES ('CONTROL', 5, 5, 10, 10);

INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score, credit_score_review_date)
VALUES 
  ('CUST', '987654', 1, 'Mr John A Smith', '12 Oak Avenue, London', '1980-05-15', 750, '2024-11-15'),
  ('CUST', '987654', 2, 'Mrs Sarah B Johnson', '45 Elm Street, Manchester', '1975-08-22', 680, '2024-11-20'),
  ('CUST', '987654', 3, 'Dr Robert C Williams', '78 Pine Road, Birmingham', '1985-12-03', 820, '2024-11-25'),
  ('CUST', '987654', 4, 'Ms Emily D Brown', '23 Maple Close, Leeds', '1990-03-18', 590, '2024-11-10'),
  ('CUST', '987654', 5, 'Mr David E Jones', '56 Birch Lane, Liverpool', '1982-07-09', 710, '2024-11-30');

INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, interest_rate, opened_date, overdraft_limit, last_statement_date, next_statement_date, available_balance, actual_balance)
VALUES
  ('ACCT', 1, '987654', '00000001', 'CURRENT', 0.5, '2023-01-15', 1000, '2024-09-15', '2024-10-15', 2500.00, 2500.00),
  ('ACCT', 1, '987654', '00000002', 'SAVING', 2.5, '2023-02-20', 0, '2024-09-20', '2024-10-20', 15000.00, 15000.00),
  ('ACCT', 2, '987654', '00000003', 'CURRENT', 0.5, '2023-03-10', 500, '2024-09-10', '2024-10-10', -200.00, -200.00),
  ('ACCT', 2, '987654', '00000004', 'ISA', 3.0, '2023-04-05', 0, '2024-09-05', '2024-10-05', 8500.00, 8500.00),
  ('ACCT', 3, '987654', '00000005', 'CURRENT', 0.5, '2023-05-12', 2000, '2024-09-12', '2024-10-12', 5200.00, 5200.00),
  ('ACCT', 3, '987654', '00000006', 'MORTGAGE', 4.5, '2023-06-01', 0, '2024-09-01', '2024-10-01', -250000.00, -250000.00),
  ('ACCT', 4, '987654', '00000007', 'CURRENT', 0.5, '2023-07-18', 750, '2024-09-18', '2024-10-18', 1200.00, 1200.00),
  ('ACCT', 4, '987654', '00000008', 'LOAN', 6.5, '2023-08-22', 0, '2024-09-22', '2024-10-22', -15000.00, -15000.00),
  ('ACCT', 5, '987654', '00000009', 'CURRENT', 0.5, '2023-09-30', 1500, '2024-09-30', '2024-10-30', 3800.00, 3800.00),
  ('ACCT', 5, '987654', '00000010', 'SAVING', 2.0, '2023-10-15', 0, '2024-09-15', '2024-10-15', 22000.00, 22000.00);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES
  ('PRTR', 0, '987654', '00000001', '2024-10-01', '09:15:30', 1001000, 'CRE', 'Salary Payment', NULL, NULL, 3000.00),
  ('PRTR', 0, '987654', '00000001', '2024-10-05', '14:22:15', 1001001, 'DEB', 'ATM Withdrawal', NULL, NULL, 100.00),
  ('PRTR', 0, '987654', '00000001', '2024-10-10', '11:45:00', 1001002, 'TFR', 'Transfer to Smith', '123456', '99999999', 500.00),
  ('PRTR', 0, '987654', '00000001', '2024-10-15', '16:30:45', 1001003, 'FEE', 'Account Maintenance Fee', NULL, NULL, 10.00),
  ('PRTR', 0, '987654', '00000002', '2024-10-02', '10:20:00', 1001004, 'CRE', 'Deposit', NULL, NULL, 1000.00),
  ('PRTR', 0, '987654', '00000002', '2024-10-12', '15:10:30', 1001005, 'INT', 'Monthly Interest Credit', NULL, NULL, 31.25),
  ('PRTR', 0, '987654', '00000003', '2024-10-03', '12:05:15', 1002000, 'DEB', 'Online Purchase', NULL, NULL, 50.00),
  ('PRTR', 0, '987654', '00000003', '2024-10-08', '09:30:00', 1002001, 'CRE', 'Cash Deposit', NULL, NULL, 200.00),
  ('PRTR', 0, '987654', '00000003', '2024-10-18', '17:45:20', 1002002, 'FEE', 'Overdraft Fee', NULL, NULL, 25.00),
  ('PRTR', 0, '987654', '00000004', '2024-10-04', '11:15:45', 1002003, 'CRE', 'Transfer In', NULL, NULL, 500.00),
  ('PRTR', 0, '987654', '00000005', '2024-10-06', '13:25:10', 1003000, 'DEB', 'Direct Debit - Utilities', NULL, NULL, 150.00),
  ('PRTR', 0, '987654', '00000005', '2024-10-11', '10:40:00', 1003001, 'CRE', 'Freelance Payment', NULL, NULL, 2500.00),
  ('PRTR', 0, '987654', '00000005', '2024-10-20', '14:55:30', 1003002, 'TFR', 'Payment to Williams', '654321', '88888888', 300.00),
  ('PRTR', 0, '987654', '00000006', '2024-10-01', '09:00:00', 1003003, 'DEB', 'Monthly Mortgage Payment', NULL, NULL, 1200.00),
  ('PRTR', 0, '987654', '00000007', '2024-10-07', '15:20:15', 1004000, 'CRE', 'Refund', NULL, NULL, 75.00),
  ('PRTR', 0, '987654', '00000007', '2024-10-14', '11:30:45', 1004001, 'DEB', 'Card Payment', NULL, NULL, 45.00),
  ('PRTR', 0, '987654', '00000008', '2024-10-01', '08:00:00', 1004002, 'DEB', 'Loan Repayment', NULL, NULL, 500.00),
  ('PRTR', 0, '987654', '00000009', '2024-10-09', '16:15:00', 1005000, 'CRE', 'Birthday Gift', NULL, NULL, 100.00),
  ('PRTR', 0, '987654', '00000009', '2024-10-16', '12:40:30', 1005001, 'DEB', 'Restaurant', NULL, NULL, 85.00),
  ('PRTR', 0, '987654', '00000010', '2024-10-13', '10:05:00', 1005002, 'INT', 'Monthly Interest Credit', NULL, NULL, 36.67);

INSERT INTO application_error (timestamp, application_id, transaction_id, error_code, program_name, error_message, stack_trace, response_code, response2_code, sql_code, freeform_text)
VALUES 
  ('2024-10-27T10:00:00', 'CBSA-MIG', 'TEST001', 'CONN_ERR', 'TestProgram', 'Sample test error for validation', 'Stack trace would go here', 'RESP001', 'RESP2001', 'SQL0001', 'This is a sample error record for testing purposes');
