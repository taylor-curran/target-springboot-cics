
INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number)
VALUES ('CONTROL', 10, 10, 15, 15);

INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score, credit_score_review_date)
VALUES 
('CUST', '987654', 1, 'Mr Michael A Jones', '42 Acacia Avenue, Norwich', '1985-03-15', 720, '2024-11-15'),
('CUST', '987654', 2, 'Mrs Charlotte B Smith', '17 Birch Boulevard, Cambridge', '1978-07-22', 805, '2024-11-20'),
('CUST', '987654', 3, 'Dr James C Taylor', '8 Cypress Close, Oxford', '1990-01-10', 650, '2024-11-25'),
('CUST', '987654', 4, 'Miss Amy D Evans', '23 Douglas Drive, Manchester', '1995-11-30', 780, '2024-12-01'),
('CUST', '987654', 5, 'Mr Will E Roberts', '56 Elm Escalade, Birmingham', '1982-05-18', 695, '2024-12-05'),
('CUST', '987654', 6, 'Ms Rachel F Wright', '91 Fir Frontage, Bristol', '1988-09-25', 840, '2024-12-10'),
('CUST', '987654', 7, 'Professor Peter G Walker', '34 Gorse Lane, Edinburgh', '1970-12-08', 775, '2024-12-15'),
('CUST', '987654', 8, 'Mrs Samantha H Green', '15 Holly Mews, Glasgow', '1992-04-14', 710, '2024-12-20'),
('CUST', '987654', 9, 'Sir Donald J Price', '67 Ironwood Rise, Leeds', '1965-08-03', 890, '2024-12-25'),
('CUST', '987654', 10, 'Dr Lucy K Brown', '29 Joshua Court, York', '1987-02-27', 745, '2025-01-01');

INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, interest_rate, opened_date, overdraft_limit, last_statement_date, next_statement_date, available_balance, actual_balance)
VALUES
('ACCT', 1, '987654', '00000001', 'CURRENT', 0.00, '2023-01-15', 100, '2024-09-15', '2024-10-15', 1250.50, 1250.50),
('ACCT', 1, '987654', '00000002', 'SAVING', 1.75, '2023-06-20', 0, '2024-09-20', '2024-10-20', 5000.00, 5000.00),
('ACCT', 2, '987654', '00000003', 'ISA', 2.10, '2022-04-10', 0, '2024-09-10', '2024-10-10', 12500.75, 12500.75),
('ACCT', 2, '987654', '00000004', 'CURRENT', 0.00, '2022-04-10', 100, '2024-09-10', '2024-10-10', 850.25, 850.25),
('ACCT', 3, '987654', '00000005', 'MORTGAGE', 5.25, '2020-11-05', 0, '2024-09-05', '2024-10-05', -250000.00, -250000.00),
('ACCT', 4, '987654', '00000006', 'CURRENT', 0.00, '2024-01-12', 100, '2024-09-12', '2024-10-12', 2100.00, 2100.00),
('ACCT', 4, '987654', '00000007', 'SAVING', 1.75, '2024-01-12', 0, '2024-09-12', '2024-10-12', 15000.50, 15000.50),
('ACCT', 5, '987654', '00000008', 'LOAN', 17.90, '2023-08-22', 0, '2024-09-22', '2024-10-22', -15000.00, -15000.00),
('ACCT', 6, '987654', '00000009', 'ISA', 2.10, '2021-03-18', 0, '2024-09-18', '2024-10-18', 18750.25, 18750.25),
('ACCT', 6, '987654', '00000010', 'CURRENT', 0.00, '2021-03-18', 100, '2024-09-18', '2024-10-18', 3500.00, 3500.00),
('ACCT', 7, '987654', '00000011', 'SAVING', 1.75, '2019-12-01', 0, '2024-09-01', '2024-10-01', 45000.00, 45000.00),
('ACCT', 8, '987654', '00000012', 'CURRENT', 0.00, '2023-05-14', 100, '2024-09-14', '2024-10-14', 890.75, 890.75),
('ACCT', 9, '987654', '00000013', 'ISA', 2.10, '2018-07-30', 0, '2024-09-30', '2024-10-30', 19999.99, 19999.99),
('ACCT', 9, '987654', '00000014', 'MORTGAGE', 5.25, '2018-07-30', 0, '2024-09-30', '2024-10-30', -380000.00, -380000.00),
('ACCT', 10, '987654', '00000015', 'CURRENT', 0.00, '2024-02-28', 100, '2024-09-28', '2024-10-28', 1750.00, 1750.00);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES
('PRTR', 0, '987654', '00000001', '2024-10-01', '09:15:30', 100001, 'CRE', 'Salary Deposit', NULL, NULL, 2500.00),
('PRTR', 0, '987654', '00000001', '2024-10-05', '14:22:15', 100002, 'DEB', 'ATM Withdrawal', NULL, NULL, 100.00),
('PRTR', 0, '987654', '00000001', '2024-10-10', '11:45:00', 100003, 'DEB', 'Card Purchase', NULL, NULL, 45.50),
('PRTR', 0, '987654', '00000002', '2024-10-01', '10:00:00', 100004, 'CRE', 'Interest Credit', NULL, NULL, 7.25),
('PRTR', 0, '987654', '00000003', '2024-10-02', '12:30:45', 100005, 'CRE', 'Transfer In', NULL, NULL, 1000.00),
('PRTR', 0, '987654', '00000004', '2024-10-03', '15:20:10', 100006, 'DEB', 'Direct Debit', NULL, NULL, 75.50),
('PRTR', 0, '987654', '00000004', '2024-10-08', '08:45:30', 100007, 'TFR', 'Transfer Out', '987654', '00000002', 200.00),
('PRTR', 0, '987654', '00000006', '2024-10-01', '09:00:00', 100008, 'CRE', 'Salary Deposit', NULL, NULL, 3500.00),
('PRTR', 0, '987654', '00000006', '2024-10-12', '16:30:00', 100009, 'DEB', 'Standing Order', NULL, NULL, 500.00),
('PRTR', 0, '987654', '00000007', '2024-10-15', '10:15:20', 100010, 'CRE', 'Cash Deposit', NULL, NULL, 250.00),
('PRTR', 0, '987654', '00000009', '2024-10-01', '11:00:00', 100011, 'CRE', 'Interest Credit', NULL, NULL, 30.75),
('PRTR', 0, '987654', '00000010', '2024-10-05', '13:45:15', 100012, 'DEB', 'ATM Withdrawal', NULL, NULL, 50.00),
('PRTR', 0, '987654', '00000010', '2024-10-18', '14:20:00', 100013, 'FEE', 'Monthly Account Fee', NULL, NULL, 5.00),
('PRTR', 0, '987654', '00000012', '2024-10-20', '09:30:45', 100014, 'CRE', 'Refund', NULL, NULL, 25.50),
('PRTR', 0, '987654', '00000012', '2024-10-22', '11:15:30', 100015, 'DEB', 'Card Purchase', NULL, NULL, 150.00),
('PRTR', 0, '987654', '00000013', '2024-10-01', '10:30:00', 100016, 'CRE', 'Interest Credit', NULL, NULL, 35.00),
('PRTR', 0, '987654', '00000015', '2024-10-25', '15:00:00', 100017, 'CRE', 'Transfer In', NULL, NULL, 500.00),
('PRTR', 0, '987654', '00000015', '2024-10-26', '12:00:00', 100018, 'DEB', 'Direct Debit', NULL, NULL, 120.00),
('PRTR', 0, '987654', '00000001', '2024-10-27', '10:00:00', 100019, 'TFR', 'Transfer Out', '987654', '00000002', 300.00),
('PRTR', 0, '987654', '00000006', '2024-10-27', '11:30:00', 100020, 'FEE', 'Overdraft Fee', NULL, NULL, 10.00);

INSERT INTO application_error (timestamp, application_id, transaction_id, error_code, program_name, error_message, stack_trace, response_code, response2_code, sql_code, freeform_text)
VALUES
('2024-10-15T10:30:15', 'CBSA001', 'TXN12345', 'ABND001', 'CRECUST', 'Customer creation failed - duplicate key', 'java.sql.SQLException: UNIQUE constraint failed', 'DUPREC', NULL, '-803', 'Attempted to create customer with existing customer_number'),
('2024-10-16T14:22:30', 'CBSA001', 'TXN12346', 'ABND002', 'UPDACC', 'Account update failed - insufficient funds', NULL, 'INVBAL', NULL, NULL, 'Withdrawal of 500.00 exceeds available balance'),
('2024-10-17T09:45:00', 'CBSA001', 'TXN12347', 'ABND003', 'XFRFUN', 'Transfer failed - invalid target account', NULL, 'NOTFND', NULL, NULL, 'Target account 99999999 does not exist'),
('2024-10-18T16:15:45', 'CBSA002', 'TXN12348', 'ABND004', 'DBCRFUN', 'Transaction rejected - account locked', NULL, 'LOCKED', NULL, NULL, 'Account 00000005 is locked for fraud investigation'),
('2024-10-19T11:00:00', 'CBSA002', 'TXN12349', 'ABND005', 'INQCUST', 'Customer inquiry timeout', 'java.net.SocketTimeoutException: Read timed out', 'TIMEOUT', NULL, NULL, 'Database connection timeout after 30 seconds');
