
INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number) 
VALUES ('CONTROL', 5, 5, 8, 8);

INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score, credit_score_review_date) VALUES
('CUST', '987654', 1, 'Mr Michael A Jones', '42 Acacia Avenue, Norwich', '1985-03-15', 750, '2025-11-15'),
('CUST', '987654', 2, 'Mrs Belinda C Smith', '17 Birch Boulevard, Cambridge', '1978-07-22', 680, '2025-11-10'),
('CUST', '987654', 3, 'Dr James F Taylor', '8 Cypress Close, Oxford', '1990-11-08', 820, '2025-11-20'),
('CUST', '987654', 4, 'Ms Lucy M Evans', '23 Douglas Drive, Bristol', '1995-01-30', 590, '2025-11-12'),
('CUST', '987654', 5, 'Professor Stephen R Brown', '56 Elm Court, Manchester', '1972-09-14', 910, '2025-11-25');

INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, interest_rate, opened_date, overdraft_limit, last_statement_date, next_statement_date, available_balance, actual_balance) VALUES
('ACCT', 1, '987654', '00000001', 'CURRENT', 0.00, '2024-01-15', 100, '2025-09-15', '2025-10-15', 1250.50, 1250.50),
('ACCT', 1, '987654', '00000002', 'ISA', 2.10, '2024-03-20', 0, '2025-09-20', '2025-10-20', 5000.00, 5000.00),
('ACCT', 2, '987654', '00000003', 'SAVING', 1.75, '2023-11-10', 0, '2025-09-10', '2025-10-10', 12500.75, 12500.75),
('ACCT', 3, '987654', '00000004', 'CURRENT', 0.00, '2024-05-05', 100, '2025-09-05', '2025-10-05', 3200.00, 3200.00),
('ACCT', 3, '987654', '00000005', 'MORTGAGE', 5.25, '2023-06-01', 0, '2025-09-01', '2025-10-01', -250000.00, -250000.00),
('ACCT', 4, '987654', '00000006', 'CURRENT', 0.00, '2024-08-12', 100, '2025-09-12', '2025-10-12', 450.25, 450.25),
('ACCT', 5, '987654', '00000007', 'SAVING', 1.75, '2024-02-28', 0, '2025-09-28', '2025-10-28', 8750.00, 8750.00),
('ACCT', 5, '987654', '00000008', 'ISA', 2.10, '2024-04-15', 0, '2025-09-15', '2025-10-15', 15000.00, 15000.00);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount) VALUES
('TRAN', 0, '987654', '00000001', '2025-10-01', '09:15:00', 1001, 'CRE', 'Salary Deposit', NULL, NULL, 2500.00),
('TRAN', 0, '987654', '00000001', '2025-10-05', '14:30:00', 1002, 'DEB', 'ATM Withdrawal', NULL, NULL, -200.00),
('TRAN', 0, '987654', '00000001', '2025-10-10', '10:45:00', 1003, 'DEB', 'Card Purchase', NULL, NULL, -150.50),
('TRAN', 0, '987654', '00000001', '2025-10-15', '16:20:00', 1004, 'DEB', 'Direct Debit', NULL, NULL, -899.00),
('TRAN', 0, '987654', '00000002', '2025-10-01', '10:00:00', 2001, 'CRE', 'Cash Deposit', NULL, NULL, 1000.00),
('TRAN', 0, '987654', '00000002', '2025-10-15', '11:20:00', 2002, 'INT', 'Interest Credit', NULL, NULL, 8.75),
('TRAN', 0, '987654', '00000003', '2025-10-02', '13:00:00', 3001, 'CRE', 'Transfer In', NULL, NULL, 500.00),
('TRAN', 0, '987654', '00000003', '2025-10-12', '16:45:00', 3002, 'DEB', 'Standing Order', NULL, NULL, -250.00),
('TRAN', 0, '987654', '00000003', '2025-10-20', '09:30:00', 3003, 'INT', 'Interest Credit', NULL, NULL, 18.25),
('TRAN', 0, '987654', '00000004', '2025-10-03', '08:00:00', 4001, 'CRE', 'Salary Deposit', NULL, NULL, 3500.00),
('TRAN', 0, '987654', '00000004', '2025-10-08', '12:15:00', 4002, 'DEB', 'ATM Withdrawal', NULL, NULL, -300.00),
('TRAN', 0, '987654', '00000005', '2025-10-01', '00:00:01', 5001, 'DEB', 'Mortgage Payment', NULL, NULL, -1250.00),
('TRAN', 0, '987654', '00000005', '2025-10-01', '00:00:02', 5002, 'INT', 'Interest Charge', NULL, NULL, -1093.75),
('TRAN', 0, '987654', '00000006', '2025-10-04', '14:20:00', 6001, 'CRE', 'Cash Deposit', NULL, NULL, 500.00),
('TRAN', 0, '987654', '00000006', '2025-10-09', '11:30:00', 6002, 'DEB', 'Card Purchase', NULL, NULL, -49.75),
('TRAN', 0, '987654', '00000007', '2025-10-05', '15:00:00', 7001, 'CRE', 'Transfer In', NULL, NULL, 2000.00),
('TRAN', 0, '987654', '00000007', '2025-10-18', '10:00:00', 7002, 'INT', 'Interest Credit', NULL, NULL, 12.76),
('TRAN', 0, '987654', '00000008', '2025-10-06', '09:45:00', 8001, 'CRE', 'Cash Deposit', NULL, NULL, 5000.00),
('TRAN', 0, '987654', '00000008', '2025-10-20', '10:30:00', 8002, 'INT', 'Interest Credit', NULL, NULL, 26.30);
