-- Based on the COBOL data structures migrated to a relational model

INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number)
VALUES ('CONTROL', 10, 10, 15, 15);

INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score, credit_score_review_date)
VALUES 
    ('CUST', '987654', 1, 'Mr Michael A Jones', '42 Oak Avenue, Norwich', '1985-03-15', 720, '2025-11-15'),
    ('CUST', '987654', 2, 'Mrs Amy B Smith', '15 Birch Close, Cambridge', '1978-07-22', 680, '2025-11-10'),
    ('CUST', '987654', 3, 'Miss Charlotte C Taylor', '8 Maple Drive, Peterborough', '1992-11-30', 750, '2025-12-01'),
    ('CUST', '987654', 4, 'Dr James D Evans', '23 Pine Street, Oxford', '1970-05-18', 820, '2025-11-20'),
    ('CUST', '987654', 5, 'Ms Lucy E Roberts', '56 Elm Boulevard, Bristol', '1988-09-07', 690, '2025-11-25'),
    ('CUST', '987654', 6, 'Mr David F Wright', '91 Cedar Court, Manchester', '1995-12-14', 710, '2025-11-18'),
    ('CUST', '987654', 7, 'Mrs Patricia G Walker', '34 Willow Lane, Birmingham', '1982-04-25', 760, '2025-12-05'),
    ('CUST', '987654', 8, 'Professor Howard H Green', '77 Sycamore Rise, Edinburgh', '1965-08-09', 840, '2025-11-30'),
    ('CUST', '987654', 9, 'Miss Rachel I Price', '12 Acacia Avenue, Glasgow', '1990-02-28', 700, '2025-11-22'),
    ('CUST', '987654', 10, 'Mr Anthony J Baker', '45 Cypress Crescent, Cardiff', '1987-06-13', 730, '2025-12-10');

INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, interest_rate, opened_date, overdraft_limit, last_statement_date, next_statement_date, available_balance, actual_balance)
VALUES 
    ('ACCT', 1, '987654', '00000001', 'ISA', 2.10, '2024-01-15', 0, '2024-09-15', '2024-10-15', 5000.00, 5000.00),
    ('ACCT', 1, '987654', '00000002', 'CURRENT', 0.00, '2024-01-15', 100, '2024-09-15', '2024-10-15', 1250.50, 1250.50),
    
    ('ACCT', 2, '987654', '00000003', 'SAVING', 1.75, '2023-06-20', 0, '2024-08-20', '2024-09-20', 15000.00, 15000.00),
    ('ACCT', 2, '987654', '00000004', 'CURRENT', 0.00, '2023-06-20', 100, '2024-08-20', '2024-09-20', -45.00, -45.00),
    
    ('ACCT', 3, '987654', '00000005', 'ISA', 2.10, '2024-03-10', 0, '2024-09-10', '2024-10-10', 8500.00, 8500.00),
    
    ('ACCT', 4, '987654', '00000006', 'MORTGAGE', 5.25, '2020-11-05', 0, '2024-08-05', '2024-09-05', -250000.00, -250000.00),
    ('ACCT', 4, '987654', '00000007', 'SAVING', 1.75, '2020-11-05', 0, '2024-08-05', '2024-09-05', 45000.00, 45000.00),
    
    ('ACCT', 5, '987654', '00000008', 'CURRENT', 0.00, '2024-02-28', 100, '2024-09-28', '2024-10-28', 890.25, 890.25),
    ('ACCT', 5, '987654', '00000009', 'LOAN', 17.90, '2023-12-15', 0, '2024-08-15', '2024-09-15', -12000.00, -12000.00),
    
    ('ACCT', 6, '987654', '00000010', 'CURRENT', 0.00, '2024-05-01', 100, '2024-09-01', '2024-10-01', 2100.00, 2100.00),
    
    ('ACCT', 7, '987654', '00000011', 'ISA', 2.10, '2022-07-18', 0, '2024-08-18', '2024-09-18', 12000.00, 12000.00),
    ('ACCT', 7, '987654', '00000012', 'SAVING', 1.75, '2022-07-18', 0, '2024-08-18', '2024-09-18', 25000.00, 25000.00),
    
    ('ACCT', 8, '987654', '00000013', 'MORTGAGE', 5.25, '2015-03-22', 0, '2024-08-22', '2024-09-22', -180000.00, -180000.00),
    ('ACCT', 8, '987654', '00000014', 'CURRENT', 0.00, '2015-03-22', 100, '2024-08-22', '2024-09-22', 3450.75, 3450.75),
    
    ('ACCT', 9, '987654', '00000015', 'SAVING', 1.75, '2024-04-12', 0, NULL, NULL, 6500.00, 6500.00);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES 
    ('CRE', 0, '987654', '00000001', '2024-10-01', '09:15:30', 1001, 'CRE', 'Salary Deposit', NULL, NULL, 2500.00),
    ('INT', 0, '987654', '00000001', '2024-10-15', '00:01:00', 1002, 'INT', 'Interest Credit', NULL, NULL, 8.75),
    
    ('CRE', 0, '987654', '00000002', '2024-10-01', '09:15:35', 2001, 'CRE', 'Cash Deposit', NULL, NULL, 500.00),
    ('DEB', 0, '987654', '00000002', '2024-10-05', '14:22:10', 2002, 'DEB', 'ATM Withdrawal', NULL, NULL, 100.00),
    ('DEB', 0, '987654', '00000002', '2024-10-08', '11:35:45', 2003, 'DEB', 'Card Purchase', NULL, NULL, 45.50),
    ('TFR', 0, '987654', '00000002', '2024-10-10', '10:00:00', 2004, 'TFR', 'Transfer Out', '987654', '00000001', 200.00),
    
    ('CRE', 0, '987654', '00000003', '2024-10-01', '08:00:00', 3001, 'CRE', 'Transfer In', NULL, NULL, 1000.00),
    ('INT', 0, '987654', '00000003', '2024-10-15', '00:01:00', 3002, 'INT', 'Interest Credit', NULL, NULL, 21.88),
    
    ('DEB', 0, '987654', '00000004', '2024-10-03', '15:30:00', 4001, 'DEB', 'Direct Debit', NULL, NULL, 150.00),
    ('FEE', 0, '987654', '00000004', '2024-10-05', '00:01:00', 4002, 'FEE', 'Overdraft Fee', NULL, NULL, 5.00),
    ('CRE', 0, '987654', '00000004', '2024-10-12', '10:15:00', 4003, 'CRE', 'Cash Deposit', NULL, NULL, 100.00),
    
    ('CRE', 0, '987654', '00000005', '2024-10-01', '09:00:00', 5001, 'CRE', 'Salary Deposit', NULL, NULL, 1500.00),
    ('INT', 0, '987654', '00000005', '2024-10-15', '00:01:00', 5002, 'INT', 'Interest Credit', NULL, NULL, 14.88),
    
    ('DEB', 0, '987654', '00000006', '2024-10-01', '00:05:00', 6001, 'DEB', 'Mortgage Payment', NULL, NULL, 1250.00),
    
    ('CRE', 0, '987654', '00000007', '2024-10-01', '09:00:00', 7001, 'CRE', 'Salary Deposit', NULL, NULL, 3000.00),
    ('INT', 0, '987654', '00000007', '2024-10-15', '00:01:00', 7002, 'INT', 'Interest Credit', NULL, NULL, 65.63),
    
    ('CRE', 0, '987654', '00000008', '2024-10-01', '09:30:00', 8001, 'CRE', 'Salary Deposit', NULL, NULL, 1800.00),
    ('DEB', 0, '987654', '00000008', '2024-10-02', '14:15:00', 8002, 'DEB', 'Card Purchase', NULL, NULL, 85.25),
    ('DEB', 0, '987654', '00000008', '2024-10-05', '16:45:00', 8003, 'DEB', 'Standing Order', NULL, NULL, 200.00),
    ('TFR', 0, '987654', '00000008', '2024-10-10', '11:00:00', 8004, 'TFR', 'Transfer Out', '987654', '00000009', 500.00),
    
    ('CRE', 0, '987654', '00000009', '2024-10-01', '00:05:00', 9001, 'CRE', 'Loan Payment', NULL, NULL, 350.00),
    
    ('CRE', 0, '987654', '00000010', '2024-10-01', '08:45:00', 10001, 'CRE', 'Salary Deposit', NULL, NULL, 2200.00),
    ('DEB', 0, '987654', '00000010', '2024-10-03', '12:30:00', 10002, 'DEB', 'ATM Withdrawal', NULL, NULL, 200.00),
    ('DEB', 0, '987654', '00000010', '2024-10-07', '18:20:00', 10003, 'DEB', 'Card Purchase', NULL, NULL, 125.50),
    
    ('CRE', 0, '987654', '00000011', '2024-10-01', '10:00:00', 11001, 'CRE', 'Transfer In', NULL, NULL, 1000.00),
    ('INT', 0, '987654', '00000011', '2024-10-15', '00:01:00', 11002, 'INT', 'Interest Credit', NULL, NULL, 21.00),
    
    ('CRE', 0, '987654', '00000014', '2024-10-01', '09:00:00', 14001, 'CRE', 'Salary Deposit', NULL, NULL, 4500.00),
    ('DEB', 0, '987654', '00000014', '2024-10-05', '10:15:00', 14002, 'DEB', 'Card Purchase', NULL, NULL, 250.00),
    ('FEE', 0, '987654', '00000014', '2024-10-10', '00:01:00', 14003, 'FEE', 'Monthly Account Fee', NULL, NULL, 10.00),
    
    ('CRE', 0, '987654', '00000015', '2024-10-12', '11:30:00', 15001, 'CRE', 'Cash Deposit', NULL, NULL, 5000.00),
    ('CRE', 0, '987654', '00000015', '2024-10-20', '14:00:00', 15002, 'CRE', 'Transfer In', NULL, NULL, 1500.00),
    
    ('DEB', 1, '987654', '00000002', '2024-10-15', '15:30:00', 2005, 'DEB', 'Cancelled Transaction', NULL, NULL, 75.00);
