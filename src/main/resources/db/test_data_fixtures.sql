
INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number)
VALUES ('CONTROL', 5, 5, 12, 12);

INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score, credit_score_review_date)
VALUES 
('CUST', '987654', 1, 'Mr John A Smith', '10 Oak Street, London', '1980-05-15', 750, '2024-12-01'),
('CUST', '987654', 2, 'Mrs Jane B Johnson', '25 Maple Avenue, Manchester', '1975-08-22', 680, '2024-11-15'),
('CUST', '987654', 3, 'Dr Robert C Williams', '42 Pine Road, Birmingham', '1985-03-10', 820, '2024-12-10'),
('CUST', '987654', 4, 'Ms Sarah D Brown', '17 Elm Close, Liverpool', '1990-11-28', 590, '2024-10-30'),
('CUST', '987654', 5, 'Mr David E Davis', '8 Cedar Lane, Leeds', '1978-07-04', 710, '2024-11-20');

INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, interest_rate, opened_date, overdraft_limit, last_statement_date, next_statement_date, available_balance, actual_balance)
VALUES
('ACCT', 1, '987654', '00000001', 'CURRENT', 0.01, '2020-01-15', 1000, '2024-09-30', '2024-10-31', 2500.00, 2500.00),
('ACCT', 1, '987654', '00000002', 'SAVINGS', 0.02, '2020-06-01', 0, '2024-09-30', '2024-10-31', 15000.00, 15000.00),
('ACCT', 1, '987654', '00000003', 'ISA', 0.03, '2021-04-06', 0, '2024-09-30', '2024-10-31', 8000.00, 8000.00),

('ACCT', 2, '987654', '00000004', 'CURRENT', 0.01, '2019-03-20', 500, '2024-09-30', '2024-10-31', 1200.50, 1200.50),
('ACCT', 2, '987654', '00000005', 'SAVINGS', 0.025, '2019-08-15', 0, '2024-09-30', '2024-10-31', 25000.00, 25000.00),

('ACCT', 3, '987654', '00000006', 'CURRENT', 0.01, '2018-11-10', 2000, '2024-09-30', '2024-10-31', 5500.75, 5500.75),
('ACCT', 3, '987654', '00000007', 'MORTGAGE', 0.035, '2019-01-15', 0, '2024-09-30', '2024-10-31', -250000.00, -250000.00),

('ACCT', 4, '987654', '00000008', 'CURRENT', 0.01, '2022-05-20', 750, '2024-09-30', '2024-10-31', 850.25, 850.25),
('ACCT', 4, '987654', '00000009', 'LOAN', 0.055, '2023-02-10', 0, '2024-09-30', '2024-10-31', -15000.00, -15000.00),

('ACCT', 5, '987654', '00000010', 'CURRENT', 0.01, '2017-09-05', 1500, '2024-09-30', '2024-10-31', 3200.00, 3200.00),
('ACCT', 5, '987654', '00000011', 'SAVINGS', 0.02, '2018-03-12', 0, '2024-09-30', '2024-10-31', 42000.00, 42000.00),
('ACCT', 5, '987654', '00000012', 'ISA', 0.03, '2020-04-06', 0, '2024-09-30', '2024-10-31', 12000.00, 12000.00);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES
('DEBT', 0, '987654', '00000001', '2024-10-01', '10:30:00', 1, 'DEBIT', 'Grocery Store Purchase', NULL, NULL, -75.50),
('CRED', 0, '987654', '00000001', '2024-10-05', '14:15:00', 2, 'CREDIT', 'Salary Deposit', NULL, NULL, 3000.00),
('DEBT', 0, '987654', '00000001', '2024-10-08', '16:20:00', 3, 'DEBIT', 'Utility Bill Payment', NULL, NULL, -125.00),
('DEBT', 0, '987654', '00000001', '2024-10-12', '11:45:00', 4, 'DEBIT', 'Restaurant Meal', NULL, NULL, -45.75),

('CRED', 0, '987654', '00000002', '2024-10-01', '09:00:00', 5, 'CREDIT', 'Interest Payment', NULL, NULL, 25.00),
('XFER', 0, '987654', '00000002', '2024-10-15', '12:30:00', 6, 'TRANSFER', 'Transfer to Current Account', '987654', '00000001', -500.00),

('DEBT', 0, '987654', '00000004', '2024-10-02', '08:15:00', 7, 'DEBIT', 'ATM Withdrawal', NULL, NULL, -100.00),
('CRED', 0, '987654', '00000004', '2024-10-10', '15:30:00', 8, 'CREDIT', 'Freelance Payment', NULL, NULL, 750.00),
('DEBT', 0, '987654', '00000004', '2024-10-18', '10:00:00', 9, 'DEBIT', 'Online Shopping', NULL, NULL, -89.99),

('CRED', 0, '987654', '00000006', '2024-10-05', '09:00:00', 10, 'CREDIT', 'Monthly Salary', NULL, NULL, 4500.00),
('DEBT', 0, '987654', '00000006', '2024-10-07', '14:20:00', 11, 'DEBIT', 'Mortgage Payment', NULL, NULL, -1500.00),
('DEBT', 0, '987654', '00000006', '2024-10-15', '11:30:00', 12, 'DEBIT', 'Insurance Premium', NULL, NULL, -200.00),

('CRED', 0, '987654', '00000008', '2024-10-03', '10:15:00', 13, 'CREDIT', 'Part-time Job Payment', NULL, NULL, 450.00),
('DEBT', 0, '987654', '00000008', '2024-10-10', '16:45:00', 14, 'DEBIT', 'Student Supplies', NULL, NULL, -35.50),

('CRED', 0, '987654', '00000010', '2024-10-01', '09:30:00', 15, 'CREDIT', 'Pension Payment', NULL, NULL, 2200.00),
('DEBT', 0, '987654', '00000010', '2024-10-05', '13:00:00', 16, 'DEBIT', 'Council Tax', NULL, NULL, -150.00),

('CRED', 0, '987654', '00000011', '2024-10-01', '09:00:00', 17, 'CREDIT', 'Interest Payment', NULL, NULL, 70.00),

('XFER', 0, '987654', '00000001', '2024-10-20', '15:00:00', 18, 'TRANSFER', 'Transfer to Savings', '987654', '00000002', -200.00),

('XFER', 0, '987654', '00000006', '2024-10-22', '11:00:00', 19, 'TRANSFER', 'Gift Transfer', '987654', '00000010', -500.00),
('XFER', 0, '987654', '00000010', '2024-10-22', '11:00:00', 20, 'TRANSFER', 'Gift Received', '987654', '00000006', 500.00);
