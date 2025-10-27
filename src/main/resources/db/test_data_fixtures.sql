
INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number)
VALUES ('CONTROL', 5, 5, 10, 10);

INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score, credit_score_review_date)
VALUES 
('CUST', '987654', 1, 'Mr John A Smith', '10 Oak Lane, London', '1980-05-15', 750, '2025-11-15'),
('CUST', '987654', 2, 'Ms Sarah B Johnson', '25 Elm Street, Manchester', '1975-08-22', 820, '2025-11-20'),
('CUST', '987654', 3, 'Dr David C Williams', '42 Pine Road, Birmingham', '1968-03-10', 690, '2025-11-10'),
('CUST', '987654', 4, 'Mrs Emma D Brown', '15 Maple Avenue, Liverpool', '1992-12-05', 780, '2025-11-25'),
('CUST', '987654', 5, 'Mr James E Davis', '88 Cedar Close, Leeds', '1985-07-18', 810, '2025-12-01');

INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, interest_rate, opened_date, overdraft_limit, last_statement_date, next_statement_date, available_balance, actual_balance)
VALUES
('ACCT', 1, '987654', '00000001', 'CURRENT', 0.0, '2023-01-15', 1000, '2025-09-15', '2025-10-15', 2500.00, 2500.00),
('ACCT', 1, '987654', '00000002', 'SAVING', 2.5, '2023-03-20', 0, '2025-09-20', '2025-10-20', 15000.00, 15000.00),
('ACCT', 2, '987654', '00000003', 'CURRENT', 0.0, '2022-06-10', 500, '2025-09-10', '2025-10-10', 3200.50, 3200.50),
('ACCT', 2, '987654', '00000004', 'ISA', 3.0, '2023-04-05', 0, '2025-09-05', '2025-10-05', 8500.00, 8500.00),
('ACCT', 3, '987654', '00000005', 'CURRENT', 0.0, '2021-11-22', 2000, '2025-09-22', '2025-10-22', -150.00, -150.00),
('ACCT', 3, '987654', '00000006', 'LOAN', 5.5, '2024-02-14', 0, '2025-09-14', '2025-10-14', -12000.00, -12000.00),
('ACCT', 4, '987654', '00000007', 'CURRENT', 0.0, '2023-07-30', 1500, '2025-09-30', '2025-10-30', 4100.75, 4100.75),
('ACCT', 4, '987654', '00000008', 'SAVING', 2.5, '2023-08-12', 0, '2025-09-12', '2025-10-12', 25000.00, 25000.00),
('ACCT', 5, '987654', '00000009', 'CURRENT', 0.0, '2022-09-05', 1000, '2025-09-05', '2025-10-05', 1800.00, 1800.00),
('ACCT', 5, '987654', '00000010', 'MORTGAGE', 3.5, '2020-01-15', 0, '2025-09-15', '2025-10-15', -285000.00, -285000.00);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES
('TRAN', 0, '987654', '00000001', '2025-10-20', '09:30:00', 1001, 'DEBIT', 'Grocery shopping', NULL, NULL, -85.50),
('TRAN', 0, '987654', '00000001', '2025-10-21', '14:15:00', 1002, 'DEBIT', 'Gas station', NULL, NULL, -45.00),
('TRAN', 0, '987654', '00000001', '2025-10-22', '10:00:00', 1003, 'CREDIT', 'Salary deposit', NULL, NULL, 2800.00),
('TRAN', 0, '987654', '00000002', '2025-10-20', '11:00:00', 1004, 'CREDIT', 'Interest payment', NULL, NULL, 31.25),
('TRAN', 0, '987654', '00000003', '2025-10-19', '16:45:00', 1005, 'DEBIT', 'Online purchase', NULL, NULL, -120.00),
('TRAN', 0, '987654', '00000003', '2025-10-23', '12:30:00', 1006, 'CREDIT', 'Salary deposit', NULL, NULL, 3500.00),
('TRAN', 0, '987654', '00000003', '2025-10-24', '13:00:00', 1007, 'TRANSFER', 'Transfer to savings', '987654', '00000004', -500.00),
('TRAN', 0, '987654', '00000004', '2025-10-24', '13:00:00', 1008, 'TRANSFER', 'Transfer from current', '987654', '00000003', 500.00),
('TRAN', 0, '987654', '00000005', '2025-10-18', '08:20:00', 1009, 'DEBIT', 'Coffee shop', NULL, NULL, -4.50),
('TRAN', 0, '987654', '00000005', '2025-10-25', '15:30:00', 1010, 'DEBIT', 'Restaurant', NULL, NULL, -65.00),
('TRAN', 0, '987654', '00000006', '2025-10-15', '09:00:00', 1011, 'DEBIT', 'Loan repayment', NULL, NULL, -250.00),
('TRAN', 0, '987654', '00000007', '2025-10-21', '10:30:00', 1012, 'DEBIT', 'Utility bill', NULL, NULL, -95.25),
('TRAN', 0, '987654', '00000007', '2025-10-26', '11:15:00', 1013, 'CREDIT', 'Freelance payment', NULL, NULL, 850.00),
('TRAN', 0, '987654', '00000008', '2025-10-22', '14:00:00', 1014, 'CREDIT', 'Interest payment', NULL, NULL, 52.08),
('TRAN', 0, '987654', '00000009', '2025-10-19', '09:45:00', 1015, 'DEBIT', 'Supermarket', NULL, NULL, -125.60),
('TRAN', 0, '987654', '00000009', '2025-10-23', '16:00:00', 1016, 'CREDIT', 'Salary deposit', NULL, NULL, 2200.00),
('TRAN', 0, '987654', '00000010', '2025-10-01', '08:00:00', 1017, 'DEBIT', 'Mortgage payment', NULL, NULL, -1450.00),
('TRAN', 0, '987654', '00000001', '2025-10-25', '17:30:00', 1018, 'TRANSFER', 'Transfer to savings', '987654', '00000002', -200.00),
('TRAN', 0, '987654', '00000002', '2025-10-25', '17:30:00', 1019, 'TRANSFER', 'Transfer from current', '987654', '00000001', 200.00),
('TRAN', 0, '987654', '00000007', '2025-10-26', '18:00:00', 1020, 'DEBIT', 'Insurance premium', NULL, NULL, -120.50);
