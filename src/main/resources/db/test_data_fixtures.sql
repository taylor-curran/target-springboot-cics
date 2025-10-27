
INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number)
VALUES ('CONTROL', 5, 5, 10, 10);

INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score, credit_score_review_date)
VALUES 
('CUST', '987654', 1, 'Mr John A Smith', '42 Oak Avenue, London', '1985-03-15', 750, '2025-11-15'),
('CUST', '987654', 2, 'Mrs Sarah B Johnson', '15 Elm Street, Manchester', '1990-07-22', 680, '2025-11-20'),
('CUST', '987654', 3, 'Ms Emily C Williams', '88 Pine Road, Birmingham', '1978-11-30', 820, '2025-11-25'),
('CUST', '987654', 4, 'Dr Robert D Brown', '23 Maple Drive, Liverpool', '1982-05-18', 795, '2025-12-01'),
('CUST', '987654', 5, 'Miss Jane E Davis', '67 Cedar Lane, Leeds', '1995-09-08', 710, '2025-12-05');

INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, interest_rate, opened_date, overdraft_limit, last_statement_date, next_statement_date, available_balance, actual_balance)
VALUES
('ACCT', 1, '987654', '00000001', 'CURRENT', 0.5, '2024-01-15', 1000, '2025-09-15', '2025-10-15', 2500.00, 2500.00),
('ACCT', 1, '987654', '00000002', 'SAVING', 2.5, '2024-02-20', 0, '2025-09-20', '2025-10-20', 15000.00, 15000.00),
('ACCT', 2, '987654', '00000003', 'CURRENT', 0.5, '2024-03-10', 500, '2025-09-10', '2025-10-10', 1200.00, 1200.00),
('ACCT', 2, '987654', '00000004', 'ISA', 3.0, '2024-04-05', 0, '2025-09-05', '2025-10-05', 8000.00, 8000.00),
('ACCT', 3, '987654', '00000005', 'CURRENT', 0.5, '2024-05-12', 2000, '2025-09-12', '2025-10-12', 5600.00, 5600.00),
('ACCT', 3, '987654', '00000006', 'MORTGAGE', 3.75, '2024-06-18', 0, '2025-09-18', '2025-10-18', -250000.00, -250000.00),
('ACCT', 4, '987654', '00000007', 'SAVING', 2.5, '2024-07-22', 0, '2025-09-22', '2025-10-22', 25000.00, 25000.00),
('ACCT', 4, '987654', '00000008', 'CURRENT', 0.5, '2024-08-01', 1500, '2025-09-01', '2025-10-01', 3800.00, 3800.00),
('ACCT', 5, '987654', '00000009', 'CURRENT', 0.5, '2024-09-10', 800, '2025-09-10', '2025-10-10', 950.00, 950.00),
('ACCT', 5, '987654', '00000010', 'LOAN', 5.5, '2024-10-15', 0, '2025-09-15', '2025-10-15', -5000.00, -5000.00);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES
('TRAN', 0, '987654', '00000001', '2025-10-20', '10:30:00', 1001, 'DEPOSIT', 'ATM Deposit', NULL, NULL, 500.00),
('TRAN', 0, '987654', '00000001', '2025-10-21', '14:15:00', 1002, 'WITHDRAWAL', 'Cash Withdrawal', NULL, NULL, -100.00),
('TRAN', 0, '987654', '00000002', '2025-10-22', '09:45:00', 1003, 'DEPOSIT', 'Transfer In', NULL, NULL, 1000.00),
('TRAN', 0, '987654', '00000003', '2025-10-23', '16:20:00', 1004, 'PAYMENT', 'Bill Payment', NULL, NULL, -250.00),
('TRAN', 0, '987654', '00000005', '2025-10-24', '11:00:00', 1005, 'TRANSFER', 'Transfer to savings', '987654', '00000006', -500.00),
('TRAN', 0, '987654', '00000006', '2025-10-24', '11:00:00', 1006, 'TRANSFER', 'Transfer from current', '987654', '00000005', 500.00),
('TRAN', 0, '987654', '00000007', '2025-10-25', '13:30:00', 1007, 'INTEREST', 'Monthly Interest', NULL, NULL, 50.00),
('TRAN', 0, '987654', '00000008', '2025-10-26', '10:00:00', 1008, 'DEPOSIT', 'Salary Payment', NULL, NULL, 3000.00),
('TRAN', 0, '987654', '00000009', '2025-10-26', '15:45:00', 1009, 'WITHDRAWAL', 'ATM Withdrawal', NULL, NULL, -50.00),
('TRAN', 0, '987654', '00000010', '2025-10-27', '09:15:00', 1010, 'PAYMENT', 'Loan Payment', NULL, NULL, -200.00);
