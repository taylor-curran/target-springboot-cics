
INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number)
VALUES ('CONTROL', 5, 5, 5, 5);

INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score, credit_score_review_date)
VALUES 
('CUST', '987654', 1, 'Mr John A Smith', '10 Oak Street, London', '1980-05-15', 750, '2025-12-01'),
('CUST', '987654', 2, 'Ms Sarah B Johnson', '25 Maple Avenue, Manchester', '1975-08-22', 680, '2025-11-15'),
('CUST', '987654', 3, 'Dr David C Williams', '42 Pine Road, Birmingham', '1988-03-10', 820, '2025-12-10'),
('CUST', '987654', 4, 'Mrs Emily D Brown', '15 Cedar Close, Leeds', '1992-11-30', 590, '2025-11-20'),
('CUST', '987654', 5, 'Prof Robert E Davis', '8 Birch Lane, Liverpool', '1965-07-18', 710, '2025-12-05');

INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, interest_rate, opened_date, overdraft_limit, last_statement_date, next_statement_date, available_balance, actual_balance)
VALUES 
('ACCT', 1, '987654', '00000001', 'CURRENT', 0.0, '2020-01-15', 500, '2025-09-15', '2025-10-15', 2500.50, 2500.50),
('ACCT', 2, '987654', '00000002', 'SAVING', 2.5, '2019-06-20', 0, '2025-09-20', '2025-10-20', 15000.00, 15000.00),
('ACCT', 3, '987654', '00000003', 'ISA', 3.0, '2021-04-10', 0, '2025-09-10', '2025-10-10', 8000.75, 8000.75),
('ACCT', 4, '987654', '00000004', 'LOAN', 17.9, '2023-02-28', 0, '2025-08-28', '2025-09-28', -5000.00, -5000.00),
('ACCT', 5, '987654', '00000005', 'CURRENT', 0.0, '2018-11-05', 1000, '2025-09-05', '2025-10-05', 1250.25, 1250.25);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES 
('PRTR', 0, '987654', '00000001', '2025-10-01', '09:15:30', 1001001, 'CRE', 'Salary Payment', NULL, NULL, 3000.00),
('PRTR', 0, '987654', '00000001', '2025-10-05', '14:22:15', 1001002, 'DEB', 'ATM Withdrawal', NULL, NULL, 100.00),
('PRTR', 0, '987654', '00000001', '2025-10-10', '11:45:00', 1001003, 'DEB', 'Card Purchase - Supermarket', NULL, NULL, 75.50),
('PRTR', 0, '987654', '00000002', '2025-10-02', '10:30:00', 1002001, 'CRE', 'Transfer from Current', NULL, NULL, 500.00),
('PRTR', 0, '987654', '00000002', '2025-10-15', '16:00:00', 1002002, 'INT', 'Monthly Interest Credit', NULL, NULL, 31.25),
('PRTR', 0, '987654', '00000003', '2025-10-03', '13:10:45', 1003001, 'CRE', 'ISA Contribution', NULL, NULL, 200.00),
('PRTR', 0, '987654', '00000004', '2025-10-01', '08:00:00', 1004001, 'DEB', 'Loan Repayment', NULL, NULL, 150.00),
('PRTR', 0, '987654', '00000005', '2025-10-08', '12:30:15', 1005001, 'TFR', 'Transfer to Smith', '123456', '98765432', 250.00),
('PRTR', 0, '987654', '00000005', '2025-10-12', '15:45:30', 1005002, 'FEE', 'Monthly Account Fee', NULL, NULL, 5.00),
('PRTR', 0, '987654', '00000001', '2025-10-20', '10:15:22', 1001004, 'CRE', 'Bank Transfer', NULL, NULL, 450.00);
