
INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number)
VALUES ('CONTROL', 2, 2, 3, 3);

INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score, credit_score_review_date)
VALUES 
('CUST', '987654', 1, 'Mr John A Smith', '123 Main Street, London', '1980-05-15', 750, '2025-11-15'),
('CUST', '987654', 2, 'Ms Jane B Doe', '456 Oak Avenue, Manchester', '1992-08-22', 820, '2025-11-20');

INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, interest_rate, opened_date, overdraft_limit, last_statement_date, next_statement_date, available_balance, actual_balance)
VALUES 
('ACCT', 1, '987654', '00000001', 'CURRENT', 0.0, '2023-01-10', 500, '2025-10-01', '2025-11-01', 1500.00, 1500.00),
('ACCT', 1, '987654', '00000002', 'SAVING', 2.5, '2023-03-15', 0, '2025-10-01', '2025-11-01', 5000.00, 5000.00),
('ACCT', 2, '987654', '00000003', 'CURRENT', 0.0, '2024-06-20', 1000, '2025-10-01', '2025-11-01', 2500.00, 2500.00);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES 
('TRAN', 0, '987654', '00000001', '2025-10-25', '10:30:00', 1, 'DEBIT', 'ATM Withdrawal', NULL, NULL, -50.00),
('TRAN', 0, '987654', '00000001', '2025-10-26', '14:15:00', 2, 'CREDIT', 'Salary Deposit', NULL, NULL, 2000.00),
('TRAN', 0, '987654', '00000002', '2025-10-25', '09:00:00', 3, 'CREDIT', 'Transfer from Current', NULL, NULL, 500.00),
('TRAN', 0, '987654', '00000003', '2025-10-26', '11:20:00', 4, 'DEBIT', 'Online Purchase', NULL, NULL, -75.50),
('TRAN', 0, '987654', '00000003', '2025-10-27', '16:45:00', 5, 'CREDIT', 'Refund', NULL, NULL, 25.00);
