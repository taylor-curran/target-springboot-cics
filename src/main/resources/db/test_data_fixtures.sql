
DELETE FROM bank_transaction;
DELETE FROM account;
DELETE FROM customer;
DELETE FROM control;

INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number) 
VALUES ('CONTROL', 3, 3, 5, 5);

INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score, credit_score_review_date)
VALUES 
('CUST', '987654', 1, 'Lord Gretchen F Jones', '68 Quercine Lane, Aylsham', '1901-10-02', 889, '2025-11-04'),
('CUST', '987654', 2, 'Professor Will M Leigh', '66 Nutmeg Frontage, Bath', '1971-04-02', 459, '2025-11-03'),
('CUST', '987654', 3, 'Ms Andy E Jones', '11 Birch Grove, Ormskirk', '1980-01-12', 642, '2025-10-28');

INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, interest_rate, opened_date, overdraft_limit, last_statement_date, next_statement_date, available_balance, actual_balance)
VALUES
('ACCT', 1, '987654', '00000001', 'CURRENT', 0.0, '2025-07-30', 100, '2025-08-30', NULL, 22528.71, 22528.71),
('ACCT', 1, '987654', '00000002', 'LOAN', 17.9, '2024-12-24', 0, '2025-01-24', NULL, -33046.41, -33046.41),
('ACCT', 2, '987654', '00000003', 'SAVING', 2.5, '2025-05-25', 0, '2025-06-25', NULL, 15000.00, 15000.00),
('ACCT', 2, '987654', '00000004', 'ISA', 3.0, '2025-01-15', 0, '2025-02-15', NULL, 8500.00, 8500.00),
('ACCT', 3, '987654', '00000005', 'CURRENT', 0.0, '2025-03-10', 500, '2025-04-10', NULL, 3245.50, 3245.50);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES
('PRTR', 0, '987654', '00000001', '2025-09-27', '18:38:48', 1001000, 'DEB', 'Direct Debit', NULL, NULL, 65.3),
('PRTR', 0, '987654', '00000001', '2025-09-28', '13:32:08', 1001001, 'DEB', 'Card Purchase', NULL, NULL, 224.52),
('PRTR', 0, '987654', '00000001', '2025-09-29', '10:27:35', 1001002, 'DEB', 'Card Purchase', NULL, NULL, 115.73),
('PRTR', 0, '987654', '00000001', '2025-09-30', '09:15:22', 1001003, 'CR', 'Salary Payment', NULL, NULL, 3500.00),
('PRTR', 0, '987654', '00000001', '2025-10-01', '14:45:10', 1001004, 'TFR', 'Transfer to savings', '987654', '00000003', 500.00);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES
('PRTR', 0, '987654', '00000002', '2025-09-25', '08:00:00', 1002000, 'DEB', 'Loan Payment', NULL, NULL, 250.00),
('PRTR', 0, '987654', '00000002', '2025-10-01', '08:00:00', 1002001, 'DEB', 'Loan Payment', NULL, NULL, 250.00);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES
('PRTR', 0, '987654', '00000003', '2025-09-20', '11:30:00', 1003000, 'CR', 'Transfer from current', '987654', '00000001', 500.00),
('PRTR', 0, '987654', '00000003', '2025-10-01', '10:00:00', 1003001, 'CR', 'Interest Payment', NULL, NULL, 31.25),
('PRTR', 0, '987654', '00000003', '2025-10-05', '16:20:00', 1003002, 'DEB', 'Withdrawal', NULL, NULL, 200.00);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES
('PRTR', 0, '987654', '00000004', '2025-09-15', '12:00:00', 1004000, 'CR', 'ISA Deposit', NULL, NULL, 1000.00),
('PRTR', 0, '987654', '00000004', '2025-10-01', '09:00:00', 1004001, 'CR', 'Interest Payment', NULL, NULL, 21.25);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES
('PRTR', 0, '987654', '00000005', '2025-09-22', '15:45:00', 1005000, 'CR', 'Paycheck', NULL, NULL, 2500.00),
('PRTR', 0, '987654', '00000005', '2025-09-28', '11:22:33', 1005001, 'DEB', 'Rent Payment', NULL, NULL, 850.00),
('PRTR', 0, '987654', '00000005', '2025-10-03', '19:10:15', 1005002, 'DEB', 'Grocery Shopping', NULL, NULL, 125.50);
