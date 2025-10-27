
INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number)
VALUES ('CONTROL', 3, 3, 5, 5);

INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score, credit_score_review_date)
VALUES 
  ('CUST', '987654', 1, 'Mr. John A Smith', '123 High Street, London', '1980-05-15', 750, '2025-11-15'),
  ('CUST', '987654', 2, 'Mrs. Jane B Jones', '456 Main Road, Manchester', '1975-08-22', 680, '2025-11-20'),
  ('CUST', '987654', 3, 'Dr. Robert C Brown', '789 Park Avenue, Birmingham', '1990-03-10', 820, '2025-11-25');

INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, interest_rate, opened_date, overdraft_limit, last_statement_date, next_statement_date, available_balance, actual_balance)
VALUES
  ('ACCT', 1, '987654', '00000001', 'CURRENT', 0.0, '2024-01-15', 100, '2025-09-15', '2025-10-15', 1500.00, 1500.00),
  ('ACCT', 1, '987654', '00000002', 'SAVING', 2.5, '2024-02-01', 0, '2025-09-01', '2025-10-01', 5000.00, 5000.00),
  ('ACCT', 2, '987654', '00000003', 'CURRENT', 0.0, '2024-03-20', 200, '2025-09-20', '2025-10-20', 2500.00, 2500.00),
  ('ACCT', 2, '987654', '00000004', 'ISA', 3.0, '2024-04-10', 0, NULL, NULL, 10000.00, 10000.00),
  ('ACCT', 3, '987654', '00000005', 'MORTGAGE', 4.5, '2023-06-01', 0, '2025-09-01', '2025-10-01', -150000.00, -150000.00);

INSERT INTO bank_transaction (eye_catcher, logically_deleted, sort_code, account_number, transaction_date, transaction_time, reference_number, transaction_type, description, target_sort_code, target_account_number, amount)
VALUES
  ('PRTR', 0, '987654', '00000001', '2025-10-01', '09:30:00', 1001, 'DEB', 'Direct Debit', NULL, NULL, 50.00),
  ('PRTR', 0, '987654', '00000001', '2025-10-05', '14:22:15', 1002, 'CRE', 'Salary Payment', NULL, NULL, 3000.00),
  ('PRTR', 0, '987654', '00000002', '2025-10-10', '10:15:30', 1003, 'CRE', 'Interest Payment', NULL, NULL, 10.50),
  ('PRTR', 0, '987654', '00000003', '2025-10-15', '16:45:00', 1004, 'TFR', 'Transfer to Savings', '987654', '00000004', 500.00),
  ('PRTR', 0, '987654', '00000004', '2025-10-15', '16:45:05', 1005, 'TFR', 'Transfer from Current', '987654', '00000003', 500.00);
