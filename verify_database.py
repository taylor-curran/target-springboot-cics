#!/usr/bin/env python3

import sqlite3
import sys
from pathlib import Path

def verify_database(db_path):
    
    print(f"Verifying database at: {db_path}")
    print("=" * 60)
    
    if not db_path.exists():
        print(f"ERROR: Database file not found at {db_path}")
        return False
    
    conn = sqlite3.connect(str(db_path))
    cursor = conn.cursor()
    
    try:
        print("\n1. Checking table existence...")
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")
        tables = [row[0] for row in cursor.fetchall()]
        
        expected_tables = ['account', 'application_error', 'bank_transaction', 'control', 'customer']
        
        print(f"   Found tables: {tables}")
        
        for table in expected_tables:
            if table in tables:
                print(f"   ✓ Table '{table}' exists")
            else:
                print(f"   ✗ Table '{table}' MISSING")
                return False
        
        print("\n2. Checking table record counts...")
        table_counts = {}
        for table in expected_tables:
            cursor.execute(f"SELECT COUNT(*) FROM {table}")
            count = cursor.fetchone()[0]
            table_counts[table] = count
            print(f"   {table}: {count} records")
        
        print("\n3. Checking indexes...")
        cursor.execute("SELECT name FROM sqlite_master WHERE type='index' ORDER BY name")
        indexes = [row[0] for row in cursor.fetchall()]
        print(f"   Found {len(indexes)} indexes:")
        for idx in indexes:
            if not idx.startswith('sqlite_'):
                print(f"   - {idx}")
        
        print("\n4. Checking foreign key constraints...")
        cursor.execute("PRAGMA foreign_keys")
        fk_enabled = cursor.fetchone()[0]
        print(f"   Foreign keys enabled: {bool(fk_enabled)}")
        
        print("\n5. Sampling data...")
        cursor.execute("SELECT name, sort_code, customer_number FROM customer LIMIT 3")
        customers = cursor.fetchall()
        print(f"   Sample customers:")
        for cust in customers:
            print(f"   - {cust[0]} (Sort: {cust[1]}, Num: {cust[2]})")
        
        cursor.execute("SELECT account_number, account_type, actual_balance FROM account LIMIT 3")
        accounts = cursor.fetchall()
        print(f"   Sample accounts:")
        for acc in accounts:
            print(f"   - Account {acc[0]}: {acc[1]} (Balance: {acc[2]})")
        
        print("\n" + "=" * 60)
        print("✓ Database verification PASSED")
        print("=" * 60)
        
        return True
        
    except Exception as e:
        print(f"\n✗ ERROR during verification: {e}")
        return False
    finally:
        conn.close()

def test_fixtures_loading(repo_root):
    
    print("\n" + "=" * 60)
    print("Testing fixture loading...")
    print("=" * 60)
    
    fixtures_path = repo_root / "src/main/resources/db/test_data_fixtures.sql"
    schema_path = repo_root / "src/main/resources/db/schema.sql"
    
    if not fixtures_path.exists():
        print(f"✗ Fixtures file not found at: {fixtures_path}")
        return False
    
    if not schema_path.exists():
        print(f"✗ Schema file not found at: {schema_path}")
        return False
    
    test_db = repo_root / "test_verify.db"
    if test_db.exists():
        test_db.unlink()
    
    try:
        conn = sqlite3.connect(str(test_db))
        cursor = conn.cursor()
        
        print("\n1. Loading schema...")
        with open(schema_path, 'r') as f:
            schema_sql = f.read()
        cursor.executescript(schema_sql)
        print("   ✓ Schema loaded")
        
        print("\n2. Loading test fixtures...")
        with open(fixtures_path, 'r') as f:
            fixtures_sql = f.read()
        cursor.executescript(fixtures_sql)
        print("   ✓ Fixtures loaded")
        
        print("\n3. Verifying fixture data...")
        cursor.execute("SELECT COUNT(*) FROM customer")
        cust_count = cursor.fetchone()[0]
        print(f"   - Customers: {cust_count}")
        
        cursor.execute("SELECT COUNT(*) FROM account")
        acc_count = cursor.fetchone()[0]
        print(f"   - Accounts: {acc_count}")
        
        cursor.execute("SELECT COUNT(*) FROM bank_transaction")
        txn_count = cursor.fetchone()[0]
        print(f"   - Transactions: {txn_count}")
        
        cursor.execute("SELECT COUNT(*) FROM application_error")
        err_count = cursor.fetchone()[0]
        print(f"   - Errors: {err_count}")
        
        cursor.execute("SELECT COUNT(*) FROM control")
        ctrl_count = cursor.fetchone()[0]
        print(f"   - Control: {ctrl_count}")
        
        conn.close()
        
        print("\n✓ Fixture loading test PASSED")
        print("=" * 60)
        
        return True
        
    except Exception as e:
        print(f"\n✗ ERROR during fixture loading: {e}")
        import traceback
        traceback.print_exc()
        return False
    finally:
        if test_db.exists():
            test_db.unlink()

def main():
    repo_root = Path(__file__).parent
    db_path = repo_root / "banking.db"
    
    print("Banking Database Verification Script")
    print("=" * 60)
    
    db_ok = verify_database(db_path)
    
    fixtures_ok = test_fixtures_loading(repo_root)
    
    if db_ok and fixtures_ok:
        print("\n✅ ALL CHECKS PASSED")
        sys.exit(0)
    else:
        print("\n❌ SOME CHECKS FAILED")
        sys.exit(1)

if __name__ == "__main__":
    main()
