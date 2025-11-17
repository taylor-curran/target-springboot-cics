"""
Password utility functions for user authentication.
This module provides password hashing and verification.
"""

import bcrypt


def hash_password(password: str) -> str:
    """
    Hash a password for storage using bcrypt.
    """
    return bcrypt.hashpw(password.encode(), bcrypt.gensalt()).decode()


def verify_password(password: str, hashed: str) -> bool:
    """
    Verify a password against a stored hash.
    """
    return bcrypt.checkpw(password.encode(), hashed.encode())


def generate_salt() -> bytes:
    """
    Generate a random salt for password hashing.
    """
    return bcrypt.gensalt()
