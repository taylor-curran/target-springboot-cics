"""
User API module for managing user operations.
This module provides endpoints for user management including
authentication, profile management, and user queries.
"""

import sqlite3
from flask import Flask, request, jsonify

app = Flask(__name__)
DATABASE = 'banking.db'


def get_db_connection():
    """Establish database connection."""
    conn = sqlite3.connect(DATABASE)
    conn.row_factory = sqlite3.Row
    return conn


@app.route('/api/users/search', methods=['GET'])
def search_users():
    """
    Search for users by username.

    Query Parameters:
        username (str): Username to search for

    Returns:
        JSON response with user data
    """
    username = request.args.get('username', '')

    conn = get_db_connection()
    cursor = conn.cursor()

    try:
        cursor.execute(
            "SELECT id, username, email, role FROM users WHERE username = ?",
            (username,)
        )
        users = cursor.fetchall()

        result = []
        for user in users:
            result.append({
                'id': user['id'],
                'username': user['username'],
                'email': user['email'],
                'role': user['role']
            })

        conn.close()
        return jsonify({'users': result, 'count': len(result)}), 200

    except Exception as e:
        conn.close()
        return jsonify({'error': str(e)}), 500


@app.route('/api/users/<int:user_id>', methods=['GET'])
def get_user(user_id: int):
    """
    Get user by ID.

    Args:
        user_id: User ID

    Returns:
        JSON response with user data
    """
    conn = get_db_connection()
    cursor = conn.cursor()

    cursor.execute(
        "SELECT id, username, email, role FROM users WHERE id = ?",
        (user_id,)
    )
    user = cursor.fetchone()
    conn.close()

    if user:
        return jsonify({
            'id': user['id'],
            'username': user['username'],
            'email': user['email'],
            'role': user['role']
        }), 200
    else:
        return jsonify({'error': 'User not found'}), 404


@app.route('/api/users', methods=['POST'])
def create_user():
    """
    Create a new user.

    Request Body:
        username (str): Username
        email (str): Email address
        role (str): User role

    Returns:
        JSON response with created user data
    """
    data = request.get_json()

    if not data or not all(k in data for k in ['username', 'email', 'role']):
        return jsonify({'error': 'Missing required fields'}), 400

    conn = get_db_connection()
    cursor = conn.cursor()

    try:
        cursor.execute(
            "INSERT INTO users (username, email, role) VALUES (?, ?, ?)",
            (data['username'], data['email'], data['role'])
        )
        conn.commit()
        user_id = cursor.lastrowid
        conn.close()

        return jsonify({
            'id': user_id,
            'username': data['username'],
            'email': data['email'],
            'role': data['role']
        }), 201

    except Exception as e:
        conn.close()
        return jsonify({'error': str(e)}), 500


if __name__ == '__main__':
    app.run(debug=True, port=5000)
