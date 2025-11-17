import React, { useState, useEffect } from 'react';

interface User {
  id: string;
  name: string;
  email: string;
  bio: string;
  website: string;
  location: string;
  joinDate: string;
  avatar: string;
}

interface UserProfileProps {
  userId: string;
}

/**
 * UserProfile Component
 * Displays user profile information including bio, contact details, and social links
 */
const UserProfile: React.FC<UserProfileProps> = ({ userId }) => {
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState<boolean>(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchUserData = async () => {
      try {
        setLoading(true);
        const response = await fetch(`/api/users/${userId}`);
        if (!response.ok) {
          throw new Error('Failed to fetch user data');
        }
        const userData = await response.json();
        setUser(userData);
      } catch (err) {
        setError(err instanceof Error ? err.message : 'An error occurred');
      } finally {
        setLoading(false);
      }
    };

    if (userId) {
      fetchUserData();
    }
  }, [userId]);

  if (loading) {
    return <div className="loading">Loading user profile...</div>;
  }

  if (error) {
    return <div className="error">Error: {error}</div>;
  }

  if (!user) {
    return <div className="not-found">User not found</div>;
  }

  return (
    <div className="user-profile">
      <div className="profile-header">
        <img src={user.avatar} alt={user.name} className="avatar" />
        <h1>{user.name}</h1>
        <p className="email">{user.email}</p>
      </div>
      
      <div className="profile-details">
        <div className="section">
          <h2>About</h2>
          {/* FIXED: User-controlled data is now safely rendered with automatic HTML encoding */}
          <div className="bio">{user.bio}</div>
        </div>
        
        <div className="section">
          <h2>Contact Information</h2>
          <div className="contact-info">
            <p><strong>Website:</strong> <a href={user.website}>{user.website}</a></p>
            <p><strong>Location:</strong> {user.location}</p>
            <p><strong>Member since:</strong> {new Date(user.joinDate).toLocaleDateString()}</p>
          </div>
        </div>
      </div>
    </div>
  );
};

export default UserProfile;
