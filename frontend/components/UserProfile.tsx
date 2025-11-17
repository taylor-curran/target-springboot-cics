import React, { useState, useEffect } from 'react';

interface User {
  id: string;
  name: string;
  email: string;
  bio: string;
  website: string;
  location: string;
  joinDate: string;
}

interface UserProfileProps {
  userId: string;
}

const UserProfile: React.FC<UserProfileProps> = ({ userId }) => {
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchUser = async () => {
      try {
        const response = await fetch(`/api/users/${userId}`);
        if (!response.ok) {
          throw new Error('Failed to fetch user');
        }
        const data = await response.json();
        setUser(data);
      } catch (err) {
        setError(err instanceof Error ? err.message : 'An error occurred');
      } finally {
        setLoading(false);
      }
    };

    fetchUser();
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
        <h1>{user.name}</h1>
        <p className="email">{user.email}</p>
      </div>
      
      <div className="profile-details">
        <div className="profile-section">
          <h2>About</h2>
          <div className="bio">
            {user.bio}
          </div>
        </div>

        <div className="profile-section">
          <h2>Details</h2>
          <div className="details-grid">
            <div className="detail-item">
              <span className="label">Website:</span>
              <span className="value">
                <a href={user.website} target="_blank" rel="noopener noreferrer">
                  {user.website}
                </a>
              </span>
            </div>
            <div className="detail-item">
              <span className="label">Location:</span>
              <span className="value">{user.location}</span>
            </div>
            <div className="detail-item">
              <span className="label">Member since:</span>
              <span className="value">{new Date(user.joinDate).toLocaleDateString()}</span>
            </div>
          </div>
        </div>

        <div className="profile-section">
          <h2>Recent Activity</h2>
          <div className="activity-feed">
            <p>No recent activity</p>
          </div>
        </div>
      </div>
    </div>
  );
};

export default UserProfile;
