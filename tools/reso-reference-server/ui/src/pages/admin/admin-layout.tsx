import { useState } from 'react';
import { Outlet, useNavigate } from 'react-router';
import { clearAdminToken, getAdminToken, setAdminToken } from '../../api/admin-client';

/** Admin section layout with auth token input and amber accent bar. */
export const AdminLayout = () => {
  const [token, setToken] = useState(getAdminToken() ?? 'admin-token');
  const [isAuthenticated, setIsAuthenticated] = useState(!!getAdminToken());
  const navigate = useNavigate();

  const handleLogin = () => {
    if (token.trim()) {
      setAdminToken(token.trim());
      setIsAuthenticated(true);
    }
  };

  const handleLogout = () => {
    clearAdminToken();
    setIsAuthenticated(false);
    navigate('/');
  };

  if (!isAuthenticated) {
    return (
      <div className="h-full overflow-y-auto p-4 sm:p-6">
        <div className="max-w-md mx-auto mt-12">
          <div className="bg-amber-50 dark:bg-amber-900/20 border border-amber-300 dark:border-amber-700 rounded-lg p-6">
            <h2 className="text-lg font-semibold text-amber-800 dark:text-amber-200 mb-4">Admin Authentication</h2>
            <p className="text-sm text-amber-700 dark:text-amber-300 mb-4">Enter your admin token to access admin tools.</p>
            <div className="flex gap-2">
              <input
                type="password"
                value={token}
                onChange={e => setToken(e.target.value)}
                onKeyDown={e => e.key === 'Enter' && handleLogin()}
                placeholder="Admin token"
                className="flex-1 px-3 py-2 border border-amber-300 dark:border-amber-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100 text-sm focus:outline-none focus:ring-2 focus:ring-amber-500"
              />
              <button
                type="button"
                onClick={handleLogin}
                className="px-4 py-2 bg-amber-600 hover:bg-amber-700 text-white text-sm font-medium rounded-lg transition-colors">
                Login
              </button>
            </div>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="h-full flex flex-col min-h-0">
      {/* Admin accent bar */}
      <div className="shrink-0 bg-amber-500 dark:bg-amber-600 px-4 py-2 flex items-center justify-between">
        <span className="text-sm font-medium text-white">Admin Tools</span>
        <button type="button" onClick={handleLogout} className="text-xs text-amber-100 hover:text-white underline transition-colors">
          Logout
        </button>
      </div>

      <div className="flex-1 overflow-y-auto min-h-0 p-4 sm:p-6">
        <Outlet />
      </div>
    </div>
  );
};
