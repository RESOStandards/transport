import { Outlet, useLocation, useParams } from 'react-router';
import { useDarkMode } from '../hooks/use-dark-mode';
import { ResourceNav } from './resource-nav';

const LOGO_LIGHT = 'https://www.reso.org/wp-content/uploads/2020/06/RESO-Logo_Horizontal_Blue.png';
const LOGO_DARK = 'https://www.reso.org/wp-content/uploads/2020/06/RESO-Logo_Horizontal_White.png';

/** Derives the current page indicator from the URL path. */
const getPageIndicator = (pathname: string, resource?: string): string | null => {
  if (!resource) return null;
  if (pathname.includes('/add')) return 'Add';
  if (pathname.includes('/edit')) return 'Edit';
  if (pathname.includes('/delete')) return 'Delete';
  // Check if it's a detail page (/:resource/:key but not /add, /edit, /delete)
  const parts = pathname.split('/').filter(Boolean);
  if (parts.length >= 2 && parts[0] === resource && !['add', 'edit', 'delete'].includes(parts[1])) return 'Detail';
  return 'Search';
};

/** App shell with responsive sidebar nav, RESO branding, dark mode toggle, and main content. */
export const Layout = () => {
  const { isDark, toggle } = useDarkMode();
  const { resource } = useParams<{ resource: string }>();
  const location = useLocation();
  const pageIndicator = getPageIndicator(location.pathname, resource);

  return (
    <div className="h-screen flex flex-col overflow-hidden bg-gray-50 dark:bg-gray-900 transition-colors">
      {/* Header */}
      <header className="bg-white dark:bg-gray-800 border-b border-gray-200 dark:border-gray-700 px-4 py-3 sm:px-6">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-3">
            {/* RESO Logo */}
            <img src={isDark ? LOGO_DARK : LOGO_LIGHT} alt="RESO" className="h-8 sm:h-10" />
            {/* Title */}
            <div className="flex items-baseline gap-2">
              <h1 className="text-lg sm:text-xl font-semibold text-gray-900 dark:text-gray-100">RESO Web API Reference Server</h1>
              {pageIndicator && <span className="text-sm text-gray-500 dark:text-gray-400 hidden sm:inline">/ {pageIndicator}</span>}
            </div>
          </div>

          {/* Dark mode toggle */}
          <button
            type="button"
            onClick={toggle}
            className="p-2 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-700 text-gray-500 dark:text-gray-400 transition-colors"
            aria-label={isDark ? 'Switch to light mode' : 'Switch to dark mode'}
            title={isDark ? 'Switch to light mode' : 'Switch to dark mode'}>
            {isDark ? (
              <svg
                xmlns="http://www.w3.org/2000/svg"
                viewBox="0 0 20 20"
                fill="currentColor"
                className="w-5 h-5"
                role="img"
                aria-hidden="true">
                <path d="M10 2a.75.75 0 01.75.75v1.5a.75.75 0 01-1.5 0v-1.5A.75.75 0 0110 2zM10 15a.75.75 0 01.75.75v1.5a.75.75 0 01-1.5 0v-1.5A.75.75 0 0110 15zM10 7a3 3 0 100 6 3 3 0 000-6zM15.657 5.404a.75.75 0 10-1.06-1.06l-1.061 1.06a.75.75 0 001.06 1.06l1.06-1.06zM6.464 14.596a.75.75 0 10-1.06-1.06l-1.06 1.06a.75.75 0 001.06 1.06l1.06-1.06zM18 10a.75.75 0 01-.75.75h-1.5a.75.75 0 010-1.5h1.5A.75.75 0 0118 10zM5 10a.75.75 0 01-.75.75h-1.5a.75.75 0 010-1.5h1.5A.75.75 0 015 10zM14.596 15.657a.75.75 0 001.06-1.06l-1.06-1.061a.75.75 0 10-1.06 1.06l1.06 1.06zM5.404 6.464a.75.75 0 001.06-1.06l-1.06-1.06a.75.75 0 10-1.06 1.06l1.06 1.06z" />
              </svg>
            ) : (
              <svg
                xmlns="http://www.w3.org/2000/svg"
                viewBox="0 0 20 20"
                fill="currentColor"
                className="w-5 h-5"
                role="img"
                aria-hidden="true">
                <path
                  fillRule="evenodd"
                  d="M7.455 2.004a.75.75 0 01.26.77 7 7 0 009.958 7.967.75.75 0 011.067.853A8.5 8.5 0 116.647 1.921a.75.75 0 01.808.083z"
                  clipRule="evenodd"
                />
              </svg>
            )}
          </button>
        </div>
      </header>

      <div className="flex flex-col sm:flex-row flex-1 overflow-hidden">
        {/* Sidebar — fixed, does not scroll with content */}
        <nav className="w-full sm:w-56 shrink-0 bg-white dark:bg-gray-800 border-b sm:border-b-0 sm:border-r border-gray-200 dark:border-gray-700 p-3 sm:p-4 sm:overflow-y-auto">
          <ResourceNav />
        </nav>

        {/* Main content — each page manages its own scrolling */}
        <main className="flex-1 overflow-hidden">
          <Outlet />
        </main>
      </div>
    </div>
  );
};
