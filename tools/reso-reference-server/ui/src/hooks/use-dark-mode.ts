import { useCallback, useEffect, useState } from 'react';

const STORAGE_KEY = 'reso-theme';

/**
 * Manages dark mode state. Priority order:
 * 1. URL query param `?theme=dark|light` (one-time override, not required on every page)
 * 2. localStorage preference (persists across navigation)
 * 3. System preference (prefers-color-scheme)
 */
export const useDarkMode = () => {
  const getInitial = (): boolean => {
    // Check URL param first (one-time override)
    const url = new URL(window.location.href);
    const themeParam = url.searchParams.get('theme');
    if (themeParam === 'dark') return true;
    if (themeParam === 'light') return false;

    // Check localStorage
    const stored = localStorage.getItem(STORAGE_KEY);
    if (stored === 'dark') return true;
    if (stored === 'light') return false;

    // Fall back to system preference
    return window.matchMedia('(prefers-color-scheme: dark)').matches;
  };

  const [isDark, setIsDark] = useState(getInitial);

  // Apply dark class to <html> element and persist to localStorage
  useEffect(() => {
    document.documentElement.classList.toggle('dark', isDark);
    localStorage.setItem(STORAGE_KEY, isDark ? 'dark' : 'light');
  }, [isDark]);

  const toggle = useCallback(() => {
    setIsDark(prev => !prev);
  }, []);

  return { isDark, toggle };
};
