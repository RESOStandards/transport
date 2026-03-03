import { useCallback, useEffect, useState } from 'react';
import { useSearchParams } from 'react-router';

/** Manages dark mode state. Persists in URL query param `theme` and falls back to system preference. */
export const useDarkMode = () => {
  const [searchParams, setSearchParams] = useSearchParams();
  const themeParam = searchParams.get('theme');

  // Determine initial state from URL param, or fall back to system preference
  const getInitial = (): boolean => {
    if (themeParam === 'dark') return true;
    if (themeParam === 'light') return false;
    return window.matchMedia('(prefers-color-scheme: dark)').matches;
  };

  const [isDark, setIsDark] = useState(getInitial);

  // Apply dark class to <html> element
  useEffect(() => {
    document.documentElement.classList.toggle('dark', isDark);
  }, [isDark]);

  const toggle = useCallback(() => {
    setIsDark(prev => {
      const next = !prev;
      const params = new URLSearchParams(searchParams);
      params.set('theme', next ? 'dark' : 'light');
      setSearchParams(params, { replace: true });
      return next;
    });
  }, [searchParams, setSearchParams]);

  return { isDark, toggle };
};
