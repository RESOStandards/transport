import type { FieldGroups, UiConfig } from '../types';

let cachedUiConfig: UiConfig | null = null;
let cachedFieldGroups: FieldGroups | null = null;

/** Fetches the UI config from the server. Cached after first call. */
export const fetchUiConfig = async (): Promise<UiConfig> => {
  if (cachedUiConfig) return cachedUiConfig;
  const res = await fetch('/ui-config');
  if (!res.ok) throw new Error(`Failed to fetch UI config: ${res.statusText}`);
  cachedUiConfig = await res.json();
  return cachedUiConfig!;
};

/** Fetches the field groups mapping from the server. Cached after first call. */
export const fetchFieldGroups = async (): Promise<FieldGroups> => {
  if (cachedFieldGroups) return cachedFieldGroups;
  const res = await fetch('/field-groups');
  if (!res.ok) throw new Error(`Failed to fetch field groups: ${res.statusText}`);
  cachedFieldGroups = await res.json();
  return cachedFieldGroups!;
};
