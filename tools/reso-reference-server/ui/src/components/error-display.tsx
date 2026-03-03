interface ErrorDisplayProps {
  readonly message: string;
  readonly details?: ReadonlyArray<{ readonly target?: string; readonly message: string }>;
}

/** Displays API errors and validation failure details. */
export const ErrorDisplay = ({ message, details }: ErrorDisplayProps) => (
  <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded p-3">
    <p className="text-sm text-red-700 dark:text-red-400 font-medium">{message}</p>
    {details && details.length > 0 && (
      <ul className="mt-1 space-y-0.5">
        {details.map(d => (
          <li key={d.target ?? d.message} className="text-xs text-red-600 dark:text-red-400">
            {d.target && <span className="font-mono">{d.target}: </span>}
            {d.message}
          </li>
        ))}
      </ul>
    )}
  </div>
);
