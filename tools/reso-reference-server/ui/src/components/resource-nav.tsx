import { NavLink, useParams } from 'react-router';
import { TARGET_RESOURCES } from '../types';

/** Sidebar navigation with resource links and CRUD sub-links. */
export const ResourceNav = () => {
  const { resource: activeResource } = useParams<{ resource: string }>();

  return (
    <div>
      <h2 className="text-xs font-semibold uppercase tracking-wider text-gray-500 dark:text-gray-400 mb-2">Resources</h2>
      <ul className="flex flex-row sm:flex-col gap-1 overflow-x-auto sm:overflow-visible">
        {TARGET_RESOURCES.map(resource => {
          const isActive = activeResource === resource;
          return (
            <li key={resource}>
              <NavLink
                to={`/${resource}`}
                className={`block px-3 py-1.5 rounded text-sm whitespace-nowrap ${
                  isActive
                    ? 'bg-blue-50 dark:bg-blue-900/30 text-blue-700 dark:text-blue-300 font-medium'
                    : 'text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
                }`}>
                {resource}
              </NavLink>
              {isActive && (
                <div className="hidden sm:flex flex-col ml-4 mt-1 gap-0.5">
                  <NavLink
                    to={`/${resource}/add`}
                    className="text-xs text-gray-500 dark:text-gray-400 hover:text-blue-600 dark:hover:text-blue-400 px-2 py-0.5">
                    + Add
                  </NavLink>
                  <NavLink
                    to={`/${resource}/edit`}
                    className="text-xs text-gray-500 dark:text-gray-400 hover:text-blue-600 dark:hover:text-blue-400 px-2 py-0.5">
                    Edit
                  </NavLink>
                  <NavLink
                    to={`/${resource}/delete`}
                    className="text-xs text-gray-500 dark:text-gray-400 hover:text-blue-600 dark:hover:text-blue-400 px-2 py-0.5">
                    Delete
                  </NavLink>
                </div>
              )}
            </li>
          );
        })}
      </ul>
    </div>
  );
};
