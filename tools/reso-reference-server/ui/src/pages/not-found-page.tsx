import { useNavigate } from 'react-router';

/** 404 fallback page. */
export const NotFoundPage = () => {
  const navigate = useNavigate();

  return (
    <div className="h-full overflow-y-auto p-4 sm:p-6">
      <div className="text-center py-12">
        <h2 className="text-2xl font-semibold text-gray-700 dark:text-gray-300 mb-2">Page Not Found</h2>
        <p className="text-gray-500 dark:text-gray-400 mb-4">The page you are looking for does not exist.</p>
        <button
          type="button"
          onClick={() => navigate('/Property')}
          className="px-4 py-2 bg-blue-600 text-white text-sm rounded hover:bg-blue-700">
          Go to Property
        </button>
      </div>
    </div>
  );
};
