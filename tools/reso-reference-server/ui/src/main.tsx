import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { Navigate, RouterProvider, createBrowserRouter } from 'react-router';
import { Layout } from './components/layout';
import './index.css';
import { AddPage } from './pages/add-page';
import { DeletePage } from './pages/delete-page';
import { DetailPage } from './pages/detail-page';
import { EditPage } from './pages/edit-page';
import { NotFoundPage } from './pages/not-found-page';
import { SearchPage } from './pages/search-page';

const router = createBrowserRouter([
  {
    path: '/',
    element: <Layout />,
    children: [
      { index: true, element: <Navigate to="/Property" replace /> },
      { path: ':resource', element: <SearchPage /> },
      { path: ':resource/add', element: <AddPage /> },
      { path: ':resource/edit', element: <EditPage /> },
      { path: ':resource/edit/:key', element: <EditPage /> },
      { path: ':resource/delete', element: <DeletePage /> },
      { path: ':resource/:key', element: <DetailPage /> },
      { path: '*', element: <NotFoundPage /> }
    ]
  }
]);

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <RouterProvider router={router} />
  </StrictMode>
);
