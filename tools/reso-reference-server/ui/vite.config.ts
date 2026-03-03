import tailwindcss from '@tailwindcss/vite';
import react from '@vitejs/plugin-react';
import { defineConfig } from 'vite';

export default defineConfig({
  plugins: [react(), tailwindcss()],
  server: {
    port: 5173,
    proxy: {
      '/Property': 'http://localhost:8080',
      '/Member': 'http://localhost:8080',
      '/Office': 'http://localhost:8080',
      '/Media': 'http://localhost:8080',
      '/OpenHouse': 'http://localhost:8080',
      '/Showing': 'http://localhost:8080',
      '/ui-config': 'http://localhost:8080',
      '/field-groups': 'http://localhost:8080',
      '/api': 'http://localhost:8080',
      '/health': 'http://localhost:8080',
      '/images': 'http://localhost:8080',
      '/$metadata': 'http://localhost:8080'
    }
  }
});
