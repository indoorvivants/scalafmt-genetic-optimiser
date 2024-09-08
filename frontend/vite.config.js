import { defineConfig } from 'vite'
export default defineConfig({
  server: {
    proxy: {
      '/api': 'http://localhost:9999',
      '/ws': {
        target: 'ws://127.0.0.1:9999/',
        ws: true,
      },
    },
  },
})
