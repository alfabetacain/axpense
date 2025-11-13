import { defineConfig, Plugin } from 'vite';

const isProd = process.env.NODE_ENV == "production";

export default defineConfig({
  resolve: {
    alias: [
      {
        // to resolve scalajs import in main.js
        find: /^scalajs:(.*)$/,
        replacement: `/out/client/${isProd ? 'full' : 'fast'}LinkJS.dest/$1`
      }
    ]
  },
  base: '/alfabetacain/',
  server: {
    port: 5173,
    proxy: {
      '^/api/.*': {
        target: 'http://localhost:8080',
        changeOrigin: true,
      },
    },
  }
});
