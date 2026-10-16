import { defineConfig } from "vite";

// Tauri expects a fixed dev port and no screen-clearing so its logs stay visible.
export default defineConfig({
  clearScreen: false,
  // Allow importing QUARC_SYNTAX.md from the repo root (one level above gui/).
  server: { port: 1420, strictPort: true, fs: { allow: [".."] } },
  build: { target: "esnext" },
});
