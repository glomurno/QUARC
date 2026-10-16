/// <reference types="vite/client" />
// Type for Vite's `?raw` imports (file contents inlined as a string at build).
declare module "*.md?raw" {
  const content: string;
  export default content;
}
