// Example protocols bundled into the app at build time (from the repo's
// samples/), so a new user can try the workbench without a file of their own.
// They open as unsaved buffers: there is no path on disk to write back to.

const SOURCES = import.meta.glob<string>("../../samples/*/*.quarc", {
  query: "?raw",
  import: "default",
  eager: true,
});

export interface Example {
  /** Display / buffer name, e.g. `teleportation.quarc`. */
  name: string;
  title: string;
  description: string;
  text: string;
}

// Curated order; the description falls back to the file's leading `--` comment.
const CURATED: { path: string; title: string; description?: string }[] = [
  {
    path: "teleportation/teleportation.quarc",
    title: "Teleportation",
    description: "Alice sends a qubit to Bob using a Bell pair and two classical bits; compared against its spec.",
  },
  { path: "superdensecoding/superdensecoding.quarc", title: "Superdense coding" },
  { path: "cointoss/cointoss.quarc", title: "Coin toss" },
  { path: "lottery/lottery.quarc", title: "Quantum lottery" },
  { path: "qss/qss.quarc", title: "Quantum secret sharing" },
  { path: "ftl/ftl.quarc", title: "No-signalling" },
];

/** First `--` comment line of a source, without the marker. */
function leadingComment(text: string): string {
  const line = text.split("\n").find((l) => l.trim().startsWith("--"));
  // Drop a leading "Topic: " label, which would just repeat the title.
  const desc = line
    ? line
        .trim()
        .replace(/^--\s*/, "")
        .replace(/^[^:()]{1,40}:\s+/, "")
    : "";
  return desc.charAt(0).toUpperCase() + desc.slice(1);
}

export const EXAMPLES: Example[] = CURATED.flatMap(({ path, title, description }) => {
  const text = SOURCES[`../../samples/${path}`];
  if (text == null) return [];
  return [{ name: path.split("/").pop()!, title, description: description ?? leadingComment(text), text }];
});
