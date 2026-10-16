// Small DOM helpers. setHTML parses an HTML string into a fragment and swaps it
// in (the mockup's pattern); all dynamic, potentially-unsafe substrings must be
// passed through `esc` first. The HTML here is engine-controlled (numeric
// matrices + escaped process strings), and the app runs locally on the user's
// own files, so the surface is limited — but we escape interpolated text anyway.

export const $ = <T extends HTMLElement = HTMLElement>(sel: string): T => {
  const el = document.querySelector<T>(sel);
  if (!el) throw new Error(`missing element: ${sel}`);
  return el;
};

export function setHTML(el: Element, html: string): void {
  el.replaceChildren(document.createRange().createContextualFragment(html));
}

export function esc(s: string): string {
  return s.replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c] as string));
}
