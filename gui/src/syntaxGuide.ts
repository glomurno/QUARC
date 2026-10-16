// Renders QUARC_SYNTAX.md (bundled at build time) to HTML for the in-app guide
// overlay. A small purpose-built Markdown pass covers exactly what the guide
// uses — headings, fenced code, tables, lists, rules, and inline code/bold/
// links — and highlights ```quarc fences with the shared tokenizer so code
// samples match the editor. Cached after the first render.

import guideMd from "../../QUARC_SYNTAX.md?raw";
import { esc } from "./dom";
import { highlightQuarc } from "./quarcSyntax";

let cache: string | null = null;

/** Render the syntax guide to an HTML string (memoized). */
export function renderSyntaxGuide(): string {
  if (cache == null) cache = mdToHtml(guideMd);
  return cache;
}

// ── inline spans ──
// Split on `code` first so bold/link rules never touch code contents, then
// escape and apply the remaining inline rules to the plain-text segments.
function inline(s: string): string {
  return s
    .split(/(`[^`]+`)/g)
    .map((seg) => {
      if (seg.startsWith("`") && seg.endsWith("`") && seg.length >= 2) {
        return `<code>${esc(seg.slice(1, -1))}</code>`;
      }
      return esc(seg)
        .replace(/\[([^\]]+)\]\(([^)]+)\)/g, (_m, t: string, u: string) => `<a href="${u}" target="_blank" rel="noopener">${t}</a>`)
        .replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>");
    })
    .join("");
}

const splitRow = (line: string): string[] =>
  line
    .trim()
    .replace(/^\||\|$/g, "")
    .split("|")
    .map((c) => c.trim());

// ── block level ──
function mdToHtml(src: string): string {
  const lines = src.replace(/\r\n?/g, "\n").split("\n");
  const n = lines.length;
  let html = "";
  let i = 0;

  while (i < n) {
    const line = lines[i];

    // fenced code — ```lang … ```
    const fence = line.match(/^```(\w*)\s*$/);
    if (fence) {
      const lang = fence[1];
      const code: string[] = [];
      i++;
      while (i < n && !/^```\s*$/.test(lines[i])) code.push(lines[i++]);
      i++; // closing fence
      const raw = code.join("\n");
      const body = lang === "quarc" ? highlightQuarc(raw) : esc(raw);
      html += `<pre class="guide-code${lang === "quarc" ? " quarc" : ""}"><code>${body}</code></pre>`;
      continue;
    }

    // horizontal rule (a line of only dashes; table separators carry pipes)
    if (/^---+\s*$/.test(line)) {
      html += "<hr>";
      i++;
      continue;
    }

    // ATX heading
    const h = line.match(/^(#{1,6})\s+(.*)$/);
    if (h) {
      const lv = h[1].length;
      html += `<h${lv}>${inline(h[2].trim())}</h${lv}>`;
      i++;
      continue;
    }

    // table — header row, dashed separator, then body rows
    if (/^\s*\|.*\|\s*$/.test(line) && i + 1 < n && /^\s*\|?[\s:|-]*-[\s:|-]*\|?\s*$/.test(lines[i + 1])) {
      const header = splitRow(line);
      i += 2; // header + separator
      const rows: string[][] = [];
      while (i < n && /\|/.test(lines[i]) && lines[i].trim() !== "") rows.push(splitRow(lines[i++]));
      const th = header.map((c) => `<th>${inline(c)}</th>`).join("");
      const trs = rows.map((r) => `<tr>${r.map((c) => `<td>${inline(c)}</td>`).join("")}</tr>`).join("");
      html += `<table class="guide-table"><thead><tr>${th}</tr></thead><tbody>${trs}</tbody></table>`;
      continue;
    }

    // unordered list
    if (/^\s*[-*]\s+/.test(line)) {
      const items: string[] = [];
      while (i < n && /^\s*[-*]\s+/.test(lines[i])) items.push(lines[i++].replace(/^\s*[-*]\s+/, ""));
      html += `<ul>${items.map((it) => `<li>${inline(it)}</li>`).join("")}</ul>`;
      continue;
    }

    // blank line
    if (line.trim() === "") {
      i++;
      continue;
    }

    // paragraph — accumulate until a blank line or the next block starts
    const buf: string[] = [];
    while (
      i < n &&
      lines[i].trim() !== "" &&
      !/^```/.test(lines[i]) &&
      !/^#{1,6}\s/.test(lines[i]) &&
      !/^---+\s*$/.test(lines[i]) &&
      !/^\s*[-*]\s+/.test(lines[i]) &&
      !/^\s*\|.*\|\s*$/.test(lines[i])
    ) {
      buf.push(lines[i++]);
    }
    html += `<p>${inline(buf.join(" "))}</p>`;
  }

  return html;
}
