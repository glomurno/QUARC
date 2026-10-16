// Welcome screen shown over the empty canvas until a source is loaded: open a
// file, reopen a recent one, try a bundled example, or read the syntax guide.

import { esc, setHTML } from "./dom";
import { EXAMPLES, type Example } from "./examples";
import { icon } from "./icons";
import { fileDir, fileName, recentFiles } from "./recent";

export interface WelcomeHooks {
  open(): void;
  openRecent(path: string): void;
  removeRecent(path: string): void;
  openExample(ex: Example): void;
  guide(): void;
}

const mod = navigator.platform.toLowerCase().includes("mac") ? "⌘" : "Ctrl";

export function renderWelcome(el: HTMLElement, hooks: WelcomeHooks): void {
  const recents = recentFiles();
  const recentHtml = recents.length
    ? recents
        .map(
          (p, i) => `<div class="wl-item" data-recent="${i}" title="${esc(p)}">
            <span class="ic">${icon("file")}</span>
            <span class="wl-name">${esc(fileName(p))}</span>
            <span class="wl-dir">${esc(fileDir(p))}</span>
            <button class="wl-forget" data-forget="${i}" title="Remove from recent files">${icon("x")}</button>
          </div>`,
        )
        .join("")
    : `<div class="wl-none">No recent files yet.</div>`;
  const exampleHtml = EXAMPLES.map(
    (ex, i) => `<button class="wl-example" data-example="${i}">
        <span class="wl-ex-title">${esc(ex.title)}</span>
        <span class="wl-ex-desc">${esc(ex.description)}</span>
      </button>`,
  ).join("");

  setHTML(
    el,
    `<div class="wl-card">
      <div class="wl-hero">
        <div class="wl-title">QUARC workbench</div>
        <div class="wl-sub">Typecheck, explore and compare quantum protocols written in lqCCS.</div>
        <div class="wl-actions">
          <button class="tbtn primary" data-act="open"><span class="ic">${icon("folder")}</span>Open .quarc file…<kbd>${mod}+O</kbd></button>
          <button class="tbtn" data-act="guide"><span class="ic">${icon("book-open")}</span>Syntax guide</button>
        </div>
        <div class="wl-drop">${icon("download")}<span>or drop a <code>.quarc</code> file anywhere in the window</span></div>
      </div>
      <div class="wl-cols">
        <section>
          <h5 class="label-cap">Recent</h5>
          <div class="wl-list">${recentHtml}</div>
        </section>
        <section>
          <h5 class="label-cap">Examples</h5>
          <div class="wl-examples">${exampleHtml}</div>
        </section>
      </div>
    </div>`,
  );

  el.querySelector('[data-act="open"]')?.addEventListener("click", () => hooks.open());
  el.querySelector('[data-act="guide"]')?.addEventListener("click", () => hooks.guide());
  el.querySelectorAll<HTMLElement>("[data-recent]").forEach((row) => {
    row.addEventListener("click", () => hooks.openRecent(recents[Number(row.dataset.recent)]));
  });
  el.querySelectorAll<HTMLElement>("[data-forget]").forEach((btn) => {
    btn.addEventListener("click", (e) => {
      e.stopPropagation();
      hooks.removeRecent(recents[Number(btn.dataset.forget)]);
    });
  });
  el.querySelectorAll<HTMLElement>("[data-example]").forEach((btn) => {
    btn.addEventListener("click", () => hooks.openExample(EXAMPLES[Number(btn.dataset.example)]));
  });
}
