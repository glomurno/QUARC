// Left-pane Sources/Configs panel. Lists a loaded file's start-blocks (configs,
// from the `outline` RPC) and reports which one to evaluate. The unnamed/default
// block (`null`) renders as ⟨default⟩ and is evaluated with no config arg.

import { esc, setHTML } from "./dom";

export type ConfigPick = string | null;

/** Render the file as a tree node with its configs nested; `onPick` fires on
 * click with the chosen config (`null` = the default/unnamed block). */
export function renderSources(
  el: HTMLElement,
  file: string,
  configs: ConfigPick[],
  onPick: (config: ConfigPick) => void,
): void {
  const name = file.split(/[\\/]/).pop() ?? file;
  if (configs.length === 0) {
    setHTML(el, `<div class="tfile"><div class="row"><span class="fname">${esc(name)}</span></div></div>
      <div class="src-empty">No start blocks.</div>`);
    return;
  }
  const items = configs
    .map((c, i) =>
      c == null
        ? `<button class="tconfig" data-i="${i}"><span class="default">⟨default⟩</span></button>`
        : `<button class="tconfig" data-i="${i}">${esc(c)}</button>`,
    )
    .join("");
  setHTML(
    el,
    `<div class="tfile">
      <div class="row"><span class="fname">${esc(name)}</span><span class="badge">${configs.length} cfg</span></div>
      <div class="tconfigs">${items}</div>
    </div>`,
  );

  const buttons = Array.from(el.querySelectorAll<HTMLButtonElement>(".tconfig"));
  buttons.forEach((btn) => {
    btn.addEventListener("click", () => {
      buttons.forEach((b) => b.classList.remove("active"));
      btn.classList.add("active");
      onPick(configs[Number(btn.dataset.i)]);
    });
  });
}

/** Highlight the active config by index (used when main auto-evals one). */
export function markActive(el: HTMLElement, index: number): void {
  el.querySelectorAll<HTMLButtonElement>(".tconfig").forEach((b) => {
    b.classList.toggle("active", Number(b.dataset.i) === index);
  });
}

/** Show (or remove) the "start blocks changed" notice above the config list. */
export function setConfigsStale(el: HTMLElement, stale: boolean, onRefresh: () => void): void {
  el.querySelector(".src-stale")?.remove();
  if (!stale) return;
  const note = document.createElement("button");
  note.className = "src-stale";
  note.title = "Re-read the start blocks from the editor buffer (your edits are kept)";
  setHTML(note, `<span class="dot"></span><span>Start blocks changed in the editor.</span><b>Refresh</b>`);
  note.addEventListener("click", onRefresh);
  el.prepend(note);
}
