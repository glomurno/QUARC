// Silent-τ picker: which τ tags are silent under branching bisimilarity. The
// engine lists every τ tag of the LTS (`tauTags`) with its tag-equivalence class
// and current silence; tags in one class are silent together, so the picker
// shows one row per class. A selection is sent back as a `silence` override that
// replaces the file's `silent`/`visible` blocks, and can be copied as a block.

import type { SilenceSpec, SourceSpan, TagRef, TauTagInfo } from "./contract";
import { esc, setHTML } from "./dom";

/** Stable key for a tag, shared with the graph's edge highlighting. */
export const tagKey = (t: TagRef): string => (typeof t === "number" ? `${t}` : `${t[0]},${t[1]}`);

const tagText = (t: TagRef): string => (typeof t === "number" ? `t${t}` : `(t${t[0]}, t${t[1]})`);
const tagQuarc = (t: TagRef): string => (typeof t === "number" ? `${t}` : `(${t[0]}, ${t[1]})`);

/** The τ tags of one tag-equivalence class. */
export interface TagGroup {
  key: string;
  tags: TauTagInfo[];
  silent: boolean;
}

export function groupTauTags(tags: TauTagInfo[]): TagGroup[] {
  const groups = new Map<string, TagGroup>();
  for (const t of tags) {
    const key = tagKey(t.norm);
    const g = groups.get(key) ?? { key, tags: [], silent: t.silent };
    g.tags.push(t);
    groups.set(key, g);
  }
  return [...groups.values()];
}

/** Keys of every tag in a group plus its class representative, which labels
 * quotient edges (for edge highlighting). */
export const groupTagKeys = (g: TagGroup): Set<string> => new Set([g.key, ...g.tags.map((t) => tagKey(t.tag))]);

/** Override making exactly the groups for which `silent` holds silent. */
export function overrideFor(groups: TagGroup[], silent: (g: TagGroup) => boolean): SilenceSpec {
  return { silent: [], visible: groups.filter((g) => !silent(g)).flatMap((g) => g.tags.map((t) => t.tag)) };
}

/** The shorter of a `silent` or `visible` block reproducing the selection.
 * `scope` names the compared start blocks for a scoped block. */
export function silenceBlock(groups: TagGroup[], scope?: [string, string]): string {
  const on = groups.filter((g) => g.silent);
  const off = groups.filter((g) => !g.silent);
  if (off.length === 0) return "-- every τ tag is silent (the default)";
  const sc = scope ? ` "${scope[0]}" "${scope[1]}"` : "";
  const list = (gs: TagGroup[]) => gs.flatMap((g) => g.tags.map((t) => tagQuarc(t.tag))).join(" ");
  // `silent {}` would mean "all silent", so an empty silent set needs `visible`.
  return on.length > 0 && on.length < off.length ? `silent${sc} { ${list(on)} }` : `visible${sc} { ${list(off)} }`;
}

export interface SilencePanelHooks {
  onToggle(g: TagGroup): void;
  onHover(g: TagGroup | null): void;
  onReveal(span: SourceSpan): void;
  onAll(): void;
  onNone(): void;
  onReset(): void;
  onCopy(): void;
}

export function renderSilencePanel(
  el: HTMLElement,
  groups: TagGroup[],
  overridden: boolean,
  hooks: SilencePanelHooks,
) {
  const rows = groups
    .map((g, i) => {
      const label = g.tags.map((t) => tagText(t.tag)).join(" ~ ");
      const loc = g.tags.find((t) => t.loc)?.loc;
      const where = loc ? `<button class="sil-loc" data-loc="${i}" title="Show in editor">line ${loc.start.line}</button>` : "";
      return `<div class="pset sil-row" data-row="${i}">
        <div><div class="sil-tag">${esc(label)}</div>${where}</div>
        <div class="sw-toggle${g.silent ? " on" : ""}"><i></i></div>
      </div>`;
    })
    .join("");
  setHTML(
    el,
    `<h5>Silent τ tags</h5>
     <div class="sil-desc">Silent steps may be skipped by branching bisimilarity; the others count as visible actions.${overridden ? " <b>Edited</b> — not saved to the file." : ""}</div>
     <div class="sil-actions">
       <button class="mini-btn" data-act="all">all</button>
       <button class="mini-btn" data-act="none">none</button>
       <button class="mini-btn" data-act="reset" ${overridden ? "" : "disabled"} title="Back to the file's silent/visible blocks">file</button>
       <button class="mini-btn" data-act="copy" title="Copy a silent/visible block for the .quarc file">copy block</button>
     </div>
     <div class="sil-list">${rows || `<div class="sil-desc">No τ steps in this graph.</div>`}</div>`,
  );
  el.querySelectorAll<HTMLElement>(".sil-row").forEach((row) => {
    const g = groups[+row.dataset.row!];
    row.addEventListener("click", () => hooks.onToggle(g));
    row.addEventListener("mouseenter", () => hooks.onHover(g));
    row.addEventListener("mouseleave", () => hooks.onHover(null));
  });
  el.querySelectorAll<HTMLElement>(".sil-loc").forEach((b) =>
    b.addEventListener("click", (e) => {
      e.stopPropagation();
      const loc = groups[+b.dataset.loc!].tags.find((t) => t.loc)?.loc;
      if (loc) hooks.onReveal(loc);
    }),
  );
  const act: Record<string, () => void> = {
    all: hooks.onAll,
    none: hooks.onNone,
    reset: hooks.onReset,
    copy: hooks.onCopy,
  };
  el.querySelectorAll<HTMLElement>("[data-act]").forEach((b) => b.addEventListener("click", () => act[b.dataset.act!]()));
}
