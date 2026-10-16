// Command registry behind the command palette (Ctrl+K), the keyboard shortcuts
// and the shortcut sheet (?). Every action is declared once with its group, key
// and an availability check that returns the reason it's unavailable, so the
// palette, the shortcuts and the button tooltips all agree on what can run.

import { esc, setHTML } from "./dom";

export interface Command {
  id: string;
  group: string;
  title: string;
  /** Extra words matched by the palette filter. */
  keywords?: string;
  /** Shortcut spec, e.g. "Mod+O", "M", "?" (see `keySpec`). */
  key?: string;
  /** Hide from the palette (shortcut-only, or covered by a dynamic entry). */
  hidden?: boolean;
  /** Shortcut-sheet row for a hidden command (e.g. one row for keys 1–9); hidden commands without it are left out. */
  sheet?: { title: string; key: string };
  /** `null` when runnable, otherwise the reason it isn't. */
  why?: () => string | null;
  /** When unavailable, let the key through (e.g. Mod+Enter to the editor) instead of reporting why. */
  fallthrough?: boolean;
  run: () => void | Promise<void>;
}

export const isMac = navigator.platform.toLowerCase().includes("mac");

/** Render a key spec ("Mod+K") for display ("Ctrl+K" / "⌘K"). */
export const showKey = (k: string): string => k.replace(/Mod\+/g, isMac ? "⌘" : "Ctrl+");

/** Normalize a keydown into the spec form used by `Command.key`. */
function keySpec(e: KeyboardEvent): string {
  const parts: string[] = [];
  if (e.ctrlKey || e.metaKey) parts.push("Mod");
  if (e.altKey) parts.push("Alt");
  let k = e.key;
  if (k.length === 1) k = k.toUpperCase();
  // Shift is implied by the produced character for symbols ("?", "["), but kept for letters/named keys.
  if (e.shiftKey && (k.length > 1 || /[A-Z]/.test(k))) parts.push("Shift");
  parts.push(k === " " ? "Space" : k);
  return parts.join("+");
}

/** Focus is in a text field or the code editor: plain-key shortcuts must not fire. */
function typing(e: KeyboardEvent): boolean {
  const t = e.target as HTMLElement | null;
  if (!t) return false;
  return (
    t.isContentEditable ||
    t.closest(".cm-editor") != null ||
    t instanceof HTMLInputElement ||
    t instanceof HTMLTextAreaElement ||
    t instanceof HTMLSelectElement
  );
}

export class Commands {
  private list: Command[] = [];
  private byKey = new Map<string, Command>();

  add(...cmds: Command[]): void {
    for (const c of cmds) {
      this.list.push(c);
      if (c.key) this.byKey.set(c.key, c);
    }
  }

  get(id: string): Command | undefined {
    return this.list.find((c) => c.id === id);
  }

  all(): Command[] {
    return this.list;
  }

  /** Run if available; otherwise report the reason via `onBlocked`. */
  invoke(c: Command, onBlocked: (why: string) => void): void {
    const why = c.why?.() ?? null;
    if (why) onBlocked(why);
    else void c.run();
  }

  /** Global shortcut dispatch. Modifier combos work everywhere (even in the
   * editor); single keys only when not typing and no dialog is open. Runs in
   * the capture phase so CodeMirror and the webview don't see claimed combos. */
  install(onBlocked: (why: string) => void, isModal: () => boolean): void {
    window.addEventListener(
      "keydown",
      (e) => {
        if (e.isComposing) return;
        const spec = keySpec(e);
        const c = this.byKey.get(spec);
        if (!c) return;
        const mod = spec.startsWith("Mod+");
        if (!mod && (typing(e) || isModal())) return;
        if (mod && isModal() && c.id !== "palette") return;
        if (c.fallthrough && c.why?.()) return;
        e.preventDefault();
        e.stopPropagation();
        if (e.repeat) return;
        this.invoke(c, onBlocked);
      },
      true,
    );
  }
}

/** Write each command's shortcut onto its `[data-cmd]` control for the tooltip. */
export function annotateKeys(cmds: Commands, root: ParentNode = document): void {
  for (const el of root.querySelectorAll<HTMLElement>("[data-cmd]")) {
    const c = cmds.get(el.dataset.cmd!);
    if (c?.key) el.dataset.key = showKey(c.key);
  }
}

// ─── palette ──────────────────────────────────────────────────────────────────

/** Fuzzy score (higher is better), or -1 when `q` isn't a subsequence of `text`. */
function score(q: string, text: string): number {
  if (!q) return 0;
  const t = text.toLowerCase();
  const direct = t.indexOf(q);
  if (direct >= 0) return 1000 - direct + (direct === 0 || t[direct - 1] === " " ? 50 : 0);
  let ti = 0;
  let s = 0;
  let run = 0;
  for (const ch of q) {
    const at = t.indexOf(ch, ti);
    if (at < 0) return -1;
    run = at === ti ? run + 1 : 0;
    s += 10 + run * 5 - (at - ti);
    ti = at + 1;
  }
  return s;
}

export interface PaletteEls {
  overlay: HTMLElement;
  input: HTMLInputElement;
  list: HTMLElement;
}

export class Palette {
  private items: Command[] = [];
  private shown: Command[] = [];
  private sel = 0;

  constructor(
    private els: PaletteEls,
    /** Commands plus dynamic entries (configs, recents, examples), built per open. */
    private source: () => Command[],
    private onBlocked: (why: string) => void,
  ) {
    els.input.addEventListener("input", () => this.filter());
    els.input.addEventListener("keydown", (e) => this.onKey(e));
    els.overlay.addEventListener("mousedown", (e) => {
      if (e.target === els.overlay) this.close();
    });
  }

  get isOpen(): boolean {
    return !this.els.overlay.classList.contains("hidden");
  }

  open(query = ""): void {
    this.items = this.source().filter((c) => !c.hidden);
    this.els.overlay.classList.remove("hidden");
    this.els.input.value = query;
    this.filter();
    this.els.input.focus();
  }

  close(): void {
    this.els.overlay.classList.add("hidden");
  }

  private filter() {
    const q = this.els.input.value.trim().toLowerCase();
    const scored = this.items
      .map((c, i) => ({ c, i, s: score(q, `${c.group} ${c.title} ${c.keywords ?? ""}`) }))
      .filter((x) => x.s >= 0);
    // With a query: runnable before unavailable, then best match. Without: declared order.
    if (q) scored.sort((a, b) => Number(!!a.c.why?.()) - Number(!!b.c.why?.()) || b.s - a.s || a.i - b.i);
    this.shown = scored.map((x) => x.c);
    this.sel = 0;
    this.render();
  }

  private render() {
    if (!this.shown.length) {
      setHTML(this.els.list, `<div class="pal-none">No matching commands.</div>`);
      return;
    }
    let lastGroup = "";
    const q = this.els.input.value.trim();
    const rows = this.shown
      .map((c, i) => {
        const why = c.why?.() ?? null;
        const head = !q && c.group !== lastGroup ? `<div class="pal-group">${esc(c.group)}</div>` : "";
        lastGroup = c.group;
        return `${head}<div class="pal-item${i === this.sel ? " sel" : ""}${why ? " off" : ""}" data-i="${i}">
          <span class="pal-title">${q ? `<span class="pal-g">${esc(c.group)} ›</span> ` : ""}${esc(c.title)}</span>
          ${why ? `<span class="pal-why">${esc(why)}</span>` : ""}
          ${c.key ? `<kbd>${esc(showKey(c.key))}</kbd>` : ""}
        </div>`;
      })
      .join("");
    setHTML(this.els.list, rows);
    this.els.list.querySelectorAll<HTMLElement>(".pal-item").forEach((row) => {
      row.addEventListener("mousemove", () => this.select(Number(row.dataset.i), false));
      row.addEventListener("click", () => this.runAt(Number(row.dataset.i)));
    });
  }

  private select(i: number, scroll = true) {
    if (!this.shown.length) return;
    const next = (i + this.shown.length) % this.shown.length;
    if (next === this.sel) return;
    this.sel = next;
    this.els.list.querySelectorAll(".pal-item").forEach((r, j) => r.classList.toggle("sel", j === this.sel));
    if (scroll) this.els.list.querySelector(".pal-item.sel")?.scrollIntoView({ block: "nearest" });
  }

  private runAt(i: number) {
    const c = this.shown[i];
    if (!c) return;
    const why = c.why?.() ?? null;
    if (why) {
      this.onBlocked(why);
      return;
    }
    this.close();
    void c.run();
  }

  private onKey(e: KeyboardEvent) {
    if (e.key === "ArrowDown") this.select(this.sel + 1);
    else if (e.key === "ArrowUp") this.select(this.sel - 1);
    else if (e.key === "Enter") this.runAt(this.sel);
    else if (e.key === "Escape") this.close();
    else return;
    e.preventDefault();
  }
}

// ─── shortcut sheet ──────────────────────────────────────────────────────────

export function renderShortcutSheet(el: HTMLElement, cmds: Commands): void {
  const groups = new Map<string, Command[]>();
  for (const c of cmds.all()) {
    if (!c.key || (c.hidden && !c.sheet)) continue;
    const row = c.sheet ? { ...c, ...c.sheet } : c;
    groups.set(c.group, [...(groups.get(c.group) ?? []), row]);
  }
  const html = [...groups]
    .map(
      ([g, cs]) => `<section><h5 class="label-cap">${esc(g)}</h5>${cs
        .map((c) => `<div class="ks-row"><span>${esc(c.title)}</span><kbd>${esc(showKey(c.key!))}</kbd></div>`)
        .join("")}</section>`,
    )
    .join("");
  setHTML(
    el,
    `<div class="ks-grid">${html}</div><p class="ks-foot">Single-key shortcuts are ignored while typing in the editor or a field.
     <kbd>${esc(showKey("Mod+K"))}</kbd> finds any command by name.</p>`,
  );
}
