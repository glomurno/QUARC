// Draggable pane splitters: the Sources | canvas | Inspector columns and the
// canvas / editor rows. Sizes live in CSS custom properties (`--left-w`,
// `--right-w` on #main, `--editor-h` on #center), persist in localStorage, and a
// double-click on a handle restores the default size.

interface SplitSpec {
  handle: HTMLElement;
  /** Element holding the CSS variable. */
  host: HTMLElement;
  cssVar: string;
  storageKey: string;
  /** New size in px from the pointer event, before clamping. */
  sizeFrom(e: PointerEvent): number;
  /** [min, max] allowed size in px, evaluated at drag time (depends on the window). */
  bounds(): [number, number];
  onStart?(): void;
}

const clamp = (v: number, [lo, hi]: [number, number]) => Math.min(Math.max(v, lo), Math.max(lo, hi));

function load(key: string): number | null {
  try {
    const v = Number(localStorage.getItem(key));
    return Number.isFinite(v) && v > 0 ? v : null;
  } catch {
    return null;
  }
}

function save(key: string, v: number | null) {
  try {
    if (v == null) localStorage.removeItem(key);
    else localStorage.setItem(key, String(Math.round(v)));
  } catch {
    /* storage unavailable: sizes just won't persist */
  }
}

function makeSplitter(s: SplitSpec) {
  const apply = (px: number | null) => {
    if (px == null) s.host.style.removeProperty(s.cssVar);
    else s.host.style.setProperty(s.cssVar, `${px}px`);
  };
  const stored = load(s.storageKey);
  if (stored != null) apply(clamp(stored, s.bounds()));

  let current: number | null = stored;
  let dragging = false;
  s.handle.addEventListener("pointerdown", (e) => {
    if (e.button !== 0) return;
    e.preventDefault();
    e.stopPropagation();
    s.onStart?.();
    dragging = true;
    try {
      s.handle.setPointerCapture(e.pointerId);
    } catch {
      /* no active pointer (synthetic event): moves still reach the handle */
    }
    document.body.classList.add("resizing", s.handle.dataset.axis === "y" ? "resizing-y" : "resizing-x");
    s.handle.classList.add("dragging");
  });
  s.handle.addEventListener("pointermove", (e) => {
    if (!dragging) return;
    current = clamp(s.sizeFrom(e), s.bounds());
    apply(current);
  });
  const end = (e: PointerEvent) => {
    if (!dragging) return;
    dragging = false;
    if (s.handle.hasPointerCapture(e.pointerId)) s.handle.releasePointerCapture(e.pointerId);
    document.body.classList.remove("resizing", "resizing-x", "resizing-y");
    s.handle.classList.remove("dragging");
    save(s.storageKey, current);
  };
  s.handle.addEventListener("pointerup", end);
  s.handle.addEventListener("pointercancel", end);
  s.handle.addEventListener("dblclick", (e) => {
    e.stopPropagation();
    current = null;
    apply(null);
    save(s.storageKey, null);
  });
  // Clicks on the editor handle must not toggle the editor rail underneath.
  s.handle.addEventListener("click", (e) => e.stopPropagation());
}

export function installSplitters(opts: {
  main: HTMLElement;
  center: HTMLElement;
  left: HTMLElement;
  right: HTMLElement;
  /** Called when the editor handle is dragged while the editor is collapsed. */
  openEditor(): void;
}): void {
  const { main, center } = opts;
  const mk = (parent: HTMLElement, cls: string, axis: "x" | "y", title: string) => {
    const h = document.createElement("div");
    h.className = `splitter ${cls}`;
    h.dataset.axis = axis;
    h.title = title;
    parent.appendChild(h);
    return h;
  };

  makeSplitter({
    handle: mk(opts.left, "split-left", "x", "Drag to resize · double-click to reset"),
    host: main,
    cssVar: "--left-w",
    storageKey: "quarc-left-w",
    sizeFrom: (e) => e.clientX - main.getBoundingClientRect().left,
    bounds: () => [160, Math.min(520, main.clientWidth * 0.4)],
  });
  makeSplitter({
    handle: mk(opts.right, "split-right", "x", "Drag to resize · double-click to reset"),
    host: main,
    cssVar: "--right-w",
    storageKey: "quarc-right-w",
    sizeFrom: (e) => main.getBoundingClientRect().right - e.clientX,
    bounds: () => [240, Math.min(720, main.clientWidth * 0.5)],
  });
  makeSplitter({
    handle: mk($editorRail(center), "split-editor", "y", "Drag to resize the editor · double-click to reset"),
    host: center,
    cssVar: "--editor-h",
    storageKey: "quarc-editor-h",
    sizeFrom: (e) => center.getBoundingClientRect().bottom - e.clientY,
    bounds: () => [120, center.clientHeight - 120],
    onStart: () => {
      if (!center.classList.contains("editor-open")) opts.openEditor();
    },
  });
}

function $editorRail(center: HTMLElement): HTMLElement {
  const el = center.querySelector<HTMLElement>("#editorRail");
  if (!el) throw new Error("missing #editorRail");
  return el;
}
