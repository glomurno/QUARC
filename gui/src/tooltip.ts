// Custom tooltips for the chrome: a description, the keyboard shortcut, and —
// for a disabled control — why it is disabled. Native `title` tooltips can't do
// the last part: browsers (WebKitGTK in particular) don't deliver hover events
// to disabled buttons. So disabled `[data-tip]` controls get `pointer-events:
// none` (styles.css) and are found geometrically under their hovered parent.
//
// Elements opt in with `data-tip` (or a plain `title`, migrated on first hover);
// `data-key` adds a shortcut chip and `data-why` the disabled reason.

const DELAY = 380;

let tipEl: HTMLElement;
let current: HTMLElement | null = null;
let timer: number | undefined;

/** Set an element's tooltip text. */
export function setTip(el: HTMLElement, text: string): void {
  el.removeAttribute("title");
  el.dataset.tip = text;
  if (current === el) show(el);
}

/** Enable or disable a control, recording why it is unavailable for its tooltip. */
export function setAvailable(el: HTMLElement & { disabled?: boolean }, why: string | null): void {
  const off = why != null;
  if ("disabled" in el) el.disabled = off;
  else el.classList.toggle("is-disabled", off);
  if (off) el.dataset.why = why;
  else delete el.dataset.why;
  if (current === el) show(el);
}

/** Migrate a native title into data-tip so the browser tooltip doesn't double up. */
function adopt(el: HTMLElement): HTMLElement {
  if (el.title) {
    el.dataset.tip = el.title;
    el.removeAttribute("title");
  }
  return el;
}

const TIPPED = "[data-tip],[title]";

/** The tooltip owner under the pointer, including disabled (pointer-events: none) controls. */
function ownerAt(target: Element | null, x: number, y: number): HTMLElement | null {
  const direct = target?.closest<HTMLElement>(TIPPED);
  if (direct && !direct.matches("svg *, svg")) return adopt(direct);
  if (!(target instanceof HTMLElement)) return null;
  for (const el of target.querySelectorAll<HTMLElement>(":disabled[data-tip],:disabled[title],.is-disabled[data-tip]")) {
    const r = el.getBoundingClientRect();
    if (x >= r.left && x <= r.right && y >= r.top && y <= r.bottom) return adopt(el);
  }
  return null;
}

function esc(s: string): string {
  return s.replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]!);
}

function show(el: HTMLElement) {
  const tip = el.dataset.tip ?? "";
  const key = el.dataset.key;
  const disabled = (el as HTMLButtonElement).disabled || el.classList.contains("is-disabled");
  const why = disabled ? el.dataset.why : undefined;
  if (!tip && !why) return hide();
  const html =
    `<div class="tt-main">${esc(tip)}${key ? `<kbd>${esc(key)}</kbd>` : ""}</div>` +
    (why ? `<div class="tt-why">${esc(why)}</div>` : "");
  // Static markup from our own strings (escaped); no user HTML.
  tipEl.replaceChildren(document.createRange().createContextualFragment(html));
  tipEl.classList.add("show");
  const r = el.getBoundingClientRect();
  const t = tipEl.getBoundingClientRect();
  let left = r.left + r.width / 2 - t.width / 2;
  left = Math.max(8, Math.min(left, window.innerWidth - t.width - 8));
  let top = r.bottom + 8;
  if (top + t.height > window.innerHeight - 8) top = r.top - t.height - 8;
  tipEl.style.left = `${left}px`;
  tipEl.style.top = `${top}px`;
}

function hide() {
  clearTimeout(timer);
  current = null;
  tipEl?.classList.remove("show");
}

export function installTooltips(): void {
  tipEl = document.createElement("div");
  tipEl.className = "tooltip";
  tipEl.setAttribute("role", "tooltip");
  document.body.appendChild(tipEl);

  document.addEventListener("pointermove", (e) => {
    const owner = ownerAt(e.target as Element, e.clientX, e.clientY);
    if (owner === current) return;
    clearTimeout(timer);
    tipEl.classList.remove("show");
    current = owner;
    if (owner) timer = window.setTimeout(() => current === owner && show(owner), DELAY);
  });
  document.addEventListener("pointerdown", hide, true);
  document.addEventListener("keydown", hide, true);
  document.addEventListener("pointerleave", hide);
  window.addEventListener("blur", hide);
  document.addEventListener("scroll", hide, true);
}
