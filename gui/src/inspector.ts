// Right-pane inspector: node summary (eager) + quantum distribution matrices
// (lazy via getState). Matrices render as a phase/magnitude heatmap (default)
// or precise KaTeX; ≥5 qubits are heatmap-only with KaTeX offered export-only.
// Each ρ can be exported as a QUARC `state … := density … dm { }` def (§7.4).

import katex from "katex";
import "katex/dist/katex.min.css";
import type { ComplexMatrix, NodeDetail, NodeSummary, SourceSpan } from "./contract";
import type { Divergence } from "./divergence";
import { esc, setHTML } from "./dom";
import { icon } from "./icons";
import { toDensitySnippet, toLatex } from "./matrix";
import { renderProc } from "./proc";
import { getState } from "./rpc";
import { classColor } from "./theme";

// dim ≤ 16 (≤4 qubits) renders inline as KaTeX; larger is heatmap-only.
const KATEX_MAX_DIM = 16;

/** Side-effecting hooks the inspector needs from the app shell. */
export interface InspectorDeps {
  /** Insert a top-level QUARC definition into the editor buffer. */
  insertDef(text: string): void;
  /** Reveal a source span in the editor. */
  jump(span: SourceSpan): void;
  /** Briefly flash a status message. */
  toast(msg: string): void;
  /** Whether the buffer already defines a `state` with this name. */
  stateExists(name: string): boolean;
}

const mag = (c: [number, number]) => Math.hypot(c[0], c[1]);

function fmtC(c: [number, number]): string {
  const r = +c[0].toFixed(2);
  const i = +c[1].toFixed(2);
  if (Math.abs(i) < 1e-9) return `${r}`;
  if (Math.abs(r) < 1e-9) return `${i}i`;
  return `${r}${i < 0 ? "−" : "+"}${Math.abs(i)}i`;
}

// ─── matrix views ────────────────────────────────────────────────────────────

let matrixView: "heat" | "katex" = "heat";

/** Inline view: heatmap always; KaTeX only when small enough. */
function renderMatrix(m: ComplexMatrix): string {
  return matrixView === "katex" && m.cols <= KATEX_MAX_DIM ? renderKatex(m) : renderHeat(m);
}

// magnitude → lightness, PHASE → hue (so Φ⁺ vs Φ⁻ are visibly distinct).
export function renderHeat(m: ComplexMatrix): string {
  const cs = m.cols <= 2 ? 30 : m.cols <= 4 ? 24 : m.cols <= 8 ? 16 : m.cols <= 16 ? 11 : 7;
  const showNums = cs >= 16;
  const light = document.documentElement.dataset.theme === "light";
  let cells = "";
  for (let k = 0; k < m.rows * m.cols; k++) {
    const c = m.data[k];
    const v = mag(c);
    const hue = (((Math.atan2(c[1], c[0]) / (2 * Math.PI)) * 360) + 360) % 360;
    // dark: lift blue-ish hues that read dark at equal lightness; light: no lift.
    const blueLift = light ? 0 : 8 * Math.max(0, Math.cos(((hue - 240) * Math.PI) / 180));
    const L = light ? 26 + Math.sqrt(v) * 36 : 34 + Math.sqrt(v) * 48 + blueLift;
    const base = v < 1e-9 ? "var(--bg-grid)" : `hsl(${hue.toFixed(0)} 85% ${L.toFixed(0)}%)`;
    const num = showNums && v > 0.04 ? `<span>${+v.toFixed(2)}</span>` : "";
    cells += `<div class="cell${v < 1e-9 ? " zero" : ""}" style="background:${base}" title="ρ=${esc(fmtC(c))}  |ρ|=${v.toFixed(2)}">${num}</div>`;
  }
  return `<div class="heat-wrap"><div class="heat" style="grid-template-columns:repeat(${m.cols},1fr);--cs:${cs}px">${cells}</div></div>
    <div class="heat-legend">
      <span>mag</span><div class="heat-scale"></div>
      <span style="margin-left:6px">phase</span><div class="phase-wheel"></div>
      <span class="mono">0…2π</span>
    </div>`;
}

function renderKatex(m: ComplexMatrix): string {
  const tex = toLatex(m);
  const id = "kx" + Math.random().toString(36).slice(2);
  // Host is in the string we're about to inject; render once it's in the DOM.
  setTimeout(() => {
    const el = document.getElementById(id);
    if (!el) return;
    try {
      katex.render(tex, el, { throwOnError: false, displayMode: true });
    } catch {
      el.textContent = tex;
    }
  }, 0);
  return `<div class="katex-host" id="${id}"></div>`;
}

// ─── inspector states ────────────────────────────────────────────────────────

export interface InspectorEls {
  title: HTMLElement;
  sub: HTMLElement;
  body: HTMLElement;
}

export function renderEmpty(els: InspectorEls) {
  current = null;
  els.title.textContent = "Inspector";
  els.sub.textContent = "";
  setHTML(els.body, `<div class="insp-empty"><div class="big">${icon("target")}</div>Select a state in the graph<br>to inspect its quantum distribution.</div>`);
}

/** Human-readable name of an equiv `bisimType`, e.g. `rooted-branching-tag` →
 * `rooted branching + tag`, `branching-angelic` → `branching + angelic tags`;
 * the bare `tag` is strong with tag equivalences. */
export function bisimTypeLabel(bisimType: string): string {
  if (bisimType === "tag") return "strong + tag";
  const m = /^(.*?)(-tag|-angelic)?$/.exec(bisimType)!;
  const base = m[1].replace(/-/g, " ");
  return base + (m[2] === "-tag" ? " + tag" : m[2] === "-angelic" ? " + angelic tags" : "");
}

export interface VerdictInfo {
  bisimilar: boolean;
  bisimType: string;
  /** Pane names (config names) of side 0 and side 1. */
  names: [string, string];
  div: Divergence | null;
  /** Matched states are currently dimmed. */
  dimmed: boolean;
}

export interface VerdictHooks {
  /** Select a state by id and bring it into view. */
  reveal(id: number): void;
  /** Emphasize a side's path to its first unmatched state. */
  showPath(origin: number): void;
  toggleDim(): void;
}

const classChip = (c: number | undefined) =>
  c == null ? "" : `<span class="chip" style="border-color:${classColor(c)};color:${classColor(c)}">${c}</span>`;
const stateLink = (n: NodeSummary) => `<button class="state-link" data-node="${n.id}">s${n.id}</button>`;

/** Compare verdict + where the two sides diverge (§6), from the class partition. */
export function renderVerdict(els: InspectorEls, info: VerdictInfo, hooks: VerdictHooks) {
  current = null;
  const { bisimilar, bisimType, names, div } = info;
  els.title.textContent = "Comparison";
  els.sub.textContent = bisimTypeLabel(bisimType);
  const verdict = bisimilar
    ? `<div class="verdict-line ok">≈ bisimilar</div>`
    : `<div class="verdict-line no">≉ not bisimilar</div>`;
  let body = `<div class="insp-sec">${verdict}<div class="cap"><b>${esc(names[0])}</b> vs <b>${esc(names[1])}</b> · ${esc(
    bisimTypeLabel(bisimType),
  )} bisimilarity · hover a state to light up its class in both panes</div></div>`;

  if (div) {
    const [a, b] = div.sides;
    const initRow = (s: typeof a, name: string) =>
      s.initial ? `<div class="k">${esc(name)}</div><div class="v">${stateLink(s.initial)} ${classChip(s.initial.classId)}</div>` : "";
    body += `<div class="insp-sec"><h4>Initial states</h4><div class="kv">${initRow(a, names[0])}${initRow(b, names[1])}</div>
      <div class="cap">${div.initialsMatch ? "Same class: they are equivalent." : "Different classes: they are not equivalent."}</div></div>`;

    if (!bisimilar) {
      const firsts = [a, b].filter((s) => s.first);
      let where = "";
      if (firsts.length) {
        where = firsts
          .map((s) => {
            const f = s.first!;
            const me = names[s.origin];
            const them = names[1 - s.origin];
            const steps = f.path.length
              ? `<ol class="trace">${f.path
                  .map((st) => `<li><span class="act">${esc(st.label)}</span> → <button class="state-link" data-node="${st.to}">s${st.to}</button></li>`)
                  .join("")}</ol>`
              : "";
            return `<div class="div-side">
              <div><b>${esc(me)}</b> ${f.path.length ? `reaches ${stateLink(f.node)} after ${f.path.length} step${f.path.length > 1 ? "s" : ""}` : `starts in ${stateLink(f.node)}`};
              no state of <b>${esc(them)}</b> is equivalent to it ${classChip(f.node.classId)}.</div>
              ${steps}
              <button class="mini-btn" data-path="${s.origin}">${icon("target")}<span>show path</span></button>
            </div>`;
          })
          .join("");
      } else {
        // Every class occurs on both sides: the initial states just sit in different ones.
        const peer = (s: typeof a) => s.initialPeers[0];
        where = [a, b]
          .filter((s) => s.initial && peer(s))
          .map(
            (s) =>
              `<div class="div-side"><b>${esc(names[s.origin])}</b>'s initial state behaves like ${stateLink(peer(s))} of <b>${esc(
                names[1 - s.origin],
              )}</b>, which is not its initial state.</div>`,
          )
          .join("");
        where ||= `<div class="cap">Every class has states on both sides; the initial states are in different classes.</div>`;
      }
      body += `<div class="insp-sec"><h4>First divergence</h4>${where}
        <div class="cap">A run of one side ending in a state the other side can't match. Not a minimal distinguishing trace.</div></div>`;

      const listSide = (s: typeof a) => {
        if (!s.unmatchedCount) return "";
        const MAX_ROWS = 8;
        const more = s.unmatched.length - MAX_ROWS;
        const rows = s.unmatched
          .slice(0, MAX_ROWS)
          .map((c) => {
            const n = c.nodes[0];
            return `<div class="um-row">${classChip(c.classId)}<span>${c.nodes.length} state${c.nodes.length > 1 ? "s" : ""}</span><span class="um-near">nearest ${stateLink(n)}</span></div>`;
          })
          .join("") + (more > 0 ? `<div class="cap um-more">+ ${more} more class${more > 1 ? "es" : ""}, farther from the initial state</div>` : "");
        return `<div class="um-side"><div class="cap"><b>${s.unmatchedCount}</b> state${s.unmatchedCount > 1 ? "s" : ""} of <b>${esc(names[s.origin])}</b> in ${s.unmatched.length} class${
          s.unmatched.length > 1 ? "es" : ""
        } with no equivalent in <b>${esc(names[1 - s.origin])}</b></div>${rows}</div>`;
      };
      const lists = listSide(a) + listSide(b);
      body += `<div class="insp-sec"><h4>Unmatched states</h4>${
        lists || `<div class="cap">None: every state has an equivalent on the other side.</div>`
      }
        ${lists ? `<div class="pset" data-dim="1"><div><div>dim matched states</div><div class="desc">grey out everything the other side can match</div></div><div class="sw-toggle${info.dimmed ? " on" : ""}"><i></i></div></div>` : ""}
      </div>`;
    } else {
      body += `<div class="insp-sec"><div class="cap">Every state has an equivalent on the other side.</div></div>`;
    }
  }
  setHTML(els.body, body);

  els.body.querySelectorAll<HTMLElement>(".state-link").forEach((el) => {
    el.addEventListener("click", () => hooks.reveal(Number(el.dataset.node)));
  });
  els.body.querySelectorAll<HTMLElement>("[data-path]").forEach((el) => {
    el.addEventListener("click", () => hooks.showPath(Number(el.dataset.path)));
  });
  els.body.querySelector("[data-dim]")?.addEventListener("click", () => hooks.toggleDim());
}

// QUARC identifiers (Parser.Internal.pIdent): a letter, then letters, digits or `_`.
const IDENT = /^\p{L}[\p{L}\p{N}_]*$/u;

/** Inline "name this state" form under a branch's action row (replaces a native
 * prompt, which the desktop webview doesn't reliably support). */
function openInsertForm(row: HTMLElement, suggested: string, onInsert: (name: string) => void, deps: InspectorDeps) {
  const existing = row.nextElementSibling;
  if (existing?.classList.contains("insert-form")) {
    existing.querySelector("input")?.focus();
    return;
  }
  const form = document.createElement("form");
  form.className = "insert-form";
  setHTML(
    form,
    `<div class="cap">state name</div>
     <div class="if-row">
       <input type="text" spellcheck="false" autocomplete="off" />
       <button type="submit" class="mini-btn primary">insert</button>
       <button type="button" class="mini-btn" data-cancel>cancel</button>
     </div>
     <div class="if-msg"></div>`,
  );
  const input = form.querySelector("input")!;
  const submit = form.querySelector<HTMLButtonElement>('[type="submit"]')!;
  const msg = form.querySelector<HTMLElement>(".if-msg")!;
  input.value = suggested;
  const check = (): string | null => {
    const name = input.value.trim();
    if (!name) return "Enter a name.";
    if (!IDENT.test(name)) return "Use a letter, then letters, digits or _.";
    if (deps.stateExists(name)) return `A state named ${name} already exists in the file.`;
    return null;
  };
  const validate = () => {
    const err = check();
    msg.textContent = err ?? "";
    form.classList.toggle("invalid", err != null);
    submit.disabled = err != null;
  };
  const close = () => form.remove();
  input.addEventListener("input", validate);
  input.addEventListener("keydown", (e) => {
    if (e.key === "Escape") {
      e.preventDefault();
      e.stopPropagation(); // don't also cancel a running job / close popovers
      close();
    }
  });
  form.querySelector("[data-cancel]")!.addEventListener("click", close);
  form.addEventListener("submit", (e) => {
    e.preventDefault();
    if (check()) return validate();
    onInsert(input.value.trim());
    close();
  });
  row.after(form);
  validate();
  input.focus();
  input.select();
}

// Cached so the heatmap/KaTeX toggle re-paints without re-fetching the matrices.
interface Current {
  els: InspectorEls;
  node: NodeSummary;
  detail: NodeDetail;
  deps: InspectorDeps;
}
let current: Current | null = null;

export async function renderNode(els: InspectorEls, handle: number, node: NodeSummary, deps: InspectorDeps) {
  els.title.textContent = `State s${node.id}`;
  els.sub.textContent = node.initial ? "initial" : node.classId != null ? `class ${node.classId}` : "";

  setHTML(els.body, summarySection(node) + `<div class="insp-sec" id="qdist"><h4>Quantum distribution</h4><div class="loading">fetching matrices…</div></div>`);
  wireSummary(els, node, deps);

  try {
    const detail = await getState(handle, node.id);
    current = { els, node, detail, deps };
    paintDist();
  } catch (e) {
    const q = els.body.querySelector("#qdist");
    const msg = (e as { message?: string })?.message ?? String(e);
    if (q) setHTML(q, `<div class="error">getState failed: ${esc(msg)}</div>`);
  }
}

function summarySection(node: NodeSummary): string {
  const qubits = Math.log2(node.dim) | 0;
  const fileName = node.loc ? node.loc.file.split(/[\\/]/).pop() ?? "" : null;
  const chip =
    node.classId != null
      ? `<span class="chip" style="border-color:${classColor(node.classId)};color:${classColor(node.classId)}">${node.classId}</span>`
      : "";
  return `<div class="insp-sec">
      <h4>Summary</h4>
      <div class="kv">
        <div class="k">Hilbert dim</div><div class="v">${node.dim} · ${qubits}q</div>
        <div class="k">branches</div><div class="v">${node.branches}</div>
        ${node.classId != null ? `<div class="k">class</div><div class="v">${chip}</div>` : ""}
        ${node.origin != null ? `<div class="k">origin</div><div class="v">${node.origin === 0 ? "⟨main⟩" : "⟨spec⟩"}</div>` : ""}
        ${fileName != null ? `<div class="k">source</div><div class="v src-jump" data-jump="1">${esc(fileName)}:${node.loc!.start.line}</div>` : ""}
      </div>
      <div class="proc-wrap"><div class="cap">process</div><div class="proc-box">${renderProc(node.procShort)}</div></div>
    </div>`;
}

function wireSummary(els: InspectorEls, node: NodeSummary, deps: InspectorDeps) {
  const jump = els.body.querySelector<HTMLElement>("[data-jump]");
  if (jump && node.loc) jump.onclick = () => deps.jump(node.loc!);
}

/** Paint the quantum-distribution section from the cached detail. */
function paintDist() {
  if (!current) return;
  const { els, node, detail } = current;
  const q = els.body.querySelector("#qdist");
  if (!q) return;
  const bigMat = node.dim > KATEX_MAX_DIM; // 5+ qubits → KaTeX export-only

  const toggle = `<div class="mtoggle" id="mtog">
      <button class="${matrixView === "heat" || bigMat ? "on" : ""}" data-mv="heat">heatmap</button>
      ${bigMat
      ? `<button disabled title="≥5 qubits — KaTeX export only">KaTeX ${icon("download")}</button>`
      : `<button class="${matrixView === "katex" ? "on" : ""}" data-mv="katex">KaTeX</button>`}
    </div>
    ${bigMat ? `<div class="cap warn">dim ${node.dim} (${Math.log2(node.dim) | 0} qubits) — inline KaTeX disabled; export only</div>` : ""}`;

  const branches = detail.dist
    .map(
      (sub, i) => `<div class="substate">
        <div class="sub-head">
          <span class="cap">branch ${i}</span>
          <div class="prob-bar"><i style="width:${(sub.prob * 100).toFixed(0)}%"></i></div>
          <span class="prob-val">p=${+sub.prob.toFixed(3)}</span>
        </div>
        <div class="sub-body">
          <div class="proc-box small">${renderProc(sub.proc)}</div>
          <div class="cap" style="margin-top:8px">ρ · dim ${sub.qstate.dim}</div>
          <div class="mhost">${renderMatrix(sub.qstate.rho)}</div>
          <div class="matrix-actions" data-mi="${i}">
            <button class="mini-btn" data-act="insert">${icon("download")}<span>insert as state</span></button>
            <button class="mini-btn" data-act="latex">${icon("download")}<span>LaTeX</span></button>
            <button class="mini-btn" data-act="copy">${icon("copy")}<span>copy</span></button>
          </div>
        </div>
      </div>`,
    )
    .join("");

  setHTML(
    q,
    `<h4>Quantum distribution · QDist</h4>${toggle}${branches}<div class="cap env-cap">env ρ · discarded qubits</div><div class="mhost">${renderMatrix(detail.env)}</div>`,
  );
  wireDist();
}

function wireDist() {
  if (!current) return;
  const { els, node, detail, deps } = current;
  els.body.querySelectorAll<HTMLButtonElement>("#mtog button:not([disabled])").forEach((b) => {
    b.onclick = () => {
      matrixView = b.dataset.mv as "heat" | "katex";
      paintDist();
    };
  });
  els.body.querySelectorAll<HTMLElement>(".matrix-actions").forEach((row) => {
    const i = +(row.dataset.mi ?? "0");
    const rho = detail.dist[i]?.qstate.rho;
    if (!rho) return;
    row.querySelectorAll<HTMLButtonElement>("[data-act]").forEach((btn) => {
      btn.onclick = () => {
        if (btn.dataset.act === "insert") {
          openInsertForm(row, `Rho_s${node.id}_${i}`, (name) => {
            deps.insertDef(toDensitySnippet(name, rho));
            deps.toast(`inserted  state ${name} := density … dm { … }`);
          }, deps);
        } else if (btn.dataset.act === "latex") {
          void navigator.clipboard.writeText(toLatex(rho));
          deps.toast("LaTeX \\begin{bmatrix}…\\end{bmatrix} copied");
        } else {
          void navigator.clipboard.writeText(toDensitySnippet(`Rho_s${node.id}_${i}`, rho));
          deps.toast("density snippet copied to clipboard");
        }
      };
    });
  });
}
