// App bootstrap: three-pane workbench shell (styling ported from gui-mockup).
// Flow: Load → outline (configs + source) → pick config → eval → render graph →
// click node → lazy getState. The view header switches between three views of
// the selected configuration: explore (the full LTS), minimize (its quotient) and
// compare (equiv on two configs, side-by-side synced panes, §6). Engine calls run
// as cancellable jobs with progress in the status bar.

// Self-hosted fonts (Archivo, Spline Sans Mono) so the app is byte-for-byte
// identical offline — no Google Fonts CDN. Family names match styles.css.
import "@fontsource/archivo/400.css";
import "@fontsource/archivo/500.css";
import "@fontsource/archivo/600.css";
import "@fontsource/archivo/700.css";
import "@fontsource/spline-sans-mono/400.css";
import "@fontsource/spline-sans-mono/500.css";
import "@fontsource/spline-sans-mono/600.css";
import "./styles.css";
import { renderCompare, type CompareHandle } from "./compare";
import type { DocumentResult, FileTagEquiv, JsonDiagnostic, LtsDocument, NodeSummary, RpcError, SilenceSpec, Source, TagPolicy } from "./contract";
import { analyzeDivergence, type Divergence } from "./divergence";
import { $, esc, setHTML } from "./dom";
import { makeEditor } from "./editor";
import {
  buildGraph,
  DEFAULT_STRETCH_X,
  DEFAULT_STRETCH_Y,
  LAYOUT_MAX_NODES,
  layoutSkipped,
  makeController,
  type Controller,
} from "./graph";
import { hydrateIcons, icon, setIcon } from "./icons";
import {
  bisimTypeLabel,
  renderEmpty,
  renderNode,
  renderVerdict,
  type InspectorDeps,
  type InspectorEls,
} from "./inspector";
import { annotateKeys, Commands, Palette, renderShortcutSheet, showKey, type Command } from "./commands";
import { EXAMPLES } from "./examples";
import { renderSyntaxGuide } from "./syntaxGuide";
import { installTooltips, setAvailable, setTip } from "./tooltip";
import {
  check,
  confirmDiscard,
  equivSource,
  evalSource,
  exportSource,
  guardWindowClose,
  isCancelled,
  minimizeSource,
  onFileDrop,
  outlineFile,
  outlineText,
  pickQuarcFile,
  pickSavePath,
  saveFile,
  type CallOpts,
  type Equivalence,
  type Progress,
} from "./rpc";
import type { Example } from "./examples";
import { fileName, pushRecent, recentFiles, removeRecent } from "./recent";
import { markActive, renderSources, setConfigsStale, type ConfigPick } from "./sources";
import { installSplitters } from "./splitters";
import { renderWelcome } from "./welcome";
import { groupTagKeys, groupTauTags, overrideFor, renderSilencePanel, silenceBlock, type TagGroup } from "./silence";

const els: InspectorEls = {
  title: $("#inspectorTitle"),
  sub: $("#inspectorSub"),
  body: $("#inspectorBody"),
};
const toastEl = $("#toast");
const canvas = $("#canvas");
const canvasA = $("#canvasA");
const canvasB = $("#canvasB");
const bannerMsg = $("#banner-msg");
const openBtn = $<HTMLButtonElement>("#openBtn");
const fileMoreBtn = $<HTMLButtonElement>("#fileMoreBtn");
const fileMenu = $("#fileMenu");
const fileLabel = $("#fileLabel");
const recentList = $("#recentList");
const reloadBtn = $<HTMLButtonElement>("#reloadBtn");
const refreshCfgBtn = $<HTMLButtonElement>("#refreshCfgBtn");
const welcomeEl = $("#welcome");
const canvasEmpty = $("#canvasEmpty");
const dropHint = $("#dropHint");
const viewSeg = $("#viewSeg");
const equivSel = $<HTMLSelectElement>("#equivSel");
const filePath = $<HTMLInputElement>("#filePath");
const main = $("#main");
const center = $("#center");
const sourcesBody = $("#sourcesBody");
const canvasWrap = $("#canvasWrap");
const compareBar = $("#compareBar");
const verdictBanner = $("#verdictBanner");
const cfgA = $<HTMLSelectElement>("#cfgA");
const cfgB = $<HTMLSelectElement>("#cfgB");
const tagMode = $<HTMLSelectElement>("#tagMode");
const tauBtn = $<HTMLButtonElement>("#tauBtn");
const normBtn = $<HTMLButtonElement>("#normBtn");
const silenceBtn = $<HTMLButtonElement>("#silenceBtn");
const silencePanel = $("#silencePanel");
const stretchXInput = $<HTMLInputElement>("#stretchX");
const stretchYInput = $<HTMLInputElement>("#stretchY");
const exportSvgBtn = $<HTMLButtonElement>("#exportSvgBtn");
const exportPngBtn = $<HTMLButtonElement>("#exportPngBtn");
const exportJsonBtn = $<HTMLButtonElement>("#exportJsonBtn");
const diagStrip = $("#diagStrip");
const stDiag = $("#stDiag");
const saveBtn = $<HTMLButtonElement>("#saveBtn");
const saveLabel = $("#saveLabel");
const syntaxBtn = $<HTMLButtonElement>("#syntaxBtn");
const guideOverlay = $("#guideOverlay");
const guideBody = $("#guideBody");

// Swap every [data-icon] placeholder for its inline SVG before anything renders.
hydrateIcons();

let ctrl: Controller | null = null;
let cmp: CompareHandle | null = null;
let currentFile = "";
// Display name of the loaded buffer: the file name, or an example's name for
// buffers with no file on disk (currentFile = ""). "" = nothing loaded yet.
let bufferName = "";
// Label suffix for buffers with no file on disk ("example", "unsaved").
let bufferTag = "";
// Editor buffer has unsaved edits relative to disk (drives the save button).
let dirty = false;
let currentConfigs: ConfigPick[] = [];
// The loaded buffer's `tag-equiv` blocks (from the last outline).
let fileTagEquiv: FileTagEquiv = { global: false, scopes: [] };
// `undefined` = no config chosen yet; `null` is the valid default/unnamed block.
let activeConfig: ConfigPick | undefined = undefined;
let showTau = true;
let showNorm = false;
// Color nodes by equivalence class in minimize/compare views (§6). Off = plain
// ink; persisted so the preference survives reloads.
let colorClasses = localStorage.getItem("quarc-class-colors") !== "off";
// Layout stretch (toolbar X/Y sliders); applied to every live pane on render.
const stretch = { x: DEFAULT_STRETCH_X, y: DEFAULT_STRETCH_Y };
// Global render settings (§8.1): presentational flags affecting pretty strings.
let sugar = false;
let simplify = false;
// Tagging policy for untagged actions (engine default: branch); persisted, as it
// changes the semantics every view is computed from.
let tagging: TagPolicy = localStorage.getItem("quarc-tagging") === "linear" ? "linear" : "branch";
// Last drawn result, kept so a theme switch can recolor without an RPC round-trip.
let lastSingle: DocumentResult | null = null;
let lastCompare: { res: DocumentResult; a: string; b: string; bt: string } | null = null;
let lastDiags: JsonDiagnostic[] = [];
// Silent-τ selection made in the picker; replaces the file's silent/visible
// blocks until the next load, config pick or new comparison. null = file's.
let silenceOverride: SilenceSpec | null = null;

const editor = makeEditor($("#editorBody"), $("#editorTitle"), {
  check: (text) =>
    check(text, bufferPath()).then((r) => {
      detectConfigChange(text, r.diagnostics);
      return r.diagnostics;
    }),
  onDocChange: () => {
    if (bufferName) setDirty(true);
    scheduleRerun();
  },
  onDiagnostics: (diags) => renderDiags(diags),
});

// Reflect unsaved state on the save button (label + enablement). Save is only
// available once a file is loaded (nothing to write back to otherwise).
function setDirty(next: boolean) {
  dirty = next;
  saveLabel.textContent = dirty ? "save •" : "save";
  refreshUi();
}

// Write the editor buffer back to its file on disk via the sidecar.
async function save() {
  if (!currentFile || !dirty) return;
  try {
    await saveFile(currentFile, editor.getText());
    setDirty(false);
    toast("Saved");
  } catch (e) {
    showError((e as RpcError)?.message ?? "save failed");
  }
}

const currentFlags = (): string[] => [...(sugar ? ["sugar"] : []), ...(simplify ? ["simplify"] : [])];

function setStatus(states: number, edges: number) {
  $("#stStates").textContent = String(states);
  $("#stEdges").textContent = String(edges);
}

function showError(msg: string) {
  bannerMsg.textContent = msg;
  bannerMsg.classList.add("show");
}

function clearError() {
  bannerMsg.classList.remove("show");
  bannerMsg.textContent = "";
}

function namedConfigs(): string[] {
  return currentConfigs.filter((c): c is string => c != null);
}

/** The graph is evaluated from the live editor buffer (path kept for diagnostics). */
function bufferSource(): Source {
  return { text: editor.getText(), path: bufferPath(), tagging };
}

/** Path reported with buffer RPCs (diagnostic spans); examples use their name. */
const bufferPath = () => currentFile || bufferName;

/** Forward source link (§5.3): reveal a node's span and pop the editor open. */
function revealNode(node: NodeSummary | null) {
  if (!node?.loc) return;
  center.classList.add("editor-open");
  editor.revealSpan(node.loc);
}

// Briefly flash a transient status message (matrix export feedback, etc.).
let toastTimer: number | undefined;
function toast(msg: string) {
  toastEl.textContent = msg;
  toastEl.classList.add("show");
  clearTimeout(toastTimer);
  toastTimer = window.setTimeout(() => toastEl.classList.remove("show"), 1900);
}

// Side-effecting hooks the inspector calls back into (source jump, buffer
// insert for matrix export, toast).
const inspectorDeps: InspectorDeps = {
  insertDef: (text) => {
    center.classList.add("editor-open");
    editor.insertDefinition(text);
  },
  jump: (span) => {
    center.classList.add("editor-open");
    editor.revealSpan(span);
  },
  toast,
  stateExists: (name) =>
    new RegExp(`^\\s*state\\s+${name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\s*:=`, "m").test(editor.getText()),
};

// Debounced re-run of the current view from the buffer after edits. A new run
// supersedes (cancels) one still in flight, so typing never queues evaluations.
let rerunTimer: number | undefined;
function scheduleRerun() {
  if (view === "compare" ? !lastCompare : activeConfig === undefined) return;
  clearTimeout(rerunTimer);
  rerunTimer = window.setTimeout(() => void (view === "compare" ? runCompare() : showSingle()), 700);
}

// ─── diagnostics strip (§5.2) ────────────────────────────────────────────────

function renderDiags(diags: JsonDiagnostic[]) {
  lastDiags = diags;
  if (!diags.length) {
    setHTML(diagStrip, `<div class="diag-empty">No diagnostics.</div>`);
    stDiag.textContent = "";
    return;
  }
  const rows = diags
    .map((d, i) => {
      const sev = d.severity === "error" ? `<span class="sev err">ERR</span>` : `<span class="sev warn">WARN</span>`;
      const where = d.span ? `<span class="where">:${d.span.start.line}</span>` : "";
      return `<div class="diag" data-i="${i}">${sev}<span class="msg">${esc(d.message)}</span>${where}</div>`;
    })
    .join("");
  setHTML(diagStrip, rows);
  diagStrip.querySelectorAll<HTMLElement>(".diag").forEach((el) => {
    el.addEventListener("click", () => {
      const span = lastDiags[Number(el.dataset.i)]?.span;
      if (span) {
        center.classList.add("editor-open");
        editor.revealSpan(span);
      }
    });
  });
  setHTML(stDiag, `${icon("alert-triangle")}<span>${diags.length} diagnostic${diags.length > 1 ? "s" : ""}</span>`);
}

// ─── jobs: progress + cancel ─────────────────────────────────────────────────
// Engine calls that can take a while run as jobs: the status bar (and, after a
// moment, a pill on the canvas) shows the phase and elapsed time with a cancel
// button. Starting a new view job supersedes (cancels) the running one, so a
// burst of edits or setting changes never queues stale evaluations.

type JobSlot = "view" | "export";
interface Job {
  label: string;
  ctl: AbortController;
  started: number;
  progress?: Progress;
}
const jobs = new Map<JobSlot, Job>();
let jobTimer: number | undefined;

const PHASES: Record<string, string> = {
  parse: "parsing",
  evaluate: "evaluating",
  minimize: "minimizing",
  compare: "comparing",
  layout: "laying out",
  render: "rendering the image",
};
const fmtCount = (n: number) => n.toLocaleString("en-US");

function jobText(j: Job): string {
  const secs = ((performance.now() - j.started) / 1000).toFixed(1);
  const p = j.progress;
  const phase = p ? `${PHASES[p.phase] ?? p.phase}${p.states != null ? ` ${fmtCount(p.states)} states` : ""}` : "starting";
  return `${j.label} · ${phase} · ${secs} s`;
}

function renderJobs() {
  const j = jobs.get("view") ?? jobs.get("export");
  const statusbar = $("#statusbar");
  statusbar.classList.toggle("computing", j != null);
  $("#stJobText").textContent = j ? jobText(j) : "ready";
  $("#stCancel").classList.toggle("hidden", j == null);
  // The canvas pill only for view jobs that take noticeably long.
  const v = jobs.get("view");
  const busy = $("#busy");
  busy.classList.toggle("hidden", !v || performance.now() - v.started < 400);
  if (v) $("#busyText").textContent = jobText(v);
  canvasWrap.classList.toggle("stale", v != null && performance.now() - v.started >= 400);
  if (j && jobTimer == null) jobTimer = window.setInterval(renderJobs, 200);
  if (!j && jobTimer != null) {
    clearInterval(jobTimer);
    jobTimer = undefined;
  }
}

/** Run an engine call as a job in `slot`, superseding the slot's running job. */
async function runJob<T>(slot: JobSlot, label: string, call: (opts: CallOpts) => Promise<T>, done: (r: T) => void) {
  jobs.get(slot)?.ctl.abort();
  const job: Job = { label, ctl: new AbortController(), started: performance.now() };
  jobs.set(slot, job);
  const isCurrent = () => jobs.get(slot) === job;
  renderJobs();
  clearError();
  try {
    const r = await call({
      signal: job.ctl.signal,
      onProgress: (p) => {
        job.progress = p;
        if (isCurrent()) renderJobs();
      },
    });
    if (isCurrent()) done(r);
  } catch (e) {
    if (!isCancelled(e) && isCurrent()) showError((e as RpcError)?.message ?? `${label} failed`);
  } finally {
    if (isCurrent()) jobs.delete(slot);
    renderJobs();
    refreshUi();
  }
}

function cancelJobs() {
  if (!jobs.size) return;
  for (const j of jobs.values()) j.ctl.abort();
  jobs.clear();
  renderJobs();
  toast("Cancelled");
}
$("#busyCancel").addEventListener("click", cancelJobs);
$("#stCancel").addEventListener("click", cancelJobs);

// ─── views: explore / minimize / compare ─────────────────────────────────────

type View = "explore" | "minimize" | "compare";
let view: View = "explore";
// The single view compare returns to.
let singleView: "explore" | "minimize" = "explore";
// Shared by minimize and compare; persisted.
let equivalence: Equivalence =
  (localStorage.getItem("quarc-equivalence") ?? localStorage.getItem("quarc-min-equiv")) === "branching" ? "branching" : "strong";
equivSel.value = equivalence;
// Compare-only options.
let rooted = false;
let compareMinimized = false;
let dimMatched = false;
let lastDivergence: Divergence | null = null;
// The user picked a tag mode for this buffer; until then it follows the file.
let tagModePicked = false;

const configLabel = (c: ConfigPick | undefined) => (c === undefined ? "" : (c ?? "⟨default⟩"));

/** Whether the file declares tag equivalences that apply to comparing `a` with `b`. */
const hasTagEquivFor = (a: string, b: string) =>
  fileTagEquiv.global || fileTagEquiv.scopes.some(([x, y]) => (x === a && y === b) || (x === b && y === a));

/** Default tag mode, unless the user picked one: the file's tag-equiv when it
 * has blocks for the compared pair, angelic otherwise. */
function defaultTagMode() {
  if (tagModePicked) return;
  tagMode.value = hasTagEquivFor(cfgA.value, cfgB.value) ? "tag" : "angelic";
  describeSelect(tagMode);
}

/** The `bisimType` sent to `equiv` for the compare bar's settings. */
function bisimTypeString(): string {
  const base = equivalence === "branching" && rooted ? "rooted-branching" : equivalence;
  return tagMode.value ? `${base}-${tagMode.value}` : base;
}

/** Reflect the view in the segmented control, breadcrumb, compare bar and status bar. */
function updateViewUi() {
  viewSeg.querySelectorAll<HTMLButtonElement>("[data-view]").forEach((b) => {
    const on = b.dataset.view === view;
    b.classList.toggle("on", on);
    b.setAttribute("aria-selected", String(on));
  });
  compareBar.classList.toggle("hidden", view !== "compare");
  $("#rootedBtn").classList.toggle("on", rooted && equivalence === "branching");
  $("#quotientBtn").classList.toggle("on", compareMinimized);

  const sep = `<span class="cr-sep">›</span>`;
  const parts: string[] = [];
  if (bufferName) parts.push(`<span class="cr-file">${esc(bufferName)}</span>`);
  let mode = "";
  if (view === "compare") {
    if (cfgA.value) parts.push(`<b>${esc(cfgA.value)}</b> <span class="cr-vs">vs</span> <b>${esc(cfgB.value)}</b>`);
    mode = `${bisimTypeLabel(bisimTypeString())} bisimilarity${compareMinimized ? " · quotients" : ""}`;
  } else if (activeConfig !== undefined) {
    parts.push(`<b>${esc(configLabel(activeConfig))}</b>`);
    mode = view === "minimize" ? `quotient by ${equivalence} bisimilarity` : "full LTS";
  }
  setHTML($("#viewCrumb"), parts.length ? `${parts.join(sep)}${mode ? `${sep}<span class="cr-mode">${esc(mode)}</span>` : ""}` : "");
  $("#stView").textContent = bufferName ? `${view}${view === "explore" ? "" : ` · ${equivalence}`}` : "";
}

/** Show the "layout skipped" note when the engine didn't run graphviz. */
function updateLayoutNote(doc: LtsDocument | undefined) {
  const skipped = doc != null && layoutSkipped(doc.graph, doc.pos);
  const n = doc?.graph.nodes.length ?? 0;
  const note = $("#layoutNote");
  note.classList.toggle("hidden", !skipped);
  $("#stLayout").classList.toggle("hidden", !skipped);
  if (skipped) {
    note.textContent = `Graphviz layout is skipped above ${fmtCount(LAYOUT_MAX_NODES)} states (this graph has ${fmtCount(n)}), so a simple layered layout is shown. Minimize to get a smaller graph.`;
    $("#stLayout").textContent = "layered layout (graphviz skipped)";
  }
}

function render(res: DocumentResult) {
  if (ctrl) ctrl.destroy();
  canvasEmpty.classList.add("hidden");
  lastSingle = res;
  lastCompare = null;
  lastDivergence = null;
  const g = res.document.graph;
  const graph = buildGraph(g, res.document.pos, res.document.kind === "minimize" && colorClasses);
  ctrl = makeController(canvas, graph, {
    onSelect: (id, node) => {
      ctrl?.setSelected(id);
      // Light up the selected node's class-mates (compare panes already do this
      // on select/hover; mirror it here for the single view).
      ctrl?.setHighlightClass(node?.classId ?? null);
      if (id == null || node == null) renderEmpty(els);
      else {
        revealNode(node);
        void renderNode(els, res.handle, node, inspectorDeps);
      }
    },
  });
  ctrl.setShowTauLabels(showTau);
  ctrl.setShowNormTags(showNorm);
  ctrl.setStretch(stretch.x, stretch.y);
  setStatus(g.nodes.length, g.edges.length);
  setTimeout(() => ctrl?.fit(), 30);
  renderEmpty(els);
  updateLayoutNote(res.document);
  updateViewUi();
  updateSilenceUi();
}

/** Tear down the compare panes and show the single canvas. */
function enterSingleView() {
  if (cmp) {
    cmp.destroy();
    cmp = null;
  }
  canvasWrap.classList.remove("compare");
  updateViewUi();
  refreshUi();
}

async function showSingle() {
  if (activeConfig === undefined) return;
  const src = bufferSource();
  const cfg = activeConfig ?? undefined;
  const minimize = view === "minimize";
  await runJob(
    "view",
    `${minimize ? "minimize" : "explore"} ${configLabel(activeConfig)}`,
    (o) =>
      minimize
        ? minimizeSource(src, cfg, currentFlags(), equivalence, silenceOverride, o)
        : evalSource(src, cfg, currentFlags(), o),
    render,
  );
}

/** Switch view; picking a single view needs a configuration (the first, if none is chosen). */
function setView(v: View) {
  if (v === "compare") return void enterCompare();
  if (view !== v) silenceOverride = null;
  view = v;
  singleView = v;
  enterSingleView();
  if (activeConfig === undefined && currentConfigs.length) {
    activeConfig = currentConfigs[0];
    markActive(sourcesBody, 0);
  }
  updateViewUi();
  void showSingle();
}

/** A configuration picked in Sources: show it in the current single view. */
async function evalConfig(config: ConfigPick) {
  activeConfig = config;
  silenceOverride = null;
  if (view === "compare") view = singleView;
  enterSingleView();
  await showSingle();
}

const toggleMinimize = () => setView(view === "minimize" ? "explore" : "minimize");
const toggleCompare = () => (view === "compare" ? setView(singleView) : enterCompare());

// ─── compare view ─────────────────────────────────────────────────────────────

/** Enter compare: fill the pickers (keeping valid previous picks) and run. */
function enterCompare() {
  const named = namedConfigs();
  if (named.length < 2) return;
  const prevA = cfgA.value;
  const prevB = cfgB.value;
  const opts = named.map((c) => `<option value="${esc(c)}">${esc(c)}</option>`).join("");
  setHTML(cfgA, opts);
  setHTML(cfgB, opts);
  const active = typeof activeConfig === "string" ? activeConfig : undefined;
  cfgA.value = named.includes(prevA) ? prevA : (active ?? named[0]);
  cfgB.value = named.includes(prevB) && prevB !== cfgA.value ? prevB : (named.find((c) => c !== cfgA.value) ?? named[1]);
  describeSelect(cfgA);
  describeSelect(cfgB);
  defaultTagMode();
  view = "compare";
  updateViewUi();
  void runCompare();
}

function setVerdict(bisimilar: boolean, bt: string) {
  verdictBanner.classList.toggle("ok", bisimilar);
  verdictBanner.classList.toggle("no", !bisimilar);
  setIcon($("#verdictBanner .vmark"), bisimilar ? "check" : "x");
  $("#verdictText").textContent = bisimilar ? "bisimilar" : "not bisimilar";
  $("#verdictSub").textContent = `${bisimTypeLabel(bt)} bisimilarity`;
  $("#verdictWhere").classList.toggle("hidden", bisimilar);
}

function showVerdict() {
  if (!lastCompare) return;
  const { res, a, b, bt } = lastCompare;
  renderVerdict(
    els,
    { bisimilar: res.document.equiv?.bisimilar ?? false, bisimType: bt, names: [a, b], div: lastDivergence, dimmed: dimMatched },
    { reveal: revealState, showPath, toggleDim },
  );
}

const compareNode = (id: number) => lastCompare?.res.document.graph.nodes.find((n) => n.id === id);

function revealState(id: number) {
  const n = compareNode(id);
  if (n) cmp?.reveal(n);
}

/** Emphasize a side's path from its initial state to its first unmatched state. */
function showPath(origin: number) {
  const side = lastDivergence?.sides[origin];
  if (!side?.first || !side.initial) return;
  const nodes = new Set([String(side.initial.id), ...side.first.path.map((st) => String(st.to))]);
  const edges = new Set(side.first.path.map((st) => `${st.from}>${st.to}`));
  cmp?.setPath(nodes, edges);
  cmp?.focus(side.first.node);
  showVerdict();
}

/** Banner "where?": the first divergence of whichever side has one. */
function showDivergence() {
  const side = lastDivergence?.sides.find((s) => s.first);
  if (side) showPath(side.origin);
  else showVerdict();
}

/** While inspecting a state in compare, offer the way back to the verdict panel. */
function addBackToComparison() {
  const back = document.createElement("button");
  back.className = "mini-btn back-link";
  back.textContent = "← comparison";
  back.addEventListener("click", () => {
    cmp?.deselect();
    showVerdict();
  });
  els.body.prepend(back);
}

function toggleDim() {
  dimMatched = !dimMatched;
  cmp?.setDimmed(dimMatched && lastDivergence ? lastDivergence.unmatchedIds : null);
  showVerdict();
}

/** Draw an already-fetched equiv result into the two panes (no RPC). */
function drawCompare(res: DocumentResult, a: string, b: string, bt: string) {
  if (ctrl) {
    ctrl.destroy();
    ctrl = null;
  }
  if (cmp) cmp.destroy();
  lastSingle = null;
  lastCompare = { res, a, b, bt };
  lastDivergence = analyzeDivergence(res.document);
  canvasWrap.classList.add("compare");
  canvasEmpty.classList.add("hidden");
  $("#paneHeadA").textContent = a;
  $("#paneHeadB").textContent = b;
  const verdict = res.document.equiv?.bisimilar ?? false;
  cmp = renderCompare(
    canvasA,
    canvasB,
    res.document,
    {
      onSelect: (id, node) => {
        if (id == null || node == null) showVerdict();
        else {
          revealNode(node);
          void renderNode(els, res.handle, node, inspectorDeps);
          addBackToComparison();
        }
      },
    },
    colorClasses,
  );
  cmp.setShowTauLabels(showTau);
  cmp.setShowNormTags(showNorm);
  cmp.setStretch(stretch.x, stretch.y);
  if (dimMatched && lastDivergence) cmp.setDimmed(lastDivergence.unmatchedIds);
  setStatus(res.document.graph.nodes.length, res.document.graph.edges.length);
  setVerdict(verdict, bt);
  showVerdict();
  updateLayoutNote(res.document);
  updateViewUi();
  updateSilenceUi();
}

async function runCompare() {
  if (view !== "compare") return;
  const a = cfgA.value;
  const b = cfgB.value;
  const bt = bisimTypeString();
  // A silence selection names the tags of one comparison; drop it for another.
  if (lastCompare && (lastCompare.a !== a || lastCompare.b !== b || lastCompare.bt !== bt)) silenceOverride = null;
  const src = bufferSource();
  updateViewUi();
  await runJob(
    "view",
    `compare ${a} vs ${b}`,
    (o) => equivSource(src, a, b, bt, currentFlags(), silenceOverride, compareMinimized, o),
    (r) => drawCompare(r, a, b, bt),
  );
}

// ─── open / load ───────────────────────────────────────────────────────────────

/** Remove the current graph (single or compare) and reset the inspector. */
function clearGraph() {
  enterSingleView();
  if (ctrl) {
    ctrl.destroy();
    ctrl = null;
  }
  lastSingle = null;
  lastCompare = null;
  lastDivergence = null;
  jobs.get("view")?.ctl.abort();
  setStatus(0, 0);
  renderEmpty(els);
  updateLayoutNote(undefined);
  updateSilenceUi();
}

function updateFileLabel() {
  fileLabel.textContent = bufferName ? (currentFile ? bufferName : `${bufferName} (${bufferTag})`) : "open file…";
  setTip(openBtn, currentFile ? `${currentFile} — click to open another file` : "Open a .quarc file");
  openBtn.classList.toggle("has-file", !!bufferName);
  refreshUi();
}

/** Index of `config` in the current list, or -1. */
const configIndex = (config: ConfigPick | undefined) =>
  config === undefined ? -1 : currentConfigs.findIndex((c) => c === config);

function showSources() {
  renderSources(sourcesBody, bufferName, currentConfigs, (config) => void evalConfig(config));
  setStale(false);
}

/** Take over a freshly outlined buffer and evaluate its first configuration. */
async function adopt(outline: { configs: ConfigPick[]; source: string; tagEquiv: FileTagEquiv }, file: string, name: string) {
  currentFile = file;
  bufferName = name;
  currentConfigs = outline.configs;
  fileTagEquiv = outline.tagEquiv;
  tagModePicked = false;
  activeConfig = undefined;
  silenceOverride = null;
  if (view === "compare") view = singleView;
  editor.setSource(outline.source, name);
  setDirty(false);
  updateFileLabel();
  welcomeEl.classList.add("hidden");
  clearGraph();
  showSources();
  if (outline.configs.length > 0) {
    markActive(sourcesBody, 0);
    await evalConfig(outline.configs[0]);
  } else {
    showCanvasEmpty();
  }
}

function showCanvasEmpty() {
  setHTML(
    canvasEmpty,
    `<div class="ce-title">No start blocks</div>
     <div class="ce-sub">Add a <code>start</code> block (or fix the errors shown in the editor), then press
     <span class="ic">${icon("refresh-cw")}</span> in <b>Sources</b> to pick it up.</div>`,
  );
  canvasEmpty.classList.remove("hidden");
  center.classList.add("editor-open");
}

const confirmLeave = async () =>
  !dirty || (await confirmDiscard("You have unsaved changes. Discard them and open a different file?"));

/** Open a .quarc file from disk by path. */
async function openPath(path: string) {
  const file = path.trim();
  if (!file || !(await confirmLeave())) return;
  clearError();
  closeFileMenu();
  try {
    const outline = await outlineFile(file);
    pushRecent(file);
    await adopt(outline, file, fileName(file));
  } catch (e) {
    showError((e as RpcError)?.message ?? "could not open the file");
  }
}

/** Open an in-memory source (bundled example, or a browser drop) as an unsaved buffer. */
async function openText(name: string, text: string, tag: string) {
  if (!(await confirmLeave())) return;
  clearError();
  try {
    bufferTag = tag;
    await adopt(await outlineText(text, name), "", name);
  } catch (e) {
    showError((e as RpcError)?.message ?? "could not open the example");
  }
}

async function openDialog() {
  closeFileMenu();
  const picked = await pickQuarcFile();
  if (picked) await openPath(picked);
}

// ─── stale configuration list ─────────────────────────────────────────────────
// The Sources list comes from the last outline. Edits that add, remove or rename
// start blocks don't refresh it automatically; the refresh button lights up
// instead, and refreshing re-outlines the live buffer (edits are kept).

let configsStale = false;
function setStale(stale: boolean) {
  configsStale = stale;
  refreshCfgBtn.classList.toggle("stale", stale);
  setTip(
    refreshCfgBtn,
    stale
      ? "Start blocks changed in the editor — click to refresh the configurations"
      : "Refresh configurations from the editor",
  );
  setConfigsStale(sourcesBody, stale, () => void refreshConfigs());
}

const sameConfigs = (a: ConfigPick[], b: ConfigPick[]) => a.length === b.length && a.every((c, i) => c === b[i]);

function detectConfigChange(text: string, diags: JsonDiagnostic[]) {
  if (!bufferName) return;
  void outlineText(text, bufferPath()).then(
    (o) => {
      // Outdated answer, or a parse failure (no configs + errors): keep the last verdict.
      if (editor.getText() !== text) return;
      if (o.configs.length === 0 && diags.some((d) => d.severity === "error")) return;
      const changed = !sameConfigs(o.configs, currentConfigs);
      if (changed !== configsStale) setStale(changed);
    },
    () => {},
  );
}

async function refreshConfigs() {
  if (!bufferName) return;
  clearError();
  try {
    const o = await outlineText(editor.getText(), bufferPath());
    const prev = activeConfig;
    currentConfigs = o.configs;
    fileTagEquiv = o.tagEquiv;
    showSources();
    const i = configIndex(prev);
    if (view === "compare" && namedConfigs().length >= 2) {
      if (i >= 0) markActive(sourcesBody, i);
      enterCompare();
    } else if (i >= 0) {
      markActive(sourcesBody, i);
      if (view === "compare") view = singleView;
      enterSingleView();
      void showSingle();
    } else if (o.configs.length > 0) {
      markActive(sourcesBody, 0);
      await evalConfig(o.configs[0]);
    } else {
      activeConfig = undefined;
      clearGraph();
      showCanvasEmpty();
    }
    toast(`${o.configs.length} configuration${o.configs.length === 1 ? "" : "s"}`);
  } catch (e) {
    showError((e as RpcError)?.message ?? "could not refresh the configurations");
  }
}
refreshCfgBtn.addEventListener("click", (e) => {
  e.stopPropagation();
  void refreshConfigs();
});

// ─── file menu, welcome, drag & drop ─────────────────────────────────────────

function renderRecentMenu() {
  const list = recentFiles();
  if (!list.length) {
    setHTML(recentList, `<div class="menu-none">No recent files.</div>`);
    return;
  }
  setHTML(
    recentList,
    list
      .map(
        (p, i) =>
          `<button class="menu-item recent" data-i="${i}" title="${esc(p)}"><span class="ic">${icon("file")}</span><span class="rn">${esc(fileName(p))}</span></button>`,
      )
      .join(""),
  );
  recentList.querySelectorAll<HTMLElement>(".recent").forEach((b) => {
    b.addEventListener("click", () => void openPath(list[Number(b.dataset.i)]));
  });
}

function closeFileMenu() {
  fileMenu.classList.remove("show");
}

function showWelcome() {
  renderWelcome(welcomeEl, {
    open: () => void openDialog(),
    openRecent: (p) => void openPath(p),
    removeRecent: (p) => {
      removeRecent(p);
      showWelcome();
    },
    openExample: (ex: Example) => void openText(ex.name, ex.text, "example"),
    guide: () => openGuide(),
  });
}

openBtn.addEventListener("click", () => void openDialog());
fileMoreBtn.addEventListener("click", (e) => {
  e.stopPropagation();
  if (fileMenu.classList.toggle("show")) {
    renderRecentMenu();
    if (currentFile) filePath.value = currentFile;
    filePath.focus();
    filePath.select();
  }
});
filePath.addEventListener("keydown", (e) => {
  if (e.key === "Enter") void openPath(filePath.value);
  if (e.key === "Escape") closeFileMenu();
});
$("#pathGoBtn").addEventListener("click", () => void openPath(filePath.value));
reloadBtn.addEventListener("click", () => {
  if (currentFile) void openPath(currentFile);
});

void onFileDrop(
  (over) => dropHint.classList.toggle("hidden", !over),
  (f) => void ("path" in f ? openPath(f.path) : openText(f.name, f.text, "unsaved")),
);

viewSeg.querySelectorAll<HTMLButtonElement>("[data-view]").forEach((b) =>
  b.addEventListener("click", () => {
    const v = b.dataset.view as View;
    if (v === "compare" && view === "compare") return;
    setView(v);
  }),
);
equivSel.addEventListener("change", () => {
  equivalence = equivSel.value === "branching" ? "branching" : "strong";
  localStorage.setItem("quarc-equivalence", equivalence);
  describeSelect(equivSel);
  silenceOverride = null;
  if (view === "compare") void runCompare();
  else if (view === "minimize") void showSingle();
  else updateViewUi();
  refreshUi();
});
for (const sel of [cfgA, cfgB]) sel.addEventListener("change", () => (defaultTagMode(), void runCompare()));
tagMode.addEventListener("change", () => {
  tagModePicked = true;
  void runCompare();
});
function toggleRooted() {
  rooted = !rooted;
  void runCompare();
}
function toggleQuotients() {
  compareMinimized = !compareMinimized;
  void runCompare();
}
$("#rootedBtn").addEventListener("click", toggleRooted);
$("#quotientBtn").addEventListener("click", toggleQuotients);
$("#runCompare").addEventListener("click", () => void runCompare());
$("#verdictWhere").addEventListener("click", showDivergence);

// Rail toggles.
const sourcesToggle = $("#sourcesToggle");
sourcesToggle.addEventListener("click", (e) => {
  e.stopPropagation();
  toggleSidebar();
});
function toggleSidebar() {
  const collapsed = main.classList.toggle("left-collapsed");
  setTip(sourcesToggle, collapsed ? "Expand sidebar" : "Collapse sidebar");
  setIcon(sourcesToggle, collapsed ? "chevrons-right" : "chevrons-left");
}
$("#edHead").addEventListener("click", () => center.classList.toggle("editor-open"));
saveBtn.addEventListener("click", (e) => {
  e.stopPropagation(); // don't toggle the editor rail
  void save();
});

// Syntax guide overlay (rendered from QUARC_SYNTAX.md, lazily on first open).
let guideRendered = false;
const openGuide = () => {
  if (!guideRendered) {
    setHTML(guideBody, renderSyntaxGuide());
    guideRendered = true;
  }
  guideOverlay.classList.remove("hidden");
};
const closeGuide = () => guideOverlay.classList.add("hidden");
syntaxBtn.addEventListener("click", (e) => {
  e.stopPropagation(); // don't toggle the editor rail
  openGuide();
});
$("#guideClose").addEventListener("click", closeGuide);
guideOverlay.addEventListener("click", (e) => {
  if (e.target === guideOverlay) closeGuide(); // click the backdrop to dismiss
});

// Graph controls: τ-label toggle, restore layout, fit (apply to all live panes).
function toggleTau() {
  showTau = !showTau;
  tauBtn.classList.toggle("on", showTau);
  ctrl?.setShowTauLabels(showTau);
  cmp?.setShowTauLabels(showTau);
}
function toggleNorm() {
  showNorm = !showNorm;
  normBtn.classList.toggle("on", showNorm);
  ctrl?.setShowNormTags(showNorm);
  cmp?.setShowNormTags(showNorm);
}
function restoreLayout() {
  ctrl?.restoreLayout();
  cmp?.restoreLayout();
}
function fitView() {
  ctrl?.fit();
  cmp?.fit();
}
tauBtn.addEventListener("click", toggleTau);
normBtn.addEventListener("click", toggleNorm);
$("#restoreBtn").addEventListener("click", restoreLayout);
$("#fitBtn").addEventListener("click", fitView);

// Layout-stretch sliders: rescale live while dragging, preserving the current
// zoom/pan (setStretch never moves the camera), even if the graph outgrows the
// view — use the fit button to reframe.
const spacingPanel = $("#spacingPanel");
const spacingBtn = $<HTMLButtonElement>("#spacingBtn");
const syncStretchInputs = () => {
  stretchXInput.value = String(stretch.x);
  stretchYInput.value = String(stretch.y);
  $("#stretchXVal").textContent = `${stretch.x.toFixed(1)}×`;
  $("#stretchYVal").textContent = `${stretch.y.toFixed(1)}×`;
};
syncStretchInputs();
const applyStretch = () => {
  syncStretchInputs();
  ctrl?.setStretch(stretch.x, stretch.y);
  cmp?.setStretch(stretch.x, stretch.y);
};
stretchXInput.addEventListener("input", () => {
  stretch.x = +stretchXInput.value;
  applyStretch();
});
stretchYInput.addEventListener("input", () => {
  stretch.y = +stretchYInput.value;
  applyStretch();
});
$("#spacingReset").addEventListener("click", () => {
  stretch.x = DEFAULT_STRETCH_X;
  stretch.y = DEFAULT_STRETCH_Y;
  applyStretch();
});
function toggleSpacing() {
  const open = !spacingPanel.classList.contains("show");
  closePopovers();
  spacingPanel.classList.toggle("show", open);
  spacingBtn.classList.toggle("on", open);
}
spacingBtn.addEventListener("click", (e) => {
  e.stopPropagation();
  toggleSpacing();
});

// Theme toggle: flip the data-theme token, persist, then recolor the live graph
// (colors are baked into graphology attrs, so redraw the last result — no RPC).
const themeBtn = $<HTMLButtonElement>("#themeBtn");
function applyTheme(theme: "lab" | "light") {
  document.documentElement.dataset.theme = theme;
  setIcon(themeBtn, theme === "light" ? "sun" : "moon");
  localStorage.setItem("quarc-theme", theme);
}
function toggleTheme() {
  const next = document.documentElement.dataset.theme === "light" ? "lab" : "light";
  applyTheme(next);
  if (lastCompare) drawCompare(lastCompare.res, lastCompare.a, lastCompare.b, lastCompare.bt);
  else if (lastSingle) render(lastSingle);
}
themeBtn.addEventListener("click", toggleTheme);
applyTheme(localStorage.getItem("quarc-theme") === "light" ? "light" : "lab");

// ─── silent τ tags ────────────────────────────────────────────────────────────

/** Whether the current view is checked by branching bisimilarity, the only
 * equivalence that silence affects. */
function silenceApplies(): boolean {
  if (lastCompare) return lastCompare.bt.includes("branching");
  return lastSingle?.document.kind === "minimize" && equivalence === "branching";
}

const currentTagGroups = (): TagGroup[] =>
  groupTauTags((lastCompare?.res.document ?? lastSingle?.document)?.tauTags ?? []);

function highlightTags(keys: Set<string> | null) {
  ctrl?.setHighlightTags(keys);
  cmp?.setHighlightTags(keys);
}

function closeSilencePanel() {
  silencePanel.classList.remove("show");
  highlightTags(null);
}

/** Apply a new selection and re-run the current view. */
function setSilence(spec: SilenceSpec | null) {
  silenceOverride = spec;
  applySettings();
}

/** Refresh the toolbar button (and the panel, if open) from the current document. */
function updateSilenceUi() {
  const groups = currentTagGroups();
  const applies = silenceApplies() && groups.length > 0;
  $("#silenceLabel").textContent = applies
    ? `silent τ ${groups.filter((g) => g.silent).length}/${groups.length}`
    : "silent τ";
  silenceBtn.classList.toggle("active", silenceOverride != null);
  if (!applies) closeSilencePanel();
  else if (silencePanel.classList.contains("show")) renderSilence(groups);
  refreshUi();
}

function renderSilence(groups: TagGroup[]) {
  renderSilencePanel(silencePanel, groups, silenceOverride != null, {
    onToggle: (g) => setSilence(overrideFor(groups, (x) => (x === g ? !x.silent : x.silent))),
    onHover: (g) => highlightTags(g ? groupTagKeys(g) : null),
    onReveal: (span) => {
      center.classList.add("editor-open");
      editor.revealSpan(span);
    },
    onAll: () => setSilence(overrideFor(groups, () => true)),
    onNone: () => setSilence(overrideFor(groups, () => false)),
    onReset: () => setSilence(null),
    onCopy: () => {
      const block = silenceBlock(groups, lastCompare ? [lastCompare.a, lastCompare.b] : undefined);
      void navigator.clipboard.writeText(block).then(
        () => toast("Copied: " + block),
        () => showError("could not copy to the clipboard"),
      );
    },
  });
}

function toggleSilencePanel() {
  if (silencePanel.classList.contains("show")) closeSilencePanel();
  else {
    closePopovers();
    renderSilence(currentTagGroups());
    anchorPopover(silencePanel, silenceBtn);
    silencePanel.classList.add("show");
  }
}
silenceBtn.addEventListener("click", (e) => {
  e.stopPropagation();
  toggleSilencePanel();
});

// ─── settings & export ───────────────────────────────────────────────────────

const settingsPanel = $("#settingsPanel");
const settingsBtn = $("#settingsBtn");
const exportMenu = $("#exportMenu");
const exportBtn = $<HTMLButtonElement>("#exportBtn");
function toggleSettings() {
  const open = !settingsPanel.classList.contains("show");
  closePopovers();
  settingsPanel.classList.toggle("show", open);
}
function toggleExportMenu() {
  const open = !exportMenu.classList.contains("show");
  closePopovers();
  if (open) {
    anchorPopover(exportMenu, exportBtn, "right");
    exportMenu.classList.add("show");
  }
}
settingsBtn.addEventListener("click", (e) => {
  e.stopPropagation();
  toggleSettings();
});
exportBtn.addEventListener("click", (e) => {
  e.stopPropagation();
  toggleExportMenu();
});

/** Popovers dismissed by an outside click (each paired with the button that opens it). */
const POPOVERS: [HTMLElement, HTMLElement, () => void][] = [
  [silencePanel, silenceBtn, closeSilencePanel],
  [fileMenu, fileMoreBtn, () => closeFileMenu()],
  [settingsPanel, settingsBtn, () => settingsPanel.classList.remove("show")],
  [exportMenu, exportBtn, () => exportMenu.classList.remove("show")],
  [
    spacingPanel,
    spacingBtn,
    () => {
      spacingPanel.classList.remove("show");
      spacingBtn.classList.remove("on");
    },
  ],
];
function closePopovers(except?: Node) {
  for (const [pop, btn, close] of POPOVERS) {
    if (pop.classList.contains("show") && !(except && (pop.contains(except) || btn.contains(except)))) close();
  }
}
document.addEventListener("click", (e) => closePopovers(e.target as Node));

/** Place a top-bar popover under its button (left- or right-aligned to it). */
function anchorPopover(pop: HTMLElement, btn: HTMLElement, align: "left" | "right" = "left") {
  const r = btn.getBoundingClientRect();
  pop.style.top = `${r.bottom + 6}px`;
  if (align === "left") {
    pop.style.left = `${Math.max(8, Math.min(r.left, window.innerWidth - pop.offsetWidth - 8))}px`;
    pop.style.right = "auto";
  } else {
    pop.style.right = `${Math.max(8, window.innerWidth - r.right)}px`;
    pop.style.left = "auto";
  }
}

// Sugar/Simplify only change the pretty strings, so re-run the current view to
// refresh the cached node detail with the new flags.
function applySettings() {
  if (view === "compare") void runCompare();
  else if (activeConfig !== undefined) {
    void showSingle();
  }
}
$("#psetSugar").addEventListener("click", () => {
  sugar = !sugar;
  $("#sugarSw").classList.toggle("on", sugar);
  applySettings();
});
$("#psetSimplify").addEventListener("click", () => {
  simplify = !simplify;
  $("#simplifySw").classList.toggle("on", simplify);
  applySettings();
});
// Tagging changes the tags (and so the LTS), so re-run like the render flags;
// a silence selection names tags of the old policy, so drop it.
$("#linearTagsSw").classList.toggle("on", tagging === "linear");
$("#psetLinearTags").addEventListener("click", () => {
  tagging = tagging === "linear" ? "branch" : "linear";
  localStorage.setItem("quarc-tagging", tagging);
  $("#linearTagsSw").classList.toggle("on", tagging === "linear");
  silenceOverride = null;
  applySettings();
});
// Class colors are baked into graphology attrs at build time, so a toggle just
// redraws the last result — no RPC needed (like the theme switch).
$("#classColorsSw").classList.toggle("on", colorClasses);
$("#psetClassColors").addEventListener("click", () => {
  colorClasses = !colorClasses;
  localStorage.setItem("quarc-class-colors", colorClasses ? "on" : "off");
  $("#classColorsSw").classList.toggle("on", colorClasses);
  if (lastCompare) drawCompare(lastCompare.res, lastCompare.a, lastCompare.b, lastCompare.bt);
  else if (lastSingle) render(lastSingle);
});

function download(name: string, data: string, mime: string) {
  triggerDownload(name, new Blob([data], { type: mime }));
}

/** Decode a base64 payload (e.g. PNG from the engine) and download it. */
function downloadBase64(name: string, b64: string, mime: string) {
  const bin = atob(b64);
  const bytes = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
  triggerDownload(name, new Blob([bytes], { type: mime }));
}

function triggerDownload(name: string, blob: Blob) {
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = name;
  a.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}

const baseName = () => (bufferName || "graph").replace(/\.quarc$/, "");

/** Default save location: next to the source file when there is one. */
function defaultExportPath(name: string): string {
  if (!currentFile) return name;
  const i = Math.max(currentFile.lastIndexOf("/"), currentFile.lastIndexOf("\\"));
  return i >= 0 ? currentFile.slice(0, i + 1) + name : name;
}

// Tauri's webview ignores `<a download>`, so on the desktop exports go through
// the native save dialog and are written to disk: images by the engine (binary
// safe, no base64 round trip), JSON via the sidecar's `save`. The browser-dev
// build keeps the download fallback.
function exportImage(format: "svg" | "png") {
  const kind = view === "minimize" ? "minimize" : "eval";
  const name = `${baseName()}-${kind === "minimize" ? `minimize-${equivalence}` : kind}.${format}`;
  const src = bufferSource();
  const cfg = activeConfig ?? undefined;
  const flags = currentFlags();
  const silence = silenceOverride;
  void (async () => {
    const path = await pickSavePath(defaultExportPath(name), { name: format.toUpperCase(), extensions: [format] });
    if (path === null) return; // dialog cancelled
    await runJob(
      "export",
      `export ${format.toUpperCase()}`,
      (o) => exportSource(src, cfg, kind, format, flags, equivalence, silence, path, o),
      (r) => {
        if (r.path) toast(`Exported to ${r.path}`);
        else if (format === "png") downloadBase64(name, r.data ?? "", "image/png");
        else download(name, r.data ?? "", "image/svg+xml");
      },
    );
  })();
}

function exportJson() {
  const doc = lastCompare?.res.document ?? lastSingle?.document;
  if (!doc) return;
  const name = `${baseName()}-${doc.kind}.json`;
  void (async () => {
    const path = await pickSavePath(defaultExportPath(name), { name: "JSON", extensions: ["json"] });
    if (path === null) return;
    const text = JSON.stringify(doc, null, 2);
    if (path === undefined) return download(name, text, "application/json");
    try {
      await saveFile(path, text);
      toast(`Exported to ${path}`);
    } catch (e) {
      showError((e as RpcError)?.message ?? "export failed");
    }
  })();
}
const afterMenu = (f: () => void) => () => {
  exportMenu.classList.remove("show");
  f();
};
exportSvgBtn.addEventListener("click", afterMenu(() => exportImage("svg")));
exportPngBtn.addEventListener("click", afterMenu(() => exportImage("png")));
exportJsonBtn.addEventListener("click", afterMenu(exportJson));


// ─── availability (why a control is disabled) ────────────────────────────────
// One reason per action, shared by the buttons' tooltips, the palette and the
// shortcuts, so a greyed-out control always says what would enable it.

const hasGraph = () => lastSingle != null || lastCompare != null;
const currentDoc = () => lastCompare?.res.document ?? lastSingle?.document;

const why = {
  loaded: (): string | null => (bufferName ? null : "Open a .quarc file (or an example) first"),
  config(): string | null {
    return (
      why.loaded() ??
      (currentConfigs.length === 0
        ? "This file has no start blocks to evaluate"
        : activeConfig === undefined
          ? "Pick a configuration in Sources first"
          : null)
    );
  },
  single: (): string | null => why.loaded() ?? (currentConfigs.length === 0 ? "This file has no start blocks to evaluate" : null),
  inCompare: (): string | null => (view === "compare" ? null : "Switch to the compare view first"),
  rooted: (): string | null =>
    why.inCompare() ?? (equivalence === "branching" ? null : "Rooted only applies to branching bisimilarity — set equivalence to “branching”"),
  divergence(): string | null {
    if (!lastCompare) return "Run a comparison first";
    if (lastCompare.res.document.equiv?.bisimilar) return "The two sides are bisimilar: there is no divergence";
    return lastDivergence ? null : "This comparison has no class information";
  },
  compare(): string | null {
    const n = namedConfigs().length;
    return (
      why.loaded() ??
      (n < 2
        ? `Compare needs two named start blocks (start "name" { … }); this file has ${n === 0 ? "none" : "one"}. The unnamed default block can't be compared.`
        : null)
    );
  },
  graph: (): string | null => (hasGraph() ? null : (why.config() ?? "Nothing is drawn yet")),
  exportImage: (): string | null =>
    why.graph() ??
    (lastCompare ? "Image export covers a single configuration — switch to explore or minimize (JSON export works here)" : null),
  save(): string | null {
    if (!bufferName) return "Nothing to save — open a file first";
    if (!currentFile) return "This buffer has no file on disk (an example or a dropped file), so it can't be saved in place";
    return dirty ? null : "No unsaved changes";
  },
  reload: (): string | null => why.loaded() ?? (currentFile ? null : "This buffer has no file on disk to reload"),
  silence(): string | null {
    const g = why.graph();
    if (g) return g;
    if (!silenceApplies())
      return view === "explore"
        ? "Silent τ only affects the minimize and compare views (with branching bisimilarity)"
        : "Silent τ only matters for branching bisimilarity — set equivalence to “branching”";
    return currentTagGroups().length ? null : "This LTS has no tagged τ steps";
  },
  norm(): string | null {
    return (
      why.graph() ??
      (currentDoc()?.graph.edges.some((e) => e.labels?.length)
        ? null
        : "Normalized tags appear in minimize and compare views when tag equivalences apply")
    );
  },
};

function refreshUi() {
  setAvailable(saveBtn, why.save());
  setAvailable(reloadBtn, why.reload());
  setAvailable(refreshCfgBtn, why.loaded());
  for (const b of viewSeg.querySelectorAll<HTMLButtonElement>("[data-view]"))
    setAvailable(b, b.dataset.view === "compare" ? why.compare() : why.single());
  setAvailable($<HTMLButtonElement>("#rootedBtn"), why.rooted());
  setAvailable(silenceBtn, why.silence());
  setAvailable($<HTMLButtonElement>("#verdictWhere"), why.divergence());
  setAvailable($<HTMLButtonElement>("#exportBtn"), why.graph());
  for (const [btn, reason] of [
    [exportSvgBtn, why.exportImage()],
    [exportPngBtn, why.exportImage()],
    [exportJsonBtn, why.graph()],
  ] as const) {
    setAvailable(btn, reason);
    btn.querySelector(".mi-why")!.textContent = reason ?? "";
  }
  for (const id of ["#fitBtn", "#restoreBtn", "#spacingBtn", "#tauBtn"]) setAvailable($<HTMLButtonElement>(id), why.graph());
  setAvailable(normBtn, why.norm());
}

/** Mirror a select's current option description into its tooltip. */
function describeSelect(sel: HTMLSelectElement) {
  const desc = sel.selectedOptions[0]?.dataset.desc;
  setTip(sel, desc ?? sel.selectedOptions[0]?.textContent ?? "");
}
for (const sel of [equivSel, tagMode, cfgB]) {
  describeSelect(sel);
  sel.addEventListener("change", () => describeSelect(sel));
}
cfgA.addEventListener("change", () => describeSelect(cfgA));

// ─── commands, palette, shortcuts ────────────────────────────────────────────

const keysOverlay = $("#keysOverlay");
const cmds = new Commands();
const palette = new Palette(
  { overlay: $("#paletteOverlay"), input: $<HTMLInputElement>("#palInput"), list: $("#palList") },
  () => [...cmds.all(), ...dynamicCommands()],
  (reason) => toast(reason),
);

/** Step the active configuration by `delta` (wrapping). */
function stepConfig(delta: number) {
  const n = currentConfigs.length;
  const i = configIndex(activeConfig);
  const next = ((i < 0 ? (delta > 0 ? -1 : 0) : i) + delta + n) % n;
  pickConfig(next);
}
function pickConfig(i: number) {
  markActive(sourcesBody, i);
  void evalConfig(currentConfigs[i]);
}
function openShortcuts() {
  renderShortcutSheet($("#keysBody"), cmds);
  keysOverlay.classList.remove("hidden");
}
const closeShortcuts = () => keysOverlay.classList.add("hidden");
$("#keysClose").addEventListener("click", closeShortcuts);
keysOverlay.addEventListener("click", (e) => {
  if (e.target === keysOverlay) closeShortcuts();
});
$("#keysBtn").addEventListener("click", openShortcuts);
$("#paletteBtn").addEventListener("click", () => palette.open());

function setEquivalence(eq: Equivalence) {
  equivSel.value = eq;
  equivSel.dispatchEvent(new Event("change"));
}
const toggleSwitch = (id: string) => () => $(id).click();

cmds.add(
  // General
  { id: "palette", group: "General", title: "Command palette", key: "Mod+K", hidden: true, sheet: { title: "Command palette", key: "Mod+K" }, run: () => palette.open() },
  { id: "palette2", group: "General", title: "Command palette", key: "Mod+Shift+P", hidden: true, run: () => palette.open() },
  { id: "shortcuts", group: "General", title: "Keyboard shortcuts", key: "?", run: openShortcuts },
  { id: "guide", group: "General", title: "QUARC syntax guide", key: "F1", keywords: "help language reference", run: () => openGuide() },
  { id: "settings", group: "General", title: "Settings", key: "Mod+,", keywords: "preferences", run: toggleSettings },
  { id: "theme", group: "General", title: "Toggle light/dark theme", keywords: "dark light colors", run: toggleTheme },
  // File
  { id: "open", group: "File", title: "Open file…", key: "Mod+O", run: () => void openDialog() },
  {
    id: "fileMenu",
    group: "File",
    title: "Open by path / recent files…",
    key: "Mod+Shift+O",
    run: () => fileMoreBtn.click(),
  },
  { id: "save", group: "File", title: "Save", key: "Mod+S", why: why.save, run: () => void save() },
  { id: "reload", group: "File", title: "Reload from disk", why: why.reload, run: () => void openPath(currentFile) },
  {
    id: "refreshConfigs",
    group: "File",
    title: "Refresh configurations from the editor",
    key: "R",
    keywords: "start blocks outline",
    why: why.loaded,
    run: () => void refreshConfigs(),
  },
  { id: "welcome", group: "File", title: "Show the welcome screen", keywords: "examples recent start", run: () => {
      showWelcome();
      welcomeEl.classList.remove("hidden");
    } },
  // Configuration
  { id: "nextConfig", group: "Configuration", title: "Next configuration", key: "]", why: why.loaded, run: () => stepConfig(1) },
  { id: "prevConfig", group: "Configuration", title: "Previous configuration", key: "[", why: why.loaded, run: () => stepConfig(-1) },
  ...Array.from({ length: 9 }, (_, i): Command => ({
    id: `config${i + 1}`,
    group: "Configuration",
    title: `Configuration ${i + 1}`,
    key: String(i + 1),
    hidden: true,
    ...(i === 0 ? { sheet: { title: "Show configuration 1–9", key: "1 … 9" } } : {}),
    why: () => why.loaded() ?? (i < currentConfigs.length ? null : `This file has only ${currentConfigs.length} configuration${currentConfigs.length === 1 ? "" : "s"}`),
    run: () => pickConfig(i),
  })),
  // Analysis
  { id: "explore", group: "Analysis", title: "Explore view (full LTS)", key: "X", keywords: "eval view", why: why.single, run: () => setView("explore") },
  { id: "minimize", group: "Analysis", title: "Minimize view (toggle)", key: "M", keywords: "quotient view", why: why.single, run: toggleMinimize },
  { id: "compare", group: "Analysis", title: "Compare view (toggle)", key: "C", keywords: "bisimilar equivalence view", why: why.compare, run: toggleCompare },
  { id: "eqStrong", group: "Analysis", title: "Equivalence: strong bisimilarity", run: () => setEquivalence("strong") },
  { id: "eqBranching", group: "Analysis", title: "Equivalence: branching bisimilarity", run: () => setEquivalence("branching") },
  { id: "rooted", group: "Analysis", title: "Toggle rooted (compare)", why: why.rooted, run: toggleRooted },
  { id: "quotients", group: "Analysis", title: "Toggle quotients (compare)", key: "Q", keywords: "minimized sides", why: why.inCompare, run: toggleQuotients },
  { id: "runCompare", group: "Analysis", title: "Re-run the comparison", key: "Mod+Enter", fallthrough: true, why: why.inCompare, run: () => void runCompare() },
  { id: "divergence", group: "Analysis", title: "Show the first divergence", key: "D", keywords: "not bisimilar where witness trace", why: why.divergence, run: showDivergence },
  { id: "dimMatched", group: "Analysis", title: "Dim matched states (compare)", key: "U", keywords: "unmatched highlight", why: why.divergence, run: toggleDim },
  { id: "cancel", group: "Analysis", title: "Cancel the running job", keywords: "stop abort", why: () => (jobs.size ? null : "Nothing is running"), run: cancelJobs },
  { id: "silence", group: "Analysis", title: "Choose silent τ tags…", key: "S", keywords: "tau branching", why: why.silence, run: toggleSilencePanel },
  { id: "tagging", group: "Analysis", title: "Toggle linear tagging", keywords: "tags policy branch", run: toggleSwitch("#psetLinearTags") },
  // View
  { id: "fit", group: "View", title: "Fit graph to view", key: "F", keywords: "zoom", why: why.graph, run: fitView },
  { id: "restoreLayout", group: "View", title: "Restore layout", key: "L", keywords: "undo drag reset", why: why.graph, run: restoreLayout },
  { id: "spacing", group: "View", title: "Layout spacing…", keywords: "stretch", why: why.graph, run: toggleSpacing },
  { id: "tau", group: "View", title: "Toggle τ labels", key: "T", keywords: "tau", why: why.graph, run: toggleTau },
  { id: "norm", group: "View", title: "Toggle normalized tags", key: "N", why: why.norm, run: toggleNorm },
  { id: "editor", group: "View", title: "Toggle editor", key: "E", run: () => void center.classList.toggle("editor-open") },
  { id: "sidebar", group: "View", title: "Toggle sources sidebar", key: "Mod+B", run: toggleSidebar },
  { id: "classColors", group: "View", title: "Toggle class colors", run: toggleSwitch("#psetClassColors") },
  { id: "sugar", group: "View", title: "Toggle syntactic sugar", run: toggleSwitch("#psetSugar") },
  { id: "simplify", group: "View", title: "Toggle simplification", run: toggleSwitch("#psetSimplify") },
  // Export
  { id: "exportMenu", group: "Export", title: "Export…", hidden: true, why: why.graph, run: toggleExportMenu },
  { id: "exportSvg", group: "Export", title: "Export graph as SVG", why: why.exportImage, run: () => exportImage("svg") },
  { id: "exportPng", group: "Export", title: "Export graph as PNG", why: why.exportImage, run: () => exportImage("png") },
  { id: "exportJson", group: "Export", title: "Export document as JSON", why: why.graph, run: exportJson },
);

/** Palette-only entries built from the current state. */
function dynamicCommands(): Command[] {
  const out: Command[] = currentConfigs.map((c, i) => ({
    id: `pick:${i}`,
    group: "Show configuration",
    title: configLabel(c) + (c === activeConfig ? "  (current)" : ""),
    run: () => pickConfig(i),
  }));
  for (const p of recentFiles()) {
    if (p === currentFile) continue;
    out.push({ id: `recent:${p}`, group: "Open recent", title: fileName(p), keywords: p, run: () => void openPath(p) });
  }
  for (const ex of EXAMPLES) {
    out.push({ id: `example:${ex.name}`, group: "Open example", title: ex.title, keywords: ex.description, run: () => void openText(ex.name, ex.text, "example") });
  }
  return out;
}

const isModal = () =>
  palette.isOpen || !guideOverlay.classList.contains("hidden") || !keysOverlay.classList.contains("hidden");
cmds.install((reason) => toast(reason), isModal);
annotateKeys(cmds);
$("#paletteKbd").textContent = showKey("Mod+K");
$("#hintPal").textContent = showKey("Mod+K");

// Esc backs out of whatever is on top: dialogs, then popovers, then a running job.
window.addEventListener("keydown", (e) => {
  if (e.key !== "Escape" || palette.isOpen) return;
  if (!keysOverlay.classList.contains("hidden")) closeShortcuts();
  else if (!guideOverlay.classList.contains("hidden")) closeGuide();
  else if (POPOVERS.some(([pop]) => pop.classList.contains("show"))) closePopovers();
  else if (jobs.size) cancelJobs();
  else return;
  e.preventDefault();
});

renderEmpty(els);
renderDiags([]);
showWelcome();
installTooltips();
refreshUi();
installSplitters({
  main,
  center,
  left: $("#left"),
  right: $("#right"),
  openEditor: () => center.classList.add("editor-open"),
});

// Prompt before the window closes with unsaved edits in the buffer.
void guardWindowClose(() => dirty, "You have unsaved changes. Close anyway?");
