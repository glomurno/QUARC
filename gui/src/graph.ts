// Graph rendering: graphology model + Sigma WebGL renderer. Ported from the
// mockup's buildGraph + makeController (gui-mockup/index.html:694, :736). The only
// data difference is that `graph`/`pos` now come from a live `eval` response.

import Graph from "graphology";
import EdgeCurveProgram from "@sigma/edge-curve";
import Sigma from "sigma";
import type { ActionLabel, Graph as LtsGraph, NodeSummary } from "./contract";
import { actionColor, classColor, cssv, isTauAct } from "./theme";

export interface Controller {
  sigma: Sigma;
  setSelected(id: number | null): void;
  /** Emphasize every node sharing this equivalence class (compare panes, §6). */
  setHighlightClass(classId: number | null): void;
  /** Emphasize the τ edges whose tag key (see silence.ts `tagKey`) is in the set. */
  setHighlightTags(keys: Set<string> | null): void;
  /** Hide τ (internal-action) edge labels; the edges themselves stay (§4.3). */
  setShowTauLabels(show: boolean): void;
  /** Label actions with their tag-equivalence-normalized tags. */
  setShowNormTags(show: boolean): void;
  /** Rescale the layout from the raw graphviz coords by (sx, sy). Resets any
   * node drags to the new home positions; does not move the camera. */
  setStretch(sx: number, sy: number): void;
  /** Snap every node back to the server layout (`_x0`/`_y0`) and refit (§4.2). */
  restoreLayout(): void;
  fit(): void;
  /** Grey out every node not in `keep` (null = no dimming). */
  setDimmed(keep: Set<string> | null): void;
  /** Emphasize a path: its nodes and its `from>to` edges (null = none). */
  setPath(nodes: Set<string> | null, edges: Set<string> | null): void;
  /** Pan/zoom the camera onto a node. */
  focusNode(id: number): void;
  /** Whether this pane draws the node. */
  hasNode(id: number): boolean;
  destroy(): void;
}

export interface ControllerHooks {
  onSelect: (id: number | null, node: NodeSummary | null) => void;
  /** Fired on node hover; used to sync class highlighting across compare panes. */
  onHover?: (node: NodeSummary | null) => void;
}

/**
 * Build a graphology multigraph from an LTS graph + position map. Each
 * ActionLabel becomes its own edge; parallel/back edges fan out as curves.
 * Graphviz plain and Sigma are both y-up, so positions pass through unflipped.
 */
// The server (Api.Layout) lays out top-to-bottom, so depth runs down the y axis
// and same-depth states spread along x. A wide rank (many states at one depth)
// makes the graph wide and short; once fit to the landscape canvas the ranks
// bunch up vertically and crowd the transition labels between them. The raw
// graphviz coordinates are stored per node (`_px`/`_py`); the live x/y are those
// scaled by a user-adjustable stretch (Controller.setStretch, driven by the X/Y
// sliders in the toolbar). Edges are drawn client-side, so stretching only moves
// nodes apart — it never desyncs splines. These are the sane starting values.
export const DEFAULT_STRETCH_X = 1.0;
export const DEFAULT_STRETCH_Y = 1.6;

/** The engine's graphviz cap (`Api.Layout.layout_max_nodes`); above it, positions come back empty. */
export const LAYOUT_MAX_NODES = 5000;

/** Whether the engine skipped graphviz (it does above its layout node cap). */
export const layoutSkipped = (g: LtsGraph, pos: Record<string, [number, number]>): boolean =>
  g.nodes.length > 0 && Object.keys(pos).length === 0;

/** Cheap layered fallback for graphs graphviz didn't lay out: rank = BFS depth
 * from the initial states, spread evenly within each rank (graphviz units, y-up). */
export function layeredLayout(g: LtsGraph): Record<string, [number, number]> {
  const out = new Map<number, number[]>();
  for (const e of g.edges) (out.get(e.from) ?? out.set(e.from, []).get(e.from)!).push(e.to);
  const depth = new Map<number, number>();
  const roots = g.initial.length ? g.initial : g.nodes.filter((n) => n.initial).map((n) => n.id);
  const queue = [...roots];
  roots.forEach((r) => depth.set(r, 0));
  for (let i = 0; i < queue.length; i++) {
    const d = depth.get(queue[i])!;
    for (const t of out.get(queue[i]) ?? []) {
      if (!depth.has(t)) {
        depth.set(t, d + 1);
        queue.push(t);
      }
    }
  }
  const ranks = new Map<number, number[]>();
  const maxDepth = Math.max(0, ...depth.values());
  for (const n of g.nodes) {
    const d = depth.get(n.id) ?? maxDepth + 1; // unreachable: one extra rank
    (ranks.get(d) ?? ranks.set(d, []).get(d)!).push(n.id);
  }
  const pos: Record<string, [number, number]> = {};
  for (const [d, ids] of ranks) ids.forEach((id, i) => (pos[String(id)] = [(i - (ids.length - 1) / 2) * 40, -d * 60]));
  return pos;
}

export function buildGraph(
  g: LtsGraph,
  pos: Record<string, [number, number]>,
  colorByClass: boolean,
): Graph {
  const graph = new Graph({ multi: true, type: "directed" });
  if (layoutSkipped(g, pos)) pos = layeredLayout(g);

  g.nodes.forEach((n) => {
    const p = pos[String(n.id)] ?? [0, 0]; // raw graphviz coords (both y-up)
    const x = p[0] * DEFAULT_STRETCH_X;
    const y = p[1] * DEFAULT_STRETCH_Y;
    let color = cssv("--ink-2");
    if (colorByClass && n.classId != null) color = classColor(n.classId);
    else if (n.initial) color = cssv("--accent");
    graph.addNode(String(n.id), {
      x,
      y,
      _px: p[0], // raw coords, kept so setStretch can rescale from source
      _py: p[1],
      _x0: x, // current-stretch home position (restoreLayout / drag reset)
      _y0: y,
      size: (n.initial ? 6 : 4.5) + Math.min(n.branches, 3) * 0.6,
      // label: `s${n.id}`,
      color,
      _node: n,
    });
  });

  // explode edges → one unit per action
  type Unit = {
    from: number;
    to: number;
    a: LtsGraph["edges"][number]["actions"][number];
    labels?: { orig: string; norm: string };
  };
  const units: Unit[] = [];
  g.edges.forEach((e) => {
    if (!graph.hasNode(String(e.from)) || !graph.hasNode(String(e.to))) return;
    e.actions.forEach((a, i) => units.push({ from: e.from, to: e.to, a, labels: e.labels?.[i] }));
  });

  // group by unordered pair → distribute curvature so parallels/back-edges separate
  const groups: Record<string, Unit[]> = {};
  units.forEach((u) => {
    const k = [u.from, u.to]
      .slice()
      .sort((a, b) => a - b)
      .join("~");
    (groups[k] = groups[k] ?? []).push(u);
  });
  Object.values(groups).forEach((list) => {
    const n = list.length;
    const multi = n > 1;
    const spread = 0.55;
    list.forEach((u, i) => {
      let curvature = 0;
      if (multi) {
        curvature = ((i - (n - 1) / 2) / (n - 1 || 1)) * spread;
        if (u.from > u.to) curvature = -curvature;
        if (Math.abs(curvature) < 1e-3) curvature = 0.04;
      }
      graph.addEdgeWithKey(
        `e_${u.from}_${u.to}_${i}`,
        String(u.from),
        String(u.to),
        {
          size: 1.6,
          type: multi ? "curved" : "arrow",
          curvature,
          color: cssv("--line-2"),
          label: u.a.label,
          forceLabel: true,
          _action: u.a,
          _labels: u.labels,
        },
      );
    });
  });

  return graph;
}

/** Tag key of a τ action (matches silence.ts `tagKey`); "" for visible actions. */
function actionTagKey(a: ActionLabel): string {
  if (a.kind === "tau") return `${a.tag}`;
  if (a.kind === "tau2") return `${a.sendTag},${a.recvTag}`;
  return "";
}

/** Attach a Sigma renderer to a container with selection + hover + dragging. */
export function makeController(
  container: HTMLElement,
  graph: Graph,
  hooks: ControllerHooks,
): Controller {
  // [width, height] of the current node bounding box (graph coords). Matches the
  // extent Sigma feeds its autoRescale normalization; used by setStretch.
  const bboxExtent = (): [number, number] => {
    let minX = Infinity;
    let maxX = -Infinity;
    let minY = Infinity;
    let maxY = -Infinity;
    graph.forEachNode((_n, a) => {
      if (a.x < minX) minX = a.x;
      if (a.x > maxX) maxX = a.x;
      if (a.y < minY) minY = a.y;
      if (a.y > maxY) maxY = a.y;
    });
    return [maxX - minX, maxY - minY];
  };

  const ui: {
    hoverNode: string | null;
    selected: string | null;
    highlightClass: number | null;
    highlightTags: Set<string> | null;
    showTauLabels: boolean;
    showNormTags: boolean;
    keep: Set<string> | null;
    pathNodes: Set<string> | null;
    pathEdges: Set<string> | null;
  } = {
    hoverNode: null,
    selected: null,
    highlightClass: null,
    highlightTags: null,
    showTauLabels: true,
    showNormTags: false,
    keep: null,
    pathNodes: null,
    pathEdges: null,
  };

  const sigma = new Sigma(graph, container, {
    defaultEdgeType: "arrow",
    edgeProgramClasses: { curved: EdgeCurveProgram },
    renderEdgeLabels: true,
    enableEdgeEvents: true,
    // Node radius in px = size / zoomToSizeRatioFunction(cameraRatio). Sigma's
    // default (Math.sqrt) grows circles as 1/√ratio on zoom-in, while edge/node
    // labels stay a fixed pixel size, so zooming to read transition labels lets
    // the circles swallow them. Dampen zoom-in growth to a gentle 4th-root while
    // keeping the default √ shrink on zoom-out (ratio > 1) for decluttering.
    zoomToSizeRatioFunction: (ratio: number) => (ratio < 1 ? Math.pow(ratio, 0.25) : Math.sqrt(ratio)),
    labelFont: "monospace",
    labelSize: 11,
    labelColor: { color: cssv("--ink-2") },
    edgeLabelFont: "monospace",
    edgeLabelSize: 10,
    edgeLabelColor: { color: cssv("--ink-3") },
    labelRenderedSizeThreshold: 0,
    nodeReducer: (node, data) => {
      const r = { ...data };
      const n = data._node as NodeSummary | undefined;
      if (ui.keep && !ui.keep.has(node)) {
        r.color = cssv("--ink-3");
        r.size = data.size * 0.75;
        r.zIndex = 0;
      }
      if (ui.pathNodes?.has(node)) {
        r.size = data.size + 1.5;
        r.zIndex = 2;
        r.forceLabel = true;
      }
      if (ui.highlightClass != null && n?.classId === ui.highlightClass) {
        r.size = data.size + 2;
        r.zIndex = 2;
        r.forceLabel = true;
      }
      if (ui.selected === node) {
        r.color = cssv("--warm");
        r.size = data.size + 2.5;
        r.zIndex = 3;
        r.forceLabel = true;
      }
      return r;
    },
    edgeReducer: (edge, data) => {
      const r = { ...data };
      const [s, t] = graph.extremities(edge);
      const near =
        ui.hoverNode != null && (s === ui.hoverNode || t === ui.hoverNode);
      const sel =
        ui.selected != null && (s === ui.selected || t === ui.selected);
      r.color = near || sel ? actionColor(data._action) : cssv("--line-2");
      if (near || sel) {
        r.size = 2.6;
        r.zIndex = 1;
      }
      if (ui.pathEdges?.has(`${s}>${t}`)) {
        r.color = cssv("--warm");
        r.size = 3.2;
        r.zIndex = 3;
      }
      if (ui.highlightTags && ui.highlightTags.has(actionTagKey(data._action))) {
        r.color = cssv("--warm");
        r.size = 3;
        r.zIndex = 2;
      }
      const lbls = data._labels as { orig: string; norm: string } | undefined;
      if (lbls) r.label = ui.showNormTags ? lbls.norm : lbls.orig;
      if (!ui.showTauLabels && isTauAct(data._action)) r.label = "";
      return r;
    },
  });

  let dragNode: string | null = null;
  sigma.on("clickNode", ({ node }) =>
    hooks.onSelect(+node, graph.getNodeAttribute(node, "_node") as NodeSummary),
  );
  sigma.on("clickStage", () => hooks.onSelect(null, null));
  sigma.on("enterNode", ({ node }) => {
    ui.hoverNode = node;
    if (!dragNode) container.style.cursor = "grab";
    hooks.onHover?.(graph.getNodeAttribute(node, "_node") as NodeSummary);
    sigma.refresh({ skipIndexation: true });
  });
  sigma.on("leaveNode", () => {
    ui.hoverNode = null;
    if (!dragNode) container.style.cursor = "";
    hooks.onHover?.(null);
    sigma.refresh({ skipIndexation: true });
  });

  // node dragging — writes back to graphology coords (client-only)
  const mc = sigma.getMouseCaptor();
  sigma.on("downNode", ({ node }) => {
    dragNode = node;
    if (!sigma.getCustomBBox()) sigma.setCustomBBox(sigma.getBBox());
    container.style.cursor = "grabbing";
  });
  mc.on("mousemovebody", (e) => {
    if (!dragNode) return;
    const p = sigma.viewportToGraph(e);
    graph.setNodeAttribute(dragNode, "x", p.x);
    graph.setNodeAttribute(dragNode, "y", p.y);
    e.preventSigmaDefault();
    e.original.preventDefault();
    e.original.stopPropagation();
  });
  mc.on("mouseup", () => {
    if (dragNode) {
      dragNode = null;
      container.style.cursor = "";
    }
  });

  // Sigma only listens to window resizes; follow the container too, so dragging
  // a pane splitter or opening the editor re-sizes the canvas instead of clipping it.
  const resizeObs = new ResizeObserver(() => sigma.scheduleRefresh());
  resizeObs.observe(container);

  return {
    sigma,
    setSelected(id) {
      ui.selected = id == null ? null : String(id);
      sigma.refresh({ skipIndexation: true });
    },
    setHighlightClass(classId) {
      ui.highlightClass = classId;
      sigma.refresh({ skipIndexation: true });
    },
    setHighlightTags(keys) {
      ui.highlightTags = keys;
      sigma.refresh({ skipIndexation: true });
    },
    setShowTauLabels(show) {
      ui.showTauLabels = show;
      sigma.refresh({ skipIndexation: true });
    },
    setShowNormTags(show) {
      ui.showNormTags = show;
      sigma.refresh({ skipIndexation: true });
    },
    setStretch(sx, sy) {
      // Sigma (autoRescale) normalizes by ratio = max(bboxW, bboxH), so growing
      // the bbox shrinks the whole graph uniformly — stretching one axis reads as
      // the other axis being squished. Counteract by scaling the camera zoom by
      // the change in that divisor, so the on-screen scale is preserved and the
      // graph simply overflows the view instead of rescaling.
      const before = Math.max(...bboxExtent()) || 1;
      graph.forEachNode((node, attr) => {
        const x = attr._px * sx;
        const y = attr._py * sy;
        graph.mergeNodeAttributes(node, { x, y, _x0: x, _y0: y });
      });
      const after = Math.max(...bboxExtent()) || 1;
      const cam = sigma.getCamera();
      const state = cam.getState();
      cam.setState({ ...state, ratio: (state.ratio * before) / after });
      sigma.refresh(); // positions moved → reindex for hover/click hit-testing
    },
    restoreLayout() {
      graph.forEachNode((node, attr) => {
        graph.setNodeAttribute(node, "x", attr._x0);
        graph.setNodeAttribute(node, "y", attr._y0);
      });
      sigma.getCamera().animatedReset({ duration: 300 });
    },
    fit() {
      sigma.getCamera().animatedReset({ duration: 300 });
    },
    setDimmed(keep) {
      ui.keep = keep;
      sigma.refresh({ skipIndexation: true });
    },
    setPath(nodes, edges) {
      ui.pathNodes = nodes;
      ui.pathEdges = edges;
      sigma.refresh({ skipIndexation: true });
    },
    focusNode(id) {
      const d = sigma.getNodeDisplayData(String(id));
      if (!d) return;
      const cam = sigma.getCamera();
      void cam.animate({ x: d.x, y: d.y, ratio: Math.min(cam.getState().ratio, 0.6) }, { duration: 350 });
    },
    hasNode(id) {
      return graph.hasNode(String(id));
    },
    destroy() {
      resizeObs.disconnect();
      sigma.kill();
    },
  };
}
