// Side-by-side compare (§6): one `equiv` document → two panes, each rooted at one
// initial state (split by NodeSummary.origin), nodes colored by shared classId so
// equivalent states match across panes. Hovering/selecting a node lights up its
// class-mates in BOTH panes.

import type { Graph as LtsGraph, LtsDocument, NodeSummary } from "./contract";
import { buildGraph, makeController, type Controller } from "./graph";

export interface CompareHandle {
  destroy(): void;
  fit(): void;
  setShowTauLabels(show: boolean): void;
  setShowNormTags(show: boolean): void;
  setHighlightTags(keys: Set<string> | null): void;
  setStretch(sx: number, sy: number): void;
  restoreLayout(): void;
  setDimmed(keep: Set<string> | null): void;
  setPath(nodes: Set<string> | null, edges: Set<string> | null): void;
  /** Select a state (as if clicked) and bring it into view in its pane. */
  reveal(node: NodeSummary): void;
  /** Highlight a state and bring it into view, without selecting it (the inspector stays). */
  focus(node: NodeSummary): void;
  /** Clear the selection. */
  deselect(): void;
}

export interface CompareHooks {
  onSelect: (id: number | null, node: NodeSummary | null) => void;
}

/** Nodes for one side of the comparison; edges auto-filter in buildGraph (their
 * missing endpoints are skipped), so cross-origin edges never appear. */
function side(g: LtsGraph, origin: number): LtsGraph {
  return { ...g, nodes: g.nodes.filter((n) => n.origin === origin) };
}

export function renderCompare(
  elA: HTMLElement,
  elB: HTMLElement,
  doc: LtsDocument,
  hooks: CompareHooks,
  colorByClass = true,
): CompareHandle {
  const g = doc.graph;
  const graphA = buildGraph(side(g, 0), doc.pos, colorByClass);
  const graphB = buildGraph(side(g, 1), doc.pos, colorByClass);

  let ctrlA: Controller;
  let ctrlB: Controller;

  // Class of the currently selected node; hovering overrides it transiently, and
  // leaving a node falls back to it so the selection highlight persists (§6).
  let selectedClass: number | null = null;
  const highlight = (classId: number | null) => {
    ctrlA.setHighlightClass(classId);
    ctrlB.setHighlightClass(classId);
  };

  const paneHooks = {
    onSelect: (id: number | null, node: NodeSummary | null) => {
      ctrlA.setSelected(id);
      ctrlB.setSelected(id);
      selectedClass = node?.classId ?? null;
      highlight(selectedClass);
      hooks.onSelect(id, node);
    },
    onHover: (node: NodeSummary | null) => highlight(node?.classId ?? selectedClass),
  };

  ctrlA = makeController(elA, graphA, paneHooks);
  ctrlB = makeController(elB, graphB, paneHooks);

  const fit = () => {
    ctrlA.fit();
    ctrlB.fit();
  };
  setTimeout(fit, 30);

  return {
    fit,
    setShowTauLabels(show) {
      ctrlA.setShowTauLabels(show);
      ctrlB.setShowTauLabels(show);
    },
    setShowNormTags(show) {
      ctrlA.setShowNormTags(show);
      ctrlB.setShowNormTags(show);
    },
    setHighlightTags(keys) {
      ctrlA.setHighlightTags(keys);
      ctrlB.setHighlightTags(keys);
    },
    setStretch(sx, sy) {
      ctrlA.setStretch(sx, sy);
      ctrlB.setStretch(sx, sy);
    },
    restoreLayout() {
      ctrlA.restoreLayout();
      ctrlB.restoreLayout();
    },
    setDimmed(keep) {
      ctrlA.setDimmed(keep);
      ctrlB.setDimmed(keep);
    },
    setPath(nodes, edges) {
      ctrlA.setPath(nodes, edges);
      ctrlB.setPath(nodes, edges);
    },
    reveal(node) {
      paneHooks.onSelect(node.id, node);
      (ctrlA.hasNode(node.id) ? ctrlA : ctrlB).focusNode(node.id);
    },
    focus(node) {
      ctrlA.setSelected(node.id);
      ctrlB.setSelected(node.id);
      selectedClass = node.classId ?? null;
      highlight(selectedClass);
      (ctrlA.hasNode(node.id) ? ctrlA : ctrlB).focusNode(node.id);
    },
    deselect() {
      ctrlA.setSelected(null);
      ctrlB.setSelected(null);
      selectedClass = null;
      highlight(null);
    },
    destroy() {
      ctrlA.destroy();
      ctrlB.destroy();
    },
  };
}
