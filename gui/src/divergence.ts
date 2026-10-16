// Where two compared LTSs diverge, computed client-side from an `equiv`
// document's joint class partition (§6). A class with states on only one side is
// behaviour the other side can't match; the nearest such state to a side's
// initial state, with the path that reaches it, says where the two first differ.
//
// This is not a distinguishing formula (that needs the engine's refinement
// history), but the path is a real run of that side ending in a state with no
// equivalent on the other side.

import type { LtsDocument, NodeSummary } from "./contract";

export interface Step {
  from: number;
  to: number;
  label: string;
}

export interface UnmatchedClass {
  classId: number;
  /** Its states, nearest to the side's initial state first. */
  nodes: NodeSummary[];
}

export interface SideInfo {
  origin: number;
  initial?: NodeSummary;
  /** States of the other side in the same class as this side's initial state. */
  initialPeers: NodeSummary[];
  unmatched: UnmatchedClass[];
  unmatchedCount: number;
  /** Nearest unmatched state and the path from the initial state to it. */
  first?: { node: NodeSummary; path: Step[] };
}

export interface Divergence {
  sides: [SideInfo, SideInfo];
  /** Ids (as strings) of every unmatched state, for dimming the rest. */
  unmatchedIds: Set<string>;
  /** Initial states are in the same class. */
  initialsMatch: boolean;
}

/** `null` when the document carries no classes (not an equiv document). */
export function analyzeDivergence(doc: LtsDocument): Divergence | null {
  const nodes = doc.graph.nodes;
  if (!nodes.some((n) => n.classId != null)) return null;
  const byId = new Map(nodes.map((n) => [n.id, n]));
  const classesOn = [new Set<number>(), new Set<number>()];
  for (const n of nodes) if (n.classId != null) classesOn[n.origin ?? 0].add(n.classId);

  // Adjacency with a readable label per edge (multi-action edges joined).
  const out = new Map<number, { to: number; label: string }[]>();
  for (const e of doc.graph.edges) {
    const label = e.actions.map((a) => a.label).join(" | ");
    (out.get(e.from) ?? out.set(e.from, []).get(e.from)!).push({ to: e.to, label });
  }

  const unmatchedIds = new Set<string>();
  const side = (origin: number): SideInfo => {
    const other = classesOn[1 - origin];
    const initial =
      nodes.find((n) => n.origin === origin && doc.graph.initial.includes(n.id)) ??
      nodes.find((n) => n.origin === origin && n.initial);
    // BFS from the initial state: distance order + parent pointers for paths.
    const order: number[] = [];
    const parent = new Map<number, Step | null>();
    if (initial) {
      parent.set(initial.id, null);
      order.push(initial.id);
      for (let i = 0; i < order.length; i++) {
        for (const { to, label } of out.get(order[i]) ?? []) {
          if (parent.has(to)) continue;
          parent.set(to, { from: order[i], to, label });
          order.push(to);
        }
      }
    }
    // States the BFS didn't reach (shouldn't happen) still count, after the rest.
    for (const n of nodes) if (n.origin === origin && !parent.has(n.id)) order.push(n.id);

    const classes = new Map<number, NodeSummary[]>();
    for (const id of order) {
      const n = byId.get(id)!;
      if (n.classId == null || other.has(n.classId)) continue;
      unmatchedIds.add(String(id));
      (classes.get(n.classId) ?? classes.set(n.classId, []).get(n.classId)!).push(n);
    }
    const unmatched = [...classes].map(([classId, ns]) => ({ classId, nodes: ns }));
    let first: SideInfo["first"];
    if (unmatched.length) {
      const node = unmatched[0].nodes[0]; // classes are in first-reached order
      const path: Step[] = [];
      for (let s = parent.get(node.id); s; s = parent.get(s.from)) path.unshift(s);
      first = { node, path };
    }
    const initialPeers = initial?.classId != null ? nodes.filter((n) => n.origin === 1 - origin && n.classId === initial.classId) : [];
    return {
      origin,
      initial,
      initialPeers,
      unmatched,
      unmatchedCount: unmatched.reduce((k, c) => k + c.nodes.length, 0),
      first,
    };
  };

  const a = side(0);
  const b = side(1);
  return {
    sides: [a, b],
    unmatchedIds,
    initialsMatch: a.initial?.classId != null && a.initial.classId === b.initial?.classId,
  };
}
