// TypeScript mirror of the `quarc.lts/1` contract (app/Api/Contract.hs) plus the
// JSON-RPC envelope used by the sidecar (app/Server/Protocol.hs). This is the
// single source of truth for the GUI↔engine boundary.

export interface SourceSpan {
  file: string;
  start: { line: number; col: number };
  end: { line: number; col: number };
}

export interface Chan {
  name: string;
  type: string;
}

export type ActionLabel =
  | { kind: "tau"; tag: number; label: string; loc?: SourceSpan }
  | {
      kind: "tau2";
      sendTag: number;
      recvTag: number;
      label: string;
      loc?: SourceSpan;
    }
  | {
      kind: "send";
      tag: number;
      channel: Chan;
      value: string;
      label: string;
      loc?: SourceSpan;
    }
  | {
      kind: "recv";
      tag: number;
      channel: Chan;
      value: string;
      label: string;
      loc?: SourceSpan;
    };

export interface Edge {
  from: number;
  to: number;
  actions: ActionLabel[];
  /** Per action: its label with the original tags and with normalized tags. */
  labels?: { orig: string; norm: string }[];
  bends?: { x: number; y: number }[];
}

export interface NodeSummary {
  id: number;
  dim: number; // Hilbert dimension = 2^qubits
  branches: number;
  procShort: string;
  initial?: boolean;
  classId?: number;
  origin?: number;
  loc?: SourceSpan;
}

export interface Graph {
  initial: number[];
  noFreeInputs: boolean;
  nodes: NodeSummary[];
  edges: Edge[];
}

/** Row-major complex matrix; each entry is `[re, im]`. */
export interface ComplexMatrix {
  rows: number;
  cols: number;
  data: [number, number][];
}

export interface SubState {
  prob: number;
  qstate: { dim: number; rho: ComplexMatrix };
  proc: string;
}

export interface NodeDetail {
  env: ComplexMatrix;
  dist: SubState[];
}

export type DocKind = "eval" | "minimize" | "equiv";

/** A tag: `n` for a single prefix, `[n, m]` for a synchronisation. */
export type TagRef = number | [number, number];

/** A τ tag occurring in the LTS, its tag-equivalence class, and whether it is
 * silent under branching bisimilarity. */
export interface TauTagInfo {
  tag: TagRef;
  norm: TagRef;
  silent: boolean;
  loc?: SourceSpan;
}

/** Which τ tags are silent (`silence` request param; replaces the file's blocks).
 * With `silent` empty every τ tag is silent, minus `visible`. */
export interface SilenceSpec {
  silent: TagRef[];
  visible: TagRef[];
}

export interface LtsDocument {
  schema: "quarc.lts/1";
  kind: DocKind;
  graph: Graph;
  classes?: number[][];
  equiv?: { bisimilar: boolean };
  bisimType?: string;
  /** node id → `[x, y]` (graphviz coords, y-up). */
  pos: Record<string, [number, number]>;
  /** node id → detail (empty for eval/equiv summaries; fetched via getState). */
  detail: Record<string, NodeDetail>;
  /** τ tags and their silence (minimize/equiv only). */
  tauTags?: TauTagInfo[];
}

export interface JsonDiagnostic {
  severity: string;
  message: string;
  span?: SourceSpan;
}

export interface RpcError {
  code: number;
  message: string;
  data?: { diagnostics?: JsonDiagnostic[] };
}

export interface RpcResponse {
  jsonrpc: "2.0";
  id: number | null;
  result?: unknown;
  error?: RpcError;
}

/** How untagged actions are tagged; the engine defaults to `branch`. */
export type TagPolicy = "branch" | "linear";

export type Source = ({ file: string } | { text: string; path: string }) & { tagging?: TagPolicy };

/** Result of `eval` / `minimize` / `equiv`. */
export interface DocumentResult {
  handle: number;
  document: LtsDocument;
}

/** Result of `check`. */
export interface CheckResult {
  diagnostics: JsonDiagnostic[];
}

/** Result of `outline`: a file's start-block names (the unnamed default block is
 * `null`), the raw source text for the editor rail, and its tag equivalences. */
export interface OutlineResult {
  configs: (string | null)[];
  source: string;
  tagEquiv: FileTagEquiv;
}

/** Which `tag-equiv` blocks a file declares: whether it has global ones, and the
 * config pairs (unordered) its scoped ones apply to. */
export interface FileTagEquiv {
  global: boolean;
  scopes: [string, string][];
}

/** Result of `export`: a rendered artifact (e.g. SVG text) + its format tag. */
/** Result of `export`: the payload (`data`), or the `path` it was written to. */
export interface ExportResult {
  format: string;
  data?: string;
  path?: string;
}
