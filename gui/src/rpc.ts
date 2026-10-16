// The single transport seam: the rest of the UI only ever calls `call(...)`.
// In Tauri, this spawns `quarc serve` once via the shell plugin and speaks
// newline-delimited JSON-RPC over its stdio. In a plain browser (`npm run dev`),
// it falls back to captured fixtures so the renderer can be iterated without the
// desktop shell.

import { Command, type Child } from "@tauri-apps/plugin-shell";
import type {
  CheckResult,
  DocumentResult,
  ExportResult,
  NodeDetail,
  OutlineResult,
  RpcError,
  RpcResponse,
  SilenceSpec,
  Source,
} from "./contract";

/** Native file picker for a `.quarc` file. In Tauri this is the OS open dialog and
 * returns the absolute path; in a plain browser it falls back to an `<input
 * type=file>` (name only — fine for the fixture-backed dev mode). `null` on cancel. */
export async function pickQuarcFile(): Promise<string | null> {
  if (inTauri()) {
    const { open } = await import("@tauri-apps/plugin-dialog");
    const sel = await open({
      multiple: false,
      directory: false,
      filters: [{ name: "QUARC", extensions: ["quarc"] }],
    });
    return typeof sel === "string" ? sel : null;
  }
  return new Promise((resolve) => {
    const inp = document.createElement("input");
    inp.type = "file";
    inp.accept = ".quarc";
    inp.addEventListener("change", () => resolve(inp.files?.[0]?.name ?? null));
    inp.addEventListener("cancel", () => resolve(null));
    inp.click();
  });
}

/** A file dropped on the window: a real path in Tauri, file contents in the browser. */
export type DroppedFile = { path: string } | { name: string; text: string };

/** Listen for files dragged onto the window. `onHover` toggles a drop hint;
 * `onDrop` gets the first dropped file. In Tauri the webview intercepts native
 * drops (HTML5 drop events never fire), so use its drag-drop event there. */
export async function onFileDrop(onHover: (over: boolean) => void, onDrop: (f: DroppedFile) => void): Promise<void> {
  if (inTauri()) {
    const { getCurrentWebview } = await import("@tauri-apps/api/webview");
    await getCurrentWebview().onDragDropEvent((ev) => {
      const p = ev.payload;
      if (p.type === "enter" || p.type === "over") onHover(true);
      else if (p.type === "leave") onHover(false);
      else if (p.type === "drop") {
        onHover(false);
        if (p.paths.length) onDrop({ path: p.paths[0] });
      }
    });
    return;
  }
  const hasFiles = (e: DragEvent) => e.dataTransfer?.types.includes("Files") ?? false;
  window.addEventListener("dragover", (e) => {
    if (!hasFiles(e)) return;
    e.preventDefault();
    onHover(true);
  });
  window.addEventListener("dragleave", (e) => {
    if (e.relatedTarget == null) onHover(false);
  });
  window.addEventListener("drop", (e) => {
    if (!hasFiles(e)) return;
    e.preventDefault();
    onHover(false);
    const file = e.dataTransfer?.files[0];
    if (file) void file.text().then((text) => onDrop({ name: file.name, text }));
  });
}

export function inTauri(): boolean {
  return typeof (window as unknown as { __TAURI_INTERNALS__?: unknown }).__TAURI_INTERNALS__ !== "undefined";
}

/** Ask the user to confirm discarding unsaved edits. Native dialog in Tauri;
 * `window.confirm` in the browser-dev fallback. Resolves `true` to proceed. */
export async function confirmDiscard(message: string): Promise<boolean> {
  if (inTauri()) {
    const { ask } = await import("@tauri-apps/plugin-dialog");
    return ask(message, { title: "Unsaved changes", kind: "warning" });
  }
  return window.confirm(message);
}

/** Intercept the native window close while `shouldConfirm()` is true, prompting
 * before discarding unsaved edits. No-op outside Tauri. */
export async function guardWindowClose(shouldConfirm: () => boolean, message: string): Promise<void> {
  if (!inTauri()) return;
  const { getCurrentWindow } = await import("@tauri-apps/api/window");
  await getCurrentWindow().onCloseRequested(async (event) => {
    if (shouldConfirm() && !(await confirmDiscard(message))) event.preventDefault();
  });
}

/** A request's progress: the phase it entered and, once known, its state count. */
export interface Progress {
  phase: string;
  states?: number;
}

/** Per-call options: progress callback and cancellation. */
export interface CallOpts {
  onProgress?: (p: Progress) => void;
  /** Aborting cancels the request in the engine (`cancel`) and rejects with `cancelled`. */
  signal?: AbortSignal;
}

/** JSON-RPC code for a cancelled request (as LSP's RequestCancelled). */
export const CANCELLED = -32800;
export const isCancelled = (e: unknown): boolean => (e as RpcError | undefined)?.code === CANCELLED;
const cancelledError = (): RpcError => ({ code: CANCELLED, message: "cancelled" });

interface Pending {
  resolve: (v: unknown) => void;
  reject: (e: RpcError) => void;
  onProgress?: (p: Progress) => void;
}

/** Owns the long-lived `quarc serve` child and correlates responses by id. */
class Sidecar {
  private child: Child | null = null;
  private starting: Promise<void> | null = null;
  private pending = new Map<number, Pending>();
  private nextId = 1;
  private buf = "";

  private ensure(): Promise<void> {
    if (this.child) return Promise.resolve();
    if (this.starting) return this.starting;
    this.starting = (async () => {
      // The engine's graph layout shells out to `dot`; it must be on the user's
      // PATH (the sidecar inherits this process's environment). Graphviz is a
      // documented runtime requirement — see the GUI README.
      const cmd = Command.sidecar("binaries/quarc", ["serve"]);
      cmd.stdout.on("data", (chunk: string) => this.onData(chunk));
      cmd.stderr.on("data", () => {
        /* engine logs on stderr; ignored for now */
      });
      cmd.on("close", () => {
        this.child = null;
        this.starting = null;
        this.failAll({ code: -1, message: "sidecar exited" });
      });
      this.child = await cmd.spawn();
    })();
    return this.starting;
  }

  // The shell plugin may emit per-line or in arbitrary chunks; handle both by
  // newline framing and, as a fallback, attempting to parse the whole buffer.
  private onData(chunk: string) {
    this.buf += chunk;
    let nl: number;
    while ((nl = this.buf.indexOf("\n")) >= 0) {
      const line = this.buf.slice(0, nl).trim();
      this.buf = this.buf.slice(nl + 1);
      if (line) this.handleLine(line);
    }
    const rest = this.buf.trim();
    if (rest) {
      try {
        const v = JSON.parse(rest) as RpcResponse;
        this.buf = "";
        this.dispatch(v);
      } catch {
        /* partial line; wait for more data */
      }
    }
  }

  private handleLine(line: string) {
    try {
      this.dispatch(JSON.parse(line) as RpcResponse);
    } catch (e) {
      console.error("rpc: unparseable line", line, e);
    }
  }

  private dispatch(res: RpcResponse) {
    if (res.id == null) {
      const note = res as unknown as { method?: string; params?: { id: number } & Progress };
      if (note.method === "progress" && note.params) {
        const { id, ...prog } = note.params;
        this.pending.get(id)?.onProgress?.(prog);
      } else if (res.error) console.error("rpc error (no id):", res.error);
      return;
    }
    const p = this.pending.get(res.id);
    if (!p) return;
    this.pending.delete(res.id);
    if (res.error) p.reject(res.error);
    else p.resolve(res.result);
  }

  private failAll(err: RpcError) {
    for (const p of this.pending.values()) p.reject(err);
    this.pending.clear();
  }

  async call<T>(method: string, params?: unknown, opts: CallOpts = {}): Promise<T> {
    if (opts.signal?.aborted) throw cancelledError();
    await this.ensure();
    const id = this.nextId++;
    const line = JSON.stringify({ jsonrpc: "2.0", id, method, params }) + "\n";
    return new Promise<T>((resolve, reject) => {
      this.pending.set(id, { resolve: resolve as (v: unknown) => void, reject, onProgress: opts.onProgress });
      // Reject at once (the UI shouldn't wait), and tell the engine to stop; its
      // own "cancelled" reply then finds no pending entry and is dropped.
      opts.signal?.addEventListener("abort", () => {
        if (!this.pending.delete(id)) return;
        reject(cancelledError());
        const cancel = JSON.stringify({ jsonrpc: "2.0", id: this.nextId++, method: "cancel", params: { id } }) + "\n";
        void this.child?.write(cancel).catch(() => {});
      });
      this.child!.write(line).catch((e) => reject({ code: -1, message: String(e) }));
    });
  }
}

const sidecar = new Sidecar();

const sleep = (ms: number, signal?: AbortSignal) =>
  new Promise<void>((resolve, reject) => {
    const t = setTimeout(resolve, ms);
    signal?.addEventListener("abort", () => {
      clearTimeout(t);
      reject(cancelledError());
    });
  });

// Simulated phases (ms) so progress + cancel are exercisable in browser-dev.
const FIXTURE_PHASES: Record<string, [string, number][]> = {
  eval: [["parse", 80], ["evaluate", 500], ["layout", 250]],
  minimize: [["parse", 80], ["evaluate", 500], ["minimize", 300], ["layout", 150]],
  equiv: [["parse", 80], ["evaluate", 900], ["layout", 250], ["compare", 400]],
  export: [["parse", 80], ["evaluate", 400], ["render", 300]],
};

async function fixtureCallWith<T>(method: string, params: unknown, opts: CallOpts): Promise<T> {
  for (const [phase, ms] of FIXTURE_PHASES[method] ?? []) {
    opts.onProgress?.({ phase, ...(phase === "evaluate" || phase === "parse" ? {} : { states: 47 }) });
    await sleep(ms, opts.signal);
  }
  if (opts.signal?.aborted) throw cancelledError();
  return fixtureCall<T>(method, params);
}

// Browser-dev fallback: serve captured `quarc serve` responses.
async function fixtureCall<T>(method: string, params?: unknown): Promise<T> {
  if (method === "eval") {
    const ev = (await import("./fixtures/eval.json")).default;
    return ev as unknown as T;
  }
  if (method === "getState") {
    const st = (await import("./fixtures/state.json")).default;
    return st as unknown as T;
  }
  if (method === "minimize") {
    const mn = (await import("./fixtures/minimize.json")).default;
    return mn as unknown as T;
  }
  if (method === "equiv") {
    const eq = (await import("./fixtures/equiv.json")).default;
    return eq as unknown as T;
  }
  if (method === "check") return { diagnostics: [] } as unknown as T;
  if (method === "save") return { ok: true } as unknown as T;
  if (method === "outline") {
    // Buffer outlines (examples, refresh): approximate the engine by scanning
    // for start blocks, so the stale-config flow is exercisable in browser-dev.
    const text = (params as { source?: { text?: string } })?.source?.text;
    if (text != null) {
      const configs = [...text.matchAll(/^\s*start\s*(?:"([^"]*)")?\s*\{/gm)].map((m) => m[1] ?? null);
      const blocks = [...text.matchAll(/^\s*tag-equiv\s*(?:"([^"]*)"\s*"([^"]*)")?\s*\{/gm)];
      const tagEquiv = {
        global: blocks.some((m) => m[1] == null),
        scopes: blocks.filter((m) => m[1] != null).map((m) => [m[1], m[2]]),
      };
      return { configs, source: text, tagEquiv } as unknown as T;
    }
    const ol = (await import("./fixtures/outline.json")).default;
    return ol as unknown as T;
  }
  if (method === "export") {
    if ((params as { format?: string })?.format === "png") {
      // 1×1 transparent PNG, base64 — enough to exercise the binary download path.
      const png =
        "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==";
      return { format: "png", data: png } as unknown as T;
    }
    const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="120" height="40"><text x="6" y="24" font-family="monospace" font-size="12">quarc export (dev)</text></svg>`;
    return { format: "svg", data: svg } as unknown as T;
  }
  throw { code: -1, message: `no fixture for ${method}` } as RpcError;
}

export function call<T>(method: string, params?: unknown, opts: CallOpts = {}): Promise<T> {
  return inTauri() ? sidecar.call<T>(method, params, opts) : fixtureCallWith<T>(method, params, opts);
}

// ─── typed helpers ───────────────────────────────────────────────────────────

export const evalSource = (source: Source, config?: string, flags?: string[], opts?: CallOpts) =>
  call<DocumentResult>("eval", { source, ...(config ? { config } : {}), ...(flags ? { flags } : {}) }, opts);

/** Behavioural equivalence used to minimize; the engine defaults to `strong`. */
export type Equivalence = "strong" | "branching";

export const minimizeSource = (
  source: Source,
  config?: string,
  flags?: string[],
  equivalence?: Equivalence,
  silence?: SilenceSpec | null,
  opts?: CallOpts,
) =>
  call<DocumentResult>(
    "minimize",
    {
      source,
      ...(config ? { config } : {}),
      ...(flags ? { flags } : {}),
      ...(equivalence ? { equivalence } : {}),
      ...(silence ? { silence } : {}),
    },
    opts,
  );

export const equivSource = (
  source: Source,
  config1: string,
  config2: string,
  bisimType?: string,
  flags?: string[],
  silence?: SilenceSpec | null,
  minimized?: boolean,
  opts?: CallOpts,
) =>
  call<DocumentResult>(
    "equiv",
    {
      source,
      config1,
      config2,
      ...(bisimType ? { bisimType } : {}),
      ...(flags ? { flags } : {}),
      ...(silence ? { silence } : {}),
      ...(minimized ? { minimized } : {}),
    },
    opts,
  );

export const exportSource = (
  source: Source,
  config: string | undefined,
  kind: string,
  format: string,
  flags?: string[],
  equivalence?: Equivalence,
  silence?: SilenceSpec | null,
  /** Write the image to this path (engine-side) instead of returning it. */
  path?: string,
  opts?: CallOpts,
) =>
  call<ExportResult>(
    "export",
    {
      source,
      ...(config ? { config } : {}),
      kind,
      format,
      ...(flags ? { flags } : {}),
      ...(equivalence ? { equivalence } : {}),
      ...(silence ? { silence } : {}),
      ...(path ? { path } : {}),
    },
    opts,
  );

/** Ask where to save an export. In Tauri: the native save dialog (path, or null
 * on cancel). In the browser: `undefined`, meaning "use a download instead" —
 * webviews don't support `<a download>`, so Tauri must write the file itself. */
export async function pickSavePath(defaultPath: string, filter: { name: string; extensions: string[] }): Promise<string | null | undefined> {
  if (!inTauri()) return undefined;
  const { save } = await import("@tauri-apps/plugin-dialog");
  return save({ defaultPath, filters: [filter] });
}

export const getState = (handle: number, id: number) =>
  call<NodeDetail>("getState", { handle, id });

export const check = (text: string, path: string) =>
  call<CheckResult>("check", { source: { text, path } });

export const outlineFile = (file: string) =>
  call<OutlineResult>("outline", { source: { file } });

/** Outline of an in-memory buffer (configs of the live editor text). */
export const outlineText = (text: string, path: string) =>
  call<OutlineResult>("outline", { source: { text, path } });

/** Write the editor buffer back to `path` on disk (via the `quarc serve` sidecar). */
export const saveFile = (path: string, text: string) =>
  call<{ ok: boolean }>("save", { path, text });
