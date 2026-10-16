// Editor bottom rail: CodeMirror 6, editable. Provides QUARC syntax highlighting, a
// live `check`-driven linter (squiggles), change notifications (so main can
// re-eval from the buffer), and forward source linking — revealSpan() scrolls to
// and highlights a node/edge's `loc` (§5.2, §5.3).

import { defaultKeymap, history, historyKeymap } from "@codemirror/commands";
import { HighlightStyle, StreamLanguage, syntaxHighlighting } from "@codemirror/language";
import { type Diagnostic, linter, lintGutter } from "@codemirror/lint";
import { EditorSelection, EditorState, StateEffect, StateField } from "@codemirror/state";
import {
  Decoration,
  type DecorationSet,
  EditorView,
  drawSelection,
  highlightActiveLine,
  keymap,
  lineNumbers,
} from "@codemirror/view";
import { tags } from "@lezer/highlight";
import type { JsonDiagnostic, SourceSpan } from "./contract";
import { quarcStartState, quarcToken } from "./quarcSyntax";

// ── QUARC stream tokenizer (shared with the inspector process boxes) ──
const quarc = StreamLanguage.define({ startState: quarcStartState, token: quarcToken });

const hs = HighlightStyle.define([
  { tag: tags.keyword, color: "var(--accent)", fontWeight: "600" },
  { tag: tags.comment, color: "var(--ink-3)", fontStyle: "italic" },
  { tag: tags.className, color: "var(--c3)" }, // process names
  { tag: tags.typeName, color: "var(--c5)" }, // types
  { tag: tags.labelName, color: "var(--warm)" }, // tags
  { tag: tags.atom, color: "var(--c4)" }, // qubit constants
  { tag: tags.macroName, color: "var(--c2)" }, // fixed actions (disc/tau)
  { tag: tags.operatorKeyword, color: "var(--c6)" }, // superoperators
  { tag: tags.controlKeyword, color: "var(--c7)" }, // measurements
  { tag: tags.number, color: "var(--ink-2)" },
  { tag: tags.operator, color: "var(--ink-2)" },
]);

const theme = EditorView.theme({
  "&": { backgroundColor: "transparent", color: "var(--ink)", height: "100%" },
  ".cm-scroller": { fontFamily: "var(--mono)", fontSize: "12px", lineHeight: "1.5" },
  ".cm-content": { caretColor: "var(--accent)" },
  ".cm-gutters": { backgroundColor: "transparent", color: "var(--ink-3)", border: "none" },
  ".cm-activeLine": { backgroundColor: "var(--bg-grid)" },
  ".cm-activeLineGutter": { backgroundColor: "transparent", color: "var(--accent)" },
  ".cm-link-hl": { backgroundColor: "rgba(239, 179, 68, 0.22)", boxShadow: "inset 2px 0 0 var(--warm)" },
  "&.cm-focused": { outline: "none" },
});

// ── forward-link highlight (revealSpan) ──
const setHl = StateEffect.define<{ from: number; to: number } | null>();
const hlMark = Decoration.mark({ class: "cm-link-hl" });
const hlField = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update(deco, tr) {
    deco = deco.map(tr.changes);
    for (const e of tr.effects) {
      if (e.is(setHl)) deco = e.value ? Decoration.set([hlMark.range(e.value.from, e.value.to)]) : Decoration.none;
    }
    return deco;
  },
  provide: (f) => EditorView.decorations.from(f),
});

export interface EditorOpts {
  /** Typecheck the current buffer; drives the linter squiggles. */
  check: (text: string) => Promise<JsonDiagnostic[]>;
  /** Fired on user edits (not programmatic setSource); main debounces re-eval. */
  onDocChange: (text: string) => void;
  /** Fired with each typecheck's diagnostics, for the side strip + status count. */
  onDiagnostics?: (diags: JsonDiagnostic[]) => void;
}

export interface Editor {
  setSource(text: string, fileName?: string): void;
  getText(): string;
  revealSpan(span: SourceSpan): void;
  /** Insert a top-level definition, placed before the first `start` block (defs
   * must precede start blocks); reveals and selects the inserted text. */
  insertDefinition(text: string): void;
}

export function makeEditor(host: HTMLElement, titleEl: HTMLElement, opts: EditorOpts): Editor {
  let programmatic = false;

  const lintSource = linter(
    async (view): Promise<Diagnostic[]> => {
      const text = view.state.doc.toString();
      if (!text.trim()) {
        opts.onDiagnostics?.([]);
        return [];
      }
      const diags = await opts.check(text);
      opts.onDiagnostics?.(diags);
      return diags.map((d) => toCM(view.state, d));
    },
    { delay: 400 },
  );

  const changeListener = EditorView.updateListener.of((u) => {
    if (u.docChanged && !programmatic) opts.onDocChange(u.state.doc.toString());
  });

  const view = new EditorView({
    parent: host,
    state: EditorState.create({
      doc: "",
      extensions: [
        lineNumbers(),
        history(),
        drawSelection(),
        highlightActiveLine(),
        keymap.of([...defaultKeymap, ...historyKeymap]),
        quarc,
        syntaxHighlighting(hs),
        theme,
        hlField,
        lintGutter(),
        lintSource,
        changeListener,
      ],
    }),
  });

  return {
    setSource(text, fileName) {
      titleEl.textContent = fileName ? `editor · ${fileName}` : "editor";
      programmatic = true;
      view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: text } });
      programmatic = false;
    },
    getText: () => view.state.doc.toString(),
    revealSpan(span) {
      const { from, to } = spanToRange(view.state, span);
      view.dispatch({
        selection: EditorSelection.single(from),
        effects: [setHl.of({ from, to }), EditorView.scrollIntoView(from, { y: "center" })],
      });
    },
    insertDefinition(text) {
      const at = startBlockOffset(view.state);
      const block = (at > 0 ? "\n" : "") + text + "\n\n";
      view.dispatch({
        changes: { from: at, insert: block },
        selection: EditorSelection.single(at, at + block.length),
        effects: EditorView.scrollIntoView(at, { y: "center" }),
      });
      view.focus();
    },
  };
}

// Offset just before the first `start` block (defs precede start blocks); falls
// back to end-of-document when there is no start block.
function startBlockOffset(state: EditorState): number {
  for (let i = 1; i <= state.doc.lines; i++) {
    const line = state.doc.line(i);
    if (/^\s*start\b/.test(line.text)) return line.from;
  }
  return state.doc.length;
}

// ── span/offset mapping (engine spans are 1-based line/col) ──
function lineColToOffset(state: EditorState, line: number, col: number): number {
  const ln = state.doc.line(Math.min(Math.max(line, 1), state.doc.lines));
  return Math.min(ln.from + Math.max(col - 1, 0), ln.to);
}

function spanToRange(state: EditorState, span: SourceSpan): { from: number; to: number } {
  const from = lineColToOffset(state, span.start.line, span.start.col);
  // The engine's end column is inclusive (Parser.Internal trackExtent records
  // ec-1, the token's last column); CodeMirror ranges are end-exclusive, so
  // advance one past it — clamped to the end line so we never cross the newline.
  const endLine = state.doc.line(Math.min(Math.max(span.end.line, 1), state.doc.lines));
  let to = Math.min(endLine.from + span.end.col, endLine.to);
  if (to <= from) to = Math.min(from + 1, state.doc.length);
  return { from, to };
}

function toCM(state: EditorState, d: JsonDiagnostic): Diagnostic {
  const { from, to } = d.span ? spanToRange(state, d.span) : { from: 0, to: Math.min(1, state.doc.length) };
  return { from, to, severity: d.severity === "error" ? "error" : "warning", message: d.message };
}
