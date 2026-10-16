// Shared QUARC tokenizer. One classifier drives both the CodeMirror editor
// (via StreamLanguage, see editor.ts) and the read-only process boxes in the
// inspector (via highlightQuarc). Token classes mirror src/LqCCS/Parser and the
// pretty-printed forms emitted by src/LqCCS/Pretty (τ for tau, `@Name$0` for
// tag-offset-instantiated calls, spaced `t7 : …` tags).

// Structural keywords; types, tags, qubit literals and the fixed actions
// disc/tau are tokenized separately, so they are excluded here.
export const KW =
  /^(start|process|proc|state|density|dm|if|then|else|import|with|tag_offset|tag-equiv|silent|visible|operator|measurement|effect|kraus|dim|controlled|adjoint|sequence|tensor|true|false)\b/;

export interface QuarcState {
  block: boolean; // inside an open {- … -} block comment
  afterProc: boolean; // last token was `proc`; the next name is a declaration
}

export const quarcStartState = (): QuarcState => ({ block: false, afterProc: false });

/** The subset of CodeMirror's StringStream the tokenizer relies on. The
 * editor passes a real StringStream; highlightQuarc passes {@link LineStream}. */
export interface TokenStream {
  string: string;
  pos: number;
  eol(): boolean;
  next(): string | void;
  eatSpace(): boolean;
  current(): string;
  match(re: RegExp): unknown; // truthy on match; advances pos past it
}

/** Classify the token at the stream cursor; returns a @lezer/highlight tag
 * name (also used as the `quarc-<name>` CSS class) or null for unstyled text. */
export function quarcToken(s: TokenStream, st: QuarcState): string | null {
  // block comment body, possibly spanning lines
  if (st.block) {
    while (!s.eol()) {
      if (s.match(/^-\}/)) {
        st.block = false;
        break;
      }
      s.next();
    }
    return "comment";
  }
  if (s.eatSpace()) return null;
  if (s.match(/^\{-/)) {
    st.block = true;
    while (!s.eol()) {
      if (s.match(/^-\}/)) {
        st.block = false;
        break;
      }
      s.next();
    }
    return "comment";
  }
  if (s.match(/^--(?!-).*/)) return "comment"; // line comment
  // declaration name after `proc` (pProcName)
  if (st.afterProc) {
    st.afterProc = false;
    if (s.match(/^[A-Z][A-Za-z0-9_]*(\$[0-9]+)?/)) return "className";
  }
  // process call: `@Name`, optionally tag-offset-instantiated as `@Name$0` (pCall)
  if (s.match(/^@[A-Z][A-Za-z0-9_]*(\$[0-9]+)?/)) return "className";
  // tags: offset form `$name (+ n)?` (pOffsetTag) and concrete `t<n>` (pTag)
  if (s.match(/^\$[A-Za-z_][A-Za-z0-9_]*(\s*\+\s*[0-9]+)?/)) return "labelName";
  if (s.match(/^t[0-9]+\b/)) return "labelName";
  // qubit literal `qb<n>` (pQubit) — before the type/keyword rules
  if (s.match(/^qb[0-9]+\b/)) return "atom";
  // types (pType): bits before bit
  if (s.match(/^(chan|bits|bit|bool|qubit)\b/)) return "typeName";
  // fixed actions — before the call rule, since disc is also `name(...)`.
  // τ is the pretty-printed form of tau.
  if (s.match(/^(disc|tau)\b/) || s.match(/^τ/)) return "macroName";
  // keywords — before the call rule, since some (controlled/tensor/…) take `(`
  if (s.match(KW)) {
    if (s.current() === "proc") st.afterProc = true;
    return "keyword";
  }
  // superoperator application (pQop) / measurement (pMop): an identifier
  // directly followed by `(`. Measurements start with `M` and carry a `>`
  // separator inside the parens; lqCCS expressions never produce `>`.
  if (s.match(/^[A-Za-z][A-Za-z0-9]*(?=\s*\()/)) {
    const rest = s.string.slice(s.pos);
    return /^\s*\([^()]*>/.test(rest) ? "controlKeyword" : "operatorKeyword";
  }
  // remaining PascalCase names (bare process refs, definition names)
  if (s.match(/^[A-Z][A-Za-z0-9_]*(\$[0-9]+)?/)) return "className";
  // plain identifiers (channel/variable names) — consumed whole and left
  // unstyled, so trailing digits (e.g. c1, d2) aren't split off as numbers.
  if (s.match(/^[a-z_][A-Za-z0-9_]*/)) return null;
  if (s.match(/^0b[01]+/) || s.match(/^[0-9]+(\.[0-9]+)?/)) return "number";
  if (s.match(/^(:=|=>|\|\||->|\/\\|\\\/|<=|[!?+\-*&^=<>~])/)) return "operator";
  s.next();
  return null;
}

// ── read-only HTML highlighting (inspector process boxes) ──

const escapeHtml = (str: string): string =>
  str.replace(/[&<>]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;" })[c] as string);

/** A minimal StringStream over a single line, for {@link highlightQuarc}. */
class LineStream implements TokenStream {
  pos = 0;
  start = 0;
  constructor(public string: string) {}
  eol() {
    return this.pos >= this.string.length;
  }
  next(): string | void {
    if (this.pos < this.string.length) return this.string[this.pos++];
  }
  eatSpace() {
    const at = this.pos;
    while (this.pos < this.string.length && (this.string[this.pos] === " " || this.string[this.pos] === "\t")) this.pos++;
    return this.pos > at;
  }
  current() {
    return this.string.slice(this.start, this.pos);
  }
  match(re: RegExp): unknown {
    const m = this.string.slice(this.pos).match(re);
    if (m && m.index === 0) {
      this.pos += m[0].length;
      return m;
    }
    return null;
  }
}

/** Highlight a QUARC source/process string to HTML using the shared tokenizer.
 * Emits `<span class="quarc-<tag>">…</span>`; text is HTML-escaped. */
export function highlightQuarc(src: string): string {
  const st = quarcStartState();
  return src
    .split("\n")
    .map((line) => {
      const s = new LineStream(line);
      let out = "";
      while (!s.eol()) {
        s.start = s.pos;
        const cls = quarcToken(s, st);
        if (s.pos === s.start) s.next(); // guarantee progress
        const text = line.slice(s.start, s.pos);
        out += cls ? `<span class="quarc-${cls}">${escapeHtml(text)}</span>` : escapeHtml(text);
      }
      return out;
    })
    .join("\n");
}
