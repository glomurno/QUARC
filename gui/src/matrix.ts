// Client-side serializers for a density matrix already fetched via getState.
// The matrix data lives in the browser, so we format the QUARC `density … dm { }`
// snippet and LaTeX here — no engine round-trip (GUI_DESIGN §7.4).

import type { ComplexMatrix } from "./contract";

/** Format one complex entry in the parser's `cmplx` grammar: `a`, `bi`, or
 * `a+bi` (see LqCCS.Parser.File.cmplx). Trailing zeros are trimmed. */
export function fmtComplex(re: number, im: number): string {
  const r = trim(re);
  const i = trim(im);
  if (Math.abs(im) < 1e-12) return r;
  if (Math.abs(re) < 1e-12) return `${i}i`;
  const sign = im < 0 ? "-" : "+";
  return `${r}${sign}${trim(Math.abs(im))}i`;
}

// ~12 significant digits is plenty below double precision while staying compact.
function trim(x: number): string {
  if (Object.is(x, -0)) x = 0;
  return parseFloat(x.toPrecision(12)).toString();
}

/** Qubit count for a 2^q × 2^q density matrix (the `density <q>` field). */
function qubitsOf(m: ComplexMatrix): number {
  return Math.round(Math.log2(m.rows));
}

/** Render the matrix as a QUARC definition: `state <Name> := density <q> dm { … }`,
 * one row per line, ready to insert before the file's start blocks. */
export function toDensitySnippet(name: string, m: ComplexMatrix): string {
  const q = qubitsOf(m);
  const rows: string[] = [];
  for (let r = 0; r < m.rows; r++) {
    const cells: string[] = [];
    for (let c = 0; c < m.cols; c++) {
      const [re, im] = m.data[r * m.cols + c];
      cells.push(fmtComplex(re, im));
    }
    rows.push("  " + cells.join(", "));
  }
  return `state ${name} := density ${q} dm {\n${rows.join("\n")}\n}`;
}

/** Render the matrix as a LaTeX `bmatrix` (math-mode body, no delimiters). */
export function toLatex(m: ComplexMatrix): string {
  const rows: string[] = [];
  for (let r = 0; r < m.rows; r++) {
    const cells: string[] = [];
    for (let c = 0; c < m.cols; c++) {
      const [re, im] = m.data[r * m.cols + c];
      cells.push(fmtComplex(re, im));
    }
    rows.push(cells.join(" & "));
  }
  return `\\begin{bmatrix}${rows.join(" \\\\ ")}\\end{bmatrix}`;
}
