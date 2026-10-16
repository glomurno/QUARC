// Read-only process-string highlighting for the inspector, using the same
// tokenizer as the editor so the two stay in sync (see quarcSyntax.ts).
import { highlightQuarc } from "./quarcSyntax";

export function renderProc(s: string): string {
  return highlightQuarc(s);
}
