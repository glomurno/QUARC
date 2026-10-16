// Theme/color helpers reading CSS custom properties (ported from the mockup).
import type { ActionLabel } from "./contract";

export const CLASS_VARS = ["--c0", "--c1", "--c2", "--c3", "--c4", "--c5"];

export const cssv = (v: string) =>
  getComputedStyle(document.documentElement).getPropertyValue(v).trim();

export const classColor = (i: number) => cssv(CLASS_VARS[i % CLASS_VARS.length]);

export const isTauAct = (a: ActionLabel) => a.kind === "tau" || a.kind === "tau2";

export function actionColor(a: ActionLabel): string {
  if (a.kind === "tau" || a.kind === "tau2") return cssv("--ink-3");
  if (a.kind === "send") return cssv("--accent");
  return cssv("--warm");
}
