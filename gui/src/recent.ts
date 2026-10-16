// Recently opened .quarc files (most recent first), persisted in localStorage so the
// welcome screen and the file menu can offer them on the next launch.

const KEY = "quarc-recent-files";
const MAX = 8;

export function recentFiles(): string[] {
  try {
    const v = JSON.parse(localStorage.getItem(KEY) ?? "[]");
    return Array.isArray(v) ? v.filter((x): x is string => typeof x === "string") : [];
  } catch {
    return [];
  }
}

function store(list: string[]) {
  try {
    localStorage.setItem(KEY, JSON.stringify(list.slice(0, MAX)));
  } catch {
    /* storage unavailable: recents are a convenience only */
  }
}

/** Move `path` to the front of the list. */
export function pushRecent(path: string): void {
  store([path, ...recentFiles().filter((p) => p !== path)]);
}

export function removeRecent(path: string): void {
  store(recentFiles().filter((p) => p !== path));
}

/** Last path component, for compact display. */
export const fileName = (path: string): string => path.split(/[\\/]/).pop() ?? path;

/** Parent directory, for disambiguating recents with the same name. */
export const fileDir = (path: string): string => {
  const i = Math.max(path.lastIndexOf("/"), path.lastIndexOf("\\"));
  return i > 0 ? path.slice(0, i) : "";
};
