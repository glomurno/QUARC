# QUARC · LTS Workbench (GUI)

A desktop frontend for the QUARC engine.

## Prerequisites

- Node ≥ 20, Rust ≥ 1.77, and the QUARC engine buildable with `cabal` (run from the
  repo root).
- Linux: WebKitGTK 4.1 (`webkit2gtk-4.1`).
- [Graphviz](https://graphviz.org/) (`dot`) on your `PATH` — the engine shells out
  to it to lay out graphs. Without it, nodes collapse to the origin. This is a
  **runtime requirement for the packaged app too** (Graphviz is not bundled).

## Run (desktop)

```bash
cd gui
npm install
npm run tauri:dev      # builds then launches the app
```

## Build (installers)

```bash
cd gui
npm run tauri:build    # bundles under src-tauri/target/release/bundle/
```

Release builds use the optimised engine; pass the same cabal flags CI uses:

```bash
QUARC_CABAL_OPT="--project-file=cabal.project.release --builddir=dist-release" npm run tauri:build
```

Add `-- --bundles appimage` to build only the AppImage.

**AppImage on rolling-release distros (Arch, CachyOS, Fedora, …):** bundling can
fail with `failed to run linuxdeploy`. With `--verbose` the log shows
`strip: … unknown type [0x13] section '.relr.dyn'`. linuxdeploy ships an old
`strip` that can't read newer system libraries. Skip stripping:

```bash
NO_STRIP=true npm run tauri:build -- --bundles appimage
```
