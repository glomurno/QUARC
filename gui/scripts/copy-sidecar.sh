#!/usr/bin/env bash
# Build the QUARC engine and stage it as the Tauri sidecar binary under the
# platform-specific name Tauri expects: binaries/quarc-<target-triple>.
set -euo pipefail

cd "$(dirname "$0")/.." # gui/
ROOT="$(cd .. && pwd)"  # repo root
TRIPLE="$(rustc -vV | sed -n 's/host: //p')"

# Optional cabal flags for the engine build. Empty by default so local dev stays
# fast; release CI sets QUARC_CABAL_OPT="--project-file=cabal.project.release".
# Locally, add --builddir=dist-release to share the benchmark's release build.
# The SAME flags must reach both `build` and `list-bin`, or `list-bin` resolves
# another build's path.
QUARC_CABAL_OPT="${QUARC_CABAL_OPT:-}"

mkdir -p src-tauri/binaries

(cd "$ROOT" && cabal build exe:quarc $QUARC_CABAL_OPT)
# `list-bin` may first print git output (syncing the source-repository-package
# in cabal.project) to stdout; the binary path is always the last line.
BIN="$(cd "$ROOT" && cabal list-bin exe:quarc $QUARC_CABAL_OPT | tail -n 1)"
cp "$BIN" "src-tauri/binaries/quarc-${TRIPLE}"
echo "staged sidecar: src-tauri/binaries/quarc-${TRIPLE}"

# linuxdeploy's built-in excludelist skips libgmp.so.10 (assumed present on the
# host), so the AppImage would load whatever GMP the user has, or none. Stage
# the exact copy the engine was linked against; `appimage.files` in
# tauri.conf.json drops it into the AppDir's usr/lib (the sidecar's RUNPATH).
if [ "$(uname -s)" = "Linux" ]; then
  GMP="$(ldd "$BIN" | awk '$1 == "libgmp.so.10" { print $3 }')"
  if [ -z "$GMP" ] || [ ! -e "$GMP" ]; then
    echo "error: libgmp.so.10 not resolved for $BIN" >&2
    exit 1
  fi
  mkdir -p src-tauri/binaries/lib
  cp -L "$GMP" src-tauri/binaries/lib/libgmp.so.10
  echo "staged GMP: $GMP -> src-tauri/binaries/lib/libgmp.so.10"
fi
