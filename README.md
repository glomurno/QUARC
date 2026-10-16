# QUARC

QUARC (QUAntum pRotocol Checker) is a command-line toolkit for the verification of quantum communication protocols expressed in the lqCCS process algebra.
It features:

- Typechecking
- Evaluation into LTS and rendering in PNG, SVG, or JSON
- Minimization modulo strong or branching bisimilarity
- Equivalence checking with strong or branching bisimilarity
- Interactive GUI

## Requirements

- [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) (`cabal-version: 3.14`, cabal-install 3.12+ recommended)
- [Graphviz](https://graphviz.org/) (`dot`) on your `PATH` for PNG/SVG graph rendering
- BLAS/LAPACK development libraries, required by the `hmatrix` dependency (e.g. `libopenblas-dev` / `openblas` + `lapack`)
- _Exclusively for the GUI:_ Node ≥ 20, Rust ≥ 1.77 and WebKitGTK 4.1 (`webkit2gtk-4.1`)

## Platform support

**Linux (x86-64) is currently the only officially supported platform.** Release
binaries, the GUI AppImage and the Docker image are built and tested for Linux only.

On Linux the GUI is also available as a self-contained **AppImage** on the GitHub Releases page.
The AppImage requires:

- an x86-64 desktop distribution with glibc 2.35 or newer (e.g. Ubuntu 22.04+,
  Debian 12+, Fedora 36+), with a working X11 or Wayland session
- FUSE 2 to mount the image (`libfuse2` on Debian/Ubuntu, `libfuse2t64` on
  Ubuntu 24.04+, `fuse2` on Arch, `fuse-libs` on Fedora); without it, run the
  AppImage with `--appimage-extract-and-run`
- [Graphviz](https://graphviz.org/) (`dot`) on your `PATH`, which is not bundled
  and is used for graph layout

```bash
chmod +x QUARC_*_amd64.AppImage
./QUARC_*_amd64.AppImage
```

QUARC itself is portable Haskell and Rust/TypeScript, so it can also run on other
platforms when built from source; these setups are untested and best-effort:

- **macOS:** install GHC and Cabal through [GHCup](https://www.haskell.org/ghcup/) and
  Graphviz with `brew install graphviz`. Then follow [Usage](#usage) as
  on Linux; the GUI does not need WebKitGTK.
- **Windows:** the simplest route is [WSL 2](https://learn.microsoft.com/windows/wsl/install)
  with an Ubuntu distribution, where the Linux instructions apply unchanged (WSLg can
  also display the GUI).
- **Any OS with Docker:** the [Docker image](#docker-cli-only) runs the CLI wherever
  Docker Desktop does. It is built for `linux/amd64`; on Apple silicon add
  `--platform linux/amd64` (emulated) or build the image locally with `docker build`.

## Usage

Build the tool with

```bash
cabal build                 # build the CLI and the lqCCS library
cabal build lib:quarc       # build only the library
cabal test                  # run the test suite
```

Invoke the tool through `cabal run quarc -- <subcommand> [options]` (or run the built binary directly).

```bash
cabal run quarc -- --help
cabal run quarc -- check protocol.quarc
cabal run quarc -- check --help
cabal run quarc -- eval protocol.quarc --format svg -o protocol
cabal run quarc -- minimize protocol.quarc -f simplify_lts --format png
cabal run quarc -- equiv --config1 protocol --config2 correctness protocol.quarc
```

For the GUI

```bash
cd gui
npm install            # fetch dependencies
npm run tauri:dev      # builds, then launches the app
```

### Docker (CLI only)

A prebuilt image with the CLI, BLAS/LAPACK and Graphviz is published to GHCR.
Mount your working directory at `/work`:

```bash
docker run --rm -v "$PWD:/work" ghcr.io/glomurno/quarc eval protocol.quarc --format svg -o protocol
```

To build the image locally instead, run from the repository root:

```bash
docker build -t quarc .
```

Then use it exactly like the published one, with `quarc` as the image name:

```bash
docker run --rm -v "$PWD:/work" quarc check protocol.quarc
```

The container only sees the mounted directory, so any files a protocol `import`s
must be inside it too. Files written by the container (e.g. `-o protocol`) are
owned by root; add `--user "$(id -u):$(id -g)"` to keep them owned by you.

The image does **not** include the GUI. For the GUI, use the desktop installers
(AppImage, etc.) from the GitHub Releases page, or build it from `gui/` as above.

### Subcommands

| Command    | Purpose                                                                      |
| ---------- | ---------------------------------------------------------------------------- |
| `check`    | Typecheck every process and start block of a file                            |
| `eval`     | Evaluate a process and render its LTS                                        |
| `minimize` | Minimize a process semantics via strong or branching bisimilarity            |
| `equiv`    | Check equivalence between two processes via strong or branching bisimilarity |

If no input file is given, `check`, `eval`, and `minimize` read from stdin.

### Output files

`eval`, `minimize` and `equiv` write their output to a file in the current
directory. The file is named after the input files (without directory or
extension) and the command, joined by `_`, with the extension of `--format`:

| Invocation                                     | Output                                             |
| ---------------------------------------------- | -------------------------------------------------- |
| `eval protocol.quarc`                          | `protocol_eval.png`                                |
| `minimize protocol.quarc --format svg`         | `protocol_minimize.svg`                            |
| `equiv a.quarc b.quarc`                        | `a_b_equiv.png`, `a_b_equiv_minimal.png`           |
| `equiv protocol.quarc --config1 x --config2 y` | `protocol_equiv.png`, `protocol_equiv_minimal.png` |
| `eval` reading from stdin                      | `eval.png`                                         |

Use `-o NAME` to pick another name (the extension is still added), or
`--stdout` to print the output instead of writing a file. `equiv` renders two
graphs, so it only accepts `--stdout` with `--format json`.

### Common options

| Option                             | Effect                                                                                                  |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------- |
| `--format png\|svg\|json`          | Output format for `eval`/`minimize`/`equiv` (default `png`)                                             |
| `-f, --flag simplify_lts`          | Apply some syntactical simplifications in the render                                                    |
| `-f, --flag no_names`              | Omit process names from the render graph                                                                |
| `-f, --flag only_dots`             | Render only node dots                                                                                   |
| `-f, --flag sugar`                 | Apply syntactic sugar to the processes in the render                                                    |
| `-f, --flag print_lts`             | Print the LTS textually                                                                                 |
| `-o, --output NAME`                | Output file name without extension (default `<input>_<command>`, see [Output files](#output-files))     |
| `--stdout`                         | Write the output to stdout instead of a file (`equiv`: only with `--format json`)                       |
| `--config NAME`                    | Select a named `start` block (`eval`/`minimize`/`check`)                                                |
| `--processes`                      | Only typecheck process declarations, not start blocks (`check`)                                         |
| `--json`                           | Emit `check` results as JSON (per-target `results` + flat `diagnostics`)                                |
| `--config1 / --config2 NAME`       | Select the two blocks to compare (`equiv`, single file)                                                 |
| `--tag-equiv A=B`                  | Declare tags `A` and `B` equivalent (repeatable)                                                        |
| `--equivalence strong\|branching`  | Equivalence for `minimize`/`equiv` (default `strong`)                                                   |
| `--rooted`                         | With `branching`, check rooted branching bisimilarity (`equiv`)                                         |
| `--silent TAGS` / `--visible TAGS` | With `branching`, choose which `tau` tags are silent (see `silent`/`visible` blocks in QUARC_SYNTAX.md) |

### Global options

`-v`/`-vv`/`-vvv` raise log verbosity, `-q` suppresses everything but errors, `--log-file PATH` redirects logs, and `--timing` prints per-phase wall-clock timings.
These apply to every subcommand.

For the list of options of each subcommand refer to its help output with `-h, --help`.

## The `.quarc` format

A `.quarc` file defines operators, measurements, named states, and processes, plus one or more `start` blocks describing what to run.
Order between definitions is free; `start` and `tag-equiv` blocks follow the definitions.

```quarc
proc Alice[q0,q1](outz: chan bit, outx: chan bit) :=
  t10:CNOT(q0,q1).t10:H(q0).t10:M01(q0>z).t10:M01(q1>x).
  ((t10:outz!z.disc(q0)) || (t10:outx!x.disc(q1)))

proc Bob[q](inz: chan bit, inx: chan bit, out: chan qubit) :=
  t11:inz?z.t11:inx?x.
  (if z = 0 then t11:I(q) else t11:Z(q)).
  (if x = 0 then t11:I(q) else t11:X(q)).
  t11:out!q.disc()

state START := density 3 H(0).CNOT(0,1)

start "protocol" {
  density START
  process (@Alice[qb0,qb1](c,d) || @Bob[qb2](c,d,qout)) \ c \ d
}
```

The full grammar is documented in [`QUARC_SYNTAX.md`](QUARC_SYNTAX.md).
Ready-to-run examples live under [`samples/`](samples/).

### Benchmarks

The per-phase benchmark suite runs natively (`python3 bench/run_benchmarks.py`) or
in Docker without a local Haskell toolchain:

```bash
docker build --target bench --build-arg GIT_COMMIT="$(git rev-parse HEAD)" -t quarc-bench .
```

```bash
docker run --rm --user "$(id -u):$(id -g)" -v "$PWD/bench/results:/src/bench/results" quarc-bench
```

Results land in `bench/results/<timestamp>/`. Refer to [`bench/README.md`](bench/README.md)
for the options, what is measured and the output format.

## License

MIT — see [`LICENSE`](LICENSE). © Giuseppe Lomurno.
