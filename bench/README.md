# Benchmarks

## Running

```bash
python3 bench/run_benchmarks.py                       # build, then run every task
python3 bench/run_benchmarks.py --skip-group large    # leave out the 20-35 s/repetition cases
python3 bench/run_benchmarks.py --filter cointoss --quick
python3 bench/run_benchmarks.py --list                # show the selected tasks
python3 bench/run_benchmarks.py --report bench/results/<run>/results.json  # regenerate the tables
```

Run `--help` to see every option. The driver needs only Python 3.11+ and its standard library.

### With Docker

The `bench` stage of the [`Dockerfile`](../Dockerfile) holds GHC, cabal, Python and a prebuilt harness, so you don't need a local Haskell toolchain. It isn't part of the default image. Build it from the repository root:

```bash
docker build --target bench --build-arg GIT_COMMIT="$(git rev-parse HEAD)" -t quarc-bench .
```

`GIT_COMMIT` is optional, it allows for the results to record the commit.

Mount `bench/results` to keep the output. Arguments after the image name go to `run_benchmarks.py`:

```bash
mkdir -p bench/results
docker run --rm --user "$(id -u):$(id -g)" -v "$PWD/bench/results:/src/bench/results" quarc-bench
```

```bash
docker run --rm --user "$(id -u):$(id -g)" -v "$PWD/bench/results:/src/bench/results" quarc-bench --filter cointoss --quick
```

Rebuild after changing the code, `bench/benchmarks.toml` or `samples/`.
To try a different workload file without rebuilding, mount it and pass `--config /path/in/container.toml`.

Container timings include Docker's overhead and differ from native runs, so only compare results from the same setup.

## What is measured

Each task in `benchmarks.toml` runs in its own `pipeline-bench` process. That process repeats the full pipeline from the source text: `warmup` unmeasured runs, then measured runs until it has both `min_reps` runs and `min_time` seconds, or reaches `max_reps`. Before each run it performs a major GC. The phases are:

| phase       | check | eval | minimize | compare | what it covers                                           |
| ----------- | :---: | :--: | :------: | :-----: | -------------------------------------------------------- |
| `parse`     |   ✓   |  ✓   |    ✓     |    ✓    | parsing                                                  |
| `tagging`   |   ✓   |  ✓   |    ✓     |    ✓    | automatic tagging                                        |
| `typecheck` |   ✓   |  ✓   |    ✓     |    ✓    | typechecks every start block, unless otherwise specified |
| `eval`      |       |  ✓   |    ✓     |    ✓    | LTS generation                                           |
| `minimize`  |       |      |    ✓     |         | bisimilarity classes and the quotient LTS                |
| `compare`   |       |      |          |    ✓    | bisimilarity between the initial states                  |

For every phase and run, the harness records:

- wall-clock time from the monotonic clock (ns)
- bytes allocated by the thread (`getAllocationCounter`)
- GC wall-clock time (`+RTS -T` statistics)

Per process it also records GHC's RTS totals (peak memory in use, max live bytes, GC counts) and the process's peak RSS (`VmHWM`).

The harness is linked with the `quarc` executable's RTS options (`-threaded -N -qg`) plus `-T`. Use `rts` in the config or `--rts` to change them. Set `pin_cpu` or pass `--pin-cpu N` to run the tasks under `taskset`.

## Output

Each run writes to `bench/results/<UTC timestamp>[-label]/`.

| file                                     | contents                                                                                                                                                                                       |
| ---------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `results.json` (`quarc.bench.results/1`) | everything: environment, workload config, the exact harness command lines, raw per-run samples, statistics                                                                                     |
| `summary.csv`                            | one row per task per phase (plus `total`): outcome, then wall time n/median/q1/q3/mean/sd/min/max and a 95% CI of the median (ms), plus median allocation, median GC time and peak RSS (bytes) |
| `samples.csv`                            | one row per task per run per phase: raw `wall_ns`, `alloc_bytes`, `gc_ns`                                                                                                                      |
| `summary.txt`                            | the human-readable table printed at the end                                                                                                                                                    |
| `summary.tex`                            | the same table as a booktabs `tabular`                                                                                                                                                         |

The 95% CI of the median uses order statistics (distribution-free, n ≥ 6).

The environment block in `results.json` records:

- git commit, branch and dirty state, plus a hash of the diff
- GHC and cabal versions, the build command, and SHA-256 hashes of `quarc.cabal`, the `cabal.project*` files, `Pipeline.hs` and the harness binary
- the RTS `--info` output
- CPU model, thread count, memory, kernel, frequency governors and boost, and load average
- the SHA-256 of every input `.quarc` file

The driver warns about a dirty working tree, a non-`performance` CPU governor and a high load average.

## Workloads

`benchmarks.toml` lists the tasks per file: `check`, `eval`, `minimize` and `compare`. A list entry can be an inline table that overrides any `[defaults]` key for that one task. A `[[file]]` can set defaults keys for all of its tasks. An `equivalence` list expands into one task per value.

The default suite, with `tags = "angelic"` and the `branch` tagging policy:
