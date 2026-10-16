#!/usr/bin/env python3
"""Run the QUARC pipeline benchmark and write human- and machine-readable results.

Each task from the workload file (bench/benchmarks.toml by default) runs in a
fresh `pipeline-bench` process that times parse, tagging, typecheck, eval,
minimize and compare separately; layout and rendering are never measured.

Usage:
    python3 bench/run_benchmarks.py                      # build, run the default suite
    python3 bench/run_benchmarks.py --skip-group large   # skip the multi-second cases
    python3 bench/run_benchmarks.py --filter cointoss --quick
    python3 bench/run_benchmarks.py --list
    python3 bench/run_benchmarks.py --report bench/results/<run>/results.json

Every run writes into bench/results/<UTC timestamp>[-label]/:
    results.json   environment, configuration, raw per-repetition samples, statistics
    summary.csv    one row per task and phase with summary statistics (ms)
    samples.csv    one row per task, repetition and phase (raw ns and bytes)
    summary.txt    the table printed at the end
    summary.tex    the same table as a LaTeX booktabs tabular

Only the Python standard library is needed (3.11+ for tomllib).
"""

import argparse
import copy
import csv
import datetime as dt
import hashlib
import json
import math
import os
import platform
import re
import shlex
import shutil
import statistics
import subprocess
import sys
import tempfile
import threading
import time
from pathlib import Path

import tomllib

ROOT = Path(__file__).resolve().parent.parent
RESULTS_SCHEMA = "quarc.bench.results/1"
PHASES = ["parse", "tagging", "typecheck", "eval", "minimize", "compare"]
TASK_KINDS = ["check", "eval", "minimize", "compare"]
TASK_KEYS = {
    "group",
    "warmup",
    "min_reps",
    "min_time",
    "max_reps",
    "timeout",
    "tagging",
    "tags",
    "equivalence",
    "rooted",
    "rts",
    "pin_cpu",
}
BUILTIN_DEFAULTS = {
    "group": "default",
    "warmup": 3,
    "min_reps": 20,
    "min_time": 2.0,
    "max_reps": 500,
    "timeout": 3600,
    "tagging": "branch",
    "tags": "angelic",
    "equivalence": "branching",
    "rooted": False,
    "rts": [],
    "pin_cpu": "",
}


class ConfigError(Exception):
    pass


def load_config(path: Path) -> dict:
    with path.open("rb") as f:
        return tomllib.load(f)


def expand_tasks(cfg: dict) -> list[dict]:
    """Turn the [[file]] tables into a flat list of fully specified tasks."""
    defaults = {**BUILTIN_DEFAULTS, **cfg.get("defaults", {})}
    unknown = set(defaults) - TASK_KEYS
    if unknown:
        raise ConfigError(f"unknown [defaults] keys: {sorted(unknown)}")
    tasks: list[dict] = []
    for file_cfg in cfg.get("file", []):
        if "path" not in file_cfg:
            raise ConfigError("every [[file]] needs a path")
        path = file_cfg["path"]
        file_opts = {k: v for k, v in file_cfg.items() if k in TASK_KEYS}
        stray = set(file_cfg) - TASK_KEYS - set(TASK_KINDS) - {"path"}
        if stray:
            raise ConfigError(f"{path}: unknown keys {sorted(stray)}")
        base = {**defaults, **file_opts}
        if file_cfg.get("check"):
            tasks.append(make_task(path, "check", [], base, {}))
        for kind, arity in (("eval", 1), ("minimize", 1), ("compare", 2)):
            for entry in file_cfg.get(kind, []):
                configs, overrides = parse_entry(path, kind, arity, entry)
                tasks.extend(expand_equivalence(path, kind, configs, base, overrides))
    seen: dict[str, dict] = {}
    for t in tasks:
        if t["id"] in seen:
            raise ConfigError(
                f"duplicate task id {t['id']!r}; give one of them an explicit id"
            )
        seen[t["id"]] = t
    return tasks


def parse_entry(path: str, kind: str, arity: int, entry) -> tuple[list[str], dict]:
    if isinstance(entry, str):
        configs, overrides = [entry], {}
    elif isinstance(entry, list):
        configs, overrides = entry, {}
    elif isinstance(entry, dict):
        overrides = dict(entry)
        if "configs" in overrides:
            configs = overrides.pop("configs")
        elif "config" in overrides:
            configs = [overrides.pop("config")]
        else:
            raise ConfigError(f"{path}: {kind} entry {entry} needs config or configs")
        stray = set(overrides) - TASK_KEYS - {"id"}
        if stray:
            raise ConfigError(f"{path}: {kind} entry has unknown keys {sorted(stray)}")
    else:
        raise ConfigError(f"{path}: cannot read {kind} entry {entry!r}")
    if len(configs) != arity or not all(isinstance(c, str) for c in configs):
        raise ConfigError(
            f"{path}: {kind} takes {arity} start block name(s), got {configs!r}"
        )
    return configs, overrides


def expand_equivalence(path, kind, configs, base, overrides) -> list[dict]:
    equivs = overrides.get("equivalence", base["equivalence"])
    equivs = equivs if isinstance(equivs, list) else [equivs]
    out = []
    for eq in equivs:
        task = make_task(path, kind, configs, base, {**overrides, "equivalence": eq})
        if "id" in overrides and len(equivs) > 1:
            task["id"] = f"{overrides['id']}/{eq}"
        out.append(task)
    return out


def make_task(
    path: str, kind: str, configs: list[str], base: dict, overrides: dict
) -> dict:
    opts = {k: v for k, v in {**base, **overrides}.items() if k in TASK_KEYS}
    if isinstance(opts["equivalence"], list):
        # check/eval ignore the equivalence; record a single value.
        opts["equivalence"] = opts["equivalence"][0]
    task_id = overrides.get("id")
    if task_id is None:
        task_id = f"{Path(path).stem}/{kind}"
        if configs:
            task_id += "/" + "~".join(configs)
        if kind in ("minimize", "compare"):
            task_id += (
                "/"
                + opts["equivalence"]
                + ("-rooted" if kind == "compare" and opts["rooted"] else "")
            )
    return {"id": task_id, "file": path, "kind": kind, "configs": configs, **opts}


def select_tasks(tasks, args) -> list[dict]:
    out = []
    for t in tasks:
        if args.filter and not any(re.search(p, t["id"]) for p in args.filter):
            continue
        if args.exclude and any(re.search(p, t["id"]) for p in args.exclude):
            continue
        if args.group and t["group"] not in args.group:
            continue
        if args.skip_group and t["group"] in args.skip_group:
            continue
        out.append(t)
    return out


def apply_overrides(tasks, args) -> None:
    for t in tasks:
        if args.quick:
            t.update(warmup=1, min_reps=3, min_time=0.0)
        for key in ("warmup", "min_reps", "max_reps", "min_time", "timeout"):
            v = getattr(args, key)
            if v is not None:
                t[key] = v
        if args.rts is not None:
            t["rts"] = shlex.split(args.rts)
        if args.pin_cpu is not None:
            t["pin_cpu"] = args.pin_cpu


def run_out(cmd: list[str], cwd: Path = ROOT) -> str | None:
    try:
        return subprocess.run(
            cmd, cwd=cwd, capture_output=True, text=True, check=True
        ).stdout.strip()
    except (OSError, subprocess.CalledProcessError):
        return None


def build(cfg: dict, skip: bool) -> tuple[Path, list[str]]:
    b = cfg.get("build", {})
    flags = [
        f"--project-file={b.get('project_file', 'cabal.project.release')}",
        f"--builddir={b.get('builddir', 'dist-release')}",
        *b.get("cabal_flags", ["--enable-benchmarks"]),
    ]
    target = "bench:pipeline-bench"
    build_cmd = ["cabal", "build", *flags, target]
    if not skip:
        print(f"building: {' '.join(build_cmd)}", file=sys.stderr)
        r = subprocess.run(build_cmd, cwd=ROOT)
        if r.returncode != 0:
            sys.exit("build failed")
    out = run_out(["cabal", "list-bin", *flags, target])
    exe = out.splitlines()[-1] if out else None
    if not exe or not Path(exe).exists():
        sys.exit("could not locate the pipeline-bench executable; build it first")
    return Path(exe), build_cmd


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def read_text(path: str) -> str | None:
    try:
        return Path(path).read_text().strip()
    except OSError:
        return None


def cpu_model() -> str | None:
    info = read_text("/proc/cpuinfo") or ""
    m = re.search(r"^model name\s*:\s*(.+)$", info, re.MULTILINE)
    return m.group(1) if m else platform.processor() or None


def mem_total_bytes() -> int | None:
    m = re.search(
        r"^MemTotal:\s*(\d+) kB", read_text("/proc/meminfo") or "", re.MULTILINE
    )
    return int(m.group(1)) * 1024 if m else None


def project_files(build_cmd: list[str]) -> list[str]:
    """The project file the build used, and the files it imports."""
    files, todo = (
        [],
        [a.split("=", 1)[1] for a in build_cmd if a.startswith("--project-file=")]
        or ["cabal.project"],
    )
    while todo:
        f = todo.pop()
        if f in files or not (ROOT / f).exists():
            continue
        files.append(f)
        todo += re.findall(r"^import:\s*(\S+)", (ROOT / f).read_text(), re.MULTILINE)
    return files


def collect_environment(exe: Path, build_cmd: list[str], tasks: list[dict]) -> dict:
    dirty = run_out(["git", "status", "--porcelain"])
    files = sorted({t["file"] for t in tasks})
    governors = sorted(
        {
            g
            for g in (
                read_text(str(p))
                for p in Path("/sys/devices/system/cpu").glob(
                    "cpu[0-9]*/cpufreq/scaling_governor"
                )
            )
            if g
        }
    )
    return {
        "timestamp_utc": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
        "command": [sys.executable, *sys.argv],
        "git": {
            "commit": run_out(["git", "rev-parse", "HEAD"])
            or os.environ.get("QUARC_GIT_COMMIT")
            or None,
            "branch": run_out(["git", "rev-parse", "--abbrev-ref", "HEAD"]),
            "dirty": bool(dirty),
            "status": dirty.splitlines() if dirty else [],
            "diff_sha256": hashlib.sha256(
                (run_out(["git", "diff", "HEAD"]) or "").encode()
            ).hexdigest()
            if dirty
            else None,
        },
        "toolchain": {
            "ghc": run_out(["ghc", "--numeric-version"]),
            "cabal": run_out(["cabal", "--numeric-version"]),
            "build_command": build_cmd,
            "project_files_sha256": {
                f: sha256(ROOT / f)
                for f in (*project_files(build_cmd), "quarc.cabal", "bench/Pipeline.hs")
                if (ROOT / f).exists()
            },
            "executable": str(
                exe.relative_to(ROOT) if exe.is_relative_to(ROOT) else exe
            ),
            "executable_sha256": sha256(exe),
            "rts_info": run_out([str(exe), "+RTS", "--info", "-RTS"]),
            "python": platform.python_version(),
        },
        "machine": {
            "hostname": platform.node(),
            "os": platform.platform(),
            "kernel": platform.release(),
            "arch": platform.machine(),
            "cpu_model": cpu_model(),
            "logical_cpus": os.cpu_count(),
            "mem_total_bytes": mem_total_bytes(),
            "cpu_governors": governors,
            "cpu_boost": read_text("/sys/devices/system/cpu/cpufreq/boost"),
            "loadavg_at_start": list(os.getloadavg())
            if hasattr(os, "getloadavg")
            else None,
        },
        "inputs": {
            f: {
                "sha256": sha256(ROOT / f),
                "lines": len((ROOT / f).read_text().splitlines()),
            }
            for f in files
        },
    }


def warn_environment(env: dict) -> list[str]:
    warnings = []
    if env["git"]["dirty"]:
        warnings.append(
            "the working tree has uncommitted changes (recorded in results.json)"
        )
    govs = env["machine"]["cpu_governors"]
    if govs and govs != ["performance"]:
        warnings.append(
            f"CPU frequency governor is {', '.join(govs)}; 'performance' gives steadier timings"
        )
    load = env["machine"]["loadavg_at_start"]
    if (
        load
        and env["machine"]["logical_cpus"]
        and load[0] > 0.25 * env["machine"]["logical_cpus"]
    ):
        warnings.append(
            f"system load is {load[0]:.2f}; other processes may disturb the timings"
        )
    return warnings


def harness_command(exe: Path, task: dict, out: Path) -> list[str]:
    cmd = [str(exe), "--file", str(ROOT / task["file"]), "--task", task["kind"]]
    for c in task["configs"]:
        cmd += ["--config", c]
    cmd += [
        "--tagging",
        task["tagging"],
        "--tags",
        task["tags"],
        "--equivalence",
        task["equivalence"],
        "--warmup",
        str(task["warmup"]),
        "--min-reps",
        str(task["min_reps"]),
        "--max-reps",
        str(task["max_reps"]),
        "--min-time",
        str(float(task["min_time"])),
        "--output",
        str(out),
    ]
    if task["rooted"]:
        cmd.append("--rooted")
    if task["rts"]:
        cmd += ["+RTS", *task["rts"], "-RTS"]
    if task["pin_cpu"] != "":
        if not shutil.which("taskset"):
            sys.exit("pin_cpu is set but taskset is not available")
        cmd = ["taskset", "-c", str(task["pin_cpu"]), *cmd]
    return cmd


def run_task(exe: Path, task: dict, tmpdir: Path) -> dict:
    out = tmpdir / "task.json"
    out.unlink(missing_ok=True)
    cmd = harness_command(exe, task, out)
    start = time.monotonic()
    proc = subprocess.Popen(
        cmd, cwd=ROOT, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True
    )
    timed_out = threading.Event()

    def kill():
        timed_out.set()
        proc.kill()

    timer = threading.Timer(task["timeout"], kill)
    timer.start()
    stderr_chunks: list[str] = []
    reader = threading.Thread(target=lambda: stderr_chunks.append(proc.stderr.read()))  # ty: ignore[unresolved-attribute]
    reader.start()
    _, status, rusage = os.wait4(proc.pid, 0)
    proc.returncode = os.waitstatus_to_exitcode(status)
    timer.cancel()
    reader.join()
    wall = time.monotonic() - start
    record = {
        "task": task,
        "harness_command": cmd,
        "process": {
            "exit_code": proc.returncode,
            "wall_s": wall,
            # Includes the moment before exec, when the child is a copy of this driver.
            "max_rss_bytes_wait4": rusage.ru_maxrss * 1024,
            "user_cpu_s": rusage.ru_utime,
            "sys_cpu_s": rusage.ru_stime,
        },
    }
    if timed_out.is_set():
        record["error"] = f"timed out after {task['timeout']} s"
    elif proc.returncode != 0 or not out.exists():
        record["error"] = (
            "".join(stderr_chunks).strip() or f"exit code {proc.returncode}"
        )
    else:
        record["measurement"] = json.loads(out.read_text())
        record["stats"] = summarize(record["measurement"])
    return record


def describe(values: list[float]) -> dict:
    """Location and spread of a sample, plus a distribution-free 95% CI for the median."""
    xs = sorted(values)
    n = len(xs)
    if n == 0:
        return {"n": 0}
    q1, q3 = (
        (statistics.quantiles(xs, n=4, method="inclusive")[i] for i in (0, 2))
        if n >= 2
        else (xs[0], xs[0])
    )
    d = {
        "n": n,
        "median": statistics.median(xs),
        "q1": q1,
        "q3": q3,
        "mean": statistics.fmean(xs),
        "sd": statistics.stdev(xs) if n >= 2 else 0.0,
        "min": xs[0],
        "max": xs[-1],
        "median_ci95": None,
    }
    if n >= 6:
        # Order statistics at 1-based ranks n/2 - 1.96 sqrt(n)/2 and 1 + n/2 + 1.96 sqrt(n)/2
        # (normal approximation to Bin(n, 1/2); Conover, Practical Nonparametric Statistics).
        half = 1.96 * math.sqrt(n) / 2
        lo = max(1, math.floor(n / 2 - half))
        hi = min(n, math.ceil(1 + n / 2 + half))
        d["median_ci95"] = [xs[lo - 1], xs[hi - 1]]
    return d


def summarize(m: dict) -> dict:
    phases = m["phases"]
    samples = m["samples"]
    out = {}
    for p in [*phases, "total"]:
        if p == "total":
            wall = [sum(s["wall_ns"].values()) / 1e6 for s in samples]
            alloc = [sum(s["alloc_bytes"].values()) for s in samples]
            gc = [sum(s["gc_ns"].values()) / 1e6 for s in samples]
        else:
            wall = [s["wall_ns"][p] / 1e6 for s in samples]
            alloc = [s["alloc_bytes"][p] for s in samples]
            gc = [s["gc_ns"][p] / 1e6 for s in samples]
        out[p] = {
            "wall_ms": describe(wall),
            "alloc_bytes": describe(alloc),
            "gc_ms": describe(gc),
        }
    return out


def fmt_ms(x: float | None) -> str:
    if x is None:
        return "-"
    if x >= 1000:
        return f"{x / 1000:.2f} s"
    if x >= 100:
        return f"{x:.0f}"
    if x >= 10:
        return f"{x:.1f}"
    if x >= 1:
        return f"{x:.2f}"
    return f"{x:.3f}"


def fmt_mib(b: float | None) -> str:
    return "-" if b is None else f"{b / 2**20:.0f}"


def peak_rss(rec: dict) -> int | None:
    """The harness's own VmHWM; the wait4 figure also counts the forked driver."""
    return (
        rec.get("measurement", {}).get("peak_rss_bytes")
        or rec["process"]["max_rss_bytes_wait4"]
    )


def outcome_cells(rec: dict) -> tuple[str, str, str]:
    o = rec.get("measurement", {}).get("outcome", {})
    states = o.get("lts_states")
    trans = o.get("lts_transitions")
    if "bisimilar" in o:
        result = "bisim" if o["bisimilar"] else "not bisim"
    elif "min_states" in o:
        result = f"{o['min_states']} cls"
    elif "start_entries" in o:
        result = f"{o['start_entries_ok']}/{o['start_entries']} ok"
    else:
        result = ""
    return (
        "" if states is None else str(states),
        "" if trans is None else str(trans),
        result,
    )


TABLE_PHASES = [
    ("parse", "parse"),
    ("tagging", "tag"),
    ("typecheck", "typecheck"),
    ("eval", "eval"),
    ("bisim", "min/cmp"),
]


def phase_median(rec: dict, phase: str) -> float | None:
    stats = rec.get("stats", {})
    if phase == "bisim":
        phase = "minimize" if "minimize" in stats else "compare"
    s = stats.get(phase)
    return s["wall_ms"]["median"] if s else None


def table_rows(results: dict) -> tuple[list[str], list[list[str]]]:
    header = [
        "task",
        "states",
        "trans",
        "result",
        "reps",
        *[h for _, h in TABLE_PHASES],
        "total",
        "IQR",
        "RSS MiB",
    ]
    rows = []
    for rec in results["tasks"]:
        tid = rec["task"]["id"]
        if "error" in rec:
            rows.append(
                [tid, "", "", "ERROR", "", *[""] * len(TABLE_PHASES), "", "", ""]
            )
            continue
        states, trans, result = outcome_cells(rec)
        tot = rec["stats"]["total"]["wall_ms"]
        rows.append(
            [
                tid,
                states,
                trans,
                result,
                str(tot["n"]),
                *[fmt_ms(phase_median(rec, p)) for p, _ in TABLE_PHASES],
                fmt_ms(tot["median"]),
                fmt_ms(tot["q3"] - tot["q1"]),
                fmt_mib(peak_rss(rec)),
            ]
        )
    return header, rows


def text_table(results: dict) -> str:
    header, rows = table_rows(results)
    widths = [max(len(r[i]) for r in [header, *rows]) for i in range(len(header))]
    right = set(range(1, len(header))) - {3}

    def line(r):
        return "  ".join(
            c.rjust(w) if i in right else c.ljust(w)
            for i, (c, w) in enumerate(zip(r, widths))
        )

    env = results["environment"]
    lines = [
        f"QUARC pipeline benchmark  {env['timestamp_utc']}",
        (
            f"commit {env['git']['commit'] or '?'}{' (dirty)' if env['git']['dirty'] else ''} on {env['git']['branch']}; "
            f"GHC {env['toolchain']['ghc']}; {env['machine']['cpu_model']} ({env['machine']['logical_cpus']} threads); {env['machine']['os']}"
        ),
        (
            "Times are medians in ms (s where marked) over the measured repetitions; IQR is the interquartile range of the total;"
            " RSS is the peak of the task's process."
        ),
        "",
        line(header),
        line(["-" * w for w in widths]),
        *[line(r) for r in rows],
    ]
    errors = [rec for rec in results["tasks"] if "error" in rec]
    if errors:
        lines += ["", "Errors:"] + [
            f"  {rec['task']['id']}: {rec['error'].splitlines()[-1] if rec['error'] else ''}"
            for rec in errors
        ]
    return "\n".join(lines) + "\n"


def latex_escape(s: str) -> str:
    return re.sub(
        r"([_&%#$~{}])",
        lambda m: "\\textasciitilde{}" if m.group(1) == "~" else "\\" + m.group(1),
        s,
    )


def latex_table(results: dict) -> str:
    header, rows = table_rows(results)
    cols = "l" + "r" * 2 + "l" + "r" * (len(header) - 4)
    out = [
        "% Generated by bench/run_benchmarks.py; requires \\usepackage{booktabs}.",
        f"% {results['environment']['timestamp_utc']}, commit {results['environment']['git']['commit']}",
        f"\\begin{{tabular}}{{{cols}}}",
        "\\toprule",
        " & ".join(latex_escape(h) for h in header) + " \\\\",
        "\\midrule",
    ]
    out += [" & ".join(latex_escape(c) for c in r) + " \\\\" for r in rows]
    out += ["\\bottomrule", "\\end{tabular}"]
    return "\n".join(out) + "\n"


def write_csvs(results: dict, outdir: Path) -> None:
    stat_keys = ["n", "median", "q1", "q3", "mean", "sd", "min", "max"]
    with (outdir / "summary.csv").open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(
            [
                "task_id",
                "file",
                "kind",
                "configs",
                "equivalence",
                "tags",
                "tagging",
                "rooted",
                "lts_states",
                "lts_transitions",
                "classes",
                "min_states",
                "min_transitions",
                "bisimilar",
                "phase",
                *[f"wall_ms_{k}" for k in stat_keys],
                "wall_ms_median_ci95_lo",
                "wall_ms_median_ci95_hi",
                "alloc_bytes_median",
                "gc_ms_median",
                "peak_rss_bytes",
                "error",
            ]
        )
        for rec in results["tasks"]:
            t = rec["task"]
            o = rec.get("measurement", {}).get("outcome", {})
            prefix = [
                t["id"],
                t["file"],
                t["kind"],
                "~".join(t["configs"]),
                t["equivalence"],
                t["tags"],
                t["tagging"],
                t["rooted"],
                o.get("lts_states"),
                o.get("lts_transitions"),
                o.get("classes"),
                o.get("min_states"),
                o.get("min_transitions"),
                o.get("bisimilar"),
            ]
            if "error" in rec:
                w.writerow(
                    [
                        *prefix,
                        "",
                        *[""] * (len(stat_keys) + 4),
                        peak_rss(rec),
                        rec["error"],
                    ]
                )
                continue
            for phase, s in rec["stats"].items():
                ci = s["wall_ms"]["median_ci95"] or [None, None]
                w.writerow(
                    [
                        *prefix,
                        phase,
                        *[s["wall_ms"][k] for k in stat_keys],
                        ci[0],
                        ci[1],
                        s["alloc_bytes"]["median"],
                        s["gc_ms"]["median"],
                        peak_rss(rec),
                        "",
                    ]
                )
    with (outdir / "samples.csv").open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["task_id", "rep", "phase", "wall_ns", "alloc_bytes", "gc_ns"])
        for rec in results["tasks"]:
            for s in rec.get("measurement", {}).get("samples", []):
                for phase, ns in s["wall_ns"].items():
                    w.writerow(
                        [
                            rec["task"]["id"],
                            s["rep"],
                            phase,
                            ns,
                            s["alloc_bytes"][phase],
                            s["gc_ns"][phase],
                        ]
                    )


def write_reports(results: dict, outdir: Path) -> str:
    outdir.mkdir(parents=True, exist_ok=True)
    (outdir / "results.json").write_text(json.dumps(results, indent=1) + "\n")
    write_csvs(results, outdir)
    table = text_table(results)
    (outdir / "summary.txt").write_text(table)
    (outdir / "summary.tex").write_text(latex_table(results))
    return table


def main() -> None:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    ap.add_argument(
        "--config",
        type=Path,
        default=ROOT / "bench" / "benchmarks.toml",
        help="workload file (TOML)",
    )
    ap.add_argument(
        "--filter",
        action="append",
        metavar="REGEX",
        help="only run task ids matching (repeatable)",
    )
    ap.add_argument(
        "--exclude",
        action="append",
        metavar="REGEX",
        help="skip task ids matching (repeatable)",
    )
    ap.add_argument(
        "--group", action="append", help="only run tasks in this group (repeatable)"
    )
    ap.add_argument(
        "--skip-group", action="append", help="skip tasks in this group (repeatable)"
    )
    ap.add_argument(
        "--out",
        type=Path,
        help="output directory (default: bench/results/<timestamp>[-label])",
    )
    ap.add_argument(
        "--label",
        help="suffix for the default output directory, recorded in results.json",
    )
    ap.add_argument(
        "--list", action="store_true", help="list the selected tasks and exit"
    )
    ap.add_argument(
        "--no-build", action="store_true", help="use the existing pipeline-bench build"
    )
    ap.add_argument(
        "--quick",
        action="store_true",
        help="smoke test: warmup 1, 3 repetitions, no minimum time",
    )
    ap.add_argument(
        "--warmup", type=int, help="override warmup repetitions for every task"
    )
    ap.add_argument(
        "--min-reps", type=int, help="override minimum measured repetitions"
    )
    ap.add_argument(
        "--max-reps", type=int, help="override maximum measured repetitions"
    )
    ap.add_argument(
        "--min-time", type=float, help="override minimum measured seconds per task"
    )
    ap.add_argument(
        "--timeout", type=float, help="override the per-task timeout in seconds"
    )
    ap.add_argument("--rts", help="override the extra RTS flags, e.g. '-A64m'")
    ap.add_argument("--pin-cpu", help="run every task under taskset -c PIN_CPU")
    ap.add_argument(
        "--report",
        type=Path,
        metavar="RESULTS_JSON",
        help="regenerate the tables of an earlier run and exit",
    )
    args = ap.parse_args()

    if args.report:
        results = json.loads(args.report.read_text())
        print(write_reports(results, args.report.parent), end="")
        return

    try:
        cfg = load_config(args.config)
        tasks = select_tasks(expand_tasks(cfg), args)
    except (ConfigError, tomllib.TOMLDecodeError, KeyError) as e:
        sys.exit(f"{args.config}: {e}")
    apply_overrides(tasks, args)
    if args.list:
        for t in tasks:
            print(
                f"{t['id']:60} group={t['group']} warmup={t['warmup']} reps={t['min_reps']}..{t['max_reps']} min_time={t['min_time']}"
            )
        return
    if not tasks:
        sys.exit("no tasks selected")

    exe, build_cmd = build(cfg, args.no_build)
    env = collect_environment(exe, build_cmd, tasks)
    for w in warn_environment(env):
        print(f"warning: {w}", file=sys.stderr)

    stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    outdir = args.out or ROOT / "bench" / "results" / (
        stamp + (f"-{args.label}" if args.label else "")
    )
    results = {
        "schema": RESULTS_SCHEMA,
        "label": args.label,
        "environment": env,
        "config_file": str(args.config),
        "config": cfg,
        "tasks": [],
    }
    with tempfile.TemporaryDirectory(prefix="quarc-bench-") as tmp:
        for i, task in enumerate(tasks, 1):
            print(
                f"[{i}/{len(tasks)}] {task['id']} ...",
                end=" ",
                flush=True,
                file=sys.stderr,
            )
            rec = run_task(exe, task, Path(tmp))
            results["tasks"].append(rec)
            if "error" in rec:
                print(
                    f"ERROR: {rec['error'].splitlines()[-1] if rec['error'] else ''}",
                    file=sys.stderr,
                )
            else:
                tot = rec["stats"]["total"]["wall_ms"]
                print(
                    f"{fmt_ms(tot['median'])}{'' if tot['median'] >= 1000 else ' ms'} median over {tot['n']} reps ({rec['process']['wall_s']:.1f} s)",
                    file=sys.stderr,
                )
            # Keep partial results if the run is interrupted.
            write_reports(copy.deepcopy(results), outdir)
    results["environment"]["loadavg_at_end"] = (
        list(os.getloadavg()) if hasattr(os, "getloadavg") else None
    )
    table = write_reports(results, outdir)
    print()
    print(table, end="")
    print(
        f"\nresults written to {outdir.relative_to(ROOT) if outdir.is_relative_to(ROOT) else outdir}/"
    )
    if any("error" in r for r in results["tasks"]):
        sys.exit(1)


if __name__ == "__main__":
    main()
