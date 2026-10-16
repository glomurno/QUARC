# QUARC File Format Reference

QUARC files (`.quarc`) define quantum communication protocols in the lqCCS process algebra. A file contains definitions (operators, measurements, named states, processes), one or more `start` blocks specifying what to run, and optionally tag-equivalence and silent-tag declarations for bisimilarity checking.

---

## Comments

Line comments start with `--`:

```quarc
-- this is a comment
proc Alice(q: qubit) :=
    H(q).disc(q)   -- inline comment
```

Block comments use `{- ... -}`:

```quarc
{-
  Multi-line comment.
-}
```

---

## Top-level structure

All top-level items are **order-independent** relative to each other, but `start` blocks must come after all definitions.

```quarc
import "path/to/file.quarc"

operator ...
measurement ...
state ...
proc ...

start { ... }

tag-equiv { ... }
silent { ... }
visible { ... }
```

---

## Imports

```quarc
import "path/to/file.quarc"
```

Paths are relative to the importing file's directory. Imports are resolved before everything else. All definitions from the imported file (operators, measurements, states, proc declarations) enter scope. Cyclic imports are detected and reported as errors. Multiple imports are allowed.

```quarc
import "stdlib/bell.quarc"
import "lib/ops.quarc"
```

---

## Operator definitions

### Raw Kraus matrix

```quarc
operator MyGate dim=2 {
    1,0,0,0
    0,1,0,0
    0,0,0,1
    0,0,1,0
}
```

`dim` is the number of qubits the operator acts on. The matrix has `2^dim × 2^dim` entries. Rows can be separated by newlines or semicolons; entries within a row by commas or whitespace.

For a superoperator with multiple Kraus operators:

```quarc
operator Depolarize dim=1 {
    kraus { 1,0; 0,1 }
    kraus { 0,1; 1,0 }
}
```

### Algebraic composition

```quarc
operator CtrlH    := controlled(H)
operator MyAdj    := adjoint(MyGate)
operator BellPrep := sequence(H, CNOT)
operator BigGate  := tensor(H, X)
```

| Combinator         | Meaning                           |
| ------------------ | --------------------------------- |
| `controlled(G)`    | Adds a control qubit to the left; |
| `adjoint(G)`       | Conjugate transpose of `G`        |
| `sequence(G1, G2)` | Apply `G1` then `G2`              |
| `tensor(G1, G2)`   | Kronecker product `G1 ⊗ G2`       |

### Complex literals in matrices

Full form: `a+bi` where `a` and `b` are signed decimals.

```
1+0i    0+1i    0.707+0.707i    -1    2i    0.5    -0.5i
```

### Built-in operators

`H`, `X`, `Z`, `I`, `S`, `ZX`, `CNOT`, `SWAP`, `SetPhiP`

---

## Measurement definitions

```quarc
measurement MyMeas dim=1 {
    effect { 1,0; 0,0 }
    effect { 0,0; 0,1 }
}
```

Each `effect` block is a Kraus operator for one measurement outcome. Outcomes are numbered implicitly in order (0, 1, …).

### Built-in measurements

- `M01`: standard basis measurement on a single qubit.
- `M201`: standard basis measurement on two qubits.
- `MH`: hadamard basis measurement on a single qubit.
- `MHI`: Y-basis measurement on a single qubit.
- `MBell`: Bell basis measurement on two qubits.
- `MRandBit`: Random bit generator.

---

## Named quantum states

```quarc
state Bell  := density 2 H(0).CNOT(0,1)
state GHZ   := density 3 H(0).CNOT(0,1).CNOT(1,2)
state Mixed := density 1 dm { 0.5,0; 0,0.5 }
```

Named states can be referenced by name inside `start` blocks.

### State specification forms

**Implicit (operator sequence):** `density <dim> Operator(qubits)...`

Starts from the all-zero state `|0...0⟩⟨0...0|` and applies the listed gates left to right. Qubit indices are zero-based.

```quarc
state Bell := density 2 H(0).CNOT(0,1)
```

**Explicit density matrix:** `density <dim> dm { entries }`

Entries separated by commas, semicolons, or whitespace. Row-per-line layout is accepted.

```quarc
state Mixed := density 2 dm {
    0.5,0,0,0.5;
    0,0,0,0;
    0,0,0,0;
    0.5,0,0,0.5
}
```

**Raw flat matrix:** `density <dim> <entries>` (no `dm` keyword)

```quarc
state Zero := density 1 1,0,0,0
```

---

## Process declarations

```quarc
proc Alice[q](c: chan bit, x: bit) :=
    t1: H(q).
    t2: c!x.
    disc(q)
```

### Parameter sorts

Qubit parameters are declared in square brackets before the regular parameter list.
Channel parameters must come before value parameters within `(...)`.

| Sort     | Declared as                      |
| -------- | -------------------------------- |
| `qubit`  | `[q, r, ...]` (square brackets)  |
| `chan T` | `(c: chan T, ...)` before values |
| `bit`    | `(x: bit, ...)`                  |
| `bits N` | `(x: bits 2, ...)`               |
| `bool`   | `(b: bool, ...)`                 |

`T` in `chan T` can be `bit`, `bits <n>`, `bool`, or `qubit`.

### Tags

Tags appear as `label:` before an action:

```quarc
proc Bob[q](c: chan bit) :=
    t0: c?y.
    t1: M01(q > r).
    out!r.disc(q)
```

Tags are optional.
Actions without a tag are assigned one automatically.

### Tag offsets

When a declaration is meant to be instantiated multiple times in parallel, each instance must use distinct tags so the LTS analysis can distinguish them. The `tag_offset` modifier lets a declaration declare one or more named tag bases that callers supply as concrete integers. Inside the body, offset tags are written as `$base+N`; the concrete tag resolved at the call site is `supplied_value + N`.

**Declaration:**

```quarc
proc Agent[q](out: chan bit) with tag_offset(base) :=
    $base+0: H(q).
    $base+1: M01(q > x).
    out!x.disc(q)
```

Multiple bases are allowed:

```quarc
proc Alice[q](a: chan bit, b: chan bit) with tag_offset(sendBase), tag_offset(recvBase) :=
    $sendBase+0: a!0.
    $recvBase+0: b?x.
    disc(q)
```

**Call site:**

Callers supply a concrete integer for each declared base using a `with` clause:

```quarc
@Agent[qb0](c) with base=10
@Agent[qb1](d) with base=20
```

Tags resolve to `t10, t11` and `t20, t21` respectively.
The two instances can then be composed in parallel without tag collisions:

```quarc
(@Agent[qb0](c) with base=10) || (@Agent[qb1](d) with base=20)
```

All declared bases must be supplied at every call site; supplying an undeclared base name is an error.

### Process syntax

| Syntax                           | Meaning                                   |
| -------------------------------- | ----------------------------------------- |
| `tau.P`                          | Silent action                             |
| `disc()`                         | End of process with no qubits             |
| `disc(q,...)`                    | End of process and discards passed qubits |
| `S(q,...).P`                     | Apply the superoperator `S`               |
| `M(q,... > x).P`                 | Measure with `M`, bind result to `x`      |
| `c!v.P`                          | Send value `v` on channel `c`             |
| `c?x.P`                          | Receive on channel `c`, bind to `x`       |
| `P + Q`                          | Non-deterministic choice                  |
| `P \|\| Q`                       | Parallel composition                      |
| `P \ c`                          | Restriction (hide channel `c`)            |
| `@Name[qargs](args)`             | Call named process                        |
| `@Name[qargs](args) with base=N` | Call with tag offset                      |

### Syntactic Sugar

| Sugar                     | Syntax                        |
| ------------------------- | ----------------------------- |
| `(a1.a2 \|\| b1.b2.b3).P` | `(a1.a2.P) \|\| (b1.b2.b3.P)` |
| `(a1.a2.a3 + b1.b2).P`    | `(a1.a2.a3.P) + (b1.b2.P)`    |
| `(if e then a else b).P`  | `if e then a.P else b.P`      |

---

## Start blocks

Start blocks specify the initial quantum state and process to evaluate.

### Single configuration

```quarc
start {
    density Bell
    process (@Alice[qb0](c, 42) || @Bob[qb1](c)) \ c
}
```

`density Bell` references a named state. Any state specification form (implicit, `dm { }`, raw matrix) also works inline:

```quarc
start {
    density 2 H(0).CNOT(0,1)
    process disc()
}
```

### Named start blocks

For comparing two protocols with the `equiv` command:

```quarc
start "honest" {
    density Bell
    process (@Alice[qb0](c) || @Bob[qb1](c)) \ c
}

start "adversary" {
    density Bell
    process @Eve[qb0, qb1](c)
}
```

Names are arbitrary strings. The `equiv` command selects blocks by name:

```
quarc equiv --config1 honest --config2 adversary protocol.quarc
```

Without `--config1`/`--config2`, `equiv` takes two separate files:

```
quarc equiv file1.quarc file2.quarc
```

---

## Tag equivalence

Declares that actions with different tags are considered equivalent for bisimilarity checking. This is used by the `equiv` and `minimize` commands.

### Global (applies to all configs)

```quarc
tag-equiv {
    10 ~ 30
    (10, 20) ~ (30, 40)
}
```

Multiple blocks and multiple pairs per block are accepted. Blocks may appear before or after `start` blocks.

### Scoped (applies to a specific config pair)

```quarc
tag-equiv "honest" "adversary" {
    10 ~ 30
    (10, 20) ~ (30, 40)
}
```

Scoped equivalences are only applied when comparing the two named configs. The pair order does not matter (`"honest" "adversary"` and `"adversary" "honest"` are equivalent).

## Silent tags

Under branching bisimilarity (`--equivalence branching`), `tau` steps are silent: they can be skipped as long as they don't change the observable behaviour or the environment. By default every `tau` tag is silent, including gate and measurement steps and synchronisations. `silent` and `visible` blocks choose which ones:

```quarc
silent { 1 2 (10, 20) }   -- only these tau tags are silent
visible { 7 }             -- keep tag 7 observable
```

- If any `silent` tags are listed, only those are silent; otherwise every `tau` tag is.
- `visible` tags are then removed from the silent set.
- Silence is decided after tag equivalence, so equivalent tags are silent together.
- A non-silent `tau` counts as a visible action and is only matched by a `tau` with the same (equivalent) tag.

Blocks can be repeated and may appear before or after `start` blocks. Like `tag-equiv`, they can be scoped to a pair of configs, in which case they only apply when comparing those two:

```quarc
visible "honest" "adversary" { 7 }
```

`minimize` uses the global blocks. `equiv` uses the global blocks plus the ones scoped to the compared pair. The `--silent` and `--visible` options add to the file's blocks. Silence has no effect on strong bisimilarity.
