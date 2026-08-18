# Meridian MUD — Relian demonstration corpus

**Synthetic. Not production. Not derived from any customer or benchmark
source.** This is a hand-written COBOL-85 code set built to exercise the Relian
pipeline end to end for a demo / proof-of-concept / MVP walkthrough. It stands
in for the batch billing suite of a fictional municipal water utility,
*Meridian Municipal Utility District* (Meridian MUD).

Nothing here overlaps the sealed benchmark corpus under `bench/`, and no file
here was read from or copied out of `relian-bench-private`. The programs were
written against the transpiler's *observed* supported subset
(`docs/C1_SUPPORTED_VERBS_OBSERVED.md`) so that the demo lands on each of the
outcomes Relian is designed to report honestly.

## What the set is for

The set is arranged in three tiers so a single assessment run shows the whole
honesty spectrum — clean migration, partial coverage, and a principled refusal
— instead of only the happy path.

| Program | Tier | What it demonstrates |
|---|---|---|
| `src/MUBRATE.cbl` | A — fully in subset | Six-block inclining rate schedule. COMP-3 arithmetic, `EVALUATE`, `PERFORM VARYING`, `OCCURS`/`SEARCH`, `INSPECT`, edited pictures, `ROUNDED` vs. truncating `COMPUTE`, non-zero `RETURN-CODE` exits. Transpiles, compiles, runs. |
| `src/MUBPENL.cbl` | A — fully in subset | Delinquency penalty + simple daily interest. Carries a **deliberate** rounded-vs-unrounded pair (assessed interest is `ROUNDED`; the GL memo figure is a plain `COMPUTE` that truncates) to show the two COBOL rounding modes side by side. 88-levels with multiple `VALUE`s, `VALUE` initialisation, `FUNCTION TRIM`/`LENGTH`/`NUMVAL`, `AND` conditions. |
| `src/MUBSURC.cbl` | A — fully in subset | Conservation surcharge with a 12-entry history table and a statutory exemption `SEARCH`. Ends with `EXIT PROGRAM` (called-module form). |
| `src/MUBBILL.cbl` | B — partial | Bill assembly. Business arithmetic is in-subset, but module linkage (`CALL`), text assembly (`STRING`), the alternate arithmetic forms (`SUBTRACT/MULTIPLY/DIVIDE … GIVING`, `ADD … GIVING`) and a paragraph `PERFORM` are not. Produces a **partial coverage** figure and an honest transpile refusal — never fabricated Java. |
| `src/MUBPOST.cbl` | C — blocked | Nightly cash posting against an indexed master. File handling (`SELECT/FD/OPEN/READ/WRITE/REWRITE/START/CLOSE`), unstructured flow (`GO TO`, `GO TO … DEPENDING ON`, `ALTER`), a storage overlay (`REDEFINES`) and a variable table (`OCCURS DEPENDING ON`). Assesses **BLOCKED**; the transpiler refuses at the first unsupported verb. |

Supporting artifacts exercise the rest of the intake surface:

- `copy/MUBCONS.cpy`, `copy/MUBBREC.cpy`, `copy/MUBCUST.cpy` — copybooks, to
  populate the copybook fan-in table.
- `jcl/MUBNITE.jcl` — the nightly job stream, classified as `jcl` in the
  manifest (not a program, not transpiled).

## Reproduce the assessment

Read-only and offline. From the repository root:

```bash
python3 -m src.assessment.cli examples/demo --out ./output/demo-assess --no-docx
```

The run assesses 5 programs across 9 manifest files and reports (measured, not
projected): portfolio construct coverage, a per-program coverage map, the
unsupported-construct inventory with source lines, the DATA DIVISION feature
table (note COMP-3 and REDEFINES report as `accepted_ignored`, not
`supported`), complexity, copybook fan-in, and a per-program risk tier spanning
LOW → MED → BLOCKED. Every figure carries a Trutina grade and a provenance
string; two runs over the same tree produce byte-identical output.

## Reproduce the tier-A transpiles

```bash
python3 examples/demo/run_demo.py
```

This transpiles the three tier-A programs through the deterministic C1 core,
compiles the emitted Java with `javac`, runs each on a sample record from
`data/`, and confirms the tier-B/C programs refuse rather than emit a
placeholder. It writes nothing back into the repository.

## Run the assessment from the UI

The same assessment is reachable from the Relian frontend. The API exposes it
read-only and offline at `GET /api/v1/assess/demo` (see `src/api/main.py`),
which runs the engine over this corpus and returns the measured bundle plus a
`report_hash` computed exactly as the CLI computes `assessment.sha256`. The
corpus measurements match the CLI's byte for byte; the hash itself embeds
`tool_versions` (invocation, Python, platform), so it equals the CLI's only when
produced in the same runtime — the manifest hash, taken over the source bytes,
matches regardless. The **Assess (demo)** tab in the UI (`src/ui/views/AssessView.tsx`)
renders the portfolio coverage, per-program risk tiers (LOW → MED → BLOCKED),
and the ranked unsupported-construct inventory, each figure carrying its Trutina
grade. Start both services and open the tab:

```bash
uvicorn src.api.main:app          # API on :8000
npm run dev                       # UI on :5173
```
