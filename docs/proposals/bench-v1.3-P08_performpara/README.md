# Proposal — RELIAN-BENCH v1.3 corpus program P08_performpara

**Status: DRAFT. Not part of the benchmark.** This directory lives under
`docs/proposals/`, not `bench/`. Nothing here is sealed, scored, or authoritative
until the operator acts on it (see "What the operator must do"). It is drafted
this way on purpose: `bench/` is operator-only (CLAUDE.md rule 4), and a benchmark
an agent could edit is worthless.

## Why this program should exist

The WP-1.9 dry runs, re-measured after PR #16, show **out-of-line `PERFORM` is the
single largest blocker to migrating real COBOL**: 1,380 occurrences, 42.6% of all
unsupported-construct occurrences across the three third-party corpora. With
paragraph `EXIT` (367), which is the same feature — a performed paragraph needs an
exit — performed paragraphs are **53.9%** of all blockers, larger than CICS, file
I/O, and `GO TO` combined.

RELIAN-BENCH v1.2 does not cover it. Every `PERFORM` in the sealed corpus is the
inline `VARYING` form. So by R7, no implementation of performed-paragraph support
may merge until a sealed benchmark covers it — and the bench commit must predate
the grammar/dispatch merge. This program is the proposed coverage.

P07_exitflow's own header already anticipated this program: rev 1 tried to carry
out-of-line `PERFORM` and paragraph `EXIT`, could not gate the three WP-1.5.5
handlers alone, and moved them to the deferred list (Bugbot, PR #10). This is that
deferred work, packaged for sealing.

## What it exercises, and how a wrong implementation fails

`PERFPAR01` is a commission calculator. Every construct in it **except**
out-of-line `PERFORM` and paragraph `EXIT` is already corpus-proven and
measured-supported (ACCEPT, UNSTRING, COMPUTE incl. ROUNDED, MOVE, IF/ELSE,
EVALUATE `<selector>`/WHEN, DISPLAY, GOBACK, MOVE n TO RETURN-CODE, FUNCTION
NUMVAL/TRIM, COMP-3, edited pictures). So a red→green transition on this program
is attributable to performed-paragraph support **alone** — the isolation is
verified below.

Six behaviours the vectors pin, each with the failure mode it catches:

| Behaviour | Where | A wrong implementation… |
|---|---|---|
| **Return after PERFORM** | every call site | …that jumps without returning loses every later paragraph's effect. |
| **No fall-through** | `VALIDATE-INPUT` is followed in source by `SET-RATE` | …that runs on into the next paragraph computes a rate on a rejected record; the reject vectors diverge. |
| **Reuse from ≥2 sites** | `COMPUTE-BONUS` performed from two sites with different `WS-TIER` | …that inlines the first call and drops the second returns `BONUS=0.00` on the 5-year tier. |
| **Nested PERFORM** | `APPLY-TIER` is performed and itself performs `COMPUTE-BONUS` | …that is one level deep fails. |
| **Paragraph `EXIT`** | every paragraph ends with bare `EXIT` | …that treats bare `EXIT` as anything but a no-op-that-returns diverges. |
| **`RETURN-CODE`** | reject path does `MOVE 8 TO RETURN-CODE` | …that drops `RETURN-CODE` semantics fails on exit code (WP-1.5.0d compares it). |

The vectors also exercise `ROUNDED` HALF_UP on a value that is not exact
(`1234.10 × 0.05 = 61.705 → 61.71`) and bonus rounding stacked on an
already-rounded base, so a `RoundingMode` slip is caught too.

## Draft vectors (`public.jsonl`)

14 vectors, **every one produced by executing the oracle**, not written by hand:

```
cobc -x -o perfpar01 program.cbl        # GnuCOBOL 3.1.2.0
echo "125000.00,1,12" | ./perfpar01     # -> STATUS=OK BASE=2500.00 BONUS=250.00 TOTAL=2750.00 (exit 0)
```

Coverage of the input space: all four rate codes; the tier boundaries at
`YEARS = 10`, `9`, `5`, `4`; the no-bonus tier; both rounding cases; max accepted
sales (`999999.99`); and all three reject paths (sales over limit, ratecode below
range, ratecode above range) with their exit-8. `expected_exit` is explicit on
every vector.

These are the **PUBLIC** split only. The held-out split is not in this proposal
and must not be — see below.

## Bench-first red — verified now, at this commit

The point of a bench-first program is that it fails before the work lands. At the
transpiler on this branch (`transpiler/c1_rulebased.py`, PR #16 state):

```
$ python3 -c "from transpiler.c1_rulebased import Transpiler; \
    Transpiler(open('program.cbl').read(), 'Perfpar01').transpile()"
UnsupportedConstruct: unsupported COBOL construct 'PERFORM' at source line 79 in paragraph MAIN-PARA
```

Refused, by name, at the first out-of-line `PERFORM`. No Java emitted.

## Isolation — verified now

The read-only assessment engine over this program reports **exactly** the two
constructs this program is meant to gate, and nothing else:

```
$ python3 -m src.assessment.cli program.cbl --out /tmp/p08 --no-docx
coverage : 0.7255 (PLAUSIBLE)   # 37/51 statements
unsupported ranked: [('PERFORM', 8), ('EXIT', 6)]
```

`PERFORM ×8` and `EXIT ×6` are the only unsupported constructs. When the two are
implemented and bench-sealed, this program goes to 51/51 and — if the transpiler
is correct — BER 1.0 on both splits. If anything else were unsupported here, the
program would not isolate the feature, and the delta could not be attributed.

## What the operator must do (agent cannot)

1. **Generate the held-out split** with the sealed generator in
   `relian-bench-private` (`gen_vectors.py` + seed). That generator and seed must
   never enter this repo (rule 6): generator + seed + corpus regenerates the
   held-out set and destroys the benchmark. The agent did not and must not write
   `heldout.jsonl` (rule 1).
2. **Move** `program.cbl` and the public vectors into
   `bench/corpus/P08_performpara/` (rename to the sealed layout: `program.cbl`,
   `vectors/public.jsonl`), add the run script, and register the main class in
   `bench/harness/mains.json` as `{"P08_performpara": "Perfpar01"}`.
3. **Seal** a v1.3 ledger (Ed25519), tag `relian-bench-v1.3`, and merge it. The
   sealed bench commit must predate any dispatch-table/grammar merge that claims
   `PERFORM <paragraph>` or paragraph `EXIT` (R7).
4. Only then may performed-paragraph support be implemented. Expected acceptance:
   held-out BER 1.0 on P08, build 1.0, branch coverage ≥ 0.80, and P01–P07 Java
   byte-identical to their current baseline (the change must be additive).

## Provenance

- Oracle: GnuCOBOL 3.1.2.0, fixed-format, `cobc -x`.
- Vectors: stdout+exit of the oracle on each input, this run. No hand-entered
  expected values.
- Refusal and isolation figures: `transpiler/c1_rulebased.py` and
  `src/assessment/` at the PR #16 commit.
- Determinism: re-running the oracle on the same input reproduces each vector
  exactly; the program is fixed-format so column semantics are stable.
