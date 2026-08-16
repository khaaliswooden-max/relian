# WP-1.9 dry runs — what real COBOL actually contains

Every number below was produced by `python -m src.assessment.cli <root> --out <dir>`
with **zero manual intervention** — no per-codebase configuration, no hand-editing
of output, no re-runs to get a better answer.

**Third-party source is not committed here.** Only the assessment artifacts are:
`assessment.json` (the canonical ledger), `assessment.md`, and
`assessment.sha256`. `assessment.docx` is regenerable from the same command and
is committed only for `bench_corpus`, as evidence the DOCX path works.

The report title in each `assessment.md` shows the scratch path the codebase was
cloned to. The identity of the code is the manifest hash, not that path.

## Corpora

| Run | Source | License | Programs | Files | Wall seconds |
|---|---|---|---|---|---|
| `bench_corpus` | `bench/corpus` (this repo) | internal | 5 | 15 | 0.53 (incl. DOCX) |
| `examples_cobol` | `examples/cobol` (this repo) | internal | 1 | 1 | 0.24 |
| `aws_carddemo` | github.com/aws-samples/aws-mainframe-modernization-carddemo | Apache-2.0 | 44 | 329 | 4.61 |
| `omp_cobol_course` | github.com/openmainframeproject/cobol-programming-course | CC-BY-4.0 | 30 | 360 | 0.95 |
| `gnucobol` | github.com/OCamlPro/gnucobol | GPL-3.0 | 7 | 406 | 1.73 |

Wall seconds are `--no-docx` (analysis + JSON + Markdown) except where noted.
DOCX rendering is not in the same league: the 44-program CardDemo report analysed
in 4.61 s and was still inside `python-docx` after seven minutes when the render
was killed. Hence `--no-docx`, and hence the committed third-party runs carry no
DOCX.

All three third-party licenses permit analysis. Nothing was redistributed.

`programs/` was named in the work package as a dry-run target; it holds the Rust
Solana program (Phase 3 scaffolding), not COBOL, so it is not a dry run — the CLI
correctly reports zero programs on it.

## Results

> **Re-run after WP-1.5.4 / WP-1.5.5** (VALUE clause; CONTINUE / GOBACK /
> EXIT PROGRAM). The committed artifacts and the tables below are the re-run;
> the original WP-1.9 figures appear in the before/after table at the end.
> Input trees are content-identical to the originals (same manifest hashes),
> so every delta is attributable to the transpiler change alone.

| Run | Coverage | Grade | Portfolio risk | Quotable-today LOC | LOC needing grammar work |
|---|---|---|---|---|---|
| `bench_corpus` (now 7 programs, v1.2) | 1.0000 | PLAUSIBLE | LOW | 384 | 0 |
| `examples_cobol` | 0.7545 | PLAUSIBLE | HIGH | 241 | 27 |
| `aws_carddemo` | 0.8511 | PLAUSIBLE | BLOCKED | 21,454 | 1,450 |
| `omp_cobol_course` | 0.6945 | PLAUSIBLE | BLOCKED | 2,505 | 234 |
| `gnucobol` | 0.5968 | PLAUSIBLE | BLOCKED | 5,584 | 177 |

Two results are worth reading carefully.

**`bench_corpus` scores 1.0000 and the same corpus scores BER 1.0000 on the
public split.** The analyzer and the transpiler were built independently — one
walks source, the other emits Java — and they agree that the corpus is fully
inside the supported set. That is a genuine cross-validation, not a tautology,
and it is the only place in this table where "quotable today" means all of it.

**Every third-party run is graded PLAUSIBLE, and every program in every one of
them was analysed by `token_scan`.** Not one real-world program parsed cleanly
under the ANTLR grammar bundled in this repo. That is the Phase 1 finding that
most affects Phase 2 planning; see "Escalation" in `docs/PHASE1_LOG.md`.

## The demand signal — unsupported constructs across the three real-world corpora

1,861 occurrences of constructs C1 cannot transpile (2,190 before
WP-1.5.4/1.5.5 — `CONTINUE` and `GOBACK` cleared entirely, and one same-line
`EXIT PROGRAM`; the 367 remaining `EXIT`s are paragraph exits or line-final,
deliberately still unsupported pending performed-paragraph work).

| Rank | Construct | Occurrences | Share | Cumulative |
|---|---|---|---|---|
| 1 | `EXIT` (paragraph form) | 367 | 19.7% | 19.7% |
| 2 | `EXEC` (CICS/SQL) | 306 | 16.4% | 36.2% |
| 3 | `WRITE` | 224 | 12.0% | 48.2% |
| 4 | `GO TO` | 186 | 10.0% | 58.2% |
| 5 | `CALL` | 183 | 9.8% | 68.0% |
| 6 | `INITIALIZE` | 124 | 6.7% | 74.7% |
| 7 | `STRING` | 121 | 6.5% | 81.2% |
| 8 | `OPEN` | 96 | 5.2% | 86.4% |
| 9 | `CLOSE` | 91 | 4.9% | 91.2% |
| 10 | `READ` | 56 | 3.0% | 94.3% |
| 11 | `COPY` | 50 | 2.7% | 96.9% |
| 12 | `SUBTRACT` | 18 | 1.0% | 97.9% |
| 13– | `REWRITE`, `USE`, `CANCEL`, `DELETE`, `GENERATE`, `ALTER`, `ENTRY`, `SORT`, `DIVIDE` | 39 | 2.1% | 100% |

Reading it as Phase 4 work items, cheapest-first:

1. ~~`EXIT`, `CONTINUE`, `GOBACK` — dispatch entries~~ **Done for
   `CONTINUE`, `GOBACK` and the qualified `EXIT PROGRAM` (WP-1.5.5,
   bench-gated by RELIAN-BENCH v1.2's P07_exitflow). Paragraph `EXIT`
   remains: it is inseparable from performed-paragraph support (Bugbot
   finding, PR #10) and is NOT dispatch-table-only work.**
2. **`SUBTRACT` and `DIVIDE` (20)** are arithmetic C1 already models; `SUBTRACT`
   is currently a statement-boundary token with no handler, which is the exact
   silent-drop hazard WP-0.3 documented.
3. **Sequential file I/O — `WRITE`, `OPEN`, `CLOSE`, `READ`, `REWRITE` (473,
   22%)** is one coherent feature, and it pairs with the DATA DIVISION gap
   below: `FILE SECTION (FD) record` is `unsupported`, appearing 54 times in
   CardDemo alone. File I/O is the largest single semantic addition and the one
   that turns batch programs from partially-quotable to fully-quotable.
4. **`GO TO` and `CALL` (369, 17%)** change the control-flow model — inter-paragraph
   jumps and inter-program linkage. Real work, not dispatch-table work.
5. **`EXEC CICS` / `EXEC SQL` (306, 14%)** is a different problem entirely:
   transaction and database middleware, not a COBOL construct. This is a
   product decision before it is an engineering one.

## DATA DIVISION features — where "it parses" is not "it is supported"

Counts from CardDemo, the most representative corpus. `accepted_ignored` means
the clause parses and the field exists, but the clause itself is discarded, so
generated Java cannot depend on it.

| Feature | Occurrences | C1 status |
|---|---|---|
| 88-level condition name | 840 | supported |
| VALUE clause on a data item | 589 | supported (WP-1.5.4) |
| USAGE COMP / BINARY | 206 | accepted_ignored |
| REDEFINES | 103 | accepted_ignored |
| FILE SECTION (FD) record | 54 | unsupported |
| USAGE COMP-3 (packed decimal) | 30 | accepted_ignored |
| OCCURS fixed size | 24 | supported |
| OCCURS DEPENDING ON | 21 | accepted_ignored |

`VALUE` at 589 occurrences deserved attention: a `VALUE` clause on a
working-storage item is an *initialiser*, and C1 used to discard it while
initialising every numeric field to zero and every alphanumeric field to the
empty string. **As of WP-1.5.4 the probe reports `supported`**: numeric,
alphanumeric and COMP-3 VALUE, group-level VALUE spread over subordinates,
and multi-literal 88-levels are modelled and initialise the generated Java —
bench-gated by RELIAN-BENCH v1.2's P06_valinit, whose vectors fail any
zero-initialising migration on vector 1. Unrepresentable VALUE forms raise
`UnsupportedConstruct`-style errors rather than silently zero-initialising
(R2).

## Before / after — WP-1.5.4 + WP-1.5.5 re-run (same input trees)

Coverage is statements-supported / statements-recovered, token-scan, graded
PLAUSIBLE. Manifest hashes match the original WP-1.9 runs, so the input code
is byte-identical and the delta is the transpiler change alone.

| Run | Before | After | Newly supported statements |
|---|---|---|---|
| `aws_carddemo` | 0.8209 (7,994/9,738) | **0.8511** (8,288/9,738) | +294 |
| `omp_cobol_course` | 0.6606 (506/766) | **0.6945** (532/766) | +26 |
| `gnucobol` | 0.5763 (253/439) | **0.5968** (262/439) | +9 |
| `examples_cobol` | 0.7545 (83/110) | 0.7545 (83/110) | 0 (no CONTINUE/GOBACK/EXIT PROGRAM) |
| `bench_corpus` | 1.0000 (126/126, 5 programs) | **1.0000** (173/173, 7 programs, v1.2) | corpus grew |

+329 across the three real-world corpora = the demand table's `CONTINUE`
(271) + `GOBACK` (57) + 1 same-line `EXIT PROGRAM`. Paragraph `EXIT` (367)
is deliberately unclaimed.

## Reproducing

```bash
python -m src.assessment.cli bench/corpus     --out docs/dryruns/bench_corpus
python -m src.assessment.cli examples/cobol   --out docs/dryruns/examples_cobol
git clone --depth 1 https://github.com/aws-samples/aws-mainframe-modernization-carddemo.git /tmp/carddemo
python -m src.assessment.cli /tmp/carddemo    --out docs/dryruns/aws_carddemo
```

`assessment.sha256` in each directory is the hash of that run's `assessment.json`.
Re-running on the same input reproduces it exactly.
