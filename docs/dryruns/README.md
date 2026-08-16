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

| Run | Coverage | Grade | Portfolio risk | Quotable-today LOC | LOC needing grammar work |
|---|---|---|---|---|---|
| `bench_corpus` | 1.0000 | PLAUSIBLE | LOW | 281 | 0 |
| `examples_cobol` | 0.7545 | PLAUSIBLE | HIGH | 241 | 27 |
| `aws_carddemo` | 0.8209 | PLAUSIBLE | BLOCKED | 21,160 | 1,744 |
| `omp_cobol_course` | 0.6606 | PLAUSIBLE | BLOCKED | 2,479 | 260 |
| `gnucobol` | 0.5763 | PLAUSIBLE | BLOCKED | 5,575 | 186 |

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

2,190 occurrences of constructs C1 cannot transpile, ranked. The cumulative
column is what makes this actionable: **six constructs account for 70% of
everything blocking migration today.**

| Rank | Construct | Occurrences | Share | Cumulative |
|---|---|---|---|---|
| 1 | `EXIT` | 368 | 16.8% | 16.8% |
| 2 | `EXEC` (CICS/SQL) | 306 | 14.0% | 30.8% |
| 3 | `CONTINUE` | 271 | 12.4% | 43.2% |
| 4 | `WRITE` | 224 | 10.2% | 53.4% |
| 5 | `GO TO` | 186 | 8.5% | 61.9% |
| 6 | `CALL` | 183 | 8.4% | 70.2% |
| 7 | `INITIALIZE` | 124 | 5.7% | 75.9% |
| 8 | `STRING` | 121 | 5.5% | 81.4% |
| 9 | `OPEN` | 96 | 4.4% | 85.8% |
| 10 | `CLOSE` | 91 | 4.2% | 90.0% |
| 11 | `GOBACK` | 57 | 2.6% | 92.6% |
| 12 | `READ` | 56 | 2.6% | 95.1% |
| 13 | `COPY` | 50 | 2.3% | 97.4% |
| 14 | `SUBTRACT` | 18 | 0.8% | 98.2% |
| 15– | `REWRITE`, `USE`, `CANCEL`, `DELETE`, `GENERATE`, `ALTER`, `ENTRY`, `SORT`, `DIVIDE` | 39 | 1.8% | 100% |

Reading it as Phase 4 work items, cheapest-first:

1. **`EXIT`, `CONTINUE`, `GOBACK` (696 occurrences, 32%)** are no-ops or a
   return. They are the single cheapest third of the backlog and need no new
   semantics — only dispatch entries.
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
| VALUE clause on a data item | 589 | accepted_ignored |
| USAGE COMP / BINARY | 206 | accepted_ignored |
| REDEFINES | 103 | accepted_ignored |
| FILE SECTION (FD) record | 54 | unsupported |
| USAGE COMP-3 (packed decimal) | 30 | accepted_ignored |
| OCCURS fixed size | 24 | supported |
| OCCURS DEPENDING ON | 21 | accepted_ignored |

`VALUE` at 589 occurrences deserves attention: a `VALUE` clause on a working-storage
item is an *initialiser*, and C1 discards it while initialising every numeric
field to zero and every alphanumeric field to the empty string. On the bench
corpus that is harmless because nothing there relies on a non-zero initial value.
On CardDemo it is 589 places where it might not be. This is not a hypothetical —
it is the class of defect the benchmark exists to catch, and it argues for
covering `VALUE` in RELIAN-BENCH before claiming any of these programs.

## Reproducing

```bash
python -m src.assessment.cli bench/corpus     --out docs/dryruns/bench_corpus
python -m src.assessment.cli examples/cobol   --out docs/dryruns/examples_cobol
git clone --depth 1 https://github.com/aws-samples/aws-mainframe-modernization-carddemo.git /tmp/carddemo
python -m src.assessment.cli /tmp/carddemo    --out docs/dryruns/aws_carddemo
```

`assessment.sha256` in each directory is the hash of that run's `assessment.json`.
Re-running on the same input reproduces it exactly.
