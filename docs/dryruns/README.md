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

> **Re-run after PR #16** (`PERFORM` registered under its qualified key). The
> committed artifacts and the tables below are that re-run. Input trees are
> content-identical to every prior run (same manifest hashes), so every delta
> is attributable to the transpiler change alone.
>
> **These figures are LOWER than the WP-1.5.4/1.5.5 ones they replace, and the
> earlier numbers were wrong.** `PERFORM` was registered as a bare verb while
> only the inline `PERFORM VARYING` form had a handler, so the analyzer —
> which reads the dispatch table — counted every out-of-line
> `PERFORM <paragraph>` as transpilable. It is not. Correcting the
> registration removed 1,380 falsely-claimed statements across the three
> third-party corpora. Nothing about the transpiler's actual capability
> changed; the claim came down to meet it.

> **Re-run after WP-2.0** (the COBOL-85 grammar swap). The committed artifacts
> and the table below are that re-run. Every input tree is byte-identical to the
> previous run — all five manifest hashes match — so every delta is attributable
> to the grammar alone. The transpiler was not touched, and the SUPPORTED set in
> each report's Appendix E is unchanged.

| Run | Coverage | Grade | Portfolio risk | Quotable-today LOC | LOC needing grammar work | Programs on the tree path |
|---|---|---|---|---|---|---|
| `bench_corpus` (7 programs, v1.2) | 1.0000 | **VERIFIED** | LOW | 384 | 0 | **7 of 7** |
| `examples_cobol` | 0.5818 | PLAUSIBLE | BLOCKED | 222 | 46 | 0 of 1 |
| `aws_carddemo` | 0.7248 | PLAUSIBLE | BLOCKED | 20,224 | 2,680 | 2 of 44 |
| `omp_cobol_course` | 0.5287 | PLAUSIBLE | BLOCKED | 2,378 | 361 | 5 of 30 |
| `gnucobol` | 0.5444 | PLAUSIBLE | BLOCKED | 5,561 | 200 | 0 of 6 |

Only the grade column and the last column moved. Every coverage ratio outside
`bench_corpus` is unchanged to four decimal places, and `bench_corpus` went from
173/173 to 174/174 — the tree also counts the `MOVE` in
`P04_taxtable/program.cbl:54`, `AT END MOVE 5 TO WS-IDX`, which the token scan
skips because `AT` is not a statement-start context (its documented
under-count, rule 4). Coverage is a statement about the transpiler,
not the parser, so a grammar swap moving it would have been a bug.

Two results are worth reading carefully.

**`bench_corpus` scores 1.0000 and the same corpus scores BER 1.0000 on the
public split.** The analyzer and the transpiler were built independently — one
walks source, the other emits Java — and they agree that the corpus is fully
inside the supported set. That is a genuine cross-validation, not a tautology,
and it is the only place in this table where "quotable today" means all of it.

**Seven real-world programs now parse cleanly, and none did before WP-2.0.**
The Phase 1 finding this section used to record — that not one real-world
program parsed under the reduced grammar this repo shipped, so every result was
`token_scan`/PLAUSIBLE — was the reason for the grammar swap, and it no longer
holds: 2 CardDemo programs and 5 from the COBOL course reach `antlr_tree` and
grade VERIFIED.

What still falls back is worth reading, because it is not grammar weakness:

* **`COPY`.** 40 of CardDemo's 44 programs carry one. `COPY` is a lexer token
  in the vendored grammar that no parser rule references — upstream consumes it
  in a separate preprocessor grammar this repo vendors but does not yet run —
  so those programs cannot parse cleanly by construction.
* **Dialect, not COBOL-85.** `examples_cobol` uses `EXIT PERFORM`
  (COBOL-2002); `gnucobol` is a compiler test suite using `BINARY-LONG`, `@`
  test macros and compiler directives. A COBOL-85 grammar rejecting these is
  correct behaviour, not a defect.
* **Comment entries.** The free text after `AUTHOR.` needs a `*>CE` marker that
  upstream's preprocessor inserts.

The fallback therefore stays, and every result still says which method produced
it. See `docs/GRAMMAR_PROVENANCE.md` and the WP-2.0 entry in
`docs/PHASE2_LOG.md`.

## The demand signal — unsupported constructs across the three real-world corpora

3,241 occurrences of constructs C1 cannot transpile. The jump from the 1,861
previously reported is not new code and not a regression — it is the 1,380
out-of-line `PERFORM`s that were always unsupported and were being counted as
supported (PR #16).

| Rank | Construct | Occurrences | Share | Cumulative |
|---|---|---|---|---|
| 1 | `PERFORM` (out-of-line / non-VARYING) | 1,380 | 42.6% | 42.6% |
| 2 | `EXIT` (paragraph form) | 367 | 11.3% | 53.9% |
| 3 | `EXEC` (CICS/SQL) | 306 | 9.4% | 63.3% |
| 4 | `WRITE` | 224 | 6.9% | 70.2% |
| 5 | `GO TO` | 186 | 5.7% | 76.0% |
| 6 | `CALL` | 183 | 5.6% | 81.6% |
| 7 | `INITIALIZE` | 124 | 3.8% | 85.4% |
| 8 | `STRING` | 121 | 3.7% | 89.2% |
| 9 | `OPEN` | 96 | 3.0% | 92.1% |
| 10 | `CLOSE` | 91 | 2.8% | 94.9% |
| 11 | `READ` | 56 | 1.7% | 96.7% |
| 12 | `COPY` | 50 | 1.5% | 98.2% |
| 13 | `SUBTRACT` | 18 | 0.6% | 98.8% |
| 14– | `REWRITE`, `USE`, `CANCEL`, `DELETE`, `GENERATE`, `ALTER`, `ENTRY`, `SORT`, `DIVIDE` | 39 | 1.2% | 100% |

**Out-of-line `PERFORM` is now, by a wide margin, the single largest blocker to
migrating real COBOL** — bigger than CICS, file I/O and `GO TO` individually.
It also subsumes the paragraph-`EXIT` item below: the two are the same feature
(a performed paragraph needs an exit), so items 1 and 2 together are 53.9% of
all blockers and are one work package, not two.

Reading it as Phase 4 work items, cheapest-first:

0. **Performed paragraphs — `PERFORM <paragraph>` + paragraph `EXIT` (1,747,
   53.9%)** is now the top item and was previously invisible, because the
   analyzer counted these `PERFORM`s as already supported. It is a control-flow
   model change (paragraphs become callable units), not dispatch-table work,
   and it is bench-gated: RELIAN-BENCH has no program exercising an out-of-line
   `PERFORM`, so a sealed corpus addition must land before any implementation
   (R7).
1. ~~`EXIT`, `CONTINUE`, `GOBACK` — dispatch entries~~ **Done for
   `CONTINUE`, `GOBACK` and the qualified `EXIT PROGRAM` (WP-1.5.5,
   bench-gated by RELIAN-BENCH v1.2's P07_exitflow). Paragraph `EXIT`
   remains, and folds into item 0 above: it is inseparable from
   performed-paragraph support (Bugbot finding, PR #10).**
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

## Before / after — PR #16 correction (same input trees)

Coverage is statements-supported / statements-recovered, token-scan, graded
PLAUSIBLE. Manifest hashes match every prior run, so the input code is
byte-identical and the delta is the registration change alone.

This table runs the wrong way on purpose. Every other re-run in this file
raised coverage by adding capability; this one lowers it by withdrawing a claim
that was never true.

| Run | WP-1.5.4/1.5.5 (overstated) | PR #16 (corrected) | Statements withdrawn |
|---|---|---|---|
| `aws_carddemo` | 0.8511 (8,288/9,738) | **0.7248** (7,058/9,738) | −1,230 |
| `omp_cobol_course` | 0.6945 (532/766) | **0.5287** (405/766) | −127 |
| `gnucobol` | 0.5968 (262/439) | **0.5444** (239/439) | −23 |
| `examples_cobol` | 0.7545 (83/110) | **0.5818** (64/110) | −19 |
| `bench_corpus` | 1.0000 (173/173) | **1.0000** (173/173) | 0 |

−1,380 across the three third-party corpora, exactly the demand table's new
`PERFORM` row. `bench_corpus` is unchanged at 1.0000 because every `PERFORM` in
the committed corpus is the inline `VARYING` form, which is genuinely
supported — and the corpus still cross-validates against BER 1.0000 on the
public split. The sealed benchmark never depended on the false claim.

### For the earlier before/after (WP-1.5.4 + WP-1.5.5)

That re-run added `CONTINUE` (271), `GOBACK` (57) and one same-line
`EXIT PROGRAM` — +329 real statements, from 0.8209 / 0.6606 / 0.5763. Those
gains are still real and are included in the corrected figures above; the
`PERFORM` correction is subtracted on top of them.

## Reproducing

```bash
python -m src.assessment.cli bench/corpus     --out docs/dryruns/bench_corpus
python -m src.assessment.cli examples/cobol   --out docs/dryruns/examples_cobol
git clone --depth 1 https://github.com/aws-samples/aws-mainframe-modernization-carddemo.git /tmp/carddemo
python -m src.assessment.cli /tmp/carddemo    --out docs/dryruns/aws_carddemo
```

`assessment.sha256` in each directory is the hash of that run's `assessment.json`.
Re-running on the same input reproduces it exactly.
