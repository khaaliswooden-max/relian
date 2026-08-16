# Phase 1 log (append-only)

Every entry records what was actually executed and what it actually returned.
No entry may contain a number that was not produced by a command in that entry.

---

## 2026-08-16 · WP-0 · Baseline

- **HEAD at start:** `9d364d8` (`fix(ci): workflow references v1.1 ledger (v1.0 archived)`)
- **Branch:** `claude/read-and-run-f4626d`
- **Working tree:** clean at session start
- **Python:** 3.11.15 · **javac/java:** present · **gnucobol (`cobc`):** NOT present in this container
- **Deps installed into the container to run the suite at all** (not committed;
  they are already declared in `pyproject.toml`): `pytest`,
  `antlr4-python3-runtime`, `numpy`, `openai`, `anthropic`, `neo4j`, `pydantic`.
  Before installing, 3 test modules failed at *collection* on missing imports.

### Test suite
```
python3 -m pytest -q -o addopts=""
→ 4 failed, 55 passed, 2 warnings in 1.87s
```
Failing at baseline (pre-existing, not caused by this session):

| Test | Assertion | Reading |
|---|---|---|
| `tests/test_core.py::TestMigrationResult::test_default_values` | `None != 0.0` | The test still expects the **old fabricated default**. Measured-only remediation returns `None`. The test is stale, the code is right (R1). |
| `tests/test_core.py::TestMigrationOrchestrator::test_migrate_with_cobol_parser` | `FAILED != COMPLETED` | Orchestrator correctly refuses to emit a placeholder for a program C1 cannot transpile. Honest failure (R2); the test predates it. |
| `tests/analysis/test_generator.py::test_generate_tests` | `0 != 3` | Generator no longer fabricates test cases it cannot derive. |
| `tests/analysis/test_generator.py::test_generate_pytest_file` | expected body absent | Same cause. |

**Not fixed in WP-0.** Changing these tests changes what the repo asserts about
its own honesty guarantees; that is an operator call, and none of them touch
Phase 1 scope. Recorded here so "4 failed" is not mistaken for a regression later.

### Local pipeline run
```
python3 examples/migrate.py --source examples/cobol/banking-system.cbl --target java --no-blockchain
→ Status: FAILED
→ "deterministic transform does not support this program's constructs
   ('NoneType' object has no attribute 'groups'); refusing to emit a placeholder
   in place of a migration"
→ warning: llm_confidence=None (informational only)
```
This is the designed behavior, not a defect: `banking-system.cbl` uses
constructs outside the C1 subset and the pipeline refuses to fabricate output.
ANTLR also reported `line 3:7 extraneous input 'AUTHOR'` during the parse leg.

### Bench leg — PUBLIC split only (R3)
`bench/harness/runner.py`'s `__main__` defaults to the **held-out** split, which
must never run here. `bench/` is read-only (CLAUDE.md rule 4), so the public leg
runs through a new driver outside it: `scripts/bench_public.py`. It regenerates
the C1 candidate from `transpiler/c1_rulebased.py` into a scratch directory and
scores it against `bench/corpus/*/vectors/public.jsonl`.

```
python3 scripts/bench_public.py --out <scratch>/c1_before
→ ber_overall: 1.0   build_rate: 1.0   valid: true
```
Generated-Java SHA-256, the WP-1.2 before/after gate:

| Program | sha256 |
|---|---|
| P01_payroll | `860562d24b0889c60785f78440299478dd8a79f10d7ad8cf157188e2320edaa7` |
| P02_interest | `f1f706ff2921b6aa13be91238185938fcce6fc58e052043e303ad34d3c663394` |
| P03_eligibility | `6257e89acf12877b766b56bd9f0281784ee57f05a98a2c3cb34afe1292506709` |
| P04_taxtable | `318d41bebe582d369703a1dff5a0ad4ba2151d68da65e210e07b2aed6dbca0fc` |
| P05_validate | `2b7536a1cbe96f01d4e9ba937d062baa88dfe8a7c7c631f511d8479ce27785e0` |

Branch coverage: **not measured** in this leg — JaCoCo jars are present but the
public-split scores above were taken without asserting a coverage number here.
Reported as absent rather than estimated (R1).

### WP-0.3 observation
`docs/C1_SUPPORTED_VERBS_OBSERVED.md` written. Headline finding: there is **no
`else` branch in `Transpiler.stmt()`** and no `unsupported()` function anywhere.
Honest failure exists only at *program* granularity, in the callers, and only
for two of the four observed failure shapes. An unsupported verb that begins a
statement is **silently discarded**; one that follows a statement is absorbed
into its predecessor and surfaces as a misattributed error. Full mechanism, with
a reproducible probe transcript, in that document.

**WP-0 acceptance: met.** CLAUDE.md appended; baseline recorded above;
`C1_SUPPORTED_VERBS_OBSERVED.md` names the failure mechanism precisely.

---

## 2026-08-16 · WP-1.1 · Intake

`src/assessment/models.py`, `src/assessment/intake.py`, `tests/assessment/test_intake.py`.
```
pytest tests/assessment/test_intake.py -q → 11 passed
```
Deviation recorded: `line_ending` has a fourth value, `NONE`, for files with no
line terminator. The work package enumerated three. A file with no line break
has no line ending, and labelling it `LF` would be an invented value (R1).

## 2026-08-16 · WP-1.2 · Dispatch-table refactor (the one transpiler touch)

`transpiler/c1_rulebased.py`: the `if/elif` chain in `Transpiler.stmt()` is now
`SUPPORTED_STATEMENTS: Dict[str, Callable]`, each handler being the former
branch body verbatim. Unknown verbs go through the new
`Transpiler.unsupported(verb, line_no)`, which records the occurrence and — by
default — emits nothing, exactly as the old fall-through did.

Two supporting changes, both required for `unsupported()` to report a real line
number and both verified not to change emitted bytes:
- comment lines become empty lines instead of being dropped, so list index k in
  the stripped body is always source line k+1 (every downstream consumer already
  skips blank lines);
- `_statements()` records the source line of each statement in `self.stmt_lines`.

### Behavior-preservation gate — PASSED
```
python3 scripts/bench_public.py --out <scratch>/c1_after
→ ber_overall: 1.0   build_rate: 1.0   valid: true
```
All five generated-Java SHA-256 are **identical to the WP-0 baseline**, and each
also matches the committed `bench/candidates/C1_rulebased/**` byte for byte:

| Program | sha256 | vs baseline |
|---|---|---|
| P01_payroll | `860562d2…0edaa7` | identical |
| P02_interest | `f1f706ff…c663394` | identical |
| P03_eligibility | `6257e89a…292506709` | identical |
| P04_taxtable | `318d41be…6dbca0fc` | identical |
| P05_validate | `2b7536a1…ce27785e0` | identical |

`diff -r` between the pre- and post-refactor output trees is empty. That gate is
now a permanent test
(`test_supported.py::test_refactor_is_byte_identical_to_committed_candidate`)
rather than a one-off manual check.

### Strict mode — flagged for operator decision
`unsupported()` supports `strict=True`, which raises `UnsupportedConstruct`
instead of dropping the statement. It is **off by default**. Turning it on is
the correct end state under R2 (a silently dropped verb is exactly the failure
R2 forbids), but it changes transpiler behavior and therefore is an operator
decision, not an agent one. **Escalation item #1 for Khaalis.**

### `supported.py`
- `supported_verbs()` → 18 keys read from `SUPPORTED_STATEMENTS`:
  ACCEPT ADD COMPUTE DISPLAY ELSE END-EVALUATE END-IF END-PERFORM EVALUATE IF
  INSPECT MOVE PERFORM SEARCH SET STOP UNSTRING WHEN.
- `boundary_only_tokens()` → AT, END-SEARCH, END-UNSTRING, **SUBTRACT**. These
  are in `VERBS` and look supported; `SUBTRACT` has no handler, so a SUBTRACT
  statement is dropped. Reporting them as supported would be R1 fabrication.
- `supported_data_features()` is **probed**, not listed: each feature is a real
  COBOL program run through `Transpiler.__init__`, and the resulting `fields`
  model is inspected. Measured result:

| status | features |
|---|---|
| supported | PIC 9, PIC X, PIC S9, PIC 9V9, edited (Z/-/.), 88-level, OCCURS fixed |
| accepted_ignored | USAGE COMP-3, USAGE COMP/BINARY, REDEFINES, VALUE on a data item, OCCURS DEPENDING ON, PIC CR/DB, SIGN IS SEPARATE |
| unsupported | PIC A alphabetic, PIC check-protect (`*`), FILE SECTION (FD) records |

  `accepted_ignored` is a deliberate third state: the declaration parses and the
  field exists, but the clause is discarded, so generated Java cannot depend on
  it. Ignoring a clause is not supporting it.

### Suite after WP-1.2
```
pytest -q → 4 failed, 104 passed
```
Same 4 pre-existing failures as the WP-0 baseline; 49 new assessment tests.

## 2026-08-16 · WP-1.3 · Coverage analyzer — and a contradiction in the ground truth

The work package's ground-truth table describes `src/parsers/cobol.py` as the
"full-grammar parser (ANTLR **Cobol85** grammar)", and WP-1.3 is built on that:
"parse with the full ANTLR Cobol85 tree — this is why coverage can be complete
even where the transpiler is not." The instruction was to verify only if
something contradicts. Something contradicts.

`src/parsers/grammars/Cobol85.g4` is a **reduced** COBOL-85 subset of about 180
rules, not the standard grammar. Measured, not inferred:

| Program | ANTLR syntax errors | Statements recovered |
|---|---|---|
| `bench/corpus/P01_payroll/program.cbl` | 11 | **0** |
| `bench/corpus/P03_eligibility/program.cbl` | 4 | **0** |
| `examples/cobol/banking-system.cbl` | 1 | **0** |

Zero statements, not "some". A DATA DIVISION error resynchronises past the
entire PROCEDURE DIVISION, so the tree that survives contains no statements at
all. Concrete gaps found by probing the grammar directly:
- `usageClause` requires the literal `USAGE` keyword, so `PIC S9(5)V99 COMP-3.`
  — the form the corpus uses — is a syntax error.
- `occursClause` requires `TIMES`.
- `computeStatement` is `COMPUTE IDENTIFIER+ EQUAL arithmeticExpression`; the
  `=` sign is not accepted, so no real `COMPUTE` parses.
- `IF A GREATER THAN B` fails (`GREATER` alone parses; `GREATER THAN` does not).
- There are no rules for `ALTER`, `EXEC`, or `ACCEPT` — and `ACCEPT` is a verb
  C1 *supports*.

**Decision, and why it was not escalated as a blocker.** An analyzer that used
only the tree would return "no data" for every real program, which is a useless
$8K deliverable. So `analyze()` has two methods and always says which one ran:
`antlr_tree` (used only on a zero-error parse, graded **VERIFIED**) and a
documented `token_scan` (graded **PLAUSIBLE**), with the parser errors attached
either way. This is not a "reasonable default" of the kind R1 forbids — a token
scan is a real measurement, just a weaker one, and it is labelled as such
everywhere it appears. **Escalation item #2 for Khaalis:** whether Phase 2
replaces this grammar. Note R7 — a bench commit must predate any grammar merge.

Fixture `ANTLRFIT.cbl` is written to fit the reduced grammar precisely so the
tree path is exercised by the suite rather than being dead code, and both
methods are cross-validated against each other on it (both recover 10
statements).

### Cross-check — the R2 guarantee, and one honest narrowing
The work package asks that every UNSUPPORTED statement invoke `unsupported()`.
That cannot be asserted literally, and the reason is in `C1_SUPPORTED_VERBS_OBSERVED.md`
§3 case B: an unsupported verb *following* another statement is glued onto its
predecessor by `_statements()` and never reaches `stmt()`. Making it reach
`stmt()` would change how source is chopped into statements, which changes
emitted bytes, which the WP-1.2 gate forbids. So the guarantee is asserted in
four checkable forms (G1–G4 in `test_cross_check.py`), the strongest being:
**if the transpiler fails or drops a statement, the analyzer flagged an
unsupported construct in that program first.** A planted-disagreement test
proves the check has teeth.

## 2026-08-16 · WP-1.4 – 1.8 · LOC, complexity, risk, report, CLI

```
pytest tests/assessment -q → 177 passed, 7 skipped
pytest -q                  → 4 failed, 233 passed, 7 skipped
```
Same 4 pre-existing failures as the WP-0 baseline. No existing test changed.

**Four defects found by the new tests, each fixed with a regression test:**
1. `\bIF\b` matched the `IF` inside `END-IF` — a hyphen is not a word character,
   so every scope terminator inflated the decision-point count. All COBOL word
   matching now goes through `coverage.word_re()`.
2. `GO TO A B C DEPENDING ON N` counted `DEPENDING`, `ON` and `N` as jump
   targets.
3. A lone `END-IF.` line was registered as a paragraph, creating phantom
   paragraphs in the complexity table and in reachability. `GOBACK.` and
   `EXIT.` had the opposite problem — real statements read as labels and
   dropped from the inventory.
4. **A determinism bug in the ledger.** ANTLR's ALL(*) prediction cache warms
   across parses within a process, so the `expecting {…}` set in a syntax-error
   message *grows* between the first and second parse of the same file. Those
   strings were in the hashed JSON, so two identical runs produced two different
   `report_hash` values. Found by `test_two_runs_are_byte_identical`. The
   expected-token set is a parser artifact, not a fact about the customer's
   code, so it is elided; position and offending token are kept.

Fixed-format detection was also corrected: the first rule only recognised blank
sequence-number areas, so a file with real sequence numbers in columns 1–6 was
misread as free format.

**DOCX.** Rendered with `python-docx` (chosen over a Node `docx` toolchain so the
CLI needs no JavaScript runtime). A missing dependency is reported, never faked
with an empty file. Measured: DOCX rendering dominates wall time on large
portfolios by orders of magnitude. The 44-program, 329-file CardDemo assessment
completes analysis, JSON and Markdown in **4.61 s**, then sat inside
`python-docx` for over **seven minutes** before the render was killed.
`--no-docx` was added in response; the finding is recorded rather than smoothed
over, and DOCX for a large portfolio should be treated as a known slow path
until someone optimises the table emission.

## 2026-08-16 · WP-1.9 · Dry runs

Full results and the construct ranking: `docs/dryruns/README.md`. Every run was
zero-intervention.

| Run | Programs | Files | Coverage | Grade | Risk | Wall |
|---|---|---|---|---|---|---|
| `bench/corpus` | 5 | 15 | 1.0000 | PLAUSIBLE | LOW | 0.53 s |
| `examples/cobol` | 1 | 1 | 0.7545 | PLAUSIBLE | HIGH | 0.24 s |
| AWS CardDemo (Apache-2.0) | 44 | 329 | 0.8209 | PLAUSIBLE | BLOCKED | 4.61 s |
| OMP COBOL course (CC-BY-4.0) | 30 | 360 | 0.6606 | PLAUSIBLE | BLOCKED | 0.95 s |
| GnuCOBOL (GPL-3.0) | 7 | 406 | 0.5763 | PLAUSIBLE | BLOCKED | 1.73 s |

`programs/` was listed as a dry-run target but holds the Rust Solana program,
not COBOL; the CLI correctly reports zero programs on it, so it is not a dry run.

**Cross-validation worth noting:** the analyzer scores `bench/corpus` at 1.0000
construct coverage, and the transpiler scores the same corpus at BER 1.0000 on
the public split. The two were built independently — one walks source, the other
emits Java. Their agreement is evidence, not tautology.

**Every third-party program was analysed by `token_scan`.** Not one real-world
program parsed cleanly under the bundled grammar. This is the strongest evidence
for escalation item #2.

**Demand signal (2,190 unsupported occurrences across the three real-world
corpora):** `EXIT` 368 · `EXEC` 306 · `CONTINUE` 271 · `WRITE` 224 · `GO TO` 186
· `CALL` 183 — six constructs are 70% of everything blocking migration. The
cheapest third of the backlog (`EXIT`, `CONTINUE`, `GOBACK`, 696 occurrences) is
dispatch-table work with no new semantics.

---

# Phase 1 acceptance

| # | Criterion | Status |
|---|---|---|
| 1 | `pytest tests/assessment` green; existing suite unchanged | **Met.** 177 passed, 7 skipped. Whole suite: 4 failed / 233 passed — the same 4 pre-existing failures as the WP-0 baseline, no existing test edited. |
| 2 | WP-1.2 refactor merged, bench leg identical before/after | **Met locally.** All 5 corpus programs regenerate byte-identical to baseline *and* to the committed candidates; public-split BER 1.0000, build 1.00. CI held-out is not runnable from here (R3) — **operator to confirm on the PR**. |
| 3 | Cross-check green: analyzer ↔ transpiler agree | **Met, with one narrowing stated in the WP-1.3 entry and in the test's docstring.** |
| 4 | Determinism green incl. CRLF | **Met.** Also caught and fixed a real determinism bug in the hashed ledger. |
| 5 | Template lint green and proven to fail on a planted literal | **Met.** Two gates, each with a planted-failure test. |
| 6 | Dry runs on public corpus + ≥2 real-world codebases, zero intervention | **Met.** 3 real-world codebases; outputs in `docs/dryruns/`. |
| 7 | Operator wall-clock for a fresh codebase < 1 day | **Met.** Operator time is one command. Machine time for the largest codebase tried (44 programs, 329 files): 4.61 s. |

## Escalation — operator decisions, not agent decisions

1. **Strict mode for `unsupported()`.** A COBOL verb with no handler is silently
   dropped today. `Transpiler(..., strict=True)` raises instead. It is off by
   default because turning it on changes transpiler behavior, and R2 says the
   silent drop is wrong. Recommendation: turn it on, behind a bench run.
2. **The bundled ANTLR grammar.** It is a reduced subset that parses no
   real-world program in any corpus tried. Phase 2 should decide whether to
   replace it with the full Cobol85 grammar. R7 applies: the bench commit must
   predate any grammar merge.
3. **Four stale tests in the existing suite** assert pre-remediation fabricated
   defaults (`0.0` where the code now correctly returns `None`, `COMPLETED`
   where it now correctly fails). They were left untouched — changing what the
   repo asserts about its own honesty guarantees is an operator call.
4. **`VALUE` clauses are discarded** (589 occurrences in CardDemo alone). C1
   initialises every field to zero or empty regardless. Harmless on the bench
   corpus, potentially not on real code. Recommendation: cover `VALUE` in
   RELIAN-BENCH before quoting any program that uses it.
