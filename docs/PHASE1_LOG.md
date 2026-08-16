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
pytest -q                  → 4 failed, 232 passed, 7 skipped
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
| 1 | `pytest tests/assessment` green; existing suite unchanged | **Met.** 177 passed, 7 skipped. Whole suite: 4 failed / 232 passed — the same 4 pre-existing failures as the WP-0 baseline, no existing test edited. |
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

---

## 2026-08-16 · Phase 1 close-out re-verification

Run at the end of the session, on the final tree:

```
python3 -m pytest -q -o addopts=""
→ 4 failed, 232 passed, 7 skipped

python3 -m pytest -q -o addopts="" tests/assessment
→ 177 passed, 7 skipped

python3 scripts/bench_public.py --out <scratch>/c1_final
→ ber_overall: 1.0   build_rate: 1.0   valid: true
   all 5 generated-Java SHA-256 identical to the WP-0 baseline
   diff -r against the pre-refactor tree: empty
```

The WP-1.2 behavior-preservation gate still holds at the end of Phase 1, not
only at the moment of the refactor.

---

## 2026-08-16 · PR #3 review round 1

### CI: the `bench` gate is green and measures nothing — **escalation item #5**
Checking CI on PR #3 surfaced this in the job log of the PR's own run
([job 95152859738](https://github.com/khaaliswooden-max/relian/actions/runs/31942183656/job/95152859738)):

```
ledger verified: 8756173e6fc136f6
Cloning into '/tmp/bench-private'...
no candidate output at candidates/current — skipping score
```

`.github/workflows/bench.yml` looks for `candidates/current` "populated by the
pipeline under test". Nothing populates it — no step runs the transpiler and the
directory is not committed — so the early exit is taken on every run,
`run_candidate(..., split='heldout')` is never called, and `ber_heldout_min` is
never compared against anything. The job has exited 0 on every commit since it
was written.

This predates this branch. It matters here because **Phase 1 acceptance criterion
#2 cannot be satisfied by a green `bench` check** — the check does not mean what
its name implies. Corrected on the PR rather than left standing.

A gate that exits 0 without measuring is the same defect class as
`validation_score = 95.0`: a target reported as a result. Fix raised separately
as **PR #4** (operator chose a separate PR over pushing onto #3), which
generates the candidate from the transpiler under test and turns the thresholds
into real failures — missing candidate, INVALID run, null BER, null build_rate,
or a metric below its ledger threshold each exit 1. Gate mechanics verified
locally against the **public** split (BER 1.0, build_rate 1.0 → THRESHOLD MET)
and against a missing candidate directory (exit 1). No held-out vector read.

### Review finding: nesting was tracked with a counter, not a stack
Cursor Bugbot flagged `_OPEN_SCOPE` / `_CLOSE_SCOPE` asymmetry in
`complexity.py`. It was correct. `_CLOSE_SCOPE` matched `END-PERFORM`,
`END-SEARCH`, `END-READ`, `END-CALL`, `END-STRING` and `END-UNSTRING` while
`_OPEN_SCOPE` matched only `IF` and `EVALUATE`, so those terminators decremented
a depth they had never incremented.

Reproduced before fixing: `IF` > inline `PERFORM` > `IF` reported
`max_nesting_depth` **1** where the true depth is **2** — the `END-PERFORM`
cancelled the enclosing `IF`, and everything nested after it was undercounted.

Two consequences beyond the number. `max_nesting_depth` feeds the
`MED: max_nesting_depth>4` rule, so tiers could be wrong. And the module
docstring — reproduced verbatim into appendix B of the customer report — said
inline `PERFORM` raised depth when it never did, so the report stated a formula
the code did not implement (R9).

Now a scope stack: `IF`, `EVALUATE`, `SEARCH` and *inline* `PERFORM` push; each
`END-…` pops only its own opener; an unmatched terminator is inert; a period
closes everything open. `PERFORM <paragraph>` opens nothing — calling three
paragraphs is not three levels of nesting. Eight regression tests added.

```
pytest tests/assessment -q → 185 passed, 7 skipped
pytest -q                  → 4 failed, 240 passed, 7 skipped
```

Dry-run artifacts regenerated at the commit containing the fix.
`max_nesting_depth` changed for **30 of 87** programs; largest corrections
`CBL_OC_DUMP.cob` 5→6 and `COTRTLIC.cbl` 4→5. **No risk tier moved**, at program
or portfolio level — every affected program was already tiered by an
earlier-firing rule, so the nesting rule was not the deciding one anywhere in
these corpora.

---

## 2026-08-16 · Acceptance criterion #2 — RESOLVED, measured for the first time

PR #4 merged the gate fix into its own branch and CI ran it. This is the first
time in the history of this repository that the `bench` job has scored the
held-out split. From
[job 95154014247](https://github.com/khaaliswooden-max/relian/actions/runs/31942680176/job/95154014247)
(head `0ae61b6`):

```
P01_payroll: emitted 3424 bytes -> candidates/current/P01_payroll/Payroll01.java
P02_interest: emitted 2759 bytes -> candidates/current/P02_interest/Interest01.java
P03_eligibility: emitted 3150 bytes -> candidates/current/P03_eligibility/Eligible01.java
P04_taxtable: emitted 5150 bytes -> candidates/current/P04_taxtable/Taxtbl01.java
P05_validate: emitted 3219 bytes -> candidates/current/P05_validate/Validat01.java

rep = run_candidate('current', out, mains, split='heldout')
{
  "ber": 1.0,
  "build_rate": 1.0,
  "valid": true,
  "reason": null
}
THRESHOLD MET: BER 1.0 >= 0.95, build_rate 1.0 >= 1.0
```

**BER 1.0000 on the HELD-OUT split. build_rate 1.00. valid.**

The `no candidate output at candidates/current — skipping score` line is gone,
and the scoring step took ~56 s where the skipping version took under one, which
is what running the vectors actually costs.

Corroborating detail: the five emitted byte counts are identical to the local
generation run, which was in turn byte-identical to
`bench/candidates/C1_rulebased/**`. So the artifact CI scored is the same
artifact verified locally.

### What this does and does not certify

It certifies the transpiler **on `main`** (`9d364d8`), because PR #4 branches
from main and does not contain the WP-1.2 refactor.

It certifies PR #3's transpiler too, by deduction rather than by a separate run:
the WP-1.2 gate is that all five corpus programs regenerate **byte-identical**
output, asserted locally and by
`test_supported.py::test_refactor_is_byte_identical_to_committed_candidate`.
Identical Java handed to the same scorer against the same vectors produces the
same score. Once PR #4 is on main and PR #3 picks it up, that becomes a direct
measurement rather than a deduction, and it should be re-read then.

**Acceptance criterion #2 is met**, and escalation item #5 is resolved pending
the merge of PR #4.

---

## 2026-08-16 · Bench gate merged — operator decision recorded

**PR #4 merged to main as `c4e5047`** (squash). The `bench` job now generates
`candidates/current` from the transpiler at the commit under test and scores it
against the held-out split, and every threshold is a hard failure.

### The held-out numbers, in full

First real measurement, [job 95154014247](https://github.com/khaaliswooden-max/relian/actions/runs/31942680176/job/95154014247)
on `0ae61b6`:

```
P01_payroll:     emitted 3424 bytes -> candidates/current/P01_payroll/Payroll01.java
P02_interest:    emitted 2759 bytes -> candidates/current/P02_interest/Interest01.java
P03_eligibility: emitted 3150 bytes -> candidates/current/P03_eligibility/Eligible01.java
P04_taxtable:    emitted 5150 bytes -> candidates/current/P04_taxtable/Taxtbl01.java
P05_validate:    emitted 3219 bytes -> candidates/current/P05_validate/Validat01.java

rep = run_candidate('current', out, mains, split='heldout')
{
  "ber": 1.0,
  "build_rate": 1.0,
  "valid": true,
  "reason": null
}
THRESHOLD MET: BER 1.0 >= 0.95, build_rate 1.0 >= 1.0
```

| Metric | Ledger threshold | Measured (held-out) | Verdict |
|---|---|---|---|
| `ber_heldout` | ≥ 0.95 | **1.0000** | PASS |
| `build_rate` | ≥ 1.00 | **1.00** | PASS |
| `valid` | true | **true** | PASS (no anti-gaming violation) |
| `branch_coverage` | ≥ 0.80 | **not gated** | see below |

Evidence it measured rather than skipped: the `skipping score` line is gone; the
scoring step took ~56 s where the skipping version took under one; and the five
emitted byte counts match the local generation run, which was byte-identical to
`bench/candidates/C1_rulebased/**`.

`branch_coverage_min` (0.80) is in the ledger and is **deliberately not wired
into the gate**. JaCoCo measurement exists in `harness/coverage.py` and the
runner reports it, but gating on it is a separate decision with its own failure
modes. Recorded as an open item rather than quietly omitted.

### Operator decision — gate trigger scope

**Khaalis, 2026-08-16: the gate stays on every `push` and `pull_request`, under
R10.** The consequence is accepted deliberately: if held-out BER ever falls
below 0.95, every PR in the repository goes red until it is fixed, including
PRs that do not touch the transpiler. That is the intended behaviour of a merge
gate on a protected branch — a benchmark that only runs when someone remembers
to run it is not a gate. Recorded here so the first time it blocks unrelated
work, the blocking is understood as designed rather than diagnosed as a fault.

### Consequence for acceptance criterion #2

`main` now carries the working gate, and it has been merged into
`claude/read-and-run-f4626d`. This branch's own `bench` run will therefore
generate the candidate from **the WP-1.2 refactored transpiler** and score it
against the held-out split directly — converting criterion #2 from a deduction
about byte-identical output into a measurement of this branch's own code. The
result is recorded in the next entry.

## 2026-08-16 · Criterion #2 — direct measurement of THIS branch

The previous entry recorded the held-out score of `main`'s transpiler and
covered this branch by deduction. That deduction is now replaced by a
measurement of this branch's own code.

`bench` on `claude/read-and-run-f4626d` at head `c7c7018`
([job 95156214766](https://github.com/khaaliswooden-max/relian/actions/runs/31943595041/job/95156214766)),
which generates the candidate from **the WP-1.2 refactored transpiler in this
branch** and scores it against the held-out split:

```
P01_payroll:     emitted 3424 bytes -> candidates/current/P01_payroll/Payroll01.java
P02_interest:    emitted 2759 bytes -> candidates/current/P02_interest/Interest01.java
P03_eligibility: emitted 3150 bytes -> candidates/current/P03_eligibility/Eligible01.java
P04_taxtable:    emitted 5150 bytes -> candidates/current/P04_taxtable/Taxtbl01.java
P05_validate:    emitted 3219 bytes -> candidates/current/P05_validate/Validat01.java

rep = run_candidate('current', out, mains, split='heldout')
{
  "ber": 1.0,
  "build_rate": 1.0,
  "valid": true,
  "reason": null
}
THRESHOLD MET: BER 1.0 >= 0.95, build_rate 1.0 >= 1.0
```

| Metric | Ledger threshold | main (`0ae61b6`) | this branch (`c7c7018`) |
|---|---|---|---|
| `ber_heldout` | ≥ 0.95 | 1.0000 | **1.0000** |
| `build_rate` | ≥ 1.00 | 1.00 | **1.00** |
| `valid` | true | true | **true** |

**The five emitted byte counts are identical across the two runs** — 3424 /
2759 / 3150 / 5150 / 3219 — which is the WP-1.2 behavior-preservation gate
confirmed inside CI, on a different machine, from a fresh checkout, rather than
only on the development container. The refactored dispatch table emits exactly
what the `if/elif` chain emitted, and the held-out vectors agree.

Scoring took 77 s (11:10:32 → 11:11:49).

**Acceptance criterion #2 is now evidenced by direct measurement of this
branch's transpiler, not by inference.** All seven Phase 1 acceptance criteria
are met and evidenced.

Review status at this head: the single Bugbot thread (nesting depth) is
**resolved**; the re-review on `c7c7018` completed with no new findings.

---

## 2026-08-16 · Phase 1.5 pre-work · Held-out branch coverage recorded, gate-teeth proof, Taxtbl01 dead-branch analysis

### Held-out branch coverage — the numbers, with provenance

`branch_coverage_min` went live in the gate with PR #5 (`254c5c7`). The first
held-out coverage measurements, identical across two independent CI runs —
the PR run ([job 95158641704](https://github.com/khaaliswooden-max/relian/actions/runs/31944621421/job/95158641704)
on `254c5c7`) and the post-merge main run
([job 95159624559](https://github.com/khaaliswooden-max/relian/actions/runs/31945038971/job/95159624559)
on `6e989e0`):

```
branch_coverage: 0.8824   coverage_tool: jacoco-0.8.12
THRESHOLD MET: BER 1.0 >= 0.95, build_rate 1.0 >= 1.0, branch_coverage 0.8824 >= 0.8
```

| Program | held-out (CI) | public (local, this session) |
|---|---|---|
| P01_payroll | 1.0 (8/8) | 1.0 (8/8) |
| P02_interest | 1.0 (2/2) | 1.0 (2/2) |
| P03_eligibility | 0.9 (18/20) | 0.8 (16/20) |
| P04_taxtable | 0.6875 (11/16) | 0.6875 (11/16) |
| P05_validate | 0.9545 (21/22) | 0.8182 (18/22) |
| **aggregate** | **0.8824 (60/68)** | 0.8088 (55/68) |

Margin: the threshold needs ≥ 55 of 68 branches (0.80 × 68 = 54.4). Held-out
covers 60 — six fewer covered branches before the gate goes red. The public
split sits exactly at the 55-branch minimum, as PR #5's margin warning said.
P04_taxtable is 11/16 on **both** splits — see the analysis below.

### WP-1.5.0b follow-up: the coverage gate's red path, proven in CI (PR #6)

Verification of PRs #4/#5 found that all 26 bench runs in CI history were
green: no threshold's failure path had ever fired in CI, and PR #5 (one
commit) had no planted-red run. PR #6 added the missing proof. The gate was
not touched; the candidate-generation step temporarily appended a
never-called method (8 dead branches) to each generated class, so BER and
build_rate stayed 1.0 and only the *measured* coverage moved:

- planted `a956734` → [run 31947301751](https://github.com/khaaliswooden-max/relian/actions/runs/31947301751)
  **RED**: `THRESHOLD FAILED: branch_coverage 0.5556 < 0.8` (60/108 held-out),
  with `ber 1.0`, `build_rate 1.0` — the coverage threshold alone turned the
  gate red, which is exactly the path under proof.
- revert `e5bed57` → [run 31947314457](https://github.com/khaaliswooden-max/relian/actions/runs/31947314457)
  **GREEN**: back to 0.8824 (60/68), `THRESHOLD MET`.

Both commits are in main's history via a merge commit (net diff zero); the
runs are the deliverable (R10: honest-failure records are audit evidence).

### WP-1.5.0 verification note — contract deviations, recorded not hidden

The WP-1.5.0 contract asked for a machine-readable `bench_summary.json`
workflow artifact (n_vectors, ber, build_rate, branch_cov, git_sha,
ledger_ref) and a literal `n_vectors == 0 → exit 1` check. Neither exists in
the merged gate. What does exist: a missing `candidates/current` fails, a
missing `heldout.jsonl` fails the fetch/score step, an empty vector set
yields `ber = None` and `BER not measured (null)` fails — so "cannot score →
must not pass" holds by construction, but the vector *count* is not printed
anywhere and there is no summary artifact. Recorded here as open follow-up
work rather than silently reinterpreted as done; the planted-red requirement
for PR #4 itself was likewise never executed (its two green commits fixed a
real stub defect; the BER/build red path remains proven only by PR #6's
mechanism applying to the same failure-list code path).

### Taxtbl01 `_tx_search` dead-branch hypothesis — DISMISSED (read-only, JaCoCo line-level)

P04_taxtable measures 11/16 on both splits. Line-level JaCoCo on the public
split (local run, jacococli XML report) puts the five missed branches at
generated-Java lines 13, 19 (×2), 85, 91:

| Line | Code | Verdict |
|---|---|---|
| 85 | `for (; BI <= 5; BI++)` loop-exhaust branch (`_tx_search` emission) | **Not dead.** Reachable only when the SEARCH exhausts: `WS-INC` > `BR-CEIL(5)` = 999999999.00. `WS-INC` is `PIC 9(9)V99`, max 999999999.99, so the AT END path is live for the 99-cent window (999999999.00, 999999999.99]. No vector on either split lands there. |
| 91 | `if (!_found)` — the AT END fallback (`_tx_search` emission) | Same as line 85. The emission is the faithful translation of `SEARCH … AT END`; removing it would fabricate behavior (R2). |
| 13 | `R.dnumU` zero-pad loop | Structurally dead **for this program**: only called with `intd=1` (`WS-IDX PIC 9`) and `toPlainString()` never yields length < 1. Live in programs that pad wider fields. |
| 19 | `R.rtrim` — empty-string entry + trailing-space trim | Live but unexercised: needs an empty or blank-padded filing status; every vector sends `S` or `M`. |

**Conclusion: no transpiler defect — no WP-1.5.6.** The `_tx_search`-emitted
branches are reachable and semantically required; the gap is a property of
the vector sets (nothing exercises SEARCH AT END on P04, on either split),
not of the emission. Caveat stated plainly: line-level identity was measured
on the **public** split only; for held-out, CI reports the matching 11/16
count but no line detail, so "same five branches" is consistent, not
measured.

---

## 2026-08-16 · WP-1.5.1 · PR #3 close-out

PR #3 merged to main as `33430d09` (merge commit). The post-merge `bench` run
on main — [run 31944108333](https://github.com/khaaliswooden-max/relian/actions/runs/31944108333/job/95157439562)
— scored the held-out split green on the merged tree (scoring step 79 s,
completed 11:23:24Z). The rebase step of WP-1.5.1 was unnecessary: main's
fixed gate (`c4e5047`) had already been merged into the branch (`4d07bea`)
before the merge, so the branch's own PR runs #20/#22 scored held-out
directly, as recorded in the criterion #2 entry above.

## 2026-08-16 · WP-1.5.2 · Strict mode ON (R2)

`Transpiler(..., strict=True)` is now the default. An unsupported verb
raises `UnsupportedConstruct` carrying **verb + source line + paragraph**
(paragraph tracking added to `_statements` as bookkeeping — no emission
change). `strict=False` remains as the documented inventory mode: record
every occurrence, emit nothing.

**Finding: the corpus was never hit-free.** P01 and P04 contain a bare
`END-UNSTRING` line that reached `stmt()` with no handler and took the
silent fall-through on every run since WP-1.2 (it was also *recorded* as an
"unsupported hit", which was a false positive — nothing was dropped, the
UNSTRING handler had already consumed the construct and a bare terminator's
correct translation is nothing). Naive strict therefore broke byte-identity
on P01/P04 by refusing valid programs. Resolution: a **bare**
`END-UNSTRING`/`END-SEARCH` (the exact token, no content) is a no-op
terminator — not dropped, not flagged, not raised; anything content-bearing
(`SUBTRACT …`, a stray `AT END …`, `END-UNSTRING <content>`) still raises.
The "no unsupported verbs in the corpus by definition" premise in the work
package was true of verbs but not of boundary tokens; recorded here rather
than papered over.

Callers:
- `orchestrator._transform_to_java` catches `UnsupportedConstruct`, re-runs
  in inventory mode, and surfaces the honest-failure result with the full
  inventory: `unsupported COBOL construct 'SUBTRACT' at source line 9 in
  paragraph MAIN-PARA … inventory: [SUBTRACT (line 9)]` (measured on a
  synthetic program this session). Programs that fail before dispatch
  (e.g. `banking-system.cbl`, parse failure in `__init__`) keep the
  pre-existing generic honest-failure path — unchanged, re-verified.
- Cross-check `_transpile` uses inventory mode explicitly (full-inventory
  comparison is its job); new guarantee **G5** asserts that wherever
  inventory mode records hits, the strict default raises.
- Known narrowing unchanged (C1_SUPPORTED_VERBS_OBSERVED.md §3 case B): an
  unsupported verb NOT in `VERBS` that follows another statement (e.g.
  `GOBACK` after `DISPLAY`) is glued to its predecessor and never reaches
  dispatch, so strict cannot see it. Fixing that changes statement
  segmentation, which changes bytes — deferred exactly as in WP-1.3.

### Gates, measured on this tree
```
python3 -m pytest -q -o addopts=""  → 4 failed, 242 passed, 12 skipped
  (the same 4 pre-existing stale tests as the WP-0 baseline — WP-1.5.3 next)
python3 scripts/bench_public.py     → ber_overall 1.0, build_rate 1.0, valid
  all 5 generated-Java SHA-256 IDENTICAL to the WP-0 baseline
  diff -r (java trees) vs pre-strict baseline: empty
local gate replica (public split)   → branch_coverage 0.8088 (55/68), exit 0
```
Public split byte-identical, as the work package required. Held-out: the
gate on this PR's CI run is the measurement; see the PR.

### PR #8 review round — phantom paragraph names (Bugbot, confirmed)

Bugbot flagged that the new paragraph tracker treated any lone `NAME.` line
as a paragraph, so a standalone `END-IF.` / `GOBACK.` / `ELSE.` would
pollute the paragraph reported by `UnsupportedConstruct`. Confirmed real —
the assessment scanner solved this exact phantom-paragraph problem in
WP-1.4 (defect #3) and the transpiler tracker did not mirror it. Fixed: the
NAME assignment is now guarded (`VERBS`, `END-*`, and the reserved lone-line
keywords are never paragraph names); the label-skip behavior itself is
untouched (WP-1.2 byte-identity). Regression test added; suite 247 passed;
all 5 public-split SHA-256 still identical to baseline.

---

## 2026-08-16 · WP-1.5.3 · The four stale tests — remediation debt cleared

No production code changed. Each test asserted a pre-remediation fabricated
value; each now asserts the honest behavior the code has had since
remediation. Measured before editing (each old assertion re-run and observed
failing for exactly the documented reason):

| Test | Was asserting | Now asserts |
|---|---|---|
| `test_core.py::TestMigrationResult::test_default_values` | `semantic_score == 0.0`, `risk_score == 0.0` (fabricated defaults) | both `None` — unmeasured is `None`, never a number (R1) |
| `test_core.py::TestMigrationOrchestrator::test_migrate_with_cobol_parser` | `COMPLETED` + `semantic_score > 0` for a program outside the C1 subset | `FAILED`, an error containing "refusing to emit a placeholder", `semantic_score is None` (R2) |
| `test_generator.py::test_generate_tests` | 3 fabricated test cases with invented `expected_output 15` from a nonexistent executable | `tests == []` — no oracle, no cases (R1) |
| `test_generator.py::test_generate_pytest_file` | fabricated `def test_calc_func_path_0` present in the emitted file | no `def test_` in the emitted file |

```
python3 -m pytest -q -o addopts=""  → 247 passed, 12 skipped, 0 failed
```
First fully green suite in the repository's recorded history: every WP-0
baseline failure is now cleared, and "4 failed" stops being a number every
future session must explain.

---

## 2026-08-16 · WP-1.5.4 · VALUE clause — flagged now, bench next

**Interpretation recorded rather than silently applied.** The work package
says "register VALUE as an unsupported data feature in
`supported_data_features()`" — but that function is a *measurement* (it
probes the transpiler), and the measured truth is `accepted_ignored`: the
declaration parses, the field exists, the clause is discarded. Hard-coding
"unsupported" would be a constant overriding a probe — the exact R1 failure
this repo exists to prevent. What the work package is actually after is
that programs using VALUE stop looking safe. Delivered where the decision
lives:

- **Risk rule (new, HIGH):** `HIGH: VALUE clause present but discarded by
  the transpiler (initialization semantics lost)`. Fires on any VALUE-clause
  hit whose *probed* status is not "supported" — so the rule retires itself
  the day VALUE is implemented and probes supported, with no constant to
  flip. Deliberately scoped to VALUE, not to every `accepted_ignored`
  feature: COMP-3 is accepted_ignored too, and held-out BER 1.0000 on a
  corpus that uses COMP-3 throughout is measured evidence that that discard
  preserves behavior; the VALUE discard demonstrably does not (C1 zero-inits
  every field).
- The analyzer already inventoried VALUE per program (WP-1.4) and the report
  already prints the three-state data-feature table; the missing link was
  risk.

Measured end-to-end this session: a program using VALUE (the VALINIT01
draft) assessed via the CLI tiers **HIGH** with the new rule; the bench
corpus re-assessed unchanged (all five LOW — no non-88 VALUE in the corpus).
Suite: 250 passed (3 new rule tests, incl. the self-retirement case).

**Draft oracle candidate:** `bench/candidates/drafts/VALINIT01/program.cbl`
— VALUE on numeric/alphanumeric/COMP-3, group-level VALUE, 88-levels with
single and multiple values; every output line depends on an initial value.
Compiled and executed with GnuCOBOL 3.1.2 this session (`cobc -x`; sample
runs in the drafts README are real output). **Sealing/signing is
Khaalis-only; implementation of VALUE in `parse_working_storage`/emit is
GATED on the sealed bench commit predating the grammar merge (R7). Not
started.**

## 2026-08-16 · WP-1.5.5 · EXIT / CONTINUE / GOBACK — draft only, implementation gated

**Draft oracle candidate:** `bench/candidates/drafts/EXITFLW01/program.cbl`
(rev 2) — CONTINUE (bare / in IF / in EVALUATE), EXIT PROGRAM in a main
program, GOBACK with and without `MOVE 0 TO RETURN-CODE`. Compiled and
executed with GnuCOBOL 3.1.2 this session.

**Bugbot finding on PR #10, confirmed, and it narrows the work package.**
Rev 1 drove its loop with out-of-line `PERFORM <para>` and tested paragraph
EXIT. C1's `_tx_perform` supports only the inline
`PERFORM VARYING … END-PERFORM` form — an out-of-line PERFORM crashes the
handler — so rev 1 could never gate the three handlers alone. Consequence,
recorded rather than papered over: **paragraph EXIT (the dominant form
behind the 368-occurrence EXIT count) is inseparable from
performed-paragraph support and is NOT dispatch-table-only work.** It joins
the deferred list; the truly cheap constructs are CONTINUE, GOBACK, and
EXIT PROGRAM-in-main.

Three more measured findings from drafting, all in the drafts README:
- vectors must not set a nonzero RETURN-CODE (`runner._run_java` treats a
  nonzero exit as a failed run — needs a harness decision first);
- GnuCOBOL quirk: a WHEN branch whose only statement is EXIT PROGRAM
  compiles to an empty branch and chains into the next WHEN (`WHEN OTHER`
  also runs). Vectors must never encode that oracle quirk;
- counters must be wider than the input domain (`WS-I PIC 9(4)` wraps at
  9999+1 and loops forever — rev 2 hung on input `9999,P` until widened).

**Everything after the draft is gated:** Khaalis seals → handlers land in
`SUPPORTED_STATEMENTS` → held-out green → analyzer picks the verbs up
automatically (it reads the dispatch table) → WP-1.9 dry runs re-run and the
before/after coverage numbers recorded side by side. None of that is
started; the 696-occurrence demand signal (EXIT 368+CONTINUE 271 across the
three real-world corpora, plus GOBACK) is unchanged from the WP-1.9 entry.

### Deferred, with the written reason (work-package requirement)

- **`EXEC` (306 occurrences):** CICS/SQL are external-interface semantics —
  transaction context, cursors, commit scopes — not a dispatch-table job. A
  handler that stubbed them would be a placeholder wearing a verb's name
  (R2/R5). Phase 4 candidate after buyer signal.
- **`WRITE` (224):** real file-section/record semantics; needs FD modelling
  (currently `unsupported` in the data-feature probe) and its own oracle
  program with file-based vectors — a harness extension, since vectors today
  are stdin/stdout.
- **`GO TO` (186):** unstructured control flow; the Java emission model has
  no jump primitive — needs a control-flow restructuring design, not a
  handler.
- **Paragraph `EXIT` / out-of-line `PERFORM <para>` (moved here by the
  PR #10 Bugbot finding):** paragraph EXIT is only meaningful as the return
  point of a performed paragraph, and C1 has no performed-paragraph
  emission (paragraphs would need to become methods). That is an emission
  architecture change, not a `SUPPORTED_STATEMENTS` entry.
- **`CALL` (183):** cross-program linkage (LINKAGE SECTION, BY
  REFERENCE/CONTENT) and multi-binary oracles; also the vehicle for real
  EXIT PROGRAM vectors. Rank against WRITE/GO TO by demand after the 1.5.5
  re-run.

---

## 2026-08-16 · WP-1.5.0c · bench_summary.json + the explicit n_vectors check

Completes the WP-1.5.0 contract literally (the deviations were recorded in
the verification entry above rather than reinterpreted as done). Workflow
change only; no harness or transpiler byte changed.

- The scoring step now writes **`bench_summary.json`** on every path —
  including the refusal paths, since a red run's evidence of what was NOT
  measured is the point of the artifact. Fields: `n_vectors`, `ber`,
  `build_rate`, `branch_coverage`, `coverage_tool`, `git_sha`,
  `ledger_ref` (tag + signed manifest sha256), `failures`. Printed in the
  job log and uploaded via `actions/upload-artifact` with `if: always()`.
- `n_vectors` = vectors the scorer actually compared (`vectors_total` per
  program); a program that fails to build contributes 0, so a gate that
  compiled nothing cannot report having scored anything.
- The explicit contract check: `n_vectors == 0` → exit 1 with
  **"held-out not scored — refusing to certify"**. It fires even when other
  metrics would already fail, so the certification refusal is named, not
  implied.

Verified locally against the PUBLIC split by extracting the exact heredoc
from the workflow (R3 — held-out stays CI-only):

| Path | Result |
|---|---|
| real candidate | exit 0, summary `n_vectors: 60` (5×12 public), no failures |
| missing `candidates/current` | exit 1, summary written with the failure |
| empty candidate dirs | exit 1, first failure line `held-out not scored — refusing to certify (n_vectors == 0)` |

The ledger's own `vector_counts` records 60 held-out vectors per program, so
the green CI run on this PR must show `n_vectors: 300` — recorded on the PR.
