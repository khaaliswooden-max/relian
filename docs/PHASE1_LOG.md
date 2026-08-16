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
