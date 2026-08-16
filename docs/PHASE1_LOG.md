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
