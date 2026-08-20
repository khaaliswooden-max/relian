# Phase 2 log (append-only)

Every entry records what was actually executed and what it actually returned.
No entry may contain a number that was not produced by a command in that entry
(R1). Where a figure was reported to this session rather than measured in it,
it is labelled as such and attributed.

---

## 2026-08-20 · WP-2.0.-1 · Environment pin, toolchain guard, CI gate

- **HEAD at start:** `27980da` (`Merge pull request #21 from
  khaaliswooden-max/claude/wp-2-remove-generative-ai-uc4zqm`)
- **Branch:** `claude/phase2-wp-2-0-execution-kn14f5`
- **Scope guard:** nothing under `bench/`, `transpiler/`, `src/ml/` or
  `src/intelligence/` was modified. Verified against `git diff --stat` at
  close-out (§7).
- **Baseline to preserve:** `288 passed, 10 skipped, 0 failed`, reported by the
  operator as measured on WSL Ubuntu 24.04 with Python 3.12.3, GnuCOBOL
  3.1.2.0 and OpenJDK 21.0.11.

---

### 1. Dependency set — `pyproject.toml`

Four distributions were declared as hard runtime requirements with no import
site anywhere in the transform path. Measured before removing anything:

```
grep -rEn "^\s*(import|from)\s+(langchain|transformers|torch|xgboost|pandas|numpy)\b" \
    --include="*.py" src/ transpiler/ demo/ bench/ tests/
→ src/intelligence/migration_intelligence.py:363:            import numpy as np
  src/intelligence/migration_intelligence.py:364:            import xgboost as xgb
  src/ml/risk_scorer.py:6:import numpy as np
  src/ml/risk_scorer.py:9:    import xgboost as xgb
```

Four hits, two distributions. `langchain`, `transformers` and `torch` have
zero. A second, wider sweep for any textual reference (not just imports)
confirmed it:

```
grep -rn "pandas\|pd\." --include="*.py" src/ transpiler/ demo/ bench/ tests/ scripts/
→ (no output)
```

**`pandas` was verified NOT imported and was therefore removed**, on the same
evidence as the other three. Its only surviving mention in the repository is a
`pip install` line in `VERIFY_PROMPT_relian.md`, which is prose, not a
declaration.

| Distribution | Import sites | Disposition |
|---|---|---|
| `langchain>=0.1.0` | 0 | **removed** — the residual flagged in `docs/R6_AUDIT_2026-08.md` §8 |
| `transformers>=4.35.0` | 0 | **removed** |
| `torch>=2.1.0` | 0 | **removed** |
| `pandas>=2.0.0` | 0 | **removed** — verified per the instruction before keeping |
| `xgboost>=2.0.0` | 2 (`src/ml`, `src/intelligence`) | **kept** — pending separate disposition, not this WP's call |
| `numpy>=1.24.0` | 2 (`src/ml`, `src/intelligence`) | **kept** — genuinely imported |

`pytest`, `pytest-cov`, `black`, `pylint` and `mypy` moved from
`[project.dependencies]` to `[project.optional-dependencies].dev`. A
customer-perimeter install of the transform path now pulls neither a test
runner nor three linters. `ipython`, `jupyter` and `pre-commit` were already in
`dev` and were left there — see the open question in §6.

---

### 2. Environment pin — `requirements.lock`

Generated, not hand-written:

```
uv pip compile pyproject.toml --extra dev --generate-hashes \
    --python-version 3.12 --universal --no-annotate \
    --output-file requirements.lock
→ 140 pinned distributions, every one carrying its SHA-256
```

Resolved for CPython 3.12 because that is the interpreter
`.github/workflows/tests.yml` pins; `--universal` retains the environment
markers so one file serves the WSL box and the CI runner.

**Verified installable, not assumed.** A clean 3.12 virtualenv was built and
the lock installed into it under hash enforcement:

```
python3.12 -m venv /tmp/ci-venv
/tmp/ci-venv/bin/pip install -r requirements.lock
→ Successfully installed … (137 packages; 3 of the 140 pins are
  marker-excluded on linux/x86_64: `appnope` (darwin), `colorama` and
  `pywinpty` (win32))
/tmp/ci-venv/bin/pip install -e ".[dev]"
→ resolved nothing new; the lock already satisfies every name
/tmp/ci-venv/bin/pip check
→ No broken requirements found.
```

#### Measured environment

| Item | Measured value |
|---|---|
| Interpreter | `Python 3.12.3 (main, Mar  3 2026, 12:15:18) [GCC 13.3.0]` |
| Platform | `Linux-6.18.5-fc-v20-x86_64-with-glibc2.39` (Ubuntu 24.04.4 LTS) |
| GnuCOBOL | `cobc (GnuCOBOL) 3.1.2.0` |
| Java compiler | `javac 21.0.10` |
| `pip freeze` hash | `c8c37f126d4c231ab9d6d2091147759cd247f3725b211028ddf44304eab5c38f` |
| `requirements.lock` hash | `dc22e6c3d679af86a7730d6ab54fb3c22dbd4be11c0ae396c43953e5e3a09649` |

The `pip freeze` hash is over the **sorted, editable-excluded** freeze, because
`pip freeze` renders the editable install as a `-e git+…@<sha>#egg=relian` line
that embeds the checkout's commit and would change the hash on every commit.
The reproducible recipe is:

```
pip freeze --exclude-editable | sort | sha256sum
→ c8c37f126d4c231ab9d6d2091147759cd247f3725b211028ddf44304eab5c38f
```

**Deviation from the stated baseline, recorded rather than smoothed over:** the
operator's baseline names OpenJDK **21.0.11**; the JDK available in this
container is **21.0.10**. The suite result is identical on both (§7), but the
figure above is what was measured here, not what was reported for WSL.

---

### 3. Toolchain guard — `@needs_toolchain`

`tests/demo/test_demo.py` guarded ten tests on `shutil.which("cobc")` alone.
Every one of them builds **both** sides of the differential comparison, so the
guard was wrong in both directions. Measured, by shadowing the tools off
`PATH` and running `tests/demo/test_demo.py` four ways:

| `cobc` | `javac` | Before (`@needs_cobc`) | After (`@needs_toolchain` / `@needs_javac`) |
|---|---|---|---|
| present | present | 22 passed | **22 passed, 0 skipped, 0 failed** |
| present | **absent** | **12 failed**, 10 passed | **9 passed, 13 skipped, 0 failed** |
| **absent** | present | 12 passed, 10 skipped | **12 passed, 10 skipped, 0 failed** |
| **absent** | **absent** | 3 failed, 9 passed, 10 skipped | **9 passed, 13 skipped, 0 failed** |

Measured by shadowing each tool off `PATH` (a directory of symlinks to
`/usr/bin` with the tool omitted) and running `tests/demo/test_demo.py`; the
"before" column was measured against `git checkout HEAD -- tests/demo/test_demo.py`
with `tests/conftest.py` and `tests/toolchain.py` removed, not inferred.

Row 2 against row 4 is the pathology the work package describes, now with
numbers on it: **one** missing tool produced twelve failures, **both** missing
produced three. The more incomplete environment gave the greener suite.

**The work package's figure of twelve is exactly right** — `12 failed, 10
passed` with `javac` off `PATH` and GnuCOBOL present. It decomposes as nine of
the ten `@needs_cobc`-guarded tests, plus three that carried no guard at all:
`test_no_metric_is_invented_without_the_oracle`,
`test_offline_run_is_not_an_execution_outage` and `test_offline_run_exits_zero`.
Those three monkeypatch `oracle.detect` to report no GnuCOBOL, so they were
never `cobc`-guarded — but the pipeline compiles the Java side before it
discovers there is nothing to compare against, so without a JDK all three
return `BUILD_FAILED`/`rc=1` and fail on a verdict assertion.

Thirteen tests are guarded now, against twelve that failed, and the extra one
is worth naming. `test_execution_outage_exits_nonzero` **passed** without
`javac` — but for the wrong reason: it asserts the CLI exits `1` on a total
execution outage, and a run that never compiled exits `1` too. It was reading
a build failure as the outage it meant to detect. It was already
`@needs_cobc`-guarded, so `@needs_toolchain` preserves its intent and removes
the false pass.

The three unguarded tests got a **narrower** guard, `@needs_javac`, rather than
`@needs_toolchain`. Gating them on the full toolchain would skip the
offline-mode tests precisely on the machines where offline mode is the real
configuration, which would delete the coverage the tests exist to provide. Row
3 is the check that this works: twelve tests still run and pass with no
GnuCOBOL at all.

Version recording lives in `tests/conftest.py`: a session-scoped autouse
fixture writes the interpreter, platform and both tool versions into the JUnit
XML as `testsuite` properties, and a `pytest_report_header` hook prints them at
the top of every run. Absent tools record the string `absent`, never a
plausible-looking version (R1). Observed in the CI gate's own output:

```
--- measured environment ---
  python_version: 3.12.3
  platform: Linux-6.18.5-fc-v20-x86_64-with-glibc2.39
  cobc_version: cobc (GnuCOBOL) 3.1.2.0
  javac_version: javac 21.0.10
  toolchain_complete: yes
```

Skipping is only honest because the skip **count** is pinned in CI (§5).
Neither control works alone: the guard without the count is green-by-skip, the
count without the guard is the twelve spurious failures in row 2 above.

---

### 4. The ten skips — enumerated and classified

All ten are in `tests/assessment/test_cross_check.py`. Measured per fixture:

```
ANTLRFIT.cbl   stmts=10   cov=0.7     inventory=[PERFORM, CALL, GO]              transpiler: clean
BROKEN.cbl     stmts=None cov=None    error='no statements recovered by either method'  transpiler: IndexError
COPYUSER.cbl   stmts=5    cov=0.6     inventory=[CALL, CALL]                     transpiler: AttributeError
FULLSUP.cbl    stmts=18   cov=1.0     inventory=[]                               transpiler: clean
HEAVY.cbl      stmts=14   cov=0.2143  inventory=[OPEN, READ, EXEC, …, EXIT]      transpiler: clean
PARTIAL.cbl    stmts=6    cov=0.5     inventory=[SUBTRACT, CALL, GO]             transpiler: clean
```

| # | Test | Fixture | Stated reason | Classification |
|---|---|---|---|---|
| 1 | `test_g1_analyzer_supported_never_reported_unsupported` | `BROKEN.cbl` | no statements recovered; nothing to cross-check | **capability-guarding** |
| 2 | `test_g3_transpiler_failure_was_predicted_by_the_analyzer` | `FULLSUP.cbl` | transpiles cleanly; nothing to predict | legitimately conditional |
| 3 | `test_g4_full_coverage_means_a_clean_transpile` | `ANTLRFIT.cbl` | coverage is not 1.0 | legitimately conditional |
| 4 | `test_g4_full_coverage_means_a_clean_transpile` | `BROKEN.cbl` | coverage is not 1.0 | legitimately conditional |
| 5 | `test_g4_full_coverage_means_a_clean_transpile` | `COPYUSER.cbl` | coverage is not 1.0 | legitimately conditional |
| 6 | `test_g4_full_coverage_means_a_clean_transpile` | `HEAVY.cbl` | coverage is not 1.0 | legitimately conditional |
| 7 | `test_g4_full_coverage_means_a_clean_transpile` | `PARTIAL.cbl` | coverage is not 1.0 | legitimately conditional |
| 8 | `test_g5_strict_default_raises_where_inventory_is_nonempty` | `BROKEN.cbl` | fails before/independent of dispatch (`IndexError`) | **capability-guarding** |
| 9 | `test_g5_strict_default_raises_where_inventory_is_nonempty` | `COPYUSER.cbl` | fails before/independent of dispatch (`AttributeError`) | **capability-guarding** |
| 10 | `test_g5_strict_default_raises_where_inventory_is_nonempty` | `FULLSUP.cbl` | transpiles cleanly; strict has nothing to refuse | legitimately conditional |

**Seven legitimately conditional.** G3, G4 and G5 are implications whose
antecedent is false for that fixture — G4 says "*if* the analyzer claims 100%,
the transpile is clean", and only `FULLSUP.cbl` claims 100%, so the other five
have nothing to test. These cannot skip the file into vacuity, because
`test_cross_check_actually_exercises_both_outcomes` fails if no fixture reaches
100% coverage *or* if no fixture is partial. The guarantee is asserted
somewhere for every fixture; each individual skip just says "not by this one".

**Three capability-guarding.** These three stand in for real gaps and should not
be read as neutral:

- **#8, #9** — the transpiler exits `BROKEN.cbl` with `IndexError: list index
  out of range` and `COPYUSER.cbl` with `AttributeError: 'NoneType' object has
  no attribute 'group'`. R2 requires honest failure to be a *named* refusal;
  an unhandled exception is not one. #9 is the sharper of the two: the analyzer
  does build a non-empty inventory for `COPYUSER.cbl` (`[CALL, CALL]`), so
  strict mode *should* have refused it by name — it never gets the chance,
  because it dies first. The skip holds that gap open.
- **#1** — the analyzer recovers no statements at all from `BROKEN.cbl` and
  reports `error='no statements recovered by either method — no coverage ratio
  is reported (R1)'`. Refusing to report is the honest outcome and R1 is
  satisfied. It is classified as capability-guarding rather than conditional
  because the same skip would fire on any customer program the analyzer cannot
  parse, and nothing else in the suite would register that it had.

None of the three is in WP-2.0.-1's scope to fix. They are recorded here so
that "10 skipped" is read as three known gaps plus seven satisfied
preconditions, not as ten neutral non-events.

---

### 5. CI gate — `.github/workflows/tests.yml`

Runs on push to `main` and on pull requests targeting `main`. Steps: checkout →
`actions/setup-python` pinned to `3.12` → `apt-get install gnucobol
default-jdk-headless` → `pip install -r requirements.lock` → `pip install -e
".[dev]"` → `pytest -q -rs --junitxml=junit.xml` → count assertion.

There is **no `continue-on-error` anywhere in the file** (the only occurrence of
the string is the comment explaining its absence). A step allowed to fail
without failing the job is a gate that does not gate.

The count assertion parses `junit.xml` and fails on any of: a missing report,
any failure, any error, a skip count other than `EXPECTED_SKIPS` (10), or a
zero pass count. Verified against a real report both ways — the assertion
script was extracted from the workflow and run locally:

```
# green path, full toolchain
→ GATE MET: 288 passed, 10 skipped (expected 10), 0 failed, 0 errored   (rc=0)

# runner with cobc and javac shadowed off PATH
pytest → 275 passed, 23 skipped   ← zero failures; pytest itself is GREEN
→ GATE FAILED: skip count drifted: 23, expected 10 …                    (rc=1)
```

The second case is the control that matters. A runner that silently loses its
toolchain produces a green pytest run, and the gate turns it red.

---

### 6. Open items — not decided by this work package

- **`xgboost`** stays declared, per the instruction; it is imported only by
  `src/ml/risk_scorer.py` and `src/intelligence/migration_intelligence.py`,
  neither of which is in the Phase 1/2 transform path. Separate disposition.
- **`jupyter`/`ipython`/`pre-commit` in the `dev` extra.** The WP specifies CI
  runs `pip install -e ".[dev]"`, so CI now installs the full Jupyter tree
  (about 90 of the 140 locked distributions) in order to run pytest. That is
  deterministic and hashed, so it is not a correctness problem, but it is a
  large transitive surface installed for nothing — the same objection §1 makes
  against `torch`. Splitting `dev` (test/lint toolchain) from a `notebook`
  extra would fix it. Not done here: the WP said to move five named tools into
  `dev`, not to restructure the extras.
- **`src/analysis` guard, PEP 420 caveat.** See §7.

---

### 7. R6 guard — `src/analysis` (WP item 4)

`test_deleted_analysis_package_is_gone` asserted
`not (REPO_ROOT / "src" / "analysis").exists()`. That tests the filesystem when
what R6 constrains is the import graph, and on 2026-08-20 it went red on a
developer box whose only residue under `src/analysis` was a `__pycache__`
directory `git clean` had not reached.

**A bare `pytest.raises(ModuleNotFoundError)` would NOT have fixed it.** Under
PEP 420 a directory containing nothing but build artifacts still imports, as a
namespace package. Measured directly:

```
mkdir -p src/analysis/__pycache__ && touch src/analysis/__pycache__/semantic.cpython-311.pyc
python3 -c "import importlib; m = importlib.import_module('src.analysis'); print(m, m.__file__, m.__path__)"
→ <module 'src.analysis' (<_frozen_importlib_external.NamespaceLoader …>)>
  __file__ = None
  __path__ = ['/home/user/relian/src/analysis']
```

So the literal assertion would have reproduced the very false positive it was
written to remove. The implemented guard
(`test_deleted_analysis_package_is_not_importable`) runs an out-of-process
probe that classifies the outcome three ways — `absent`, `namespace-shell`
(imports, but `__file__` is None and no `*.py` under `__path__`), or
`importable` — and fails only on `importable`. It then separately asserts that
`src.analysis.semantic`, the module that actually put customer source into an
OpenAI prompt, raises `ModuleNotFoundError`.

Verified in all three states:

| State of `src/analysis` | Expected | Measured |
|---|---|---|
| absent | pass | **1 passed** |
| `__pycache__` only (the false positive) | pass | **1 passed** |
| `__init__.py` + `semantic.py` with `from openai import AsyncOpenAI` | fail | **1 failed** — `assert 'importable' != 'importable'` |

The guard keeps its teeth and loses the false positive.

---

### 8. Result

Measured in the CI-equivalent virtualenv (CPython 3.12.3, GnuCOBOL 3.1.2.0,
`javac` 21.0.10), after every change in this work package:

```
/tmp/ci-venv/bin/python -m pytest -q -rs
→ 288 passed, 10 skipped in 30.27s
```

**`288 passed, 10 skipped, 0 failed` — the baseline is preserved exactly.**
