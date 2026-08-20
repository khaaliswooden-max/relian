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

### 8. The gate's first run caught a real defect — in this work package

Run 1 of `tests.yml` (PR #22) went **red**: `9 errors in 2.35s`, zero tests
collected, nine `ModuleNotFoundError: No module named 'src'` at import time
across `tests/assessment/`.

The cause was a flaw in how §1–§7 were verified, not in the runner. Local
verification used `python -m pytest`; the workflow runs bare `pytest`, as the
work package specifies. Those are not equivalent:

- **`python -m pytest`** prepends the working directory to `sys.path` — a
  documented side effect of `-m`. From the repo root that puts the repo root on
  the path, so `from src.assessment.coverage import analyze` resolves.
- **`pytest`** does not. Nothing else supplied the repo root either.
  `pip install -e .` does **not**: setuptools auto-detects this project as
  src-layout and points the editable path entry at `<repo>/src`, so
  `assessment` and `core` become importable as TOP-LEVEL names while
  `src.assessment` does not resolve at all. Measured:

```
cat .venv/lib/python3.12/site-packages/__editable__.relian-0.1.0.pth
→ /home/user/relian/src
```

So bare `pytest` has never worked in this repository from a clean checkout.
The latent breakage predates this work package; the new gate is what surfaced
it, on its first run, which is the gate doing its job.

Fixed at the root rather than by changing the command: `pythonpath = ["."]` in
`[tool.pytest.ini_options]`. It is rootdir-relative, so it holds for either
invocation and for a contributor running bare `pytest` locally.

Reproduced and verified, in the CI-equivalent virtualenv:

| Invocation | Before the fix | After |
|---|---|---|
| `pytest -q -rs` (what CI runs) | **9 errors, 0 collected** | **288 passed, 10 skipped** |
| `python -m pytest -q -rs` | 288 passed, 10 skipped | 288 passed, 10 skipped |

The gate script was then re-run against the resulting `junit.xml` under bash:
`GATE MET: 288 passed, 10 skipped (expected 10), 0 failed, 0 errored`.

**Observed, not fixed (pre-existing, out of scope).**
`tests/assessment/test_complexity.py::test_module_declares_no_thresholds` opens
`'src/assessment/complexity.py'` by relative path, so it fails with
`FileNotFoundError` when pytest is invoked from any directory other than the
repository root. CI always runs from the root, so it does not affect the gate.
Recorded here rather than repaired, on the same basis as the three
capability-guarding skips in §4.

---

### 9. Result

Measured in the CI-equivalent virtualenv (CPython 3.12.3, GnuCOBOL 3.1.2.0,
`javac` 21.0.10), after every change in this work package, using the exact
command the workflow runs:

```
pytest -q -rs
→ 288 passed, 10 skipped in 29.10s
```

**`288 passed, 10 skipped, 0 failed` — the baseline is preserved exactly.**

---

### 10. CI, measured on the runner

`tests.yml` run 2 (PR #22, head `6578bfb`) — **green**:

```
--- measured environment ---
  python_version: 3.12.14
  platform: Linux-6.17.0-1022-azure-x86_64-with-glibc2.39
  cobc_version: cobc (GnuCOBOL) 3.1.2.0
  javac_version: javac 17.0.20
  toolchain_complete: yes
--- result ---
  288 passed, 10 skipped, 0 failed, 0 errored
GATE MET: 288 passed, 10 skipped (expected 10), 0 failed, 0 errored
```

`RELIAN-BENCH scoring` is also green on this head.

**Finding — the JDK is NOT actually pinned, and the §3 fixture is what showed
it.** Three different JDKs have now run this suite:

| Where | `javac` |
|---|---|
| Operator's WSL baseline (reported) | 21.0.11 |
| This container (measured, §2) | 21.0.10 |
| **GitHub runner (measured, this run)** | **17.0.20** |

`apt-get install default-jdk-headless` resolves to whatever the runner image
calls default, and on `ubuntu-latest` today that is **Java 17**, not the 21 the
baseline names. The suite passes on all three, so nothing is broken — but §2 is
titled "pin the environment", and `requirements.lock` pins only the Python side.
The JDK is a floating dependency of a differential-equivalence harness whose
entire job is comparing compiled output.

Not changed here, because the work package specifies the install line verbatim
(`apt-get install gnucobol default-jdk-headless`). The one-line remedy, for the
operator to accept or decline, is to name the version:

```yaml
- uses: actions/setup-java@v4
  with: {distribution: temurin, java-version: '21'}
```

Recorded rather than acted on, and worth stating plainly: this is exactly the
class of drift the session fixture was added to make visible, and it made it
visible on the first green run.

---

## 2026-08-20 · WP-2.0.-1a · JDK pinned across both workflows

Operator decision on the §10 finding: accept the remedy, in both workflows,
before PR #22 merges.

### 11. What changed

| Workflow | Before | After |
|---|---|---|
| `.github/workflows/tests.yml` | `apt-get install … gnucobol default-jdk-headless` | `apt-get install … gnucobol` + `actions/setup-java@v4` (`temurin`, `21`) |
| `.github/workflows/bench.yml` | `apt-get install -y gnucobol default-jdk-headless` | `apt-get install -y gnucobol` + `actions/setup-java@v4` (`temurin`, `21`) |

`bench.yml` is the one that matters most. It compiles the Java candidates whose
behaviour produces the scored BER, so an unpinned compiler meant the scored
number's conditions were partly unrecorded — a provenance hole in the one place
the benchmark cannot afford one.

Both workflows now print `cobc --version`, `javac -version` and `java -version`
into the run log before any measurement. `bench.yml` additionally exports them
to `$GITHUB_ENV` and stamps them into `bench_summary.json` under a new
`toolchain` key, so a scored result carries its own compiler provenance rather
than requiring the reader to go find the log. Absent values record `unknown`,
never a plausible-looking default (R1).

### 11a. Measured on the runner after the pin

Both workflows, PR #22, head `2a0a0d7` — both **green**, and both resolved the
same Temurin build (`Java_Temurin-Hotspot_jdk/21.0.12-8/x64`):

| Workflow | Run | `javac -version` | Result |
|---|---|---|---|
| `tests` | 4 | **`javac 21.0.12`** | `GATE MET: 288 passed, 10 skipped (expected 10), 0 failed, 0 errored` |
| `RELIAN-BENCH scoring` | 122 | **`javac 21.0.12`** | `THRESHOLD MET: n_vectors 425, BER 1.0 >= 0.95, build_rate 1.0 >= 1.0, branch_coverage 0.8854 >= 0.8 (jacoco-0.8.12)` |

`tests.yml`, from the session fixture via the JUnit properties:

```
--- measured environment ---
  python_version: 3.12.14
  cobc_version: cobc (GnuCOBOL) 3.1.2.0
  javac_version: javac 21.0.12
  javac_path: /opt/hostedtoolcache/Java_Temurin-Hotspot_jdk/21.0.12-8/x64/bin/javac
  toolchain_complete: yes
```

`bench_summary.json` now carries its own compiler provenance:

```json
"toolchain": {
  "cobc":  "cobc (GnuCOBOL) 3.1.2.0",
  "javac": "javac 21.0.12"
}
```

The held-out BER is unchanged at **1.0 over 425 vectors** and branch coverage at
**0.8854**, against 17.0.20 previously — so the compiler move cost nothing and
the scored number now travels with the compiler that produced it.

**§12's caveat was demonstrated on the first run.** The pin says `'21'`; the
runner resolved **21.0.12**, while the local validation in §13 ran on
**21.0.10**. Two different patch releases under one pin, on the same day. The
major-version jump is closed; patch drift is real, visible, and recorded rather
than assumed — which is exactly the arrangement §12 describes.

### 12. `java-version: '21'` pins the MAJOR version only

This is a real, remaining limitation and is not papered over. `setup-java` with
`java-version: '21'` resolves to the newest Temurin 21.x.y available to the
runner at the time of the run. **Patch releases will drift** — 21.0.10 today,
21.0.13 in some later month — without any change to this repository, and
`requirements.lock` has no equivalent for the JDK.

What this does and does not buy:

- **Fixed:** the major-version jump. A silent 17-vs-21 difference between the
  baseline and CI, which is where language- and library-level behaviour changes
  actually live, can no longer happen unnoticed.
- **Not fixed:** patch drift within 21.x.

The mitigation is recording rather than pinning: every run log and every
`bench_summary.json` now carries the exact `javac -version` string that produced
the result, so a scored number can always be re-derived against the compiler
that produced it. A reader who needs bit-exact reproduction reads the recorded
string; they are never asked to assume it.

Pinning the patch release exactly (`java-version: '21.0.10+7'`) is possible and
was deliberately not done: it pins the repo to a release that Temurin
eventually stops publishing, converting a provenance question into a build
failure. That trade is the operator's to revisit if bit-exact JDK reproduction
becomes a requirement.

### 13. Verified before pushing

The compiler change was validated against the **public** split before it went
anywhere near CI — a scratch copy of `bench/` and `transpiler/` outside the
repository, so nothing under `bench/` was written to:

```
javac 21.0.10
run_candidate('jdk21check', …, split='public')
→ ber 1.0, build_rate 1.0, branch_coverage 0.8333,
  coverage_tool jacoco-0.8.12, valid true
  P01_payroll … P07_exitflow: build_ok=True ber=1.0 (all seven)
```

Against the v1.2 ledger thresholds (`ber_heldout_min 0.95`,
`build_rate_min 1.00`, `branch_coverage_min 0.80`) the JDK switch moves no
scored number on the public split. The held-out split is CI-only (R3) and was
neither read nor run here.

### 14. STOPPED — `bench/SPEC.md` is inside the seal

The work package asked for a line in `bench/SPEC.md` naming the Java compiler
alongside `cobc 3.1.2.0`, with the instruction to stop and escalate if SPEC.md
is covered by the v1.2 manifest hash.

**It is covered. Nothing under `bench/` was edited.** Measured:

```
bench/harness/commit.py:  INCLUDE_FILES = ["SPEC.md"]

LEDGER_relian-bench-v1.2.json → files[] entry:
  {"path": "SPEC.md",
   "sha256": "409f6df7141b15fdee65ba24224cdb8daa762429df57f5948bf22231f3c87463"}

sha256(bench/SPEC.md) on disk:
   409f6df7141b15fdee65ba24224cdb8daa762429df57f5948bf22231f3c87463   ← matches; seal intact
```

A one-line append was simulated in a scratch copy: the hash moves to
`117f2929f107c60aa9e54fee818206c2a973df42ae841d4b4a12490c359ca104`, which
breaks `payload_sha256`, which breaks `manifest_sha256`, which breaks the
Ed25519 signature — and `bench.yml`'s own first gate (`assert verify(m),
'LEDGER signature invalid — benchmark tampered'`) would then fail the build.
There is no such thing as a documentation-only edit to a sealed file.

**Related finding, and the reason the request was well-aimed.** The sealed
ledger's own `toolchain` block already has the field and it is empty:

```json
"toolchain": {
  "cobc":   "cobc (GnuCOBOL) 3.1.2.0",
  "javac":  "UNAVAILABLE",
  "java":   "UNAVAILABLE",
  "jacoco": "0.8.12 (agent sha256:115e8e6e…, cli sha256:594c0112…)"
}
```

The sealing machine had no JDK. So the Java compiler is not merely missing from
SPEC's prose — it was never captured as a sealing condition at all, and the
signed record says so honestly. That gap cannot be closed by editing SPEC.md;
it closes only on a re-seal, which is an operator action under ZCS-6 Phase 4 and
CLAUDE.md rule 4.

Recorded here so the requirement is not lost: **a v1.3 re-seal should populate
`toolchain.javac` / `toolchain.java` and add the Java compiler to SPEC's
toolchain section.** Until then, per-run recording (§11, §12) is what carries
the Java-side provenance.
