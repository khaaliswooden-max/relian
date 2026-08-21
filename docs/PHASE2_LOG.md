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

---

## 2026-08-20 · WP-2.0.0 · Pre-flight (read-only)

- **HEAD at start:** `3faa4d7` (`Merge pull request #22 from
  khaaliswooden-max/claude/phase2-wp-2-0-execution-kn14f5`), verified equal to
  `origin/main` — `git rev-parse HEAD origin/main` → both
  `3faa4d70438addf990b6452810835236e5338ba2`.
- **Branch:** `claude/wp-2-0-0-preflight-1vql0s`
- **Scope:** read-only. The only file written inside the repository is this
  log. The CardDemo clone lives at `~/corpora/carddemo`, the upstream grammar
  at `/workspace/antlr/grammars-v4`, and the D3 probe at `/tmp/d3probe` — all
  outside the working tree, none added to `.gitignore`.

### §0 — Pre-flight findings

Every row is a value produced by the command beside it, in this run, on the
pinned environment described in §0.0. Rows that could not be measured say so
and are marked **UNRESOLVED** rather than inferred (R1).

#### §0.0 Environment — reconstructed to the pin before anything was measured

The container this session started in was **not** the pinned environment:
`python3 -VV` → `3.11.15`, and `command -v cobc` → empty. Both were corrected
before item 2 or item 5 was measured, and the correction was verified against
the recorded pin in §2 rather than assumed.

| Item | Measured value | Command |
|---|---|---|
| Interpreter | `Python 3.12.3 (main, Mar  3 2026, 12:15:18) [GCC 13.3.0]` | `python3.12 -m venv /tmp/pin-venv && /tmp/pin-venv/bin/python -VV` |
| `pip freeze` hash | `c8c37f126d4c231ab9d6d2091147759cd247f3725b211028ddf44304eab5c38f` — **identical to the value recorded in §2** | `/tmp/pin-venv/bin/pip install -r requirements.lock && /tmp/pin-venv/bin/pip freeze --exclude-editable \| sort \| sha256sum` |
| GnuCOBOL | `cobc (GnuCOBOL) 3.1.2.0` | `apt-get install -y gnucobol3` (`3.1.2-5.1ubuntu1`); `cobc --version` |
| Java compiler | `javac 21.0.10` | `javac -version` |
| ANTLR runtime | `antlr4-python3-runtime 4.13.2` | `/tmp/pin-venv/bin/pip show antlr4-python3-runtime` |

The freeze-hash match is what licenses calling the §0.2 table a measurement on
the pinned toolchain: it is the same interpreter and the same 137 installed
distributions that §2 recorded, not merely a similar one.

---

#### §0.1 WP-1.5.4 / WP-1.5.5 merge status — **D5's gate is satisfied; WP-2.0 is unblocked**

| Row | Measured value | Command |
|---|---|---|
| Commits ever touching the file | 7 | `git log --oneline -- transpiler/c1_rulebased.py` |
| The WP-1.5.4/1.5.5 commit | `f5c361d` — `WP-1.5.4 + WP-1.5.5: VALUE clause; CONTINUE / GOBACK / EXIT PROGRAM` | as above |
| Is it on `main`? | **YES** | `git merge-base --is-ancestor f5c361d origin/main` → exit 0 |
| Its own diff | `+203 −21` (net **+182**) | `git diff --numstat f5c361d^ f5c361d -- transpiler/c1_rulebased.py` |
| The pull that landed it | **PR #15**, merge `377bb32` | `git rev-list --ancestry-path --merges f5c361d..3faa4d7 \| tail -1` |
| That pull's effect on the file | `+208 −21` (net **+187**), 758 → 945 lines | `git diff --numstat 377bb32^1 377bb32 -- transpiler/c1_rulebased.py` |
| Current file length | 966 lines | `wc -l transpiler/c1_rulebased.py` |

**Plainly: yes.** The lines are the VALUE clause (WP-1.5.4) and
CONTINUE / GOBACK / EXIT PROGRAM (WP-1.5.5) handlers, not something else. All
three verbs are registered in the dispatch table at `3faa4d7`, including the
two-word `EXIT PROGRAM` key that WP-1.5.5 introduced:

```
grep -nE '^\s*"(CONTINUE|GOBACK|EXIT PROGRAM)"\s*:' transpiler/c1_rulebased.py
→ 911:    "CONTINUE": _tx_continue,
  922:    "EXIT PROGRAM": _tx_exit_program,
  923:    "GOBACK": _tx_goback,
```

and the VALUE-clause machinery WP-1.5.4 added is present:

```
grep -nE '^_VALUE_RE|^def _(value|normalise_value|rescale)' transpiler/c1_rulebased.py
→ 115:_VALUE_RE = re.compile(
  120:def _rescale_numeric(lit: str, dec: int, context: str) -> str:

grep -c "WP-1.5.4\|WP-1.5.5" transpiler/c1_rulebased.py
→ 19
```

**Correction to the premise, recorded rather than smoothed over.** The figure
"~310 lines in the pull that landed on main" was not reproduced by any
measurement. The candidates are +182 (the commit), +187 (its pull), +208
(gross added lines in that pull), or +383 net / `562 −179` across the whole
span from the stale local `main` ref (`9d364d8`, 583 lines) to `3faa4d7`. None
is 310. This does not affect the gate — the handlers are on `main` either way —
but the 310 figure should not be carried forward as measured.

Two later commits also touch the file and are on `main`: `8c676e9` (`+6 −1`,
alpha `VALUE ZERO` fills `'0'`) and `6e9dfa6` (`+22 −1`, PERFORM registered
under its qualified key). Both are fixes to WP-1.5.4/1.5.5 behaviour.

---

#### §0.2 Pre-swap ANTLR baseline — re-measured on the pinned environment

This is WP-2.0's acceptance baseline. It is a measurement executed in this run
on the §0.0 toolchain, not a citation of the shipped reports. It mirrors
`src/assessment/cli.py`'s loop exactly (`build_inventory` → `coverage.analyze`),
because `antlr_syntax_errors` is only emitted into the provenance string when
`method == "token_scan"` (`src/assessment/coverage.py:504`) and the baseline
must record it for all seven regardless of method.

```
/tmp/pin-venv/bin/python /tmp/wp200/baseline.py
```

| program | antlr_syntax_errors | method | parse_ok | grade | coverage_ratio |
|---|---|---|---|---|---|
| P01_payroll/program.cbl | 11 | token_scan | False | PLAUSIBLE | 1.0 |
| P02_interest/program.cbl | 8 | token_scan | False | PLAUSIBLE | 1.0 |
| P03_eligibility/program.cbl | 4 | token_scan | False | PLAUSIBLE | 1.0 |
| P04_taxtable/program.cbl | 8 | token_scan | False | PLAUSIBLE | 1.0 |
| P05_validate/program.cbl | 2 | token_scan | False | PLAUSIBLE | 1.0 |
| P06_valinit/program.cbl | 5 | token_scan | False | PLAUSIBLE | 1.0 |
| P07_exitflow/program.cbl | 1 | token_scan | False | PLAUSIBLE | 1.0 |

**Drift from 11/8/4/8/2: none.** P01–P05 reproduce the shipped figures exactly.
The reason is measurable rather than lucky — the registry the coverage pass
reads is byte-identical to the one the shipped report used:

```
grep -o "SUPPORTED_STATEMENTS@[a-f0-9]*" docs/dryruns/bench_corpus/assessment.md | sort -u
→ SUPPORTED_STATEMENTS@5fcbba7
/tmp/pin-venv/bin/python -c "from src.assessment.supported import registry_provenance; print(registry_provenance())"
→ SUPPORTED_STATEMENTS@3faa4d7 (c1_rulebased.py sha256:a440ac2751bb738d)
```

The git ref label moved (`5fcbba7` → `3faa4d7`); the content hash
`a440ac2751bb738d` did not. Same input, same parser, same counts.

**Correction to the premise.** P06 and P07 were *not* uncaptured. They are in
the shipped `docs/dryruns/bench_corpus/assessment.md` at lines 75–76 with
`antlr_syntax_errors=5` and `=1`, added by `f5c361d` and `6e9dfa6`
(`git log --oneline -- docs/dryruns/bench_corpus/assessment.md`). This run
reproduces both. What *was* true is that those figures predate the environment
pin (`3606151`, 2026-08-20); they are now re-measured on it and unchanged.

`parse_ok=False` for all seven, so every program falls to `token_scan` and
every grade is `PLAUSIBLE` — no program in the corpus currently reaches the
`antlr_tree` / `VERIFIED` path. That is precisely the condition WP-2.0's
grammar swap exists to change, and this table is the "before" it will be
scored against. The dominant cause is a single unsupported construct:

```
P01–P06: "extraneous input 'COMP-3' expecting {...}"   (USAGE COMP-3)
P07:     "no viable alternative at input 'ACCEPTWS-RAW'"
```

**Determinism verified**, since an acceptance baseline that drifts is worthless:

```
# baseline table, run twice
sha256sum /tmp/wp200/run1.txt /tmp/wp200/run2.txt
→ 64895524871402aa9907edb9611ce29e98b54e7f669f935f2eebcc301d204309  (both)
# full CLI, run twice into separate output directories
/tmp/pin-venv/bin/python -m src.assessment.cli bench/corpus --out /tmp/wp200/r1 --json-only
/tmp/pin-venv/bin/python -m src.assessment.cli bench/corpus --out /tmp/wp200/r2 --json-only
sha256sum /tmp/wp200/r1/assessment.json /tmp/wp200/r2/assessment.json
→ ac5cab0c6b75797a53511b973bc4f7cd6c635134433429100f82170ca907cb8d  (both)
```

Portfolio figures from that run: 7 programs, 19 manifest files, coverage
`1.0 (PLAUSIBLE)`, portfolio risk `LOW`, `manifest_hash
cc4513ba7feda336a554556ed8e638e99ec7144b064294cfc92681564d54bb90`.

---

#### §0.3 Grammar target

**Current, in-repo** (`src/parsers/grammars/Cobol85.g4`):

| Row | Measured value | Command |
|---|---|---|
| Line count | **376** (matches the stated 376) | `wc -l src/parsers/grammars/Cobol85.g4` |
| Parser rules | **119** (matches the stated 119) | `grep -cE '^[a-z][A-Za-z0-9_]*\s*:' …` |
| Lexer rules | 201 | `grep -cE '^[A-Z][A-Za-z0-9_]*\s*:' …` |
| sha256 | `eb88e8c1a8d570c59271924e547983a107405752bf971fdbd2f3f2ac787a89bf` | `sha256sum …` |

**Upstream target**, fetched to `/workspace/antlr/grammars-v4`, **not vendored**
— nothing was copied into the working tree:

| Row | Measured value | Command |
|---|---|---|
| Repository | `https://github.com/antlr/grammars-v4` | `git remote get-url origin` |
| Repo HEAD at fetch | `aca577d9e30e591eacbc414f1280f22645412af4` | `git rev-parse HEAD` |
| `Cobol85.g4` last commit | `753536777d827ccc0c9b108531ea67375c2039ac` (2023-11-29, "Reformatting all grammars (#3843)") | `git log -1 --format=%H -- cobol85/Cobol85.g4` |
| `Cobol85.g4` sha256 | `c338bff84b5a7d89113dacdff69764593688fd0915f24fba2f07a5fec2063e35` | `sha256sum cobol85/Cobol85.g4` |
| `Cobol85.g4` lines | **5654** (15.0× the current 376) | `wc -l cobol85/Cobol85.g4` |
| `Cobol85.g4` parser rules | **595** (5.0× the current 119) | hanging-colon rule count (below) |
| `Cobol85.g4` lexer rules | 565 | as above |
| `Cobol85Preprocessor.g4` exists? | **YES — a separate grammar** | `ls cobol85/` |
| Preprocessor sha256 | `8d88a679ae574a2645c827c21f467031669e2713d149c8fec46bc0dab86b4841` | `sha256sum cobol85/Cobol85Preprocessor.g4` |
| Preprocessor lines / rules | 1902 lines; 30 parser + 292 lexer rules | as above |
| Licence | **MIT**, per the grammar's own header (© 2017 Ulrich Wolffgang, from `github.com/uwol/cobol85parser`) | `sed -n '1,8p' cobol85/Cobol85.g4` |

The upstream file uses ANTLR's hanging-colon layout (rule name alone on a line,
`:` on the next), so a naïve `^name\s*:` count returns 0 and would have been
reported as a false zero. The counts above come from:

```
awk '/^[A-Za-z_][A-Za-z0-9_]*[ \t]*$/{name=$1;pending=1;next}
     /^[ \t]*:/{if(pending){if(name~/^[a-z]/)p++;else l++};pending=0;next}
     {pending=0} END{printf "parser=%d lexer=%d\n",p,l}' cobol85/Cobol85.g4
→ parser=595 lexer=565
```

**Is the preprocessor required for COPY-bearing source to parse? YES —
established by measurement, not by reading the docs.** In the main grammar
`COPY` exists only as a lexer token, and **no parser rule anywhere references
it**:

```
sed -n '3666,3668p' cobol85/Cobol85.g4
→ COPY
      : C O P Y
      ;
  (an uppercase-initial rule, i.e. a lexer token)

# strip the lexer region, then look for COPY in parser rules:
awk 'BEGIN{p=0} /^[a-z][A-Za-z0-9_]*[ \t]*$/{p=1} /^[A-Z][A-Za-z0-9_]*[ \t]*$/{p=0} p' \
    cobol85/Cobol85.g4 | grep -cw COPY
→ 0
```

A token no rule can consume is a guaranteed syntax error. `COPY` and `REPLACE`
are consumed only in `Cobol85Preprocessor.g4` (`copyStatement`, `copySource`,
`copyLibrary`, `replaceArea`, `replaceByStatement`, `replaceOffStatement`,
`replaceClause`, `replaceable`, `replacement` — lines 237–289). The grammar's
own header agrees: *"To be used in conjunction with the provided preprocessor,
which executes COPY and REPLACE statements."* Adopting `Cobol85.g4` without the
preprocessor would leave every COPY-bearing program unparseable — which, per
§0.4, is 40 of CardDemo's 44 programs.

**UNRESOLVED:** `antlr/grammars-v4` carries **no `LICENSE` file** at the
repository root or in `cobol85/` (`find . -maxdepth 1 -iname 'LICENSE*'` →
empty). The MIT designation above rests on the grammar file's own header, which
points at a `LICENSE file` that is not present in the repository. Flagged for
the operator: the licence claim for a vendored grammar should not rest on a
header referencing a missing file.

---

#### §0.4 CardDemo

Cloned to `~/corpora/carddemo` — outside the repository, not added to the
working tree and not added to `.gitignore`.

```
git clone --depth=1 https://github.com/aws-samples/aws-mainframe-modernization-carddemo.git \
    ~/corpora/carddemo
```

| Row | Measured value | Command |
|---|---|---|
| Commit sha | `59cc6c2fd7ebd7ef7925cad552a01a4b8b6e4d5e` (2025-10-16) | `git rev-parse HEAD` |
| Licence | **Apache-2.0** (not MIT-0) — `LICENSE` is the Apache 2.0 text; `NOTICE` reads "Copyright Amazon.com, Inc."; `README.md:384` says "released under the Apache 2.0 license" | `head -5 LICENSE; cat NOTICE` |
| `.cbl` files | **44** | `find . -path ./.git -prune -o -type f -iname '*.cbl' -print \| wc -l` |
| `.cpy` files | **62** | same, `*.cpy` |
| `.jcl` files | **55** | same, `*.jcl` |

**Copybook fan-in.** Measured with a fixed-format-aware scan (columns 7–72,
`*`/`/` comment lines skipped) and a regex that refuses to match the tail of a
hyphenated identifier — the naïve `\bCOPY\s+` pattern reports a phantom
copybook named `REPLACING`, matched out of
`INITIALIZE REQUEST-MSG-COPY REPLACING NUMERIC BY ZEROES`
(`app/app-vsam-mq/cbl/CODATE01.cbl:294`). That artifact is excluded below.

| Row | Measured value |
|---|---|
| Programs with ≥1 `COPY` | **40 of 44** |
| Distinct copybook names referenced | **67** |
| — resolvable to a `.cpy` in the repo | **59** |
| — **not** resolvable | **8**: `DFHAID`, `DFHBMSCA` (CICS-supplied), `CMQGMOV`, `CMQMDV`, `CMQODV`, `CMQPMOV`, `CMQTML`, `CMQV` (MQ-supplied) |
| `.cpy` present but never referenced | 3: `CSDB2RPY`, `CSDB2RWY`, `UNUSED1Y` |
| Total (program, copybook) edges | **306** |
| Maximum fan-out | **18**, `COACTUPC.cbl` |
| Most-shared copybooks | `COCOM01Y`, `COTTL01Y`, `CSDAT01Y`, `CSMSG01Y` — 21 programs each |
| `COPY … REPLACING` sites | **40** |

The 8 unresolved names matter for WP-2.1: CardDemo is **not self-contained**.
`DFHAID` and `DFHBMSCA` are referenced by 21 programs each but ship with
CICS, not with the sample. A discovery bench must treat an unresolvable `COPY`
as a first-class outcome, not a crash.

**JCL DD statements — confirmed, they carry both DSN and DCB:**

| Row | Measured value |
|---|---|
| `.jcl` members | 55 |
| DD statements | **524**, in 53 of 55 members |
| `DSN=` / `DSNAME=` | **245**, in 42 members |
| `DCB=` | **43**, in 20 members |

DCB attributes are real sub-parameters, not placeholders — e.g.
`app/jcl/READACCT.jcl:39` → `DCB=(LRECL=107,RECFM=FB,DSORG=PS,BLKSIZE=0)`.
`LRECL` and `RECFM` are exactly the fields a record-layout oracle needs to
cross-check a copybook's computed length against, which makes CardDemo usable
for the D3 oracle and not only for discovery.

---

#### §0.5 `cobc` probe-oracle feasibility (D3)

Under `cobc (GnuCOBOL) 3.1.2.0`. Probe written to `/tmp/d3probe`, **not** the
repository; no probe code is committed.

**Route A — the listing.** `cobc -t <file> -ftsymbols` both exist and work:

| Row | Measured value | Command |
|---|---|---|
| `-t <file>` | present — "generate and place a program listing into `<file>`" | `cobc --help \| grep -E '^\s+-t '` |
| `-ftsymbols` | present — "specify symbols in listing" | `cobc --help \| grep ftsymbols` |
| Listing produced? | **YES**, a `SIZE / TYPE / LVL / NAME / PICTURE` symbol table | `cobc -x -t sym.lst -ftsymbols -I. SYMPROBE.cbl` |
| Offsets in the listing? | **NO — there is no offset column** | inspection of `sym.lst` |

```
SIZE  TYPE           LVL  NAME                           PICTURE
      WORKING-STORAGE SECTION
00054 GROUP          01   WS-CUST-WORK
00010 ALPHANUMERIC   05   WS-CW-ACCOUNT                  X(10)
00030 ALPHANUMERIC   05   WS-CW-NAME                     X(30)
00001 ALPHANUMERIC   05   WS-CW-CLASS                    X(01)
00006 NUMERIC        05   WS-CW-PRIOR-BALANCE            S9(09)V99 COMP-3
00006 NUMERIC        05   WS-CW-NEW-BALANCE              S9(09)V99 COMP-3
00001 ALPHANUMERIC   05   WS-CW-DELINQUENT-SW            X(01)
```

So the listing yields **lengths directly but offsets only by inference** —
cumulative summation, which is sound only for a flat layout and silently wrong
under `REDEFINES`, `OCCURS`, or `SYNCHRONIZED`. As a *secondary cross-check* on
lengths and on the group total it is genuinely useful; as an offset oracle it
is not, on its own.

**Route B — the D3 runtime probe.** `MOVE LOW-VALUES` to the record,
`MOVE HIGH-VALUES` to one field, then read the bytes back. Exercised against
`examples/demo/copy/MUBCUST.cpy` (6 fields, 54 bytes, two of them COMP-3):

| Field | offset | len | Command |
|---|---|---|---|
| `WS-CW-ACCOUNT` | 1 | 10 | `cobc -x -I. D3PROBE.cbl && ./D3PROBE` |
| `WS-CW-NAME` | 11 | 30 | same |
| `WS-CW-CLASS` | 41 | 1 | same |
| `WS-CW-PRIOR-BALANCE` (COMP-3) | 42 | 6 | `cobc -x -I. D3COMP3.cbl && ./D3COMP3` |
| `WS-CW-NEW-BALANCE` (COMP-3) | 48 | 6 | same |
| `WS-CW-DELINQUENT-SW` | 54 | 1 | `./D3PROBE` |

**Both offset and length are recoverable this way — yes, including packed
decimal.** The recovered layout tiles 1–54 with no gap or overlap and its total
equals the `00054` group SIZE the `-ftsymbols` listing reported independently,
so the two routes cross-check each other. That is the D3 design working as
intended: Route B measures, Route A confirms.

Three implementation facts worth carrying into D3, all measured:

1. **`FUNCTION HEX-OF` does not exist in GnuCOBOL 3.1.2.** `cobc` →
   `error: FUNCTION 'HEX-OF' unknown`. The "DISPLAY as hex" step must instead
   be a byte scan through a `REDEFINES … OCCURS n TIMES PIC X(01)` table
   comparing each byte to `HIGH-VALUE`, which is what the probe above does.
2. **`MOVE HIGH-VALUES` to a COMP-3 item compiles and produces correct bytes**,
   but `-Wall` flags it: `warning: MOVE of figurative constant to numeric item
   is archaic in GnuCOBOL [-Warchaic]`. It is a warning, not an error; D3
   should expect it and not treat it as failure.
3. The probe needs the record's total length up front to size the redefining
   table. That length comes from Route A (`-ftsymbols`) or from
   `LENGTH OF <group>`, so the two routes are ordered: listing first, probe
   second.

**Feasibility verdict: D3 is viable as specified.** `-ftsymbols` alone is a
lengths-and-sizes oracle, not an offset oracle; combined with the LOW/HIGH-VALUES
runtime probe it gives offset, length, and an independent cross-check.

---

#### §0.6 `examples/demo/` — contradicts ground-truth rows G12/G13

| Row | Measured value | Command |
|---|---|---|
| Contents | 5 `.cbl`, 3 `.cpy`, 1 `.jcl`, 3 `.csv`, 1 `.md`, 1 `.py` = 14 files | `find examples/demo -type f \| sed 's/.*\.//' \| sort \| uniq -c` |
| Origin | **Authored for the demo. Not derived from a real system.** | see below |
| FD / SELECT | **Yes, in exactly one program** — `MUBPOST.cbl`: 1 `FILE-CONTROL`, 4 `SELECT`, 1 `FILE SECTION`, 4 `FD`. The other four programs have none. | `grep -cE '^\s{7}FD\s' src/MUBPOST.cbl` etc. |
| JCL DD statements | 12 DD, 7 `DSN=`, 1 `DCB=(RECFM=FB,LRECL=80,BLKSIZE=8000)` | `grep -cE '^//\S+\s+DD\s' jcl/MUBNITE.jcl` |
| Assessment | 5 programs, 14 manifest files, coverage `0.8768 (PLAUSIBLE)`, portfolio risk **BLOCKED** | `python -m src.assessment.cli examples/demo --out … --json-only` |

**What these files are.** Hand-authored synthetic material, stated in-band in
every artifact rather than inferred. Each copybook carries
`* SYNTHETIC DEMONSTRATION CODE.  NOT PRODUCTION.`; the JCL carries
`SYNTHETIC DEMONSTRATION JCL.  NOT PRODUCTION.  NOT DERIVED FROM ANY CUSTOMER
OR BENCHMARK SOURCE.`; `README.md` opens **"Synthetic. Not production. Not
derived from any customer or benchmark source."** They model a fictional
municipal water utility (Meridian MUD) and were written against
`docs/C1_SUPPORTED_VERBS_OBSERVED.md` to land deliberately on three tiers —
`MUBRATE`/`MUBPENL`/`MUBSURC` fully in subset, `MUBBILL` partial, `MUBPOST`
blocked. The measured per-program figures confirm the tiering is real:

| program | antlr_syntax_errors | method | parse_ok | grade | coverage_ratio |
|---|---|---|---|---|---|
| src/MUBBILL.cbl | 1 | token_scan | False | PLAUSIBLE | 0.76 |
| src/MUBPENL.cbl | 1 | token_scan | False | PLAUSIBLE | 1.0 |
| src/MUBPOST.cbl | 1 | token_scan | False | PLAUSIBLE | 0.4528 |
| src/MUBRATE.cbl | 1 | token_scan | False | PLAUSIBLE | 1.0 |
| src/MUBSURC.cbl | 1 | token_scan | False | PLAUSIBLE | 1.0 |

**G12/G13 are stale.** Those rows were written when no copybooks or JCL existed
on the box; both now exist and are non-trivial. The work package's ground truth
should be corrected to match the tree at `3faa4d7`. That correction is an
operator action — this entry records the contradiction, it does not resolve it.

**Suitable as WP-2.1 discovery-bench seed material? Yes, with two limits, and
not on its own.**

Fit: it is the only material on the box that exercises the full intake surface
at once — `COPY` fan-in across three copybooks, a JCL job stream with `DSN` and
`DCB`, `FD`/`SELECT` file declarations, and a program that assesses BLOCKED. It
is synthetic and repo-local, so it carries no licence or customer-perimeter
constraint (R12) and cannot leak held-out material (R3) — it was written
against the *observed* verb list, not against `bench/`.

Limits, both measured:

1. **Scale.** 5 programs, 3 copybooks, 1 JCL member, and **4** (program,
   copybook) edges — `MUBBILL→MUBBREC`, `MUBBILL→MUBCONS`, `MUBPOST→MUBCONS`,
   `MUBPOST→MUBCUST`, max fan-out 2, every `COPY` target resolvable — against
   CardDemo's 44/62/55, 306 edges, max fan-out 18 and 8 unresolvable targets.
   It cannot exercise fan-in depth, copybook sharing across dozens of programs,
   `COPY … REPLACING`, or unresolvable `COPY` targets — CardDemo covers those
   and `examples/demo` does not.
2. **Every program fails ANTLR parse identically**, on the *same* construct:
   `line N:0 extraneous input 'AUTHOR' expecting {...}`. The current reduced
   grammar has no `AUTHOR` paragraph, so all five fall to `token_scan` and
   grade `PLAUSIBLE`. As a discovery bench this is useful — it is a real
   finding about the current grammar — but it means `examples/demo` cannot
   distinguish grammar improvements until `AUTHOR` parses, and so cannot serve
   as the *only* seed.

Recommendation for WP-2.1: seed with `examples/demo` for intake-surface breadth
and licence-free iteration, and with CardDemo for fan-in scale and
unresolvable-`COPY` behaviour. Neither alone covers the other's ground.

---

### §0.7 What this entry did not resolve

- The `~310 lines` figure in the work package (§0.1) — not reproduced by any
  measurement; four candidate values recorded instead. **UNRESOLVED**, and
  immaterial to the gate.
- The licence of `antlr/grammars-v4` (§0.3) — the repository has no `LICENSE`
  file; the MIT claim rests on a file header pointing at a missing file.
  **UNRESOLVED**, operator call before vendoring.
- G12/G13 correction (§0.6) — contradiction recorded; the work package is not
  edited by this entry.

### §0.8 Verified before pushing

This entry changes no source, no test, no workflow, no `pyproject.toml`, and
nothing under `bench/`. Confirmed, not assumed:

```
git status --short
→  M docs/PHASE2_LOG.md          (the only entry)
```

The suite was run anyway, on the §0.0 pinned environment, because a docs-only
claim is still a claim until it is measured:

```
/tmp/pin-venv/bin/python -m pytest -q -rs --junitxml=/tmp/wp200/junit.xml
→ 288 passed, 10 skipped in 38.53s
```

That is exactly the sealed triple `.github/workflows/tests.yml` gates on
(`288 passed, 10 skipped, 0 failed`, `EXPECTED_SKIPS: "10"`), measured with
`cobc (GnuCOBOL) 3.1.2.0` and `javac 21.0.10` both present — so the ten skips
are the ten fixture-shape skips enumerated in §4, not demo tests guarding
themselves off against a missing toolchain.

The clones and probes made for this entry live at `~/corpora/carddemo`,
`/workspace/antlr/grammars-v4` and `/tmp/d3probe`. None is inside the working
tree, none was added to `.gitignore`, and no probe code is committed:

```
git status --porcelain --ignored | grep -iE "carddemo|grammars-v4|corpora"
→ (no output)
```

---

## 2026-08-20 · WP-2.0.−3 · Delete the fabricated-metric ML limb (R1)

- **HEAD at start:** `a32931d` (`Merge pull request #23 from
  khaaliswooden-max/claude/wp-2-0-0-preflight-1vql0s`), verified equal to
  `origin/main` — `git rev-parse HEAD origin/main` → both
  `a32931d02fe94b47f95a891bcd7065e9bdb9f1dd`.
- **Branch:** `claude/remove-fabricated-metric-ml-hjeio7` · **PR:** #24
- **Commit:** `2beb579`
- **Scope guard:** nothing under `bench/`, `transpiler/`, `src/assessment/` or
  `demo/` was modified. `git status --porcelain bench/ transpiler/
  src/assessment/ demo/` → no output, at close-out.
- **Disposition record:** `docs/R1_ML_DISPOSITION_2026-08.md`. This entry
  records only what was executed and returned; the reasoning lives there.

### §1. Both workflows, measured on the runner

| Workflow | Run | Conclusion | Result line |
|---|---|---|---|
| `tests` | [32400032003](https://github.com/khaaliswooden-max/relian/actions/runs/32400032003) (run 9, `pull_request`) | **success** | `GATE MET: 285 passed, 10 skipped (expected 10), 0 failed, 0 errored` |
| `RELIAN-BENCH scoring` | [32398932208](https://github.com/khaaliswooden-max/relian/actions/runs/32398932208) (run 129, `push`) | **success** | `THRESHOLD MET: n_vectors 425, BER 1.0 >= 0.95, build_rate 1.0 >= 1.0, branch_coverage 0.8854 >= 0.8 (jacoco-0.8.12)` |

Toolchain recorded by the session fixture on the `tests` runner:

```
python_version: 3.12.14
cobc_version:   cobc (GnuCOBOL) 3.1.2.0
javac_version:  javac 21.0.12
toolchain_complete: yes
```

`bench` measured the same `cobc (GnuCOBOL) 3.1.2.0` / `javac 21.0.12`, stamped
into `bench_summary.json` (artifact 9417773004) against ledger
`relian-bench-v1.2`, manifest `a47305c2…`.

**`tests.yml` cannot run on a feature-branch push.** Its trigger is
`push: branches: [main]` + `pull_request: branches: [main]`, so the branch push
started `bench` only; `tests` first ran when PR #24 opened. The two runs are
therefore against the same `head_sha` (`2beb579`) but different events, which is
why their run numbers are not adjacent.

### §2. Suite delta

| | Before (`a32931d`) | After (`2beb579`) |
|---|---|---|
| passed | 288 | **285** |
| skipped | 10 | **10** |
| failed | 0 | **0** |

Net −3 passed: −12 from the deleted `tests/test_ml.py`, +9 from the added
`tests/test_no_fabricated_metrics.py`. `EXPECTED_SKIPS` is unchanged at 10 and
still gates — the ten are the fixture-shape skips enumerated in §4 of the
WP-2.0.−1 entry, not toolchain skips.

The prose triple in `.github/workflows/tests.yml` and `tests/conftest.py` was
updated from 288 to 285. **The historical entries above this one were not
touched** — this log is append-only and each entry records what was measured at
the time it was written.

### §3. Dependency set

`xgboost` removed from `[project.dependencies]`; `jupyter`, `ipython` and
`pre-commit` moved from the `dev` extra to a new `notebook` extra that CI does
not install. `requirements.lock` regenerated with the command in its own header.

```
git show a32931d:requirements.lock | grep -cE '^[a-zA-Z0-9][a-zA-Z0-9._-]*=='
→ 140
git show 2beb579:requirements.lock | grep -cE '^[a-zA-Z0-9][a-zA-Z0-9._-]*=='
→ 38
```

The CI invariant that `pip install -e ".[dev]"` resolves nothing new after
`pip install -r requirements.lock` was re-verified in a fresh 3.12 venv:
`Successfully installed relian-0.1.0` alone, then `pip check` →
`No broken requirements found.` On the runner the pinned-dependency install step
took **12s** (17:52:58 → 17:53:10).

### §4. Residual — `numpy` has no import site

```
grep -rEn "^\s*(import|from)\s+numpy\b" --include="*.py" .
→ (no output)
```

Both of its consumers were the two deleted modules. It is **left declared**, as
removing it was outside this WP's brief; the justification comment in
`pyproject.toml` was corrected to state that rather than keep the void one. On
the WP-2.0.−1 precedent (`langchain`, `transformers`, `torch`, `pandas`) it is a
candidate for the next pass. **UNRESOLVED — operator's call.**

### §5. Guard, planted-red verified

`tests/test_no_fabricated_metrics.py`, 9 tests. Each assertion was broken,
observed, and reverted before the commit:

| Planted defect | Result |
|---|---|
| `src/ml/` recreated with a loadable `risk_scorer.py` | **2 failed** as designed |
| `src/ml/__pycache__/risk_scorer.cpython-312.pyc` and nothing else | **9 passed** — as designed; the PEP 420 namespace-shell false positive is correctly classified clean |
| `xgboost>=2.0.0` re-added to `pyproject.toml` | **1 failed** as designed |
| `import xgboost as xgb` inside `_score_risk`'s body | **2 failed** as designed |
| `_score_risk` returning `{"overall_score": 42.0, "risk_level": "medium"}` | **1 failed** as designed |
| `intelligence=None` re-added to `MigrationOrchestrator.__init__` | **1 failed** as designed |

All reverted; 9/9 green afterwards, and the full suite green as recorded in §1.

### §6. A guard repaired rather than lost

`test_no_generative_ai_in_transform_path.py::test_relative_imports_in_package_inits_resolve`
used `src/ml/__init__.py` as its only fixture behind
`if not init.is_file(): pytest.skip(...)`. Deleting `src/ml` would have turned
an R6 regression guard into a permanent silent skip **and** drifted the skip
count to 11, failing the gate — the gate would have caught it, but the honest
fix is not to raise `EXPECTED_SKIPS`. The assertion now runs over every package
`__init__.py` under `src/` carrying a relative import (`src/core`,
`src/parsers`, `src/blockchain`, `src/generators`) and asserts non-empty, so it
pins the resolver property itself and cannot decay into a skip again.

---

## 2026-08-20 · WP-2.0 · Replace the COBOL grammar

- **HEAD at start:** `4ecfcc7` (`Merge pull request #24 from
  khaaliswooden-max/claude/remove-fabricated-metric-ml-hjeio7`), verified equal
  to `origin/main` — `git rev-list --left-right --count origin/main...HEAD` →
  `0	0`.
- **Branch:** `claude/wp-2-0-cobol-grammar-n2sre1`
- **Scope guard:** nothing under `bench/corpus/`, `bench/harness/` or
  `transpiler/` was modified. `git status --porcelain bench/ transpiler/` → no
  output, at close-out (§9).
- **Baseline scored against:** the WP-2.0.0 pre-flight table in §0.2 above.

### §1. Environment — reconstructed to the pin before anything was measured

The container this session started in was not the pinned environment:
`python3 -VV` → `3.11.15`, `command -v cobc` → empty, no ANTLR jar. Corrected
and verified before any figure below was taken.

| Item | Measured value | Command |
|---|---|---|
| Interpreter | `Python 3.12.3 (main, Mar  3 2026, 12:15:18) [GCC 13.3.0]` | `python3.12 -m venv /tmp/pin-venv && /tmp/pin-venv/bin/python -VV` |
| GnuCOBOL | `cobc (GnuCOBOL) 3.1.2.0` | `apt-get install -y gnucobol3; cobc --version` |
| Java compiler | `javac 21.0.10` | `javac -version` |
| ANTLR runtime | `antlr4-python3-runtime 4.13.2` | `/tmp/pin-venv/bin/pip show antlr4-python3-runtime` |
| `pip freeze` hash | `ad41a502b4e2f700fd2a7c95e873d19cdbb75827b186c062ca10d80536aa4946` | `pip install -r requirements.lock && pip freeze --exclude-editable \| sort \| sha256sum` |

**The freeze hash is NOT the `c8c37f12…` recorded in §0.0, and that is
expected, not drift.** §0.0 hashed the 140-pin lock; WP-2.0.−3 regenerated
`requirements.lock` down to 38 pins (§3 of that entry). Confirmed rather than
assumed:

```
grep -cE '^[a-zA-Z0-9][a-zA-Z0-9._-]*==' requirements.lock
→ 38
```

The `ad41a502…` value is the hash of the current lock, measured in this run.

### §2. Licence gate — both conditions checked BEFORE anything was copied

The work package made vendoring the preprocessor conditional on its header
carrying the same `Copyright (C) 2017, Ulrich Wolffgang` / MIT block as the
main grammar. It is not merely equivalent — it is **byte-identical**:

```
sha256sum <(sed -n '1,7p' cobol85/Cobol85.g4) <(sed -n '1,7p' cobol85/Cobol85Preprocessor.g4)
→ 614ee811d5e6ce31a3f2bc511901aed2828ca3e5d27f591cf0ac2b61291fd2e3   (both)
diff <(sed -n '1,7p' cobol85/Cobol85.g4) <(sed -n '1,7p' cobol85/Cobol85Preprocessor.g4)
→ (no output)
```

**Gate satisfied; the preprocessor was vendored.** Both header blocks are
intact in the vendored files.

**§0.7's UNRESOLVED licence question is closed.** The objection was correct:
`antlr/grammars-v4` carries no `LICENSE` file, so the MIT claim rested on a
header pointing at a file absent from that repository. It is closed by looking
where the header actually points — the ProLeap COBOL parser — which does carry
the text. Fetched and archived verbatim, not retyped:

```
curl -fsSL https://raw.githubusercontent.com/uwol/proleap-cobol-parser/main/LICENSE \
     -o docs/licenses/proleap-cobol85-MIT.txt
→ HTTP 200, 21 lines, "MIT License / Copyright (c) 2017 Ulrich Wolffgang"
sha256sum docs/licenses/proleap-cobol85-MIT.txt
→ 5de028e49764aa5f3212085092085b3c26350cb68d73535264667f96b05a98ac
```

Full provenance per file — author, year, licence, upstream project, vendored-from
repo and commit, sha256 — is in `docs/GRAMMAR_PROVENANCE.md`.

### §3. The swap

Upstream pinned at `aca577d9e30e591eacbc414f1280f22645412af4`; both file
hashes reproduce §0.3's exactly, so the bytes vendored are the bytes that entry
inspected.

| | Before | After |
|---|---|---|
| `Cobol85.g4` lines | 376 | **5654** |
| `Cobol85.g4` parser rules | 119 | **595** |
| `Cobol85.g4` lexer rules | 201 | **565** |
| `Cobol85.g4` sha256 | `eb88e8c1a8d570c5…` | `c338bff84b5a7d89…` |
| `Cobol85Preprocessor.g4` | absent | 1902 lines, 30 parser + 292 lexer rules, `8d88a679ae574a26…` |

```
sha256sum src/parsers/grammars/*.g4
→ c338bff84b5a7d89113dacdff69764593688fd0915f24fba2f07a5fec2063e35  Cobol85.g4
  8d88a679ae574a2645c827c21f467031669e2713d149c8fec46bc0dab86b4841  Cobol85Preprocessor.g4
```

### §4. Regeneration

`tools/regen_parser.sh`, pinning ANTLR **4.13.2** by SHA-256. The jar was taken
from `antlr.org` and **verified byte-identical to Maven Central's copy** before
being trusted — two independent sources, not one download:

```
curl -sSL https://www.antlr.org/download/antlr-4.13.2-complete.jar -o antlr.jar
curl -sSL https://repo1.maven.org/maven2/org/antlr/antlr4/4.13.2/antlr4-4.13.2-complete.jar -o antlr-maven.jar
cmp antlr.jar antlr-maven.jar   → identical
sha256sum antlr.jar             → eae2dfa119a64327444672aff63e9ec35a20180dc5b8090b7a6ab85125df4d76
```

Committed output (`tools/regen_parser.sh`, 2.9 s wall):

| File | Bytes | sha256 |
|---|---|---|
| `Cobol85Lexer.py` | 213,682 | `593d56f63fd90009…` |
| `Cobol85Parser.py` | 1,986,745 | `5d5f085a5d2467df…` |
| `Cobol85Listener.py` | 202,945 | `65032301226a23fc…` |
| `Cobol85Visitor.py` | 118,015 | `4624bdadac4099be…` |
| `Cobol85.tokens` / `Cobol85Lexer.tokens` | 7,262 | `c9161b91f1093d78…` |
| `Cobol85.interp` | 262,922 | `9ad130afde1b0108…` |
| `Cobol85Lexer.interp` | 224,750 | `f3fe3d78a44cc81e…` |

The jar is **not** committed; the script fetches it to `.antlr/` (added to
`.gitignore`) and verifies its hash on every run, not only after fetching.

#### The byte-identity gate, with teeth proven

`.github/workflows/tests.yml` gains a job **`parser-regen`** running
`tools/regen_parser.sh --check`. Each failure mode was planted, observed, and
reverted:

| Planted defect | Result |
|---|---|
| two lines appended to `Cobol85Parser.py` | **exit 1**, `DIFFERS from a fresh generation` |
| an extra `Stray.py` in the output directory | **exit 1**, `UNEXPECTED file in src/parsers/antlr/cobol` |
| one byte appended to the pinned jar | **exit 1**, `ANTLR jar sha256 mismatch` |
| (reverted) | **exit 0**, `OK — committed parser is byte-identical to a fresh generation` |

One real bug was found and fixed while proving this: the diff printer piped
through `head -40`, and under `set -o pipefail` a long diff makes `head` close
the pipe, turning a reportable difference into a SIGPIPE exit that hides the
remaining files. Replaced with `sed -n '1,40p'`, which drains its input.

### §5. Re-mapping the tree walker

**`_STATEMENT_VERBS`** — an explicit table, one row per alternative of the
grammar's `statement` rule, mapping generated context class → the verb the
transpiler's dispatch table is keyed by. Auditable by reading it.

It is **checked against the grammar on every walk**, and the check does not
compare against a second hand-written list — that would only prove the two
lists agree with each other. `statement_alternatives()` reads the alternatives
off the generated parser (ANTLR gives `StatementContext` exactly one accessor
per alternative, defined on the class rather than inherited):

```
table rows: 49 | grammar alternatives: 49 | mismatches: ()
distinct verbs: 47
```

49 rows collapse to 47 verbs because `execCicsStatement`, `execSqlStatement`
and `execSqlImsStatement` all report verb `EXEC`, with the product carried in
the hit's `context` — matching token-scan rule 5 so the two methods count the
same thing.

**The guard fired during development, which is the evidence it works.** A first
version enumerated the *non*-statement rules by hand to exclude them; the check
immediately reported 29 rules ending in `Statement` with no row
(`ModeStatementContext`, `AddToStatementContext`, `GoToDependingOnStatementContext`, …).
Those are sub-clauses, not statements. Rather than lengthen the exclusion list,
the check was rewritten to read the alternatives off `StatementContext`, where
no such list is possible.

**Two-word verbs.** `PERFORM VARYING` (supported) and out-of-line `PERFORM`
(not) are one grammar rule with an optional tail, so the distinction exists only
in the tree. `_qualifier()` returns the statement's second token so `analyze()`
can look up the qualified key — the same rule the token scan applies to
`next_tok`. This was measured wrong first: an initial version used
`getText()`, which concatenates a subtree with no separators, so
`PERFORM VARYING I FROM 1` came back as `VARYINGIFROM1` and failed the
`isalpha()` test. Two genuinely-supported `PERFORM VARYING`s were being counted
unsupported (P02 18/19, P07 32/33) until it was fixed to read the sub-rule's
first *token*.

#### `ScannedSource.antlr_source()` — verified, and two indicators were wrong

The work package asked for this to be verified rather than assumed. Verified,
and it was **not** correct as it stood:

| Indicator | Before | After |
|---|---|---|
| columns 1–6 | dropped ✓ | dropped |
| column 73+ | dropped ✓ | dropped |
| `*` / `/` | blanked, line number preserved ✓ | unchanged |
| **`D` / `d`** | **treated as ordinary code** ✗ | blanked as a comment — a debugging line compiles only under `WITH DEBUGGING MODE`, which this pre-pass does not assume |
| **`-`** | **not handled** ✗ — the continuation's code area was emitted as its own line, splitting the word or literal it continues | appended to the line it continues, with an empty line left in its place so line numbers still point at where the statement starts |

The code area is now taken **unstripped** for the ANTLR feed, because trailing
spaces inside a continued literal are part of that literal; the previously
`rstrip`ped `code` field is unchanged for every other consumer.

Census across all five corpora, so the fix is sized rather than guessed:

```
find <corpus> \( -iname '*.cbl' -o -iname '*.cob' -o -iname '*.cpy' \) | xargs awk '...'
→ bench/corpus      cont(-)=0   debug(D)=0  comment(*,/)=75
  examples/cobol    cont(-)=0   debug(D)=0  comment(*,/)=56
  gnucobol          cont(-)=0   debug(D)=0  comment(*,/)=205
  carddemo          cont(-)=11  debug(D)=0  comment(*,/)=4895
```

Continuation appears 11 times, in CardDemo only; no `D` line appears anywhere.
Both are fixed regardless — the pre-pass is a correctness surface, not a
best-effort one.

#### Entry rule

`_antlr_parse` now enters at `startRule` (`compilationUnit EOF`), the grammar's
declared entry point, not at `compilationUnit`. Entering at `compilationUnit`
lets a file whose tail the grammar cannot parse report **zero errors** over its
prefix and be graded VERIFIED on a partial tree. `src/parsers/cobol.py` was
changed the same way.

### §6. Acceptance gate

#### Criterion 1–3 — the seven bench programs

```
PYTHONPATH=. /tmp/pin-venv/bin/python /tmp/wp20/measure.py bench/corpus
```

(The script mirrors `src/assessment/cli.py`'s loop, because
`antlr_syntax_errors` reaches the provenance string only when
`method == "token_scan"` and the table must record it either way — the same
method §0.2 used.)

| program | errs before | **errs after** | method before | **after** | grade before | **after** | ratio before | **after** | sup/tot before | **after** |
|---|---|---|---|---|---|---|---|---|---|---|
| P01_payroll | 11 | **0** | token_scan | **antlr_tree** | PLAUSIBLE | **VERIFIED** | 1.0 | **1.0** | 25/25 | **25/25** |
| P02_interest | 8 | **0** | token_scan | **antlr_tree** | PLAUSIBLE | **VERIFIED** | 1.0 | **1.0** | 19/19 | **19/19** |
| P03_eligibility | 4 | **0** | token_scan | **antlr_tree** | PLAUSIBLE | **VERIFIED** | 1.0 | **1.0** | 24/24 | **24/24** |
| P04_taxtable | 8 | **0** | token_scan | **antlr_tree** | PLAUSIBLE | **VERIFIED** | 1.0 | **1.0** | 33/33 | **34/34** |
| P05_validate | 2 | **0** | token_scan | **antlr_tree** | PLAUSIBLE | **VERIFIED** | 1.0 | **1.0** | 25/25 | **25/25** |
| P06_valinit | 5 | **0** | token_scan | **antlr_tree** | PLAUSIBLE | **VERIFIED** | 1.0 | **1.0** | 14/14 | **14/14** |
| P07_exitflow | 1 | **0** | token_scan | **antlr_tree** | PLAUSIBLE | **VERIFIED** | 1.0 | **1.0** | 33/33 | **33/33** |
| **total** | **39** | **0** | | | | | | | 173 | **174** |

The "before" column was re-measured in this run at `4ecfcc7`, not cited: it
reproduced §0.2's `11/8/4/8/2/5/1` exactly before anything was changed.

**All three criteria met.** Zero syntax errors on all seven; `antlr_tree` and
VERIFIED on all seven; ratio 1.0000 throughout.

**Granularity change, recorded as required:** P04 moves 33 → 34 statements. The
one extra statement is isolated by differencing the two methods' hit lists on
that program rather than inferred:

```
ONLY in antlr_tree : [(54, 'MOVE')]
ONLY in token_scan : []
bench/corpus/P04_taxtable/program.cbl:54 →  AT END MOVE 5 TO WS-IDX
```

It is the `MOVE` in the `SEARCH`'s `AT END` phrase. The token scan skips it
because `AT` is not a statement-start context — its documented under-count
(rule 4) — and the tree is right: that is a real MOVE statement. The same
class of difference, on a different verb, appears in `FULLSUP.cbl`
(18 → 19, `AT END DISPLAY WS-A`). Everywhere else the two methods agree
exactly, which is a real cross-validation: the scan and the tree were written
independently.

**Correction:** an earlier draft of this entry attributed P04's extra statement
to `AT END DISPLAY`, carrying `FULLSUP.cbl`'s example across without measuring
P04. The mechanism is the same; the verb is `MOVE`, at P04 line 54, as the
difference above shows.

Determinism (R8), and input identity:

```
python -m src.assessment.cli bench/corpus --out /tmp/wp20/det{1,2} --json-only
sha256sum /tmp/wp20/det{1,2}/assessment.json
→ 4229b450332b9c871eda43c90a179b5bd500d7cc474992f677a1ebc0c8527866  (both)
manifest_hash before → cc4513ba7feda336a554556ed8e638e99ec7144b064294cfc92681564d54bb90
manifest_hash after  → cc4513ba7feda336a554556ed8e638e99ec7144b064294cfc92681564d54bb90
```

Identical manifest hash means the input tree is byte-identical, so every delta
above is attributable to the grammar swap alone.

#### Criterion 4 — `test_cross_check.py`

**Green and unchanged.** `git status --porcelain tests/assessment/test_cross_check.py`
→ no output. Its ten skips are the same ten `EXPECTED_SKIPS` gates on; the skip
*reasons* moved (fixtures that now parse report "coverage is not 1.0" rather
than a parse failure) but the count did not.

#### Criterion 5 — `supported` counts across all five dry runs

**Unchanged on all five, verified by content hash rather than by eye.**
Appendix E of each report is the SUPPORTED set read from the transpiler; the
only difference in any of the five is the git-ref *label*:

```
diff <(awk '/^### Appendix E/,/^### Appendix F/' docs/dryruns/<run>/assessment.md) …
→ Registry: `SUPPORTED_STATEMENTS@5fcbba7 (c1_rulebased.py sha256:a440ac2751bb738d)`
  Registry: `SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d)`
  (no other line differs, in any of the five)
```

The label moved `5fcbba7` → `4ecfcc7`; the **content hash `a440ac2751bb738d` did
not**. `transpiler/c1_rulebased.py` is untouched, so the supported set could not
have moved — and now it is measured, not argued.

**And the same question asked of the counts, not only the set**, because
"supported" has two readings and only one of them was covered above. The
`supported_statements` numerator per dry run, read from each report's
`portfolio_coverage` at `4ecfcc7` and at this commit:

```
git show <ref>:docs/dryruns/<run>/assessment.json | jq .portfolio_coverage
```

| dry run | supported before → after | Δ | total before → after | Δ | ratio |
|---|---|---|---|---|---|
| `bench_corpus` | 173 → **174** | **+1** | 173 → 174 | +1 | 1.0 → 1.0 |
| `examples_cobol` | 64 → 64 | 0 | 110 → 110 | 0 | 0.5818 → 0.5818 |
| `aws_carddemo` | 7058 → 7058 | 0 | 9738 → 9738 | 0 | 0.7248 → 0.7248 |
| `omp_cobol_course` | 405 → 405 | 0 | 766 → 766 | 0 | 0.5287 → 0.5287 |
| `gnucobol` | 239 → 239 | 0 | 439 → 439 | 0 | 0.5444 → 0.5444 |
| **all five** | 7939 → **7940** | **+1** | | | |

**The supported COUNT moved by exactly +1, in one program, and it is the same
statement as the granularity change in §6** — the `MOVE` at
`P04_taxtable/program.cbl:54`. Numerator and denominator both rose by one, so
every ratio is unchanged to four decimal places. Stated plainly rather than
buried:

* The supported **set** did not move. `transpiler/c1_rulebased.py` is
  byte-identical across the two commits
  (`sha256 a440ac2751bb738d…`, `git diff 4ecfcc7 HEAD -- transpiler/ bench/`
  → empty), and the registry is the same 21 keys.
* The supported **count** is `|statements recovered ∩ registry|`. It is a
  function of the registry *and* of how many statements the analyzer recovers.
  The grammar swap changed recovery by one statement, and that statement's verb
  (`MOVE`) is in the registry, so the count followed.

So this is not the failure mode the criterion guards against — no capability
claim widened, no verb became supported that was not supported before. It is
one previously-missed real statement becoming visible, on the correct side of
a registry that did not change. Downstream figures for `bench_corpus` confirm
nothing else moved: `quotable_loc` 384 → 384, `grammar_expansion_loc` 0 → 0,
`unsupported_inventory` 0 → 0, portfolio risk tier LOW → LOW (7 programs at
LOW, both times).

**This is an operator call, not one this entry makes.** The criterion says any
movement in `supported` blocks merge; the movement here is +1 and fully
accounted for.

> **AMENDED, WP-2.0.1 (2026-08-20).** The criterion quoted in the paragraph
> above — *"any movement in supported counts is a bug and blocks merge"* — was
> wrong, and it is what produced the false merge-block on PR #25 that the
> paragraph is arguing against. It is superseded by the wording in the WP-2.0.1
> entry, §3 of this log. In short: **the gate is the supported SET** — the
> `transpiler/c1_rulebased.py` sha256 and the registry key list must be
> identical — and count movement is permitted when it is attributable to
> statements already in that set, with a program, line number and verb given.
> The measurements in this section are unchanged and were already sufficient to
> clear the amended gate: sha256 `a440ac2751bb738d…` identical, registry the
> same 21 keys, and the +1 attributed to the `MOVE` at
> `P04_taxtable/program.cbl:54`.

Portfolio figures, all five re-run on byte-identical inputs (all five manifest
hashes match):

| Run | Coverage before → after | Grade before → after | Risk | Programs on the tree path | Unsupported inventory before → after |
|---|---|---|---|---|---|
| `bench_corpus` | 1.0000 → **1.0000** (173/173 → 174/174) | PLAUSIBLE → **VERIFIED** | LOW → LOW | 0 → **7 of 7** | 0 → 0 |
| `examples_cobol` | 0.5818 → 0.5818 (64/110) | PLAUSIBLE → PLAUSIBLE | BLOCKED | 0 of 1 | 46 → 46 |
| `aws_carddemo` | 0.7248 → 0.7248 (7058/9738) | PLAUSIBLE → PLAUSIBLE | BLOCKED | 0 → **2 of 44** | 2680 → 2680 |
| `omp_cobol_course` | 0.5287 → 0.5287 (405/766) | PLAUSIBLE → PLAUSIBLE | BLOCKED | 0 → **5 of 30** | 361 → 361 |
| `gnucobol` | 0.5444 → 0.5444 (239/439) | PLAUSIBLE → PLAUSIBLE | BLOCKED | 0 of 6 | 200 → 200 |

Seven third-party programs are promoted to VERIFIED, and **their statement
counts are identical under both methods** — where the scan and the tree can both
read a program, they agree. The committed artifacts under `docs/dryruns/` were
refreshed from these runs, and `docs/dryruns/README.md` was corrected: it
asserted "not one real-world program parsed cleanly", which the swap has made
false.

#### Criterion 6 — regeneration byte-identical, lint

`tools/regen_parser.sh --check` → exit 0, with teeth proven in §4.

**Lint, stated precisely rather than as "green".** Neither workflow runs a
linter, and the repository is **not** `black`-formatted: at `4ecfcc7`,
`black --check --line-length 100 src/ tests/` reports **49 files would be
reformatted, 9 left unchanged**. After this work package the figure is
**identical — 49 and 9**. This WP therefore adds no lint debt, but "lint green"
would be a false claim about this repository and is not made. Reformatting 49
files was out of scope and would have buried the diff.

### §7. Real-world scope — the criterion as written could NOT be met

The work package required at least one of `examples_cobol` / `gnucobol` to reach
zero syntax errors. **Neither does, and neither can**, for reasons measured
here and outside this WP's reach. Per-file counts, as required:

```
PYTHONPATH=. /tmp/pin-venv/bin/python /tmp/wp20/perfile.py <root>
```

**`examples_cobol`** — 1 file, 1 → 1 errors:

| file | errs before | errs after | first error |
|---|---|---|---|
| `banking-system.cbl` | 1 | 1 | `line 3:8 mismatched input 'RELIAN-DEMO' expecting <EOF>` |

**`gnucobol`** — 7 files, 301 errors (50 is the collector's cap):

| file | errs after | first error |
|---|---|---|
| `NEWS` | 50 | `line 1:0 mismatched input 'NEWS'` |
| `cobc/ChangeLog` | 50 | `line 2:0 mismatched input '2026-06-08'` |
| `tests/ChangeLog` | 50 | `line 2:0 mismatched input '2025-12-04'` |
| `extras/CBL_OC_DUMP.cob` | 50 | `line 36:32 mismatched input 'binary-long'` |
| `tests/testsuite.src/numeric-display.cob` | 50 | `line 22:17 token recognition error at: '@'` |
| `tests/testsuite.src/numeric-dump.cob` | 50 | `line 23:17 token recognition error at: '@'` |
| `tests/testsuite.src/tutorial.cob` | 1 | `line 10:0 mismatched input 'SET' expecting {ID, IDENTIFICATION}` |

Why, per corpus:

* **`gnucobol` is not COBOL-85 source.** It is a compiler's own test suite:
  `BINARY-LONG` is a GnuCOBOL usage, `@` is its test-macro syntax, and
  `tutorial.cob` opens with a compiler directive before the IDENTIFICATION
  DIVISION. A COBOL-85 grammar rejecting these is correct behaviour. Four of
  the seven "programs" are `NEWS` and `ChangeLog` text files that intake
  classifies as programs — a pre-existing intake issue, not a grammar one.
* **`examples_cobol` needs two things, and only one of them is the
  preprocessor.** The single error is the comment entry after `AUTHOR.`, which
  is reachable only through a `*>CE` marker that upstream's preprocessor
  inserts.

**The work package asked to be told if preprocessor integration was small
enough to include. It was measured instead of estimated, and the answer is that
it would not be sufficient — so there is nothing to ask for.** A throwaway
pre-pass tagging comment entries with `*>CE` was applied to
`banking-system.cbl` (in `/tmp`, not committed):

```
PYTHONPATH=. /tmp/pin-venv/bin/python /tmp/wp20/ce.py examples/cobol/banking-system.cbl
→ errors=3
    line 288:12 no viable alternative at input 'EXIT PERFORM\n            NOT'
```

Errors go **1 → 3**, not 1 → 0: tagging lets the parser reach further into the
file, where it meets `EXIT PERFORM` at line 288. That is COBOL-2002; the
vendored grammar's rule is `exitStatement: EXIT PROGRAM?`. No preprocessor work
fixes it — only a post-85 dialect would.

**What did reach zero**, recorded because the criterion's intent was
real-world evidence: **7 third-party programs** across `aws_carddemo` (2) and
`omp_cobol_course` (5) now parse cleanly and grade VERIFIED, against **zero**
before this WP. `omp_cobol_course` was not in the named pair but is the corpus
that best demonstrates the criterion's intent, at 5 of 30 clean.

**This criterion is recorded as NOT MET rather than reinterpreted.** It is the
one acceptance item in this entry that fails.

### §8. Suite

```
PYTHONPATH=. /tmp/pin-venv/bin/python -m pytest -q -rs --no-cov
→ 286 passed, 10 skipped in 86.70s
```

**286 passed, 10 skipped, 0 failed.** Net **+1** against the sealed 285. Five
tests changed, and every one of them was pinning the *old grammar's failure*
rather than behaviour worth preserving:

| Test | Was | Now | Why |
|---|---|---|---|
| `test_coverage.py::test_token_scan_path_is_used_when_the_parse_has_errors` | asserted `PARTIAL.cbl` → `token_scan` | repointed at `COPYUSER.cbl` | PARTIAL.cbl now parses. COPYUSER.cbl still cannot: `COPY` is a lexer token no parser rule references. |
| `test_coverage.py::test_previously_unparseable_fixture_now_reaches_the_tree` | — | **new** | pins PARTIAL.cbl's promotion to `antlr_tree`/VERIFIED, so the swap's effect cannot silently regress |
| `test_coverage.py::test_hits_carry_file_line_and_paragraph` | verb `"GO"` | verb `"GO TO"` | the tree reports the two-word verb the dispatch table is keyed by |
| `test_coverage.py::test_provenance_names_the_registry_and_the_file` | `method=token_scan` | `method=antlr_tree` | same fixture, now parsed |
| `test_determinism.py::test_parse_errors_do_not_drift…` | used `PARTIAL.cbl` | uses `BROKEN.cbl` | needs a fixture that still *produces* an `expecting {…}` set to normalise |
| `test_loc.py::test_logical_is_statements_not_periods` | `logical == 18` | `logical == 19` | the tree counts `AT END DISPLAY`'s DISPLAY; 18 was the scan's under-count |

`tests/parsers/test_cobol.py` keeps its four tests. Its fixture was split: the
COPY was moved into a dedicated fixture, because a `COPY` in the PROCEDURE
DIVISION aborts the parse there and everything after it — including `CALC-PARA`
— is never seen. The COPY test now also asserts the edge is stamped
`recovered_by="token_scan"`.

`.github/workflows/tests.yml` and `tests/conftest.py` were updated 285 → 286.
`EXPECTED_SKIPS` is unchanged at 10 and still gates.

**Public bench split, as a regression check** (the transpiler was not touched,
so this should not have moved, and did not):

```
run_candidate('current', …, split='public')
→ build 7/7, vectors 89/89, BER 1.0
   P01 12/12  P02 12/12  P03 12/12  P04 17/17  P05 12/12  P06 12/12  P07 12/12
```

### §9. Second consumer, re-mapped: `src/parsers/cobol.py`

`COBOLParser` bound to the old grammar's contexts and failed at import
(`Cobol85Parser has no attribute 'SectionContext'`). Re-mapped: `dataDivision`
reaches sections through `dataDivisionSection`, `procedureDivision` through
`procedureDivisionBody`, a data item's name and clauses live on
`dataDescriptionEntryFormat1`, and `visitSection` became
`visitProcedureSection`.

`visitCopyStatement` was **deleted, not ported** — there is no `copyStatement`
rule to visit. Dropping the dependency edge silently would have been the wrong
answer, so `COPY <name>` targets are recovered from the **token stream** and
every node produced that way carries `recovered_by="token_scan"`, so a consumer
can tell a scanned edge from a parsed one. It does not resolve `REPLACING` and
does not know whether the copybook exists; both are preprocessor work.

`src/assessment/coverage.py`'s module docstring is reproduced **verbatim** as
Appendix D of every shipped report (`report.py:438`). It described the old
reduced grammar and, after the swap, asserted things that are false ("reports
syntax errors on 5 of 5 programs"). Rewritten, and four ANTLR-tree counting
rules (7–10) added alongside the existing token-scan rules.

### §10. Verified before pushing

```
git status --porcelain bench/ transpiler/
→ (no output)
```

Nothing under `bench/corpus/`, `bench/harness/` or `transpiler/` was modified.

### §11. What this entry did not resolve

- **Acceptance criterion 5 is NOT MET** (§7). Neither named corpus can reach
  zero under a COBOL-85 grammar; the blockers are dialect constructs, not
  grammar defects. Operator's call whether to accept the 7-clean-programs
  evidence, retarget the criterion at `omp_cobol_course`, or defer.
- **Preprocessor integration remains out of scope**, and §7 shows it would not
  have satisfied criterion 5 anyway. `Cobol85Preprocessor.g4` is vendored with
  full provenance but not generated; `COPY`, `REPLACE` and comment entries all
  wait on it.
- **gnucobol intake misclassification.** `NEWS` and two `ChangeLog` files are
  classified as COBOL programs. Pre-existing, unrelated to this WP, not fixed
  here. **UNRESOLVED.**
- **The repository is not `black`-formatted** (49 files, unchanged by this WP).
  No linter runs in CI. **UNRESOLVED — operator's call.**

---

## 2026-08-20 · WP-2.0.1 · Housekeeping — CI trigger, `numpy`, criterion wording

- **HEAD at start:** `3f5b885` (`Merge pull request #25 from
  khaaliswooden-max/claude/wp-2-0-cobol-grammar-n2sre1`), verified equal to
  `origin/main` — `git rev-list --left-right --count origin/main...HEAD` →
  `0	0`.
- **Branch:** `claude/focused-pasteur-2lrp50`
- **Scope guard:** nothing under `bench/` or `transpiler/` was modified.
  `git status --porcelain bench/ transpiler/` → no output, at close-out (§5).
- **Nature of the work package:** three items carried as residuals by earlier
  entries. No transform-path behaviour changes; the supported set is untouched.

### §1. The unit-test gate could not run on a feature branch

`.github/workflows/tests.yml` read:

```yaml
on:
  push:
    branches: [main]
  pull_request:
    branches: [main]
```

while `.github/workflows/bench.yml` reads, on its first line, `on: [push,
pull_request]`. That is the wrong way round in both directions at once. The
cheap gate — a pytest run — was restricted to `main`, so a push to a work-package
branch produced **no suite result at all**; the only way to see one was to open a
pull request. The expensive gate — held-out scoring, which compiles seven Java
candidates and runs them against the sealed corpus — was unrestricted and ran on
every push to every branch.

The cost is recorded rather than asserted: WP-2.0.−3 (PR #24), WP-2.0 (PR #25)
and this work package all had to open a pull request before any CI-measured
suite result existed for the branch. Three consecutive work packages is the
measurement.

`push:` is now unrestricted; `pull_request:` stays scoped to `main`, which is
what the merge gate is about. Parsed rather than eyeballed:

```
python3 -c "import yaml; print(yaml.safe_load(open('.github/workflows/tests.yml'))[True])"
→ {'push': None, 'pull_request': {'branches': ['main']}}
```

(`True` because YAML 1.1 resolves the bare key `on` to a boolean — the reason
this is read back through a parser rather than by eye.)

`bench.yml` was **not** changed. Narrowing the held-out job is a benchmark-policy
decision, not housekeeping, and it is out of this work package's brief.
**UNRESOLVED — operator's call:** the expensive job still runs on every push.

### §2. `numpy` removed — direct and transitive, both measured

WP-2.0.−3 §4 left `numpy` declared and marked the disposition **UNRESOLVED**.
Its two consumers, `src/ml/risk_scorer.py` and
`src/intelligence/migration_intelligence.py`, were deleted under R1 in that same
work package. Re-measured here rather than taken from that note.

Direct import sites, repository-wide:

```
grep -rEn "^\s*(import|from)\s+numpy\b" --include="*.py" .
→ (no output)
grep -rEn "\bnumpy\b|\bnp\.[a-z]" --include="*.py" .
→ (no output)
```

The second pattern is there because the first would miss a function-body
`import numpy as np` written with leading whitespace inside a `try:` — the exact
shape `migration_intelligence.py:363` used before it was deleted.

**The transitive question, which is the one that actually decides this.** A
distribution with no import site can still be load-bearing if something else in
the closure requires it, so the check is the resolver's, not a grep's: regenerate
the lock and see whether `numpy` survives.

```
uv pip compile pyproject.toml --extra dev --generate-hashes \
    --python-version 3.12 --universal --no-annotate \
    --output-file requirements.lock
```

| | Before | After |
|---|---|---|
| pinned distributions | 38 | **37** |
| `numpy` pinned | `numpy==2.5.2` | **absent** |

```
diff <(grep -oE '^[a-z0-9][a-z0-9._-]*==[^ ]*' <old lock> | sort) \
     <(grep -oE '^[a-z0-9][a-z0-9._-]*==[^ ]*' requirements.lock | sort)
→ 21d20
  < numpy==2.5.2
```

**One line differs, and it is the removal.** No other pin changed version. Had
anything in `demo/`, `bench/harness/`, `transpiler/` or `src/` been reaching
`numpy` through another distribution, the resolver would have kept it in the
closure; it did not, so nothing does.

Install verified in a fresh CPython 3.12 venv, including the CI invariant that
`pip install -e ".[dev]"` resolves nothing new after the lock:

```
pip install -r requirements.lock   → Successfully installed <35 distributions>
pip install -e ".[dev]"            → Successfully installed relian-0.1.0   (alone)
pip check                          → No broken requirements found.
pip freeze --exclude-editable | sort | sha256sum
→ d5dc6dd01f5f003fb82e4797813912c9dbb2ec934a1fa840f8aaf13ea3ae3a3b
pip freeze --exclude-editable | wc -l → 36
```

**37 pinned, 36 installed, and the difference is not drift.** `colorama==0.4.6`
carries the marker `sys_platform == 'win32'` and does not install on Linux;
`--universal` keeps markers rather than resolving them away, which is what the
lock's own header says it is for.

### §3. The WP-2.0 supported-count criterion, amended

The criterion, as written for WP-2.0, was:

> any movement in supported counts is a bug and blocks merge

**It is replaced, in full, by:**

> **The gate is the supported SET.** Two things must be identical across the
> commits being compared: the sha256 of `transpiler/c1_rulebased.py`, and the
> registry key list (`SUPPORTED_STATEMENTS`). Either one moving is a capability
> claim and blocks merge.
>
> **Movement in the supported COUNT is permitted** when it is attributable to
> statements already in that set — that is, statements whose verb was supported
> both before and after. Such movement must be reported with a **program, a line
> number, and a verb** for each attributed statement. Count movement that cannot
> be attributed that way is unexplained and blocks merge exactly as a set change
> would.

Measured at this commit, as the values the gate reads:

```
sha256sum transpiler/c1_rulebased.py
→ a440ac2751bb738da259c641ac8a5771c2f94a179852c97225fd9427c5a3e703
python -c "from src.assessment.supported import SUPPORTED_STATEMENTS, registry_provenance; …"
→ SUPPORTED_STATEMENTS@3f5b885 (c1_rulebased.py sha256:a440ac2751bb738d)
   21 keys: ACCEPT ADD COMPUTE CONTINUE DISPLAY ELSE END-EVALUATE END-IF
            END-PERFORM EVALUATE EXIT PROGRAM GOBACK IF INSPECT MOVE
            PERFORM VARYING SEARCH SET STOP UNSTRING WHEN
```

#### Why the original wording was wrong

It gated on the wrong quantity. The supported **count** is not a property of the
transpiler at all — it is
`|statements the analyzer recovers ∩ registry|`, a function of **two** inputs.
The registry is the capability claim; statement recovery is the parser's reach
over a given corpus. The criterion was written to stop the first from widening
silently, but it was expressed in terms of a number that also moves whenever the
second changes, with the capability claim untouched.

The failure this produced is on the record. WP-2.0 replaced the reduced COBOL
grammar with ProLeap COBOL-85. `transpiler/c1_rulebased.py` was byte-identical
across the swap (`a440ac2751bb738d…` both sides) and the registry was the same
21 keys, so no capability widened by so much as one verb. But the new grammar
recovered **one statement the old one had missed** — the `MOVE` at
`P04_taxtable/program.cbl:54` — and `MOVE` is in the registry, so the count went
`173 → 174` on `bench_corpus` and `7939 → 7940` across all five dry runs.
Numerator and denominator both rose by one; every coverage ratio was unchanged
to four decimal places.

Under the original wording that was a merge-block. It should not have been: it
is the grammar swap doing precisely what it was commissioned to do, observed
through a registry that did not move. The WP-2.0 entry had to escalate it as an
operator call (§6, Criterion 5) and PR #25 stalled on it. **The criterion, not
the change, was the defect** — a gate that fires on correct behaviour trains its
readers to wave it through, which is the more expensive failure of the two.

The amended wording keeps the whole of the original's protective intent. A verb
becoming supported that was not supported before still requires a registry key
or a change to `c1_rulebased.py`, and both are gated identically and by content
hash rather than by eye. What it no longer does is confuse *seeing more of the
customer's code* with *claiming to handle more of it*. The attribution
requirement — program, line, verb — is what keeps that from becoming a loophole:
an unattributed count movement blocks merge, so "the parser must have found
something" is not an answer.

**Amendment recorded in place**, as a marked block under WP-2.0 §6 Criterion 5.
The measurements in that entry are untouched; this log is append-only and the
figures it recorded were correct. It is the criterion they were judged against
that changed, and by the amended wording WP-2.0 clears it on evidence that entry
had already measured.

**Stated elsewhere:** nowhere. The criterion is not encoded in any test or
workflow — `grep -rn "supported count" .` returns one line, in this log, and
`tests/assessment/test_supported.py` gates the registry-to-transpiler
correspondence, not counts. Nothing else needed amending.

### §4. Suite result

Run in a reconstructed pin (this container starts on 3.11 with no `cobc`):

| Item | Measured value |
|---|---|
| Interpreter | `Python 3.12.3 (main, Mar  3 2026, 12:15:18) [GCC 13.3.0]` |
| GnuCOBOL | `cobc (GnuCOBOL) 3.1.2.0` |
| Java compiler | `javac 21.0.10` |

```
pytest -q -rs --junitxml=junit.xml
→ 286 passed, 10 skipped in 306.89s (0:05:06)
```

**`286 passed, 10 skipped, 0 failed`** — the sealed triple, unchanged by this
work package, which is the expected result: nothing here touches a code path the
suite exercises. The ten skips are the same ten fixture-shape skips
`EXPECTED_SKIPS` gates on, listed by `-rs` and unchanged from WP-2.0 §6
Criterion 4. No demo test skipped, so `cobc` and `javac` were both present and
the differential comparisons actually ran.

### §5. CI — both workflows green on the runner

Pushed as `8315822`. **This is the first `tests` run in the repository's history
triggered by a push to a work-package branch** — every previous feature-branch
`tests` run was `event: pull_request`, which is the defect §1 describes, and the
run below is the fix demonstrating itself.

| Workflow | Run | Event | Conclusion |
|---|---|---|---|
| `tests` (run #15) | https://github.com/khaaliswooden-max/relian/actions/runs/32413435224 | **push** | **success** |
| `RELIAN-BENCH scoring` (run #139) | https://github.com/khaaliswooden-max/relian/actions/runs/32413435247 | push | **success** |

`tests` — both jobs green. `parser is byte-identical to its grammar` passed
(`tools/regen_parser.sh --check`, exit 0), and the `pytest` job's gate step
printed, from the JUnit report rather than from pytest's own summary:

```
--- measured environment ---
  python_version: 3.12.14
  cobc_version: cobc (GnuCOBOL) 3.1.2.0
  javac_version: javac 21.0.12
  toolchain_complete: yes
--- result ---
  286 passed, 10 skipped, 0 failed, 0 errored
GATE MET: 286 passed, 10 skipped (expected 10), 0 failed, 0 errored
```

**`286 passed, 10 skipped, 0 failed` on the runner, agreeing with §4's local
figure**, and `toolchain_complete: yes` means the ten skips are the fixture-shape
skips rather than demo tests guarding themselves off. The two environments are
not byte-identical — CPython 3.12.14 / javac 21.0.12 on the runner against
3.12.3 / 21.0.10 locally, the patch drift the `java-version: '21'` caveat in the
WP-2.0.−1 entry §11 predicts — and the triple is the same on both.

`RELIAN-BENCH scoring` — green, including `Score candidate on HELD-OUT split`
(98s) and the ledger-signature verification that precedes it. The scored figures
are in that run's `bench_summary.json` artifact and are **deliberately not
copied into this log**: held-out scoring is CI-only under R3, and nothing this
work package touches could have moved them — `transpiler/` and `bench/` are
byte-identical to `main`.

```
git status --porcelain bench/ transpiler/
→ (no output)
git diff --stat origin/main HEAD -- bench/ transpiler/
→ (no output)
```

### §6. What this entry did not resolve

- **`bench.yml` still runs held-out scoring on every push to every branch**
  (§1). Narrowing it is benchmark policy, not housekeeping. **UNRESOLVED —
  operator's call.**
- The three residuals inherited from WP-2.0 §11 — gnucobol intake
  misclassification, the repository not being `black`-formatted, and preprocessor
  integration — are untouched here and **remain UNRESOLVED**.

---

## 2026-08-20 · WP-2.0.2 · Assert the whole triple, prove the seal, narrow the bench trigger

Three things, all of them about the difference between a check that runs and a
check that *bites*: the unit-test gate asserted two of its three numbers; the
bench gate proved the manifest was intact but never that the tree still matched
it; and `bench.yml` scored the held-out split on every push to every branch.

### §1. The pass count was prose — planted red, then corrected

`tests.yml` asserted `EXPECTED_SKIPS` mechanically. The pass count it only
checked for `passed > 0`, and the file said so outright: *"the pass count is
prose, kept accurate so a reader can tell at a glance whether a run drifted."*

That leaves the largest of the three numbers unguarded, and unguarded in the
direction that matters. A test that stops being **collected** — a file renamed
out of `python_files`, an ImportError absorbed at collection time, a
parametrisation that stops expanding, a module dropped from `testpaths` — lowers
the pass count while leaving skips at 10 and failures at 0. Every assertion in
the gate is satisfied. The tick is green. Part of the suite has stopped running
and nothing says so.

`EXPECTED_PASSES` now sits beside `EXPECTED_SKIPS`, is asserted in the same
script block, and carries the same comment discipline: change it only alongside
this file, with the delta attributed test by test. Its failure message states
that FEWER passes usually means a test stopped being collected.

#### Proven on the runner, not by argument

Commit `7617fe4` set `EXPECTED_PASSES: "285"` — one below the measured 286 —
and changed nothing else. **No test was deleted or deselected to produce the
red**: a deliberately wrong expectation is the same proof without touching the
suite, and touching the suite to prove a suite gate works is exactly the kind of
circularity this repository is supposed to avoid.

| Commit | `EXPECTED_PASSES` | Run | `tests` conclusion |
|---|---|---|---|
| `7617fe4` | `285` (deliberately wrong) | https://github.com/khaaliswooden-max/relian/actions/runs/32421480377 | **failure** — planted red |
| `ea92098` | `286` (measured) | https://github.com/khaaliswooden-max/relian/actions/runs/32421999807 | **success** |

The planted-red run, from the gate step's own output:

```
--- measured environment ---
  python_version: 3.12.14
  cobc_version: cobc (GnuCOBOL) 3.1.2.0
  javac_version: javac 21.0.12
  toolchain_complete: yes
--- result ---
  286 passed, 10 skipped, 0 failed, 0 errored
GATE FAILED: pass count drifted: 286, expected 285. FEWER passes usually means
a test STOPPED BEING COLLECTED -- a renamed file that no longer matches
`python_files`, an ImportError absorbed at collection, a parametrisation that
stopped expanding -- which lowers this number while leaving skips and failures
untouched, and is exactly the drift this assertion exists to catch. [...]
##[error]Process completed with exit code 1.
```

Read the step statuses rather than only the job conclusion: **`pytest` step
`success`, `Assert the suite result` step `failure`.** Skips at 10, failures at
0, errors at 0 throughout. The job went red on the pass count and on nothing
else, which is precisely the drift that used to pass this gate in silence.

### §2. `tools/verify_manifest.py` — the tree, not just the manifest

`bench.yml` verifies the Ed25519 signature over the v1.2 ledger. That proves
**the manifest was not edited**. It does not prove **the tree still matches the
manifest**. Those are different claims, and only the second catches a silent
edit under `bench/`: change one byte in `corpus/P03_eligibility/program.cbl` and
the signature still verifies perfectly, because it covers the manifest's bytes
rather than the corpus's.

`tools/verify_manifest.py` checks three layers independently and reports which
one failed:

| Layer | Claim | What only it catches |
|---|---|---|
| `TREE` | every `files[]` path exists at `root/<path>` and hashes to its recorded `sha256`; and the include set is re-walked so nothing on disk is missing from `files[]` | a corpus byte changed; a file **added** since sealing |
| `PAYLOAD` | `payload_sha256` recomputed from `files[]` equals the recorded value | a **consistently** edited `files[]` — attacker changes the file *and* updates its recorded hash, satisfying `TREE` |
| `SIGNATURE` | Ed25519 verifies over `manifest_hash()` — the manifest minus its `signature` key | any edit to the manifest body |

The reverse walk is not decoration. Hash-checking only the recorded entries
would happily pass a tree with an extra file in it, and "every recorded file is
correct" is a weaker claim than "the tree is the tree that was sealed".

#### Measured against v1.2, in CI

```
--include-dirs corpus,harness --include-files SPEC.md
--expect-absent '**/vectors/heldout.jsonl,harness/gen_vectors.py'
--key-fingerprint 233bb4406e2de606

LAYER 1/3  TREE       recorded 29 · verified 21 · declared absent 8
                      hash mismatches 0 · missing 0 · unrecorded on disk 0   PASS
LAYER 2/3  PAYLOAD    a8695c2c… recomputed == recorded                       PASS
LAYER 3/3  SIGNATURE  a47305c2… recomputed == recorded, key 233bb4406e2de606 PASS
VERDICT: PASS (3/3 layers)
```

Verbatim from the `bench tree matches its sealed manifest` job on `a5b4efc`
(run 32422281011), including the step that follows it:

```
LAYER 1/3  TREE -- recorded files exist and hash to their recorded sha256
    recorded                    : 29
    verified                    : 21
    hash_mismatches             : 0
    missing                     : 0
    declared_absent             : 8
    declared_absent_but_present : 0
    unreadable                  : 0
    walked_on_disk              : 21
    unrecorded_on_disk          : 0
    declared absent (NOT verified): corpus/P01_payroll/vectors/heldout.jsonl
    [... six more heldout.jsonl, then harness/gen_vectors.py ...]
    RESULT: PASS

LAYER 2/3  PAYLOAD -- payload_sha256 recomputed from files[]
    recorded                    : a8695c2cbb39c15204f1dd2a7c98751c4290cff5be29f3574e89c48d4dcb48be
    recomputed                  : a8695c2cbb39c15204f1dd2a7c98751c4290cff5be29f3574e89c48d4dcb48be
    RESULT: PASS

LAYER 3/3  SIGNATURE -- Ed25519 signature over manifest_hash()
    alg                         : Ed25519
    recorded manifest_sha256    : a47305c29bfeb004f4fb4812a7f94097038e4cc0d06cbb4e5859b920662ce156
    recomputed manifest_sha256  : a47305c29bfeb004f4fb4812a7f94097038e4cc0d06cbb4e5859b920662ce156
    key fingerprint             : 233bb4406e2de606
    signature verifies          : True
    RESULT: PASS

VERDICT: PASS (3/3 layers)

--- git status --short -- bench/ ---
```

The last line is the rule-4 check, and it printed nothing: **nothing under
`bench/` was modified on the runner.**

**Correcting the brief's expected figure, which said "29 files, zero
mismatches".** Zero mismatches is right, and so is zero unrecorded files. But
the manifest records 29 files and **only 21 of them can be hash-verified inside
this repository**. The other eight are absent *by binding rule*, not by drift:

- the seven `corpus/P*/vectors/heldout.jsonl` — scoring-only and CI-only
  (rule 1 / R3);
- `harness/gen_vectors.py` — generator plus seed regenerate the held-out set
  (rule 6).

Both are in `.gitignore` for exactly those reasons. **This is not the escalation
condition and was not escalated**: no layer failed, no path mismatched, nothing
under `bench/` changed. But it is not verification either, so the tool will not
call it that. Declared-absent paths must be named on the command line via
`--expect-absent`, are reported as `declared absent (NOT verified)`, are never
folded into the `verified` count, and are not read (R1: an unavailable file is
not a checked one). If one of them ever *does* appear on disk, that is itself a
reported finding rather than a green tick.

#### The v1.2 manifest format does not record its own include/exclude rules

The manifest records `files[]`, `payload_sha256` and a signature. It does not
record `INCLUDE_DIRS`, `INCLUDE_FILES`, `EXCLUDE_SUFFIX`, `EXCLUDE_NAMES` or
`EXCLUDE_BINARIES`. Those live only in `bench/harness/commit.py`, which is
**outside the signature's protection of its own semantics** — the file is
hashed as content, but the rules it encodes are not attested as the rules that
produced `files[]`.

A verifier therefore cannot re-walk the tree without being told them, which is
why they are CLI arguments here. This is a real weakness of the format and is
worth stating rather than working around silently: an attacker who can also edit
`commit.py` changes what the *next* seal covers. The tool's defaults are
transcribed from v1.2's sealer so the common invocation stays short, but the
module docstring says plainly that they are defaults, not attestations.

`test_dropping_the_binary_exclusion_surfaces_them_as_unrecorded` pins the
consequence: drop `--exclude-basenames` and the five committed COBOL binaries
(`payroll01` and four `run` files) surface as unrecorded, because the sealer
excluded them and the manifest never said so.

#### A second limitation, in the signature itself

`bench/harness/commit.py`'s `verify()` — and layer 3 without a pin — verifies
using the public key **embedded in the manifest being checked**. That proves
self-consistency, not authorship: an attacker who edits the manifest and
re-signs it with a key they generated passes. `--key-fingerprint` pins the
expected signer, and CI passes `233bb4406e2de606`, so forging the ledger now
also requires editing `tests.yml` — visible in the diff.
`test_key_fingerprint_pin_rejects_an_otherwise_valid_resigned_manifest` proves
both halves: unpinned, the re-signed forgery verifies; pinned, it does not.

#### Deliberately not sealed, and importing nothing from the repo

The verifier lives in `tools/` and is **not** part of the sealed set. A verifier
sealed inside the artifact it verifies is circular — it would attest to its own
integrity with the key it is checking. A third party must be able to read it,
run it, or discard it and write their own from the docstring.

It imports nothing from this repository, in particular not
`bench/harness/commit.py`, whose hashing it reimplements from the format
description. A verifier that calls the sealer's own code cannot detect a bug in
the sealer's own code. Two tests enforce this against the **parsed imports**,
not a grep, so the docstring may discuss `commit.py` while the code stays clear
of it: `test_verifier_imports_nothing_from_the_repository` (imports ⊆ stdlib +
`cryptography`) and `test_verifier_does_not_manipulate_sys_path`.

#### Planted red without touching `bench/` — all three layers, independently

`tests/test_verify_manifest.py` copies `bench/` into `tmp_path`, mutates one
byte **there**, and asserts the verifier returns False. Nothing under `bench/`
is written at any point.

| Test | Mutation | `TREE` | `PAYLOAD` | `SIGNATURE` |
|---|---|---|---|---|
| `test_corpus_mutation_fails_the_tree_layer_and_only_that_layer` | one byte in `corpus/P03_eligibility/program.cbl` | **FAIL** | pass | pass |
| `test_consistently_edited_files_entry_fails_only_the_payload_layer` | same byte, `files[]` hash updated to match, manifest re-signed | pass | **FAIL** | pass |
| `test_signature_mutation_fails_only_the_signature_layer` | one hex nibble in `signature_hex` | pass | pass | **FAIL** |

Each layer is shown to have teeth **on its own**: in every row the other two
layers pass, so no layer's red can be attributed to another's. The middle row is
the sharp one — the attacker does the job properly, updates the recorded hash so
`TREE` is satisfied and re-signs so `SIGNATURE` is satisfied, and
`payload_sha256` is the only thing left standing. That is the reason the field
exists.

The re-signing helper generates an **ephemeral** `Ed25519PrivateKey` in memory
inside the test. It is never written anywhere and is not the benchmark signing
key, whose custody is the operator's and which never enters this repository
(R4). It exists to simulate the attacker, which is what makes the pin above
meaningful.

Twenty-four tests in total (§4 attributes them):

```
git status --short -- bench/
→ (no output)
git status --porcelain -- bench/
→ (no output)
```

The `bench-seal` CI job re-runs `git status --short -- bench/` after the
verification and fails if it is non-empty — rule 4 asserted by the runner, not
by assurance.

#### Why a job rather than a step inside `pytest`

`bench-seal` is a separate job for the reason `parser-regen` is one, and the
file already argues it: *"a stale parser is a provenance failure, and it should
be legible as that rather than as a test failure."* A benchmark tree that has
drifted from its seal is the same kind of failure. Keeping it separate also
means a red suite never suppresses this result and a drifted tree never
suppresses the suite's.

#### `cryptography` entered the pinned closure

The verifier's only third-party import, and it was not in `requirements.lock`.
It joins the `dev` extra — verifying a benchmark seal is a CI and third-party
audit activity, not part of the transform path a customer perimeter installs, so
it is deliberately **not** in `[project.dependencies]`.

Regenerated with the documented command, then diffed line by line rather than
assumed. **37 pinned distributions before, 40 after.** The three additions are
`cryptography==50.0.0` and its two transitive dependencies `cffi` and
`pycparser`, both carrying the non-PyPy environment markers `--universal`
preserves. **No other pin changed version.**

### §3. `bench.yml` narrowed — and the reason on the record

`bench.yml` read `on: [push, pull_request]`. Every push to every branch fetched
the private held-out vectors and scored against them. It is now:

```yaml
on:
  push:
    branches: [main]
  pull_request:
    branches: [main]
  workflow_dispatch:
```

**The reason is not runner minutes.** Recording it wrongly is how this gets
reverted in six months by whoever wants faster feedback, so the reason is in the
workflow file itself as well as here:

> Held-out scoring on every push turns the held-out set into a dev set. A score
> that comes back on every commit is a feedback channel, and a feedback channel
> is something you can iterate against one commit at a time **without ever
> intending to**. Nudge a rounding mode, push, read the BER; nudge it back,
> push, read it again. Nobody has to look at a single held-out vector for the
> held-out set to stop measuring generalisation — the scalar leaks the
> information one bit at a time, and after enough commits the transpiler has
> been fitted to vectors it was never allowed to see.
>
> That is Goodhart's law arriving through CI configuration rather than through
> anybody's intent, and it is the exact failure R3 and the pre-commit sealing
> ordering exist to prevent. Those controls govern who may **read** the vectors;
> this one governs how often anyone may **ask** them a question. Both are
> needed, because a sufficiently high query rate defeats the read restriction on
> its own.

This was live, not hypothetical: `RELIAN-BENCH scoring` run #143 scored the
held-out split on the branch push of `7617fe4` — a commit that changed one
integer in a workflow file and could not possibly have moved a BER.

`workflow_dispatch` is kept deliberately. On-demand re-verification without a
push is what the WP-2.1 key session will need, and removing the push trigger
without it would have replaced one problem with an inability to re-verify at
all.

**The cost, stated rather than glossed:** a branch that breaks the bench harness
now surfaces at PR time instead of push time. That is a real loss of feedback
speed for harness work, and it is accepted — an intact held-out set is worth more
than an earlier red tick, and `workflow_dispatch` buys the feedback back for
anyone who wants it on a specific commit.

This closes the item WP-2.0.1 §6 left as **UNRESOLVED — operator's call**.

### §4. The suite triple: 286 → 310, attributed test by test

`EXPECTED_PASSES` moves to **310** in the same commit that adds the tests.
Collection went 296 → 320; the skip count is untouched at 10. All twenty-four
additions are in `tests/test_verify_manifest.py`; **no existing test changed,
was renamed, or stopped being collected.**

| # | Test | What it pins |
|---|---|---|
| 1 | `test_sealed_v12_ledger_verifies_on_all_three_layers` | the green path: 3/3 layers on the real v1.2 ledger |
| 2 | `test_v12_census_is_21_verified_8_declared_absent_0_mismatched` | the census, and that verified + declared-absent = recorded |
| 3 | `test_pinning_the_real_signer_fingerprint_still_passes` | the pin does not break the green path |
| 4 | `test_corpus_mutation_fails_the_tree_layer_and_only_that_layer` | `TREE` teeth, isolated |
| 5 | `test_file_added_to_the_tree_is_caught_even_though_every_hash_matches` | the reverse walk |
| 6 | `test_deleted_recorded_file_is_caught` | an undeclared absence is a failure |
| 7 | `test_declared_absent_path_that_is_present_is_reported_not_hashed` | a held-out file appearing here is a finding, and is not read |
| 8 | `test_file_count_disagreeing_with_files_length_fails_the_tree_layer` | the manifest's claim about itself |
| 9 | `test_consistently_edited_files_entry_fails_only_the_payload_layer` | `PAYLOAD` teeth, isolated, against a re-signed forgery |
| 10 | `test_payload_recomputation_agrees_with_the_sealed_value` | the independent reimplementation matches the sealer |
| 11 | `test_signature_mutation_fails_only_the_signature_layer` | `SIGNATURE` teeth, isolated |
| 12 | `test_manifest_body_edit_is_caught_by_the_signature_layer` | a lowered threshold is caught |
| 13 | `test_key_fingerprint_pin_rejects_an_otherwise_valid_resigned_manifest` | unpinned a forgery verifies; pinned it does not |
| 14 | `test_manifest_hash_recomputation_agrees_with_the_sealed_value` | `manifest_hash()` reimplementation matches |
| 15 | `test_excluded_build_artifacts_are_not_reported_as_unrecorded` | the sealer's binary exclusions |
| 16 | `test_dropping_the_binary_exclusion_surfaces_them_as_unrecorded` | the exclusion is load-bearing; the walk really walks |
| 17 | `test_absent_ledger_is_fatal_and_never_a_pass` | R2 — unmeasured is not a pass |
| 18 | `test_unparseable_ledger_is_fatal_and_never_a_pass` | R2 |
| 19 | `test_absent_root_is_fatal_and_never_a_pass` | R2 |
| 20 | `test_cli_exits_zero_on_the_sealed_ledger` | the exact invocation CI runs |
| 21 | `test_cli_exits_one_and_names_the_failed_layer` | exit status and the failing path in the output |
| 22 | `test_cli_json_output_is_machine_readable` | `--json` |
| 23 | `test_verifier_imports_nothing_from_the_repository` | imports ⊆ stdlib + `cryptography`, checked by AST |
| 24 | `test_verifier_does_not_manipulate_sys_path` | no `sys.path` mutation, checked by AST |

Local, without `cobc` (which is why the local skip count is 20 rather than 10 —
ten `cobc`-gated demo tests guard themselves off):

```
pytest -q -rs
→ 300 passed, 20 skipped in 87.67s
```

300 + the ten demo tests that run only where `cobc` exists = the **310** the
runner measures.

### §5. CI — both workflows green on the branch

| Workflow | Commit | Run | Conclusion |
|---|---|---|---|
| Workflow | Commit | Run | Conclusion |
|---|---|---|---|
| `tests` #19 | `7617fe4` | https://github.com/khaaliswooden-max/relian/actions/runs/32421480377 | **failure** — the planted red of §1 |
| `tests` #20 | `ea92098` | https://github.com/khaaliswooden-max/relian/actions/runs/32421999807 | **success** — 286/10/0 |
| `tests` #21 | `a5b4efc` | https://github.com/khaaliswooden-max/relian/actions/runs/32422281011 | **success** — 310/10/0, all three jobs |
| `RELIAN-BENCH scoring` #143 | `7617fe4` | https://github.com/khaaliswooden-max/relian/actions/runs/32421480561 | success |
| `RELIAN-BENCH scoring` #144 | `ea92098` | https://github.com/khaaliswooden-max/relian/actions/runs/32421999744 | success |
| `RELIAN-BENCH scoring` | `c66ef8f`, `a5b4efc` | — | **did not run** |

The last row is §3 taking effect, observable rather than argued. Runs #143 and
#144 fetched the private held-out vectors and scored against them on branch
pushes of commits that changed, respectively, one integer in a workflow file
and a workflow comment. After `a5b4efc` narrowed the trigger, the same kind of
push produced no bench run at all. Held-out scoring now happens on `main`, on
PRs targeting `main`, and on demand — not on every commit anyone writes.

All three `tests` jobs on `a5b4efc`:

| Job | Duration | Conclusion |
|---|---|---|
| `parser is byte-identical to its grammar` | 8s | **success** |
| `bench tree matches its sealed manifest` | 19s | **success** — §2's output above |
| `pytest (CPython 3.12, GnuCOBOL + JDK)` | 4m57s | **success** |

The gate step, from the JUnit report rather than from pytest's own summary:

```
--- measured environment ---
  python_version: 3.12.14
  cobc_version: cobc (GnuCOBOL) 3.1.2.0
  javac_version: javac 21.0.12
  toolchain_complete: yes
--- result ---
  310 passed, 10 skipped, 0 failed, 0 errored
GATE MET: 310 passed (expected 310), 10 skipped (expected 10), 0 failed, 0 errored
```

**`310 passed, 10 skipped, 0 failed` on the runner**, both counts now asserted
rather than one asserted and one narrated. `toolchain_complete: yes` means the
ten skips are the fixture-shape skips of §4 rather than demo tests guarding
themselves off, so the differential comparisons actually ran.

#### A measurement caveat about how these were read

The per-job Actions API endpoint served a stale `in_progress` snapshot of the
`pytest` job on `a5b4efc` for roughly twenty-five minutes after that job had in
fact completed, at 22:06:10. Polling `actions/jobs/<id>` through that window
returned a step list frozen at `pytest: in_progress`, including well past the
job's own `timeout-minutes: 30` — a convincing but false runner-loss signature,
since that timeout is enforced by the runner. The run-level endpoint and
`list_workflow_jobs` both had the correct `completed / success` throughout.

Recorded because it is a trap for anyone reading CI state through the API
rather than the web UI: **one endpoint's `in_progress` is not evidence that a
job is running.** Cross-check against the run or the job list before concluding
anything about a job that appears stuck. Commit `b22f6da` reported a runner
loss on this job on exactly that basis. That was wrong — the run was green —
and this section supersedes it.

### §6. What this entry did not resolve

- **The v1.2 manifest format still does not record its own include/exclude
  rules** (§2). `verify_manifest.py` works around it with CLI arguments and says
  so; fixing it properly means a v1.3 manifest that records the walk rules
  inside the signature. **UNRESOLVED — needs a re-seal, operator's call.**
- **`bench/harness/commit.py`'s `verify()` still trusts the embedded public
  key** (§2). `verify_manifest.py` offers `--key-fingerprint` and CI uses it,
  but the sealer's own `verify()` — the one `bench.yml` calls — does not pin.
  Changing it means editing `bench/`, which rule 4 forbids without a new signed
  version. **UNRESOLVED — operator's call.**
- The three residuals inherited from WP-2.0 §11 — gnucobol intake
  misclassification, the repository not being `black`-formatted, and
  preprocessor integration — are untouched here and **remain UNRESOLVED**.

---

## 2026-08-20 · WP-2.1 · Build RELIAN-DISCOVERY-BENCH v0.1 (unsealed)

- **HEAD at start:** `2b01022` (merge of PR #27, WP-2.0.2)
- **Branch:** `claude/relian-discovery-bench-v0.1-oen5te`
- **Scope guard:** nothing under `bench/` modified — confirmed at close-out with
  `git status --short -- bench/` (§8). `src/discovery/` was not created —
  `git log --diff-filter=A -- src/discovery/` returns nothing (§8).
- **Baseline:** `310 passed, 10 skipped, 0 failed`, re-measured in this session
  before any change (§1).
- **Not done, deliberately:** no signing, no tagging, no `src/discovery/`. The
  sealing ceremony is operator-only (R4) and R7 puts the bench before the
  engine. This work package lands the corpus, harness, oracle, sealer and tests
  **unsigned**.

---

### 1. Toolchain, recorded before anything else

Every offset in this work package is one compiler's byte layout, so the
compiler was identified before a line was written.

```
python3 --version   → Python 3.11.15
cobc --version      → cobc (GnuCOBOL) 3.1.2.0        [first line]
dpkg -l gnucobol3   → ii  gnucobol3  3.1.2-5.1ubuntu1  amd64
javac -version      → javac 21.0.10
java -version       → openjdk version "21.0.10" 2026-01-20
git --version       → git version 2.43.0
```

`cobc` was **not present at session start** and was installed with the same
unpinned command CI uses (`apt-get install -y gnucobol`, T16). It resolved to
`3.1.2.0`, which equals T3 **including the patch level**, so the escalation
trigger ("if your `cobc` is not 3.1.2.0, stop") did not fire.

`javac` is 21.0.10 — one of the three patch levels already observed under a
major-only pin (T4), not a fourth.

The authoring interpreter is 3.11.15 while the CI gate pins 3.12. Recorded as a
deviation rather than glossed: nothing in the harness is version-sensitive, and
CI regenerating the oracle byte-identically on 3.12 is the check on that claim
rather than this sentence.

Baseline re-measured in this session, not quoted from the WP:

```
python3 -m pytest -q -rs
→ 310 passed, 10 skipped in 137.36s
```

---

### 2. `tools/seal.py` — written fresh, five differences, each tested

Not copied from and not importing `bench/harness/commit.py`. `commit.py` is
**inside the v1.2 manifest** (`bench/harness/` is an include dir), so any edit
to it — a comment included — changes its sha256, then `payload_sha256`, then
`manifest_sha256`, then invalidates the signature. Rule 4 forbids the edit
independently.

The two posix conventions are replicated verbatim with a comment citing
`commit.py` as origin and stating why they are duplicated rather than shared:
sharing would mean importing from inside the sealed tree.
`test_both_conventions_match_commit_py_on_the_same_tree` reimplements
`commit.py`'s ordering and hashing independently and asserts `seal.py` produces
the identical entry list, so a drift in either file goes red.

| # | Difference | Proof |
|---|---|---|
| 1 | Absent key file raises; **no generation fallback** | `test_absent_key_raises_and_never_generates` + the planted red `test_commit_py_is_the_behaviour_being_corrected`, which executes `commit.sign()` into `tmp_path`, asserts a PEM **was** written, asserts `commit.verify()` returns `True` on it, and asserts the resulting fingerprint is not `233bb4406e2de606` |
| 2 | Any `UNAVAILABLE` toolchain probe raises | `test_unavailable_toolchain_probe_refuses_to_sign`, plus `test_the_v1_2_ledger_still_carries_the_unavailable_this_prevents`, which reads `"javac": "UNAVAILABLE"` out of the signed v1.2 ledger rather than recalling it |
| 3 | Name, version, tag, include-set, output path are parameters | `test_identity_and_output_path_are_parameters`, `test_include_set_is_a_parameter_and_exclusions_apply`, `test_config_file_round_trips` |
| 4 | Manifest records `include_rules`, `expected_absent`, `oracle_toolchain`, `corpus_counts` | `test_manifest_records_its_own_rules_absences_toolchain_and_counts`, plus `test_v1_2_records_none_of_them_which_is_why_difference_4_exists` |
| 5 | Verification pins `233bb4406e2de606` rather than trusting the embedded key | `test_seal_verify_requires_and_honours_the_pin`, `test_seal_refuses_to_write_a_manifest_signed_by_an_unpinned_key`, and the forgery test in §3 |

**Configuration form chosen: a TOML file (`discovery-bench/seal.toml`), with CLI
flags able to override.** Stated because the WP left it to the implementer. The
include-set and `expected_absent` are part of what gets *signed*, so they should
be reviewable in a diff and identical between the ceremony and CI, rather than
retyped on a command line where a dropped `--include-dir` silently narrows what
the seal covers. `seal.toml` is itself inside the include set, so what the seal
covered is recoverable from the sealed tree. The CLI overrides exist for tests
and one-off checks, not for the ceremony.

One property found while rehearsing and then pinned: `verify()`'s
`expected_fingerprint` default is bound at **definition** time, so reassigning
`seal.EXPECTED_KEY_FINGERPRINT` does not move it. An importer cannot quietly
widen what counts as the published signer — it has to pass a different value at
the call site, where a reviewer sees it.
`test_the_pin_cannot_be_rebound_by_an_importer_at_runtime` stops a later
refactor from giving that away.

---

### 3. `tools/verify_manifest.py` — `--from-manifest` and `--pin-fingerprint`

`--from-manifest` reads `include_rules` and `expected_absent` out of the signed
payload, so verification needs no argument beyond `--ledger`; `--root` defaults
to the ledger's own directory. It **refuses** on a manifest that does not
self-describe rather than falling back to the transcribed v1.2 defaults —
falling back would let the flag report a walk the signer never attested to.
v1.2's lack of self-description is recorded as a format limitation, and its
existing CLI path is unchanged.

Regression, run before and after the change:

```
python3 tools/verify_manifest.py --ledger bench/LEDGER_relian-bench-v1.2.json \
    --root bench --include-dirs corpus,harness --include-files SPEC.md \
    --expect-absent '**/vectors/heldout.jsonl,harness/gen_vectors.py' \
    --pin-fingerprint 233bb4406e2de606
→ VERDICT: PASS (3/3 layers)
  recorded 29, verified 21, declared_absent 8, mismatches 0
```

`--pin-fingerprint` is the WP's spelling; `--key-fingerprint` is the spelling
the existing CI step already uses. Same `dest`, so both work and neither breaks
the other — `test_pin_fingerprint_and_key_fingerprint_are_the_same_flag`.

**The forgery test** — `test_resigned_forgery_passes_three_layers_and_fails_the_pin`
— is the one the WP asked for specifically. It seals a tree honestly, verifies
it, then edits a corpus file, **re-seals it consistently**, and signs with an
ephemeral attacker key. It asserts, in this order:

- layer 1 TREE **passes** (the tree matches the re-written `files[]`);
- layer 2 PAYLOAD **passes** (`payload_sha256` matches the re-written `files[]`);
- layer 3 SIGNATURE **passes** unpinned (the embedded key is the attacker's);
- the overall verdict is **PASS** without a pin;
- with `--pin-fingerprint` set to the honest signer, the run fails, and
  `failed_layers() == ["signature"]` — only the pin catches it.

No real key is touched anywhere in this file. Every test that needs one
generates an ephemeral key in `tmp_path` and pins against *its* fingerprint.

---

### 4. The corpus — 15 copybooks

Three seeded by copying `examples/demo/copy/{MUBBREC,MUBCONS,MUBCUST}.cpy`
verbatim; byte-identity is asserted **in both directions** so a change to either
copy goes red (D10). Twelve authored, one axis each, headers naming the axis and
stating the file is synthetic and not derived from any customer or benchmark
source. `examples/demo/src/MUBPOST.cbl` was read first; D07 and D08 are anchored
to its inline ODO and REDEFINES.

All fifteen compile clean under Route A and inside a generated probe — sweep run
before the harness was finished, `0 errors` and `0 warnings` on every file.

Two authoring facts were discovered by measurement rather than assumed:

- **An item may not follow an ODO table in the same group.** First draft of
  `D07_odo.cpy` had a trailer field and cobc rejected it:
  `error: 'D07-ENTRY' cannot have OCCURS DEPENDING because of '<name>'`. The
  copybook was fixed before sealing, which is where a copybook defect belongs.
- **`SYNCHRONIZED` does align in GnuCOBOL 3.1.2.** An early scratch test
  suggested it did not; that test happened to be naturally aligned already.
  `D10_sync.cpy` declares 20 bytes of fields and occupies 25 — five slack bytes,
  at offsets 2 (3 bytes), 10 (1) and 16 (1). The copybook's header was rewritten
  to state what was measured rather than what was first believed. **No
  divergence-from-standard claim survives; there was nothing to escalate here.**

---

### 5. `gen_probe.py`, `oracle_layouts.py`, and one deviation from the WP

Routes run in the fixed order the WP specifies. Route A
(`cobc -x -t sym.lst -ftsymbols -I<dir>`) gives per-field SIZE and the group
total; the probe cannot be emitted without that total, because it sizes the
byte window. Offsets are 1-based. `FUNCTION HEX-OF` is never emitted, asserted
per copybook by `test_every_probe_compiles_and_runs_under_the_pinned_cobc`.

**Deviation, and the measurement that forced it.** WP §3.3 specifies
`MOVE HIGH-VALUES TO <field>`. Measured on GnuCOBOL 3.1.2.0, that form is
unsound for the edited axis:

```
MOVE HIGH-VALUES TO <PIC ZZZZZZZZ9.99>   → first 0, last 0, marked 0
MOVE HIGH-VALUES TO <PIC ZZZ9 BLANK …>   → first 0, last 0, marked 0
MOVE HIGH-VALUES TO <PIC XXBXX>          → first 13, last 17, marked 4 of 5
```

It compiles, runs, exits zero and writes **no high-value bytes at all** on a
numeric-edited item. A probe built on it would have recorded
`offset: 0, length: 0` for every numeric-edited field in `D12_edited.cpy` **and
for `BPR-AMOUNT-DUE` in the seeded `MUBBREC.cpy`**, whose real layout is offset
46, length 12. Under D8/R1 that is a fabricated zero wearing the costume of a
measurement — worse than a null.

The probe therefore fills through `MOVE HIGH-VALUES TO <field> (1:)`, which
reference-modifies the item and makes the receiving field alphanumeric of its
full length regardless of category. Exact for all fifteen copybooks.
`test_the_naive_probe_form_silently_measures_nothing_on_a_numeric_edited_item`
compiles and runs both forms side by side and asserts `NAIVE == 0` and
`REFMOD == 12`, so if GnuCOBOL ever changes this the deviation's justification
changes with it, visibly.

Consequence for T13, recorded rather than dropped: `-Warchaic` fires on the
plain form and **not** on the reference-modified form, and it is **not enabled
by default** — a default compile of the plain form reports `0 warnings`. The
harness passes neither `-Warchaic` nor `-Werror`, suppresses nothing globally,
and treats no warning as failure.

**Second deviation, also forced.** The WP's `REDEFINES … OCCURS total_len TIMES
PIC X(01)` byte table is used for fourteen copybooks and is impossible for the
fifteenth: `error: the original definition 'D07-ODO' cannot be variable length`.
`D07_odo.cpy` reads bytes through reference modification on the group instead.
The two windows are proved equivalent, not assumed:
`test_both_byte_windows_agree_on_a_fixed_copybook` probes `D05_occurs.cpy` under
both and asserts every row identical.

D9's conditioned invariant is asserted per copybook and independently per ODO
variant; aliases are asserted separately. `gap` spans live in their own array,
never in `fields`. Level-88 rows carry `offset: null, length: null` and appear
in no field row.

---

### 6. Suite delta: 310 → 370 (+60), attributed test by test

```
python3 -m pytest -q -rs
→ 369 passed, 10 skipped in 148.53s      [before the +1 in §2's last paragraph]
python3 -m pytest tests/test_seal.py tests/test_bench_oracle.py -q
→ 60 passed in 16.64s                    [28 + 32]
```

Final expected triple: **370 passed, 10 skipped, 0 failed**. `EXPECTED_PASSES`
moved `310 → 370` in the same commit. No existing test was changed, renamed,
deleted or deselected; the entire delta is two new files.

**The skip count is unchanged at 10, and that is deliberate rather than lucky.**
`tests/test_bench_oracle.py` guards its cobc-dependent tests with a `skipif` so
a developer without GnuCOBOL still gets the JSON half of the suite — which would
be green-by-skip in CI. `RELIAN_REQUIRE_COBC=1` in the pytest job disarms the
guard, so on the runner those tests must run; a runner that lost `cobc` fails
rather than skips.

#### `tests/test_seal.py` (+28)

| Test | Pins |
|---|---|
| `test_absent_key_raises_and_never_generates` | difference 1 |
| `test_commit_py_is_the_behaviour_being_corrected` | difference 1 (planted red against `commit.py`) |
| `test_unavailable_toolchain_probe_refuses_to_sign` | difference 2 |
| `test_the_v1_2_ledger_still_carries_the_unavailable_this_prevents` | difference 2's precedent |
| `test_a_complete_toolchain_is_accepted` | difference 2, the passing side |
| `test_identity_and_output_path_are_parameters` | difference 3 |
| `test_include_set_is_a_parameter_and_exclusions_apply` | difference 3 |
| `test_an_empty_include_set_refuses_rather_than_sealing_nothing` | difference 3, refusal |
| `test_config_file_round_trips` | difference 3, TOML form |
| `test_manifest_records_its_own_rules_absences_toolchain_and_counts` | difference 4 |
| `test_v1_2_records_none_of_them_which_is_why_difference_4_exists` | difference 4's justification |
| `test_missing_oracle_refuses_rather_than_recording_zero` | R1 on `corpus_counts` |
| `test_from_manifest_reproduces_the_argument_mode_result_exactly` | D12 |
| `test_from_manifest_refuses_a_manifest_that_does_not_self_describe` | v1.2 not retrofitted |
| `test_a_file_on_disk_but_absent_from_files_fails` | reverse-walk detection |
| `test_expected_absent_must_be_empty_for_the_discovery_bench` | gate ⑤ |
| `test_the_discovery_seal_config_is_buildable_and_records_what_it_should` | the real config, dry-run |
| `test_seal_verify_requires_and_honours_the_pin` | difference 5 |
| `test_seal_refuses_to_write_a_manifest_signed_by_an_unpinned_key` | difference 5 |
| `test_the_pin_cannot_be_rebound_by_an_importer_at_runtime` | difference 5, found while rehearsing |
| `test_resigned_forgery_passes_three_layers_and_fails_the_pin` | **the hole pinning exists to close** |
| `test_pin_fingerprint_and_key_fingerprint_are_the_same_flag` | CLI compatibility |
| `test_manifest_paths_are_posix_even_for_nested_files` | posix convention #1 |
| `test_entries_are_sorted_by_the_posix_string_not_the_path_object` | posix convention #2 |
| `test_both_conventions_match_commit_py_on_the_same_tree` | the two sealers still agree |
| `test_the_manifest_is_deterministic_across_two_runs` | determinism |
| `test_the_published_fingerprint_is_the_one_this_repo_already_trusts` | D14, one custody chain |
| `test_seal_contains_no_key_generation_or_serialisation_call` | R4, asserted on the parsed AST |

#### `tests/test_bench_oracle.py` (+32)

| Test | Pins |
|---|---|
| `test_the_corpus_is_exactly_fifteen_copybooks` | gate ① |
| `test_seed_copies_are_byte_identical_in_both_directions` | D10 |
| `test_every_authored_copybook_declares_its_axis_and_that_it_is_synthetic` | in-band provenance |
| `test_no_copybook_line_exceeds_column_72` | fixed-format truncation |
| `test_committed_oracle_is_serialised_deterministically` | sorted keys, one trailing newline |
| `test_every_number_in_the_oracle_names_its_source` | **gate ④** |
| `test_the_lint_actually_bites` | gate ④'s gate |
| `test_offsets_are_one_based_and_match_the_measurement_in_the_wp` | T14 |
| `test_mubcust_reproduces_the_flat_cross_check_from_the_wp` | T15 |
| `test_every_field_row_carries_a_probe_source_and_gaps_are_kept_apart` | D8 |
| `test_conditions_carry_nulls_and_appear_in_no_field_row` | R1, no fabricated zero |
| `test_the_d9_tiling_invariant_holds_for_every_copybook_and_variant` | **D9** |
| `test_sum_of_tiling_fields_and_gaps_equals_the_route_a_group_size` | zero tolerance |
| `test_aliases_are_asserted_separately_from_the_tiling` | D9's separate half |
| `test_odo_copybook_records_both_lengths_and_they_differ` | min and max both recorded |
| `test_every_other_copybook_is_fixed_length_and_uses_the_redefines_window` | window selection |
| `test_the_sync_axis_recovered_five_slack_bytes_as_gaps` | the SYNC measurement |
| `test_filler_is_recovered_as_gap_and_never_as_a_field` | FILLER is not probeable |
| `test_the_numeric_edited_field_the_naive_probe_would_have_zeroed` | §5's deviation, on real data |
| `test_the_counts_block_matches_the_rows_it_summarises` | gate ① |
| `test_route_a_and_route_b_agreed_on_every_copybook_with_zero_tolerance` | **gate ③** |
| `test_spec_states_the_same_integers_the_oracle_does` | gate ①, SPEC vs oracle |
| `test_the_oracle_records_the_toolchain_that_produced_it` | provenance |
| `test_recorded_copybook_hashes_match_the_files_on_disk` | oracle vs corpus |
| `test_the_committed_oracle_is_byte_identical_to_a_fresh_regeneration` | **gate ②**, local half |
| `test_regeneration_is_byte_identical_across_two_consecutive_runs` | gate ②, determinism half |
| `test_every_probe_compiles_and_runs_under_the_pinned_cobc` | every copybook compiles in a probe; no `HEX-OF` |
| `test_both_byte_windows_agree_on_a_fixed_copybook` | the ODO fallback is equivalent |
| `test_a_variable_length_group_cannot_be_redefined` | why there are two windows |
| `test_the_naive_probe_form_silently_measures_nothing_on_a_numeric_edited_item` | **the planted red behind §5** |
| `test_require_cobc_env_turns_a_missing_toolchain_into_a_failure` | green-by-skip closed |
| `test_the_runner_cobc_matches_the_oracle_toolchain_recorded_in_the_seal` | gate ⑥ |

---

### 7. CI

Three changes to `.github/workflows/tests.yml`. `bench.yml` is untouched.

1. **`bench-seal` gains a discovery-ledger step.** **Choice made and stated: the
   step is gated on the ledger file's existence and skips with a named reason,
   rather than being commented out behind a `TODO`.** A commented-out step is
   invisible in the checks list; this one appears on every run and says which
   state it is in. The moment the operator commits the ledger the same step
   starts enforcing with no workflow edit, so turning the gate on cannot be the
   thing somebody forgets. A `TODO(WP-2.1-seal)` marker records the one
   post-ceremony change worth making: invert the condition so a *deleted* ledger
   fails instead of skipping. It runs `--from-manifest --pin-fingerprint`, and
   asserts `verified == recorded` and `declared_absent == 0` (gate ⑤).
2. **New `oracle-regen` job** — regenerates `oracle.json` on a clean runner and
   diffs byte-for-byte (gate ②, the `parser-regen` pattern), then confirms the
   job wrote nothing into `discovery-bench/`. Sets `RELIAN_REQUIRE_COBC=1`.
   Includes the **`cobc` version assertion** (gate ⑥): the runner's
   `cobc --version` must equal the oracle's recorded `toolchain.cobc`, and —
   once the ledger exists — the signed `oracle_toolchain.cobc` too, so the
   assertion is against what the signer attested to rather than a string in the
   workflow. Patch level, not major.
3. **`pytest` job gains `RELIAN_REQUIRE_COBC: "1"`** and `EXPECTED_PASSES`
   moves `310 → 370`.

---

### 8. Scope confirmations

```
git status --short -- bench/
→ (empty)

git log --diff-filter=A -- src/discovery/
→ (empty)

ls src/discovery 2>&1
→ No such file or directory
```

Nothing under `bench/` was modified. `src/discovery/` was never created; R7
puts the benchmark and its answer key before the engine, and the tag does not
exist yet.

---

### 9. Open, and handed back

- **The ceremony has not run.** `discovery-bench/LEDGER_relian-discovery-bench-v0.1.json`
  does not exist and no tag was created. Rehearsed end-to-end in a scratch copy
  with an **ephemeral** key: 21 recorded, 21 verified, 0 declared absent, 0
  mismatches, 3/3 layers. The real key was not touched.
- **`expected_absent` is empty and must stay empty.** An entry appearing there
  is an escalation, not a declaration.
- **The IBM-may-differ caveat is unmeasured (◐).** Everything in §8 of
  `discovery-bench/SPEC.md` is GnuCOBOL 3.1.2 behaviour. Whether IBM Enterprise
  COBOL agrees — particularly on SYNC slack, which depends on `binary-size` —
  has not been measured here and belongs in the customer report as a stated
  limitation of a GnuCOBOL-derived oracle.
- **D13's publication step is an operator call.** The fingerprint
  `233bb4406e2de606` needs to reach the Technical Delivery Sheet and every
  attestation deliverable alongside the verification command; re-derivability is
  only a real claim if the third party knows which key to expect. That is a
  quotable-capability matrix change (R11) and is not made here.

---

## WP-2.2 — copybook resolver and layout engine (2026-08-21)

Anchored at `f720cca`, tag `relian-discovery-bench-v0.1`. `discovery-bench/` is
frozen under `CLAUDE.md` rule 4 from that tag forward: read, never written. This
work package creates `src/discovery/`, which did not exist when the benchmark
was sealed. That ordering is R7 and it is now a fact in the git history rather
than a claim in a document.

### 1. The hole this package exists to not have

The oracle **is** GnuCOBOL 3.1.2.0's byte layout, measured by compiling probe
programs. If the layout engine shelled out to `cobc`, the round-trip would be
100% by construction: the oracle would be grading itself, every capability claim
built on that number would be circular, and — the part that matters — the
circularity would be invisible in a green result.

So `src/discovery/` never invokes a compiler, and
`tests/test_discovery_is_compiler_free.py` asserts it by walking the parsed
syntax tree rather than grepping, because `__import__("sub" + "process")`
defeats a grep and `"co" + "bc"` defeats the other one. Four independent
assertions: the package's own AST, the transitive first-party import closure,
the round-trip harness itself (it reads the committed `oracle.json`; re-deriving
it inside the grading step would reintroduce the dependency through the back
door), and a runtime probe that computes a real layout in an interpreter where
`subprocess`, `ctypes` and `pty` are unimportable.

It is also a shipping constraint. The product runs in the customer perimeter
(R12) on machines that have no GnuCOBOL and whose COBOL is IBM Enterprise. An
engine that needs a compiler is not shippable.

**Planted red, run once and reverted.** `import subprocess` plus
`subprocess.run(["cobc", "--version"])` added to `src/discovery/layout.py`:

```
FAILED test_module_never_invokes_a_compiler[layout.py]
    src/discovery/layout.py:50: imports subprocess
    src/discovery/layout.py:379: a non-docstring literal contains 'cobc'
FAILED test_transitive_first_party_closure_is_compiler_free
FAILED test_engine_computes_a_layout_with_process_spawning_unimportable
    ImportError: WP-2.2 D15: subprocess is not importable inside the layout engine
3 failed, 17 passed
```

All three independent assertions bit. Reverted; 20 passed.

Eleven further negative controls are **permanent** rather than anecdotal: one
per forbidden shape (`os.system`, `os.popen`, `os.exec*`, `shutil.which`,
`ctypes`, `__import__`, `eval`, …), plus a converse control proving the detector
does not flag a plain static calculator. A detector that flagged everything
would pass the negative controls and tell you nothing.

**One narrowing, stated rather than quietly applied.** The literal check covers
*invocable binaries* — `cobc`, `cobcrun`, `cob2`, `igycrctl` — and not the
product name `GnuCOBOL`, and it exempts docstrings. Both narrowings are load
bearing in the other direction: the D16 accusation block has to be able to say
"MEASURED by GnuCOBOL 3.1.2.0", which is exactly the provenance R9 requires, and
forbidding the word would push the reasoning out of the modules that most need
to carry it.

### 2. The round-trip: 186 of 186, tolerance zero

`tests/test_layout_roundtrip.py`. The seal is verified **before a single row is
trusted**, not after: all three layers of `tools/verify_manifest.py`, the signer
pinned to fingerprint `233bb4406e2de606`, the manifest hash checked against the
published `696397e0d4d865a3…`, and `oracle/oracle.json`'s digest checked against
the signed manifest. There is deliberately no code path that loads the oracle
without that chain — "we checked it in another test" is how a green-by-skip gets
in.

**What 186 is made of.** The same composition the oracle uses for its own
Route A / Route B agreement: 170 probe field rows, `offset` and `length`
compared together at tolerance zero, plus one group-length comparison per
variant, 16 of them. The count is asserted at 186 *and* its per-copybook
composition is checked against the oracle's own `agreement.per_copybook.checks`,
so two offsetting errors cannot cancel.

Gap projections, level-88 conditions, REDEFINES containment and the tiling
invariant are asserted **in addition** and counted separately — the oracle does
not fold them into its 186 either, and folding them in here would move the
number for a reason that has nothing to do with coverage.

| Axis | Copybook | Rows | Agreement |
|---|---|---|---|
| DISPLAY numeric | `D01_display.cpy` | 12 | 12 / 12 |
| COMP / COMP-4 / BINARY / COMP-5 | `D02_binary.cpy` | 13 | 13 / 13 |
| COMP-3 | `D03_packed.cpy` | 13 | 13 / 13 |
| SIGN clause | `D04_sign.cpy` | 8 | 8 / 8 |
| OCCURS fixed | `D05_occurs.cpy` | 18 | 18 / 18 |
| OCCURS nested, INDEXED BY | `D06_occurs_nested.cpy` | 27 | 27 / 27 |
| OCCURS DEPENDING ON | `D07_odo.cpy` (2 variants) | 23 | 23 / 23 |
| REDEFINES | `D08_redefines.cpy` | 14 | 14 / 14 |
| level-88 | `D09_conditions.cpy` | 6 | 6 / 6 |
| **SYNCHRONIZED** | `D10_sync.cpy` | 9 | **9 / 9** |
| **level-66 RENAMES** | `D11_renames.cpy` | 11 | **11 / 11** |
| edited / JUSTIFIED | `D12_edited.cpy` | 10 | 10 / 10 |
| seeded (FILLER, numeric-edited) | `MUBBREC.cpy` | 7 | 7 / 7 |
| seeded (COMP-3 constants) | `MUBCONS.cpy` | 7 | 7 / 7 |
| seeded (flat, tiling) | `MUBCUST.cpy` | 8 | 8 / 8 |
| **Total** | **15 copybooks, 16 variants** | **186** | **186 / 186** |

Twelve of twelve, and the two axes flagged as most likely to disagree both came
in exact on the first run.

**SYNC.** The brief allowed a PARTIAL outcome here. It was not needed: GnuCOBOL
3.1.2's placement in `D10_sync.cpy` is reproducible from one static rule —
*align a SYNCHRONIZED binary item to its own width, measured from the start of
the record* — which yields slack at offsets 2, 10 and 16 and a 25-byte group
over 20 declared bytes, byte for byte. `cobc` was not called, and the temptation
to call it "just for SYNC" was the hole in §1 reopening.

The claim is narrowed where the evidence stops rather than where the rule stops.
SYNC on a **non-binary** item and SYNC **inside an OCCURS table** are not
measured by v0.1: the engine applies no alignment, marks the layout `PARTIAL`,
and names the construct. Both cases are pinned by tests. An honest eleven of
twelve is shippable; a fudged twelve is not, and that logic does not stop
applying once the twelfth axis comes in green.

**RENAMES.** The brief anticipated that the sealed row shape might not express a
span, which would have been a D16 finding rather than a bug to work around. It
does express one. `D11_renames.cpy`'s level-66 rows carry `offset` = the renamed
start item's offset, `length` = the span, `redefines` = the start item's name,
`renames: true`, `in_tiling: false`. The engine emits that shape rather than
inventing a second one and reconciling later, and `D11-ALPHA-BETA` is asserted
to be *longer* than `D11-ALPHA` — if it were not, the axis would be
indistinguishable from REDEFINES.

**One correction to the brief, from the seal.** WP-2.2 §3.2's table gives the
binary rule as "1-4 digits → 2 bytes". The sealed measurement says otherwise:
`D02-COMP-1 PIC S9(01) COMP` occupies **1** byte and `D02-COMP-2 PIC S9(02)
COMP` occupies 1 as well, so GnuCOBOL's `binary-size: 1-2-4-8` table is
1-2 → 1, 3-4 → 2, 5-9 → 4, 10-18 → 8. The engine implements what was measured.
`BINARY_WIDTHS` is pinned by a test that says so.

### 3. Gaps compare by projection, not by label (D18)

The oracle can only observe "bytes no named field claims" and calls them `gap`.
The engine reads the source, so it distinguishes `filler` (an explicit `FILLER`
entry) from `slack` (bytes SYNCHRONIZED inserted), and the comparison asserts
that `filler ∪ slack` equals the oracle's `gap` spans **as byte ranges**.
Comparing labels directly would fail on extra information rather than on
disagreement.

All six gap rows agree: `D10_sync.cpy` at 2+3, 10+1, 16+1, all three labelled
`slack` with the causing item named; `MUBBREC.cpy` at 11+2, 44+2, 58+2, all
three labelled `filler`. "Bytes 2, 10 and 16 are SYNC slack, not data" is a
different sentence from "there is a three-byte gap", and only one of them helps
someone writing a target schema.

### 4. The accusation path, proven in both directions

100% agreement is a claim about the **pair**. A harness that can only accuse the
engine can never discover a defect in the answer key, which is most of what
committing the benchmark first was supposed to buy.

Both directions were provoked with a one-line planted defect, run, captured
verbatim, and reverted. Neither plant is in the tree.

**Direction 1 — ACCUSES: engine.** Planted in `picture_size()`: `CR` counted as
one character position instead of two.

```
AssertionError: MISMATCH  D12_edited.cpy / D12-EDITED / D12-EDITED
oracle : offset 1  length 66  source probe   (sealed, relian-discovery-bench-v0.1)
engine : offset 1  length 65  source computed
ACCUSES: engine — the oracle row is 'probe'-sourced — MEASURED by GnuCOBOL 3.1.2.0 — and the seal verifies (manifest 696397e0d4d865a3…, key 233bb4406e2de606). Fix src/discovery/, not the answer key.

MISMATCH  D12_edited.cpy / D12-EDITED / D12-CREDIT-EDIT
oracle : offset 42  length 11  source probe   (sealed, relian-discovery-bench-v0.1)
engine : offset 42  length 10  source computed
ACCUSES: engine — the oracle row is 'probe'-sourced — MEASURED by GnuCOBOL 3.1.2.0 — and the seal verifies (manifest 696397e0d4d865a3…, key 233bb4406e2de606). Fix src/discovery/, not the answer key.

MISMATCH  D12_edited.cpy / D12-EDITED / D12-JUST-X
oracle : offset 53  length 8  source probe   (sealed, relian-discovery-bench-v0.1)
engine : offset 52  length 8  source computed
ACCUSES: engine — the oracle row is 'probe'-sourced — MEASURED by GnuCOBOL 3.1.2.0 — and the seal verifies (manifest 696397e0d4d865a3…, key 233bb4406e2de606). Fix src/discovery/, not the answer key.

MISMATCH  D12_edited.cpy / D12-EDITED / D12-JUST-A
oracle : offset 61  length 6  source probe   (sealed, relian-discovery-bench-v0.1)
engine : offset 60  length 6  source computed
ACCUSES: engine — the oracle row is 'probe'-sourced — MEASURED by GnuCOBOL 3.1.2.0 — and the seal verifies (manifest 696397e0d4d865a3…, key 233bb4406e2de606). Fix src/discovery/, not the answer key.

MISMATCH  D12_edited.cpy / D12-EDITED / D12-EDITED (group length)
oracle : offset 1  length 66  source listing   (sealed, relian-discovery-bench-v0.1)
engine : offset 1  length 65  source computed
ACCUSES: engine — the oracle row is 'listing'-sourced — MEASURED by GnuCOBOL 3.1.2.0 — and the seal verifies (manifest 696397e0d4d865a3…, key 233bb4406e2de606). Fix src/discovery/, not the answer key.
assert not ['MISMATCH  D12_edited.cpy / D12-EDITED / D12-EDITED\n  oracle : offset 1  length 66  source probe   (sealed, relian-d...0 — and the seal verifies (manifest 696397e0d4d865a3…, key 233bb4406e2de606). Fix src/discovery/, not the answer key.']
```

Note the last block: the group-length comparison is `listing`-sourced rather
than `probe`-sourced, and it is accused the same way, because Route A's symbol
table is a measurement too.

**Direction 2 — ACCUSES: oracle (ESCALATE).** Planted in `_sync_align()`: slack
computed one byte short of the boundary. Same class of defect as direction 1,
but the disagreement lands on a DERIVED row, so the verdict flips.

```
Failed: MISMATCH  D10_sync.cpy / D10-SYNC / gap projection (filler ∪ slack)
oracle : spans [(2, 4), (10, 10), (16, 16)]  source gap   (sealed, relian-discovery-bench-v0.1)
engine : spans [(2, 3), (14, 15)]  source ['slack']
ACCUSES: oracle (ESCALATE) — the oracle row is 'gap'-sourced — DERIVED by subtraction, not measured. FILLER and SYNC slack cannot receive a MOVE, so the probe cannot see them and the oracle recovers them as the bytes no named field claims (SPEC.md §7). The engine reads the source and can distinguish explicit FILLER from implicit SYNC slack, so it may legitimately know more here (D18).
HALT: this work package stops. Do NOT edit oracle.json — CLAUDE.md rule 4 freezes discovery-bench/ and the Ed25519 signature over the manifest would fail regardless. The remedy is a v0.2 re-seal, which is an operator key session. Finding an oracle defect is a SUCCESSFUL outcome of WP-2.2.
engine gap detail: 2+2 slack (SYNCHRONIZED alignment of D10-BIN-A to a 4-byte boundary); 14+2 slack (SYNCHRONIZED alignment of D10-BIN-C to a 8-byte boundary)
```

Both reverted; 145 passed. A third case is pinned too: if the seal does **not**
verify, the block accuses neither artifact, because accusing the engine on the
strength of a document whose signature failed would be worse than reporting
nothing.

**No accusation landed on the oracle in this run.** All 186 comparisons passed
on the first execution of the engine against the sealed document, so the
escalation trigger did not fire.

### 5. `None` never becomes zero (acceptance ⑦)

WP-2.1 measured the trap on the other side of this boundary: `MOVE HIGH-VALUES`
to a `PIC ZZZZZZZZ9.99` compiles, runs, exits 0 and marks **zero bytes**, with
no warning enabled by default. A probe built on it would have recorded
`offset: 0, length: 0` for `BPR-AMOUNT-DUE`, whose true layout is offset 46,
length 12.

Same trap, new surface. `lint_layout()` fails on any field carrying `0` where
nothing was computed, on a `None` length with no stated reason, and on a
level-88 that grew an extent. It runs over all 15 corpus copybooks and over
every one of the 16 sealed variants inside the round-trip. Three negative
controls prove it rejects each shape — a lint that has never rejected anything
is a lint whose failure path is unproven.

The lint found one real defect during the build: when a child's length could not
be computed, the enclosing **group** row came back with `length: None` and no
`unmeasured_reason` — the same "a number that will not say why it is absent"
failure, one level up. Fixed rather than exempted.

**R7 shows up as a refusal.** A usage the sealed corpus does not measure —
`COMP-1`, `COMP-2`, `COMP-X`, `INDEX`, `POINTER` — returns `None` with a named
reason rather than a plausible width, and marks the layout `PARTIAL`. These have
widely documented widths. A documented width is not a measured width.

### 6. CardDemo dry run — resolver only

Full record: `docs/dryruns/carddemo_copybooks/`. No layout claim is made about
any CardDemo program; program parse is still blocked on WP-2.5 and the blockers
are dialect as well as `COPY`. Copybook resolution is unaffected by that (D17) —
the resolver is our own over our own grammar and imports no ANTLR parser, which
is what decouples WP-2.2 from WP-2.5 entirely.

Clone at `~/corpora/carddemo`, commit `59cc6c2fd7ebd7ef7925cad552a01a4b8b6e4d5e`,
Apache-2.0, **outside the repository**; no CardDemo bytes are committed here.

| Row | **Measured in this run** | WP-2.0.0 §0.4 | Drift |
|---|---|---|---|
| Source files scanned | **106** | 44 `.cbl` + 62 `.cpy` | none |
| Files with ≥1 `COPY` | **40** | 40 of 44 | none |
| `COPY` directive sites | **346** | not recorded | — |
| Distinct names referenced | **67** | 67 | none |
| — resolvable | **59** | 59 | none |
| — **not** resolvable | **8** | 8 | none |
| Edges | **306** | 306 | none |
| Max fan-out | **18**, `app/cbl/COACTUPC.cbl` | 18, `COACTUPC.cbl` | none |
| `COPY … REPLACING` sites | **40** | 40 | none |
| Present but unreferenced | **3** | 3 | none |
| Cycles | **0** | not recorded | — |

The run and the log agree on every row §0.4 recorded. Had they disagreed the run
would be authoritative; both are recorded either way.

The eight unresolvable names are `DFHAID` `DFHBMSCA` `CMQGMOV` `CMQMDV` `CMQODV` `CMQPMOV` `CMQTML` `CMQV` — CICS- and IBM MQ-supplied, absent
from the sample and present at the customer site. `DFHAID` and `DFHBMSCA` are
each referenced by 21 programs, the same fan-in as the four most-shared members
that *do* resolve. That is the shape of the risk, and reporting it before a
migration starts is a capability, not an apology (D20, R2).

**Two things §0.4 did not record, measured here.** `346`
directive sites against `306` edges — a program that `COPY`s a member
twice is two sites and one edge, and conflating them makes the edge count and
the REPLACING-site count the same kind of number when they are not. And the
40 `COPY … REPLACING` sites are **not** all in one program: 39 in
`app/cbl/COACTUPC.cbl` and 1 in
`app/app-transaction-type-db2/cbl/COTRTUPC.cbl`. All 40 parsed completely.

**One correction to §0.4.** The phantom `REPLACING` copybook appears at **two**
sites, not one. Measured by scanning the corpus twice with patterns differing
only in the boundary assertion — same operand grammar, same margins:

| Pattern | Distinct names |
|---|---|
| `\bCOPY\s+…` | **68** |
| `(?<![A-Za-z0-9$_-])COPY(?![A-Za-z0-9$_-])\s+…` | **67** |
| Only in the naive set | `REPLACING` |

matched out of `INITIALIZE REQUEST-MSG-COPY  REPLACING NUMERIC BY ZEROES` at
`app/app-vsam-mq/cbl/CODATE01.cbl:294` — the site §0.4 named — **and** at
`app/app-vsam-mq/cbl/COACCT01.cbl:345`, which it did not.

### 7. Two limitations the resolver reports rather than absorbing

Both would otherwise be a substitution that quietly did not happen, which is the
worst kind: the layout that follows looks complete and names the wrong fields.

* **A `REPLACING` operand spanning a line break.** Program text has a margin on
  both sides, so the bytes between two lines are not whitespace and a per-line
  substitution cannot span them. Detected by re-running the match over the
  joined program text and comparing counts; surfaced as `Assembly.unapplied`,
  which makes the assembly incomplete, which makes the layout `NONE` with the
  reason named. None of CardDemo's 40 sites hits this.
* **A substitution overflowing column 72.** A replacement longer than what it
  replaced can push program text into the identification area where nothing will
  read it. Reported, not truncated.

### 8. Test count

`EXPECTED_PASSES` 370 → **702**. Net **+332**, in four new files and nothing
else; no existing test was changed, renamed, deleted or deselected. The skip
count stays pinned at 10 — none of the 332 carries a `skipif`, and none of them
needs `cobc`, which is the point of the whole package. Attributed file by file,
and within each file by what the cases cover, in the `EXPECTED_PASSES` comment
block in `.github/workflows/tests.yml`.

```
tests/test_discovery_is_compiler_free.py   +20
tests/test_discovery_copybook.py           +37
tests/test_discovery_layout.py            +130
tests/test_layout_roundtrip.py            +145
```

### 8b. The grade states its basis, in the output rather than the log

`Layout.summary()` grades every number **PLAUSIBLE**, never VERIFIED. VERIFIED
would claim the offset holds on the compiler the customer actually runs, and
that is exactly what has not been measured.

But a grade with no stated basis is a grade with the units filed off: *plausible
with respect to what?* Recording the answer in this log and in the docstrings
would leave it where no customer will ever read it. So the reason travels inside
the artifact:

* **Inside every `Measured.provenance`.** A consumer that renders a number
  cannot render it without the basis attached.
* **As `Layout.limitations()`**, whose first entry is the IBM-equivalence
  limitation and whose remainder is that record's own `PARTIAL`/`NONE` reasons —
  so a caller that renders the tuple renders the whole caveat set rather than
  half of it.
* **In `Layout.to_dict()`**, as `grade`, `verified_against`, `benchmark` and
  `limitations`.
* **At the top of the `discovery layout` JSON document as well as on each
  record**, because a caveat that exists only one level down is a caveat a
  reader can scroll past.

The published text:

> Verified byte-for-byte against GnuCOBOL 3.1.2.0 on RELIAN-DISCOVERY-BENCH v0.1
> (relian-discovery-bench-v0.1), 186 of 186 comparisons at tolerance zero.
> Equivalence with IBM Enterprise COBOL is UNMEASURED: SYNCHRONIZED slack in
> particular moves with the compiler's binary-size setting and its SYNCHRONIZED
> handling, so an IBM layout may differ from the one below. Grade PLAUSIBLE, not
> VERIFIED, for that reason.

**The prose is pinned to the measurements it quotes.** A measurement frozen into
a string constant is a measurement that can go stale in silence, which is the R1
failure this project keeps deleting. Two tests close that:

* the "186 of 186" is read back out of the round-trip harness's
  `EXPECTED_COMPARISONS`, so changing one without the other goes red;
* `COMPILER_BASIS` is checked against `toolchain.cobc` in the **sealed** oracle,
  so a v0.2 sealed on a different compiler cannot leave this claim pointing at
  the old one.

Fifteen further cases — one per corpus copybook — assert that every number the
engine publishes carries the basis in its own provenance string.

`discovery resolve` carries a different note, because the IBM limitation does not
apply to it: resolution reads `COPY` directives rather than storage, so it is
dialect-independent. What it says instead is that it makes **no layout claim at
all**, which is better than leaving a reader to infer that from an absence.

### 9. `discovery-bench/` is byte-identical to the seal

```
$ git status --short -- discovery-bench/
(empty)
```

Nothing under `discovery-bench/` or `bench/` was created, modified or deleted.
The engine reads the corpus; the harness reads the committed `oracle.json`;
neither writes. The round-trip additionally asserts each corpus file's sha256
against the digest recorded *inside* the oracle, so a drifted corpus would go
red rather than produce a green comparison between two different corpora.

### 10. What this does NOT license

- **IBM Enterprise COBOL equivalence is unmeasured (◐).** Everything verified
  here is GnuCOBOL 3.1.2.0 behaviour. SYNC slack in particular moves with
  `binary-size` and with the compiler's `SYNCHRONIZED` handling. This belongs in
  the customer report as a stated limitation and stays out of the
  quotable-capability matrix until measured against a real IBM layout (R11).
  `Layout.summary()` grades every number **PLAUSIBLE**, not VERIFIED, and §8b
  is how that reason reaches a reader of the output rather than a reader of
  this log.
- **No layout claim about any CardDemo record.** The resolver ran; the engine
  did not.
- **Dictionary, file inventory, lineage, DDL, signed report** — WP-2.3+ (D21).
  The CLI here is `discovery layout` and `discovery resolve`, which is what the
  dry run needed and no more.
- **D13 publication remains open.** `233bb4406e2de606` still needs to reach the
  Technical Delivery Sheet and every attestation deliverable alongside the
  verification command. Operator, R11.

---

## 2026-08-21 · WP-2.3 · The signed Data Discovery report

- **HEAD at start:** `3ac6ee5` (merge of PR #30, WP-2.2)
- **Branch:** `claude/signed-data-discovery-report-oxb7w6`
- **Baseline triple, re-measured in this session:** `702 passed, 10 skipped,
  0 failed`. Re-measured rather than transcribed, and the first measurement
  disagreed: the container had no `cobc` and no `javac`, which produced
  `683 passed, 28 skipped, 1 failed`. The 19-test gap is exactly the 18 extra
  skips plus the one seal test that needs a complete toolchain probe. GnuCOBOL
  3.1.2-5.1ubuntu1 and OpenJDK were installed and the run reproduced the sealed
  triple before any code was written. A baseline taken on a degraded runner
  would have made every later delta wrong by 19.
- **Scope guard:** nothing under `bench/` or `discovery-bench/` was created,
  modified or deleted (§9). No transform code was touched, so BER cannot have
  moved; §9 records the check rather than the assumption.

---

### 1. The constraint, and what it forced

R12 keeps customer source in the customer perimeter. R4 keeps the release key
in the operator's custody. Different machines, neither may travel. So the CLI
signs a **manifest of digests** and the only thing that crosses is a
64-character hex string.

Two layers, added at different times, verified independently:

| Layer | Key | Proves |
|---|---|---|
| Instance | per-installation Ed25519, generated on first run at `~/.relian/instance-ed25519.pem` mode 0600 | integrity and provenance-of-tool. **Not** identity of signer |
| Countersignature | Visionblox release key `91e3a404155ba4dd`, detached | Visionblox attests to this report |

The instance layer proves less than it looks like it proves, and the artifact
says so in those words. `INSTANCE_LAYER_CLAIM` is in `report.json`, in
`report.md`, and in the verifier's own output; a report with a good instance
signature and no countersignature is **VALID AND UNATTESTED** with its own exit
code (3), distinct from both 0 and 1. Three tests exist only for that wording,
because a misrepresentation to a government customer is a different category of
problem from a bug.

### 2. Byte-identity, and where the one timestamp went

`report.json` carries **no timestamp at all**. The single `generated_at` lives
in the manifest, which is where the signature is taken. That is what makes ①
hold by construction rather than by care:

```
$ python3 -m src.discovery.cli report build examples/demo --out A …
$ python3 -m src.discovery.cli report build examples/demo --out B …
$ cmp A/report.json B/report.json
→ (identical)
```

Re-measured on CardDemo as well, where the report is 32 KB: identical.

The canonical form is asserted on the **emitted bytes**, not on the dict, with
the default serialisation as its converse control — without that, an assertion
that the bytes equal `json.dumps(…, separators=(",", ":"))` could pass against
a form that is not distinguishable from the default.

Section order is carried by a **list**, not a mapping. Canonical JSON sorts
keys, so a mapping cannot express "identification before scope" at all, and
D26's requirement that the missing-copybook table lead the findings would have
been unstatable.

### 3. The leak test (②)

The request line is three hexadecimal fields:

```
relian-countersign-request/1 manifest_sha256=<64> report_id=<32> instance_fingerprint=<16>
```

The argument that it cannot leak is "all three are digests". That is an
argument, not a measurement, so the measurement exists: a report is built from
a tree whose **directory name, copybook name, field names, and every free-text
engagement field** carry distinctive marker strings, and six parametrised tests
assert none reaches the line. A seventh asserts the fixture actually carried
all six — without it the leak test could pass because the seeds were never
there, which is a control that proves nothing.

`countersign_request_line` also re-checks each field as hexadecimal before
emitting, and raises rather than emitting if one is not. If a field ever has to
carry text, the line is refused and the message says to escalate: that is R12
failing and the design is wrong, not the implementation.

### 4. Four verifier layers, each planted red and reverted (③)

`tools/verify_report.py`, sha256 `43f0bf20a79bb5d07aecb619dc2b7364469769ea2d81a4679e9b1b588bc4cbd5` **as measured in this entry; superseded at WP-2.3.2 §1, and this line is left as it was because the log is append-only**. Standalone: standard library plus
`cryptography`, no repo import, no network, no credential — all three asserted
by AST rather than by grep. It prints its own digest on every run.

| Layer | Planted red as | Other three while it was red |
|---|---|---|
| FILES | an edited `report.json`, and a deleted `report.md` | all PASS — the signature covers the MANIFEST's bytes, not the report's, which is exactly why this layer exists |
| MANIFEST | a *consistently* edited manifest: report changed AND its recorded digest updated to match | FILES **PASS**, asserted, so the test proves the attack really does defeat layer 1 |
| INSTANCE | one flipped nibble in `signature_hex`; separately, a declared fingerprint that is not the signer | FILES and MANIFEST PASS |
| COUNTERSIGNATURE | a countersignature for a different report | the other three PASS |

Each test reverts and re-verifies green in the same test, so a planted-red that
left the fixture broken could not be mistaken for a passing suite.

**⑤, the re-signed forgery.** The deliverable is re-signed end to end under an
attacker-generated key — instance block and countersignature both — and then
verified twice. Pinned to the attacker's own fingerprint it returns **0**,
which is asserted first: without that assertion the test would not be
exercising a forgery that is internally consistent. Pinned to the real
fingerprint, FILES / MANIFEST / INSTANCE all **PASS** and only
COUNTERSIGNATURE fails. `--pin-fingerprint` is `required=True` with no
default, and a test runs the verifier without it and asserts argparse refuses.

### 5. R11 lint, each term its own negative control (⑥)

Vocabulary: `solana`, `on-chain`, `ml risk`, `machine learning`,
`predicted`, `estimated`. Linted against the templates **and** against the
emitted `report.json` and `report.md` — a term that only ever reached the page
through an f-string would survive a template-only lint. The CLI runs the same
lint on its own output and **refuses to write** if it fires.

Twelve parametrised negative controls: each term planted in prose, and each
term planted upper-cased. One converse control asserts the lint does not fire
on ordinary report prose; without it a lint that flagged everything would pass
all twelve and tell you nothing. The Technical Delivery Sheet gets the same
treatment, with its own six.

### 6. Measured triple

```
$ pytest -q
→ 829 passed, 10 skipped, 0 failed
```

Net **+127** on 702, attributed file by file in `.github/workflows/tests.yml`
and summarised here:

| File | Δ |
|---|---|
| `tests/test_discovery_report.py` | +51 |
| `tests/test_report_signing.py` | +31 |
| `tests/test_verify_report.py` | +27 |
| `tests/test_delivery_sheet.py` | +16 |
| `tests/test_discovery_is_compiler_free.py` | +2 |

The last is not a new test. `test_module_never_invokes_a_compiler` is
parametrised over every module in `src/discovery/`, and `report.py` and
`signing.py` are two more modules — so the D15 AST guard covers the new surface
without being told to. **The skip count is unchanged at 10**: none of the 127
carries a `skipif` and none needs `cobc` or `javac`.

### 6b. One defect this work package found in itself

The R1 gate (`lint_measured_fields`) was first exercised only against
`examples/demo` — three flat copybooks, no `OCCURS`. Run against the sealed
12-axis corpus it **refused to write a report**, naming every `OCCURS` index
vector as an ungraded number:

```
REFUSED: a number reached the report without a grade (R1/R9):
  $.sections[3].records[5].fields[2].subscripts[0]: bare number 1 with no grade …
```

A subscript is a structural coordinate, not a measurement — the same category
as `level` and `offset`, both of which were already on the allowlist and both
of which are covered by the layout's own grade and limitation block. The fix
is one entry. What matters is where it was caught: the demo tree would never
have surfaced it, and the first customer with a table in a copybook would have
received a refusal instead of a report.

`test_the_r1_gate_is_clean_over_the_sealed_twelve_axis_corpus` is the
regression, and it is guarded by a control
(`test_the_bench_fixture_exercises_more_than_the_demo_tree`) asserting the
fixture really does contain subscripts, an ODO object and a REDEFINES — so it
cannot quietly become vacuous if the corpus is ever narrowed.

### 7. Dry runs (⑧)

Full detail in `docs/dryruns/REPORT_DRYRUNS.md`. Both stop at the
countersignature, which is an operator key session exactly as a seal is.

| | `examples/demo` | CardDemo |
|---|---|---|
| report id | `017f5ff0fdc4457770ee467c7e9ff073` | `2fff4d2377c2cc199d6271bd1c21f885` |
| manifest sha256 | `f36b5dd137575325f752f64c65dce90ec616edd9c065ad8bfa435f6da305de3a` | `04b599fa41e984fc54eba4c48d6bfdb43de8ab74194fb59b9b127b5c8f3cecac` |
| instance fingerprint | `98616f9c7543d293` | `98616f9c7543d293` (same installation) |
| record layouts | 3, all COMPLETE | not included, by decision |
| missing copybooks | 0 | **8** |
| verifier | VALID AND UNATTESTED, exit 3 | VALID AND UNATTESTED, exit 3 |

CardDemo reproduced every count WP-2.2 measured — 106 files, 40 with a `COPY`,
346 directive sites, 67 distinct names, 59 resolvable, 8 not, 306 edges, max
fan-out 18 at `app/cbl/COACTUPC.cbl`, 40 `REPLACING` sites, 3 unreferenced
members, 0 cycles. Zero drift. Had a row disagreed, the run would be
authoritative and the table would say so.

**Two narrowings are worth recording, because both remove something a reader
might have expected to be there.**

*No layouts on CardDemo.* WP-2.2 §10 established that no layout claim is made
about any CardDemo record: the engine is verified against GnuCOBOL 3.1.2.0 and
CardDemo is IBM-dialect source, so a layout would be graded against nothing.
The report is resolver-only **by decision, not by failure**, and
`Report.build` refuses to emit an empty layout section over a tree with
resolvable copybooks unless a reason is supplied — an empty section with no
stated reason reads as a clean result (R2). The reason is in the artifact.

*No "likely source" column in the missing table.* The WP-2.2 dry-run note
carried one — "CICS-supplied", "IBM MQ-supplied". Both are correct, and both
are inferences about a member the engine never saw, sitting in a table where
every other cell is a count taken in the run. In an internal note that is a
useful annotation; in a customer artifact governed by R1 it is an inference
wearing the costume of a finding. What is measured (referrers, directories
searched) ships; what is inferred stays in the engagement conversation where a
person owns it.

### 8. D13 closed

`docs/TECHNICAL_DELIVERY_SHEET.md` publishes both fingerprints, each labelled
with what it signs and what it does not, and each tool's own SHA-256.
`tests/test_delivery_sheet.py` pins the published digests against the files on
disk, so editing a tool without updating the sheet turns the build red — a
digest published in a document that can go stale in silence is a digest nobody
should rely on.

The two fingerprints were **graded differently, and deliberately**:

* `233bb4406e2de606` — **VERIFIED**. It is recorded in both sealed ledgers in
  this repository and CI pins against it on every run.
* `91e3a404155ba4dd` — **PLAUSIBLE at the time of this entry.** Transcribed
  from the WP-2.3 brief. Nothing in this repository had computed it from a key.

Calling the second VERIFIED would have been a grade with nothing behind it.
**It was upgraded to VERIFIED on 2026-08-21 — see WP-2.3.1 §2 below — when the
operator derived it from the public key and supplied the method.** The upgrade
is recorded rather than applied silently: a grade that changes without a dated
reason is a grade nobody can audit.

### 9. `bench/` and `discovery-bench/` are byte-identical (⑪)

```
$ git status --short -- bench/ discovery-bench/
(empty)
```

No transform code was touched either, so BER cannot have moved. Both are
checked rather than asserted.

### 10. What this does NOT license

- **No countersignature exists.** Both dry runs are `VALID AND UNATTESTED`.
  Nothing in this repository has ever held the release key, and the flow is
  proven under a stand-in key generated inside a test.
- **No IBM Enterprise COBOL claim.** Unchanged from WP-2.2 and now stated in
  the report's top matter as well as on each record.
- **No file inventory, lineage or target-schema DDL.** WP-2.4+, and absent
  here rather than approximated.
- **PDF rendering was not built.** Markdown is the one derived rendering, and
  it names `report.json` as authoritative. Adding a second renderer before
  anyone has asked for one would double the surface on which a rendering can
  disagree with the signed object.


---

## 2026-08-21 · WP-2.3.1 · Release-key corrections

- **HEAD at start:** `c593573` (WP-2.3)
- **Branch:** `claude/signed-data-discovery-report-oxb7w6` (same branch, no PR
  merged yet)
- **Baseline:** `829 passed, 10 skipped, 0 failed`, measured by CI run
  32509335480 and reproduced locally.

Three corrections supplied by the operator, plus the fingerprint upgrade they
unblock.

### 1. The key path was wrong

`~/zil-keys/relian-release.pem` does not exist. The release key is
`~/zil-keys/visionblox-release-key-v1.pem`. Corrected in `tools/countersign.py`
(docstring, usage block, `--key` help), `docs/TECHNICAL_DELIVERY_SHEET.md`,
`docs/dryruns/REPORT_DRYRUNS.md` and the absent-key fixture in
`tests/test_report_signing.py`.

Worth naming: **nothing failed because of this.** The absent-key planted-red
passed happily against a path that was wrong in a second way, because "this
file is not there" is true of any name you invent. A refusal test proves the
refusal fires; it cannot prove the path it was pointed at was the real one.

### 2. `91e3a404155ba4dd` is now VERIFIED, and derived from public material only

Operator derivation, 2026-08-21: `visionblox-release-key-v1.pub`, a PEM
SubjectPublicKeyInfo document holding an Ed25519 key, raw public length 32
bytes; SHA-256 over those 32 raw bytes; first 16 hexadecimal characters =
`91e3a404155ba4dd`.

**Public material only, and that is the load-bearing part.** A fingerprint a
customer cannot re-derive without the private key is not published, it is
asserted — only the operator could ever check it. So the derivation now exists
in code as `tools/countersign.py::fingerprint_from_public_pem`, whose sole
input is the public document: it opens no private key and requests no
passphrase, and a test asserts it cannot be handed one.

`fingerprint_of` — which starts from a loaded private key — remains, for the
one place that genuinely holds one: refusing, at signing time, a key that does
not match the published value. Five generated keypairs assert the two functions
agree, so the number the sheet PUBLISHES and the number the tool ENFORCES
cannot drift into being different numbers wearing the same name.

The sheet now prints the six-line recipe, and
`test_the_published_recipe_computes_what_the_tool_computes` **executes those
six lines** rather than reading them. A published recipe nobody has run is a
recipe that can be wrong, and the customer is the one who finds out.

**Still open (§5).** The comparison the operator asked for — derive from the
*actual* `visionblox-release-key-v1.pub` and compare to the sheet's published
value — is not in the suite, because the public key is not in this repository
and is not on this machine. It is public material and belongs here; it is the
one artifact needed to close this.

### 3. The release key is passphrase-encrypted

`getpass.getpass`, prompted at the moment the passphrase is needed, and
accepted no other way:

| Channel | Status | Why |
|---|---|---|
| `--passphrase` on argv | **absent, and asserted absent** | argv is world-readable in `/proc` and lands in shell history |
| an environment variable | **never read, and asserted never read** | environment is inherited by every child and dumped by crash reporters |
| any log line | **never printed, and asserted** | a refusal that quotes what was typed puts the passphrase in scrollback and in every CI transcript |

The two argv/environment guards are AST walks over `countersign.py`, not greps,
and each carries the assertion that it found something to walk — a guard over
zero parsed options would pass vacuously.

**What is NOT claimed.** The passphrase is a Python `str` while in use and
CPython offers no way to scrub that memory. The code says so in a comment, and
a test asserts the comment is still there, because the failure mode for a
security claim is that it quietly becomes an overclaim.

### 4. The wrong-passphrase planted-red

The absent-key refusal proves the tool will not *invent* a key. It says nothing
about a key it can see and cannot open. Four new cases close that:

* a wrong passphrase raises and **writes no countersignature**;
* the refusal **does not echo** the passphrase (asserted with a distinctive
  string);
* an empty passphrase is refused rather than retried;
* the converse control — a plaintext key must NOT start prompting for a
  passphrase that does not exist.

Plus one control asserting the fixture key really is
`-----BEGIN ENCRYPTED PRIVATE KEY-----`, without which every case above would
pass vacuously against a plaintext PEM.

### 5. Measured triple

```
$ pytest -q
→ 848 passed, 10 skipped, 0 failed
```

Net **+19** on 829: `tests/test_report_signing.py` +15 (6 passphrase behaviour,
4 custody/no-overclaim guards, 5 public-only derivation), and
`tests/test_delivery_sheet.py` +4 (one test replaced by five: the graded row
now has to carry its method and date, R4 is asserted to have survived the
grade change, the recipe is asserted present, the recipe is EXECUTED, and the
key path with its passphrase custody is pinned).

### 6. What this does NOT resolve

- **No countersignature has been produced.** The release key has still never
  been in this repository, CI, or an agent session. Every passphrase test runs
  against a key generated inside the test.
- **The `.pub` comparison is not in the suite.** See §2. Until the public key
  is committed, the sheet's VERIFIED grade rests on the operator's derivation
  plus a tested, executable recipe — not on a re-derivation this suite performs.


---

## 2026-08-21 · WP-2.3.2 · A stated check that was not performed

- **HEAD at start:** `3a4cdef` (WP-2.3.1), PR #31 open, all five CI jobs green.
- **Found by:** automated review on PR #31, not by this suite. Recorded that
  way: a finding the tests missed is worth more as a note about the tests than
  as a silent fix.

### 1. The defect

`tools/countersign.py` records two advisory fields in every countersignature —
`report_id` and `instance_fingerprint` — under a comment saying the verifier
checks both against the manifest. `check_countersignature` compared only
`report_id`. A countersignature naming a different installation's instance key
passed layer 4 without a word.

**Severity, stated honestly rather than inflated.** This was never a soundness
hole. The countersignature covers the manifest hash, and the manifest commits
to the instance key, so a forged or swapped countersignature already failed on
the hash comparison one line above. Nothing that should have been rejected was
accepted.

What it was is a **verifier naming a check it did not perform**, on the single
surface a customer is told to trust and told to re-run themselves. That is the
same shape as the defects this package was written to prevent — a claim in an
artifact that the artifact's behaviour does not keep — and it is worse in a
verifier than anywhere else, because a verifier's whole value is that its
output means what it says.

It also cost the useful diagnostic. The realistic way that field goes wrong is
not an attack: it is an operator countersigning from the wrong request line,
for the right report but the wrong installation. That is now a named failure
with the sentence *"do not describe the same installation"* instead of silence.

### 2. The fix, and the guard that would have caught it

The comparison is added, with the reasoning inline so the next reader knows why
an advisory field is checked at all. Two tests:

* `test_layer_countersignature_fails_on_a_mismatched_instance_fingerprint` —
  planted red, other three layers asserted PASS, then reverted and re-verified
  green, matching the shape of the other layer tests.
* `test_countersign_records_exactly_the_advisory_fields_the_verifier_checks` —
  **the guard that generalises it.** For every advisory field `countersign.py`
  writes, `verify_report.py` must read it. This is the test whose absence let
  the defect exist: WP-2.3 tested each layer's failure modes thoroughly and
  never tested that the two tools agreed about what they were exchanging.

### 3. A second staleness in the same shape

`tools/verify_report.py` changed, so its published SHA-256 did. The delivery
sheet is pinned by the suite and would have gone red; `docs/dryruns/REPORT_DRYRUNS.md`
publishes the same two digests and was **not** pinned, so it would have gone
stale in silence — the exact failure the sheet's own pinning exists to prevent,
one file over. It is now pinned too.

`docs/PHASE2_LOG.md` is deliberately NOT corrected. It is append-only and its
WP-2.3 entry records what was measured at WP-2.3; the line is annotated as
superseded instead. A log that gets edited to stay current is not a log.

### 4. Measured triple

```
$ pytest -q
→ 852 passed, 10 skipped, 0 failed
```

Net **+4** on 848: `tests/test_verify_report.py` +2 (the planted-red and the
tool-agreement guard), `tests/test_delivery_sheet.py` +2 (the dry-run note's
digests pinned, parametrised over both shipped tools).

The count was predicted as +3 and measured as +4: the pinning test is
parametrised over two tools, so it contributes two cases rather than one. The
gate is set from the measurement, which is the only reason the gate is worth
having.
