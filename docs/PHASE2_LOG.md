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
