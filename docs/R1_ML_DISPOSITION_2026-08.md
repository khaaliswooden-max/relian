# R1 disposition — the fabricated-metric ML limb (`src/ml`, `src/intelligence`)

**Work package:** WP-2.0.−3
**Date:** 2026-08-20
**Rule:** R1 — *a metric is measured or None — never a constant, default, or estimate presented as
a measurement. Applies to code, tests, report templates, and UI.*
**Disposition:** **DELETED.** Both packages removed. Not repaired, not gated, not relabelled.

This closes the finding raised and deliberately escalated in
[`docs/R6_AUDIT_2026-08.md` §4](R6_AUDIT_2026-08.md), which assessed `src/ml/risk_scorer.py`
during the R6 excision and recorded: *"Disposition: ESCALATED, not changed… Recommend a dedicated
work package."* This is that work package.

## 0. Conditions of measurement

| Item | Value |
|---|---|
| Interpreter | CPython 3.12.3 (matches the version `.github/workflows/tests.yml` pins) |
| Base commit | `a32931d` (`main`) |
| Branch | `claude/remove-fabricated-metric-ml-hjeio7` |
| Suite before | 278 passed, 20 skipped, 0 failed (local; 10 of those skips are the absent GnuCOBOL toolchain) |
| Suite after | see §7 |

`cobc` is absent on the authoring box, so ten demo tests self-skip locally. CI installs GnuCOBOL
and is the authoritative measurement; the sealed triple this work package inherited was
`288 passed, 10 skipped, 0 failed`.

## 1. What was removed

| Path | Disposition |
|---|---|
| `src/ml/__init__.py`, `src/ml/risk_scorer.py` | **DELETED** — `RiskScorer`, `RiskAssessment`, `CodeMetrics` |
| `src/intelligence/__init__.py`, `src/intelligence/migration_intelligence.py` | **DELETED** — `MigrationIntelligence`, `MigrationPattern`, `BudgetState`, `IntelligenceReport` |
| `tests/test_ml.py` | **DELETED** — 12 tests, all exercising the deleted scorer |
| `xgboost>=2.0.0` | **REMOVED** from `pyproject.toml`; `requirements.lock` regenerated |

Call sites removed from `src/core/orchestrator.py`: the module-scope import, the `intelligence`
constructor parameter and `self._intelligence` assignment, and all five uses —
`retrieve_similar_patterns`, `is_retrain_ready`, `retrain_risk_model`, `record_outcome`,
`get_report`.

Git history preserves every deleted file. Nothing here is unrecoverable; it is unreachable, which
is the point.

## 2. The specific fabrications

### 2.1 The training rows — `migration_intelligence.retrain_risk_model()`

Every `RETRAIN_INTERVAL = 20` migrations, the orchestrator retrained an XGBoost regressor. The
feature matrix was built like this (verbatim from the deleted file):

```python
row = [
    p.lines_of_code,
    p.cyclomatic_complexity,
    int(p.cyclomatic_complexity * 1.2),  # cognitive (approx)
    0,  # max_nesting_depth (not stored — use 0)
    0,  # num_functions
    p.lines_of_code / max(1, 5),
    0,  # num_dependencies
    0,  # fan_in
    0,  # fan_out
    0.0,  # coupling_between_modules
    0.0,  # comment_ratio
    0.0,  # duplicate_code_ratio
    0.0,  # dead_code_ratio
    p.test_coverage / 100.0,
    0,  # num_global_variables
    p.num_goto_statements,
    0,  # num_copy_statements
    0,  # data_division_complexity
]
```

Counted rather than characterised — **of the 18 features, 12 are hardcoded zeros, 2 are formulas
over another feature, and 4 carry a stored value.**

- `int(p.cyclomatic_complexity * 1.2)` is labelled `# cognitive (approx)`. Cognitive complexity is
  a distinct measure that weights nesting and flow breaks; cyclomatic complexity times 1.2 is not
  an approximation of it, it is a rescaling of the column immediately above it. As a feature it is
  perfectly collinear with `cyclomatic_complexity` and carries no information at all.
- Twelve `0` / `0.0` placeholders, the preceding comment conceding the mechanism outright:
  `# We use the subset we have; missing fields default to 0`. A default is not a measurement — that
  is the sentence R1 exists to enforce. "Zero fan-in" and "unknown fan-in" are different claims,
  and the model could not tell them apart.
- `p.lines_of_code / max(1, 5)` stands in for average function length. `max(1, 5)` is `5`; the
  expression is `lines_of_code / 5`. Written as though a variable were being guarded against zero,
  it asserts that every program has exactly five functions.

### 2.2 The labels were the heuristic's own output

The strongest finding, and the one that decides repair-versus-deletion. The regression target was:

```python
y_rows.append(p.risk_score / 100.0)  # normalised 0–1
```

`p.risk_score` traces back through `record_outcome(risk_score=result.risk_score)`
(`orchestrator.py:337`) to `result.risk_score = risk_assessment.get("overall_score")`
(`orchestrator.py:248`) to `_score_risk()` to `RiskScorer.score()` — which, with no model file
loaded, is the `_heuristic_score()` weighted sum.

**The model was trained to predict the output of the heuristic it was meant to replace.** Not
migration outcomes; not observed defects; not anything that happened after a migration shipped. At
best it could learn to imitate a weighted sum of `min()` clamps, using features that were 12/18
constant. There is no defensible number on the other side of that pipeline.

### 2.3 The confidence constants — `risk_scorer.RiskScorer.score()`

```python
if self._is_trained and self.model and HAS_XGBOOST:
    score = float(self.model.predict(dmatrix)[0]) * 100
    confidence = 0.85          # <- constant, presented as a measurement
else:
    score = self._heuristic_score(metrics)
    confidence = 0.70          # <- constant, presented as a measurement
```

Neither literal is calibrated against anything; neither moves with the input. In the shipped
configuration no model path is passed, so `_is_trained` is `False` and **every** risk score in
practice came from the heuristic branch carrying `confidence = 0.70`. Quoted as recorded in the
R6 audit §4, which found it and left it.

### 2.4 The docstring claims

`CodeMetrics`:

> Captures 200+ metrics across complexity, coupling, and quality dimensions.

The dataclass has **18** fields. Counted.

`RiskScorer`:

> XGBoost-based risk scoring model for migration quality prediction.
> Trained on historical migration data to predict post-migration defect probability with 85%+
> accuracy.

There was no historical migration data, no defect labels, no accuracy measurement, and — absent a
`model_path` argument that nothing in the repository supplies — no trained model. Four claims, all
of them false. This is the sentence that reached Exhibit D (§5).

### 2.5 The self-financing loop

`record_outcome()` also booked revenue as `lines_of_code * PRICING_TIERS[tier]` (`$0.50`–`$5.00`
per line) against a cost of `tokens_used / 1000 * LLM_COST_PER_1K_TOKENS[tier]`, accumulating a
`reinvestment_pool_usd` that gated a "model tier". It carried a correct honesty guard, added under
RELIAN-BENCH v1.0 and protected by `CLAUDE.md` forbidden-action 3 — unmeasured `semantic_score` or
`risk_score` meant no memory write and no budget mutation. That guard is not weakened by this work
package; it is removed along with the entire code path it guarded, which is strictly stronger. The
`tokens_used=0` argument the orchestrator passed was already measured-correct after R6 removed the
only LLM call, so the pricing arithmetic ran against a zero cost basis in any case.

## 3. Why deletion rather than repair

Three reasons, in order of weight.

1. **There are no labels.** Repairing the features would leave the target untouched, and the target
   is circular (§2.2). A risk model needs observed post-migration outcomes — defects found, rework
   hours, incidents — and Relian has never recorded one. No amount of feature engineering
   substitutes for a dependent variable that does not exist.
2. **Bench-first (R7).** *"No construct or language pair is 'supported' until RELIAN-BENCH covers
   it, is sealed, and CI held-out passes. Bench commit predates grammar merge."* The same order
   applies to a predictive model: the benchmark that would measure it must exist, and be sealed,
   before the thing it measures ships. RELIAN-BENCH v1.2 covers behavioural equivalence of
   transpiled COBOL. It does not cover migration risk prediction, and no vector in it could score
   such a model. Building the model first inverts the rule.
3. **A gated fabrication is still a fabrication.** The alternatives the R6 audit listed —
   `confidence = None`, or an R9 `SPECULATIVE` label on the heuristic branch — would each fix a
   reported *value* while leaving standing an architecture whose purpose is to manufacture that
   value. The XGBoost retrain loop would still run every 20 migrations on 12/18-constant rows. R1
   is being applied here at the level the defect lives at.

**What is not affected.** `src/assessment/risk.py` is the real risk product: rule-based tiering
over the assessment engine's measured inventory, every figure carrying a Trutina grade and a
provenance string (R9). It was never part of this limb, imports none of it, and is untouched by
this work package — as are `bench/`, `transpiler/` and `demo/`.

## 4. What the pipeline reports now

`MigrationOrchestrator._score_risk()` returns `{"overall_score": None, "risk_level": None}`
unconditionally.

This is not a new failure mode. It was already the method's exception path, reached whenever the
scorer raised, and it carried this comment:

```python
# Unmeasurable risk is None, not "medium". A made-up midpoint
# is indistinguishable from a real one downstream.
```

That reasoning was correct and is now simply always in force. `risk_score` remains a key in
`MigrationResult` and in `to_dict()` — the measured-or-None discipline is preserved, the value is
permanently `None`. A `None` that always means "not measured" is worth more than a number that
sometimes does.

### Consequential edits

Two changes outside the WP's bullet list, made because the deletion forced them.

- **`src/api/main.py` — `/api/v1/analyze`.** The endpoint imported `RiskScorer` directly and
  returned its `overall_score`, `risk_level` and `recommendations`. Deleting the package would
  have turned it into an unconditional HTTP 500. It now parses the source (a real operation) and
  returns `None` for both risk fields with an empty `recommendations` list, pointing callers at
  `/api/v1/assess/*` — the assessment engine, where risk is measured. Substituting a different
  heuristic here would have repeated the defect with new arithmetic.
- **`src/api/main.py` — `MigrationStatusResponse`.** `semantic_score`, `risk_score` and
  `test_coverage` were declared `float = 0.0` while `run_migration()` writes `None` into the job
  dict for every unmeasured metric — a pydantic `ValidationError` waiting on the first unmeasured
  run, and an R1 default-as-measurement besides. Making `risk_score` permanently `None` would have
  fired it on every completed migration. All three are now `Optional[float] = None`, matching the
  correction `AnalysisResponse` already carried, and the `, 0.0` fallbacks in
  `get_migration_status()` are gone: an absent key means the stage never ran, which is "not
  measured", not "zero".

## 5. Resulting Exhibit D position

| | Before | After |
|---|---|---|
| Rule-based risk scoring | measured | **measured** (unchanged — `src/assessment/risk.py`) |
| ML risk scoring | *"85% accuracy claim has no measured model"* — an open gap | **not built** — a stated scope boundary |

The gap closes by subtraction. Previously Exhibit D had to carry an unresolved discrepancy: a
shipped component advertising `85%+ accuracy` with nothing behind it, which is a finding an auditor
must chase and a claim counsel must qualify. There is now nothing to reconcile — the component does
not exist, and "we have not built ML risk scoring" is a complete and verifiable answer.

This is a better position than a repaired model would have produced, and materially better than a
relabelled one. "Not built" is auditable in one command; "built, but its confidence figure is now
`None` and its heuristic branch is graded SPECULATIVE" invites every follow-up question the
original claim did.

Quotable-capability matrix (R11) consequence: **no quote, report, or proposal template may
reference ML-based or predictive risk scoring.** Rule-based assessment risk remains quotable on its
existing measured basis.

## 6. Guard against reintroduction

`tests/test_no_fabricated_metrics.py` — 9 tests. Structure mirrors the R6 guard.

| # | Assertion | Mechanism |
|---|---|---|
| (a) | `src.ml` and `src.intelligence` are not importable | Out-of-process probe per package and per leaf module, classifying `absent` / `namespace-shell` / `importable`. Importability, not directory existence — see below. |
| (b) | `xgboost` is declared in no dependency group | `tomllib` parse of `dependencies` **and** every `optional-dependencies` group. |
| (b) | `xgboost` is pinned in no lock file | Line scan of `requirements.lock`. Declared intent and installed reality are asserted separately. |
| (b) | `xgboost` is imported by no first-party module | Static AST walk over `src/`, `transpiler/`, `tests/`, `demo/`, `bench/`, `scripts/`, `examples/`. `ast.walk` descends into function bodies — both original import sites were deferred imports inside a method, behind `try/except ImportError`. |
| (c) | `_score_risk` returns `None` for both keys | Direct call, on non-trivial source and on empty source, so a scorer reintroduced behind an "only when there's something to score" condition still fails. |
| (c) | The constructor takes no `intelligence` parameter | `inspect.signature`. That parameter was the seam the RSI engine was injected through. |

**Importability, not directory existence.** The R6 guard's equivalent test used to assert
`not path.exists()` and failed on 2026-08-20 on a developer box where the only survivor under the
deleted tree was a `__pycache__` directory `git clean` had not reached: no source, nothing
loadable, no exposure, and a red suite. PEP 420 means such a directory still imports, as a
namespace package with `__file__` of `None`, so a bare `pytest.raises(ModuleNotFoundError)` would
reproduce that false positive rather than avoid it. An empty shell is classified clean; anything
with a loadable module in it is not. On a clean checkout the outcome is `absent` — which is
`ModuleNotFoundError`.

### Planted-red verification

A guard that has never failed is not known to work. Each assertion was deliberately broken,
observed, and reverted.

| Planted defect | Result |
|---|---|
| `src/ml/` recreated with a loadable `risk_scorer.py` | **FAILED** as designed — 2 failed (package + leaf module) |
| `src/ml/__pycache__/risk_scorer.cpython-312.pyc` and nothing else | **PASSED**, as designed — the false-positive case, correctly classified `namespace-shell` |
| `xgboost>=2.0.0` re-added to `pyproject.toml` dependencies | **FAILED** as designed — 1 failed |
| `import xgboost as xgb` added *inside a method body* of `orchestrator._score_risk` | **FAILED** as designed — 2 failed |
| `_score_risk` returning `{"overall_score": 42.0, "risk_level": "medium"}` | **FAILED** as designed — 1 failed |
| `intelligence=None` re-added to `MigrationOrchestrator.__init__` | **FAILED** as designed — 1 failed |

All reverted; 9/9 green afterwards.

### One further guard, repaired rather than lost

`tests/test_no_generative_ai_in_transform_path.py::test_relative_imports_in_package_inits_resolve`
used `src/ml/__init__.py` as its fixture and carried
`if not init.is_file(): pytest.skip(...)`. Deleting `src/ml` would have turned an R6 regression
guard into a permanent silent skip — and drifted `tests.yml`'s pinned `EXPECTED_SKIPS: "10"` to 11,
failing the gate. The assertion now runs over every package `__init__.py` under `src/` that
contains a relative import (`src/core`, `src/parsers`, `src/blockchain`, `src/generators`), pinning
the resolver property itself rather than the one package that exposed the bug, and asserts
non-empty so it cannot decay into a skip again.

## 7. Suite state

Measured on the authoring box (CPython 3.12.3, GnuCOBOL absent, Temurin 21):

| | Before (`a32931d`) | After |
|---|---|---|
| passed | 278 | 275 |
| skipped | 20 | 20 |
| failed | 0 | 0 |

Net −3 passed: −12 from the deleted `tests/test_ml.py`, +9 from
`tests/test_no_fabricated_metrics.py`. The skip count is unchanged, which is the number that
matters — ten of the twenty are the absent-toolchain demo skips that CI does not have, and the
other ten are the fixture-shape skips `tests.yml` pins.

On CI, where GnuCOBOL is installed and the ten demo tests run instead of skipping, that is
**285 passed, 10 skipped, 0 failed** — down from 288 by the same net −3, with `EXPECTED_SKIPS: "10"`
unchanged and therefore still gating. The prose triple in `.github/workflows/tests.yml` and
`tests/conftest.py` was updated to 285; the historical entries in `docs/PHASE2_LOG.md` were not,
because that log is append-only and its entries record what was measured at the time. The measured
CI result and both workflow run URLs are appended there as a new WP-2.0.−3 entry.

## 8. Residual — `numpy`

`numpy` now has **zero import sites**. Both of its consumers were the two deleted modules, and the
justification currently written into `pyproject.toml` — *"both are genuinely imported"* — no longer
holds for it.

It is **left declared**, and flagged here rather than quietly kept, because removing it was outside
this work package's brief. On the WP-2.0.−1 precedent (`langchain`, `transformers`, `torch`,
`pandas`, all removed for having no import site) it is a candidate for the next pass. The comment
in `pyproject.toml` has been corrected to state its actual status rather than the void one.

## 9. Acceptance

| # | Criterion | Status |
|---|---|---|
| ① | `src/ml` and `src/intelligence` deleted, with their tests | **MET** — 5 files removed; guard (a) asserts unimportability |
| ② | Orchestrator import, ctor parameter and all five call sites removed; `_score_risk` returns `None`/`None` unconditionally | **MET** — guard (c); no reference to either package survives outside explanatory comments |
| ③ | `xgboost` removed from `pyproject.toml`; `requirements.lock` regenerated | **MET** — 140 pinned distributions → 38 |
| ④ | `jupyter`, `ipython`, `pre-commit` moved to a `notebook` extra | **MET** — `dev` is now exactly what the gate needs; `pip install -e ".[dev]"` after the lock resolves nothing new, verified, `pip check` clean |
| ⑤ | `tests/test_no_fabricated_metrics.py` added | **MET** — 9 tests, each planted-red verified (§6) |
| ⑥ | This document exists | **MET** |
| ⑦ | `bench/`, `transpiler/`, `src/assessment/`, `demo/` untouched | **MET** — no file under any of the four is modified |
