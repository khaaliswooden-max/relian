"""WP-2.0.-3 — R1 guard: the fabricated-metric ML limb stays deleted.

R1 states that a metric is measured or None — never a constant, default, or
estimate presented as a measurement. Before this work package that rule was
violated at the architecture level rather than in any single value:

``src/intelligence/migration_intelligence.py`` built XGBoost training rows in
``retrain_risk_model()`` whose features were invented on the spot —
``int(p.cyclomatic_complexity * 1.2)`` commented ``# cognitive (approx)``,
three hardcoded ``0`` placeholders for fields the pattern store never held
(``max_nesting_depth``, ``num_functions``, ``num_dependencies``, and further
zeros besides), and ``p.lines_of_code / max(1, 5)`` standing in for average
function length — then retrained a model on them every ``RETRAIN_INTERVAL = 20``
migrations and assigned the result to ``risk_scorer.model``.

``src/ml/risk_scorer.py`` supplied the other half: ``RiskScorer.score()``
attached ``confidence = 0.85`` when a model was loaded and ``0.70`` when it fell
back to the heuristic, both hardcoded literals rather than anything the model
reported, and ``CodeMetrics``'s docstring advertised "200+ metrics across
complexity, coupling, and quality dimensions" over a dataclass holding
eighteen fields.

Both packages are deleted. This module exists so they cannot come back quietly.
Three independent assertions, mirroring the structure of the R6 guard in
tests/test_no_generative_ai_in_transform_path.py:

(a) ``src.ml`` and ``src.intelligence`` are not importable.
(b) ``xgboost`` is declared in no dependency group, pinned in no lock file, and
    imported by no first-party module.
(c) ``MigrationOrchestrator._score_risk`` returns None for both keys, and the
    constructor no longer accepts an ``intelligence`` parameter.

The rule-based risk product in ``src/assessment/risk.py`` is untouched and is
deliberately NOT covered here: it is measured, provenance-carrying, and remains
the answer to "what is this codebase's risk". See
docs/R1_ML_DISPOSITION_2026-08.md. If this file fails, escalate per §5 of the
Phase 2 work package rather than weakening the assertion.
"""

from __future__ import annotations

import ast
import asyncio
import json
import subprocess
import sys
from pathlib import Path
from typing import Dict, List, Set

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]

# The two packages WP-2.0.-3 removes.
REMOVED_PACKAGES = ("src.ml", "src.intelligence")

# The module inside each that did the fabricating. Asserted separately from the
# package so an empty leftover directory cannot mask a restored module.
REMOVED_MODULES = ("src.ml.risk_scorer", "src.intelligence.migration_intelligence")

# The distribution that existed only to serve them.
FORBIDDEN_DISTRIBUTION = "xgboost"

# First-party trees scanned for import sites. `bench/` is read, never written
# (CLAUDE.md §4).
FIRST_PARTY_DIRS = (
    "src",
    "transpiler",
    "tests",
    "demo",
    "bench",
    "scripts",
    "examples",
)


# --------------------------------------------------------------------------
# (a) neither package is importable
# --------------------------------------------------------------------------

# Runs out-of-process for the same reason the R6 guard's probe does: the parent
# interpreter has already imported half of `src`, so a stale `sys.modules` entry
# would make the answer depend on collection order rather than on what is on
# disk.
_IMPORT_PROBE = """
import importlib, json, pathlib, sys

report = {}
for name in %(targets)r:
    entry = {"outcome": None, "sources": [], "detail": ""}
    try:
        module = importlib.import_module(name)
    except ModuleNotFoundError as exc:
        entry["outcome"] = "absent"
        entry["detail"] = str(exc)
    except Exception as exc:
        entry["outcome"] = "importable"
        entry["detail"] = "%%s: %%s" %% (type(exc).__name__, exc)
    else:
        sources = sorted(
            str(path)
            for root in getattr(module, "__path__", [])
            for path in pathlib.Path(root).rglob("*.py")
        )
        entry["sources"] = sources
        entry["detail"] = repr(module)
        if getattr(module, "__file__", None) is None and not sources:
            entry["outcome"] = "namespace-shell"
        else:
            entry["outcome"] = "importable"
    report[name] = entry
print(json.dumps(report))
"""


def _probe(targets: tuple) -> Dict[str, dict]:
    """Import each dotted name in a fresh interpreter; classify the outcome."""
    proc = subprocess.run(
        [sys.executable, "-c", _IMPORT_PROBE % {"targets": list(targets)}],
        cwd=str(REPO_ROOT),
        capture_output=True,
        text=True,
        timeout=120,
    )
    assert proc.returncode == 0, (
        "import probe failed to run.\n"
        f"--- stdout ---\n{proc.stdout}\n--- stderr ---\n{proc.stderr}"
    )
    return json.loads(proc.stdout.strip().splitlines()[-1])


@pytest.mark.parametrize("package", REMOVED_PACKAGES)
def test_removed_package_is_not_importable(package: str) -> None:
    """(a) `import src.ml` / `import src.intelligence` raise ModuleNotFoundError.

    Importability is asserted, not directory existence — the same choice, and
    for the same measured reason, as the R6 guard's
    ``test_deleted_analysis_package_is_not_importable``. That test used to
    assert ``not path.exists()`` and failed on 2026-08-20 on a developer box
    where the only thing left under the deleted tree was a ``__pycache__``
    directory ``git clean`` had not reached: no source, nothing loadable, no R1
    exposure, and a red suite. A false positive on a load-bearing guard trains
    the reader to dismiss it.

    Hence the ``namespace-shell`` branch. PEP 420 means a directory holding
    nothing but build artifacts still imports, as a namespace package with
    ``__file__`` of None and no loadable submodule, so a bare
    ``pytest.raises(ModuleNotFoundError)`` would reproduce exactly that false
    positive. On a clean checkout the outcome is ``absent`` — which is
    ModuleNotFoundError, the literal property this work package asks for. An
    empty shell is classified clean; anything with a loadable module is not.
    """
    report = _probe(REMOVED_PACKAGES)[package]

    assert report["outcome"] != "importable", (
        f"R1 violation: {package} is importable again. It was deleted in "
        "WP-2.0.-3 because it presented fabricated features and hardcoded "
        f"confidence constants as measurements; {report['detail']} exposes "
        f"{report['sources']}. See docs/R1_ML_DISPOSITION_2026-08.md and "
        "escalate per Phase 2 §5 before restoring any of it."
    )


@pytest.mark.parametrize("module", REMOVED_MODULES)
def test_removed_module_is_not_importable(module: str) -> None:
    """(a) The two modules that did the fabricating stay gone.

    Belt and braces over the package assertion above: this holds even if the
    package directory lingers as an empty namespace shell.
    """
    report = _probe(REMOVED_MODULES)[module]

    assert report["outcome"] == "absent", (
        f"R1 violation: {module} imported. This is the module that "
        "fabricated the feature rows / the hardcoded confidence constants.\n"
        f"outcome={report['outcome']!r} detail={report['detail']}"
    )


# --------------------------------------------------------------------------
# (b) xgboost is declared nowhere and imported nowhere
# --------------------------------------------------------------------------

def _declared_distributions() -> Dict[str, str]:
    """Map normalized distribution name -> the raw requirement string.

    Covers ``[project.dependencies]`` and every group under
    ``[project.optional-dependencies]``, so moving a name into an extra does not
    hide it from this check.
    """
    if sys.version_info >= (3, 11):
        import tomllib
    else:  # pragma: no cover - project requires >=3.11
        pytest.skip("tomllib requires Python 3.11+")

    with (REPO_ROOT / "pyproject.toml").open("rb") as handle:
        data = tomllib.load(handle)

    project = data.get("project", {})
    requirements: List[str] = list(project.get("dependencies", []))
    for group in project.get("optional-dependencies", {}).values():
        requirements.extend(group)

    declared: Dict[str, str] = {}
    for requirement in requirements:
        name = requirement.split(";")[0].split("@")[0].strip()
        for separator in ("[", "=", ">", "<", "!", "~", " "):
            name = name.split(separator)[0]
        normalized = name.strip().lower().replace("_", "-")
        if normalized:
            declared[normalized] = requirement
    return declared


def test_pyproject_declares_no_xgboost() -> None:
    """(b) xgboost appears in no dependency group in pyproject.toml.

    The check parses the declarations rather than grepping the file, and that
    distinction is deliberate: pyproject.toml's comments name `xgboost`
    explicitly, in the note recording why it was removed. A raw text scan would
    force that rationale to be deleted to stay green, which is precisely
    backwards — the reason a dependency left is worth more to the next reader
    than the absence itself. Same approach as the R6 guard's
    ``test_pyproject_declares_no_generative_ai_dependencies``.
    """
    declared = _declared_distributions()

    assert FORBIDDEN_DISTRIBUTION not in declared, (
        "R1 violation: pyproject.toml reintroduces "
        f"{FORBIDDEN_DISTRIBUTION!r} as {declared.get(FORBIDDEN_DISTRIBUTION)!r}. "
        "It was removed in WP-2.0.-3 together with its only two import sites, "
        "both of which trained or scored a model on fabricated features. See "
        "docs/R1_ML_DISPOSITION_2026-08.md and escalate per Phase 2 §5 before "
        "adding it back."
    )


def test_lockfile_pins_no_xgboost() -> None:
    """(b) xgboost is not pinned in requirements.lock either.

    pyproject declares intent; the lock file is what CI actually installs. A
    name can survive in the lock after leaving pyproject if the lock is not
    regenerated, so both are asserted.
    """
    lock = REPO_ROOT / "requirements.lock"
    assert lock.is_file(), "requirements.lock is missing"

    pinned: Set[str] = set()
    for line in lock.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#") or stripped.startswith("--"):
            continue
        name = stripped.split("==")[0].split(";")[0].strip()
        for separator in ("[", "=", ">", "<", "!", "~", " ", "\\"):
            name = name.split(separator)[0]
        if name:
            pinned.add(name.strip().lower().replace("_", "-"))

    assert FORBIDDEN_DISTRIBUTION not in pinned, (
        f"R1 violation: requirements.lock still pins {FORBIDDEN_DISTRIBUTION!r}. "
        "Regenerate it with the command in the file's header after removing the "
        "declaration from pyproject.toml."
    )


def _python_files() -> List[Path]:
    """Every first-party ``.py`` file, excluding build and vendor output."""
    excluded = {"__pycache__", "node_modules", ".git", "htmlcov", ".venv", "venv"}
    files: List[Path] = []
    for directory in FIRST_PARTY_DIRS:
        root = REPO_ROOT / directory
        if not root.is_dir():
            continue
        for path in root.rglob("*.py"):
            if excluded.isdisjoint(path.parts):
                files.append(path)
    return sorted(files)


def test_no_first_party_module_imports_xgboost() -> None:
    """(b) No first-party module imports xgboost, at any nesting depth.

    ``ast.walk`` descends into function bodies on purpose: both original import
    sites were deferred imports inside a method (``import xgboost as xgb`` sat
    inside ``retrain_risk_model()``, behind a try/except ImportError), which a
    module-scope-only scan would miss entirely.

    Matching on parsed import nodes rather than on text means this file — which
    names the distribution repeatedly in prose — does not trip its own guard.
    """
    violations: List[str] = []
    for path in _python_files():
        try:
            tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
        except SyntaxError:  # pragma: no cover - fixtures may be intentionally bad
            continue
        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                names = [alias.name for alias in node.names]
            elif isinstance(node, ast.ImportFrom):
                names = [node.module] if node.module else []
            else:
                continue
            for name in names:
                if name and name.split(".")[0] == FORBIDDEN_DISTRIBUTION:
                    rel = path.relative_to(REPO_ROOT)
                    violations.append(f"{rel}:{node.lineno}: {name}")

    assert not violations, (
        f"R1 violation: {FORBIDDEN_DISTRIBUTION} is imported again by:\n  "
        + "\n  ".join(violations)
        + "\nThe model it trained had no measured features and no labelled "
        "outcomes. See docs/R1_ML_DISPOSITION_2026-08.md."
    )


# --------------------------------------------------------------------------
# (c) the orchestrator scores no risk
# --------------------------------------------------------------------------

def test_score_risk_returns_none_for_both_keys() -> None:
    """(c) ``_score_risk`` is None/None — permanently, not as a fallback.

    This was already the method's exception path. WP-2.0.-3 made it the only
    path: the honest permanent value, returned unconditionally. A None that
    always means "not measured" is worth more than a number that sometimes does.
    """
    from src.core.orchestrator import MigrationOrchestrator

    orchestrator = MigrationOrchestrator()
    source = (
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. RISKY.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           IF A > B THEN DISPLAY 'X' END-IF.\n"
        "           GO TO MAIN-PARA.\n"
        "           STOP RUN.\n"
    )

    assessment = asyncio.run(orchestrator._score_risk(None, source))

    assert assessment == {"overall_score": None, "risk_level": None}, (
        "R1 violation: _score_risk returned a value. Risk scoring was deleted "
        f"in WP-2.0.-3; got {assessment!r}. Measured, rule-based risk lives in "
        "src/assessment/risk.py and carries a Trutina grade per figure (R9)."
    )

    # Non-trivial input must not change the answer: a scorer reintroduced
    # behind a "only when there is something to score" condition would still
    # be a fabricated metric.
    trivial = asyncio.run(orchestrator._score_risk(None, ""))
    assert trivial == {"overall_score": None, "risk_level": None}, (
        f"_score_risk is input-sensitive: empty source gave {trivial!r}"
    )


def test_orchestrator_takes_no_intelligence_parameter() -> None:
    """(c) The RSI engine cannot be injected back through the constructor.

    ``MigrationOrchestrator(intelligence=...)`` was the seam that wired the
    pattern store and the retrain loop into the pipeline. Deleting the package
    without closing the seam would leave a parameter that only a reintroduced
    package could satisfy.
    """
    import inspect

    from src.core.orchestrator import MigrationOrchestrator

    parameters = inspect.signature(MigrationOrchestrator.__init__).parameters
    assert "intelligence" not in parameters, (
        "R1 violation: MigrationOrchestrator.__init__ accepts an "
        "'intelligence' parameter again. The RSI + self-financing engine it "
        "injected was deleted in WP-2.0.-3."
    )
    assert list(parameters) == ["self"], (
        f"unexpected constructor signature: {list(parameters)!r}"
    )

    orchestrator = MigrationOrchestrator()
    assert not hasattr(orchestrator, "_intelligence"), (
        "R1 violation: MigrationOrchestrator still carries an _intelligence "
        "attribute."
    )
