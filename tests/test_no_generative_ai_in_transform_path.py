"""WP-2.0.-2 — R6 guard: no generative-AI client in the transform path.

R6 states that no customer or State source code is given to a generative-AI
model during transformation. Before this work package that rule was violated by
construction: ``MigrationOrchestrator.migrate()`` awaited ``_analyze_semantics()``,
which built ``src.analysis.semantic.SemanticAnalyzer``, which instantiated
``AsyncOpenAI(api_key=os.getenv("OPENAI_API_KEY"))`` and put the source code in
the prompt. With no key set it raised and fell back honestly; with a key set,
customer source left the perimeter.

The call sites and the ``src/analysis`` package are gone. This module exists so
they cannot come back quietly. Three independent assertions:

(a) ``src.core.orchestrator`` imports cleanly in an interpreter where ``openai``,
    ``anthropic`` and ``neo4j`` are guaranteed absent. Run in a subprocess so
    the block holds regardless of what is installed on the developer's box.
(b) Static reachability: no module reachable from ``orchestrator.migrate`` or
    ``assessment.cli`` names any of the three, at module scope or inside a
    function body.
(c) ``pyproject.toml`` declares none of the three, in either the runtime set or
    any optional-dependency group.

(b) is the load-bearing one. (a) can be satisfied by a lazy import that only
fires at call time; (b) cannot, because it reads the source rather than running
it. If this file fails, escalate per §5 of the Phase 2 work package rather than
weakening the assertion.
"""

from __future__ import annotations

import ast
import json
import subprocess
import sys
from pathlib import Path
from typing import Dict, List, Set, Tuple

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]

# The three packages WP-2.0.-2 removes. `openai` and `anthropic` are the R6
# violation proper; `neo4j` rides along because its only consumer was the
# deleted analysis package and an on-prem install should not pull a graph-DB
# client by declaration.
FORBIDDEN: Tuple[str, ...] = ("openai", "anthropic", "neo4j")

# Entry points whose transitive import closure must stay clean.
ENTRY_POINTS: Tuple[str, ...] = ("src.core.orchestrator", "src.assessment.cli")


# --------------------------------------------------------------------------
# (a) the orchestrator imports with the generative-AI clients absent
# --------------------------------------------------------------------------

_IMPORT_PROBE = """
import sys

class _Blocker:
    \"\"\"Meta-path finder that makes the forbidden packages unimportable.\"\"\"
    BLOCKED = {blocked!r}

    def find_module(self, fullname, path=None):
        return self.find_spec(fullname, path)

    def find_spec(self, fullname, path=None, target=None):
        root = fullname.split(".")[0]
        if root in self.BLOCKED:
            raise ImportError(
                "R6 guard: %s is not installed in the customer perimeter" % fullname
            )
        return None

# Drop anything already imported, then refuse future imports.
for _name in list(sys.modules):
    if _name.split(".")[0] in _Blocker.BLOCKED:
        del sys.modules[_name]
sys.meta_path.insert(0, _Blocker())

import src.core.orchestrator as orch

# Not merely importable -- the public surface must be usable.
assert hasattr(orch, "MigrationOrchestrator")
assert hasattr(orch, "MigrationConfig")
assert hasattr(orch, "MigrationResult")

# The deleted stages must not have grown back.
for _gone in ("_analyze_semantics", "_generate_tests"):
    assert not hasattr(orch.MigrationOrchestrator, _gone), (
        "%s was deleted under R6 (WP-2.0.-2) and has been reintroduced" % _gone
    )

print("OK")
"""


def test_orchestrator_imports_without_generative_ai_clients() -> None:
    """(a) The transform path loads with openai/anthropic/neo4j unimportable."""
    proc = subprocess.run(
        [sys.executable, "-c", _IMPORT_PROBE.format(blocked=set(FORBIDDEN))],
        cwd=str(REPO_ROOT),
        capture_output=True,
        text=True,
        timeout=120,
    )
    assert proc.returncode == 0, (
        "src.core.orchestrator failed to import with "
        f"{', '.join(FORBIDDEN)} absent.\n"
        f"--- stdout ---\n{proc.stdout}\n--- stderr ---\n{proc.stderr}"
    )
    assert "OK" in proc.stdout


# --------------------------------------------------------------------------
# (b) static reachability from the transform and assessment entry points
# --------------------------------------------------------------------------

def _module_to_path(module: str) -> Path | None:
    """Resolve a dotted first-party module name to a file in this repo.

    Returns None for anything that is not a file in the repo -- third-party
    distributions, namespace packages, stdlib. Those are checked by name and
    not recursed into.
    """
    parts = module.split(".")
    candidate = REPO_ROOT.joinpath(*parts).with_suffix(".py")
    if candidate.is_file():
        return candidate
    package_init = REPO_ROOT.joinpath(*parts, "__init__.py")
    if package_init.is_file():
        return package_init
    return None


def _imports_of(path: Path) -> Set[str]:
    """Every module name imported by ``path``, at any nesting depth.

    ``ast.walk`` deliberately descends into function and method bodies: the
    original R6 violation was a deferred ``from src.analysis.semantic import
    SemanticAnalyzer`` inside a coroutine, which a module-scope-only scan would
    have missed entirely.
    """
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    found: Set[str] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                found.add(alias.name)
        elif isinstance(node, ast.ImportFrom):
            if node.level:
                # Relative import: rebuild the absolute name from the file's
                # own package so `from . import coverage` resolves correctly.
                #
                # The containing package is the parent directory in both
                # cases -- for `a/b/c.py` it is `a.b`, and for `a/b/__init__.py`
                # it is also `a.b` -- so dropping the final path component is
                # right for a module file and a package init alike. Special-
                # casing `__init__` here dropped a second component and
                # resolved `from .risk_scorer import X` in `src/ml/__init__.py`
                # to `src.risk_scorer`, a module that does not exist, so
                # anything reachable only through a package init was skipped.
                # level 1 is that package, level 2 its parent, and so on.
                rel_parts = path.relative_to(REPO_ROOT).with_suffix("").parts
                pkg_parts = list(rel_parts[:-1])
                base = pkg_parts[: len(pkg_parts) - (node.level - 1)]
                prefix = ".".join(base + ([node.module] if node.module else []))
            else:
                prefix = node.module or ""
            if prefix:
                found.add(prefix)
                for alias in node.names:
                    found.add(f"{prefix}.{alias.name}")
    return found


def _ancestors(module: str) -> List[str]:
    """Every parent package of ``module``, outermost first.

    Importing ``a.b.c`` executes ``a/__init__.py`` and then ``a/b/__init__.py``
    before ``c`` itself, so those files are on the runtime import path just as
    surely as the leaf module is. A walk that enqueues only the leaf leaves
    them unscanned -- and a forbidden client sitting in a package init is then
    invisible to this guard while still being imported at runtime.
    """
    parts = module.split(".")
    return [".".join(parts[:i]) for i in range(1, len(parts))]


def _reachable_closure(entry: str) -> Dict[str, List[str]]:
    """Walk the first-party import graph from ``entry``.

    Returns a map of every reachable module -> the chain of modules that led to
    it, so a failure names the path in, not just the offending module.
    """
    start = _module_to_path(entry)
    assert start is not None, f"entry point {entry} not found under {REPO_ROOT}"

    chains: Dict[str, List[str]] = {entry: [entry]}
    queue: List[str] = [entry]
    # The entry point's own parent packages are on its runtime path too.
    for ancestor in _ancestors(entry):
        if ancestor not in chains:
            chains[ancestor] = [entry, ancestor]
            queue.append(ancestor)
    while queue:
        module = queue.pop()
        path = _module_to_path(module)
        if path is None:
            continue  # third-party or stdlib: recorded, not traversed
        for imported in sorted(_imports_of(path)):
            # Every parent package of an imported module is imported with it.
            for name in _ancestors(imported) + [imported]:
                if name in chains:
                    continue
                chains[name] = chains[module] + [name]
                queue.append(name)
    return chains


@pytest.mark.parametrize("entry", ENTRY_POINTS)
def test_no_generative_ai_module_reachable(entry: str) -> None:
    """(b) No module reachable from the entry point imports a forbidden client."""
    chains = _reachable_closure(entry)

    violations: List[str] = []
    for module, chain in sorted(chains.items()):
        if module.split(".")[0] in FORBIDDEN:
            violations.append(" -> ".join(chain))

    assert not violations, (
        f"R6 violation: a generative-AI or graph-DB client is reachable from "
        f"{entry}. Import chains:\n  " + "\n  ".join(violations)
    )


@pytest.mark.parametrize("entry", ENTRY_POINTS)
def test_walk_covers_package_inits(entry: str) -> None:
    """The walk must scan package `__init__.py` files, not just leaf modules.

    Regression guard for a false negative found by review on PR #21. Importing
    ``src.ml.risk_scorer`` executes ``src/ml/__init__.py`` first, so a forbidden
    client placed there is imported at runtime -- but the walk enqueued only
    leaf modules, so the guard reported clean. A planted `import openai` in
    `src/ml/__init__.py` passed 5/5 before the fix.

    Asserting the closure is *closed under taking parents* pins the property
    rather than the one package that exposed it: every reachable dotted module
    must have each of its ancestor packages in the closure too.
    """
    chains = _reachable_closure(entry)
    missing = sorted(
        f"{ancestor} (parent of {module})"
        for module in chains
        for ancestor in _ancestors(module)
        if ancestor not in chains
    )
    assert not missing, (
        "reachability walk skipped parent packages, so a forbidden import in "
        "their __init__.py would be invisible:\n  " + "\n  ".join(missing)
    )


def test_relative_imports_in_package_inits_resolve() -> None:
    """Relative imports inside a package `__init__.py` resolve to real modules.

    Regression guard for an off-by-one found by review on PR #21: the resolver
    stripped `__init__` and then dropped a second component, turning
    `from .risk_scorer import ...` in `src/ml/__init__.py` into the
    non-existent `src.risk_scorer`. Anything reachable only through a package
    init was silently dropped from the walk.
    """
    init = REPO_ROOT / "src" / "ml" / "__init__.py"
    if not init.is_file():  # pragma: no cover - package may be removed later
        pytest.skip("src/ml/__init__.py not present")
    resolved = _imports_of(init)
    assert "src.ml.risk_scorer" in resolved, (
        "`from .risk_scorer import ...` in src/ml/__init__.py resolved to "
        f"{sorted(resolved)!r}; expected it to include src.ml.risk_scorer"
    )
    for name in resolved:
        if name.startswith("src.") and name.count(".") == 1:
            assert _module_to_path(name) is not None, (
                f"relative import resolved to {name!r}, which is not a file "
                f"in the repo -- the package prefix was computed wrong"
            )


# The probe runs out-of-process for the same reason (a) does: the parent
# interpreter has already imported half of `src`, and a stale entry in
# `sys.modules` or in the import caches would make the answer depend on
# collection order rather than on what is actually on disk.
_ANALYSIS_PROBE = """
import importlib, json, pathlib

report = {"outcome": None, "sources": [], "detail": ""}
try:
    module = importlib.import_module("src.analysis")
except ModuleNotFoundError as exc:
    report["outcome"] = "absent"
    report["detail"] = str(exc)
else:
    sources = sorted(
        str(path)
        for root in getattr(module, "__path__", [])
        for path in pathlib.Path(root).rglob("*.py")
    )
    report["sources"] = sources
    report["detail"] = repr(module)
    if getattr(module, "__file__", None) is None and not sources:
        report["outcome"] = "namespace-shell"
    else:
        report["outcome"] = "importable"
print(json.dumps(report))
"""


def test_deleted_analysis_package_is_not_importable() -> None:
    """The src/analysis package stays unimportable; git history preserves it.

    This used to assert ``not (REPO_ROOT / "src" / "analysis").exists()``, and
    that assertion is wrong: it tests the filesystem when what R6 constrains is
    the import graph. On 2026-08-20 it failed on a developer box where the only
    thing left under ``src/analysis`` was a ``__pycache__`` directory that
    ``git clean`` had not reached -- no source, nothing importable, no R6
    exposure whatsoever, and a red suite. A false positive on a guard this
    load-bearing is not harmless; it trains the reader to dismiss it.

    Importability is the property that matters, so importability is what is
    asserted. Note the ``namespace-shell`` branch: PEP 420 means a directory
    holding nothing but build artifacts still imports, as a namespace package
    with ``__file__`` of None and no loadable submodule. Verified directly --
    creating ``src/analysis/__pycache__/semantic.cpython-311.pyc`` and nothing
    else is enough for ``import src.analysis`` to succeed. So a bare
    ``pytest.raises(ModuleNotFoundError)`` would have reproduced the very false
    positive it was written to remove. An empty shell is classified as clean;
    anything with a loadable module in it is not.
    """
    proc = subprocess.run(
        [sys.executable, "-c", _ANALYSIS_PROBE],
        cwd=str(REPO_ROOT),
        capture_output=True,
        text=True,
        timeout=120,
    )
    assert proc.returncode == 0, (
        f"analysis-package probe failed to run.\n"
        f"--- stdout ---\n{proc.stdout}\n--- stderr ---\n{proc.stderr}"
    )
    report = json.loads(proc.stdout.strip().splitlines()[-1])

    assert report["outcome"] != "importable", (
        "R6 violation: src.analysis is importable again. It was deleted in "
        "WP-2.0.-2 because it handed customer source to a generative-AI model; "
        f"{report['detail']} exposes "
        f"{report['sources']}. See docs/R6_AUDIT_2026-08.md and escalate per "
        "Phase 2 §5 before restoring any of it."
    )

    # Belt and braces: the module that actually made the call must stay gone
    # even if the package directory lingers as an empty namespace shell.
    submodule = subprocess.run(
        [sys.executable, "-c",
         "import importlib\n"
         "try:\n"
         "    importlib.import_module('src.analysis.semantic')\n"
         "except ModuleNotFoundError as exc:\n"
         "    print('absent:', exc)\n"
         "else:\n"
         "    raise SystemExit('IMPORTED')\n"],
        cwd=str(REPO_ROOT),
        capture_output=True,
        text=True,
        timeout=120,
    )
    assert submodule.returncode == 0 and submodule.stdout.startswith("absent:"), (
        "R6 violation: src.analysis.semantic imported. SemanticAnalyzer is the "
        "class that put customer source in an OpenAI prompt.\n"
        f"--- stdout ---\n{submodule.stdout}\n--- stderr ---\n{submodule.stderr}"
    )


# --------------------------------------------------------------------------
# (c) the dependency set declares none of the three
# --------------------------------------------------------------------------

def _declared_distributions() -> Dict[str, str]:
    """Map normalized distribution name -> the raw requirement string."""
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
        # Strip extras, version specifiers, environment markers, and URLs.
        name = requirement.split(";")[0].split("@")[0].strip()
        for separator in ("[", "=", ">", "<", "!", "~", " "):
            name = name.split(separator)[0]
        normalized = name.strip().lower().replace("_", "-")
        if normalized:
            declared[normalized] = requirement
    return declared


def test_pyproject_declares_no_generative_ai_dependencies() -> None:
    """(c) openai, anthropic and neo4j appear in no dependency group."""
    declared = _declared_distributions()
    reintroduced = {
        name: declared[name] for name in FORBIDDEN if name in declared
    }
    assert not reintroduced, (
        "R6 violation: pyproject.toml reintroduces "
        f"{reintroduced}. These were removed in WP-2.0.-2 so that an on-prem "
        "install inside a customer perimeter cannot pull a generative-AI "
        "client. See docs/R6_AUDIT_2026-08.md; escalate per Phase 2 §5 before "
        "adding any of them back."
    )
