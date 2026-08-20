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
                pkg_parts = path.relative_to(REPO_ROOT).with_suffix("").parts
                if path.name == "__init__.py":
                    pkg_parts = pkg_parts[:-1]
                base = list(pkg_parts[: len(pkg_parts) - (node.level - 1) - 1])
                prefix = ".".join(base + ([node.module] if node.module else []))
            else:
                prefix = node.module or ""
            if prefix:
                found.add(prefix)
                for alias in node.names:
                    found.add(f"{prefix}.{alias.name}")
    return found


def _reachable_closure(entry: str) -> Dict[str, List[str]]:
    """Walk the first-party import graph from ``entry``.

    Returns a map of every reachable module -> the chain of modules that led to
    it, so a failure names the path in, not just the offending module.
    """
    start = _module_to_path(entry)
    assert start is not None, f"entry point {entry} not found under {REPO_ROOT}"

    chains: Dict[str, List[str]] = {entry: [entry]}
    queue: List[str] = [entry]
    while queue:
        module = queue.pop()
        path = _module_to_path(module)
        if path is None:
            continue  # third-party or stdlib: recorded, not traversed
        for imported in sorted(_imports_of(path)):
            if imported in chains:
                continue
            chains[imported] = chains[module] + [imported]
            queue.append(imported)
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


def test_deleted_analysis_package_is_gone() -> None:
    """The src/analysis package stays deleted; git history preserves it."""
    assert not (REPO_ROOT / "src" / "analysis").exists(), (
        "src/analysis was deleted under R6 (WP-2.0.-2) and has reappeared"
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
