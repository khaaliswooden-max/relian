"""WP-2.2 D15 — ``src/discovery/`` never invokes a compiler, and this asserts it.

**Why this file is the most important one in the work package.** RELIAN-
DISCOVERY-BENCH v0.1's oracle *is* GnuCOBOL 3.1.2's byte layout, measured by
compiling probe programs. If the layout engine shelled out to ``cobc``, the
round-trip in ``tests/test_layout_roundtrip.py`` would be 100% by
construction: the oracle would be grading itself. Every capability claim built
on that number would be circular, and — this is the part that matters — the
circularity would be **invisible in a green result**. A gate wearing the
costume of a measurement is the exact defect this build has caught seven times.

The two artifacts agree *because* they share no code and no process.

It is also a shipping constraint, not only a methodological one. The product
runs in the customer perimeter (R12), on machines that have no GnuCOBOL and
whose COBOL is IBM Enterprise rather than GnuCOBOL. An engine that needs a
compiler is not shippable.

**AST, not grep.** A grep for ``subprocess`` is defeated by
``__import__("sub" + "process")``, and a grep for ``cobc`` by ``"co" + "bc"``.
This walks the parsed syntax tree of every module in the package and of every
first-party module reachable from it, and fails on the *shapes* that could
start a process — including the dynamic-import and ``eval`` escape hatches
that a string split would need.

Four independent assertions:

(a) **Static, over the package.** No import of a process-spawning module, no
    call into one, no dynamic import, no ``eval``/``exec``/``compile``.
(b) **Static, over the transitive first-party closure.** ``src.discovery``
    importing a clean-looking helper that itself shells out defeats (a).
(c) **Runtime.** The package imports and computes a real layout end-to-end in
    an interpreter where ``subprocess``, ``ctypes``, ``pty`` and friends are
    unimportable. A lazy import that only fires at call time survives (a) in
    principle; it does not survive this.
(d) **Negative controls.** The detector is shown to bite on planted modules
    that do each forbidden thing. A guard that has never failed is a guard
    whose failure path is unproven.

The round-trip harness is under the same rule (D15, second paragraph): it
reads the committed ``oracle.json``, and re-deriving it inside the grading step
would reintroduce the dependency through the back door. That is asserted here
too.
"""

from __future__ import annotations

import ast
import subprocess          # this TEST may spawn; the package under test may not
import sys
import textwrap
from pathlib import Path
from typing import Dict, List, Set, Tuple

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]
PACKAGE_DIR = REPO_ROOT / "src" / "discovery"
HARNESS = REPO_ROOT / "tests" / "test_layout_roundtrip.py"

#: Modules that can start a process, load native code, or replace the current
#: image. Importing any of them inside the engine is a finding on its own —
#: there is no legitimate use in a static text calculator.
FORBIDDEN_MODULES: Tuple[str, ...] = (
    "subprocess",
    "_posixsubprocess",
    "ctypes",
    "pty",
    "popen2",
    "commands",
    "multiprocessing",
    "asyncio.subprocess",
    "distutils.spawn",
    "sh",
    "plumbum",
    "pexpect",
)

#: ``module.attribute`` shapes that spawn or locate an executable. ``shutil``
#: itself is fine — copying a file is not running a compiler — so it is the
#: attribute that is forbidden, not the import.
FORBIDDEN_ATTRIBUTES: Tuple[Tuple[str, str], ...] = tuple(
    ("os", name) for name in (
        "system", "popen", "fork", "forkpty", "spawnl", "spawnle", "spawnlp",
        "spawnlpe", "spawnv", "spawnve", "spawnvp", "spawnvpe", "posix_spawn",
        "posix_spawnp", "execl", "execle", "execlp", "execlpe", "execv",
        "execve", "execvp", "execvpe", "startfile",
    )
) + (
    ("shutil", "which"),
    ("distutils", "spawn"),
)

#: Bare-name calls that are the same thing under ``from os import system``.
FORBIDDEN_BARE_CALLS: frozenset = frozenset(
    name for _mod, name in FORBIDDEN_ATTRIBUTES
) | {"which"}

#: Escape hatches. Every one of these can reach a forbidden module without
#: naming it in a way (a) or a grep could see, so the package simply may not
#: use them. Nothing in a layout calculator needs to.
FORBIDDEN_BUILTINS: frozenset = frozenset({
    "__import__", "eval", "exec", "compile",
})

#: INVOCABLE COBOL compiler binaries. This is a belt-and-braces check over the
#: import/call assertions above, aimed at the one thing a literal can do that
#: those cannot see: name an executable for a dynamically built call.
#:
#: Two deliberate narrowings, both of which are the difference between a guard
#: and a superstition:
#:
#: * **Docstrings are exempt.** A docstring cannot be executed. Forbidding the
#:   word would push the reasoning about why this engine avoids a compiler out
#:   of the modules that most need to carry it.
#: * **Product names are not tokens.** ``GnuCOBOL 3.1.2.0`` in a failure
#:   message is *provenance* — R9 requires exactly that, and the D16
#:   accusation block is unreadable without it. ``cobc`` is a binary you can
#:   hand to a shell; ``GnuCOBOL`` is not.
COMPILER_TOKENS: Tuple[str, ...] = ("cobc", "cobcrun", "cob2", "igycrctl")


# --------------------------------------------------------------------------
# The detector
# --------------------------------------------------------------------------

def _docstring_nodes(tree: ast.AST) -> Set[int]:
    """ids of the Constant nodes that are docstrings."""
    out: Set[int] = set()
    for node in ast.walk(tree):
        if isinstance(node, (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)):
            body = getattr(node, "body", None)
            if body and isinstance(body[0], ast.Expr) and isinstance(body[0].value, ast.Constant):
                if isinstance(body[0].value.value, str):
                    out.add(id(body[0].value))
    return out


def scan_source(source: str, label: str) -> List[str]:
    """Return every compiler-invocation finding in ``source``. Empty is clean."""
    tree = ast.parse(source, filename=label)
    docstrings = _docstring_nodes(tree)
    findings: List[str] = []

    def where(node: ast.AST) -> str:
        return f"{label}:{getattr(node, 'lineno', '?')}"

    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                root = alias.name.split(".")[0]
                if alias.name in FORBIDDEN_MODULES or root in FORBIDDEN_MODULES:
                    findings.append(f"{where(node)}: imports {alias.name}")
        elif isinstance(node, ast.ImportFrom):
            module = node.module or ""
            root = module.split(".")[0]
            if module in FORBIDDEN_MODULES or root in FORBIDDEN_MODULES:
                findings.append(f"{where(node)}: imports from {module}")
            if module in ("os", "shutil", "distutils"):
                for alias in node.names:
                    if (module, alias.name) in FORBIDDEN_ATTRIBUTES:
                        findings.append(
                            f"{where(node)}: imports {module}.{alias.name}"
                        )
        elif isinstance(node, ast.Attribute):
            value = node.value
            if isinstance(value, ast.Name) and (value.id, node.attr) in FORBIDDEN_ATTRIBUTES:
                findings.append(f"{where(node)}: names {value.id}.{node.attr}")
        elif isinstance(node, ast.Name):
            if node.id in FORBIDDEN_BUILTINS:
                findings.append(f"{where(node)}: names the escape hatch {node.id}")
            elif isinstance(node.ctx, ast.Load) and node.id in FORBIDDEN_BARE_CALLS:
                findings.append(f"{where(node)}: names {node.id}")
        elif isinstance(node, ast.Constant) and isinstance(node.value, str):
            if id(node) in docstrings:
                continue
            lowered = node.value.lower()
            for token in COMPILER_TOKENS:
                if token in lowered:
                    findings.append(
                        f"{where(node)}: a non-docstring literal contains {token!r}"
                    )
    return findings


def _first_party_imports(source: str) -> Set[str]:
    """Dotted first-party module names this source imports."""
    out: Set[str] = set()
    tree = ast.parse(source)
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                if alias.name.split(".")[0] in ("src", "tests"):
                    out.add(alias.name)
        elif isinstance(node, ast.ImportFrom):
            if node.level:
                continue                    # relative: same package, already walked
            module = node.module or ""
            if module.split(".")[0] in ("src", "tests"):
                out.add(module)
                for alias in node.names:
                    out.add(f"{module}.{alias.name}")
    return out


def _module_path(dotted: str) -> Path | None:
    base = REPO_ROOT / Path(*dotted.split("."))
    if base.with_suffix(".py").is_file():
        return base.with_suffix(".py")
    if (base / "__init__.py").is_file():
        return base / "__init__.py"
    return None


def _package_files() -> List[Path]:
    files = sorted(PACKAGE_DIR.rglob("*.py"), key=lambda p: p.as_posix())
    assert files, f"no modules found under {PACKAGE_DIR} — the guard would pass vacuously"
    return files


# --------------------------------------------------------------------------
# (a) the package itself
# --------------------------------------------------------------------------

def test_package_is_not_empty() -> None:
    """A guard over an empty package is a guard that proves nothing."""
    files = _package_files()
    names = {p.name for p in files}
    assert {"__init__.py", "copybook.py", "layout.py"} <= names, sorted(names)


@pytest.mark.parametrize("path", _package_files(), ids=lambda p: p.name)
def test_module_never_invokes_a_compiler(path: Path) -> None:
    findings = scan_source(path.read_text(encoding="utf-8"), path.relative_to(REPO_ROOT).as_posix())
    assert not findings, (
        "src/discovery/ must never invoke a compiler (WP-2.2 D15). If it did, "
        "the layout round-trip would be 100% by construction and the oracle "
        "would be grading itself.\n  " + "\n  ".join(findings)
    )


# --------------------------------------------------------------------------
# (b) the transitive first-party closure
# --------------------------------------------------------------------------

def _closure() -> Dict[str, Path]:
    seen: Dict[str, Path] = {}
    queue = [f"src.discovery.{p.stem}" if p.stem != "__init__" else "src.discovery"
             for p in _package_files()]
    while queue:
        dotted = queue.pop()
        if dotted in seen:
            continue
        path = _module_path(dotted)
        if path is None:
            continue
        seen[dotted] = path
        queue.extend(_first_party_imports(path.read_text(encoding="utf-8")))
    return seen


def test_transitive_first_party_closure_is_compiler_free() -> None:
    """(b) A clean-looking helper that shells out defeats (a). Close that."""
    closure = _closure()
    assert "src.assessment.intake" in closure, (
        "the closure walker found no reachable assessment module, so it is not "
        "actually walking anything — src/discovery/ reuses assessment.intake"
    )
    findings: List[str] = []
    for dotted, path in sorted(closure.items()):
        findings.extend(scan_source(path.read_text(encoding="utf-8"), dotted))
    assert not findings, (
        "a module reachable from src/discovery/ can invoke a compiler:\n  "
        + "\n  ".join(findings)
    )


def test_roundtrip_harness_does_not_regenerate_the_oracle() -> None:
    """D15: the harness reads the committed oracle. It does not re-derive it.

    Re-running ``cobc`` inside the grading step would reintroduce the
    dependency through the back door — the harness would be comparing the
    engine against a fresh compile rather than against a sealed artifact.
    """
    assert HARNESS.is_file(), f"{HARNESS} is missing"
    source = HARNESS.read_text(encoding="utf-8")
    tree = ast.parse(source)
    docstrings = _docstring_nodes(tree)
    findings: List[str] = []
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                if alias.name.split(".")[0] in FORBIDDEN_MODULES:
                    findings.append(f"line {node.lineno}: imports {alias.name}")
        elif isinstance(node, ast.ImportFrom):
            if (node.module or "").split(".")[0] in FORBIDDEN_MODULES:
                findings.append(f"line {node.lineno}: imports from {node.module}")
        elif isinstance(node, ast.Constant) and isinstance(node.value, str):
            if id(node) in docstrings:
                continue
            lowered = node.value.lower()
            for token in COMPILER_TOKENS:
                if token in lowered:
                    findings.append(f"line {node.lineno}: literal contains {token!r}")
    assert not findings, (
        "tests/test_layout_roundtrip.py must grade against the SEALED oracle, "
        "never against a fresh compile:\n  " + "\n  ".join(findings)
    )


# --------------------------------------------------------------------------
# (c) runtime — the package works with the process-spawning modules absent
# --------------------------------------------------------------------------

_RUNTIME_PROBE = """
import sys

BLOCKED = {blocked!r}

class _Blocker:
    def find_module(self, fullname, path=None):
        return self.find_spec(fullname, path)

    def find_spec(self, fullname, path=None, target=None):
        if fullname.split(".")[0] in BLOCKED:
            raise ImportError(
                "WP-2.2 D15: %s is not importable inside the layout engine" % fullname
            )
        return None

for _name in list(sys.modules):
    if _name.split(".")[0] in BLOCKED:
        del sys.modules[_name]
sys.meta_path.insert(0, _Blocker())

from pathlib import Path
from src.discovery import compute, resolve

layout = compute(Path("discovery-bench/corpus/D10_sync.cpy"))
assert layout.group == "D10-SYNC", layout.group
assert layout.group_length == 25, layout.group_length
assert [g.span() for g in layout.gaps] == [(2, 4), (10, 10), (16, 16)], layout.gaps

resolution = resolve(Path("discovery-bench/corpus"))
assert not resolution.unresolved, resolution.unresolved

print("OK")
"""


def test_engine_computes_a_layout_with_process_spawning_unimportable() -> None:
    """(c) The whole pipeline runs with the escape routes closed at import time."""
    blocked = set(FORBIDDEN_MODULES) - {"multiprocessing"}   # pytest's own plugins may want it
    proc = subprocess.run(
        [sys.executable, "-c", _RUNTIME_PROBE.format(blocked=blocked)],
        cwd=str(REPO_ROOT), capture_output=True, text=True, timeout=180,
    )
    assert proc.returncode == 0, (
        "src.discovery failed with the process-spawning modules unimportable.\n"
        f"--- stdout ---\n{proc.stdout}\n--- stderr ---\n{proc.stderr}"
    )
    assert "OK" in proc.stdout


# --------------------------------------------------------------------------
# (d) negative controls — the detector is shown to bite
# --------------------------------------------------------------------------

_PLANTED = {
    "import subprocess": "import subprocess\nsubprocess.run(['x'])\n",
    "from subprocess import run": "from subprocess import run\nrun(['x'])\n",
    "os.system": "import os\nos.system('x')\n",
    "os.popen": "import os\nos.popen('x')\n",
    "os.execv": "import os\nos.execv('/bin/x', ['x'])\n",
    "shutil.which": "import shutil\nshutil.which('x')\n",
    "from shutil import which": "from shutil import which\nwhich('x')\n",
    "ctypes": "import ctypes\n",
    "dynamic import": "m = __import__('sub' + 'process')\n",
    "eval": "eval('1')\n",
    "split compiler literal": "TOOL = 'cobc'\n",
}


@pytest.mark.parametrize("label", sorted(_PLANTED), ids=lambda s: s.replace(" ", "-"))
def test_detector_bites_on_a_planted_violation(label: str) -> None:
    """(d) Each forbidden shape is caught. A guard with an unproven failure
    path is a guard nobody should trust."""
    findings = scan_source(_PLANTED[label], f"<planted:{label}>")
    assert findings, f"the detector did NOT catch the planted {label!r}"


def test_detector_accepts_the_real_engine_shape() -> None:
    """The converse control: a plain static calculator is not flagged.

    Without this, a detector that flagged everything would pass (d) and tell
    you nothing.
    """
    clean = textwrap.dedent(
        '''
        """A docstring may discuss the compiler this engine deliberately avoids."""
        import math
        import re
        from pathlib import Path

        def size(digits: int) -> int:
            return math.ceil((digits + 1) / 2)
        '''
    )
    assert scan_source(clean, "<clean>") == []
