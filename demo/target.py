"""The migrated side: build and execute the generated Java.

Symmetric with :mod:`demo.oracle`. The Java is compiled with ``javac`` and each
vector is executed with ``java``; the exit code is captured, never filtered,
because a COBOL ``RETURN-CODE`` is part of the program's observable behavior
(the bench harness settled this in WP-1.5.0d and this demo scores the same way).

The anti-gaming token list is imported from the sealed bench harness rather than
restated here. A migration that shelled out to the COBOL binary, or that
embedded the answers, would score a perfect equivalence rate while migrating
nothing; the demo has to be as hostile to that as the benchmark is, and it must
use the *same* list so the two can never drift apart.
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path
from typing import List, Optional, Tuple

_BENCH = Path(__file__).resolve().parents[1] / "bench"
if str(_BENCH) not in sys.path:
    sys.path.insert(0, str(_BENCH))

from harness.runner import FORBIDDEN_TOKENS  # noqa: E402  (sealed, read-only)

COMPILE_TIMEOUT_S = 180
RUN_TIMEOUT_S = 20


def scan_for_gaming(java_dir: Path) -> List[str]:
    """Report every forbidden token found in the generated Java."""
    hits: List[str] = []
    for f in sorted(java_dir.rglob("*.java")):
        text = f.read_text(errors="ignore")
        for tok in FORBIDDEN_TOKENS:
            if tok in text:
                hits.append(f"{f.name}: forbidden token {tok!r}")
    return hits


def compile_java(java_dir: Path, classes_dir: Path) -> Tuple[bool, str]:
    """Compile every ``.java`` under ``java_dir``. Returns ``(ok, stderr)``."""
    srcs = sorted(str(p) for p in java_dir.rglob("*.java"))
    if not srcs:
        return False, "no .java sources were generated"
    classes_dir.mkdir(parents=True, exist_ok=True)
    try:
        p = subprocess.run(
            ["javac", "-d", str(classes_dir), *srcs],
            capture_output=True, text=True, timeout=COMPILE_TIMEOUT_S,
        )
    except subprocess.TimeoutExpired:
        return False, f"javac timed out after {COMPILE_TIMEOUT_S}s"
    except Exception as exc:  # pragma: no cover - environment failure
        return False, f"javac could not be run: {exc}"
    return p.returncode == 0, p.stderr.strip()


def run(classes_dir: Path, main_class: str,
        stdin_line: str) -> Tuple[Optional[str], Optional[int]]:
    """Execute one vector. ``(None, None)`` means the JVM could not be run."""
    try:
        p = subprocess.run(
            ["java", "-cp", str(classes_dir), main_class],
            input=stdin_line + "\n", capture_output=True, text=True,
            timeout=RUN_TIMEOUT_S,
        )
    except Exception:
        return None, None
    return p.stdout.strip(), p.returncode
