"""The legacy oracle: GnuCOBOL.

The demo's central claim is that the migrated Java behaves like the COBOL.
That claim is only worth something if *the COBOL actually ran*. So this module
compiles the original program with ``cobc`` and executes it, in this run, on
the same inputs the Java gets. Nothing here reads a stored expectation.

If GnuCOBOL is absent the functions say so and return ``None``. They never
substitute a recorded value for an execution that did not happen — a demo that
silently fell back to a stored answer would be asserting equivalence it had not
observed, which is the exact failure mode RELIAN rule R1 exists to prevent.

Source format: the corpus and the demo programs are fixed-format COBOL
(sequence area 1-6, indicator 7, code 8-72), which is ``cobc``'s default. No
``-free``.
"""

from __future__ import annotations

import re
import shutil
import subprocess
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path
from typing import Optional, Tuple

COMPILE_TIMEOUT_S = 120
RUN_TIMEOUT_S = 20


@dataclass(frozen=True)
class OracleInfo:
    """What COBOL toolchain is present, if any."""

    available: bool
    binary: Optional[str]
    version: Optional[str]

    @property
    def label(self) -> str:
        return self.version or "not available"


@lru_cache(maxsize=1)
def detect() -> OracleInfo:
    """Locate ``cobc`` and record its version string verbatim."""
    exe = shutil.which("cobc")
    if not exe:
        return OracleInfo(available=False, binary=None, version=None)
    try:
        p = subprocess.run([exe, "--version"], capture_output=True, text=True, timeout=30)
    except Exception:
        return OracleInfo(available=True, binary=exe, version=None)
    first = (p.stdout or p.stderr).strip().splitlines()
    if not first:
        return OracleInfo(available=True, binary=exe, version=None)
    # "cobc (GnuCOBOL) 3.1.2.0" -> "GnuCOBOL 3.1.2.0"
    m = re.search(r"\(([^)]+)\)\s*([0-9][0-9.]*)", first[0])
    version = f"{m.group(1)} {m.group(2)}" if m else first[0].strip()
    return OracleInfo(available=True, binary=exe, version=version)


def compile_program(source: Path, out_binary: Path) -> Tuple[bool, str]:
    """Compile one COBOL program to a native executable.

    Returns ``(ok, diagnostics)``. ``ok`` is the compiler's own exit status —
    not an inference from whether a file appeared.
    """
    info = detect()
    if not info.available:
        return False, "GnuCOBOL (cobc) is not installed in this environment"
    out_binary.parent.mkdir(parents=True, exist_ok=True)
    try:
        p = subprocess.run(
            [info.binary, "-x", "-o", str(out_binary), str(source)],
            capture_output=True, text=True, timeout=COMPILE_TIMEOUT_S,
        )
    except subprocess.TimeoutExpired:
        return False, f"cobc timed out after {COMPILE_TIMEOUT_S}s"
    except Exception as exc:  # pragma: no cover - environment failure
        return False, f"cobc could not be run: {exc}"
    return p.returncode == 0, _clean_diagnostics(p.stderr)


def run(binary: Path, stdin_line: str) -> Tuple[Optional[str], Optional[int]]:
    """Execute the COBOL binary on one input line.

    Returns ``(stdout, exit_code)``, or ``(None, None)`` if the process could
    not be run at all — which is an absent measurement, not a failed one, and
    the caller must treat it as such.
    """
    try:
        p = subprocess.run(
            [str(binary)], input=stdin_line + "\n",
            capture_output=True, text=True, timeout=RUN_TIMEOUT_S,
        )
    except Exception:
        return None, None
    return p.stdout.strip(), p.returncode


_NOISE = (
    "_FORTIFY_SOURCE",
    "this is the location of the previous definition",
)


def _clean_diagnostics(stderr: str) -> str:
    """Drop the glibc ``_FORTIFY_SOURCE`` redefinition warning.

    It is emitted by the host's gcc wrapper on every single compile here and
    says nothing about the COBOL. Only that warning is filtered; every real
    diagnostic is passed through untouched.
    """
    kept = [ln for ln in stderr.splitlines() if not any(n in ln for n in _NOISE)]
    return "\n".join(kept).strip()
