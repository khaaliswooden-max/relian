"""
Differential validation -- Stage 6 of the migration pipeline, made real.

Compiles the MIGRATED code, executes it against oracle-generated vectors,
and compares stdout with the LEGACY program's stdout. Returns measured
values or None. Never a constant.

Scope & honesty notes
---------------------
* In-pipeline validation runs on the PUBLIC vector split only. Held-out
  scoring happens exclusively in CI against the private vector repo. A
  public-split BER is developer feedback, not an external quality claim,
  and is labeled as such in the returned dict.
* If the migrated code does not compile, the result is measured=True with
  ber=0.0 -- failing to build is a measured failure, not an excuse.
* If we cannot even attempt (no javac, no vectors for this program), the
  result is measured=False and every metric is None.
"""

import hashlib
import json
import re
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Any, Dict, Optional

REPO_ROOT = Path(__file__).resolve().parents[2]
BENCH = REPO_ROOT / "bench"


def _extract_class_name(java_source: str) -> Optional[str]:
    m = re.search(r"public\s+class\s+([A-Za-z_][A-Za-z0-9_]*)", java_source)
    return m.group(1) if m else None


def _match_corpus_program(source_code: str) -> Optional[str]:
    """Identify which corpus program this source corresponds to via
    PROGRAM-ID. Returns corpus dir name or None."""
    m = re.search(r"PROGRAM-ID\.\s*([A-Z0-9\-]+)", source_code, re.IGNORECASE)
    if not m:
        return None
    pid = m.group(1).upper().rstrip(".")
    mapping = {
        "PAYROLL01": "P01_payroll",
        "INTEREST01": "P02_interest",
        "ELIGIBLE01": "P03_eligibility",
        "TAXTBL01": "P04_taxtable",
        "VALIDAT01": "P05_validate",
    }
    return mapping.get(pid)


def _norm(s: str) -> str:
    return " ".join(s.split())


def validate_differential(source_code: str, target_code: str) -> Dict[str, Any]:
    """Measure behavioral equivalence of target_code vs. the legacy oracle.

    Returns dict with keys: measured (bool), ber, tests_passed, tests_failed,
    validation_score, split, reason.
    """
    out: Dict[str, Any] = {
        "measured": False,
        "ber": None,
        "tests_passed": None,
        "tests_failed": None,
        "validation_score": None,
        "split": "public",
        "reason": None,
    }

    if shutil.which("javac") is None or shutil.which("java") is None:
        out["reason"] = "JDK not available"
        return out

    prog = _match_corpus_program(source_code)
    if prog is None:
        out["reason"] = (
            "source program not in benchmark corpus; no oracle vectors exist "
            "for it. Equivalence is UNMEASURED for out-of-corpus programs."
        )
        return out

    vec_file = BENCH / "corpus" / prog / "vectors" / "public.jsonl"
    if not vec_file.exists():
        out["reason"] = f"no public vectors for {prog}"
        return out
    vectors = [json.loads(l) for l in vec_file.read_text().splitlines() if l.strip()]
    if not vectors:
        out["reason"] = f"empty vector file for {prog}"
        return out

    cls = _extract_class_name(target_code)
    if cls is None:
        # Not even a parseable class: measured failure, not an excuse.
        out.update(measured=True, ber=0.0, tests_passed=0,
                   tests_failed=len(vectors), validation_score=0.0,
                   reason="no public class found in migrated output")
        return out

    with tempfile.TemporaryDirectory() as td:
        tdp = Path(td)
        (tdp / f"{cls}.java").write_text(target_code)
        comp = subprocess.run(
            ["javac", "-d", str(tdp / "classes"), str(tdp / f"{cls}.java")],
            capture_output=True, text=True, timeout=120,
        )
        if comp.returncode != 0:
            out.update(measured=True, ber=0.0, tests_passed=0,
                       tests_failed=len(vectors), validation_score=0.0,
                       reason=f"compile failed: {comp.stderr.strip()[:200]}")
            return out

        passed = 0
        for v in vectors:
            try:
                run = subprocess.run(
                    ["java", "-cp", str(tdp / "classes"), cls],
                    input=v["input"] + "\n", capture_output=True,
                    text=True, timeout=20,
                )
                if run.returncode == 0 and _norm(run.stdout.strip()) == _norm(v["expected"]):
                    passed += 1
            except Exception:
                pass

    total = len(vectors)
    ber = passed / total
    out.update(
        measured=True,
        ber=round(ber, 4),
        tests_passed=passed,
        tests_failed=total - passed,
        validation_score=round(ber * 100, 2),
        reason=(
            "public-split developer feedback only; external claims require "
            "the CI held-out score"
        ),
    )
    return out
