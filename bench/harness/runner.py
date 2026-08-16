"""
RELIAN-BENCH differential runner.

THE CARDINAL RULE
-----------------
A metric is either MEASURED or it is None. There is no third option.
No constant, no formula standing in for an observation, no fallback value.
If we cannot measure it, the run is marked INVALID and reports None.

This exists because the system under test previously reported
  test_coverage = min(80.0, len(tests) * 10)
  validation_score = 95.0            # hardcoded
  tests_failed = 0                   # hardcoded
  semantic_score = 0.85              # hardcoded fallback
i.e. it returned its targets as though they were results. A benchmark whose
harness can do the same thing is worthless. Hence: measure or None.

SCORING
-------
Behavioral Equivalence Rate (BER) = exact-match fraction of HELD-OUT vectors
where migrated stdout == legacy stdout AND the process exit code matches the
vector's expected exit code (WP-1.5.0d: behavioral equivalence includes
RETURN-CODE). A vector without an "expected_exit" field expects 0 -- which is
what every pre-WP-1.5.0d vector was recorded against, since the runner
previously discarded any run with a nonzero exit. This is the honest,
checkable replacement for the unmeasurable claim "95% semantic preservation".
"""

import json
import subprocess
import time
from dataclasses import dataclass, asdict, field
from pathlib import Path
from typing import Dict, List, Optional

ROOT = Path(__file__).resolve().parent.parent
CORPUS = ROOT / "corpus"

# --- Anti-gaming static controls (ZCS-6 Phase 3) ------------------------
# A migration that shells out to the COBOL binary, or that embeds the
# held-out answers, would score 100% while migrating nothing.
FORBIDDEN_TOKENS = [
    "Runtime.getRuntime", "ProcessBuilder", "exec(",
    "payroll01", "cobcrun", "libcob", "/corpus/",
    "heldout", "vectors.jsonl", "System.load",
]


@dataclass
class ProgramScore:
    program: str
    build_ok: Optional[bool] = None
    vectors_total: Optional[int] = None
    vectors_matched: Optional[int] = None
    ber: Optional[float] = None            # None until measured
    gaming_violations: List[str] = field(default_factory=list)
    mismatches: List[Dict] = field(default_factory=list)
    error: Optional[str] = None
    coverage_branch: Optional[float] = None
    coverage_detail: Dict = field(default_factory=dict)

    @property
    def valid(self) -> bool:
        return self.build_ok is not None and not self.gaming_violations


@dataclass
class RunReport:
    candidate: str
    scores: List[ProgramScore]
    ber_overall: Optional[float] = None
    build_rate: Optional[float] = None
    coverage_branch: Optional[float] = None   # None unless a real tool ran
    coverage_tool: Optional[str] = None
    wall_seconds: float = 0.0
    valid: bool = False
    invalid_reason: Optional[str] = None

    def to_dict(self) -> Dict:
        d = asdict(self)
        d["scores"] = [asdict(s) for s in self.scores]
        return d


def _scan_gaming(java_dir: Path) -> List[str]:
    hits = []
    for f in java_dir.rglob("*.java"):
        text = f.read_text(errors="ignore")
        for tok in FORBIDDEN_TOKENS:
            if tok in text:
                hits.append(f"{f.name}: forbidden token {tok!r}")
    return hits


def _compile(java_dir: Path, out_dir: Path) -> tuple[bool, str]:
    srcs = [str(p) for p in java_dir.rglob("*.java")]
    if not srcs:
        return False, "no .java sources"
    out_dir.mkdir(parents=True, exist_ok=True)
    p = subprocess.run(
        ["javac", "-d", str(out_dir), *srcs],
        capture_output=True, text=True, timeout=180,
    )
    return p.returncode == 0, p.stderr[-500:]


def _run_java(classes: Path, main_cls: str,
              stdin_line: str) -> tuple[Optional[str], Optional[int]]:
    """Run one vector; return (stdout, exit_code) -- (None, None) if the
    process could not be run at all.

    WP-1.5.0d: the exit code is CAPTURED, not filtered. The scorer compares
    it against the vector's expected exit code; deciding here that nonzero
    means failure would make a nonzero RETURN-CODE untestable forever.
    """
    try:
        p = subprocess.run(
            ["java", "-cp", str(classes), main_cls],
            input=stdin_line + "\n", capture_output=True, text=True, timeout=20,
        )
    except Exception:
        return None, None
    return p.stdout.strip(), p.returncode


def _norm(s: str) -> str:
    """Whitespace-normalise only. We do NOT normalise numbers -- a
    rounding divergence is a real defect, not a formatting nit."""
    return " ".join(s.split())


def score_program(name: str, java_dir: Path, main_cls: str,
                  split: str = "heldout") -> ProgramScore:
    sc = ProgramScore(program=name)
    if not java_dir.exists():
        sc.error = "no candidate output"
        sc.build_ok = False
        return sc

    sc.gaming_violations = _scan_gaming(java_dir)
    if sc.gaming_violations:
        sc.build_ok = False
        sc.error = "gaming controls tripped"
        return sc

    classes = java_dir / "_classes"
    ok, err = _compile(java_dir, classes)
    sc.build_ok = ok
    if not ok:
        sc.error = f"compile failed: {err.strip()[:200]}"
        return sc   # ber stays None -- did not build, so nothing was measured

    vec_file = CORPUS / name / "vectors" / f"{split}.jsonl"
    vectors = [json.loads(l) for l in vec_file.read_text().splitlines() if l.strip()]
    # v1.1: run under JaCoCo agent when the jars are present, so branch
    # coverage becomes a real measurement instead of a permanent None.
    from harness import coverage as _cov
    use_cov = _cov.available()
    exec_file = java_dir / "_classes" / "jacoco.exec"
    if exec_file.exists():
        exec_file.unlink()
    matched = 0
    for v in vectors:
        # WP-1.5.0d: behavioral equivalence includes the exit code. A vector
        # without "expected_exit" expects 0 -- the recorded expectation of
        # every pre-WP-1.5.0d vector, since the old runner discarded any run
        # that exited nonzero.
        expected_exit = int(v.get("expected_exit", 0))
        if use_cov:
            got, rc = _cov.run_with_coverage(classes, main_cls, v["input"], exec_file)
        else:
            got, rc = _run_java(classes, main_cls, v["input"])
        if (got is not None and rc == expected_exit
                and _norm(got) == _norm(v["expected"])):
            matched += 1
        elif len(sc.mismatches) < 5:
            sc.mismatches.append(
                {"input": v["input"], "expected": v["expected"], "got": got,
                 "expected_exit": expected_exit, "got_exit": rc}
            )
    sc.vectors_total = len(vectors)
    sc.vectors_matched = matched
    sc.ber = round(matched / len(vectors), 4) if vectors else None
    if use_cov:
        sc.coverage_branch, sc.coverage_detail = _cov.report(classes, exec_file)
    return sc


def run_candidate(candidate: str, out_root: Path,
                  mains: Dict[str, str], split: str = "heldout") -> RunReport:
    t0 = time.time()
    scores = [
        score_program(name, out_root / name, mains[name], split)
        for name in sorted(mains)
    ]
    rep = RunReport(candidate=candidate, scores=scores)
    rep.wall_seconds = round(time.time() - t0, 2)

    built = [s for s in scores if s.build_ok]
    rep.build_rate = round(len(built) / len(scores), 4) if scores else None

    measured = [s for s in scores if s.ber is not None]
    if measured:
        tot = sum(s.vectors_total for s in measured)
        mat = sum(s.vectors_matched for s in measured)
        # Programs that failed to build contribute 0 matched over their
        # full vector count -- failing to compile is not an excuse.
        unbuilt = [s for s in scores if s.ber is None]
        for s in unbuilt:
            vf = CORPUS / s.program / "vectors" / f"{split}.jsonl"
            tot += len([l for l in vf.read_text().splitlines() if l.strip()])
        rep.ber_overall = round(mat / tot, 4) if tot else None
    else:
        rep.ber_overall = 0.0

    # Coverage: aggregate of per-program JaCoCo measurements (branch-weighted).
    covd = [s for s in scores if s.coverage_branch is not None]
    if covd:
        tot_b = sum(s.coverage_detail.get("branches_total", 0) for s in covd)
        cov_b = sum(s.coverage_detail.get("branches_covered", 0) for s in covd)
        rep.coverage_branch = round(cov_b / tot_b, 4) if tot_b else None
        rep.coverage_tool = "jacoco-0.8.12"
    else:
        rep.coverage_branch = None
        rep.coverage_tool = None

    rep.valid = True
    if any(s.gaming_violations for s in scores):
        rep.valid = False
        rep.invalid_reason = "anti-gaming controls tripped"
    return rep


if __name__ == "__main__":
    import sys
    cand = sys.argv[1] if len(sys.argv) > 1 else "B0_null"
    out_root = ROOT / "candidates" / cand
    mains = json.loads((ROOT / "harness" / "mains.json").read_text())
    rep = run_candidate(cand, out_root, mains)
    print(json.dumps(rep.to_dict(), indent=2)[:3000])
