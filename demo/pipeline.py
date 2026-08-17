"""The demo pipeline: assess -> transpile -> build both -> differentially execute.

Every number this module produces comes from something that ran during the same
invocation. There are no defaults, no fallbacks and no remembered values. Where
a measurement could not be taken the field is ``None`` and the verdict says the
run was not measured — never zero, never an average, never a stored result
standing in for an execution (R1).

The pipeline is deliberately *the real one*. It imports the shipped transpiler
and the shipped assessment engine; it does not reimplement or approximate
either. If the transpiler regresses, this demo goes red.

Stage order, and what each stage is allowed to conclude:

1. **ASSESS** — the read-only assessment engine over the source. Produces the
   construct inventory and a coverage ratio: the fraction of statements the
   transpiler's dispatch table can handle. This is a *prediction*.
2. **TRANSPILE** — the transpiler is the authority. It either emits Java or
   raises ``UnsupportedConstruct``. Its answer is compared against stage 1's
   prediction, which cross-validates the two independent implementations.
3. **DETERMINISM** — transpile a second time and compare sha256. A migration
   you cannot reproduce is not evidence of anything.
4. **BUILD** — ``javac`` on the generated Java, ``cobc`` on the original COBOL.
5. **DIFFERENTIAL** — every input is fed to both binaries. Equivalence requires
   identical stdout *and* identical exit code.
6. **GATE** — decide whether the run is attestable. Nothing is signed here:
   signing keys are operator-custody only (R4) and this demo ships no simulated
   attestation (R5). The gate reports a decision, not a certificate.
"""

from __future__ import annotations

import hashlib
import sys
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from src.assessment.cli import assess_tree  # noqa: E402
from src.assessment.models import Measured  # noqa: E402
from src.assessment import supported as supported_mod  # noqa: E402
from transpiler.c1_rulebased import Transpiler, UnsupportedConstruct  # noqa: E402

from . import oracle, target  # noqa: E402
from .cases import Case  # noqa: E402

# Verdicts. Each names what was established, not how it felt.
EQUIVALENCE_MEASURED = "EQUIVALENCE_MEASURED"
DIVERGENCE_MEASURED = "DIVERGENCE_MEASURED"
REFUSED_UNSUPPORTED = "REFUSED_UNSUPPORTED"
# A crash is NOT a refusal. Both are safe — no Java is emitted either way — but
# only one of them is designed behavior, and collapsing them would let a defect
# in the diagnosis path masquerade as the honest-failure feature (R2).
TRANSPILE_CRASHED = "TRANSPILE_CRASHED"
BUILD_FAILED = "BUILD_FAILED"
NOT_MEASURED = "NOT_MEASURED"
# Some inputs could not be run on both sides (a timeout, or a process that
# would not launch). Whatever the executed subset showed, the program was not
# fully compared — and a partial comparison is not an equivalence claim.
INCOMPLETE_MEASUREMENT = "INCOMPLETE_MEASUREMENT"
GAMING_TRIPPED = "GAMING_TRIPPED"


def _sha256(text: str) -> str:
    return hashlib.sha256(text.encode()).hexdigest()


def _norm(s: str) -> str:
    """Whitespace-normalise only.

    Numbers are compared as emitted. A rounding divergence is a real defect,
    not a formatting nit, and normalising it away would be the demo lying.
    """
    return " ".join(s.split())


def _measured_to_dict(m: Optional[Measured]) -> Optional[Dict[str, Any]]:
    return None if m is None else {"value": m.value, "provenance": m.provenance,
                                   "grade": m.grade}


# --------------------------------------------------------------------------
# Result models
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class VectorOutcome:
    """One input, run through both implementations.

    Three states, not two. ``executed`` is False when a side could not be run
    at all — a timeout, or a process that would not launch. Such an input is
    **not** a divergence: nothing was observed, so nothing diverged. Collapsing
    it into ``equivalent=False`` would manufacture a measured failure out of an
    absent measurement, which is the R1 violation this whole demo exists to
    argue against.
    """

    index: int
    stdin: str
    cobol_stdout: Optional[str]
    cobol_exit: Optional[int]
    java_stdout: Optional[str]
    java_exit: Optional[int]
    executed: bool
    equivalent: bool
    # Did the locally-built COBOL reproduce the sealed public vector? None when
    # the case carries no sealed expectation, or the COBOL did not run.
    oracle_corroborated: Optional[bool]

    def to_dict(self) -> Dict[str, Any]:
        return {
            "index": self.index,
            "stdin": self.stdin,
            "cobol_stdout": self.cobol_stdout,
            "cobol_exit": self.cobol_exit,
            "java_stdout": self.java_stdout,
            "java_exit": self.java_exit,
            "executed": self.executed,
            "equivalent": self.equivalent,
            "oracle_corroborated": self.oracle_corroborated,
        }


@dataclass(frozen=True)
class AssessStage:
    parse_method: str
    total_statements: Optional[int]
    supported_statements: Optional[int]
    coverage_ratio: Optional[Measured]
    unsupported_ranked: Tuple[Tuple[str, int], ...]
    unsupported_lines: Tuple[Dict[str, Any], ...]
    physical_loc: Optional[int]
    cyclomatic: Optional[int]
    max_nesting_depth: Optional[int]
    risk_tier: str
    risk_rule: str

    @property
    def predicts_full_support(self) -> bool:
        return (self.coverage_ratio is not None
                and abs(self.coverage_ratio.value - 1.0) < 1e-9)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "parse_method": self.parse_method,
            "total_statements": self.total_statements,
            "supported_statements": self.supported_statements,
            "coverage_ratio": _measured_to_dict(self.coverage_ratio),
            "unsupported_ranked": [{"verb": v, "count": c}
                                   for v, c in self.unsupported_ranked],
            "unsupported_lines": list(self.unsupported_lines),
            "physical_loc": self.physical_loc,
            "cyclomatic": self.cyclomatic,
            "max_nesting_depth": self.max_nesting_depth,
            "risk_tier": self.risk_tier,
            "risk_rule": self.risk_rule,
        }


@dataclass
class CaseResult:
    case_id: str
    title: str
    note: str
    source_path: str
    source_sha256: str

    assess: Optional[AssessStage] = None

    transpiled: Optional[bool] = None
    transpile_error: Optional[str] = None
    refused_verb: Optional[str] = None
    refused_line: Optional[int] = None
    refused_paragraph: Optional[str] = None
    java_sha256: Optional[str] = None
    java_source: Optional[str] = None
    deterministic: Optional[bool] = None

    gaming_violations: List[str] = field(default_factory=list)

    java_build_ok: Optional[bool] = None
    java_build_error: Optional[str] = None
    cobol_build_ok: Optional[bool] = None
    cobol_build_error: Optional[str] = None

    vectors: List[VectorOutcome] = field(default_factory=list)
    vectors_total: Optional[int] = None        # inputs attempted
    vectors_executed: Optional[int] = None     # inputs that ran on BOTH sides
    vectors_equivalent: Optional[int] = None   # executed AND identical
    ber: Optional[Measured] = None
    oracle_agreement: Optional[Measured] = None

    verdict: str = NOT_MEASURED
    attestation_gate: str = "BLOCKED"
    attestation_reason: str = "no measurement was taken"
    wall_seconds: float = 0.0

    # Did stage 1's prediction agree with stage 2's authority?
    prediction_confirmed: Optional[bool] = None

    @property
    def attestable(self) -> bool:
        return self.attestation_gate == "PASSED"

    @property
    def vectors_absent(self) -> Optional[int]:
        """Inputs that could not be run on both sides. Not divergences."""
        if self.vectors_total is None or self.vectors_executed is None:
            return None
        return self.vectors_total - self.vectors_executed

    def to_dict(self) -> Dict[str, Any]:
        return {
            "case_id": self.case_id,
            "title": self.title,
            "note": self.note,
            "source_path": self.source_path,
            "source_sha256": self.source_sha256,
            "assess": None if self.assess is None else self.assess.to_dict(),
            "transpiled": self.transpiled,
            "transpile_error": self.transpile_error,
            "refused_verb": self.refused_verb,
            "refused_line": self.refused_line,
            "refused_paragraph": self.refused_paragraph,
            "java_sha256": self.java_sha256,
            "deterministic": self.deterministic,
            "gaming_violations": list(self.gaming_violations),
            "java_build_ok": self.java_build_ok,
            "java_build_error": self.java_build_error,
            "cobol_build_ok": self.cobol_build_ok,
            "cobol_build_error": self.cobol_build_error,
            "vectors_total": self.vectors_total,
            "vectors_executed": self.vectors_executed,
            "vectors_absent": self.vectors_absent,
            "vectors_equivalent": self.vectors_equivalent,
            "ber": _measured_to_dict(self.ber),
            "oracle_agreement": _measured_to_dict(self.oracle_agreement),
            "vector_detail": [v.to_dict() for v in self.vectors],
            "verdict": self.verdict,
            "attestation_gate": self.attestation_gate,
            "attestation_reason": self.attestation_reason,
            "prediction_confirmed": self.prediction_confirmed,
            "wall_seconds": round(self.wall_seconds, 2),
        }


@dataclass
class RunResult:
    cases: List[CaseResult]
    oracle_label: str
    oracle_available: bool
    transpiler_sha256: str
    git_sha: str
    wall_seconds: float = 0.0

    @property
    def measured_cases(self) -> List[CaseResult]:
        return [c for c in self.cases if c.ber is not None]

    def portfolio_ber(self) -> Optional[Measured]:
        """Aggregate equivalence over every input that actually ran.

        Cases that were refused or failed to build contribute no inputs — they
        are reported separately by verdict rather than folded into a rate. A
        single number that averaged a refusal into an equivalence rate would
        obscure exactly the behavior this demo is built to show.
        """
        measured = self.measured_cases
        if not measured:
            return None
        # Denominator is inputs that RAN on both sides, matching the per-case
        # rule. Inputs that could not be executed are reported as absent, never
        # averaged in as though they had been observed and failed.
        executed = sum(c.vectors_executed or 0 for c in measured)
        eq = sum(c.vectors_equivalent or 0 for c in measured)
        absent = sum(c.vectors_absent or 0 for c in measured)
        if not executed:
            return None
        return Measured(
            value=round(eq / executed, 4),
            provenance=(f"differential execution of {executed} input(s) across "
                        f"{len(measured)} program(s), {self.oracle_label} vs "
                        f"javac/java, this run"
                        + (f"; {absent} input(s) could not be run on both sides and "
                           f"are excluded" if absent else "")),
            grade="VERIFIED",
        )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "oracle": {"available": self.oracle_available, "label": self.oracle_label},
            "transpiler_sha256": self.transpiler_sha256,
            "git_sha": self.git_sha,
            "portfolio_ber": _measured_to_dict(self.portfolio_ber()),
            "cases": [c.to_dict() for c in self.cases],
            "wall_seconds": round(self.wall_seconds, 2),
        }


# --------------------------------------------------------------------------
# Stages
# --------------------------------------------------------------------------


def _run_assessment(source: Path) -> AssessStage:
    """Stage 1. The shipped assessment engine, over this one program."""
    bundle, _ = assess_tree(source)
    program = bundle.programs[0]
    cov = program.coverage
    comp = program.complexity
    return AssessStage(
        parse_method=cov.method,
        total_statements=cov.total_statements,
        supported_statements=cov.supported_statements,
        coverage_ratio=cov.coverage_ratio,
        unsupported_ranked=cov.ranked_unsupported(),
        unsupported_lines=tuple(
            {"verb": h.verb, "line": h.line, "paragraph": h.paragraph}
            for h in cov.unsupported_inventory
        ),
        physical_loc=program.loc.physical if program.loc else None,
        cyclomatic=comp.cyclomatic if comp else None,
        max_nesting_depth=comp.max_nesting_depth if comp else None,
        risk_tier=program.risk.tier.value,
        risk_rule=program.risk.rule,
    )


def _transpile(source_text: str, main_class: str) -> Tuple[Optional[str], Optional[Exception]]:
    try:
        return Transpiler(source_text, main_class).transpile(), None
    except Exception as exc:
        return None, exc


def run_case(case: Case, workdir: Path,
             on_stage=None) -> CaseResult:
    """Run the whole pipeline for one program."""
    started = time.time()
    say = on_stage or (lambda *_a, **_k: None)

    source_text = case.source.read_text()
    result = CaseResult(
        case_id=case.case_id,
        title=case.title,
        note=case.note,
        source_path=case.source.relative_to(ROOT).as_posix(),
        source_sha256=_sha256(source_text),
    )

    # --- 1. assess -------------------------------------------------------
    say("assess")
    result.assess = _run_assessment(case.source)

    # --- 2. transpile ----------------------------------------------------
    say("transpile")
    java, exc = _transpile(source_text, case.main_class)
    if exc is not None:
        result.transpiled = False
        result.transpile_error = f"{type(exc).__name__}: {exc}"
        if isinstance(exc, UnsupportedConstruct):
            result.refused_verb = exc.verb
            result.refused_line = exc.line_no
            result.refused_paragraph = exc.paragraph
            result.verdict = REFUSED_UNSUPPORTED
            result.attestation_reason = (
                f"the transpiler refused: unsupported construct "
                f"{exc.verb!r} at source line {exc.line_no}. No output was "
                f"produced and nothing is attestable."
            )
        else:
            result.verdict = TRANSPILE_CRASHED
            result.attestation_reason = (
                f"transpilation raised {type(exc).__name__} instead of a diagnosed "
                f"refusal. No Java was emitted, so the outcome is still safe — but "
                f"this is a defect in the diagnosis path, not designed behavior, "
                f"and it is reported as such.")
        # Did the read-only assessment predict this refusal?
        if result.assess is not None:
            result.prediction_confirmed = not result.assess.predicts_full_support
        result.wall_seconds = time.time() - started
        return result

    result.transpiled = True
    result.java_source = java
    result.java_sha256 = _sha256(java)
    if result.assess is not None:
        result.prediction_confirmed = result.assess.predicts_full_support

    # --- 3. determinism --------------------------------------------------
    say("determinism")
    java2, exc2 = _transpile(source_text, case.main_class)
    result.deterministic = exc2 is None and _sha256(java2) == result.java_sha256

    # --- write the Java out ---------------------------------------------
    case_dir = workdir / case.case_id
    java_dir = case_dir / "java"
    java_dir.mkdir(parents=True, exist_ok=True)
    (java_dir / f"{case.main_class}.java").write_text(java)

    # --- anti-gaming -----------------------------------------------------
    result.gaming_violations = target.scan_for_gaming(java_dir)
    if result.gaming_violations:
        result.verdict = GAMING_TRIPPED
        result.attestation_reason = (
            "anti-gaming controls tripped on the generated Java; the run is "
            "invalid and no measurement is reported")
        result.wall_seconds = time.time() - started
        return result

    # --- 4. build both ---------------------------------------------------
    say("build")
    classes = case_dir / "classes"
    result.java_build_ok, result.java_build_error = target.compile_java(java_dir, classes)

    cobol_bin = case_dir / "cobol" / case.case_id.lower()
    if oracle.detect().available:
        result.cobol_build_ok, result.cobol_build_error = oracle.compile_program(
            case.source, cobol_bin)
    else:
        result.cobol_build_ok = None
        result.cobol_build_error = "GnuCOBOL (cobc) is not installed"

    if not result.java_build_ok:
        result.verdict = BUILD_FAILED
        result.attestation_reason = (
            f"the generated Java did not compile: "
            f"{(result.java_build_error or '').splitlines()[0] if result.java_build_error else 'unknown error'}")
        result.wall_seconds = time.time() - started
        return result

    if not result.cobol_build_ok:
        result.verdict = NOT_MEASURED
        result.attestation_reason = (
            "the legacy oracle could not be built, so no behavioral comparison "
            "was performed. Equivalence is unmeasured, not assumed.")
        result.wall_seconds = time.time() - started
        return result

    # --- 5. differential execution ---------------------------------------
    say("differential")
    outcomes: List[VectorOutcome] = []
    corroborated = 0
    corroboration_possible = 0
    for i, stdin_line in enumerate(case.inputs):
        c_out, c_rc = oracle.run(cobol_bin, stdin_line)
        j_out, j_rc = target.run(classes, case.main_class, stdin_line)
        # An input only counts as compared if BOTH sides actually produced a
        # result. If either could not be run, there is nothing to compare —
        # `executed` is False and the input stays out of the rate entirely.
        executed = (c_out is not None and c_rc is not None
                    and j_out is not None and j_rc is not None)
        equivalent = (executed and c_rc == j_rc
                      and _norm(c_out) == _norm(j_out))
        corr: Optional[bool] = None
        if case.sealed is not None and i < len(case.sealed) and c_out is not None:
            sealed = case.sealed[i]
            corr = (_norm(c_out) == _norm(sealed.stdout)
                    and c_rc == sealed.exit_code)
            corroboration_possible += 1
            corroborated += int(corr)
        outcomes.append(
            VectorOutcome(
                index=i, stdin=stdin_line,
                cobol_stdout=c_out, cobol_exit=c_rc,
                java_stdout=j_out, java_exit=j_rc,
                executed=executed, equivalent=equivalent,
                oracle_corroborated=corr,
            )
        )

    result.vectors = outcomes
    result.vectors_total = len(outcomes)
    result.vectors_executed = sum(1 for o in outcomes if o.executed)
    result.vectors_equivalent = sum(1 for o in outcomes if o.equivalent)

    # The denominator is what RAN, not what was attempted. An input that could
    # not be executed is absent from the rate and reported separately.
    if result.vectors_executed:
        absent = result.vectors_absent or 0
        result.ber = Measured(
            value=round(result.vectors_equivalent / result.vectors_executed, 4),
            provenance=(f"differential execution of {result.vectors_executed} of "
                        f"{result.vectors_total} input(s): {oracle.detect().label} vs "
                        f"javac/java, this run; stdout and process exit code both "
                        f"compared"
                        + (f"; {absent} input(s) could not be run on both sides and "
                           f"are excluded from this rate" if absent else "")),
            grade="VERIFIED",
        )
    if corroboration_possible:
        result.oracle_agreement = Measured(
            value=round(corroborated / corroboration_possible, 4),
            provenance=(f"locally-built GnuCOBOL output vs the sealed PUBLIC "
                        f"vectors for {corroboration_possible} input(s)"),
            grade="VERIFIED",
        )

    # --- 6. gate ---------------------------------------------------------
    absent = result.vectors_absent or 0
    if result.ber is None:
        result.verdict = NOT_MEASURED
        result.attestation_reason = (
            f"none of the {result.vectors_total or 0} input(s) could be run on both "
            f"sides, so nothing was compared. This is an absent measurement, not a "
            f"passing or failing one.")
    elif absent:
        # Something ran, but not everything. Whatever the executed subset
        # showed, this program was not fully compared.
        result.verdict = INCOMPLETE_MEASUREMENT
        result.attestation_reason = (
            f"{absent} of {result.vectors_total} input(s) could not be run on both "
            f"sides. The {result.vectors_executed} that ran scored "
            f"{result.ber.value:.4f}, but a partial comparison is not an equivalence "
            f"claim and does not attest.")
    elif result.ber.value == 1.0:
        result.verdict = EQUIVALENCE_MEASURED
        if result.deterministic:
            result.attestation_gate = "PASSED"
            result.attestation_reason = (
                f"all {result.vectors_executed} input(s) produced identical stdout and "
                f"exit code on both implementations, the transpile is reproducible, "
                f"and anti-gaming controls are clean. Signing is not performed here: "
                f"key custody is operator-only and this demo ships no simulated "
                f"attestation.")
        else:
            result.attestation_reason = (
                "behavior matched on every input, but the transpile was not "
                "reproducible — a migration that cannot be regenerated byte-for-byte "
                "is not attestable")
    else:
        result.verdict = DIVERGENCE_MEASURED
        result.attestation_reason = (
            f"{result.vectors_executed - result.vectors_equivalent} of "
            f"{result.vectors_executed} executed input(s) diverged from the legacy "
            f"oracle")

    result.wall_seconds = time.time() - started
    return result


def run(cases: Sequence[Case], workdir: Path, on_stage=None,
        on_case_done=None) -> RunResult:
    """Run every case and collect the results."""
    started = time.time()
    info = oracle.detect()
    out = RunResult(
        cases=[],
        oracle_label=info.label,
        oracle_available=info.available,
        transpiler_sha256=supported_mod.transpiler_sha256(),
        git_sha=supported_mod.git_sha(),
    )
    for case in cases:
        res = run_case(case, workdir,
                       on_stage=(lambda s, c=case: on_stage(c, s)) if on_stage else None)
        out.cases.append(res)
        if on_case_done:
            on_case_done(res)
    out.wall_seconds = time.time() - started
    return out
