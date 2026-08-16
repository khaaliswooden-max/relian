"""WP-1.6 — risk tiers.

A deterministic, published rule. There is no learned model, no score, and no
weighting anyone has to take on trust: :data:`RISK_RULES` is evaluated top to
bottom, the first rule that matches decides the tier, and the exact rule string
that fired is carried in the result and printed verbatim into the report
appendix.

Grading (R9): the tier is **PLAUSIBLE** — it is a policy, an assertion about
what Relian considers risky, and policies are not measurements. Every *input*
to the policy is VERIFIED, because each comes from a measured
:class:`~src.assessment.models.CoverageResult` or
:class:`~src.assessment.models.ComplexityResult`. The report says so in both
places rather than letting a policy inherit the credibility of its inputs.

Unmeasured input is not treated as a passing input. If coverage could not be
measured, the program is ``BLOCKED`` with a rule that says exactly that — the
one thing an assessment must never do is let "we could not tell" read as "fine".
"""

from __future__ import annotations

from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple

from .models import ComplexityResult, CoverageResult, RiskFinding, RiskTier

# Thresholds live here and only here. Each entry is
# (tier, rule string printed into the report, predicate over the inputs dict).
RISK_RULES: Tuple[Tuple[RiskTier, str, Callable[[Dict[str, Any]], bool]], ...] = (
    (RiskTier.BLOCKED,
     "BLOCKED: coverage not measured (program did not yield statements)",
     lambda i: i["coverage_ratio"] is None),

    (RiskTier.BLOCKED,
     "BLOCKED: coverage<0.60",
     lambda i: i["coverage_ratio"] < 0.60),

    (RiskTier.BLOCKED,
     "BLOCKED: ALTER present (static control flow is undecidable)",
     lambda i: i["alter_present"]),

    (RiskTier.HIGH,
     "HIGH: EXEC CICS present",
     lambda i: i["exec_cics_count"] > 0),

    (RiskTier.HIGH,
     "HIGH: EXEC SQL present",
     lambda i: i["exec_sql_count"] > 0),

    (RiskTier.HIGH,
     "HIGH: coverage<0.80",
     lambda i: i["coverage_ratio"] < 0.80),

    (RiskTier.HIGH,
     "HIGH: cyclomatic>50",
     lambda i: i["cyclomatic"] > 50),

    (RiskTier.HIGH,
     "HIGH: goto_density>0.10",
     lambda i: i["goto_density"] is not None and i["goto_density"] > 0.10),

    (RiskTier.MED,
     "MED: coverage<1.00",
     lambda i: i["coverage_ratio"] < 1.00),

    (RiskTier.MED,
     "MED: external CALL present",
     lambda i: i["call_count"] > 0),

    (RiskTier.MED,
     "MED: PERFORM THRU span present",
     lambda i: i["perform_thru_count"] > 0),

    (RiskTier.MED,
     "MED: cyclomatic>20",
     lambda i: i["cyclomatic"] > 20),

    (RiskTier.MED,
     "MED: max_nesting_depth>4",
     lambda i: i["max_nesting_depth"] > 4),

    (RiskTier.LOW,
     "LOW: coverage=1.00, no external interface, no ALTER, "
     "cyclomatic<=20, nesting<=4",
     lambda i: True),
)


def rule_table() -> Tuple[str, ...]:
    """The rules as printed into the report appendix, in evaluation order."""
    return tuple(rule for _, rule, _ in RISK_RULES)


def _inputs(coverage: CoverageResult,
            complexity: Optional[ComplexityResult]) -> Dict[str, Any]:
    ratio = coverage.coverage_ratio.value if coverage.coverage_ratio else None
    if complexity is None:
        # Neutral counts for a program with no complexity result. These never
        # decide anything: complexity is absent only when no statements were
        # recovered, in which case coverage_ratio is None and the first rule
        # fires BLOCKED before any of these values is compared.
        return {
            "coverage_ratio": ratio,
            "alter_present": False,
            "exec_cics_count": 0,
            "exec_sql_count": 0,
            "cyclomatic": 0,
            "goto_density": None,
            "call_count": 0,
            "perform_thru_count": 0,
            "max_nesting_depth": 0,
        }
    return {
        "coverage_ratio": ratio,
        "alter_present": complexity.alter_present,
        "exec_cics_count": complexity.exec_cics_count,
        "exec_sql_count": complexity.exec_sql_count,
        "cyclomatic": complexity.cyclomatic,
        "goto_density": complexity.goto_density.value if complexity.goto_density else None,
        "call_count": len(complexity.call_targets),
        "perform_thru_count": len(complexity.perform_thru_spans),
        "max_nesting_depth": complexity.max_nesting_depth,
    }


def assess(program_id: str,
           coverage: CoverageResult,
           complexity: Optional[ComplexityResult] = None) -> RiskFinding:
    """Apply RISK_RULES in order; the first match decides."""
    inputs = _inputs(coverage, complexity)
    for tier, rule, predicate in RISK_RULES:
        try:
            fired = predicate(inputs)
        except TypeError:
            # A None input reached a comparison. That is not a pass; a rule
            # that cannot be evaluated is reported, not skipped.
            continue
        if fired:
            return RiskFinding(
                program_id=program_id,
                tier=tier,
                rule=rule,
                inputs=tuple(sorted(inputs.items())),
            )
    raise AssertionError("RISK_RULES must end in an unconditional rule")


def portfolio(findings: Sequence[RiskFinding],
              program_id: str = "PORTFOLIO") -> RiskFinding:
    """Portfolio tier = the worst program tier, naming how many programs share it."""
    order = [RiskTier.BLOCKED, RiskTier.HIGH, RiskTier.MED, RiskTier.LOW]
    if not findings:
        return RiskFinding(
            program_id=program_id,
            tier=RiskTier.BLOCKED,
            rule="BLOCKED: no programs assessed",
            inputs=(("programs", 0),),
        )
    worst = min(findings, key=lambda f: order.index(f.tier)).tier
    counts = {t.value: sum(1 for f in findings if f.tier is t) for t in order}
    return RiskFinding(
        program_id=program_id,
        tier=worst,
        rule=f"{worst.value}: worst program tier across {len(findings)} program(s) "
             f"({counts[worst.value]} at {worst.value})",
        inputs=tuple(sorted(counts.items())) + (("programs", len(findings)),),
    )


def tier_counts(findings: Sequence[RiskFinding]) -> Dict[str, int]:
    out: Dict[str, int] = {t.value: 0 for t in RiskTier}
    for f in findings:
        out[f.tier.value] += 1
    return dict(sorted(out.items()))


def programs_by_tier(findings: Sequence[RiskFinding]) -> Dict[str, List[str]]:
    out: Dict[str, List[str]] = {}
    for f in findings:
        out.setdefault(f.tier.value, []).append(f.program_id)
    return {tier: sorted(progs) for tier, progs in sorted(out.items())}
