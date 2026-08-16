"""WP-1.6 tests — deterministic, published risk rules."""

import inspect
from pathlib import Path

import pytest

from src.assessment import risk as risk_mod
from src.assessment.complexity import analyze as complexity_analyze
from src.assessment.coverage import analyze as coverage_analyze
from src.assessment.intake import ingest, read_source
from src.assessment.models import CoverageResult, Measured, RiskTier
from src.assessment.risk import RISK_RULES, assess, portfolio, rule_table, tier_counts

FIXTURES = Path(__file__).parent / "fixtures" / "cobol"


def cov(ratio, program_id="P"):
    """A CoverageResult carrying just the ratio the rules read."""
    if ratio is None:
        return CoverageResult(program_id, False, "none", None, None, None,
                              error="no statements recovered")
    return CoverageResult(
        program_id, True, "antlr_tree", 10, int(ratio * 10),
        Measured(ratio, "test fixture", "VERIFIED"),
    )


def comp(source: str, program_id="P"):
    return complexity_analyze(source, program_id)


def prog(body: str) -> str:
    return ("       IDENTIFICATION DIVISION.\n"
            "       PROGRAM-ID. T.\n"
            "       DATA DIVISION.\n"
            "       WORKING-STORAGE SECTION.\n"
            "       01 WS-N PIC 9(3).\n"
            "       PROCEDURE DIVISION.\n" + body)


CLEAN = prog("       MAIN-PARA.\n           DISPLAY WS-N.\n           STOP RUN.\n")


def test_unmeasured_coverage_is_blocked_not_passed():
    """The one thing an assessment must never do is let 'we could not tell'
    read as 'fine'."""
    f = assess("P", cov(None))
    assert f.tier is RiskTier.BLOCKED
    assert "coverage not measured" in f.rule


def test_low_coverage_is_blocked():
    f = assess("P", cov(0.50), comp(CLEAN))
    assert f.tier is RiskTier.BLOCKED
    assert f.rule == "BLOCKED: coverage<0.60"


def test_alter_blocks_regardless_of_coverage():
    src = prog("       MAIN-PARA.\n           ALTER X TO PROCEED TO Y.\n           STOP RUN.\n")
    f = assess("P", cov(1.0), comp(src))
    assert f.tier is RiskTier.BLOCKED
    assert "ALTER present" in f.rule


def test_exec_cics_is_high():
    src = prog("       MAIN-PARA.\n           EXEC CICS SEND MAP END-EXEC.\n           STOP RUN.\n")
    f = assess("P", cov(1.0), comp(src))
    assert f.tier is RiskTier.HIGH
    assert f.rule == "HIGH: EXEC CICS present"


def test_exec_sql_is_high():
    src = prog("       MAIN-PARA.\n           EXEC SQL SELECT A END-EXEC.\n           STOP RUN.\n")
    assert assess("P", cov(1.0), comp(src)).rule == "HIGH: EXEC SQL present"


def test_coverage_below_point_eight_is_high():
    f = assess("P", cov(0.70), comp(CLEAN))
    assert f.tier is RiskTier.HIGH
    assert f.rule == "HIGH: coverage<0.80"


def _cov_with_value_hit(status: str):
    """A full-coverage CoverageResult carrying one VALUE-clause hit."""
    from dataclasses import replace

    from src.assessment.models import DataFeatureHit

    c = cov(1.0)
    return replace(c, data_feature_inventory=(
        DataFeatureHit(feature="VALUE clause on a data item",
                       status=status, file="P", line=7),
    ))


def test_discarded_value_clause_is_high_even_at_full_coverage():
    """WP-1.5.4: C1 zero-inits every field, so a VALUE clause it discards is
    lost initialization semantics — HIGH regardless of statement coverage."""
    f = assess("P", _cov_with_value_hit("accepted_ignored"), comp(CLEAN))
    assert f.tier is RiskTier.HIGH
    assert "VALUE clause present but discarded" in f.rule
    assert ("value_discarded_count", 1) in f.inputs


def test_value_rule_retires_itself_when_value_probes_supported():
    """The rule reads the PROBED status on each hit; the day VALUE probes
    'supported' the rule stops firing with no edit to risk.py."""
    f = assess("P", _cov_with_value_hit("supported"), comp(CLEAN))
    assert f.tier is RiskTier.LOW


def test_other_accepted_ignored_features_do_not_trip_the_value_rule():
    """COMP-3 is accepted_ignored too, but BER 1.0000 on the corpus that uses
    it everywhere proves the discard is behavior-preserving there; the rule
    is deliberately scoped to VALUE."""
    from dataclasses import replace

    from src.assessment.models import DataFeatureHit

    c = replace(cov(1.0), data_feature_inventory=(
        DataFeatureHit(feature="USAGE COMP-3 (packed decimal)",
                       status="accepted_ignored", file="P", line=5),
    ))
    f = assess("P", c, comp(CLEAN))
    assert f.tier is RiskTier.LOW


def test_partial_coverage_is_at_least_medium():
    f = assess("P", cov(0.90), comp(CLEAN))
    assert f.tier is RiskTier.MED
    assert f.rule == "MED: coverage<1.00"


def test_external_call_is_medium():
    src = prog('       MAIN-PARA.\n           CALL "SUB".\n           STOP RUN.\n')
    f = assess("P", cov(1.0), comp(src))
    assert f.tier is RiskTier.MED
    assert f.rule == "MED: external CALL present"


def test_perform_thru_is_medium():
    src = prog("       MAIN-PARA.\n           PERFORM A-PARA THRU B-PARA.\n"
               "           STOP RUN.\n       A-PARA.\n           DISPLAY WS-N.\n"
               "       B-PARA.\n           DISPLAY WS-N.\n")
    assert assess("P", cov(1.0), comp(src)).rule == "MED: PERFORM THRU span present"


def test_clean_program_is_low():
    f = assess("P", cov(1.0), comp(CLEAN))
    assert f.tier is RiskTier.LOW
    assert f.rule.startswith("LOW:")


def test_first_matching_rule_wins_and_is_reported_exactly():
    """A program that trips several rules reports the first, not a blend."""
    src = prog("       MAIN-PARA.\n           EXEC CICS SEND MAP END-EXEC.\n"
               '           CALL "SUB".\n           STOP RUN.\n')
    f = assess("P", cov(0.50), comp(src))
    assert f.rule == "BLOCKED: coverage<0.60"
    assert f.rule in rule_table()


def test_finding_records_every_input_it_used():
    f = assess("P", cov(1.0), comp(CLEAN))
    keys = dict(f.inputs).keys()
    assert {"coverage_ratio", "alter_present", "exec_cics_count", "exec_sql_count",
            "cyclomatic", "goto_density", "call_count", "perform_thru_count",
            "max_nesting_depth"} <= set(keys)


def test_finding_is_graded_plausible_with_policy_provenance():
    d = assess("P", cov(1.0), comp(CLEAN)).to_dict()
    assert d["grade"] == "PLAUSIBLE"
    assert "policy" in d["provenance"]
    assert "inputs are VERIFIED" in d["provenance"]


def test_rules_are_deterministic():
    a = assess("P", cov(0.75), comp(CLEAN))
    b = assess("P", cov(0.75), comp(CLEAN))
    assert a.to_dict() == b.to_dict()


def test_rule_table_is_complete_and_ordered():
    assert len(rule_table()) == len(RISK_RULES)
    assert rule_table()[-1].startswith("LOW:"), "the table must end in a catch-all"


def test_every_rule_string_names_its_tier():
    for tier, rule, _ in RISK_RULES:
        assert rule.startswith(f"{tier.value}:")


def test_portfolio_takes_the_worst_tier():
    findings = [
        assess("a", cov(1.0), comp(CLEAN)),
        assess("b", cov(0.70), comp(CLEAN)),
        assess("c", cov(None)),
    ]
    p = portfolio(findings)
    assert p.tier is RiskTier.BLOCKED
    assert "across 3 program(s)" in p.rule


def test_portfolio_with_no_programs_is_blocked_not_low():
    assert portfolio([]).tier is RiskTier.BLOCKED


def test_tier_counts_covers_every_tier():
    counts = tier_counts([assess("a", cov(1.0), comp(CLEAN))])
    assert set(counts) == {t.value for t in RiskTier}
    assert counts["LOW"] == 1


def test_thresholds_live_only_in_the_rules_table():
    """Guard against a threshold drifting into the evaluation code.

    The evaluation code does contain zeros — the neutral counts used when a
    program has no complexity result — but it must contain no *comparison*
    against a literal, because that is what a threshold is.
    """
    import re as _re
    source = inspect.getsource(risk_mod)
    after_table = source.split(")\n\n\ndef rule_table", 1)[1]
    comparisons = _re.findall(r"[<>]=?\s*\d+", after_table)
    assert comparisons == [], f"threshold comparisons outside RISK_RULES: {comparisons}"


def test_real_fixtures_get_tiers_matching_their_findings():
    records = {r.rel_path_posix: r for r in ingest(FIXTURES)}
    heavy_src = read_source(FIXTURES / "HEAVY.cbl")
    heavy = assess("HEAVY.cbl",
                   coverage_analyze(records["HEAVY.cbl"], heavy_src),
                   comp(heavy_src, "HEAVY.cbl"))
    assert heavy.tier is RiskTier.BLOCKED

    full_src = read_source(FIXTURES / "FULLSUP.cbl")
    full = assess("FULLSUP.cbl",
                  coverage_analyze(records["FULLSUP.cbl"], full_src),
                  comp(full_src, "FULLSUP.cbl"))
    assert full.tier is RiskTier.LOW
