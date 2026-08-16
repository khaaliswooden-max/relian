"""WP-1.5 tests — complexity formulas. No thresholds are asserted here,
because no thresholds live in complexity.py; those belong to risk.py."""

import re
from pathlib import Path

from src.assessment.complexity import (
    analyze,
    copybook_fan_in,
    external_interfaces,
)
from src.assessment.intake import read_source

FIXTURES = Path(__file__).parent / "fixtures" / "cobol"


def src(name: str) -> str:
    return read_source(FIXTURES / name)


def prog(body: str, ws: str = "       01 WS-N PIC 9(3).\n") -> str:
    return ("       IDENTIFICATION DIVISION.\n"
            "       PROGRAM-ID. T.\n"
            "       DATA DIVISION.\n"
            "       WORKING-STORAGE SECTION.\n"
            + ws +
            "       PROCEDURE DIVISION.\n" + body)


def test_cyclomatic_is_decision_points_plus_one():
    r = analyze(src("FULLSUP.cbl"), "FULLSUP.cbl")
    assert r.cyclomatic == r.decision_points + 1


def test_straight_line_program_has_cyclomatic_one():
    r = analyze(prog("       MAIN-PARA.\n           DISPLAY WS-N.\n           STOP RUN.\n"))
    assert r.decision_points == 0
    assert r.cyclomatic == 1


def test_each_if_adds_one_decision_point_and_else_adds_none():
    one_if = analyze(prog(
        "       MAIN-PARA.\n"
        "           IF WS-N > 0\n"
        "               DISPLAY WS-N\n"
        "           END-IF.\n"
        "           STOP RUN.\n"))
    with_else = analyze(prog(
        "       MAIN-PARA.\n"
        "           IF WS-N > 0\n"
        "               DISPLAY WS-N\n"
        "           ELSE\n"
        "               DISPLAY WS-N\n"
        "           END-IF.\n"
        "           STOP RUN.\n"))
    assert one_if.decision_points == 1
    assert with_else.decision_points == 1, "ELSE is the other half of a counted branch"


def test_compound_conditions_add_a_decision_point_each():
    r = analyze(prog(
        "       MAIN-PARA.\n"
        "           IF WS-N > 0 AND WS-N < 9 OR WS-N = 5\n"
        "               DISPLAY WS-N\n"
        "           END-IF.\n"
        "           STOP RUN.\n"))
    assert r.decision_points == 3          # IF + AND + OR


def test_program_cyclomatic_is_not_the_sum_of_paragraph_cyclomatics():
    r = analyze(prog(
        "       MAIN-PARA.\n"
        "           IF WS-N > 0\n"
        "               DISPLAY WS-N\n"
        "           END-IF.\n"
        "           PERFORM SECOND-PARA.\n"
        "           STOP RUN.\n"
        "       SECOND-PARA.\n"
        "           IF WS-N > 1\n"
        "               DISPLAY WS-N\n"
        "           END-IF.\n"))
    assert r.cyclomatic == 3               # 2 IFs + 1
    assert sum(p.cyclomatic for p in r.paragraphs) == 4    # double-counts the +1
    assert r.cyclomatic != sum(p.cyclomatic for p in r.paragraphs)


def test_goto_depending_on_counts_each_target():
    r = analyze(prog(
        "       MAIN-PARA.\n"
        "           GO TO A-PARA B-PARA C-PARA DEPENDING ON WS-N.\n"
        "       A-PARA.\n"
        "           STOP RUN.\n"
        "       B-PARA.\n"
        "           STOP RUN.\n"
        "       C-PARA.\n"
        "           STOP RUN.\n"))
    assert r.goto_count == 3


def test_goto_density_is_measured_with_provenance():
    r = analyze(src("HEAVY.cbl"), "HEAVY.cbl")
    assert r.goto_density is not None
    assert r.goto_density.value == round(r.goto_count / r.statements, 4)
    assert "GO TO targets" in r.goto_density.provenance
    assert r.goto_density.grade == "VERIFIED"


def test_goto_density_is_none_not_zero_when_statements_unmeasured():
    r = analyze("this is not cobol\n", "BROKEN.cbl")
    assert r.statements == 0
    assert r.goto_density is None


def test_alter_is_a_boolean_presence_flag():
    assert analyze(src("HEAVY.cbl"), "HEAVY.cbl").alter_present is True
    assert analyze(src("FULLSUP.cbl"), "FULLSUP.cbl").alter_present is False


def test_perform_thru_spans_are_recorded():
    r = analyze(src("HEAVY.cbl"), "HEAVY.cbl")
    assert r.perform_thru_spans == ("FIRST-PARA THRU THIRD-PARA",)


def test_exec_cics_and_sql_are_counted_separately():
    r = analyze(src("HEAVY.cbl"), "HEAVY.cbl")
    assert r.exec_cics_count == 1
    assert r.exec_sql_count == 1


def test_copybook_fan_out_and_fan_in():
    r = analyze(src("COPYUSER.cbl"), "COPYUSER.cbl")
    assert r.copybook_fan_out == ("SHARED",)
    other = analyze(src("FULLSUP.cbl"), "FULLSUP.cbl")
    assert copybook_fan_in([r, other]) == {"SHARED": ["COPYUSER.cbl"]}


def test_call_targets_are_deduplicated_and_sorted():
    r = analyze(src("COPYUSER.cbl"), "COPYUSER.cbl")
    assert r.call_targets == ("AUDIT", "LOGGER")


def test_max_nesting_depth():
    r = analyze(prog(
        "       MAIN-PARA.\n"
        "           IF WS-N > 0\n"
        "               IF WS-N > 1\n"
        "                   IF WS-N > 2\n"
        "                       DISPLAY WS-N\n"
        "                   END-IF\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           STOP RUN.\n"))
    assert r.max_nesting_depth == 3


def test_external_interfaces_lists_every_touchpoint():
    r = analyze(src("HEAVY.cbl"), "HEAVY.cbl")
    assert external_interfaces(r) == ("EXEC CICS x1", "EXEC SQL x1")
    c = analyze(src("COPYUSER.cbl"), "COPYUSER.cbl")
    assert external_interfaces(c) == ("CALL AUDIT", "CALL LOGGER")


def test_per_paragraph_metrics_cover_every_paragraph():
    r = analyze(src("HEAVY.cbl"), "HEAVY.cbl")
    names = [p.name for p in r.paragraphs]
    assert names == ["MAIN-PARA", "FIRST-PARA", "OTHER-PARA", "THIRD-PARA", "DEAD-PARA"]
    assert all(p.cyclomatic == p.decision_points + 1 for p in r.paragraphs)


def test_module_declares_no_thresholds():
    """WP-1.5: thresholds belong to risk.py. Guard against them creeping in."""
    text = Path("src/assessment/complexity.py").read_text()
    body = "\n".join(
        line for line in text.split('"""')[2].splitlines()   # skip module docstring
        if not line.strip().startswith("#")
    )
    comparisons = re.findall(r"[<>]=?\s*\d+", body)
    assert not comparisons, f"threshold-like comparisons found: {comparisons}"


def test_metrics_are_stable_across_line_endings():
    lf = src("HEAVY.cbl")
    assert analyze(lf, "x").to_dict() == analyze(lf.replace("\n", "\r\n"), "x").to_dict()


# --------------------------------------------------------------------------
# max_nesting_depth — regressions for the counter-vs-stack defect.
# A plain open/close counter let any END-… cancel an unrelated open scope.
# --------------------------------------------------------------------------


def test_end_perform_does_not_cancel_the_enclosing_if():
    """The reported defect: END-PERFORM inside an IF cancelled the IF's depth,
    so the IF and everything nested after it were undercounted."""
    r = analyze(prog(
        "       MAIN-PARA.\n"
        "           IF WS-N > 0\n"
        "               PERFORM VARYING WS-N FROM 1 BY 1 UNTIL WS-N > 3\n"
        "                   DISPLAY WS-N\n"
        "               END-PERFORM\n"
        "               IF WS-N > 1\n"
        "                   DISPLAY WS-N\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           STOP RUN.\n"))
    assert r.max_nesting_depth == 2


def test_inline_perform_opens_a_scope():
    """The module docstring says inline PERFORM nests; it must actually nest."""
    r = analyze(prog(
        "       MAIN-PARA.\n"
        "           IF WS-N > 0\n"
        "               PERFORM UNTIL WS-N > 3\n"
        "                   DISPLAY WS-N\n"
        "               END-PERFORM\n"
        "           END-IF.\n"
        "           STOP RUN.\n"))
    assert r.max_nesting_depth == 2


def test_perform_times_is_inline_but_perform_paragraph_is_not():
    inline = analyze(prog(
        "       MAIN-PARA.\n"
        "           PERFORM 5 TIMES\n"
        "               DISPLAY WS-N\n"
        "           END-PERFORM.\n"
        "           STOP RUN.\n"))
    procedural = analyze(prog(
        "       MAIN-PARA.\n"
        "           PERFORM SUB-PARA\n"
        "           PERFORM SUB-PARA\n"
        "           PERFORM SUB-PARA.\n"
        "           STOP RUN.\n"
        "       SUB-PARA.\n"
        "           DISPLAY WS-N.\n"))
    assert inline.max_nesting_depth == 1
    assert procedural.max_nesting_depth == 0, (
        "calling three paragraphs is not three levels of nesting"
    )


def test_unmatched_terminator_is_inert():
    """END-READ closes a statement this module never counted as opening a
    scope, so it must not decrement anything."""
    r = analyze(prog(
        "       MAIN-PARA.\n"
        "           IF WS-N > 0\n"
        "               READ CUSTFILE\n"
        "               END-READ\n"
        "               IF WS-N > 1\n"
        "                   DISPLAY WS-N\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           STOP RUN.\n"))
    assert r.max_nesting_depth == 2


def test_scope_opened_and_closed_on_one_line_still_counts():
    r = analyze(prog(
        "       MAIN-PARA.\n"
        "           IF WS-N > 0 DISPLAY WS-N END-IF.\n"
        "           STOP RUN.\n"))
    assert r.max_nesting_depth == 1


def test_period_closes_every_unterminated_scope():
    r = analyze(prog(
        "       MAIN-PARA.\n"
        "           IF WS-N > 0\n"
        "               DISPLAY WS-N.\n"
        "           DISPLAY WS-N.\n"
        "           STOP RUN.\n"))
    assert r.max_nesting_depth == 1


def test_search_opens_a_scope():
    r = analyze(prog(
        "       MAIN-PARA.\n"
        "           IF WS-N > 0\n"
        "               SEARCH WS-T\n"
        "               WHEN WS-N > 1\n"
        "                   DISPLAY WS-N\n"
        "               END-SEARCH\n"
        "           END-IF.\n"
        "           STOP RUN.\n"))
    assert r.max_nesting_depth == 2


def test_nesting_feeds_the_risk_rule_it_is_documented_to_feed():
    """max_nesting_depth>4 is a MED rule; the metric must reach it correctly."""
    from src.assessment.risk import assess
    from src.assessment.models import CoverageResult, Measured, RiskTier

    body = "       MAIN-PARA.\n"
    for i in range(5):
        body += " " * (11 + i * 4) + f"IF WS-N > {i}\n"
    body += " " * 31 + "DISPLAY WS-N\n"
    for i in range(4, -1, -1):
        body += " " * (11 + i * 4) + "END-IF\n"
    body += "           .\n           STOP RUN.\n"

    r = analyze(prog(body))
    assert r.max_nesting_depth == 5
    cov = CoverageResult("P", True, "antlr_tree", 10, 10,
                         Measured(1.0, "test fixture", "VERIFIED"))
    assert assess("P", cov, r).rule == "MED: max_nesting_depth>4"
