"""WP-1.4 tests — LOC counting rules and dead-paragraph reachability."""

from pathlib import Path

import pytest

from src.assessment.intake import read_source
from src.assessment.loc import count, dead_paragraphs, portfolio_totals
from src.assessment.coverage import scan_source

FIXTURES = Path(__file__).parent / "fixtures" / "cobol"


def src(name: str) -> str:
    return read_source(FIXTURES / name)


def test_categories_partition_the_file_exactly():
    for name in sorted(p.name for p in FIXTURES.glob("*.cbl")):
        text = src(name)
        r = count(text, "program", name)
        assert r.physical == len(text.rstrip("\n").split("\n")), name
        assert r.comment + r.blank <= r.physical, name


def test_trailing_newline_does_not_add_a_line():
    body = ("       IDENTIFICATION DIVISION.\n"
            "       PROGRAM-ID. T.\n"
            "       DATA DIVISION.\n"
            "       WORKING-STORAGE SECTION.\n"
            "       01 WS-N PIC 9(3).\n"
            "       PROCEDURE DIVISION.\n"
            "       MAIN-PARA.\n"
            "           STOP RUN.")
    assert count(body).physical == count(body + "\n").physical == 8


def test_crlf_and_lf_count_identically():
    text = src("PARTIAL.cbl")
    assert count(text).to_dict() == count(text.replace("\n", "\r\n")).to_dict()


def test_comment_lines_counted_by_indicator_column():
    r = count(src("FULLSUP.cbl"), "program", "FULLSUP.cbl")
    assert r.comment == 1                      # exactly one `      *` line


def test_sequence_number_only_line_is_blank_not_code():
    text = ("000100 IDENTIFICATION DIVISION.\n"
            "000200 PROGRAM-ID. T.\n"
            "000300\n"
            "000400 DATA DIVISION.\n"
            "000500 WORKING-STORAGE SECTION.\n"
            "000600 01 WS-N PIC 9(3).\n"
            "000700 PROCEDURE DIVISION.\n"
            "000800 MAIN-PARA.\n"
            "000900     STOP RUN.\n")
    r = count(text)
    assert r.blank == 1


def test_logical_is_statements_not_periods():
    r = count(src("FULLSUP.cbl"), "program", "FULLSUP.cbl")
    # FULLSUP has one period in the whole PROCEDURE DIVISION but many statements.
    assert r.logical == 18
    assert r.logical_method in ("antlr_tree", "token_scan")


def test_logical_is_none_when_no_statements_can_be_recovered():
    r = count(src("BROKEN.cbl"), "program", "BROKEN.cbl")
    assert r.logical is None
    assert r.logical_method == "none"


def test_copybook_gets_no_logical_count_and_says_why():
    r = count(read_source(FIXTURES / "SHARED.cpy"), "copybook", "SHARED.cpy")
    assert r.logical is None
    assert r.logical_method == "not_applicable"
    assert r.note and "kind=copybook" in r.note
    assert r.physical == 3


def _prog(paragraphs: str) -> str:
    return ("       IDENTIFICATION DIVISION.\n"
            "       PROGRAM-ID. T.\n"
            "       DATA DIVISION.\n"
            "       WORKING-STORAGE SECTION.\n"
            "       01 WS-N PIC 9(3).\n"
            "       PROCEDURE DIVISION.\n" + paragraphs)


def test_entry_paragraph_is_never_dead():
    text = _prog("       MAIN-PARA.\n           STOP RUN.\n")
    assert dead_paragraphs(scan_source(text))[0] == ()


def test_unreferenced_paragraph_after_a_terminator_is_dead():
    text = _prog(
        "       MAIN-PARA.\n"
        "           STOP RUN.\n"
        "       ORPHAN-PARA.\n"
        "           DISPLAY WS-N.\n"
    )
    assert dead_paragraphs(scan_source(text))[0] == ("ORPHAN-PARA",)


def test_fall_through_reaches_the_next_paragraph():
    text = _prog(
        "       MAIN-PARA.\n"
        "           DISPLAY WS-N.\n"
        "       NEXT-PARA.\n"
        "           STOP RUN.\n"
    )
    assert dead_paragraphs(scan_source(text))[0] == ()


def test_performed_paragraph_is_reached():
    text = _prog(
        "       MAIN-PARA.\n"
        "           PERFORM FAR-PARA.\n"
        "           STOP RUN.\n"
        "       FAR-PARA.\n"
        "           DISPLAY WS-N.\n"
    )
    assert dead_paragraphs(scan_source(text))[0] == ()


def test_perform_thru_reaches_every_paragraph_in_the_span():
    text = _prog(
        "       MAIN-PARA.\n"
        "           PERFORM A-PARA THRU C-PARA.\n"
        "           STOP RUN.\n"
        "       A-PARA.\n"
        "           DISPLAY WS-N.\n"
        "           GO TO C-PARA.\n"
        "       B-PARA.\n"
        "           DISPLAY WS-N.\n"
        "       C-PARA.\n"
        "           DISPLAY WS-N.\n"
    )
    assert dead_paragraphs(scan_source(text))[0] == ()


def test_perform_varying_is_not_read_as_a_paragraph_target():
    text = _prog(
        "       MAIN-PARA.\n"
        "           PERFORM VARYING WS-N FROM 1 BY 1 UNTIL WS-N > 3\n"
        "               DISPLAY WS-N\n"
        "           END-PERFORM.\n"
        "           STOP RUN.\n"
        "       ORPHAN-PARA.\n"
        "           DISPLAY WS-N.\n"
    )
    assert dead_paragraphs(scan_source(text))[0] == ("ORPHAN-PARA",)


def test_reachability_is_transitive():
    text = _prog(
        "       MAIN-PARA.\n"
        "           PERFORM SECOND-PARA.\n"
        "           STOP RUN.\n"
        "       SECOND-PARA.\n"
        "           PERFORM THIRD-PARA.\n"
        "           GO TO FOURTH-PARA.\n"
        "       THIRD-PARA.\n"
        "           DISPLAY WS-N.\n"
        "           STOP RUN.\n"
        "       FOURTH-PARA.\n"
        "           DISPLAY WS-N.\n"
        "           STOP RUN.\n"
        "       ORPHAN-PARA.\n"
        "           DISPLAY WS-N.\n"
    )
    assert dead_paragraphs(scan_source(text))[0] == ("ORPHAN-PARA",)


def test_alter_suppresses_the_dead_paragraph_claim_and_explains_why():
    dead, note = dead_paragraphs(scan_source(src("HEAVY.cbl")))
    assert dead == ()
    assert note and "ALTER" in note
    # HEAVY does contain a paragraph nothing references; the point is that with
    # ALTER in the program we refuse to call it dead rather than guess.
    assert "DEAD-PARA" in scan_source(src("HEAVY.cbl")).paragraph_names()


def test_portfolio_totals_exclude_unmeasured_logical_counts():
    results = [count(src(n), "program", n) for n in ("FULLSUP.cbl", "BROKEN.cbl")]
    totals = portfolio_totals(results)
    assert totals["logical"] == results[0].logical
    assert totals["logical_programs_unmeasured"] == 1
    assert totals["physical"] == sum(r.physical for r in results)


def test_portfolio_logical_is_none_when_nothing_is_measurable():
    totals = portfolio_totals([count(src("BROKEN.cbl"), "program", "BROKEN.cbl")])
    assert totals["logical"] is None
