"""WP-1.3 tests — coverage analysis, both methods, and honest parse failure."""

from pathlib import Path

import pytest

from src.assessment.coverage import (
    analyze,
    data_feature_summary,
    detect_format,
    quotable_split,
    rollup,
    scan_source,
    statements_by_token_scan,
)
from src.assessment.intake import ingest, read_source

FIXTURES = Path(__file__).parent / "fixtures" / "cobol"


@pytest.fixture(scope="module")
def records():
    return {r.rel_path_posix: r for r in ingest(FIXTURES)}


def result_for(records, name):
    return analyze(records[name], read_source(FIXTURES / name))


def test_antlr_path_is_used_when_the_parse_is_clean(records):
    r = result_for(records, "ANTLRFIT.cbl")
    assert r.method == "antlr_tree"
    assert r.parse_ok is True
    assert r.parser_errors == ()
    assert r.coverage_ratio.grade == "VERIFIED"


def test_token_scan_path_is_used_when_the_parse_has_errors(records):
    # COPYUSER.cbl carries a COPY, which the vendored grammar cannot parse:
    # COPY is a lexer token there that no parser rule references, because
    # upstream consumes it in the separate preprocessor grammar this repo does
    # not run. It is therefore the fixture that still exercises the fallback.
    r = result_for(records, "COPYUSER.cbl")
    assert r.method == "token_scan"
    assert r.parse_ok is False
    assert r.parser_errors, "a token_scan result must say why the tree was rejected"
    assert r.coverage_ratio.grade == "PLAUSIBLE", (
        "a lexically-derived count must never be graded VERIFIED"
    )


def test_previously_unparseable_fixture_now_reaches_the_tree(records):
    """PARTIAL.cbl fell to token_scan under the reduced grammar this repo used
    before the ProLeap grammar was vendored (WP-2.0). It now parses, and the
    grade rises with it — that is the swap's whole point, so it is pinned."""
    r = result_for(records, "PARTIAL.cbl")
    assert r.method == "antlr_tree"
    assert r.parse_ok is True
    assert r.parser_errors == ()
    assert r.coverage_ratio.grade == "VERIFIED"


def test_both_methods_agree_on_the_fixture_they_can_both_read(records):
    """Cross-validation of the fallback against the tree it replaces."""
    r = result_for(records, "ANTLRFIT.cbl")
    scanned = scan_source(read_source(FIXTURES / "ANTLRFIT.cbl"))
    assert len(statements_by_token_scan(scanned)) == r.total_statements


def test_full_coverage_fixture(records):
    r = result_for(records, "FULLSUP.cbl")
    assert r.total_statements == r.supported_statements
    assert r.coverage_ratio.value == 1.0
    assert r.unsupported_inventory == ()


def test_unsupported_inventory_is_ranked_by_frequency(records):
    r = result_for(records, "HEAVY.cbl")
    ranked = r.ranked_unsupported()
    counts = [c for _, c in ranked]
    assert counts == sorted(counts, reverse=True)
    assert ranked[0] == ("EXEC", 2)


def test_single_word_statements_are_not_mistaken_for_paragraph_labels(records):
    """`GOBACK.` and `EXIT.` look exactly like paragraph labels."""
    scanned = scan_source(read_source(FIXTURES / "HEAVY.cbl"))
    counted = {h.verb for h in statements_by_token_scan(scanned)}
    # Both are counted as statements, not swallowed as paragraph labels…
    assert {"GOBACK", "EXIT"} <= counted
    assert not {"GOBACK", "EXIT"} & set(scanned.paragraph_names())
    # …and since WP-1.5.5 GOBACK is supported while bare EXIT (paragraph
    # exit, no PROGRAM qualifier) remains honestly unsupported.
    verbs = {h.verb for h in result_for(records, "HEAVY.cbl").unsupported_inventory}
    assert "EXIT" in verbs
    assert "GOBACK" not in verbs


def test_exec_products_are_distinguished(records):
    r = result_for(records, "HEAVY.cbl")
    contexts = {h.context for h in r.unsupported_inventory if h.verb == "EXEC"}
    assert contexts == {"EXEC SQL", "EXEC CICS"}


def test_hits_carry_file_line_and_paragraph(records):
    r = result_for(records, "PARTIAL.cbl")
    # The tree reports the two-word verb: a bare "GO" is not what the source
    # says, nor what the dispatch table is keyed by.
    go = next(h for h in r.unsupported_inventory if h.verb == "GO TO")
    assert go.file == "PARTIAL.cbl"
    assert go.paragraph == "OTHER-PARA"
    src_line = read_source(FIXTURES / "PARTIAL.cbl").splitlines()[go.line - 1]
    assert "GO TO" in src_line


def test_parse_failure_reports_no_ratio(records):
    r = result_for(records, "BROKEN.cbl")
    assert r.parse_ok is False
    assert r.coverage_ratio is None, "an unparseable program must not get a ratio (R1)"
    assert r.supported_statements is None
    assert r.error and "no coverage ratio" in r.error


def test_data_features_carry_the_probed_status(records):
    r = result_for(records, "HEAVY.cbl")
    found = {h.feature: h.status for h in r.data_feature_inventory}
    assert found["REDEFINES"] == "accepted_ignored"
    assert found["OCCURS DEPENDING ON (variable size)"] == "accepted_ignored"
    assert found["FILE SECTION (FD) record"] == "unsupported"


def test_comp3_is_detected_and_reported_as_accepted_not_supported(records):
    r = result_for(records, "PARTIAL.cbl")
    comp3 = [h for h in r.data_feature_inventory
             if h.feature == "USAGE COMP-3 (packed decimal)"]
    assert comp3 and comp3[0].status == "accepted_ignored"


def test_provenance_names_the_registry_and_the_file(records):
    r = result_for(records, "PARTIAL.cbl")
    prov = r.coverage_ratio.provenance
    assert "SUPPORTED_STATEMENTS@" in prov
    assert "PARTIAL.cbl" in prov
    assert "method=antlr_tree" in prov
    assert f"{r.supported_statements}/{r.total_statements}" in prov


def test_rollup_is_weighted_by_statement_count(records):
    names = ["FULLSUP.cbl", "PARTIAL.cbl", "HEAVY.cbl"]
    results = [result_for(records, n) for n in names]
    p = rollup(results)
    assert p.total_statements == sum(r.total_statements for r in results)
    assert p.supported_statements == sum(r.supported_statements for r in results)
    assert p.coverage_ratio.value == round(
        p.supported_statements / p.total_statements, 4
    )


def test_rollup_excludes_unmeasurable_programs_and_says_so(records):
    results = [result_for(records, n) for n in ["FULLSUP.cbl", "BROKEN.cbl"]]
    p = rollup(results)
    assert p.total_statements == results[0].total_statements
    assert any("BROKEN.cbl" in e for e in p.parser_errors)
    assert "excluded" in p.coverage_ratio.provenance


def test_rollup_grade_degrades_to_the_weakest_method(records):
    mixed = rollup([result_for(records, "ANTLRFIT.cbl"), result_for(records, "HEAVY.cbl")])
    assert mixed.coverage_ratio.grade == "PLAUSIBLE"
    tree_only = rollup([result_for(records, "ANTLRFIT.cbl")])
    assert tree_only.coverage_ratio.grade == "VERIFIED"


def test_rollup_with_nothing_measurable_returns_no_ratio(records):
    p = rollup([result_for(records, "BROKEN.cbl")])
    assert p.coverage_ratio is None
    assert p.error


def test_quotable_split_partitions_the_statements(records):
    results = [result_for(records, n) for n in ["FULLSUP.cbl", "HEAVY.cbl"]]
    today, needs_grammar = quotable_split(results)
    assert today + needs_grammar == sum(r.total_statements for r in results)
    assert needs_grammar > 0


def test_data_feature_summary_counts_occurrences(records):
    results = [result_for(records, n) for n in ["PARTIAL.cbl", "HEAVY.cbl"]]
    summary = data_feature_summary(results)
    assert summary["USAGE COMP-3 (packed decimal)"]["status"] == "accepted_ignored"
    assert summary["REDEFINES"]["occurrences"] >= 1
    assert all("status_provenance" in v for v in summary.values())


def test_free_format_source_is_detected_and_scanned():
    free = (
        "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. FREE.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "01 WS-N PIC 9(3).\n"
        "PROCEDURE DIVISION.\n"
        "MAIN-PARA.\n"
        "    MOVE 1 TO WS-N\n"
        "    ALTER X TO PROCEED TO Y\n"
        "    STOP RUN.\n"
    )
    scanned = scan_source(free)
    assert scanned.source_format == "free"
    verbs = [h.verb for h in statements_by_token_scan(scanned)]
    assert verbs == ["MOVE", "ALTER", "STOP"]


def test_fixed_format_is_detected_for_the_corpus_style():
    src = read_source(FIXTURES / "FULLSUP.cbl")
    assert detect_format(src.splitlines()) == "fixed"


def test_crlf_source_analyses_identically(records):
    lf = read_source(FIXTURES / "PARTIAL.cbl")
    crlf = lf.replace("\n", "\r\n")
    a = analyze(records["PARTIAL.cbl"], lf)
    b = analyze(records["PARTIAL.cbl"], crlf)
    assert a.to_dict() == b.to_dict()


def test_comment_lines_are_not_counted_as_statements():
    src = (
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. C.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 WS-N PIC 9(3).\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "      *    MOVE 1 TO WS-N\n"
        "           DISPLAY WS-N\n"
        "           STOP RUN.\n"
    )
    verbs = [h.verb for h in statements_by_token_scan(scan_source(src))]
    assert verbs == ["DISPLAY", "STOP"]


def test_data_division_verbs_are_not_counted_as_statements():
    """`USAGE DISPLAY` in WORKING-STORAGE is not a DISPLAY statement."""
    src = (
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. D.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 WS-N PIC 9(3) USAGE DISPLAY.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           STOP RUN.\n"
    )
    verbs = [h.verb for h in statements_by_token_scan(scan_source(src))]
    assert verbs == ["STOP"]


def test_scope_terminator_on_its_own_line_is_not_a_paragraph():
    """`END-IF.` looks exactly like a paragraph label; treating it as one
    creates phantom paragraphs in the complexity table and the dead-paragraph
    analysis."""
    src = (
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. E.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 WS-N PIC 9(3).\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           IF WS-N > 0\n"
        "               DISPLAY WS-N\n"
        "           END-IF.\n"
        "           STOP RUN.\n"
    )
    assert scan_source(src).paragraph_names() == ("MAIN-PARA",)


# --------------------------------------------------------------------------
# Comment entries (IDENTIFICATION DIVISION free text)
# --------------------------------------------------------------------------


def fixed(*code_lines: str) -> str:
    """Assemble fixed-format source: 6 blank sequence columns + indicator + code."""
    return "".join(f"      {line}\n" for line in code_lines)


ID_HEAD = (" IDENTIFICATION DIVISION.", " PROGRAM-ID. CE.")
TAIL = (
    " DATA DIVISION.",
    " WORKING-STORAGE SECTION.",
    " 01 WS-N PIC 9(3).",
    " PROCEDURE DIVISION.",
    " MAIN-PARA.",
    "     MOVE 1 TO WS-N",
    "     STOP RUN.",
)


def parse_errors_for(src: str):
    from src.assessment.coverage import _antlr_parse

    _tree, errors, _mod = _antlr_parse(scan_source(src).antlr_source())
    return errors


def test_author_comment_entry_parses_cleanly():
    """`AUTHOR.` free text is prose, not syntax — it must not fail the parse.

    Before comment-entry tagging this single line demoted an otherwise clean
    program from antlr_tree/VERIFIED to token_scan/PLAUSIBLE.
    """
    assert parse_errors_for(fixed(*ID_HEAD, " AUTHOR. MERIDIAN-MUD-DP.", *TAIL)) == ()


def test_every_comment_entry_paragraph_parses_cleanly():
    src = fixed(
        *ID_HEAD,
        " AUTHOR. A-DEPARTMENT.",
        " INSTALLATION. SOME-DATA-CENTRE.",
        " DATE-WRITTEN. 1987-04-12.",
        " DATE-COMPILED. 2026-08-23.",
        " SECURITY. NONE.",
        *TAIL,
    )
    assert parse_errors_for(src) == ()


def test_comment_entry_continues_across_area_b_lines():
    """A comment entry runs on while its text stays in Area B (columns 12+)."""
    src = fixed(
        *ID_HEAD,
        " AUTHOR. FIRST LINE OF PROSE.",
        "     CONTINUED PROSE, STILL THE COMMENT ENTRY.",
        "     AND MORE. WITH. PERIODS. AND 12345 DIGITS.",
        *TAIL,
    )
    assert parse_errors_for(src) == ()


def test_a_paragraph_in_area_a_ends_the_comment_entry():
    """Area A is the standard's own boundary, so the next header is still seen."""
    src = fixed(*ID_HEAD, " AUTHOR. PROSE.", " SECURITY. MORE PROSE.", *TAIL)
    assert parse_errors_for(src) == ()

    scanned = scan_source(src)
    tagged = scanned.antlr_source().splitlines()
    # The SECURITY header keeps its own line and is not swallowed as prose.
    assert any(l.lstrip().upper().startswith("SECURITY.") for l in tagged)


def test_program_id_is_never_tagged():
    """PROGRAM-ID's body is a real program name, not a comment entry."""
    scanned = scan_source(fixed(*ID_HEAD, *TAIL))
    program_id_line = [l for l in scanned.antlr_source().splitlines() if "PROGRAM-ID" in l]
    assert program_id_line and "*>CE" not in program_id_line[0]


def test_tagging_preserves_line_numbers():
    """One output line per input line, so diagnostics point at the real file."""
    src = fixed(*ID_HEAD, " AUTHOR. PROSE.", "     CONTINUED.", *TAIL)
    assert len(scan_source(src).antlr_source().splitlines()) == len(src.splitlines())


def test_tagging_does_not_reach_outside_the_identification_division():
    """A PROCEDURE DIVISION paragraph named AUTHOR-something stays code."""
    src = fixed(
        *ID_HEAD,
        " DATA DIVISION.",
        " WORKING-STORAGE SECTION.",
        " 01 WS-N PIC 9(3).",
        " PROCEDURE DIVISION.",
        " AUTHOR-CHECK.",
        "     MOVE 1 TO WS-N",
        "     STOP RUN.",
    )
    tagged = scan_source(src).antlr_source()
    assert "*>CE" not in tagged
    assert parse_errors_for(src) == ()


def test_free_format_tags_only_the_paragraph_line():
    """No reference areas in free format, so the entry cannot be tracked past
    its own line. Under-tagging costs a fallback; over-tagging would hide code.
    """
    free = (
        "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. FREE.\n"
        "AUTHOR. SOMEONE.\n"
        "SPILLED PROSE ON ITS OWN LINE.\n"
    )
    lines = scan_source(free).antlr_source().splitlines()
    assert lines[2].endswith("*>CE SOMEONE.")
    assert "*>CE" not in lines[3]


def test_a_genuine_syntax_error_is_still_reported():
    """Tagging must not launder broken source into a clean parse."""
    assert parse_errors_for(read_source(FIXTURES / "BROKEN.cbl")) != ()


# --------------------------------------------------------------------------
# Two-stage (SLL then LL) parsing
# --------------------------------------------------------------------------


@pytest.mark.parametrize("name", ["FULLSUP.cbl", "PARTIAL.cbl", "BROKEN.cbl", "HEAVY.cbl"])
def test_two_stage_parse_agrees_with_full_ll(name):
    """The SLL fast path must not change any verdict the LL parse would give.

    SLL is an approximation that either agrees with LL or errors, and on error
    `_antlr_parse` re-parses with LL — so the errors it reports must be exactly
    LL's errors, for clean and broken sources alike.
    """
    import antlr4
    from antlr4.atn.PredictionMode import PredictionMode
    from antlr4.error.ErrorListener import ErrorListener

    from src.assessment.coverage import _antlr_parse, _normalise_parse_error
    from src.parsers.antlr.cobol.Cobol85Lexer import Cobol85Lexer
    from src.parsers.antlr.cobol.Cobol85Parser import Cobol85Parser

    source = scan_source(read_source(FIXTURES / name)).antlr_source()

    class Collect(ErrorListener):
        def __init__(self):
            self.errors = []

        def syntaxError(self, recognizer, offending, line, column, msg, e):  # noqa: N802
            if len(self.errors) < 50:
                self.errors.append(f"line {line}:{column} {_normalise_parse_error(msg)}")

    collector = Collect()
    lexer = Cobol85Lexer(antlr4.InputStream(source))
    lexer.removeErrorListeners()
    lexer.addErrorListener(collector)
    ll_parser = Cobol85Parser(antlr4.CommonTokenStream(lexer))
    ll_parser.removeErrorListeners()
    ll_parser.addErrorListener(collector)
    ll_parser._interp.predictionMode = PredictionMode.LL
    ll_parser.startRule()

    _tree, staged_errors, _mod = _antlr_parse(source)
    assert list(staged_errors) == collector.errors


def test_parse_is_memoised_but_still_deterministic(records):
    """The cache is an optimisation, never a source of drift."""
    src = read_source(FIXTURES / "FULLSUP.cbl")
    first = analyze(records["FULLSUP.cbl"], src).to_dict()
    second = analyze(records["FULLSUP.cbl"], src).to_dict()
    assert first == second
