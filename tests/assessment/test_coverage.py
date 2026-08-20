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
