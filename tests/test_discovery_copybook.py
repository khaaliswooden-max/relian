"""WP-2.2 §3.1 — the copybook resolver.

Six things this file pins, each of which was a measured defect or a measured
property rather than a guess:

1. **Fixed-format margins.** Columns 7-72 only, ``*``/``/`` comment lines
   skipped. CardDemo carries a sequence number in columns 1-6 *and* a
   change-id in 73-80 on the same lines, so a resolver that reads whole lines
   reads data that is not code.
2. **The ``REPLACING`` false positive.** WP-2.0.0 §0.4 measured the naive
   ``\\bCOPY\\s+`` pattern inventing a copybook named ``REPLACING`` out of
   ``INITIALIZE REQUEST-MSG-COPY  REPLACING NUMERIC BY ZEROES`` at
   ``app/app-vsam-mq/cbl/CODATE01.cbl:294``. That line is a fixture here, not
   a footnote, and both halves are asserted: the phantom is not reported AND a
   real ``COPY`` in the same file still is.
3. **Unresolvable ``COPY`` is a reported outcome, never an exception** (D20,
   R2). It is the normal condition of real mainframe code — CardDemo cannot
   resolve ``DFHAID``, ``DFHBMSCA`` or six ``CMQ*`` members — and "here are the
   copybooks you are missing and who references them, before you start" is a
   capability a customer pays for. Most vendors report it as a crash.
4. **Cycles are detected and reported, never followed.**
5. **``COPY ... REPLACING ==a== BY ==b==`` substitutes before layout**, and the
   substitution is visible in the resulting layout. CardDemo has 40 such sites,
   so it is not optional.
6. **The resolver does not need the ProLeap preprocessor** (D17).
"""

from __future__ import annotations

from pathlib import Path

import pytest

from src.discovery import LayoutStatus, compute, resolve
from src.discovery.copybook import (
    apply_replacing,
    replacing_limitations,
    assemble,
    code_lines,
    detect_cycles,
    find_copy_directives,
)

REPO_ROOT = Path(__file__).resolve().parents[1]
CORPUS = REPO_ROOT / "discovery-bench" / "corpus"


def _fixed(*lines: str) -> str:
    """Build fixed-format source: 6 columns of sequence area, then column 7."""
    return "\n".join(f"{i:06d}{line}" for i, line in enumerate(lines, 1)) + "\n"


# ==========================================================================
# 1. fixed-format margins
# ==========================================================================

def test_comment_lines_are_skipped() -> None:
    source = _fixed(
        "*    COPY GHOSTA.",
        "/    COPY GHOSTB.",
        "     COPY REALONE.",
    )
    names = [d.name for d in find_copy_directives(source, "x.cbl")]
    assert names == ["REALONE"]


def test_the_identification_area_is_not_program_text() -> None:
    """Columns 73-80 carry a change-id in CardDemo. Text there is not code."""
    line = "000100     MOVE 1 TO WS-X.".ljust(72) + "COPY XYZ"
    assert find_copy_directives(line + "\n", "x.cbl") == []


def test_the_sequence_area_is_not_program_text() -> None:
    """A ``COPY`` in columns 1-6 is a sequence number, not a directive."""
    assert find_copy_directives("COPY A     MOVE 1 TO WS-X.\n", "x.cbl") == []


def test_code_lines_preserve_original_line_numbers() -> None:
    source = _fixed("*  a comment", "", "     COPY REALONE.")
    lines = code_lines(source)
    assert [line.lineno for line in lines] == [3]
    assert lines[0].text.strip() == "COPY REALONE."


# ==========================================================================
# 2. the measured REPLACING false positive
# ==========================================================================

#: Reproduced from CardDemo (Apache-2.0), app/app-vsam-mq/cbl/CODATE01.cbl:294,
#: with its sequence area and change-id intact — those margins are half of why
#: the naive scan goes wrong.
CODATE01_LINE_294 = (
    "031500     INITIALIZE REQUEST-MSG-COPY  REPLACING NUMERIC BY ZEROES   02940012"
)


def test_hyphenated_identifier_tail_is_not_a_copy_directive() -> None:
    """The measured false positive. ``\\bCOPY`` matches here; this must not.

    ``\\b`` sits happily between ``-`` and ``C``, so the naive pattern reports a
    copybook named ``REPLACING``. The lookbehind ``(?<![A-Za-z0-9$_-])`` is the
    whole difference.
    """
    directives = find_copy_directives(CODATE01_LINE_294 + "\n", "CODATE01.cbl")
    assert directives == [], [d.name for d in directives]


def test_a_real_copy_in_the_same_file_is_still_found() -> None:
    """The other half. A guard that suppressed both would also be "green"."""
    source = CODATE01_LINE_294 + "\n" + _fixed("     COPY CMQGMOV.")
    names = [d.name for d in find_copy_directives(source, "CODATE01.cbl")]
    assert names == ["CMQGMOV"]


def test_an_01_item_whose_name_ends_in_copy_is_not_a_directive() -> None:
    source = _fixed("   01  REQUEST-MSG-COPY.", "       05  RM-A  PIC X(04).")
    assert find_copy_directives(source, "x.cbl") == []


@pytest.mark.parametrize(
    "line",
    [
        "     MOVE WS-COPY TO WS-B.",
        "     MOVE WS-A TO HARD-COPY.",
        "     PERFORM COPY-THE-RECORD.",
        "     MOVE 1 TO COPYBOOK-COUNT.",
    ],
)
def test_copy_inside_another_word_is_never_a_directive(line: str) -> None:
    assert find_copy_directives(_fixed(line), "x.cbl") == []


# ==========================================================================
# COPY syntax the corpus and CardDemo actually use
# ==========================================================================

def test_quoted_member_names_resolve() -> None:
    """CardDemo writes both ``COPY 'CSSTRPFY'`` and ``COPY CSSTRPFY``."""
    single = find_copy_directives(_fixed("     COPY 'CSSTRPFY'."), "x.cbl")
    double = find_copy_directives(_fixed('     COPY "CSSTRPFY".'), "x.cbl")
    assert [d.name for d in single] == [d.name for d in double] == ["CSSTRPFY"]


def test_of_library_is_captured_not_swallowed() -> None:
    directives = find_copy_directives(_fixed("     COPY MEMBER OF MYLIB."), "x.cbl")
    assert (directives[0].name, directives[0].library) == ("MEMBER", "MYLIB")


def test_a_copy_directive_may_span_lines() -> None:
    """CardDemo's 40 REPLACING sites put each pair on its own line."""
    source = _fixed(
        "         COPY CSSETATY REPLACING",
        "           ==(TESTVAR1)== BY ==ACCT-STATUS==",
        "           ==(SCRNVAR2)== BY ==ACSTTUS==",
        "           ==(MAPNAME3)== BY ==CACTUPA== .",
    )
    directives = find_copy_directives(source, "COACTUPC.cbl")
    assert len(directives) == 1
    assert directives[0].name == "CSSETATY"
    assert directives[0].lineno == 1
    assert directives[0].replacing == (
        ("(TESTVAR1)", "ACCT-STATUS"),
        ("(SCRNVAR2)", "ACSTTUS"),
        ("(MAPNAME3)", "CACTUPA"),
    )
    assert directives[0].replacing_parsed is True


def test_an_unparseable_replacing_phrase_says_so_rather_than_half_substituting() -> None:
    """R2 — an operand with no ``BY`` is not understood, and the directive
    records that instead of applying a guess."""
    directives = find_copy_directives(_fixed("     COPY M REPLACING ==A==."), "x.cbl")
    assert directives[0].replacing_parsed is False
    assert directives[0].replacing == ()


# ==========================================================================
# 5. REPLACING substitution
# ==========================================================================

def test_replacing_substitutes_pseudo_text() -> None:
    source = _fixed("   05  (TESTVAR1)-FLAG  PIC X(01).")
    out = apply_replacing(source, (("(TESTVAR1)", "ACCT-STATUS"),))
    assert "ACCT-STATUS-FLAG" in out
    assert "(TESTVAR1)" not in out


def test_replacing_refuses_to_rewrite_the_middle_of_an_identifier() -> None:
    """``==A== BY ==B==`` must not turn ``ACCT-A-FLAG`` into ``ACCT-B-FLAG``
    by accident: pseudo-text matching is word-aware."""
    source = _fixed("   05  ACCT-A-FLAG  PIC X(01).", "   05  A          PIC X(01).")
    out = apply_replacing(source, (("A", "B"),))
    assert "ACCT-A-FLAG" in out
    assert "  B          PIC X(01)." in out


def test_replacing_is_one_pass_so_output_is_not_rescanned() -> None:
    """``A -> B`` and ``B -> C`` applied together must not yield ``C``."""
    out = apply_replacing(_fixed("   05  A  PIC X."), (("A", "B"), ("B", "C")))
    assert "05  B  PIC X." in out


def test_replacing_matches_multiple_tokens_within_one_line() -> None:
    source = _fixed("   05  FIRST SECOND  PIC X(01).")
    out = apply_replacing(source, (("FIRST SECOND", "JOINED"),))
    assert "JOINED" in out


def test_an_operand_spanning_a_line_break_is_REPORTED_not_silently_missed() -> None:
    """The honest half of a real limitation.

    Program text has a margin on both sides, so the bytes between two lines
    are not whitespace and a per-line substitution cannot span them. The
    failure mode that matters is not "it did not substitute" — it is "it did
    not substitute and said nothing", which yields a layout that looks
    complete and names the wrong fields.
    """
    source = _fixed("   05  FIRST", "       SECOND  PIC X(01).")
    pairs = (("FIRST SECOND", "JOINED"),)
    assert "JOINED" not in apply_replacing(source, pairs)
    problems = replacing_limitations(source, pairs)
    assert len(problems) == 1 and "span a line break" in problems[0]


def test_a_substitution_that_would_overflow_column_72_is_reported() -> None:
    source = _fixed("   05  SHORT-NAME" + " " * 44 + "PIC X.")
    pairs = (("SHORT-NAME", "A-VERY-MUCH-LONGER-REPLACEMENT-NAME-INDEED"),)
    problems = replacing_limitations(source, pairs)
    assert any("past column 72" in p for p in problems)


def test_an_unapplied_replacing_blocks_the_layout_rather_than_shaping_it(
    tmp_path: Path,
) -> None:
    """D20/R2 at the REPLACING boundary: a copybook whose substitution could
    not be completed gets NO length, not a length built from the wrong names."""
    lib = tmp_path / "cpy"
    lib.mkdir()
    (lib / "SPLIT.cpy").write_text(_fixed(
        "   01  SPLIT-RECORD.",
        "       05  FIRST",
        "           SECOND       PIC X(04).",
    ))
    resolution = resolve(tmp_path)
    layout = compute("SPLIT", resolution)
    assert layout is not None
    from src.discovery.copybook import assemble as _assemble
    assembly = _assemble("SPLIT", resolution, (("FIRST SECOND", "JOINED"),))
    assert assembly.complete is False
    assert assembly.unapplied and "span a line break" in assembly.unapplied[0]


def test_replacing_is_visible_in_the_computed_layout(tmp_path: Path) -> None:
    """§3.1's tests-first item: the substitution reaches the layout, which is
    the only place it can be observed to have mattered."""
    lib = tmp_path / "cpy"
    lib.mkdir()
    (lib / "TEMPLATE.cpy").write_text(_fixed(
        "   01  (PREFIX)-RECORD.",
        "       05  (PREFIX)-KEY    PIC X(06).",
        "       05  (PREFIX)-AMOUNT PIC S9(07)V99 COMP-3.",
    ))
    (tmp_path / "PROG.cbl").write_text(_fixed(
        "         COPY TEMPLATE REPLACING ==(PREFIX)== BY ==CUST==.",
    ))

    resolution = resolve(tmp_path)
    directive = resolution.directives[0]
    assembly = assemble("TEMPLATE", resolution, directive.replacing)
    layouts = compute_text_of(assembly.text)

    assert layouts[0].group == "CUST-RECORD"
    assert [(f.name, f.offset, f.length) for f in layouts[0].fields] == [
        ("CUST-RECORD", 1, 11),
        ("CUST-KEY", 1, 6),
        ("CUST-AMOUNT", 7, 5),
    ]


def compute_text_of(text: str):
    from src.discovery.layout import compute_text
    return compute_text(text)


# ==========================================================================
# 3. unresolvable COPY is a report, never an exception
# ==========================================================================

def test_unresolvable_copy_returns_a_report_and_does_not_raise(tmp_path: Path) -> None:
    (tmp_path / "PROG.cbl").write_text(_fixed(
        "         COPY DFHAID.",
        "         COPY DFHBMSCA.",
    ))
    (tmp_path / "OTHER.cbl").write_text(_fixed("         COPY DFHAID."))

    resolution = resolve(tmp_path)          # must not raise

    missing = {u.name: u for u in resolution.unresolved}
    assert sorted(missing) == ["DFHAID", "DFHBMSCA"]
    assert missing["DFHAID"].referenced_by == ("OTHER.cbl", "PROG.cbl")
    assert missing["DFHAID"].reference_count == 2
    assert missing["DFHAID"].paths_searched, (
        '"missing" without "we looked here" is an accusation, not a finding'
    )
    assert missing["DFHAID"].reason


def test_a_record_depending_on_an_unresolved_copy_claims_no_length(tmp_path: Path) -> None:
    """D20/R2 — never a partial layout presented as complete."""
    (tmp_path / "HOST.cpy").write_text(_fixed(
        "   01  HOST-RECORD.",
        "       05  HOST-KEY  PIC X(04).",
        "         COPY MISSINGY.",
    ))
    resolution = resolve(tmp_path)
    layout = compute("HOST", resolution)

    assert layout is not None
    assert layout.status is LayoutStatus.NONE
    assert layout.group_length is None, "a hole must not be reported as a length"
    assert layout.fields == ()
    assert any("MISSINGY" in reason for reason in layout.reasons)


def test_computing_an_entirely_unresolved_member_names_its_referrers(tmp_path: Path) -> None:
    (tmp_path / "PROG.cbl").write_text(_fixed("         COPY CMQV."))
    resolution = resolve(tmp_path)
    layout = compute("CMQV", resolution)
    assert layout is not None
    assert layout.status is LayoutStatus.NONE
    assert layout.group_length is None
    assert "PROG.cbl" in layout.reasons[0]


def test_resolution_reports_copybooks_present_but_never_referenced(tmp_path: Path) -> None:
    """CardDemo has three. It is the mirror image of the missing-copybook
    table and a real finding: dead members cost migration effort."""
    (tmp_path / "USED.cpy").write_text(_fixed("   01  A  PIC X."))
    (tmp_path / "UNUSED1Y.cpy").write_text(_fixed("   01  B  PIC X."))
    (tmp_path / "PROG.cbl").write_text(_fixed("         COPY USED."))
    resolution = resolve(tmp_path)
    assert resolution.unreferenced == ("UNUSED1Y",)


# ==========================================================================
# 4. cycles
# ==========================================================================

def test_a_direct_cycle_is_detected_and_reported(tmp_path: Path) -> None:
    (tmp_path / "AAA.cpy").write_text(_fixed("   01  A  PIC X.", "         COPY BBB."))
    (tmp_path / "BBB.cpy").write_text(_fixed("   01  B  PIC X.", "         COPY AAA."))
    resolution = resolve(tmp_path)
    assert resolution.cycles == (("AAA", "BBB"),)


def test_a_three_node_cycle_is_detected(tmp_path: Path) -> None:
    for name, nxt in (("AAA", "BBB"), ("BBB", "CCC"), ("CCC", "AAA")):
        (tmp_path / f"{name}.cpy").write_text(
            _fixed(f"   01  {name[0]}  PIC X.", f"         COPY {nxt}.")
        )
    assert resolve(tmp_path).cycles == (("AAA", "BBB", "CCC"),)


def test_a_cycle_is_never_followed_when_assembling(tmp_path: Path) -> None:
    """Reported, not recursed into. An assembler that followed it would hang
    or blow the stack, and either way would not say what was wrong."""
    (tmp_path / "AAA.cpy").write_text(_fixed("   01  A  PIC X.", "         COPY BBB."))
    (tmp_path / "BBB.cpy").write_text(_fixed("   01  B  PIC X.", "         COPY AAA."))
    resolution = resolve(tmp_path)
    assembly = assemble("AAA", resolution)
    assert assembly.cyclic == ["AAA -> BBB -> AAA"]
    assert assembly.complete is False

    layout = compute("AAA", resolution)
    assert layout is not None and layout.status is LayoutStatus.NONE
    assert any("cycle" in reason.lower() for reason in layout.reasons)


def test_an_acyclic_graph_reports_no_cycles() -> None:
    assert detect_cycles([("a.cbl", "B"), ("b.cpy", "C")]) == ()


def test_self_reference_is_a_cycle() -> None:
    assert detect_cycles([("A.cpy", "A")]) == (("A",),)


# ==========================================================================
# Resolution over the sealed corpus, and the fan-in graph
# ==========================================================================

def test_the_sealed_corpus_resolves_completely() -> None:
    """15 copybooks, no COPY directives among them, nothing unresolved.

    Sanity for the round-trip: if the corpus itself did not resolve, a green
    layout comparison would be describing something else.
    """
    resolution = resolve(CORPUS)
    assert resolution.unresolved == ()
    assert resolution.cycles == ()
    assert len(resolution.unreferenced) == 15


def test_fan_in_and_fan_out_are_computed_from_the_edge_list(tmp_path: Path) -> None:
    (tmp_path / "SHARED.cpy").write_text(_fixed("   01  S  PIC X."))
    (tmp_path / "ONLY.cpy").write_text(_fixed("   01  O  PIC X."))
    (tmp_path / "P1.cbl").write_text(_fixed("         COPY SHARED.", "         COPY ONLY."))
    (tmp_path / "P2.cbl").write_text(_fixed("         COPY SHARED."))

    resolution = resolve(tmp_path)
    assert resolution.fan_out() == {"P1.cbl": 2, "P2.cbl": 1}
    assert resolution.fan_in() == {"SHARED": 2, "ONLY": 1}
    assert resolution.edges == (
        ("P1.cbl", "ONLY"), ("P1.cbl", "SHARED"), ("P2.cbl", "SHARED"),
    )


def test_repeated_copy_of_the_same_member_is_one_edge_but_two_directives(tmp_path: Path) -> None:
    """Edges are the graph; directives are the sites. Conflating them would
    make the CardDemo edge count and the REPLACING-site count the same
    number, and they are not."""
    (tmp_path / "M.cpy").write_text(_fixed("   01  M  PIC X."))
    (tmp_path / "P.cbl").write_text(_fixed("         COPY M.", "         COPY M."))
    resolution = resolve(tmp_path)
    assert resolution.edges == (("P.cbl", "M"),)
    assert len(resolution.directives) == 2


def test_resolve_raises_only_for_a_root_that_does_not_exist(tmp_path: Path) -> None:
    """The one exception the resolver does raise, because a missing ROOT is a
    caller error rather than a property of the codebase."""
    with pytest.raises(FileNotFoundError):
        resolve(tmp_path / "no-such-tree")


# ==========================================================================
# 6. D17 — independence from the ProLeap preprocessor
# ==========================================================================

def test_the_resolver_imports_no_antlr_parser() -> None:
    """``Cobol85Preprocessor.g4`` is vendored but not generated and blocks
    *program* parse. Copybook resolution must not be coupled to it, which is
    what decouples WP-2.2 from WP-2.5 entirely."""
    import ast

    source = (REPO_ROOT / "src" / "discovery" / "copybook.py").read_text()
    imported = set()
    for node in ast.walk(ast.parse(source)):
        if isinstance(node, ast.Import):
            imported.update(a.name.split(".")[0] for a in node.names)
        elif isinstance(node, ast.ImportFrom) and node.module and not node.level:
            imported.add(node.module.split(".")[0])
    assert "antlr4" not in imported
    assert not any("parsers" in name for name in imported)
