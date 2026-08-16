"""WP-1.2 tests — the dispatch table is the supported set, and it cannot drift.

Two guarantees are asserted here:

1. Every verb in ``SUPPORTED_STATEMENTS`` actually transpiles a minimal program
   without reaching ``unsupported()``.
2. A verb outside the table always reaches ``unsupported()`` — including the
   boundary-only tokens (``SUBTRACT`` etc.) that live in ``VERBS`` and would
   otherwise look supported.
"""

import json
from pathlib import Path

import pytest

from src.assessment.supported import (
    STATUS_ACCEPTED_IGNORED,
    boundary_only_tokens,
    data_features_by_status,
    registry_provenance,
    supported_data_features,
    supported_verbs,
)
from transpiler.c1_rulebased import (
    SUPPORTED_STATEMENTS,
    Transpiler,
    UnsupportedConstruct,
)

REPO = Path(__file__).resolve().parents[2]

WS = """\
       01 WS-LINE PIC X(80).
       01 WS-A    PIC X(10).
       01 WS-B    PIC X(10).
       01 WS-N    PIC 9(3).
       01 WS-T.
          05 WS-E PIC 9(3) OCCURS 5.
"""

# One minimal PROCEDURE DIVISION body per supported verb. Block verbs are
# given the context they require; the verb under test is always present.
BODIES = {
    "ACCEPT": ["ACCEPT WS-LINE"],
    "ADD": ["ADD 1 TO WS-N"],
    "COMPUTE": ["COMPUTE WS-N = 1 + 2"],
    "DISPLAY": ["DISPLAY WS-N"],
    "MOVE": ["MOVE 1 TO WS-N"],
    "SET": ["SET BI TO 1"],
    "STOP": ["STOP RUN"],
    "INSPECT": ['INSPECT WS-LINE TALLYING WS-N FOR ALL "X"'],
    "UNSTRING": ['UNSTRING WS-LINE DELIMITED BY "," INTO WS-A WS-B'],
    "IF": ["IF WS-N > 0", "DISPLAY WS-N", "END-IF"],
    "END-IF": ["IF WS-N > 0", "DISPLAY WS-N", "END-IF"],
    "ELSE": ["IF WS-N > 0", "DISPLAY WS-N", "ELSE", "DISPLAY WS-N", "END-IF"],
    "EVALUATE": ["EVALUATE TRUE", "WHEN WS-N > 0", "DISPLAY WS-N", "END-EVALUATE"],
    "WHEN": ["EVALUATE TRUE", "WHEN WS-N > 0", "DISPLAY WS-N", "END-EVALUATE"],
    "END-EVALUATE": ["EVALUATE TRUE", "WHEN WS-N > 0", "DISPLAY WS-N", "END-EVALUATE"],
    "PERFORM": ["PERFORM VARYING WS-N FROM 1 BY 1 UNTIL WS-N > 3",
                "DISPLAY WS-N", "END-PERFORM"],
    "END-PERFORM": ["PERFORM VARYING WS-N FROM 1 BY 1 UNTIL WS-N > 3",
                    "DISPLAY WS-N", "END-PERFORM"],
    "SEARCH": ["SET BI TO 1", "SEARCH WS-T", "AT END DISPLAY WS-N",
               "WHEN WS-E(BI) = 1", "DISPLAY WS-N", "END-SEARCH"],
}


def program(body_lines, ws: str = WS) -> str:
    body = "\n".join("           " + line for line in body_lines)
    return (
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. PROBE.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        f"{ws}"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        f"{body}\n"
        "           STOP RUN.\n"
    )


def test_supported_verbs_non_empty_and_matches_table():
    verbs = supported_verbs()
    assert verbs
    assert verbs == frozenset(SUPPORTED_STATEMENTS)


def test_every_supported_verb_has_a_fixture():
    """If a verb is added to the table, this test forces a fixture for it."""
    assert set(BODIES) == set(supported_verbs())


@pytest.mark.parametrize("verb", sorted(SUPPORTED_STATEMENTS))
def test_supported_verb_transpiles_without_hitting_unsupported(verb):
    tp = Transpiler(program(BODIES[verb]), "Probe")
    first_tokens = {s.split()[0] for s in tp.stmts}
    assert verb in first_tokens, f"{verb} never reached stmt() in its fixture"
    tp.transpile()
    assert tp.unsupported_hits == [], f"{verb} is in the table but hit unsupported()"


@pytest.mark.parametrize(
    "stmt,verb",
    [
        ("ALTER OTHER-PARA TO PROCEED TO MAIN-PARA", "ALTER"),
        ("EXEC CICS SEND MAP END-EXEC", "EXEC"),
        ("SUBTRACT 1 FROM WS-N", "SUBTRACT"),
        ("STRING WS-A WS-B DELIMITED BY SIZE INTO WS-LINE", "STRING"),
        ("GOBACK", "GOBACK"),
    ],
)
def test_verb_outside_the_table_hits_unsupported(stmt, verb):
    tp = Transpiler(program([stmt]), "Probe", strict=False)
    tp.transpile()
    assert [v for v, _ in tp.unsupported_hits] == [verb]


def test_unsupported_records_a_real_source_line():
    src = program(["ALTER OTHER-PARA TO PROCEED TO MAIN-PARA"])
    tp = Transpiler(src, "Probe", strict=False)
    tp.transpile()
    (_, line_no), = tp.unsupported_hits
    assert src.splitlines()[line_no - 1].strip().startswith("ALTER")


def test_non_strict_emits_nothing_extra():
    """Inventory mode (strict=False) is silent-but-recorded — the bytes an
    unsupported verb contributes are exactly none, as before WP-1.2."""
    with_alter = Transpiler(program(["ALTER X TO PROCEED TO Y", "DISPLAY WS-N"]),
                            "P", strict=False)
    without = Transpiler(program(["DISPLAY WS-N"]), "P", strict=False)
    assert with_alter.transpile() == without.transpile()
    assert with_alter.unsupported_hits and not without.unsupported_hits


def test_strict_mode_raises_instead_of_dropping():
    tp = Transpiler(program(["ALTER X TO PROCEED TO Y"]), "Probe", strict=True)
    with pytest.raises(UnsupportedConstruct) as exc:
        tp.transpile()
    assert exc.value.verb == "ALTER"


def test_strict_mode_is_on_by_default():
    """WP-1.5.2: silent-drop is no longer the default (R2)."""
    tp = Transpiler(program(["ALTER X TO PROCEED TO Y"]), "Probe")
    assert tp.strict is True
    with pytest.raises(UnsupportedConstruct):
        tp.transpile()


def test_strict_error_carries_verb_line_and_paragraph():
    src = program(["ALTER OTHER-PARA TO PROCEED TO MAIN-PARA"])
    tp = Transpiler(src, "Probe")
    with pytest.raises(UnsupportedConstruct) as exc:
        tp.transpile()
    assert exc.value.verb == "ALTER"
    assert src.splitlines()[exc.value.line_no - 1].strip().startswith("ALTER")
    assert exc.value.paragraph == "MAIN-PARA"
    msg = str(exc.value)
    assert "ALTER" in msg and "MAIN-PARA" in msg and str(exc.value.line_no) in msg


def test_boundary_only_tokens_are_not_reported_as_supported():
    assert "SUBTRACT" in boundary_only_tokens()
    assert "SUBTRACT" not in supported_verbs()


def test_data_features_are_three_state_and_probed():
    feats = supported_data_features()
    assert feats, "no data features probed"
    assert set(feats.values()) <= {"supported", "accepted_ignored", "unsupported"}
    assert feats["PIC 9 unsigned integer"] == "supported"
    assert feats["PIC X alphanumeric"] == "supported"
    assert feats["88-level condition name"] == "supported"


def test_comp3_is_accepted_but_not_modelled():
    """COMP-3 parses but the usage clause is discarded. If someone implements
    it, this test fails and the report's claim changes with the code."""
    assert supported_data_features()["USAGE COMP-3 (packed decimal)"] == STATUS_ACCEPTED_IGNORED
    assert "REDEFINES" in data_features_by_status(STATUS_ACCEPTED_IGNORED)


def test_provenance_names_the_source_it_was_read_from():
    prov = registry_provenance()
    assert "SUPPORTED_STATEMENTS@" in prov and "c1_rulebased.py sha256:" in prov


# --------------------------------------------------------------------------
# The WP-1.2 gate, as a permanent test rather than a one-off manual check.
# --------------------------------------------------------------------------

MAINS = json.loads((REPO / "bench" / "harness" / "mains.json").read_text())


@pytest.mark.parametrize("prog", sorted(MAINS))
def test_refactor_is_byte_identical_to_committed_candidate(prog):
    """Regenerating the C1 candidate must reproduce the committed Java exactly.

    This is the behavior-preservation gate for the WP-1.2 dispatch refactor,
    and it stays in the suite so any future edit to the transpiler that changes
    corpus output has to be a deliberate, visible act.
    """
    cls = MAINS[prog]
    src = (REPO / "bench" / "corpus" / prog / "program.cbl").read_text()
    committed = (REPO / "bench" / "candidates" / "C1_rulebased" / prog / f"{cls}.java").read_text()
    assert Transpiler(src, cls).transpile() == committed
