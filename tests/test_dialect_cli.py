"""WP-2.7 §3.3 — ``--dialect`` on the CLI.

Two properties, and the second is the one that matters most.

**The default is the measured layout.** ``--dialect gnucobol-3.1.2`` is the
default and returns the layout the engine was verified against. It resolves to
the engine's own ``profile=None`` path, so the measured path cannot drift by
construction (acceptance (1)).

**A projection that cannot be labelled does not ship.** The escalation trigger
says: *a projection is about to be rendered without its label — stop.* That is
mechanised as an exit code rather than a warning, because this is the
highest-cost failure mode in the product: the customer acts on offsets. The
``records`` block always stays the measured layout, and the projection is only
ever an additional, labelled section (D37).
"""

from __future__ import annotations

import json
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.discovery import dialects                                    # noqa: E402
from src.discovery.cli import EXIT_REFUSED, build_parser              # noqa: E402

CORPUS = REPO_ROOT / "discovery-bench" / "corpus"


def _run(argv, capsys):
    parser = build_parser()
    args = parser.parse_args(argv)
    code = args.func(args)
    return code, json.loads(capsys.readouterr().out)


def test_the_default_dialect_is_the_measured_one(capsys) -> None:
    code, doc = _run(["layout", str(CORPUS / "D02_binary.cpy")], capsys)
    assert code == 0
    assert doc["dialect"]["requested"] == "gnucobol-3.1.2"
    assert doc["dialect"]["kind"] == "measured"
    assert "dialect_sensitivity" not in doc, (
        "the default must return the measured layout and nothing projected"
    )


def test_the_default_output_is_byte_identical_with_the_flag_spelled_out(
    capsys,
) -> None:
    """Passing the default explicitly must change nothing."""
    _code, implicit = _run(["layout", str(CORPUS / "D10_sync.cpy")], capsys)
    _code, explicit = _run(
        ["layout", str(CORPUS / "D10_sync.cpy"),
         "--dialect", "gnucobol-3.1.2"], capsys,
    )
    assert implicit == explicit


def test_an_ibm_projection_is_added_and_the_records_stay_measured(capsys) -> None:
    code, doc = _run(
        ["layout", str(CORPUS / "D02_binary.cpy"),
         "--dialect", "ibm-enterprise-cobol"], capsys,
    )
    assert code == 0
    assert doc["dialect"]["kind"] == "projected"
    assert doc["dialect"]["records_basis"] == "measured"

    # D37 / the escalation trigger: the measured layout is the deliverable.
    measured = {f["key"]: f for f in doc["records"][0]["fields"]}
    assert (measured["D02-COMP-1"]["offset"], measured["D02-COMP-1"]["length"]) == (1, 1)
    assert (measured["D02-COMP-4"]["offset"], measured["D02-COMP-4"]["length"]) == (3, 2)

    section = doc["dialect_sensitivity"][0]
    assert section["counts"] == {
        "INVARIANT": 9, "DIALECT_SENSITIVE": 2, "UNKNOWN": 0,
    }
    assert section["measured_record_length"] == 38
    assert section["projected_record_length"] == 40
    assert section["record_length_delta"] == 2

    assert any("PROJECTION" in lim for lim in doc["limitations"])


def test_every_projected_quantity_names_its_basis(capsys) -> None:
    """Acceptance (6) at the CLI boundary, where a downstream tool reads it."""
    _code, doc = _run(
        ["layout", str(CORPUS / "D02_binary.cpy"),
         "--dialect", "ibm-enterprise-cobol"], capsys,
    )
    for row in doc["dialect_sensitivity"][0]["fields"]:
        assert row["measured"]["basis"] == "measured"
        assert row["projected"]["basis"] == "projected"


def test_the_cli_refuses_rather_than_emit_an_unlabelled_projection(
    capsys, monkeypatch: pytest.MonkeyPatch,
) -> None:
    """The escalation trigger, mechanised.

    The lint is forced to fail and the CLI must refuse with
    :data:`EXIT_REFUSED` and emit **no** projection — not warn and continue.
    An unlabelled projected offset is the most expensive defect this product
    can ship, so it gets an exit code.
    """
    monkeypatch.setattr(
        dialects, "lint_projection_render",
        lambda blocks: ["planted: a projected column carries no basis"],
    )
    code, doc = _run(
        ["layout", str(CORPUS / "D02_binary.cpy"),
         "--dialect", "ibm-enterprise-cobol"], capsys,
    )
    assert code == EXIT_REFUSED
    assert doc["refused"] is True
    assert "dialect_sensitivity" not in doc
    assert any("planted" in p for p in doc["problems"])


def test_an_unknown_dialect_is_rejected_by_the_parser() -> None:
    """Refused, not silently defaulted to the measured layout: answering a
    different question than the one asked, with no way to tell from the
    output, is worse than an error."""
    with pytest.raises(SystemExit):
        build_parser().parse_args(
            ["layout", "x.cpy", "--dialect", "micro-focus"]
        )


def test_the_registry_maps_the_measured_profile_onto_the_engine_default() -> None:
    """Why acceptance (1) holds structurally rather than by vigilance."""
    assert dialects.engine_profile(dialects.GNUCOBOL_3_1_2) is None
    assert dialects.engine_profile(dialects.IBM_ENTERPRISE_COBOL) is \
        dialects.IBM_ENTERPRISE_COBOL
    assert dialects.DEFAULT_DIALECT == dialects.GNUCOBOL_3_1_2.id
    with pytest.raises(dialects.UnknownDialect):
        dialects.resolve("fujitsu")


# --------------------------------------------------------------------------
# Bugbot finding (High): --root dropped the projection while claiming one
# --------------------------------------------------------------------------

def test_the_root_path_produces_the_projection_it_claims(tmp_path, capsys) -> None:
    """`--root` is the documented path for nested COPY.

    It used to skip the sensitivity section entirely while the document still
    reported `kind: projected` — a payload claiming a projection it did not
    contain. The projection now runs over the ASSEMBLED text, which is the
    same parse the records came from.
    """
    member = tmp_path / "D02BIN.cpy"
    member.write_text(
        (CORPUS / "D02_binary.cpy").read_text(encoding="utf-8"), encoding="utf-8"
    )
    code, doc = _run(
        ["layout", str(member), "--root", str(tmp_path),
         "--dialect", "ibm-enterprise-cobol"], capsys,
    )
    assert code == 0
    assert doc["dialect"]["sensitivity_present"] is True
    assert "dialect_sensitivity" in doc
    section = doc["dialect_sensitivity"][0]
    assert section["counts"]["DIALECT_SENSITIVE"] == 2
    assert (section["measured_record_length"], section["projected_record_length"]) \
        == (38, 40)


def test_an_unavailable_projection_is_stated_not_implied(tmp_path, capsys) -> None:
    """R2. When there is genuinely no parse to project from, the absence is
    named — because a reader takes a missing sensitivity section for "no
    differences found", which is the opposite of the truth."""
    member = tmp_path / "D02BIN.cpy"
    member.write_text(
        (CORPUS / "D02_binary.cpy").read_text(encoding="utf-8"), encoding="utf-8"
    )
    empty = tmp_path / "empty"
    empty.mkdir()
    code, doc = _run(
        ["layout", str(member), "--root", str(empty),
         "--dialect", "ibm-enterprise-cobol"], capsys,
    )
    assert doc["dialect"]["sensitivity_present"] is False
    assert "did not resolve" in doc["dialect"]["projection_unavailable"]
    assert "dialect_sensitivity" not in doc
    assert any("NO PROJECTION was produced" in lim for lim in doc["limitations"])
    assert any(
        "NOT a finding that the record is dialect-invariant" in lim
        for lim in doc["limitations"]
    )
    assert code == 0


# --------------------------------------------------------------------------
# Bugbot round 2 (Medium): an empty projection was marked present
# --------------------------------------------------------------------------

def test_a_record_less_copybook_does_not_claim_a_projection(
    tmp_path, capsys,
) -> None:
    """``sensitivity_present`` keys on what was PRODUCED, not on the input.

    It was set True whenever assembled text existed. A member with no 01/77
    record therefore emitted `sensitivity_present: true` beside an EMPTY
    `dialect_sensitivity` — the exact payload the guard was added to stop,
    reached by a different route.
    """
    member = tmp_path / "NOREC.cpy"
    member.write_text(
        "      *================================================*\n"
        "      * Comments only. No 01/77 record in this member.  *\n"
        "      *================================================*\n",
        encoding="utf-8",
    )
    code, doc = _run(
        ["layout", str(member), "--dialect", "ibm-enterprise-cobol"], capsys,
    )
    assert doc["dialect"]["sensitivity_present"] is False
    assert "no 01/77 record" in doc["dialect"]["projection_unavailable"]
    assert "dialect_sensitivity" not in doc
    assert any("NO PROJECTION was produced" in lim for lim in doc["limitations"])
    # No record at all is still a failure of the command itself.
    assert code == 1


def test_a_real_record_still_reports_its_projection_present(capsys) -> None:
    """The guard must not suppress the normal case."""
    _code, doc = _run(
        ["layout", str(CORPUS / "D02_binary.cpy"),
         "--dialect", "ibm-enterprise-cobol"], capsys,
    )
    assert doc["dialect"]["sensitivity_present"] is True
    assert "projection_unavailable" not in doc["dialect"]
    assert len(doc["dialect_sensitivity"]) == 1
