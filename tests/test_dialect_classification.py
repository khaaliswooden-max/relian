"""WP-2.7 acceptance (2) and (5) — every field classified, and the shift asserted.

Two things are proved here, and the second is the one worth money.

**(2) No field is unclassified.** Over the whole sealed corpus, every field
comes back ``INVARIANT``, ``DIALECT_SENSITIVE`` or ``UNKNOWN``, and
:func:`lint_sensitivity` fails on anything else. Same discipline as WP-2.2
(7)'s ``source`` requirement: an unlabelled offset looks exactly like a
verified one to a reader.

**(5) The downstream shift.** A one-byte widening moves every subsequent field
in the record, and inside an ``OCCURS`` it multiplies by the occurrence count.
``OCCURSHIFT.cpy`` is built for exactly that: one 1-digit ``COMP`` inside a
40-entry table turns a one-byte rule difference into a **+40** record delta.
``SYNCCOUPLE.cpy`` proves the coupling the WP insists on — D10 sensitivity is
not independent of member width, because the alignment boundary *follows* the
width. There, a one-byte width change produces a **two**-byte record change,
since widening the item also brings an alignment boundary into existence that
did not constrain the narrower item at all.

The fixtures live under ``tests/fixtures/dialects/`` and are **not** part of
RELIAN-DISCOVERY-BENCH. ``discovery-bench/`` is frozen under rule 4 and is read
here, never written, and there is no re-seal in this work package.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.discovery.layout import compute_text                          # noqa: E402
from src.discovery.dialects.classify import (                          # noqa: E402
    Classification,
    analyse_path,
    lint_sensitivity,
)
from src.discovery.dialects.gnucobol_3_1_2 import PROFILE as GNUCOBOL  # noqa: E402
from src.discovery.dialects.ibm_enterprise_cobol import PROFILE as IBM # noqa: E402

CORPUS = REPO_ROOT / "discovery-bench" / "corpus"
FIXTURES = REPO_ROOT / "tests" / "fixtures" / "dialects"

CORPUS_FILES = sorted(p.name for p in CORPUS.glob("*.cpy"))


def report_for(path: Path):
    r = analyse_path(path, GNUCOBOL, IBM)
    assert r is not None, f"no record recovered from {path}"
    return r


# --------------------------------------------------------------------------
# Acceptance (2)
# --------------------------------------------------------------------------

def test_the_corpus_is_the_fifteen_copybooks_the_seal_covers() -> None:
    assert len(CORPUS_FILES) == 15, CORPUS_FILES


@pytest.mark.parametrize("name", CORPUS_FILES)
def test_every_field_in_the_sealed_corpus_carries_a_class(name: str) -> None:
    report = report_for(CORPUS / name)
    assert report.fields, f"{name} produced no rows at all"
    for f in report.fields:
        assert isinstance(f.classification, Classification), (
            f"{name}:{f.key} has classification {f.classification!r}"
        )


@pytest.mark.parametrize("name", CORPUS_FILES)
def test_the_lint_is_clean_over_the_sealed_corpus(name: str) -> None:
    assert lint_sensitivity(report_for(CORPUS / name)) == []


def test_the_lint_catches_a_field_with_no_reason_for_a_non_invariant_class() -> None:
    """Planted red for (2). A class without a reason cannot be acted on."""
    import dataclasses

    report = report_for(CORPUS / "D02_binary.cpy")
    victim = next(
        f for f in report.fields
        if f.classification is Classification.DIALECT_SENSITIVE
    )
    broken = dataclasses.replace(victim, reason=None)
    patched = dataclasses.replace(
        report,
        fields=tuple(broken if f.key == victim.key else f for f in report.fields),
    )
    problems = lint_sensitivity(patched)
    assert any("with no reason" in p for p in problems), problems


def test_the_lint_catches_an_invariant_field_that_shifted_without_saying_so() -> None:
    """The row most likely to be misread as a safe offset: invariant width at a
    moved address."""
    import dataclasses

    report = report_for(CORPUS / "D02_binary.cpy")
    victim = next(
        f for f in report.fields
        if f.classification is Classification.INVARIANT and f.shifted
    )
    broken = dataclasses.replace(victim, reason=None)
    patched = dataclasses.replace(
        report,
        fields=tuple(broken if f.key == victim.key else f for f in report.fields),
    )
    assert any("INVARIANT and shifted" in p for p in lint_sensitivity(patched))


def test_the_invariant_count_is_the_majority_and_is_a_positive_finding() -> None:
    """D37. The invariant count is stated as a finding, not as a remainder — so
    it had better be a real count over the whole corpus."""
    totals = {c.value: 0 for c in Classification}
    for name in CORPUS_FILES:
        for k, v in report_for(CORPUS / name).counts().items():
            totals[k] += v
    assert totals[Classification.INVARIANT.value] == 127
    assert totals[Classification.DIALECT_SENSITIVE.value] == 3
    assert totals[Classification.UNKNOWN.value] == 0
    assert sum(totals.values()) == 130


# --------------------------------------------------------------------------
# Acceptance (5) — D02_binary, per-profile widths and the downstream shift
# --------------------------------------------------------------------------

def test_d02_binary_is_sensitive_with_the_exact_per_profile_widths() -> None:
    """The measured premise of this whole work package.

    ``S9(01)``/``S9(02) COMP`` are ONE byte under GnuCOBOL — ``binary-size
    1-2-4-8`` gives 1-2 digits a single byte — and TWO under IBM's
    halfword-for-1-through-4-digits rule. Every other binary band agrees, which
    is why the sensitive set is small and nameable.
    """
    report = report_for(CORPUS / "D02_binary.cpy")
    by_key = {f.key: f for f in report.elementary}

    sensitive = {
        f.key for f in report.elementary
        if f.classification is Classification.DIALECT_SENSITIVE
    }
    assert sensitive == {"D02-COMP-1", "D02-COMP-2"}

    for key in ("D02-COMP-1", "D02-COMP-2"):
        assert by_key[key].measured_length == 1
        assert by_key[key].projected_length == 2
        assert by_key[key].width_delta == 1

    # The bands that agree, asserted so a regression in either profile shows up
    # as a NEW sensitivity rather than as a silent change of class.
    for key, width in (
        ("D02-COMP-4", 2), ("D02-COMP-8", 4), ("D02-COMP-9", 4),
        ("D02-COMP-18", 8), ("D02-COMP4-4", 2), ("D02-BINARY-9", 4),
        ("D02-COMP5-4", 2), ("D02-COMP5-18", 8), ("D02-COMP-UNSIGNED", 2),
    ):
        assert by_key[key].measured_length == width
        assert by_key[key].projected_length == width
        assert by_key[key].classification is Classification.INVARIANT


def test_d02_downstream_fields_shift_and_the_record_grows_by_two() -> None:
    """Acceptance (5): the shift, computed and asserted."""
    report = report_for(CORPUS / "D02_binary.cpy")
    by_key = {f.key: f for f in report.elementary}

    assert (report.measured_length, report.projected_length) == (38, 40)
    assert report.record_delta == 2

    # D02-COMP-2 sits after one widened field: +1. Everything after BOTH
    # widened fields carries the full +2.
    assert by_key["D02-COMP-2"].offset_delta == 1
    for key in (
        "D02-COMP-4", "D02-COMP-8", "D02-COMP-9", "D02-COMP-18",
        "D02-COMP4-4", "D02-BINARY-9", "D02-COMP5-4", "D02-COMP5-18",
        "D02-COMP-UNSIGNED",
    ):
        assert by_key[key].offset_delta == 2, key

    # The first field cannot shift: nothing precedes it.
    assert by_key["D02-COMP-1"].offset_delta == 0

    moved = report.shifted_after("D02-COMP-1")
    assert len(moved) == 10


def test_a_customer_using_measured_offsets_under_ibm_reads_one_byte_early() -> None:
    """The sentence this work package exists to prevent being wrong.

    ``D02-COMP-4`` is at offset 3 as measured and offset 5 as projected. A
    customer loading with the measured offset on a mainframe would start two
    bytes early on that field. That is the failure mode, stated as an
    assertion so it cannot quietly stop being modelled.
    """
    report = report_for(CORPUS / "D02_binary.cpy")
    field = next(f for f in report.elementary if f.key == "D02-COMP-4")
    assert (field.measured_offset, field.projected_offset) == (3, 5)
    assert field.classification is Classification.INVARIANT
    assert field.shifted and "does not hold there" in (field.reason or "")


# --------------------------------------------------------------------------
# Acceptance (5) — inside an OCCURS, the widening multiplies
# --------------------------------------------------------------------------

def test_a_sensitive_field_inside_an_occurs_multiplies_by_the_occurrence_count() -> None:
    """Acceptance (5)'s table case, with the record-length delta asserted.

    Member is 4 bytes measured (1 + 3) and 5 projected (2 + 3). Forty entries:
    160 -> 200. Plus a 2-byte header and a 1-byte trailer: 163 -> 203. A ONE
    byte rule difference, a FORTY byte record difference.
    """
    report = report_for(FIXTURES / "OCCURSHIFT.cpy")

    assert (report.measured_length, report.projected_length) == (163, 203)
    assert report.record_delta == 40

    ctr = [f for f in report.elementary if f.name == "T-CTR"]
    assert len(ctr) == 40, "the table did not expand to 40 occurrences"
    assert all(
        f.classification is Classification.DIALECT_SENSITIVE and f.width_delta == 1
        for f in ctr
    )

    # The last entry carries the accumulated shift of the 39 before it.
    last_ctr = ctr[-1]
    assert last_ctr.measured_offset == 159
    assert last_ctr.projected_offset == 198
    assert last_ctr.offset_delta == 39

    trailer = next(f for f in report.elementary if f.name == "T-TRAILER")
    assert trailer.classification is Classification.INVARIANT
    assert trailer.offset_delta == 40, (
        "the trailer must carry the FULL table delta; anything less means the "
        "widening was added once instead of per occurrence"
    )
    assert lint_sensitivity(report) == []


# --------------------------------------------------------------------------
# Acceptance (5) — D10_sync, coupled to member width
# --------------------------------------------------------------------------

def test_d10_sync_sensitivity_is_the_alignment_rule_not_a_width() -> None:
    """D10's binary members are all >=4 digits, so their WIDTHS agree.

    What does not agree is the alignment function: GnuCOBOL aligns a SYNC item
    to its own width (m=8 for the 18-digit item), IBM aligns binary items of
    five digits or more to a fullword (m=4). At D10's offsets the two give the
    same answer, so the numbers coincide — and the field is still
    DIALECT_SENSITIVE, because two different functions that agree at one offset
    are not the same function. Calling it INVARIANT would assert a shared rule
    that is not shared.
    """
    report = report_for(CORPUS / "D10_sync.cpy")
    by_key = {f.key: f for f in report.elementary}

    assert (report.measured_length, report.projected_length) == (25, 25)
    assert report.record_delta == 0

    bin_c = by_key["D10-BIN-C"]
    assert bin_c.classification is Classification.DIALECT_SENSITIVE
    assert bin_c.width_delta == 0 and bin_c.offset_delta == 0
    assert "alignment boundary differs" in (bin_c.reason or "")
    assert "coincide at this offset" in (bin_c.reason or "")
    assert bin_c.rules == ("sync_alignment",)

    # The <=4 and 5-9 digit members align identically under both rules.
    for key in ("D10-BIN-A", "D10-BIN-B"):
        assert by_key[key].classification is Classification.INVARIANT


def test_sync_sensitivity_is_coupled_to_member_width_not_independent_of_it() -> None:
    """The coupling, on a fixture built to isolate it.

    ``S-CTR PIC S9(01) COMP SYNC`` is 1 byte under GnuCOBOL, and a 1-byte
    boundary constrains nothing, so NO slack is inserted. Under IBM the same
    item is 2 bytes AND takes a halfword boundary, so slack appears where there
    was none. The record grows by TWO bytes from a ONE byte width rule: the
    boundary moved because the width moved. Testing D10 as though alignment
    were independent of member width would miss this entirely.
    """
    report = report_for(FIXTURES / "SYNCCOUPLE.cpy")
    by_key = {f.key: f for f in report.elementary}

    assert (report.measured_length, report.projected_length) == (6, 8)
    assert report.record_delta == 2, (
        "a one-byte width difference produced a one-byte record difference, so "
        "the alignment boundary did NOT follow the width — the coupling is gone"
    )

    ctr = by_key["S-CTR"]
    assert ctr.classification is Classification.DIALECT_SENSITIVE
    assert (ctr.measured_length, ctr.projected_length) == (1, 2)
    assert (ctr.measured_offset, ctr.projected_offset) == (2, 3)

    tail = by_key["S-TAIL"]
    assert tail.classification is Classification.INVARIANT
    assert tail.offset_delta == 2, (
        "S-TAIL must absorb the width byte AND the slack byte; +1 would mean "
        "no slack was inserted under the IBM rule"
    )

    # Directly: GnuCOBOL has no boundary for a 1-byte item, IBM has a halfword.
    assert GNUCOBOL.sync_boundary(1, 1) is None
    assert IBM.sync_boundary(1, 2) == 2


# --------------------------------------------------------------------------
# UNKNOWN is reachable, and it is not a zero
# --------------------------------------------------------------------------

def test_an_unmeasured_usage_classifies_unknown_and_leaves_length_undetermined() -> None:
    """R1/R2. ``COMP-1`` is unmeasured by the seal, so there is no GnuCOBOL side
    to compare a sourced IBM rule against. The field is UNKNOWN and the record
    length is ``None`` — undetermined, never ``0``, never estimated."""
    report = report_for(FIXTURES / "UNSOURCED.cpy")
    by_key = {f.key: f for f in report.elementary}

    unknown = by_key["U-FLOAT"]
    assert unknown.classification is Classification.UNKNOWN
    assert unknown.measured_length is None
    assert unknown.projected_length is None
    assert unknown.width_delta is None
    assert "not covered by RELIAN-DISCOVERY-BENCH" in (unknown.reason or "")

    assert report.measured_length is None
    assert report.projected_length is None
    assert report.record_delta is None
    assert "undetermined rather than estimated" in report.headline()
    assert lint_sensitivity(report) == []


def test_no_corpus_field_is_unknown_and_that_is_a_measured_finding() -> None:
    """Every construct the sealed corpus exercises is sourced on BOTH sides.

    Worth asserting rather than assuming: if a later edit removed an IBM
    citation, the affected fields would silently become UNKNOWN and the
    projection would quietly cover less than it says.
    """
    for name in CORPUS_FILES:
        report = report_for(CORPUS / name)
        assert report.unknown == (), (
            f"{name} now has UNKNOWN fields: "
            f"{[f.key for f in report.unknown]}. An IBM rule was probably lost."
        )
