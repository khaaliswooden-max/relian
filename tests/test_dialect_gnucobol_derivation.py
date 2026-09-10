"""WP-2.7 acceptance (3) — the GnuCOBOL widths come from the seal, not a table.

WP-2.2 corrected the brief's binary table *by measurement*: ``S9(01) COMP`` is
one byte under GnuCOBOL 3.1.2, not the two the brief assumed. The lesson is not
"the table was wrong once"; it is that a hand-typed table is a **second source
of truth**, and when two sources disagree nothing in the build decides which
wins.

So the profile derives its bands from ``discovery-bench/oracle/oracle.json``
and this file proves three things:

1. the derivation reproduces every width the seal measured;
2. every band *edge* is a measured digit count, not an interpolation;
3. a hand-edit that disagrees with the seal **fails** — the planted red.

The engine keeps :data:`src.discovery.layout.BINARY_WIDTHS` because it ships
inside the customer perimeter where ``discovery-bench/`` is absent (R12). What
makes that a cache rather than a rival claim is
:func:`assert_engine_agrees_with_seal`, and what makes *that* trustworthy is
the planted red below.
"""

from __future__ import annotations

import json
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.discovery import layout                                      # noqa: E402
from src.discovery.dialects import gnucobol_3_1_2 as gnu              # noqa: E402
from src.discovery.dialects.gnucobol_3_1_2 import (                   # noqa: E402
    DerivationError,
    assert_engine_agrees_with_seal,
    derive_from_seal,
)
from tests.test_layout_roundtrip import trusted_oracle                # noqa: E402


def test_the_oracle_is_on_disk_so_this_file_tests_derivation_not_fallback() -> None:
    """If the oracle were missing, every assertion below would pass vacuously
    against the engine's own constants. Say so out loud."""
    assert gnu.ORACLE_PATH.is_file(), gnu.ORACLE_PATH
    assert derive_from_seal() is not None


def test_the_derived_bands_are_the_ones_the_seal_measured() -> None:
    derived = derive_from_seal()
    assert derived["binary_bands"] == ((2, 1), (4, 2), (9, 4), (18, 8))


def test_the_profiles_compiler_constant_is_the_oracles_recorded_toolchain() -> None:
    """The profile names its compiler from its own constant, because
    ``src/discovery/`` may not carry a compiler name in a non-docstring literal
    (D15, ``test_discovery_is_compiler_free``). Reading the oracle's
    ``toolchain`` field belongs in a test, so the pin lives here.

    A v0.2 re-seal on a different patch level therefore fails HERE, which is
    the point: the profile would otherwise keep claiming a basis it no longer
    has.
    """
    # "cobc (GnuCOBOL) 3.1.2.0" -> "3.1.2.0", the same normalisation
    # tests/test_discovery_layout.py uses for COMPILER_BASIS.
    recorded = trusted_oracle()["toolchain"]["cobc"]        # type: ignore[index]
    version = str(recorded).rsplit(" ", 1)[-1]
    assert gnu.COMPILER == f"GnuCOBOL {version}", (
        f"the profile claims {gnu.COMPILER!r} but the oracle was measured on "
        f"{recorded!r}"
    )
    assert gnu.PROFILE.basis is not None
    assert gnu.COMPILER in gnu.PROFILE.basis
    assert gnu.SEAL_TAG in gnu.PROFILE.basis


def test_the_engine_constant_agrees_with_the_seal() -> None:
    """Acceptance (3), green side."""
    assert tuple(layout.BINARY_WIDTHS) == gnu.BINARY_BANDS
    assert_engine_agrees_with_seal()


def test_every_band_edge_is_a_digit_count_the_corpus_actually_measured() -> None:
    """A band whose edge is interpolated is a guess with a table's authority.

    The corpus measures 1, 2, 4, 8, 9 and 18 digits. The four band edges are
    2, 4, 9 and 18 — all measured. Interior counts (3, 5-7, 10-17) ride the
    step rule, and :data:`BINARY_MEASURED_DIGITS` is what lets a report say
    which is which instead of implying all eighteen were measured.
    """
    measured = set(gnu.BINARY_MEASURED_DIGITS)
    assert measured == {1, 2, 4, 8, 9, 18}
    for edge, _width in gnu.BINARY_BANDS:
        assert edge in measured, (
            f"band edge {edge} is not a measured digit count; it was "
            f"interpolated, and an interpolated edge is a guess carrying a "
            f"table's authority"
        )


def test_every_measured_point_round_trips_through_the_profile() -> None:
    derived = derive_from_seal()
    assert derived["binary_points"] == {1: 1, 2: 1, 4: 2, 8: 4, 9: 4, 18: 8}
    for digits, width in derived["binary_points"].items():
        assert gnu.PROFILE.binary(digits).bytes_ == width
    for digits, width in derived["packed_points"].items():
        assert gnu.PROFILE.packed(digits).bytes_ == width
    for size, width in derived["display_points"].items():
        assert gnu.PROFILE.display(size).bytes_ == width


def test_the_derivation_reads_element_size_and_not_length() -> None:
    """``element_size`` is one occurrence; ``length`` can be a whole table.

    In *this* oracle the two never differ on an elementary row — occurrences
    are expanded into separate rows, each carrying its own member width — so
    reading the wrong field would be invisible against the sealed corpus. That
    is exactly why the field being read is asserted directly, on a synthetic
    row built to make the two disagree, rather than inferred from the corpus
    happening not to distinguish them.
    """
    oracle = trusted_oracle()
    elementary_differ = [
        row
        for cb in oracle["copybooks"]
        for v in cb["variants"]
        for row in v["fields"]
        if row["elementary"]
        and (row["listing"] or {}).get("element_size") is not None
        and row["length"] != (row["listing"] or {}).get("element_size")
    ]
    assert elementary_differ == [], (
        "an elementary oracle row now distinguishes length from element_size; "
        "this test's synthetic construction below is no longer the only way to "
        "tell which field the derivation reads"
    )

    synthetic = {
        "toolchain": {"cobc": "cobc (GnuCOBOL) 3.1.2.0"},
        "copybooks": [{
            "variants": [{
                "fields": [{
                    "elementary": True,
                    # A 40-entry table of a 2-byte member: element_size is the
                    # member, length is the table. Only one of them is a width.
                    "length": 80,
                    "listing": {"picture": "S9(04) COMP", "element_size": 2},
                }],
            }],
        }],
    }
    observed = gnu._observations(synthetic)
    assert observed["binary"] == {4: 2}, (
        f"the derivation read {observed['binary']!r}; reading `length` would "
        f"give {{4: 80}} and would still look like a plausible table"
    )


# --------------------------------------------------------------------------
# Acceptance (3) — the planted red
# --------------------------------------------------------------------------

@pytest.mark.parametrize(
    "hand_edit, note",
    [
        (((4, 2), (9, 4), (18, 8)), "IBM's table typed into the GnuCOBOL engine"),
        (((2, 1), (4, 2), (9, 4), (18, 16)), "one band edge widened by hand"),
        (((2, 2), (4, 2), (9, 4), (18, 8)), "the WP-2.2 brief's original error"),
    ],
)
def test_a_hand_edit_that_disagrees_with_the_seal_fails(
    monkeypatch: pytest.MonkeyPatch, hand_edit, note: str
) -> None:
    """Acceptance (3)'s planted red, three ways.

    The third case is the real one: it is exactly the table WP-2.2's brief
    asserted before measurement corrected it. If this test can be made to pass
    with that table in place, the seal has stopped being the source of truth.
    """
    monkeypatch.setattr(layout, "BINARY_WIDTHS", hand_edit)
    with pytest.raises(DerivationError, match="The seal wins"):
        assert_engine_agrees_with_seal()


def test_a_hand_edit_to_the_packed_formula_fails(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """The packed rule is a formula rather than a table, and a formula can be
    edited just as quietly."""
    monkeypatch.setattr(layout, "packed_width", lambda digits: digits)
    with pytest.raises(DerivationError, match="packed item"):
        assert_engine_agrees_with_seal()


def test_a_non_monotonic_oracle_is_refused_rather_than_reduced(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """If the seal itself could not be reduced to a step function, the honest
    outcome is a refusal naming the problem — not a table that hides it."""
    with pytest.raises(DerivationError, match="not monotonic"):
        gnu._bands({1: 4, 2: 2})


def test_two_conflicting_widths_for_one_digit_count_are_an_accusation() -> None:
    """D16. Disagreement inside the sealed artifact is an oracle accusation and
    halts the work package; it is not repaired by picking one."""
    oracle = json.loads(gnu.ORACLE_PATH.read_text())
    # The picture has to occur MORE THAN ONCE for a conflict to be possible:
    # ``S9(04) COMP`` appears three times in the corpus, ``S9(01) COMP`` once.
    # Mutating a unique row would silently redefine the table instead of
    # contradicting it, and this test would pass while asserting nothing.
    rows = [
        row
        for cb in oracle["copybooks"]
        for v in cb["variants"]
        for row in v["fields"]
        if row["elementary"]
        and ((row.get("listing") or {}).get("picture") or "") == "S9(04) COMP"
    ]
    assert len(rows) >= 2, f"expected repeated S9(04) COMP rows, found {len(rows)}"
    rows[0]["listing"]["element_size"] += 3
    with pytest.raises(DerivationError, match="two different widths"):
        gnu._observations(oracle)
