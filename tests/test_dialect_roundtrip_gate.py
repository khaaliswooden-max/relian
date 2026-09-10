"""WP-2.7 acceptance (1) — 186/186 under ``--dialect gnucobol-3.1.2``.

D38. The sealed oracle does not move and the round-trip is the regression gate.
The dialect layer is a transform applied *on top of* measured ground truth, so
the one thing it may not do is change the measured path. An IBM improvement
bought with a GnuCOBOL regression is a net loss.

This file re-runs the WP-2.2 round-trip with the GnuCOBOL profile passed
**explicitly**, which is a stronger statement than the default path passing.
``--dialect gnucobol-3.1.2`` resolves to ``profile=None`` in the CLI precisely
so the measured path cannot drift by construction (see
:func:`src.discovery.dialects.resolve`), and that makes it possible for the
derived profile itself to be wrong without any existing test noticing. So both
are asserted here:

* the 186 comparisons pass at tolerance zero with ``profile=PROFILE``, and
* the profile's layout is byte-identical to the default path's, on every
  copybook at every ODO extent.

The seal is verified before a single row is trusted -- ``trusted_oracle()`` is
imported from the WP-2.2 module rather than re-implemented, so there remains
exactly one path in the suite that loads ``oracle.json``.
"""

from __future__ import annotations

import sys
from pathlib import Path
from typing import List, Tuple

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.discovery.layout import compute                              # noqa: E402
from src.discovery.dialects.gnucobol_3_1_2 import PROFILE as GNUCOBOL  # noqa: E402
from tests.test_layout_roundtrip import (                             # noqa: E402
    CORPUS,
    EXPECTED_COMPARISONS,
    _variants,
    trusted_oracle,
)


def _dialect_comparisons() -> Tuple[int, List[str]]:
    """Rebuild the 186 with the profile in force. Returns (count, mismatches).

    The composition is the oracle's own (SPEC.md §2.1): one comparison per
    probe field row on ``offset`` and ``length`` together, plus one group-length
    comparison per variant. Counted, not assumed -- a harness that compares
    fewer rows than the oracle holds is green-by-skip, which this build has
    found seven times.
    """
    count = 0
    bad: List[str] = []
    for label, copybook, variant in _variants():
        layout = compute(
            CORPUS / str(copybook["file"]),
            odo_value=variant["odo_value"],
            profile=GNUCOBOL,
        )
        assert layout is not None, f"no record produced for {label}"
        rows = layout.by_key()
        for row in variant["fields"]:
            count += 1
            field = rows.get(row["key"])
            if field is None:
                bad.append(f"{label}: {row['key']} missing from engine output")
                continue
            if (field.offset, field.length) != (row["offset"], row["length"]):
                bad.append(
                    f"{label}: {row['key']} engine "
                    f"offset={field.offset} length={field.length} but oracle "
                    f"offset={row['offset']} length={row['length']}"
                )
        count += 1
        # The group-length comparison, one per variant. The oracle carries the
        # group's own length on its 01-level row rather than in `counts`.
        group_row = next(
            (r for r in variant["fields"] if r["key"] == copybook["group"]), None
        )
        if group_row is not None and layout.group_length != group_row["length"]:
            bad.append(
                f"{label}: group length {layout.group_length} but oracle "
                f"{group_row['length']}"
            )
    return count, bad


def test_the_oracle_seal_verifies_before_any_dialect_row_is_trusted() -> None:
    assert trusted_oracle()["schema"] == "relian-discovery-bench/oracle/v0.1"


def test_dialect_gnucobol_round_trips_186_of_186_at_tolerance_zero() -> None:
    """Acceptance (1). The hard gate."""
    count, bad = _dialect_comparisons()
    assert count == EXPECTED_COMPARISONS, (
        f"{count} comparisons were built under --dialect gnucobol-3.1.2, not "
        f"{EXPECTED_COMPARISONS}. The dialect layer changed how many rows are "
        f"compared, which is green-by-skip rather than agreement."
    )
    assert bad == [], (
        f"the dialect layer moved the MEASURED path — {len(bad)} of {count} "
        f"comparisons drifted. D38/escalation: fix this before anything else; "
        f"an IBM improvement bought with a GnuCOBOL regression is a net loss."
        + "".join(f"\n  {b}" for b in bad[:20])
    )


@pytest.mark.parametrize("label", [lbl for lbl, _cb, _v in _variants()])
def test_the_derived_profile_is_byte_identical_to_the_measured_path(label: str) -> None:
    """The profile must not merely agree with the oracle — it must agree with
    the engine's own default path, field for field and gap for gap.

    ``--dialect gnucobol-3.1.2`` deliberately takes the ``profile=None`` route,
    so nothing else in the suite would notice if the derived profile disagreed.
    """
    copybook, variant = next(
        (cb, v) for lbl, cb, v in _variants() if lbl == label
    )
    path = CORPUS / str(copybook["file"])
    default = compute(path, odo_value=variant["odo_value"])
    profiled = compute(path, odo_value=variant["odo_value"], profile=GNUCOBOL)
    assert default is not None and profiled is not None
    assert profiled.to_dict() == default.to_dict(), (
        f"{label}: the derived GnuCOBOL profile does not reproduce the "
        f"measured path. The profile is wrong, or the engine's constants are; "
        f"either way one of them is not the seal."
    )
