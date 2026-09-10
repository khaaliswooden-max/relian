"""WP-2.7 acceptance (3) — the GnuCOBOL 3.1.2.0 profile, DERIVED from the seal.

This module does not contain a width table. It contains the code that reads
one out of ``discovery-bench/oracle/oracle.json`` and the assertions that
refuse to proceed if what it reads disagrees with the engine.

**Why derivation rather than a table.** WP-2.2 corrected the brief's binary
table by measurement: ``D02-COMP-1 PIC S9(01) COMP`` occupies **1 byte** under
GnuCOBOL 3.1.2, not the 2 the brief assumed. A hand-typed table is a second
source of truth, and the next time the two disagree there is nothing in the
build that decides which one wins. Deriving from the sealed oracle means the
seal wins by construction: the only way to change a width here is to re-seal
the benchmark, which is an operator key ceremony (rule 4).

**The one table that remains, and why it is safe.**
:data:`src.discovery.layout.BINARY_WIDTHS` stays where it is. The layout engine
ships inside the customer perimeter (R12) where ``discovery-bench/`` is not
present, so it cannot take an import-time dependency on the oracle. What this
module does instead is *derive the bands and assert the engine agrees*
(:func:`assert_engine_agrees_with_seal`). In this repository the oracle is
present, so a hand-edit to ``BINARY_WIDTHS`` that disagrees with the seal
raises here and fails CI -- which is what "the seal wins" has to mean
operationally. ``tests/test_dialect_gnucobol_derivation.py`` plants exactly
that red.

**What is measured and what is banded.** The corpus measures binary widths at
digit counts 1, 2, 4, 8, 9 and 18. Every *band edge* the derivation produces is
one of those measured points, so no edge is interpolated. Interior digit counts
(3, 5-7, 10-17) are covered by the step rule whose edges are measured, and
:data:`BINARY_MEASURED_DIGITS` records which is which so a report can say so
rather than implying all eighteen were measured.
"""

from __future__ import annotations

import json
import math
import re
from functools import lru_cache
from pathlib import Path
from typing import Dict, Optional, Sequence, Tuple

from .base import (
    Citation,
    DialectProfile,
    ProfileKind,
    Rule,
    RuleKind,
    Width,
    assert_every_rule_is_provenanced,
)

PROFILE_ID = "gnucobol-3.1.2"
COMPILER = "GnuCOBOL 3.1.2.0"
SEAL_TAG = "relian-discovery-bench-v0.1"

#: ``src/discovery/dialects/`` -> repository root.
_REPO_ROOT = Path(__file__).resolve().parents[3]
ORACLE_PATH = _REPO_ROOT / "discovery-bench" / "oracle" / "oracle.json"


class DerivationError(RuntimeError):
    """The oracle could not be reduced to a coherent width table.

    Every raise in this module is a disagreement with the seal, and a
    disagreement with the seal is an escalation rather than something to work
    around by editing a constant.
    """


# --------------------------------------------------------------------------
# Reading the seal
# --------------------------------------------------------------------------

#: ``"S9(04) COMP-3"`` -> picture ``"S9(04)"``, usage ``"COMP-3"``. The oracle
#: stores the listing's own rendering, which is the PICTURE followed by the
#: usage token when there is one.
_PICTURE_RE = re.compile(r"\A(?P<pic>\S+)(?:\s+(?P<usage>[A-Z0-9-]+))?\s*\Z")

_BINARY_USAGES = frozenset({"COMP", "COMP-4", "BINARY", "COMP-5"})
_PACKED_USAGES = frozenset({"COMP-3", "PACKED-DECIMAL"})

#: Digit-position characters in a PICTURE. ``S``, ``V`` and ``P`` contribute no
#: storage digit of their own for the purpose of the binary/packed width rules.
_DIGIT_RE = re.compile(r"9(?:\((\d+)\))?")


def _split_picture(rendered: str) -> Tuple[str, Optional[str]]:
    m = _PICTURE_RE.match(rendered.strip())
    if m is None:
        raise DerivationError(f"oracle picture {rendered!r} is not parseable")
    return m.group("pic"), m.group("usage")


def _character_positions(picture: str) -> int:
    """Character positions an alphanumeric/alphabetic PICTURE declares.

    Delegates to the engine's own PICTURE expansion rather than counting
    ``X``/``A`` runs by hand. Insertion characters occupy storage --
    ``XXBXXBXXXX`` declares eight ``X`` and occupies TEN bytes, and
    ``XXX/XX/XXXX`` likewise -- so a hand-rolled counter undercounts every
    alphanumeric-edited picture. Measured: the first version of this function
    reported 8 for a 10-byte item and the seal gate correctly refused it.

    Using the engine here does NOT make the comparison tautological. This side
    is what Relian's parser says the PICTURE declares; the other side is what
    ``cobc`` actually allocated. Two independent derivations that happen to
    agree is a passing check, not an absent one -- which is exactly what the
    original ``{element_size: element_size}`` form was not.
    """
    from ..layout import expand_picture, picture_size

    symbols = expand_picture(picture)
    return picture_size(symbols) if symbols else 0


def _digits(picture: str) -> int:
    """Count 9-positions in ``picture``, expanding ``9(nn)`` repeats."""
    total = 0
    for m in _DIGIT_RE.finditer(picture):
        total += int(m.group(1)) if m.group(1) else 1
    return total


def _load_oracle() -> Optional[dict]:
    if not ORACLE_PATH.is_file():
        return None
    return json.loads(ORACLE_PATH.read_text(encoding="utf-8"))


def _observations(oracle: dict) -> Dict[str, Dict[int, int]]:
    """Collect ``{family: {digits: measured_bytes}}`` from the sealed oracle.

    Only ``elementary`` rows with a listing ``element_size`` are read.
    ``element_size`` is one occurrence, so an ``OCCURS`` row contributes its
    member width rather than the table's total -- reading ``length`` here
    would silently multiply the width by the occurrence count.
    """
    out: Dict[str, Dict[int, int]] = {"binary": {}, "packed": {}, "display": {}}
    for copybook in oracle["copybooks"]:
        for variant in copybook["variants"]:
            for row in variant["fields"]:
                if not row.get("elementary"):
                    continue
                listing = row.get("listing") or {}
                rendered = listing.get("picture")
                size = listing.get("element_size")
                if not rendered or not isinstance(size, int):
                    continue
                picture, usage = _split_picture(rendered)
                if usage in _BINARY_USAGES:
                    family, key = "binary", _digits(picture)
                elif usage in _PACKED_USAGES:
                    family, key = "packed", _digits(picture)
                elif usage is None and picture.upper().startswith(("X", "A")):
                    # Character strings: the display rule's own evidence.
                    #
                    # The KEY must be the count of character positions the
                    # PICTURE declares, and the VALUE the width the oracle
                    # measured. Keying on `size` -- the measured width --
                    # stored {size: size} and made the assertion below
                    # `size != size`, so the display half of the seal gate
                    # could never fail. A gate that cannot fail is not a gate.
                    family, key = "display", _character_positions(picture)
                else:
                    continue
                if key <= 0:
                    continue
                prior = out[family].get(key)
                if prior is not None and prior != size:
                    raise DerivationError(
                        f"the sealed oracle reports two different widths for a "
                        f"{family} item of {key} digit(s): {prior} and {size}. "
                        f"The seal cannot be reduced to a width function; this "
                        f"is an oracle accusation and halts the work package."
                    )
                out[family][key] = size
    return out


def _bands(points: Dict[int, int]) -> Tuple[Tuple[int, int], ...]:
    """Reduce measured ``{digits: bytes}`` to ``((max_digits, bytes), ...)``.

    The band edge is the **largest measured digit count** carrying that width,
    so every edge is a measurement rather than an interpolation. Widths must be
    non-decreasing in digits; anything else is not a step function and the
    caller is told so instead of being handed a table that hides it.
    """
    if not points:
        raise DerivationError("no binary width observations in the sealed oracle")
    bands: list[list[int]] = []
    last_width = 0
    for digits in sorted(points):
        width = points[digits]
        if width < last_width:
            raise DerivationError(
                f"measured widths are not monotonic in digit count: "
                f"{digits} digits -> {width} bytes follows a wider band "
                f"({last_width} bytes). A step function cannot be derived."
            )
        if bands and bands[-1][1] == width:
            bands[-1][0] = digits
        else:
            bands.append([digits, width])
        last_width = width
    return tuple((edge, width) for edge, width in bands)


@lru_cache(maxsize=1)
def derive_from_seal() -> Optional[Dict[str, object]]:
    """Derive GnuCOBOL's width rules from ``oracle.json``, or ``None`` if absent.

    ``None`` is the customer-perimeter case (R12): the engine ships without the
    benchmark, and the profile then rests on the engine's own measured
    constants. It is never a licence to guess -- the constants it falls back to
    are the same numbers this function derives, and CI runs with the oracle
    present precisely so the two are held together.
    """
    oracle = _load_oracle()
    if oracle is None:
        return None
    observed = _observations(oracle)
    binary_bands = _bands(observed["binary"])
    return {
        "binary_bands": binary_bands,
        "binary_points": dict(sorted(observed["binary"].items())),
        "packed_points": dict(sorted(observed["packed"].items())),
        "display_points": dict(sorted(observed["display"].items())),
    }


def assert_engine_agrees_with_seal() -> None:
    """Raise if :data:`layout.BINARY_WIDTHS` disagrees with the derived bands.

    This is acceptance (3)'s teeth. The engine keeps a constant so it can run
    without the benchmark; this function makes that constant a *cache of the
    seal* rather than an independent claim. A hand-edit that disagrees stops the
    build here.
    """
    derived = derive_from_seal()
    if derived is None:
        return
    from ..layout import BINARY_WIDTHS, packed_width, picture_size

    bands = derived["binary_bands"]
    if tuple(BINARY_WIDTHS) != tuple(bands):
        raise DerivationError(
            f"layout.BINARY_WIDTHS is {tuple(BINARY_WIDTHS)!r} but the sealed "
            f"oracle derives {tuple(bands)!r}. The seal wins: either the "
            f"hand-edit is wrong, or the benchmark needs a v0.2 re-seal, which "
            f"is an operator key ceremony (rule 4). Do not reconcile by "
            f"editing the oracle."
        )
    # The same check for the packed rule, which is a formula rather than a
    # table: every measured COMP-3 point must come back out of it.
    for digits, measured in derived["packed_points"].items():
        if packed_width(digits) != measured:
            raise DerivationError(
                f"the sealed oracle measured a {digits}-digit packed item at "
                f"{measured} bytes but layout.packed_width says "
                f"{packed_width(digits)}."
            )
    # And for display, where the rule is one byte per character position. The
    # left side is the count the PICTURE DECLARES; the right side is what the
    # oracle MEASURED. Those are two different derivations, which is what
    # makes the comparison capable of failing.
    for positions, measured in derived["display_points"].items():
        if positions != measured:
            raise DerivationError(
                f"the sealed oracle measured a display item declaring "
                f"{positions} character position(s) at {measured} bytes; the "
                f"display rule is one byte per character position, so it "
                f"should occupy {positions}."
            )


# --------------------------------------------------------------------------
# The profile
# --------------------------------------------------------------------------

_DERIVED = derive_from_seal()

if _DERIVED is not None:
    BINARY_BANDS: Tuple[Tuple[int, int], ...] = _DERIVED["binary_bands"]
    BINARY_MEASURED_DIGITS: Tuple[int, ...] = tuple(_DERIVED["binary_points"])
    # The compiler is named from this module's own constant rather than read
    # out of the oracle's `toolchain` field. Two reasons, and the second is the
    # load-bearing one:
    #
    #   * src/discovery/ must never invoke a compiler (D15), and
    #     tests/test_discovery_is_compiler_free.py enforces that partly by
    #     refusing compiler names in non-docstring literals. Reaching into a
    #     toolchain field to render provenance would trip a guard that exists
    #     for a good reason, and the right response to that guard is to stop
    #     reaching, not to loosen it.
    #   * it removes a coupling the profile did not need. The constant is
    #     pinned to the oracle's recorded toolchain by
    #     tests/test_dialect_gnucobol_derivation.py, which is where reading
    #     that field belongs.
    _BASIS = f"{SEAL_TAG} oracle.json ({COMPILER})"
else:  # pragma: no cover - customer perimeter, no benchmark on disk
    from ..layout import BINARY_WIDTHS as _ENGINE_BANDS

    BINARY_BANDS = tuple(_ENGINE_BANDS)
    BINARY_MEASURED_DIGITS = ()
    _BASIS = f"{COMPILER} (engine constants; {SEAL_TAG} not present on disk)"

#: Boundaries a SYNCHRONIZED item is aligned to. MEASURED from ``D10_sync``:
#: the boundary is the item's OWN WIDTH. This is the rule that makes D10
#: sensitivity *coupled* to member width rather than independent of it -- change
#: the width and the boundary changes with it.
SYNC_BOUNDARIES = frozenset({2, 4, 8})

assert_engine_agrees_with_seal()


def _binary(digits: int) -> Width:
    for edge, width in BINARY_BANDS:
        if digits <= edge:
            return Width(width, rule="binary_width")
    return Width(
        None,
        rule="binary_width",
        reason=(
            f"{digits} digit positions is outside the 1-{BINARY_BANDS[-1][0]} "
            f"range the sealed oracle measured (R7)"
        ),
    )


def _packed(digits: int) -> Width:
    if digits <= 0:
        return Width(None, rule="packed_width", reason="no digit positions")
    return Width(math.ceil((digits + 1) / 2), rule="packed_width")


def _display(positions: int) -> Width:
    if positions <= 0:
        return Width(None, rule="display_width", reason="no character positions")
    return Width(positions, rule="display_width")


def _sync_boundary(digits: int, width: Optional[int]) -> Optional[int]:
    """GnuCOBOL aligns a SYNC binary item to its own width."""
    if width in SYNC_BOUNDARIES:
        return width
    return None


_RULES = {
    "binary_width": Rule(
        name="binary_width",
        kind=RuleKind.MEASURED,
        summary=(
            "binary-size 1-2-4-8: "
            + ", ".join(
                f"<={edge} digits -> {width} byte(s)" for edge, width in BINARY_BANDS
            )
        ),
        derived_from=_BASIS,
    ),
    "packed_width": Rule(
        name="packed_width",
        kind=RuleKind.MEASURED,
        summary="packed-decimal occupies ceil((digits + 1) / 2) bytes",
        derived_from=_BASIS,
    ),
    "display_width": Rule(
        name="display_width",
        kind=RuleKind.MEASURED,
        summary="display occupies one byte per character position",
        derived_from=_BASIS,
    ),
    "sync_alignment": Rule(
        name="sync_alignment",
        kind=RuleKind.MEASURED,
        summary=(
            "SYNCHRONIZED aligns a binary item to a boundary equal to its own "
            "width (2, 4 or 8); SYNC on a non-binary item moves nothing"
        ),
        derived_from=f"{SEAL_TAG} corpus/D10_sync.cpy",
    ),
}

PROFILE = DialectProfile(
    id=PROFILE_ID,
    label=f"{COMPILER} (measured)",
    kind=ProfileKind.MEASURED,
    binary=_binary,
    packed=_packed,
    display=_display,
    sync_boundary=_sync_boundary,
    rules=_RULES,
    basis=_BASIS,
)

assert_every_rule_is_provenanced(PROFILE)
