"""WP-2.7 D39 — IBM Enterprise COBOL, one citation per rule.

Every rule in this module carries the manual, the section, and the statement
relied on. That is R9 applied to a rule rather than to a number: "we believe
IBM does X" is not a basis for an offset a customer will load data with.

**This profile is PROJECTED, not measured.** Relian has no IBM system and does
not pretend otherwise. Everything here is the layout *implied by* applying
these sourced rules to the same parse the measured engine reads -- never a
measurement, and it must never render in a column beside one without a label
(D36). The only thing that converts any of it to a measurement is a customer
running ``discovery-verify/IBMLAYOUT.cbl`` on their own iron and returning the
result, at which point it is a measurement *of their compiler at their
settings* and is recorded that way (D40).

**What is deliberately absent.** ``COMP-1``/``COMP-2`` (floating point),
``INDEX``, ``POINTER`` and the other usages in
:data:`src.discovery.layout.UNCOVERED_USAGES` have no rule here. The sealed
oracle does not measure them, so there is no GnuCOBOL side to compare against
and a sourced IBM rule alone would produce a difference against nothing. Those
constructs classify ``UNKNOWN``, which is the honest answer rather than a
half-answer (R2).

**The one substantive rule divergence.** IBM's storage table and its
*alignment* table are not the same function. A ``PIC S9(18) COMP`` occupies 8
bytes, but the slack-byte rule aligns binary items of "five-digit length or
more" on a **fullword** -- m=4, not m=8. GnuCOBOL aligns to the item's own
width, so m=8. The two coincide at many offsets and diverge at others, which is
exactly why :mod:`classify` treats a rule difference as sensitivity even where
this record's arithmetic happens to agree.
"""

from __future__ import annotations

import math
from typing import Optional, Tuple

from .base import (
    Citation,
    DialectProfile,
    ProfileKind,
    Rule,
    RuleKind,
    Width,
    assert_every_rule_is_provenanced,
)

PROFILE_ID = "ibm-enterprise-cobol"
COMPILER = "IBM Enterprise COBOL for z/OS 6.3"

_RETRIEVED = "2026-09-10"
_LR = "IBM Enterprise COBOL for z/OS Language Reference"
_PG = "IBM Enterprise COBOL for z/OS Programming Guide"
_LR_URL = "https://www.ibm.com/docs/en/cobol-zos/6.3.0?topic=clause-computational-items"
_SYNC_URL = (
    "https://www.ibm.com/docs/en/cobol-zos/6.3.0?topic=clause-slack-bytes-within-records"
)
_SIGN_URL = "https://www.ibm.com/docs/en/cobol-zos/6.3.0?topic=entry-sign-clause"
_PG_URL = (
    "https://www.ibm.com/docs/en/cobol-zos/6.3.0?topic=arithmetic-formats-numeric-data"
)

# --------------------------------------------------------------------------
# The citations. Quoted, not paraphrased: a reader has to be able to check
# whether the rule follows from the statement, and a paraphrase has already
# done that reasoning for them invisibly.
# --------------------------------------------------------------------------

_BINARY_CITATION = Citation(
    manual=_LR,
    version="6.3",
    section="USAGE clause > Computational items > BINARY",
    statement=(
        "The amount of storage occupied by a binary item depends on the number "
        "of decimal digits defined in its PICTURE clause: 1 through 4 digits, "
        "2 bytes (halfword); 5 through 9 digits, 4 bytes (fullword); 10 through "
        "18 digits, 8 bytes (doubleword)."
    ),
    url=_LR_URL,
    retrieved=_RETRIEVED,
)

_SYNONYM_CITATION = Citation(
    manual=_LR,
    version="6.3",
    section=(
        "USAGE clause > Computational items > COMPUTATIONAL or COMP (binary); "
        "COMPUTATIONAL-4 or COMP-4 (binary)"
    ),
    statement=(
        "The COMPUTATIONAL phrase is synonymous with BINARY. "
        "[COMPUTATIONAL-4 or COMP-4] This is the equivalent of BINARY."
    ),
    url=_LR_URL,
    retrieved=_RETRIEVED,
)

_COMP5_CITATION = Citation(
    manual=_LR,
    version="6.3",
    section="USAGE clause > Computational items > COMPUTATIONAL-5 or COMP-5 (native binary)",
    statement=(
        "S9(1) through S9(4): Binary halfword (2 bytes). "
        "S9(5) through S9(9): Binary fullword (4 bytes). "
        "S9(10) through S9(18): Binary doubleword (8 bytes). "
        "9(1) through 9(4): Binary halfword (2 bytes). "
        "9(5) through 9(9): Binary fullword (4 bytes). "
        "9(10) through 9(18): Binary doubleword (8 bytes)."
    ),
    url=_LR_URL,
    retrieved=_RETRIEVED,
)

_PACKED_CITATION = Citation(
    manual=_PG,
    version="6.3",
    section="Formats for numeric data > Packed-decimal (COMP-3) items",
    statement=(
        "Packed-decimal items occupy 1 byte of storage for every two decimal "
        "digits you code in the PICTURE description, except that the rightmost "
        "byte contains only one digit and the sign."
    ),
    url=_PG_URL,
    retrieved=_RETRIEVED,
)

_DISPLAY_CITATION = Citation(
    manual=_PG,
    version="6.3",
    section="Formats for numeric data > External decimal (DISPLAY and NATIONAL) items",
    statement=(
        "each position (byte) of storage contains one decimal digit. The items "
        "are stored in displayable form."
    ),
    url=_PG_URL,
    retrieved=_RETRIEVED,
)

_SIGN_CITATION = Citation(
    manual=_LR,
    version="6.3",
    section="SIGN clause",
    statement=(
        "The operational sign is presumed to be the LEADING or TRAILING "
        "character position, whichever is specified, of the elementary numeric "
        "data item. This character position is not a digit position. The "
        "character S in the PICTURE character string is counted in determining "
        "the size of the data item (in terms of standard data format characters)."
    ),
    url=_SIGN_URL,
    retrieved=_RETRIEVED,
)

_SYNC_CITATION = Citation(
    manual=_LR,
    version="6.3",
    section="SYNCHRONIZED clause > Slack bytes within records",
    statement=(
        "[Alignment is on a halfword boundary for] binary items of four-digit "
        "length or less; [on a fullword boundary for] binary items of "
        "five-digit length or more and for COMPUTATIONAL-1 data items; [on a "
        "doubleword boundary for] COMPUTATIONAL-2 data items. The total number "
        "of bytes occupied by all elementary data items that precede the binary "
        "item are added together, including any slack bytes that are previously "
        "added. This sum is divided by m. If the remainder (r) of this division "
        "is equal to zero, no slack bytes are required. If the remainder is not "
        "equal to zero, the number of slack bytes that must be added is equal "
        "to m - r."
    ),
    url=_SYNC_URL,
    retrieved=_RETRIEVED,
)

# --------------------------------------------------------------------------
# The rules
# --------------------------------------------------------------------------

#: IBM's digit -> byte bands, read off :data:`_BINARY_CITATION`. This is the
#: whole gap: GnuCOBOL gives 1-2 digits a SINGLE byte, IBM gives 1-4 digits a
#: halfword. Every other band is identical, which is why the sensitive set is
#: small and nameable rather than "the whole record might be wrong".
BINARY_BANDS: Tuple[Tuple[int, int], ...] = ((4, 2), (9, 4), (18, 8))

#: Alignment multiplier ``m``, read off :data:`_SYNC_CITATION`. Note that this
#: is keyed on DIGITS, not on the item's width -- an 18-digit COMP is 8 bytes
#: wide but fullword-aligned. GnuCOBOL keys it on width. Different function.
SYNC_M: Tuple[Tuple[int, int], ...] = ((4, 2), (18, 4))


def _binary(digits: int) -> Width:
    for edge, width in BINARY_BANDS:
        if digits <= edge:
            return Width(width, rule="binary_width")
    return Width(
        None,
        rule="binary_width",
        reason=(
            f"{digits} digit positions is outside the 1-18 range the cited "
            f"IBM table covers"
        ),
    )


def _packed(digits: int) -> Width:
    """One byte per two digits, rightmost byte one digit plus the sign."""
    if digits <= 0:
        return Width(None, rule="packed_width", reason="no digit positions")
    return Width(math.ceil((digits + 1) / 2), rule="packed_width")


def _display(positions: int) -> Width:
    if positions <= 0:
        return Width(None, rule="display_width", reason="no character positions")
    return Width(positions, rule="display_width")


def _sync_boundary(digits: int, width: Optional[int]) -> Optional[int]:
    """IBM's ``m``, by digit count rather than by width.

    ``width`` is accepted for signature parity with the GnuCOBOL profile and
    deliberately unused: keying on it would silently import GnuCOBOL's rule
    into this profile, which is the inference D39 forbids.
    """
    if digits <= 0:
        return None
    for edge, m in SYNC_M:
        if digits <= edge:
            return m
    return None


_RULES = {
    "binary_width": Rule(
        name="binary_width",
        kind=RuleKind.SOURCED,
        summary="<=4 digits -> 2 bytes; 5-9 -> 4 bytes; 10-18 -> 8 bytes",
        citation=_BINARY_CITATION,
    ),
    "binary_synonyms": Rule(
        name="binary_synonyms",
        kind=RuleKind.SOURCED,
        summary="COMP, COMPUTATIONAL, COMP-4 and BINARY are the same usage",
        citation=_SYNONYM_CITATION,
    ),
    "comp5_width": Rule(
        name="comp5_width",
        kind=RuleKind.SOURCED,
        summary=(
            "COMP-5 occupies 2/4/8 bytes on the same digit bands as BINARY; "
            "separately sourced rather than inferred from the BINARY table"
        ),
        citation=_COMP5_CITATION,
    ),
    "packed_width": Rule(
        name="packed_width",
        kind=RuleKind.SOURCED,
        summary="packed-decimal occupies ceil((digits + 1) / 2) bytes",
        citation=_PACKED_CITATION,
    ),
    "display_width": Rule(
        name="display_width",
        kind=RuleKind.SOURCED,
        summary="display occupies one byte per character position",
        citation=_DISPLAY_CITATION,
    ),
    "sign_separate": Rule(
        name="sign_separate",
        kind=RuleKind.SOURCED,
        summary=(
            "a SEPARATE sign occupies one character position and is counted in "
            "the size of the item"
        ),
        citation=_SIGN_CITATION,
    ),
    "sync_alignment": Rule(
        name="sync_alignment",
        kind=RuleKind.SOURCED,
        summary=(
            "SYNCHRONIZED binary alignment m = 2 for <=4 digits, 4 for >=5 "
            "digits; slack = m - (preceding bytes mod m)"
        ),
        citation=_SYNC_CITATION,
    ),
}

PROFILE = DialectProfile(
    id=PROFILE_ID,
    label=f"{COMPILER} (projected)",
    kind=ProfileKind.PROJECTED,
    binary=_binary,
    packed=_packed,
    display=_display,
    sync_boundary=_sync_boundary,
    rules=_RULES,
    basis=None,
)

assert_every_rule_is_provenanced(PROFILE)
