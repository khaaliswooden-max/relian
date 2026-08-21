"""WP-2.2 §3.2 — the static record-layout engine.

Given copybook text, compute every field's byte offset and length **without
invoking a compiler**. Offsets are 1-based, matching what the sealed oracle
measured (``WS-CW-ACCOUNT`` = 1, ``WS-CW-NAME`` = 11); converting at a boundary
is a place to be wrong, so there is no conversion.

Twelve axes, matching RELIAN-DISCOVERY-BENCH v0.1's corpus exactly:

===========================  ==================================================
DISPLAY numeric              signed/unsigned, ``V`` implied decimal, ``P``
                             scaling, ``S`` with and without SEPARATE
COMP / COMP-4 / BINARY       the measured 1-2-4-8 table (see BINARY_WIDTHS)
COMP-5                       native binary widths
COMP-3                       ``ceil((digits + 1) / 2)``
SIGN clause                  LEADING/TRAILING x SEPARATE/embedded
OCCURS                       fixed, nested, INDEXED BY
OCCURS DEPENDING ON          min and max extents, ``odo_object`` recorded
REDEFINES                    offset equals the redefined item's; containment
                             is asserted in engine output independently
level-88                     ``conditions``, no storage, offset/length null
SYNCHRONIZED                 alignment and the slack bytes it inserts
level-66 RENAMES             a span, emitted in the shape the seal committed to
edited                       numeric-edited, alphanumeric-edited, BLANK WHEN
                             ZERO, JUSTIFIED
===========================  ==================================================

**A length that cannot be computed is ``None``, never ``0`` (D19).** WP-2.1
measured why: ``MOVE HIGH-VALUES`` to a ``PIC ZZZZZZZZ9.99`` compiles, runs,
exits zero and marks zero bytes with no warning — a fabricated zero wearing the
costume of a measurement, and indistinguishable from a real zero-length answer
in any comparison that checks types rather than provenance. The same trap has a
new surface here, so :func:`lint_layout` fails on any ``0`` that was not
computed, and an uncomputable field carries an ``unmeasured_reason``.

**The engine classifies gaps more finely than the oracle can (D18).** The
oracle observes only "bytes no named field claims" and labels them ``gap``.
The engine reads the source, so it distinguishes ``filler`` (an explicit
``FILLER`` entry) from ``slack`` (bytes ``SYNCHRONIZED`` inserted). The
round-trip projects engine to oracle granularity rather than comparing labels;
"bytes 2, 10 and 16 are SYNC slack, not data" is a different sentence from
"there is a gap", and only one of them helps someone writing a target schema.
"""

from __future__ import annotations

import hashlib
import math
import re
from dataclasses import dataclass, field as dc_field
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

from src.assessment.models import Grade, Measured
from .copybook import CodeLine, CONTINUATION_INDICATOR, Resolution, assemble, code_lines

# --------------------------------------------------------------------------
# Usage vocabulary
# --------------------------------------------------------------------------

DISPLAY = "DISPLAY"

BINARY_USAGES = frozenset({
    "COMP", "COMPUTATIONAL", "COMP-4", "COMPUTATIONAL-4", "BINARY",
    "COMP-5", "COMPUTATIONAL-5", "BINARY-CHAR", "BINARY-SHORT",
    "BINARY-LONG", "BINARY-DOUBLE",
})

PACKED_USAGES = frozenset({"COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL"})

#: Usages the sealed corpus does NOT exercise. R7: a construct is not
#: "supported" until RELIAN-BENCH covers it, so these return ``None`` with a
#: named reason rather than a plausible-looking width. Adding one here without
#: a bench vector that measures it is exactly the defect this project keeps
#: catching.
UNCOVERED_USAGES = frozenset({
    "COMP-1", "COMPUTATIONAL-1", "COMP-2", "COMPUTATIONAL-2",
    "COMP-6", "COMPUTATIONAL-6", "COMP-X", "COMPUTATIONAL-X",
    "INDEX", "POINTER", "PROCEDURE-POINTER", "FUNCTION-POINTER",
    "NATIONAL", "OBJECT", "OBJECT-REFERENCE",
})

ALL_USAGES = BINARY_USAGES | PACKED_USAGES | UNCOVERED_USAGES | {DISPLAY, "DISPLAY-1"}

#: GnuCOBOL 3.1.2's ``binary-size: 1-2-4-8``, as MEASURED by the oracle rather
#: than as read from a manual: ``D02-COMP-1 PIC S9(01) COMP`` occupies 1 byte
#: and ``D02-COMP-4 PIC S9(04) COMP`` occupies 2. The WP-2.2 brief's table said
#: "1-4 digits -> 2 bytes"; the sealed measurement says otherwise and the
#: measurement wins.
BINARY_WIDTHS: Tuple[Tuple[int, int], ...] = ((2, 1), (4, 2), (9, 4), (18, 8))

#: Alignment boundary a SYNCHRONIZED item is moved to. Measured from
#: ``D10_sync.cpy``: a 4-byte COMP after 1 character byte lands at offset 5
#: (0-based 4), a 2-byte COMP after 9 lands at 11 (0-based 10), an 8-byte COMP
#: after 15 lands at 17 (0-based 16). The boundary is the item's own width.
SYNC_BOUNDARIES = frozenset({2, 4, 8})


#: The compiler whose byte layout this engine is verified against, and the
#: benchmark that verifies it. Both appear in the PROVENANCE of every number
#: the engine publishes -- not only in the docstrings and the log.
COMPILER_BASIS = "GnuCOBOL 3.1.2.0"
VERIFIED_AGAINST = "RELIAN-DISCOVERY-BENCH v0.1 (relian-discovery-bench-v0.1)"

#: R11/R9. The one sentence a customer must be able to read off the artifact
#: itself, in the artifact itself. Everything this engine is verified against is
#: GnuCOBOL's byte layout; the compiler the customer actually runs is IBM
#: Enterprise COBOL, and that equivalence is UNMEASURED. Keeping this only in
#: the commit log and the docstrings would mean the report says "PLAUSIBLE"
#: without ever saying plausible-with-respect-to-what, which is a grade with the
#: units filed off.
IBM_EQUIVALENCE_LIMITATION = (
    f"Verified byte-for-byte against {COMPILER_BASIS} on {VERIFIED_AGAINST}, "
    f"186 of 186 comparisons at tolerance zero. Equivalence with IBM Enterprise "
    f"COBOL is UNMEASURED: SYNCHRONIZED slack in particular moves with the "
    f"compiler's binary-size setting and its SYNCHRONIZED handling, so an IBM "
    f"layout may differ from the one below. Grade PLAUSIBLE, not VERIFIED, for "
    f"that reason."
)

#: Why a statically computed offset is PLAUSIBLE and not VERIFIED. VERIFIED
#: would claim the number holds on the customer's compiler, which is precisely
#: what has not been measured.
LAYOUT_GRADE: Grade = "PLAUSIBLE"


class LayoutStatus(str, Enum):
    """What the engine is willing to claim about a record."""

    COMPLETE = "COMPLETE"    # every field placed, every length computed
    PARTIAL = "PARTIAL"      # some construct named and not computed (R2)
    NONE = "NONE"            # no layout at all -- e.g. an unresolved COPY


# --------------------------------------------------------------------------
# Output records
# --------------------------------------------------------------------------

@dataclass(frozen=True)
class Field:
    """One field row. ``offset``/``length`` are 1-based bytes or ``None``."""

    name: str
    key: str
    level: int
    offset: Optional[int]
    length: Optional[int]
    elementary: bool
    in_tiling: bool
    redefines: Optional[str] = None
    renames: bool = False
    subscripts: Tuple[int, ...] = ()
    picture: Optional[str] = None
    usage: str = DISPLAY
    sign: Optional[str] = None
    sign_separate: bool = False
    occurs: Optional[int] = None
    occurs_min: Optional[int] = None
    odo_object: Optional[str] = None
    sync: bool = False
    justified: bool = False
    blank_when_zero: bool = False
    category: str = "UNKNOWN"
    source: str = "computed"
    unmeasured_reason: Optional[str] = None

    @property
    def end(self) -> Optional[int]:
        if self.offset is None or self.length is None:
            return None
        return self.offset + self.length - 1

    def to_dict(self) -> Dict[str, object]:
        return {
            "name": self.name,
            "key": self.key,
            "level": self.level,
            "offset": self.offset,
            "length": self.length,
            "elementary": self.elementary,
            "in_tiling": self.in_tiling,
            "redefines": self.redefines,
            "renames": self.renames,
            "subscripts": list(self.subscripts),
            "picture": self.picture,
            "usage": self.usage,
            "sign": self.sign,
            "sign_separate": self.sign_separate,
            "occurs": self.occurs,
            "occurs_min": self.occurs_min,
            "odo_object": self.odo_object,
            "sync": self.sync,
            "justified": self.justified,
            "blank_when_zero": self.blank_when_zero,
            "category": self.category,
            "source": self.source,
            "unmeasured_reason": self.unmeasured_reason,
        }


@dataclass(frozen=True)
class Gap:
    """Bytes that belong to no named field. ``source`` is finer than the
    oracle's ``gap``: ``filler`` is an explicit FILLER entry, ``slack`` is
    inserted by SYNCHRONIZED alignment (D18)."""

    offset: int
    length: int
    source: str                       # "filler" | "slack"
    cause: str = ""

    def span(self) -> Tuple[int, int]:
        return (self.offset, self.offset + self.length - 1)

    def to_dict(self) -> Dict[str, object]:
        return {
            "offset": self.offset,
            "length": self.length,
            "source": self.source,
            "cause": self.cause,
        }


@dataclass(frozen=True)
class Condition:
    """A level-88. No storage at all, so offset and length are ``None`` —
    never ``0``, because zero is not an honest answer for a thing with no
    extent (R1)."""

    name: str
    host: str
    level: int = 88
    offset: None = None
    length: None = None
    values_raw: str = ""
    source: str = "computed"

    def to_dict(self) -> Dict[str, object]:
        return {
            "name": self.name,
            "host": self.host,
            "level": self.level,
            "offset": None,
            "length": None,
            "values_raw": self.values_raw,
            "source": self.source,
        }


@dataclass(frozen=True)
class Layout:
    """One record's computed layout, at one ODO value."""

    group: str
    group_length: Optional[int]
    status: LayoutStatus
    fields: Tuple[Field, ...] = ()
    gaps: Tuple[Gap, ...] = ()
    conditions: Tuple[Condition, ...] = ()
    reasons: Tuple[str, ...] = ()
    odo_object: Optional[str] = None
    odo_value: Optional[int] = None
    variable_length: bool = False
    origin: str = "<text>"
    source_sha256: Optional[str] = None

    # -- derived views ------------------------------------------------------

    def gap_spans(self) -> Tuple[Tuple[int, int], ...]:
        """``filler`` union ``slack``, as byte ranges. This is the projection
        the round-trip compares against the oracle's ``gap`` rows (D18)."""
        return tuple(sorted(g.span() for g in self.gaps))

    def by_key(self) -> Dict[str, Field]:
        return {f.key: f for f in self.fields}

    def tiling_fields(self) -> Tuple[Field, ...]:
        return tuple(f for f in self.fields if f.in_tiling)

    def summary(self) -> Dict[str, Optional[Measured]]:
        """Report-facing numbers, each carrying provenance and a grade (R9).

        ``None`` where nothing was computed. A statically computed layout is
        graded PLAUSIBLE rather than VERIFIED: it is verified against
        :data:`COMPILER_BASIS` by the round-trip, and unmeasured against IBM
        Enterprise COBOL, which is the compiler the customer actually runs
        (R11).

        That reason travels **inside the provenance string**, so it reaches
        every consumer of a number rather than only a reader of this docstring.
        A grade with no stated basis is a grade with the units filed off.
        """
        provenance = (
            f"src.discovery.layout static computation over {self.origin}; "
            f"{IBM_EQUIVALENCE_LIMITATION}"
        )
        grade: Grade = LAYOUT_GRADE
        declared = sum(
            f.length for f in self.fields if f.in_tiling and f.length is not None
        )
        incomplete = any(f.in_tiling and f.length is None for f in self.fields)
        return {
            "group_length": (
                Measured(self.group_length, provenance, grade)
                if self.group_length is not None else None
            ),
            "declared_field_bytes": (
                Measured(declared, provenance, grade) if not incomplete else None
            ),
            "gap_bytes": Measured(
                sum(g.length for g in self.gaps), provenance, grade
            ),
            "field_rows": Measured(len(self.fields), provenance, grade),
            "condition_rows": Measured(len(self.conditions), provenance, grade),
        }

    def limitations(self) -> Tuple[str, ...]:
        """Everything a reader must know before acting on the numbers above.

        First entry is always the IBM-equivalence limitation (R11): it applies
        to every layout this engine produces, green or not. A ``PARTIAL`` or
        ``NONE`` status appends its own reasons, so a caller that renders this
        tuple renders the whole caveat set rather than half of it.
        """
        return (IBM_EQUIVALENCE_LIMITATION,) + self.reasons

    def to_dict(self) -> Dict[str, object]:
        return {
            "group": self.group,
            "group_length": self.group_length,
            "status": self.status.value,
            "grade": LAYOUT_GRADE,
            "verified_against": COMPILER_BASIS,
            "benchmark": VERIFIED_AGAINST,
            "limitations": list(self.limitations()),
            "summary": {
                key: (measured.to_dict() if measured is not None else None)
                for key, measured in self.summary().items()
            },
            "fields": [f.to_dict() for f in self.fields],
            "gaps": [g.to_dict() for g in self.gaps],
            "conditions": [c.to_dict() for c in self.conditions],
            "reasons": list(self.reasons),
            "odo_object": self.odo_object,
            "odo_value": self.odo_value,
            "variable_length": self.variable_length,
            "origin": self.origin,
            "source_sha256": self.source_sha256,
        }


# --------------------------------------------------------------------------
# PICTURE
# --------------------------------------------------------------------------

_PIC_TOKEN_RE = re.compile(r"(CR|DB)|([A-Za-z9*/,.+\-$0])(?:\((\d+)\))?", re.IGNORECASE)

#: Character positions each picture symbol occupies in storage. ``S`` is zero
#: unless SIGN SEPARATE is in force (the caller adds that byte); ``V`` is an
#: assumed decimal point and ``P`` a scaling position, and neither occupies a
#: byte -- which is why ``PIC S9(04)PPP`` and ``PIC SPPP9(04)`` are both 4
#: bytes, as the oracle measured.
_ZERO_WIDTH = frozenset({"S", "V", "P"})

_KNOWN_PIC_SYMBOLS = frozenset({
    "9", "X", "A", "Z", "*", ",", ".", "/", "B", "0", "+", "-", "$", "E",
    "S", "V", "P", "CR", "DB",
})


def expand_picture(picture: str) -> Optional[List[str]]:
    """Expand ``S9(05)V99`` to ``['S','9','9','9','9','9','V','9','9']``.

    ``CR`` and ``DB`` stay single entries because they occupy two character
    positions each, which :func:`picture_size` accounts for. Returns ``None``
    if the string contains a symbol this engine does not model — an
    unrecognised picture yields ``None``, never a guess.
    """
    if not picture:
        return None
    symbols: List[str] = []
    pos = 0
    text = picture.strip()
    while pos < len(text):
        m = _PIC_TOKEN_RE.match(text, pos)
        if not m or m.start() != pos:
            return None
        if m.group(1):
            symbols.append(m.group(1).upper())
        else:
            sym = m.group(2).upper()
            if sym not in _KNOWN_PIC_SYMBOLS:
                return None
            count = int(m.group(3)) if m.group(3) else 1
            if count <= 0:
                return None
            symbols.extend([sym] * count)
        pos = m.end()
    return symbols or None


def picture_size(symbols: Sequence[str]) -> int:
    """Character positions occupied by an expanded picture, DISPLAY usage."""
    total = 0
    for sym in symbols:
        if sym in _ZERO_WIDTH:
            continue
        total += 2 if sym in ("CR", "DB") else 1
    return total


def picture_digits(symbols: Sequence[str]) -> int:
    """Digit positions: ``9`` and ``P``. ``P`` is a scaling position that
    counts toward the item's digit count while occupying no byte."""
    return sum(1 for sym in symbols if sym in ("9", "P"))


def picture_category(symbols: Sequence[str]) -> str:
    upper = set(symbols)
    editing = upper & {"Z", "*", ",", ".", "/", "B", "0", "+", "-", "$", "CR", "DB"}
    if "9" in upper or "P" in upper:
        return "NUMERIC-EDITED" if editing else "NUMERIC"
    if "X" in upper:
        return "ALPHANUMERIC-EDITED" if (upper & {"B", "0", "/"}) else "ALPHANUMERIC"
    if "A" in upper:
        return "ALPHANUMERIC-EDITED" if (upper & {"B", "0", "/"}) else "ALPHABETIC"
    return "UNKNOWN"


def binary_width(digits: int) -> Optional[int]:
    for max_digits, width in BINARY_WIDTHS:
        if digits <= max_digits:
            return width
    return None


def packed_width(digits: int) -> Optional[int]:
    if digits <= 0:
        return None
    return math.ceil((digits + 1) / 2)


# --------------------------------------------------------------------------
# Parsing data description entries
# --------------------------------------------------------------------------

@dataclass
class _Entry:
    level: int
    name: str
    lineno: int
    is_filler: bool = False
    picture: Optional[str] = None
    usage: Optional[str] = None
    sign: Optional[str] = None
    sign_separate: bool = False
    occurs: Optional[int] = None
    occurs_min: Optional[int] = None
    odo_object: Optional[str] = None
    indexed_by: Tuple[str, ...] = ()
    redefines: Optional[str] = None
    renames_from: Optional[str] = None
    renames_thru: Optional[str] = None
    sync: bool = False
    justified: bool = False
    blank_when_zero: bool = False
    values_raw: str = ""
    unparsed: Tuple[str, ...] = ()
    children: List["_Entry"] = dc_field(default_factory=list)
    conditions: List["_Entry"] = dc_field(default_factory=list)

    @property
    def elementary(self) -> bool:
        return not self.children


_LEVEL_RE = re.compile(r"\A(\d{1,2})(?![\w-])")
_INT_RE = re.compile(r"\A\d+\Z")


def _join_statements(lines: Sequence[CodeLine]) -> List[Tuple[int, str]]:
    """Join program-text lines and split on separator periods.

    A separator period is ``.`` followed by whitespace or end-of-text and not
    inside a literal, so ``PIC ZZZ,ZZ9.99`` and ``VALUE 0.0625`` do not end a
    statement. A column-7 ``-`` continuation is appended with no intervening
    space, which is the standard's rule for continuing a word or literal.
    """
    if not lines:
        return []
    buffer: List[Tuple[int, str]] = []
    for line in lines:
        if line.indicator == CONTINUATION_INDICATOR and buffer:
            lineno, text = buffer[-1]
            buffer[-1] = (lineno, text + line.text.strip())
        else:
            buffer.append((line.lineno, line.text))

    joined = ""
    positions: List[int] = []
    for lineno, text in buffer:
        chunk = text.strip() + " "
        joined += chunk
        positions.extend([lineno] * len(chunk))

    statements: List[Tuple[int, str]] = []
    start = 0
    quote: Optional[str] = None
    i = 0
    while i < len(joined):
        ch = joined[i]
        if quote is not None:
            if ch == quote:
                quote = None
            i += 1
            continue
        if ch in "\"'":
            quote = ch
            i += 1
            continue
        if ch == "." and (i + 1 >= len(joined) or joined[i + 1].isspace()):
            text = joined[start:i].strip()
            if text:
                statements.append((positions[start], text))
            i += 1
            start = i
            continue
        i += 1
    tail = joined[start:].strip()
    if tail:
        statements.append((positions[min(start, len(positions) - 1)], tail))
    return statements


def _tokenize(statement: str) -> List[str]:
    """Whitespace tokens, with quoted literals kept whole."""
    tokens: List[str] = []
    i = 0
    n = len(statement)
    while i < n:
        ch = statement[i]
        if ch.isspace():
            i += 1
            continue
        if ch in "\"'":
            j = i + 1
            while j < n and statement[j] != ch:
                j += 1
            tokens.append(statement[i:min(j + 1, n)])
            i = j + 1
            continue
        j = i
        while j < n and not statement[j].isspace():
            j += 1
        tokens.append(statement[i:j])
        i = j
    return tokens


def _parse_entry(lineno: int, statement: str) -> Optional[_Entry]:
    """Parse one data description entry. ``None`` if it is not one."""
    tokens = _tokenize(statement)
    if not tokens:
        return None
    m = _LEVEL_RE.match(tokens[0])
    if not m or len(tokens[0]) != len(m.group(1)):
        return None
    level = int(m.group(1))
    if not (1 <= level <= 49 or level in (66, 77, 78, 88)):
        return None

    idx = 1
    name = "FILLER"
    is_filler = True
    if idx < len(tokens) and not _is_keyword(tokens[idx]):
        name = tokens[idx].upper()
        is_filler = name == "FILLER"
        idx += 1
    entry = _Entry(level=level, name=name, lineno=lineno, is_filler=is_filler)

    unparsed: List[str] = []
    while idx < len(tokens):
        word = tokens[idx].upper()
        if word in ("PIC", "PICTURE"):
            idx += 1
            if idx < len(tokens) and tokens[idx].upper() == "IS":
                idx += 1
            if idx < len(tokens):
                entry.picture = tokens[idx].upper()
                idx += 1
            continue
        if word == "USAGE":
            idx += 1
            if idx < len(tokens) and tokens[idx].upper() == "IS":
                idx += 1
            continue
        if word in ALL_USAGES:
            entry.usage = word
            idx += 1
            continue
        if word == "REDEFINES":
            idx += 1
            if idx < len(tokens):
                entry.redefines = tokens[idx].upper()
                idx += 1
            continue
        if word == "RENAMES":
            idx += 1
            if idx < len(tokens):
                entry.renames_from = tokens[idx].upper()
                idx += 1
            if idx < len(tokens) and tokens[idx].upper() in ("THRU", "THROUGH"):
                idx += 1
                if idx < len(tokens):
                    entry.renames_thru = tokens[idx].upper()
                    idx += 1
            continue
        if word == "OCCURS":
            idx = _parse_occurs(entry, tokens, idx + 1)
            continue
        if word == "SIGN":
            idx += 1
            if idx < len(tokens) and tokens[idx].upper() == "IS":
                idx += 1
            if idx < len(tokens) and tokens[idx].upper() in ("LEADING", "TRAILING"):
                entry.sign = tokens[idx].upper()
                idx += 1
            if idx < len(tokens) and tokens[idx].upper() == "SEPARATE":
                entry.sign_separate = True
                idx += 1
                if idx < len(tokens) and tokens[idx].upper() == "CHARACTER":
                    idx += 1
            continue
        if word in ("LEADING", "TRAILING") and entry.sign is None:
            # `SIGN` is an optional word: `PIC S9(4) TRAILING SEPARATE`.
            entry.sign = word
            idx += 1
            if idx < len(tokens) and tokens[idx].upper() == "SEPARATE":
                entry.sign_separate = True
                idx += 1
                if idx < len(tokens) and tokens[idx].upper() == "CHARACTER":
                    idx += 1
            continue
        if word in ("SYNCHRONIZED", "SYNC"):
            entry.sync = True
            idx += 1
            if idx < len(tokens) and tokens[idx].upper() in ("LEFT", "RIGHT"):
                idx += 1
            continue
        if word in ("JUSTIFIED", "JUST"):
            entry.justified = True
            idx += 1
            if idx < len(tokens) and tokens[idx].upper() == "RIGHT":
                idx += 1
            continue
        if word == "BLANK":
            idx += 1
            if idx < len(tokens) and tokens[idx].upper() == "WHEN":
                idx += 1
            if idx < len(tokens) and tokens[idx].upper() == "ZERO":
                entry.blank_when_zero = True
                idx += 1
            continue
        if word in ("VALUE", "VALUES"):
            idx += 1
            if idx < len(tokens) and tokens[idx].upper() in ("IS", "ARE"):
                idx += 1
            entry.values_raw = " ".join(tokens[idx:])
            idx = len(tokens)
            continue
        if word in ("EXTERNAL", "GLOBAL", "IS", "ARE"):
            idx += 1
            continue
        unparsed.append(tokens[idx])
        idx += 1

    entry.unparsed = tuple(unparsed)
    return entry


_KEYWORDS = frozenset({
    "PIC", "PICTURE", "USAGE", "REDEFINES", "RENAMES", "OCCURS", "SIGN",
    "SYNCHRONIZED", "SYNC", "JUSTIFIED", "JUST", "BLANK", "VALUE", "VALUES",
    "EXTERNAL", "GLOBAL", "IS", "ARE", "FILLER",
}) | ALL_USAGES


def _is_keyword(token: str) -> bool:
    word = token.upper()
    return word in _KEYWORDS and word != "FILLER"


def _parse_occurs(entry: _Entry, tokens: Sequence[str], idx: int) -> int:
    n = len(tokens)
    if idx >= n or not _INT_RE.match(tokens[idx]):
        return idx
    first = int(tokens[idx])
    idx += 1
    if idx < n and tokens[idx].upper() == "TO" and idx + 1 < n and _INT_RE.match(tokens[idx + 1]):
        entry.occurs_min = first
        entry.occurs = int(tokens[idx + 1])
        idx += 2
    else:
        entry.occurs = first
    if idx < n and tokens[idx].upper() == "TIMES":
        idx += 1
    if idx < n and tokens[idx].upper() == "DEPENDING":
        idx += 1
        if idx < n and tokens[idx].upper() == "ON":
            idx += 1
        if idx < n:
            entry.odo_object = tokens[idx].upper()
            idx += 1
    while idx < n and tokens[idx].upper() in ("ASCENDING", "DESCENDING"):
        idx += 1
        if idx < n and tokens[idx].upper() == "KEY":
            idx += 1
        if idx < n and tokens[idx].upper() == "IS":
            idx += 1
        while idx < n and not _is_clause_start(tokens[idx]):
            idx += 1
    if idx < n and tokens[idx].upper() == "INDEXED":
        idx += 1
        if idx < n and tokens[idx].upper() == "BY":
            idx += 1
        names: List[str] = []
        while idx < n and not _is_clause_start(tokens[idx]):
            names.append(tokens[idx].upper())
            idx += 1
        entry.indexed_by = tuple(names)
    return idx


def _is_clause_start(token: str) -> bool:
    return token.upper() in _KEYWORDS or token.upper() in ("INDEXED", "ASCENDING", "DESCENDING")


def parse_entries(text: str, *, fixed_format: bool = True) -> List[_Entry]:
    """Parse copybook text into a forest of level-01/77 records."""
    statements = _join_statements(code_lines(text, fixed_format=fixed_format))
    roots: List[_Entry] = []
    stack: List[_Entry] = []
    last_data: Optional[_Entry] = None
    for lineno, statement in statements:
        entry = _parse_entry(lineno, statement)
        if entry is None:
            continue
        if entry.level == 88:
            if last_data is not None:
                last_data.conditions.append(entry)
            continue
        if entry.level == 78:
            continue                      # a constant, not storage
        if entry.level in (1, 77, 66):
            if entry.level == 66 and roots:
                roots[-1].children.append(entry)
                stack = [roots[-1]]
            else:
                roots.append(entry)
                stack = [entry]
            last_data = entry
            continue
        while len(stack) > 1 and stack[-1].level >= entry.level:
            stack.pop()
        if not stack:
            roots.append(entry)
            stack = [entry]
        else:
            stack[-1].children.append(entry)
            stack.append(entry)
        last_data = entry
    return roots


# --------------------------------------------------------------------------
# Placement
# --------------------------------------------------------------------------

@dataclass
class _Row:
    name: str
    level: int
    offset: Optional[int]
    length: Optional[int]
    elementary: bool
    in_tiling: bool
    entry: _Entry
    subscripts: Tuple[int, ...]
    redefines: Optional[str] = None
    renames: bool = False
    unmeasured_reason: Optional[str] = None


class _Context:
    def __init__(self, odo_value: Optional[int]) -> None:
        self.odo_value = odo_value
        self.rows: List[_Row] = []
        self.gaps: List[Gap] = []
        self.conditions: List[Condition] = []
        self.reasons: List[str] = []
        self.offsets: Dict[str, Tuple[Optional[int], Optional[int]]] = {}

    def note(self, reason: str) -> None:
        if reason not in self.reasons:
            self.reasons.append(reason)


def _effective_usage(entry: _Entry, inherited: Optional[str]) -> str:
    if entry.usage:
        return entry.usage
    if inherited:
        return inherited
    return DISPLAY


def _elementary_size(entry: _Entry, usage: str, ctx: _Context) -> Tuple[Optional[int], Optional[str]]:
    """Bytes one occurrence of an elementary item occupies, or (None, reason)."""
    if usage in UNCOVERED_USAGES:
        return None, (
            f"usage {usage} is not covered by RELIAN-DISCOVERY-BENCH v0.1; "
            f"no width is claimed for an unmeasured usage (R7)"
        )
    if entry.picture is None:
        if usage in BINARY_USAGES or usage in PACKED_USAGES:
            return None, f"{usage} item {entry.name} has no PICTURE"
        return None, f"elementary item {entry.name} has no PICTURE clause"

    symbols = expand_picture(entry.picture)
    if symbols is None:
        return None, f"PICTURE {entry.picture!r} contains a symbol this engine does not model"

    if usage in BINARY_USAGES:
        digits = picture_digits(symbols)
        width = binary_width(digits)
        if width is None:
            return None, (
                f"{usage} with {digits} digit positions is outside the 1-18 range "
                f"the oracle measured"
            )
        return width, None
    if usage in PACKED_USAGES:
        digits = picture_digits(symbols)
        width = packed_width(digits)
        if width is None:
            return None, f"{usage} item {entry.name} has no digit positions"
        return width, None

    size = picture_size(symbols)
    if entry.sign_separate:
        size += 1
    if size <= 0:
        return None, (
            f"PICTURE {entry.picture!r} yields no character positions; a zero "
            f"length here would be a fabricated measurement (D19)"
        )
    return size, None


def _occurrence_count(entry: _Entry, ctx: _Context) -> int:
    if entry.occurs is None:
        return 1
    if entry.odo_object is not None:
        if ctx.odo_value is None:
            return entry.occurs
        low = entry.occurs_min if entry.occurs_min is not None else 1
        return max(low, min(entry.occurs, ctx.odo_value))
    return entry.occurs


def _key(name: str, subscripts: Tuple[int, ...]) -> str:
    if not subscripts:
        return name
    return f"{name} ({', '.join(str(s) for s in subscripts)})"


def _place_one(
    entry: _Entry,
    start: int,
    subscripts: Tuple[int, ...],
    usage_in: Optional[str],
    ctx: _Context,
    *,
    under_alias: bool,
) -> Tuple[Optional[int], Optional[str]]:
    """Place ONE occurrence of ``entry`` at ``start``. Returns (size, reason)."""
    usage = _effective_usage(entry, usage_in)

    # A level-88 is emitted ONCE per host, on the first occurrence. Guarding on
    # "no subscripts at all" instead would silently drop every condition
    # declared on a table item -- a whole construct vanishing without anything
    # saying so, which is the failure mode R2 exists for.
    if all(index == 1 for index in subscripts):
        for cond in entry.conditions:
            ctx.conditions.append(
                Condition(name=cond.name, host=entry.name, values_raw=cond.values_raw)
            )

    if entry.elementary:
        size, reason = _elementary_size(entry, usage, ctx)
        if entry.is_filler:
            if size is not None:
                ctx.gaps.append(
                    Gap(offset=start, length=size, source="filler",
                        cause=f"explicit FILLER PIC {entry.picture}")
                )
            return size, reason
        ctx.rows.append(
            _Row(name=entry.name, level=entry.level, offset=start, length=size,
                 elementary=True, in_tiling=not under_alias, entry=entry,
                 subscripts=subscripts, unmeasured_reason=reason)
        )
        return size, reason

    row = _Row(name=entry.name, level=entry.level, offset=start, length=None,
               elementary=False, in_tiling=False, entry=entry, subscripts=subscripts)
    emit_row = not entry.is_filler
    if emit_row:
        ctx.rows.append(row)

    cursor = start
    local: Dict[str, Tuple[Optional[int], Optional[int]]] = {}
    failure: Optional[str] = None
    for child in entry.children:
        if child.level == 66:
            continue
        if child.redefines:
            target = local.get(child.redefines)
            if target is None or target[0] is None:
                ctx.note(
                    f"{child.name} REDEFINES {child.redefines}, which was not "
                    f"placed before it; no offset is claimed for {child.name}"
                )
                ctx.rows.append(
                    _Row(name=child.name, level=child.level, offset=None, length=None,
                         elementary=child.elementary, in_tiling=False, entry=child,
                         subscripts=subscripts, redefines=child.redefines,
                         unmeasured_reason="REDEFINES target not resolved")
                )
                continue
            child_start = target[0]
            size, reason = _place_occurrences(
                child, child_start, subscripts, usage, ctx,
                under_alias=True, redefines=child.redefines
            )
            if size is None and reason:
                ctx.note(reason)
            continue

        if child.sync:
            aligned, slack = _sync_align(child, cursor, usage, ctx)
            if slack:
                ctx.gaps.append(slack)
            cursor = aligned

        child_start = cursor
        total, reason = _place_occurrences(
            child, child_start, subscripts, usage, ctx, under_alias=under_alias
        )
        local[child.name] = (child_start, total)
        if total is None:
            failure = reason or f"length of {child.name} could not be computed"
            ctx.note(failure)
            break
        cursor += total

    if failure is not None:
        # The group inherits its child's failure as its OWN stated reason. A
        # length of None with nothing to say for itself is the same defect as
        # a fabricated zero, one level up: the lint rejects both (R1/R9).
        row.unmeasured_reason = failure
        return None, failure

    size = cursor - start
    if emit_row:
        row.length = size
    if size <= 0:
        return None, f"group {entry.name} computed a non-positive length"
    return size, None


def _place_occurrences(
    entry: _Entry,
    start: int,
    parent_subscripts: Tuple[int, ...],
    usage_in: Optional[str],
    ctx: _Context,
    *,
    under_alias: bool,
    redefines: Optional[str] = None,
) -> Tuple[Optional[int], Optional[str]]:
    """Place every occurrence of ``entry``. Returns (total size, reason)."""
    count = _occurrence_count(entry, ctx)
    if entry.occurs is not None and _has_sync(entry):
        ctx.note(
            f"{entry.name} carries OCCURS over a SYNCHRONIZED item; GnuCOBOL's "
            f"slack placement inside a table is not measured by "
            f"RELIAN-DISCOVERY-BENCH v0.1, so this layout is PARTIAL (R7)"
        )
    element: Optional[int] = None
    first_index = len(ctx.rows)
    for i in range(1, count + 1):
        subs = parent_subscripts + ((i,) if entry.occurs is not None else ())
        offset = start if element is None else start + (i - 1) * element
        size, reason = _place_one(
            entry, offset, subs, usage_in, ctx, under_alias=under_alias
        )
        if size is None:
            return None, reason
        if element is None:
            element = size
        elif size != element:
            ctx.note(f"{entry.name} occurrences are not uniform in size")
            return None, f"{entry.name} occurrences are not uniform in size"
    if redefines is not None:
        for row in ctx.rows[first_index:]:
            if row.name == entry.name and row.level == entry.level:
                row.redefines = redefines
    if element is None:
        return None, f"{entry.name} has no occurrences"
    return element * count, None


def _has_sync(entry: _Entry) -> bool:
    if entry.sync:
        return True
    return any(_has_sync(child) for child in entry.children)


def _sync_align(
    entry: _Entry, cursor: int, usage_in: Optional[str], ctx: _Context
) -> Tuple[int, Optional[Gap]]:
    """Move ``cursor`` to the boundary a SYNCHRONIZED item requires.

    MEASURED, not assumed: GnuCOBOL 3.1.2 aligns a SYNC binary item to its own
    width relative to the start of the record. ``D10_sync.cpy`` declares 20
    bytes and occupies 25, with slack at offsets 2, 10 and 16. SYNC on a
    non-binary item has no effect in this compiler, and the corpus does not
    measure one, so nothing is moved and nothing is claimed.
    """
    usage = _effective_usage(entry, usage_in)
    if usage not in BINARY_USAGES:
        if usage not in UNCOVERED_USAGES:
            ctx.note(
                f"SYNCHRONIZED on {entry.name} (usage {usage}) is not a binary "
                f"item; no alignment is applied and none is measured by the "
                f"sealed corpus"
            )
        return cursor, None
    size, _reason = _elementary_size(entry, usage, ctx)
    if size is None or size not in SYNC_BOUNDARIES:
        return cursor, None
    zero_based = cursor - 1
    remainder = zero_based % size
    if remainder == 0:
        return cursor, None
    slack = size - remainder
    return cursor + slack, Gap(
        offset=cursor, length=slack, source="slack",
        cause=f"SYNCHRONIZED alignment of {entry.name} to a {size}-byte boundary",
    )


def _place_renames(root: _Entry, ctx: _Context) -> None:
    """Place level-66 entries.

    A RENAMES has **no offset of its own** — it renames a span across existing
    fields. The sealed oracle expresses that span as ``offset`` = the renamed
    start item's offset, ``length`` = the span's byte count, ``redefines`` =
    the start item's name and ``renames: true``. That shape can express a span,
    so the engine emits it rather than inventing a second one (WP-2.2 §3.2).
    """
    # First row wins. A dict comprehension would silently keep the LAST row of
    # a repeated name, which for `RENAMES A THRU B` is the difference between
    # a span and a wrong span.
    placed: Dict[str, _Row] = {}
    for row in ctx.rows:
        if not row.subscripts:
            placed.setdefault(row.name, row)
    for entry in root.children:
        if entry.level != 66:
            continue
        start_row = placed.get(entry.renames_from or "")
        if start_row is None or start_row.offset is None:
            ctx.note(
                f"66 {entry.name} RENAMES {entry.renames_from}, which was not "
                f"placed; no span is claimed"
            )
            ctx.rows.append(
                _Row(name=entry.name, level=66, offset=None, length=None,
                     elementary=True, in_tiling=False, entry=entry, subscripts=(),
                     redefines=entry.renames_from, renames=True,
                     unmeasured_reason="RENAMES start item not resolved")
            )
            continue
        end_row = start_row
        if entry.renames_thru:
            end_row = placed.get(entry.renames_thru)  # type: ignore[assignment]
            if end_row is None or end_row.offset is None or end_row.length is None:
                ctx.note(
                    f"66 {entry.name} RENAMES ... THRU {entry.renames_thru}, "
                    f"which was not placed; no span is claimed"
                )
                ctx.rows.append(
                    _Row(name=entry.name, level=66, offset=None, length=None,
                         elementary=True, in_tiling=False, entry=entry, subscripts=(),
                         redefines=entry.renames_from, renames=True,
                         unmeasured_reason="RENAMES THRU end item not resolved")
                )
                continue
        if start_row.length is None or end_row.length is None or end_row.offset is None:
            ctx.rows.append(
                _Row(name=entry.name, level=66, offset=start_row.offset, length=None,
                     elementary=True, in_tiling=False, entry=entry, subscripts=(),
                     redefines=entry.renames_from, renames=True,
                     unmeasured_reason="a renamed item has no computed length")
            )
            continue
        span = end_row.offset + end_row.length - start_row.offset
        ctx.rows.append(
            _Row(name=entry.name, level=66, offset=start_row.offset, length=span,
                 elementary=True, in_tiling=False, entry=entry, subscripts=(),
                 redefines=entry.renames_from, renames=True)
        )


# --------------------------------------------------------------------------
# Public entry points
# --------------------------------------------------------------------------

def _to_field(row: _Row, usage_map: Dict[int, str]) -> Field:
    entry = row.entry
    symbols = expand_picture(entry.picture) if entry.picture else None
    category = (
        "GROUP" if not row.elementary
        else picture_category(symbols) if symbols
        else ("ALPHANUMERIC" if row.renames else "UNKNOWN")
    )
    return Field(
        name=row.name,
        key=_key(row.name, row.subscripts),
        level=row.level,
        offset=row.offset,
        length=row.length,
        elementary=row.elementary,
        in_tiling=row.in_tiling and row.elementary and not row.renames,
        redefines=row.redefines,
        renames=row.renames,
        subscripts=row.subscripts,
        picture=entry.picture,
        usage=usage_map.get(id(entry), entry.usage or DISPLAY),
        sign=entry.sign,
        sign_separate=entry.sign_separate,
        occurs=entry.occurs,
        occurs_min=entry.occurs_min,
        odo_object=entry.odo_object,
        sync=entry.sync,
        justified=entry.justified,
        blank_when_zero=entry.blank_when_zero,
        category=category,
        source="computed",
        unmeasured_reason=row.unmeasured_reason,
    )


def _usage_map(entry: _Entry, inherited: Optional[str], out: Dict[int, str]) -> None:
    usage = _effective_usage(entry, inherited)
    out[id(entry)] = usage
    for child in entry.children:
        _usage_map(child, usage, out)


def _layout_for_root(
    root: _Entry, *, odo_value: Optional[int], origin: str, sha256: Optional[str]
) -> Layout:
    ctx = _Context(odo_value)
    usage_map: Dict[int, str] = {}
    _usage_map(root, None, usage_map)

    total, reason = _place_occurrences(
        root, 1, (), None, ctx, under_alias=False
    )
    _place_renames(root, ctx)

    odo_object = _find_odo_object(root)
    if odo_object is not None and odo_value is None:
        ctx.note(
            f"this record is variable length (OCCURS DEPENDING ON "
            f"{odo_object}); the layout below is at the MAXIMUM occurrence "
            f"count. Call compute(..., odo_value=n) for a specific extent."
        )

    fields = tuple(_to_field(row, usage_map) for row in ctx.rows)
    status = LayoutStatus.COMPLETE
    if total is None:
        status = LayoutStatus.PARTIAL
    elif any(f.length is None or f.offset is None for f in fields):
        status = LayoutStatus.PARTIAL
    elif ctx.reasons:
        status = LayoutStatus.PARTIAL

    if reason:
        ctx.note(reason)

    return Layout(
        group=root.name,
        group_length=total,
        status=status,
        fields=fields,
        gaps=tuple(sorted(ctx.gaps, key=lambda g: g.offset)),
        conditions=tuple(ctx.conditions),
        reasons=tuple(ctx.reasons),
        odo_object=odo_object,
        odo_value=odo_value,
        variable_length=odo_object is not None,
        origin=origin,
        source_sha256=sha256,
    )


def _find_odo_object(entry: _Entry) -> Optional[str]:
    if entry.odo_object:
        return entry.odo_object
    for child in entry.children:
        found = _find_odo_object(child)
        if found:
            return found
    return None


def compute_text(
    text: str,
    *,
    odo_value: Optional[int] = None,
    origin: str = "<text>",
    fixed_format: bool = True,
) -> Tuple[Layout, ...]:
    """Compute a :class:`Layout` for every ``01``/``77`` record in ``text``."""
    sha = hashlib.sha256(text.encode("utf-8")).hexdigest()
    roots = parse_entries(text, fixed_format=fixed_format)
    return tuple(
        _layout_for_root(root, odo_value=odo_value, origin=origin, sha256=sha)
        for root in roots
    )


def compute(
    source: object,
    resolution: Optional[Resolution] = None,
    *,
    odo_value: Optional[int] = None,
    record: Optional[str] = None,
) -> Optional[Layout]:
    """Compute one record's layout from a path, or from a resolved member name.

    ``compute(Path(...))`` reads a copybook file directly.
    ``compute("MEMBER", resolution)`` resolves the member, expands its nested
    COPY directives and applies REPLACING first.

    Returns ``None`` only when ``text`` holds no ``01``/``77`` record at all.
    A record that depends on an **unresolved** COPY comes back as a
    :class:`Layout` with ``status = NONE`` and ``group_length = None``, naming
    the missing member in ``reasons`` — a partial layout is never presented as
    complete (D20, R2).
    """
    if isinstance(source, (str, Path)) and resolution is None:
        path = Path(source)
        text = path.read_text(encoding="utf-8", errors="replace")
        layouts = compute_text(text, odo_value=odo_value, origin=path.as_posix())
    else:
        name = str(source).upper()
        assert resolution is not None
        if name not in resolution.records:
            missing = next(
                (u for u in resolution.unresolved if u.name == name), None
            )
            reason = (
                f"COPY {name} is unresolved; searched "
                f"{len(resolution.search_paths)} path(s)"
                if missing is None else
                f"COPY {name} is unresolved: {missing.reason}. Referenced by "
                f"{', '.join(missing.referenced_by)}"
            )
            return Layout(group=name, group_length=None, status=LayoutStatus.NONE,
                          reasons=(reason,), origin=name)
        assembly = assemble(name, resolution)
        if not assembly.complete:
            reasons = tuple(
                [f"COPY {m} is unresolved and is required by {name}" for m in assembly.missing]
                + [f"COPY cycle, detected and not followed: {c}" for c in assembly.cyclic]
                + [f"REPLACING was not fully applied: {u}" for u in assembly.unapplied]
            )
            return Layout(group=name, group_length=None, status=LayoutStatus.NONE,
                          reasons=reasons, origin=resolution.records[name].path)
        layouts = compute_text(
            assembly.text, odo_value=odo_value,
            origin=resolution.records[name].path,
        )

    if not layouts:
        return None
    if record is not None:
        for layout in layouts:
            if layout.group == record.upper():
                return layout
        return None
    return layouts[0]


# --------------------------------------------------------------------------
# The lint (acceptance (7))
# --------------------------------------------------------------------------

def lint_layout(layout: Layout) -> List[str]:
    """Return every D19 violation in ``layout``. Empty list means clean.

    A ``0`` where nothing was computed is the defect WP-2.1 measured on the
    other side of this boundary: ``MOVE HIGH-VALUES`` to a numeric-edited item
    marks zero bytes, exits zero, and warns about nothing. A fabricated zero is
    worse than a null because it survives every type check.
    """
    problems: List[str] = []
    for f in layout.fields:
        where = f"{layout.group}/{f.key}"
        if f.length == 0:
            problems.append(f"{where}: length 0 — an uncomputed length must be None (D19)")
        if f.offset == 0:
            problems.append(f"{where}: offset 0 — offsets are 1-based (U5)")
        if f.length is not None and f.length < 0:
            problems.append(f"{where}: negative length {f.length}")
        if f.offset is not None and f.offset < 0:
            problems.append(f"{where}: negative offset {f.offset}")
        if f.length is None and f.unmeasured_reason is None:
            problems.append(
                f"{where}: length is None with no unmeasured_reason — an absent "
                f"number must say why it is absent (R1/R9)"
            )
        if f.length is not None and f.unmeasured_reason is not None:
            problems.append(
                f"{where}: carries both a length ({f.length}) and an "
                f"unmeasured_reason — one of the two is a lie"
            )
    for c in layout.conditions:
        if c.offset is not None or c.length is not None:
            problems.append(
                f"{layout.group}/{c.name}: a level-88 has no storage; "
                f"offset and length must both be None (U7)"
            )
    for g in layout.gaps:
        if g.length <= 0:
            problems.append(f"{layout.group}: gap at {g.offset} has length {g.length}")
        if g.offset <= 0:
            problems.append(f"{layout.group}: gap offset {g.offset} is not 1-based")
        if g.source not in ("filler", "slack"):
            problems.append(f"{layout.group}: gap source {g.source!r} is not filler or slack")
    if layout.group_length == 0:
        problems.append(f"{layout.group}: group_length 0 — an uncomputed length must be None")
    if layout.status is LayoutStatus.COMPLETE and layout.group_length is None:
        problems.append(f"{layout.group}: status COMPLETE with no group_length")
    if layout.status is not LayoutStatus.COMPLETE and not layout.reasons:
        problems.append(
            f"{layout.group}: status {layout.status.value} with no reason given (R2)"
        )
    return problems


def assert_redefines_containment(layout: Layout) -> List[str]:
    """A REDEFINES field must lie wholly inside what it redefines.

    Asserted in engine output **independently of the oracle** (WP-2.2 §3.3), so
    a round-trip that agreed with a wrong oracle would still fail here.
    """
    problems: List[str] = []
    by_name: Dict[str, Field] = {}
    for f in layout.fields:
        if not f.subscripts:
            by_name.setdefault(f.name, f)
    for f in layout.fields:
        if not f.redefines or f.renames:
            continue
        target = by_name.get(f.redefines)
        if target is None or target.offset is None or target.end is None:
            problems.append(f"{f.key} REDEFINES {f.redefines}, which is not in the layout")
            continue
        if f.offset != target.offset:
            problems.append(
                f"{f.key} REDEFINES {f.redefines} but starts at {f.offset}, "
                f"not at the redefined item's offset {target.offset}"
            )
        if f.end is not None and f.end > target.end:
            problems.append(
                f"{f.key} REDEFINES {f.redefines} but ends at {f.end}, past the "
                f"redefined item's end {target.end}"
            )
    return problems


def assert_renames_within_record(layout: Layout) -> List[str]:
    """A level-66 span starts at what it renames and stays inside the record.

    Deliberately NOT containment: ``RENAMES A THRU B`` is longer than ``A``,
    which is the point of the clause (SPEC.md §6).
    """
    problems: List[str] = []
    by_name = {f.name: f for f in layout.fields if not f.subscripts}
    for f in layout.fields:
        if not f.renames:
            continue
        if f.redefines:
            start = by_name.get(f.redefines)
            if start is not None and start.offset is not None and f.offset != start.offset:
                problems.append(
                    f"{f.key} RENAMES {f.redefines} but starts at {f.offset}, "
                    f"not at {start.offset}"
                )
        if layout.group_length is not None and f.end is not None and f.end > layout.group_length:
            problems.append(
                f"{f.key} ends at {f.end}, past the record's {layout.group_length} bytes"
            )
    return problems


def assert_tiling(layout: Layout) -> List[str]:
    """Named, elementary, non-redefining, non-renaming fields plus gap spans
    cover ``[1, group_length]`` exactly once (SPEC.md §6, D9).

    Conditioned exactly as the SPEC conditions it. An unconditional tiling
    assertion fires on correct REDEFINES/RENAMES/ODO behaviour, and a gate that
    fires on correct behaviour trains its readers to wave it through.
    """
    if layout.group_length is None:
        return [f"{layout.group}: no group_length, so tiling cannot be asserted"]
    claims: Dict[int, str] = {}
    problems: List[str] = []
    for f in layout.fields:
        if not f.in_tiling or f.offset is None or f.length is None:
            continue
        for byte in range(f.offset, f.offset + f.length):
            if byte in claims:
                problems.append(
                    f"byte {byte} is claimed by both {claims[byte]} and {f.key}"
                )
            claims[byte] = f.key
    for g in layout.gaps:
        for byte in range(g.offset, g.offset + g.length):
            if byte in claims:
                problems.append(
                    f"byte {byte} is claimed by both {claims[byte]} and a "
                    f"{g.source} span"
                )
            claims[byte] = f"{g.source}@{g.offset}"
    for byte in range(1, layout.group_length + 1):
        if byte not in claims:
            problems.append(f"byte {byte} is claimed by nothing")
    return problems
