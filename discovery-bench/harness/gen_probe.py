"""Route B -- generate the runtime layout probe for one copybook.

WHAT THE PROBE MEASURES, AND WHY IT IS A MEASUREMENT
----------------------------------------------------
``cobc``'s symbol listing (Route A) gives each field's SIZE but no OFFSET.
Offsets therefore have to be observed rather than computed, because computing
them would mean reimplementing the compiler's own layout rules and then
grading the compiler against our reimplementation. Instead:

    1. clear every byte of the group to LOW-VALUE (x'00'),
    2. write HIGH-VALUE (x'FF') into exactly one field,
    3. scan the group byte by byte and report the first FF, the last FF, and
       how many FF bytes there were.

The first FF is the field's 1-based offset. The extent ``last - first + 1`` is
its length. The count is carried separately and asserted equal to the extent by
``oracle_layouts``: if a field's bytes are not contiguous, that is a finding
about this compiler, not a number to quietly average away.

Offsets are 1-BASED, because that is what §0.5 measured on ``MUBCUST.cpy``
(``WS-CW-ACCOUNT`` = 1, ``WS-CW-NAME`` = 11) and converting at the boundary is
a place to be wrong.

WHY THE PROBE REFERENCE-MODIFIES: ``field (1:)`` RATHER THAN ``field``
----------------------------------------------------------------------
WP-2.1 §3.3 specifies ``MOVE HIGH-VALUES TO <field>``. That form is unsound for
one of the twelve axes and was replaced after measurement, not after argument.

Measured on GnuCOBOL 3.1.2.0, ``PIC ZZZZZZZZ9.99`` and ``PIC ZZZ9 BLANK WHEN
ZERO``:

    MOVE HIGH-VALUES TO WS-B      ->  first FF 0, last FF 0, count 0

The move compiles, runs, exits zero, and writes NO high-value bytes at all --
the figurative constant is moved as a NUMERIC value into an edited item and
edits away to spaces. A probe built on the plain form would have reported
``offset: 0, length: 0`` for every numeric-edited field in ``D12_edited.cpy``
and for ``BPR-AMOUNT-DUE`` in the seeded ``MUBBREC.cpy``. Under D8/R1 a zero is
not an honest answer, and a zero that looks like a measurement is worse than a
null.

``MOVE HIGH-VALUES TO WS-B (1:)`` reference-modifies the item, which makes the
receiving field alphanumeric of the item's full length regardless of its
category, and fills every byte. Measured over all fifteen copybooks it is exact
for DISPLAY, COMP, COMP-3, COMP-5, numeric-edited, alphanumeric-edited,
JUSTIFIED, BLANK WHEN ZERO, group items, REDEFINES aliases and level-66
RENAMES aliases alike.

Two smaller consequences, both recorded rather than hidden:

  * ``-Warchaic`` fires on ``MOVE <figurative constant> TO <numeric item>``
    (T13) but NOT on the reference-modified form, because reference
    modification makes the receiving item alphanumeric. The flag is not passed
    and the warning is not suppressed globally; ``oracle_layouts`` tolerates it
    wherever it appears rather than treating any warning as failure.
  * ``FUNCTION HEX-OF`` does not exist in GnuCOBOL 3.1.2 (T12) and is never
    emitted. The scan compares against the figurative constant ``HIGH-VALUE``
    directly.

TWO BYTE-WINDOW STRATEGIES, AND WHY THERE ARE TWO
-------------------------------------------------
``WINDOW_TABLE`` is the form WP-2.1 specifies: an ``01`` item that REDEFINES
the copybook's group as ``OCCURS <total_len> TIMES PIC X(01)``, indexed to read
individual bytes.

It cannot be used on ``D07_odo.cpy``. Measured:

    error: the original definition 'D07-ODO' cannot be variable length

so a group carrying OCCURS DEPENDING ON cannot be redefined at all. Those
copybooks use ``WINDOW_REFMOD``, which reads bytes as ``GROUP (I:1)`` and needs
no redefinition. The two strategies are proved equivalent rather than assumed
equivalent: ``tests/test_bench_oracle.py`` probes a fixed-length copybook under
both and asserts every row is identical.

FILLER IS NOT PROBEABLE
-----------------------
``FILLER`` cannot be the receiving item of a MOVE, and neither can the slack
bytes SYNCHRONIZED inserts (measured: ``D10_sync.cpy`` declares 20 bytes of
fields and occupies 25). Neither is emitted here. They are recovered downstream
as the bytes no named field claims, carried as ``gap`` spans in their own array
so a derived span never sits in the same list as a measurement (D8).

LINE DISCIPLINE
---------------
Emitted source is fixed-format COBOL: area A at column 8, area B at column 12,
and nothing past column 72. A generated line that overruns 72 does not fail
loudly -- it is TRUNCATED by the compiler and produces a baffling syntax error
somewhere else. ``_emit`` asserts the width on every line it appends.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field as dc_field
from typing import Dict, List, Optional, Sequence, Tuple

#: Fixed-format COBOL source line limit. Anything past this is truncated.
MAX_COLUMN = 72

#: Byte-window strategies. See the module docstring.
WINDOW_TABLE = "redefines-table"
WINDOW_REFMOD = "reference-modification"

#: Marker prefixes on the probe's stdout. Parsed by ``oracle_layouts``.
ROW_PREFIX = "F|"
LEN_PREFIX = "G|"

_ODO_DEPENDING = re.compile(
    r"\bDEPENDING\s+(?:ON\s+)?([A-Za-z0-9][A-Za-z0-9-]*)", re.IGNORECASE
)


@dataclass(frozen=True)
class ProbeTarget:
    """One thing to measure: a field, optionally subscripted.

    ``name`` is the COBOL data name. ``subscripts`` is empty for a scalar and
    holds one integer per enclosing OCCURS, outermost first, for a table
    element. ``key`` is the display form used in the probe's output and in the
    oracle, so ``D06-INNER-CODE (2, 1)`` is distinguishable from
    ``D06-INNER-CODE (1, 2)``.
    """

    name: str
    subscripts: Tuple[int, ...] = ()

    @property
    def reference(self) -> str:
        """The COBOL reference, e.g. ``D06-INNER-CODE (2, 1)``."""
        if not self.subscripts:
            return self.name
        return f"{self.name} ({', '.join(str(s) for s in self.subscripts)})"

    @property
    def key(self) -> str:
        """Stable identity for this occurrence in the oracle."""
        return self.reference


@dataclass
class ProbePlan:
    """Everything the emitter needs. Built by ``oracle_layouts`` from Route A."""

    program_id: str
    copybook_file: str
    group: str
    total_len: int
    targets: List[ProbeTarget]
    window: str = WINDOW_TABLE
    #: ODO controlling field name, or None. Set only for variable groups.
    odo_field: Optional[str] = None
    #: ODO occurrence counts to probe at, in order. Usually (min, max).
    odo_values: Tuple[int, ...] = ()
    #: Targets valid only when the ODO count is at least this many. Keyed by
    #: ProbeTarget.key. Absent means "valid at every ODO value".
    odo_requires: Dict[str, int] = dc_field(default_factory=dict)


def find_odo_field(copybook_text: str) -> Optional[str]:
    """The data name after ``DEPENDING ON``, or None if the group is fixed.

    This is the ONE structural fact the symbol listing does not carry: it
    renders the table as ``OCCURS 1 TO 4`` and never names the controlling
    item. Everything else the probe needs -- names, levels, sizes, OCCURS
    counts, REDEFINES targets -- comes from ``cobc``'s own listing rather than
    from a parser of ours, which is what makes Route A an independent check on
    Route B instead of a second opinion from the same code.

    Comment lines (``*`` in column 7) are ignored, so the word DEPENDING
    appearing in a copybook's header banner is not mistaken for a clause.
    """
    for line in copybook_text.splitlines():
        if len(line) > 6 and line[6] in "*/":
            continue
        match = _ODO_DEPENDING.search(line)
        if match:
            return match.group(1).upper()
    return None


class _Emitter:
    """Accumulates fixed-format COBOL lines, refusing to exceed column 72."""

    def __init__(self) -> None:
        self.lines: List[str] = []

    def a(self, text: str) -> None:
        """Emit in area A (column 8)."""
        self._append(" " * 7 + text)

    def b(self, text: str, indent: int = 0) -> None:
        """Emit in area B (column 12, plus ``indent`` spaces)."""
        self._append(" " * (11 + indent) + text)

    def comment(self, text: str) -> None:
        self._append("      *" + text[: MAX_COLUMN - 7])

    def blank(self) -> None:
        self._append("      *")

    def _append(self, line: str) -> None:
        if len(line) > MAX_COLUMN:
            # Loud, because the alternative is silent truncation by the
            # compiler and a syntax error pointing at the wrong place.
            raise ValueError(
                f"generated line exceeds column {MAX_COLUMN} "
                f"({len(line)} chars) and would be truncated: {line!r}"
            )
        self.lines.append(line)

    def text(self) -> str:
        return "\n".join(self.lines) + "\n"


def build(plan: ProbePlan) -> str:
    """Emit the complete probe program for ``plan``.

    Deterministic: the same plan always produces byte-identical source. Nothing
    here reads the clock, the filesystem or the environment.
    """
    if plan.total_len < 1:
        raise ValueError(
            f"{plan.copybook_file}: Route A reported a group length of "
            f"{plan.total_len}, so there is nothing to probe. Route A must run "
            f"first and must succeed -- the probe cannot size its byte window "
            f"without the group total."
        )
    if plan.window not in (WINDOW_TABLE, WINDOW_REFMOD):
        raise ValueError(f"unknown byte-window strategy {plan.window!r}")
    if plan.window == WINDOW_TABLE and plan.odo_field:
        raise ValueError(
            f"{plan.copybook_file}: a group with OCCURS DEPENDING ON cannot be "
            f"redefined -- GnuCOBOL 3.1.2 rejects it with \"the original "
            f"definition '{plan.group}' cannot be variable length\". Use "
            f"WINDOW_REFMOD."
        )
    if plan.odo_field and not plan.odo_values:
        raise ValueError(
            f"{plan.copybook_file}: an ODO group must be probed at explicit "
            f"occurrence counts; none were given. A single length would be a "
            f"choice presented as a measurement."
        )

    out = _Emitter()
    _emit_header(out, plan)
    _emit_data_division(out, plan)
    _emit_procedure_division(out, plan)
    _emit_scan_paragraph(out, plan)
    return out.text()


def _emit_header(out: _Emitter, plan: ProbePlan) -> None:
    out.comment("=" * 64)
    out.comment(f" GENERATED PROBE -- {plan.copybook_file}")
    out.comment(" RELIAN-DISCOVERY-BENCH v0.1, harness/gen_probe.py.")
    out.comment(" Generated. Do not edit; edit the generator.")
    out.comment(f" byte window: {plan.window}")
    out.comment("=" * 64)
    out.a("IDENTIFICATION DIVISION.")
    out.a(f"PROGRAM-ID. {plan.program_id}.")
    out.a("DATA DIVISION.")
    out.a("WORKING-STORAGE SECTION.")


def _emit_data_division(out: _Emitter, plan: ProbePlan) -> None:
    out.b(f'COPY "{plan.copybook_file}".')
    if plan.window == WINDOW_TABLE:
        out.comment(" The byte window: the group re-cut as single characters.")
        out.a(f"01  PB-WINDOW REDEFINES {plan.group}.")
        out.b(f"05  PB-BYTE OCCURS {plan.total_len} TIMES PIC X(01).", 4)
    else:
        out.comment(" No redefining window: this group is variable length and")
        out.comment(" GnuCOBOL 3.1.2 refuses to redefine it. Bytes are read")
        out.comment(" through reference modification on the group itself.")
    out.a("01  PB-I     PIC 9(05) COMP-5.")
    out.a("01  PB-FIRST PIC 9(05) COMP-5.")
    out.a("01  PB-LAST  PIC 9(05) COMP-5.")
    out.a("01  PB-COUNT PIC 9(05) COMP-5.")
    out.a("01  PB-LIMIT PIC 9(05) COMP-5.")
    out.a("01  PB-GLEN  PIC 9(05) COMP-5.")


def _emit_procedure_division(out: _Emitter, plan: ProbePlan) -> None:
    out.a("PROCEDURE DIVISION.")
    if plan.odo_field:
        for value in plan.odo_values:
            out.comment(f" --- ODO {plan.odo_field} = {value} ---")
            out.b(f"MOVE {value} TO {plan.odo_field}")
            out.b(f"MOVE FUNCTION LENGTH ({plan.group}) TO PB-GLEN")
            out.b("MOVE PB-GLEN TO PB-LIMIT")
            out.b(f'DISPLAY "{LEN_PREFIX}" {value} "|" PB-GLEN')
            for target in plan.targets:
                needs = plan.odo_requires.get(target.key, 0)
                if needs > value:
                    continue
                _emit_one_probe(out, plan, target, odo_value=value)
    else:
        out.b(f"MOVE {plan.total_len} TO PB-LIMIT")
        out.b(f"MOVE FUNCTION LENGTH ({plan.group}) TO PB-GLEN")
        out.b(f'DISPLAY "{LEN_PREFIX}0|" PB-GLEN')
        for target in plan.targets:
            _emit_one_probe(out, plan, target, odo_value=None)
    out.b("GOBACK.")


def _emit_one_probe(
    out: _Emitter, plan: ProbePlan, target: ProbeTarget, odo_value: Optional[int]
) -> None:
    """Clear, mark one field, scan, report.

    The clear goes through the byte window rather than the group when a window
    exists, so the whole probed extent is reset even if the group's own MOVE
    semantics differ. For an ODO group there is no window, so the group is
    cleared at its current length -- which is exactly the extent the scan will
    then cover.
    """
    clear_target = "PB-WINDOW" if plan.window == WINDOW_TABLE else plan.group
    out.b(f"MOVE LOW-VALUES TO {clear_target}")
    if odo_value is not None:
        # MOVE LOW-VALUES also clears the ODO controlling item, which would
        # shorten the group under our feet. Restore it before scanning.
        out.b(f"MOVE {odo_value} TO {plan.odo_field}")
    out.b(f"MOVE HIGH-VALUES TO {target.reference} (1:)")
    out.b("PERFORM 9000-SCAN")
    # Split across a continuation line unconditionally. The single-line form
    # fits for short names and silently overruns column 72 for longer ones --
    # measured on `D01-UNSIGNED-18`, at 73 characters -- and a rule that holds
    # only for short names is a rule that fails on the interesting file.
    out.b(f'DISPLAY "{ROW_PREFIX}{target.key}|"')
    out.b('PB-FIRST "|" PB-LAST "|" PB-COUNT', 4)


def _emit_scan_paragraph(out: _Emitter, plan: ProbePlan) -> None:
    byte_ref = (
        "PB-BYTE (PB-I)"
        if plan.window == WINDOW_TABLE
        else f"{plan.group} (PB-I:1)"
    )
    out.a("9000-SCAN.")
    out.b("MOVE 0 TO PB-FIRST")
    out.b("MOVE 0 TO PB-LAST")
    out.b("MOVE 0 TO PB-COUNT")
    out.b("PERFORM VARYING PB-I FROM 1 BY 1")
    out.b("        UNTIL PB-I > PB-LIMIT", 4)
    out.b(f"IF {byte_ref} = HIGH-VALUE", 4)
    out.b("IF PB-FIRST = 0", 8)
    out.b("MOVE PB-I TO PB-FIRST", 12)
    out.b("END-IF", 8)
    out.b("MOVE PB-I TO PB-LAST", 8)
    out.b("ADD 1 TO PB-COUNT", 8)
    out.b("END-IF", 4)
    out.b("END-PERFORM.")


@dataclass(frozen=True)
class ProbeRow:
    """One parsed line of probe output."""

    key: str
    first: int
    last: int
    count: int

    @property
    def offset(self) -> int:
        """1-based offset of the field's first byte."""
        return self.first

    @property
    def length(self) -> int:
        """Extent in bytes, from first marked byte to last."""
        return self.last - self.first + 1

    @property
    def contiguous(self) -> bool:
        """True when every byte in the extent was marked.

        False means the field's storage is not a single run. That is a finding
        about the compiler, and ``oracle_layouts`` raises on it rather than
        recording ``length`` as if it were unqualified.
        """
        return self.count == self.length


def parse_output(text: str) -> Tuple[Dict[int, List[ProbeRow]], Dict[int, int]]:
    """Split probe stdout into ``{odo_value: [rows]}`` and ``{odo_value: len}``.

    A fixed-length copybook reports under the pseudo-ODO value ``0``.
    """
    rows: Dict[int, List[ProbeRow]] = {}
    lengths: Dict[int, int] = {}
    current: Optional[int] = None
    for raw in text.splitlines():
        line = raw.strip()
        if line.startswith(LEN_PREFIX):
            _, value, glen = line.split("|", 2)
            current = int(value)
            lengths[current] = int(glen)
            rows.setdefault(current, [])
        elif line.startswith(ROW_PREFIX):
            if current is None:
                raise ValueError(
                    f"probe emitted a field row before any group-length row: "
                    f"{line!r}"
                )
            body = line[len(ROW_PREFIX):]
            key, first, last, count = body.rsplit("|", 3)
            rows[current].append(
                ProbeRow(key.strip(), int(first), int(last), int(count))
            )
    return rows, lengths


def expand_occurrences(
    dimensions: Sequence[int],
) -> List[Tuple[int, ...]]:
    """Every subscript tuple for a field nested under ``dimensions`` OCCURS.

    Outermost dimension first, and in lexicographic order, so the generated
    program and the oracle are both stable across runs.
    """
    result: List[Tuple[int, ...]] = [()]
    for size in dimensions:
        result = [prefix + (i,) for prefix in result for i in range(1, size + 1)]
    return result
