"""Generate ``IBMLAYOUT.cbl`` for one copybook — the customer verification kit.

WHAT THE PROGRAM DOES, AND WHAT IT DOES NOT DO
----------------------------------------------
It **declares** the copybook and **reports storage**. It reads no data, opens
no file and connects to nothing (D34/R12): the only thing that can leave the
customer's perimeter is a description of their own copybook, and only if they
choose to send it.

The method is the sealed oracle's Route B, unchanged, because that method is
*measured* rather than argued (``discovery-bench/harness/gen_probe.py``):

    1. clear every byte of the group to LOW-VALUE,
    2. write HIGH-VALUE into exactly one field,
    3. scan the group byte by byte and report the first FF, the last FF, and
       how many FF bytes there were.

First FF is the field's 1-based offset; ``last - first + 1`` is its length. The
count is reported separately so a non-contiguous field is a *finding* rather
than a number quietly averaged away.

``MOVE HIGH-VALUES TO <field> (1:)`` is reference-modified deliberately. The
plain form is unsound: measured under GnuCOBOL 3.1.2.0, ``MOVE HIGH-VALUES``
to a numeric-edited item writes no high-value bytes at all and would report
``offset 0, length 0`` — a zero that looks like a measurement (WP-2.1 §3.3,
D19).

THE ONE CONSTRUCT WHOSE IBM ACCEPTANCE IS NOT VERIFIED
------------------------------------------------------
That same reference modification is the kit's known portability limit, and it
is documented from IBM's own manual rather than guessed:

    "You can refer to a substring of a data item that has USAGE DISPLAY,
    DISPLAY-1, or NATIONAL by using a reference modifier."
    -- IBM Enterprise COBOL for z/OS Language Reference 6.3, "Reference
       modification"

So on IBM Enterprise COBOL a ``COMP``, ``COMP-3`` or ``COMP-5`` field cannot be
reference-modified, and those probe paragraphs are expected to be rejected at
compile time. Relian has no IBM system and will not claim otherwise, so the kit
ships **two paths** and the README tells the customer's team to prefer the
second for binary and packed fields:

* **Path B** — this program. Verified end to end under ``cobc 3.1.2.0`` and
  round-tripped against the sealed oracle (``tests/test_verification_kit.py``).
* **Path A** — compile the copybook with IBM's ``MAP`` option and return the
  listing. That is IBM's own authoritative output, needs no program to run, and
  covers every category including the ones Path B cannot reach on IBM.
  ``ingest_verification.py`` reads either.

Nothing here invokes a compiler: this module writes COBOL text from the
engine's own parse (D15). Compiling it is the customer's step, or the test's.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import List, Optional, Sequence, Tuple

_REPO_ROOT = Path(__file__).resolve().parents[1]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from src.discovery.layout import (  # noqa: E402
    BINARY_USAGES,
    PACKED_USAGES,
    Layout,
    compute_text,
)

#: Emitted in the kit's JSON so a returned file says which program produced it.
KIT_SCHEMA = "relian-discovery-verify/layout/v1"
KIT_VERSION = "1.0"

#: Usages IBM cannot reference-modify (sourced; see the module docstring). The
#: generated program marks these paragraphs so a customer's mainframe team can
#: see at a glance which rows Path B may not deliver on their compiler.
_IBM_REFMOD_INELIGIBLE = frozenset(BINARY_USAGES | PACKED_USAGES)


def _json_string(text: str) -> str:
    """JSON-escape ``text`` for embedding in a COBOL literal."""
    return text.replace("\\", "\\\\").replace('"', '\\"')


#: Fixed-format COBOL: Area A at column 8, Area B at column 12, and nothing
#: past column 72. Mainframe shops compile fixed format, so the kit is emitted
#: fixed format -- and a generator that silently overruns column 72 produces
#: "continuation character expected" on the customer's machine, which is a
#: defect they cannot fix and we would never see.
COLUMN_LIMIT = 72


class ColumnOverrun(AssertionError):
    """A generated line would not fit Area B.

    Raised at generation time. The alternative is discovering it when the
    customer's compiler rejects the program.
    """


class _Out:
    """Fixed-format COBOL emitter that refuses to emit an overlong line."""

    def __init__(self) -> None:
        self.lines: List[str] = []

    def _add(self, line: str) -> None:
        if len(line) > COLUMN_LIMIT:
            raise ColumnOverrun(
                f"generated line is {len(line)} columns, over the "
                f"{COLUMN_LIMIT}-column fixed-format limit: {line!r}"
            )
        self.lines.append(line)

    def a(self, text: str) -> None:
        self._add(" " * 7 + text)

    def b(self, text: str) -> None:
        self._add(" " * 11 + text)

    def raw(self, line: str) -> None:
        """A line from the customer's own copybook, passed through unchanged."""
        self.lines.append(line)

    def comment(self, text: str) -> None:
        self._add((" " * 6 + "*" + text)[:COLUMN_LIMIT])

    def display(self, *operands: str) -> None:
        """Emit ``DISPLAY`` with its operands wrapped inside Area B.

        Operands are COBOL literals (apostrophe-delimited, so a JSON double
        quote needs no doubling) or data names. DISPLAY concatenates its
        operands with no separator, so wrapping across source lines does not
        change the output line -- which is what makes the JSON stay on one
        line while the source stays inside column 72.
        """
        indent = " " * 11
        cont = " " * 15
        current = indent + "DISPLAY"
        for operand in operands:
            candidate = f"{current} {operand}"
            if len(candidate) <= COLUMN_LIMIT:
                current = candidate
                continue
            self._add(current)
            current = cont + operand
            if len(current) > COLUMN_LIMIT:
                raise ColumnOverrun(
                    f"single DISPLAY operand does not fit Area B: {operand!r}"
                )
        self._add(current)

    def display_at(self, extra: int, *operands: str) -> None:
        """:meth:`display`, indented ``extra`` columns inside a conditional."""
        indent = " " * (11 + extra)
        cont = " " * (15 + extra)
        current = indent + "DISPLAY"
        for operand in operands:
            candidate = f"{current} {operand}"
            if len(candidate) <= COLUMN_LIMIT:
                current = candidate
                continue
            self._add(current)
            current = cont + operand
            if len(current) > COLUMN_LIMIT:
                raise ColumnOverrun(
                    f"single DISPLAY operand does not fit Area B: {operand!r}"
                )
        self._add(current)

    def period(self) -> None:
        """Terminate the current sentence.

        A paragraph header may only follow a terminated sentence: without this,
        the compiler reads the next paragraph name as a continuation operand
        and reports it as an undefined procedure, which is a confusing way to
        be told a period is missing.
        """
        if self.lines and not self.lines[-1].rstrip().endswith("."):
            self.lines[-1] = self.lines[-1].rstrip() + "."

    def text(self) -> str:
        return "\n".join(self.lines) + "\n"


def _lit(text: str) -> str:
    """A COBOL apostrophe-delimited literal, doubling any apostrophe."""
    return "'" + text.replace("'", "''") + "'"


def _reference(name: str, subscripts: Sequence[int]) -> str:
    if not subscripts:
        return name
    return f"{name} ({', '.join(str(s) for s in subscripts)})"


def probeable_fields(layout: Layout) -> List[Tuple[str, str, Sequence[int], str]]:
    """``(key, name, subscripts, usage)`` for every field the kit can mark.

    ``FILLER`` is excluded because it cannot be the receiving item of a MOVE,
    and so are the slack bytes SYNCHRONIZED inserts — they belong to no named
    field. Both are recovered downstream as ``gap`` spans by subtraction and
    are graded ``derived`` rather than ``measured``, exactly as the oracle does.
    """
    out = []
    for f in layout.fields:
        # Groups, REDEFINES aliases and level-66 RENAMES aliases are all
        # markable and are all probed, exactly as the sealed oracle's Route B
        # probes them. Excluding them would shrink the kit's coverage of the
        # oracle below the 186 comparisons the engine is graded on, and a kit
        # that verifies less than the engine claims is a kit that agrees for
        # the wrong reason.
        if f.name.upper() == "FILLER" or f.name.upper().startswith("FILLER"):
            continue
        out.append((f.key, f.name, tuple(f.subscripts), f.usage))
    return out


def generate(
    layout: Layout,
    *,
    copy_member: Optional[str] = None,
    copybook_text: Optional[str] = None,
    program_id: str = "IBMLAYOUT",
    odo_value: Optional[int] = None,
) -> str:
    """Emit the probe program for ``layout``.

    Either ``copy_member`` (emit a ``COPY`` statement, the customer's normal
    case) or ``copybook_text`` (inline the declaration, used by the test so the
    round-trip does not depend on a copy path).
    """
    if (copy_member is None) == (copybook_text is None):
        raise ValueError("pass exactly one of copy_member or copybook_text")

    fields = probeable_fields(layout)
    o = _Out()

    o.comment("=" * 64)
    o.comment(" IBMLAYOUT -- Relian discovery verification kit.")
    o.comment(" GENERATED. Reports STORAGE for a copybook. Reads NO DATA,")
    o.comment(" opens no file, connects to nothing.")
    o.comment("")
    o.comment(f" Record  : {layout.group}")
    o.comment(f" Schema  : {KIT_SCHEMA}")
    o.comment("")
    o.comment(" Method (the sealed oracle's Route B, unchanged):")
    o.comment("   1. clear the group to LOW-VALUE")
    o.comment("   2. write HIGH-VALUE into exactly one field")
    o.comment("   3. scan the group; report first FF, last FF, count")
    o.comment("")
    o.comment(" IBM Enterprise COBOL: reference modification is restricted")
    o.comment(" to USAGE DISPLAY, DISPLAY-1 and NATIONAL, so paragraphs")
    o.comment(" marked IBM-REFMOD-INELIGIBLE may be rejected by your")
    o.comment(" compiler. That is expected and documented -- use the MAP")
    o.comment(" listing path in README.md for those fields.")
    o.comment("=" * 64)

    o.a("IDENTIFICATION DIVISION.")
    o.a(f"PROGRAM-ID. {program_id}.")
    o.a("DATA DIVISION.")
    o.a("WORKING-STORAGE SECTION.")
    if copy_member is not None:
        o.b(f"COPY {copy_member}.")
    else:
        for line in (copybook_text or "").splitlines():
            o.raw(line.rstrip())

    o.a("01  PB-CTL.")
    o.b("05  PB-I        PIC 9(09) COMP-5 VALUE 0.")
    o.b("05  PB-LEN      PIC 9(09) COMP-5 VALUE 0.")
    o.b("05  PB-FIRST    PIC 9(09) COMP-5 VALUE 0.")
    o.b("05  PB-LAST     PIC 9(09) COMP-5 VALUE 0.")
    o.b("05  PB-COUNT    PIC 9(09) COMP-5 VALUE 0.")
    # PIC Z(8)9 always renders the units digit, so zero is "        0" rather
    # than all spaces. JSON permits whitespace between a colon and a number,
    # so an edited field can be DISPLAYed straight into the document without a
    # portable left-justify -- FUNCTION TRIM is not available on every
    # compiler this kit targets.
    o.b("05  PB-E-OFF    PIC Z(8)9.")
    o.b("05  PB-E-LEN    PIC Z(8)9.")
    o.b("05  PB-E-CNT    PIC Z(8)9.")
    o.b("05  PB-E-GRP    PIC Z(8)9.")

    o.a("PROCEDURE DIVISION.")
    if layout.odo_object and odo_value is not None:
        # The extent has to be set BEFORE the group is measured: a
        # variable-length group's FUNCTION LENGTH is its length AT THE CURRENT
        # ODO VALUE, and an unset controlling item measures only the fixed
        # part. That reports a short record with no error anywhere.
        o.b(f"MOVE {odo_value} TO {layout.odo_object}")
    o.b(f"COMPUTE PB-LEN = FUNCTION LENGTH ({layout.group})")
    o.b("MOVE PB-LEN TO PB-E-GRP")
    o.display(_lit("{"))
    o.display(_lit(f'"schema":"{_json_string(KIT_SCHEMA)}",'))
    o.display(_lit(f'"kit_version":"{_json_string(KIT_VERSION)}",'))
    o.display(_lit(f'"record":"{_json_string(layout.group)}",'))
    o.display(_lit('"reads_no_data":true,'))
    o.display(_lit('"group_length":'), "PB-E-GRP", _lit(","))
    o.display(_lit('"fields":['))
    o.period()

    for index, (key, name, subs, usage) in enumerate(fields):
        para = f"PB-{index + 1:04d}"
        last = index == len(fields) - 1
        reference = _reference(name, subs)
        o.a(f"{para}.")
        if usage in _IBM_REFMOD_INELIGIBLE:
            o.comment(f" IBM-REFMOD-INELIGIBLE usage {usage}")
        o.b(f"MOVE LOW-VALUES TO {layout.group}")
        if layout.odo_object and odo_value is not None:
            # MOVE LOW-VALUES also clears the OCCURS DEPENDING ON controlling
            # item, which shortens the group under our feet: the table would
            # have zero entries and marking an occurrence would write nothing
            # at all. Restore the extent before scanning.
            o.b(f"MOVE {odo_value} TO {layout.odo_object}")
            o.b(f"COMPUTE PB-LEN = FUNCTION LENGTH ({layout.group})")
        o.b(f"MOVE HIGH-VALUES TO {reference} (1:)")
        o.b("MOVE 0 TO PB-FIRST PB-LAST PB-COUNT")
        o.b("PERFORM VARYING PB-I FROM 1 BY 1")
        o.b("        UNTIL PB-I > PB-LEN")
        o.b(f"    IF {layout.group} (PB-I:1) = HIGH-VALUE")
        o.b("        IF PB-FIRST = 0")
        o.b("            MOVE PB-I TO PB-FIRST")
        o.b("        END-IF")
        o.b("        MOVE PB-I TO PB-LAST")
        o.b("        ADD 1 TO PB-COUNT")
        o.b("    END-IF")
        o.b("END-PERFORM")
        o.b("MOVE PB-COUNT TO PB-E-CNT")
        o.display(_lit("{"))
        o.display(_lit(f'"key":"{_json_string(key)}",'))
        o.display(_lit(f'"name":"{_json_string(name)}",'))
        o.display(_lit(f'"usage":"{_json_string(usage)}",'))
        # A field the probe could not mark reports NULL, never 0. A zero here
        # would be a fabricated measurement that survives every type check --
        # exactly the defect WP-2.1 measured when `MOVE HIGH-VALUES` to a
        # numeric-edited item wrote no bytes and still exited zero (D19/R1).
        o.b("IF PB-FIRST = 0")
        o.display_at(4, _lit('"offset":null,'))
        o.display_at(4, _lit('"length":null,'))
        o.display_at(4, _lit('"marked":false,'))
        o.b("ELSE")
        o.b("    MOVE PB-FIRST TO PB-E-OFF")
        o.b("    COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1")
        o.display_at(4, _lit('"offset":'), "PB-E-OFF", _lit(","))
        o.display_at(4, _lit('"length":'), "PB-E-LEN", _lit(","))
        o.display_at(4, _lit('"marked":true,'))
        o.b("END-IF")
        o.display(_lit('"marked_bytes":'), "PB-E-CNT")
        o.display(_lit("}" if last else "},"))
        o.period()

    o.a("PB-END.")
    o.display(_lit("]"))
    o.display(_lit("}"))
    o.b("GOBACK.")
    return o.text()


def main(argv: Optional[Sequence[str]] = None) -> int:
    ap = argparse.ArgumentParser(
        prog="gen_ibmlayout",
        description="Generate the IBMLAYOUT.cbl verification probe for a copybook.",
    )
    ap.add_argument("copybook", help="a .cpy file")
    ap.add_argument("--out", default="-", help="output .cbl path, or - for stdout")
    ap.add_argument("--copy-member", default=None,
                    help="emit COPY <member> instead of inlining the declaration")
    ap.add_argument("--program-id", default="IBMLAYOUT")
    ap.add_argument("--odo", type=int, default=None)
    args = ap.parse_args(argv)

    path = Path(args.copybook)
    text = path.read_text(encoding="utf-8", errors="replace")
    layouts = compute_text(text, odo_value=args.odo, origin=path.as_posix())
    if not layouts:
        print(f"no 01/77 record found in {path}", file=sys.stderr)
        return 1

    source = generate(
        layouts[0],
        copy_member=args.copy_member,
        copybook_text=None if args.copy_member else text,
        program_id=args.program_id,
        odo_value=args.odo,
    )
    if args.out == "-":
        sys.stdout.write(source)
    else:
        Path(args.out).write_text(source, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
