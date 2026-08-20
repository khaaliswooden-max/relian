"""Build the RELIAN-DISCOVERY-BENCH layout oracle from GnuCOBOL itself.

THE ROUTES RUN IN A FIXED ORDER AND THE ORDER IS A CONTRACT
------------------------------------------------------------
    Route A   ``cobc -x -t <lst> -ftsymbols -I<corpus> <driver>`` over a driver
              that does nothing but COPY the copybook into WORKING-STORAGE.
              Yields per-field SIZE, the declaration structure (level, OCCURS,
              REDEFINES, RENAMES, condition names) and THE GROUP TOTAL.
    Route B   the generated probe. It cannot be emitted at all until Route A
              has produced the group total, because that total is what sizes
              the ``REDEFINES ... OCCURS total_len TIMES PIC X(01)`` byte table
              the probe scans.

Route A is not a convenience. It is the independent second opinion that makes
Route B checkable: the probe measures offsets, the listing states sizes, and
every field must agree on length to the byte with ZERO tolerance. Both come
from ``cobc`` rather than from a parser of ours, so an error in this file
cannot make the two agree with each other while both being wrong.

WHAT ROUTE A CANNOT TELL US, MEASURED RATHER THAN ASSUMED
---------------------------------------------------------
The symbol listing is lossy in four ways, each recorded here because each is a
place a reader could otherwise assume more than the data supports:

  * **No offsets at all.** The whole reason Route B exists (T10).
  * **The SIGN clause is erased.** ``PIC S9(04) SIGN LEADING SEPARATE`` renders
    as ``S9(04)``; only the SIZE (5 rather than 4) distinguishes it. Likewise
    JUSTIFIED and BLANK WHEN ZERO are absent from the PICTURE column.
  * **COMP-4 and BINARY are normalised to COMP.** ``D02_binary.cpy`` declares
    all three; the listing shows ``COMP`` for each. The distinction survives
    only in the sealed copybook.
  * **``DEPENDING ON`` never names its controlling item.** The listing renders
    ``OCCURS 1 TO 4`` and stops, so that one fact is read from the copybook
    source by ``gen_probe.find_odo_field``.

And one asymmetry that is easy to get backwards, so it is asserted rather than
trusted:

  * **A GROUP row's SIZE covers ALL of its own occurrences; an ELEMENTARY
    row's SIZE covers ONE.** Measured: ``D05-ROW`` (a group, OCCURS 3, one
    occurrence 5 bytes) reports SIZE 00015, while ``D05-SLOT`` (elementary,
    OCCURS 4, 5 bytes each) reports SIZE 00005. ``_expected_element_size``
    encodes exactly that and nothing more.

THE TILING INVARIANT IS CONDITIONED, OR IT IS WRONG (D9)
---------------------------------------------------------
"Fields tile the group with no gap or overlap" holds for ``MUBCUST.cpy``
because it is flat. It fails BY DESIGN under REDEFINES (deliberate overlap),
level-66 RENAMES (deliberate aliasing), OCCURS DEPENDING ON (variable extent)
and level-88 (no extent) -- which is most of what this corpus exists to test.
What is asserted instead:

    Over the projection of NAMED, ELEMENTARY, NON-REDEFINING, NON-RENAMING
    fields, the union of [offset, offset+length) plus `gap` spans covers
    [1, group_length] exactly once.

Redefining and renaming fields are asserted separately (``_check_aliases``).
ODO copybooks are probed at min and max and both lengths are recorded; the
invariant is asserted independently at each.

`gap` SPANS ARE DERIVED, NOT MEASURED, AND ARE KEPT APART
---------------------------------------------------------
``FILLER`` cannot be the receiving item of a MOVE, and neither can the slack
bytes SYNCHRONIZED inserts. Measured: ``D10_sync.cpy`` declares 20 bytes of
named fields and occupies 25. Those five bytes are real and they are not
measurable by the probe -- they are recoverable only as the bytes no named
field claims.

So they are computed by subtraction, carried in their own ``gaps`` array with
``"source": "gap"``, and never placed in ``fields``. A derived span does not
sit in the same list as a measurement (D8), and no field row is ever given a
fabricated zero. Level-88 condition names have no storage at all: they carry
``offset: null, length: null`` in ``conditions`` and appear in no field row,
because a zero is not an honest answer for a thing with no extent (R1).

DETERMINISM
-----------
``oracle.json`` is committed and CI regenerates it and diffs byte-for-byte, so
nothing here may vary between runs: no timestamps, no absolute paths, no
iteration over an unordered set. Keys are sorted, separators are fixed, and the
file ends with exactly one newline.
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import shutil
import subprocess
import tempfile
from dataclasses import dataclass, field as dc_field
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

# Imported both ways on purpose. `discovery-bench` contains a hyphen, so this
# directory can never be a normally importable package: the test suite loads it
# by path under a legal alias, while CI runs this file directly as a script.
# The relative form is correct for the first and impossible for the second.
try:
    from . import gen_probe
except ImportError:  # executed directly, e.g. `python3 .../oracle_layouts.py`
    import sys as _sys

    _sys.path.insert(0, str(Path(__file__).resolve().parent))
    import gen_probe  # type: ignore[no-redef]

ProbePlan = gen_probe.ProbePlan
ProbeRow = gen_probe.ProbeRow
ProbeTarget = gen_probe.ProbeTarget
WINDOW_REFMOD = gen_probe.WINDOW_REFMOD
WINDOW_TABLE = gen_probe.WINDOW_TABLE

HERE = Path(__file__).resolve().parent
BENCH_ROOT = HERE.parent
CORPUS_DIR = BENCH_ROOT / "corpus"
ORACLE_PATH = BENCH_ROOT / "oracle" / "oracle.json"

SCHEMA = "relian-discovery-bench/oracle/v0.1"

#: The only values a `source` key may take anywhere in the oracle.
SOURCE_VOCABULARY = ("probe", "gap", "listing", "derived", "design-constant")

#: Offsets are 1-based (T14, D8). Stored, never converted at a boundary.
OFFSET_BASE = 1

#: Set RELIAN_REQUIRE_COBC=1 to turn "cobc is missing" from a skip into a
#: failure. CI sets it; green-by-skip is closed on day one rather than found
#: later.
REQUIRE_COBC_ENV = "RELIAN_REQUIRE_COBC"

_COBC_TIMEOUT_S = 120

# --- Route A: the symbol listing -------------------------------------------

# A listing row is `SIZE TYPE LVL REST`, where SIZE is five digits or five
# blanks (a level-88 has no size), and REST is the name plus whatever
# attributes cobc chose to append.
_ROW = re.compile(r"^(\d{5}|\s{5})\s+([A-Z0-9-]+)\s+(\d{2})\s+(\S.*?)\s*$")
_SECTION = re.compile(r"^\s+([A-Z0-9-]+ SECTION)\s*$")
_OCCURS_FIXED = re.compile(r"^OCCURS\s+(\d+)$", re.IGNORECASE)
_OCCURS_RANGE = re.compile(r"^OCCURS\s+(\d+)\s+TO\s+(\d+)$", re.IGNORECASE)
_REDEFINES = re.compile(r"^REDEFINES\s+([A-Za-z0-9-]+)$", re.IGNORECASE)


class OracleError(RuntimeError):
    """A measurement did not happen, or happened and disagreed with itself."""


@dataclass
class ListingEntry:
    """One row of ``cobc``'s ``-ftsymbols`` table."""

    level: int
    name: str
    kind: str                      # GROUP / ALPHANUMERIC / NUMERIC / ...
    size: Optional[int]            # None for a level-88
    picture: Optional[str]
    occurs: Optional[int]          # max occurrences, or None
    occurs_min: Optional[int]      # set only for OCCURS ... DEPENDING ON
    redefines: Optional[str]
    order: int

    @property
    def is_condition(self) -> bool:
        return self.level == 88

    @property
    def is_rename(self) -> bool:
        return self.level == 66

    @property
    def is_group(self) -> bool:
        return self.kind == "GROUP"

    @property
    def is_filler(self) -> bool:
        return self.name == "FILLER"


def parse_listing(text: str) -> List[ListingEntry]:
    """Rows of the WORKING-STORAGE symbol table, in declaration order.

    Only the symbol table is read; the source echo above it is ignored. Parsing
    starts at the ``SIZE  TYPE  LVL  NAME  PICTURE`` header and stops at the
    compilation summary.
    """
    entries: List[ListingEntry] = []
    in_table = False
    order = 0
    for line in text.splitlines():
        if line.startswith("SIZE") and "TYPE" in line and "LVL" in line:
            in_table = True
            continue
        if not in_table:
            continue
        stripped = line.strip()
        if not stripped:
            continue
        if stripped.endswith("in compilation group"):
            break
        if _SECTION.match(line) or stripped.startswith("GnuCOBOL "):
            continue
        match = _ROW.match(line)
        if not match:
            continue
        size_raw, kind, level_raw, rest = match.groups()
        name, picture, attributes = _split_rest(rest, kind)
        occurs = occurs_min = None
        redefines = None
        for attribute in attributes:
            fixed = _OCCURS_FIXED.match(attribute)
            ranged = _OCCURS_RANGE.match(attribute)
            redef = _REDEFINES.match(attribute)
            if ranged:
                occurs_min, occurs = int(ranged.group(1)), int(ranged.group(2))
            elif fixed:
                occurs = int(fixed.group(1))
            elif redef:
                redefines = redef.group(1).upper()
        entries.append(
            ListingEntry(
                level=int(level_raw),
                name=name.upper(),
                kind=kind,
                size=None if size_raw.isspace() else int(size_raw),
                picture=picture,
                occurs=occurs,
                occurs_min=occurs_min,
                redefines=redefines,
                order=order,
            )
        )
        order += 1
    return entries


def _split_rest(rest: str, kind: str) -> Tuple[str, Optional[str], List[str]]:
    """Split a listing row's tail into ``(name, picture, attributes)``.

    Two shapes occur and they are not distinguishable by a single rule, so both
    are handled explicitly:

        ``D08-DATE-SHORT                 X(04), REDEFINES D08-DATE``
            name, then two-or-more spaces, then a comma-separated blob whose
            first element is the PICTURE.
        ``D08-KEY-R, REDEFINES D08-KEY``
            a GROUP has no PICTURE, so the attributes hang directly off the
            name with no column gap at all.
    """
    head, _, tail = rest.partition("  ")
    head = head.strip()
    tail = tail.strip()
    head_name, _, head_attrs = head.partition(",")
    attributes = [a.strip() for a in head_attrs.split(",") if a.strip()]
    picture: Optional[str] = None
    if tail:
        parts = [p.strip() for p in tail.split(",")]
        # `ZZZ,ZZ9.99` and `$$,$$9.99CR` contain a comma that is part of the
        # PICTURE, not an attribute separator. Re-join leading parts until the
        # remainder starts with a word the attribute grammar knows.
        while len(parts) > 1 and not re.match(
            r"^(OCCURS|REDEFINES)\b", parts[1], re.IGNORECASE
        ):
            parts[0:2] = [f"{parts[0]},{parts[1]}"]
        if kind == "GROUP":
            attributes.extend(p for p in parts if p)
        else:
            picture = parts[0] or None
            attributes.extend(p for p in parts[1:] if p)
    return head_name.strip(), picture, attributes


# --- The declaration tree ---------------------------------------------------


@dataclass
class Node:
    """A listing entry placed in the level hierarchy."""

    entry: ListingEntry
    parent: Optional["Node"] = None
    children: List["Node"] = dc_field(default_factory=list)
    conditions: List[ListingEntry] = dc_field(default_factory=list)

    @property
    def is_elementary(self) -> bool:
        return not self.children and not self.entry.is_group

    @property
    def ancestors(self) -> List["Node"]:
        out: List[Node] = []
        current = self.parent
        while current is not None:
            out.append(current)
            current = current.parent
        return out

    @property
    def redefines_anywhere(self) -> bool:
        """True if this node or any ancestor redefines something."""
        if self.entry.redefines:
            return True
        return any(a.entry.redefines for a in self.ancestors)

    def dimensions(self) -> List[Tuple["Node", int]]:
        """Enclosing OCCURS, outermost first, each with its max count."""
        chain = [n for n in reversed(self.ancestors) if n.entry.occurs]
        if self.entry.occurs:
            chain.append(self)
        return [(n, n.entry.occurs or 0) for n in chain]


def build_tree(entries: Sequence[ListingEntry]) -> Tuple[Node, List[Node]]:
    """Return ``(root, renames)`` from a flat listing.

    Level-88 rows attach to the field above them. Level-66 rows are siblings of
    the record, not members of it, so they are returned separately.
    """
    root: Optional[Node] = None
    renames: List[Node] = []
    stack: List[Node] = []
    last_field: Optional[Node] = None
    for entry in entries:
        if entry.is_condition:
            if last_field is None:
                raise OracleError(
                    f"level-88 {entry.name} appears before any field to "
                    f"condition; the listing is not in declaration order"
                )
            last_field.conditions.append(entry)
            continue
        if entry.is_rename:
            renames.append(Node(entry))
            continue
        node = Node(entry)
        if entry.level == 1:
            if root is not None:
                raise OracleError(
                    f"a second 01-level record {entry.name} appeared; a "
                    f"discovery-bench copybook declares exactly one"
                )
            root = node
            stack = [node]
        else:
            while stack and stack[-1].entry.level >= entry.level:
                stack.pop()
            if not stack:
                raise OracleError(f"{entry.name}: no enclosing group")
            node.parent = stack[-1]
            stack[-1].children.append(node)
            stack.append(node)
        last_field = node
    if root is None:
        raise OracleError("listing declared no 01-level record")
    return root, renames


def iter_nodes(root: Node) -> List[Node]:
    """Every node in declaration order, root first."""
    out: List[Node] = []

    def walk(node: Node) -> None:
        out.append(node)
        for child in node.children:
            walk(child)

    walk(root)
    return out


# --- Running cobc -----------------------------------------------------------


def cobc_available() -> bool:
    return shutil.which("cobc") is not None


def require_cobc() -> None:
    """Raise when ``cobc`` is missing and the environment demands it.

    ``RELIAN_REQUIRE_COBC=1`` converts what would be a skip into a failure. A
    layout oracle that quietly did not run is not a passing one.
    """
    if cobc_available():
        return
    if os.environ.get(REQUIRE_COBC_ENV) == "1":
        raise OracleError(
            "cobc is not on PATH and " + REQUIRE_COBC_ENV + "=1, so this run "
            "must fail rather than skip. The oracle is a measurement; an "
            "unmeasured oracle is not a passing one."
        )
    raise OracleError("cobc is not on PATH")


def cobc_version() -> str:
    """The first line of ``cobc --version``, verbatim."""
    require_cobc()
    proc = subprocess.run(
        ["cobc", "--version"], capture_output=True, text=True,
        timeout=_COBC_TIMEOUT_S,
    )
    for stream in (proc.stdout, proc.stderr):
        for line in (stream or "").splitlines():
            if line.strip():
                return line.strip()
    raise OracleError("cobc --version produced no output")


@dataclass
class CompileResult:
    returncode: int
    stdout: str
    stderr: str

    @property
    def warnings(self) -> List[str]:
        return [
            line for line in self.stderr.splitlines() if "warning:" in line
        ]


def _run_cobc(args: Sequence[str], cwd: Path) -> CompileResult:
    proc = subprocess.run(
        ["cobc", *args], cwd=str(cwd), capture_output=True, text=True,
        timeout=_COBC_TIMEOUT_S,
    )
    # `-Warchaic` on a figurative-constant MOVE to a numeric item is EXPECTED
    # (T13) and is not a failure. It is neither suppressed globally nor
    # promoted to an error: warnings are carried through and reported.
    return CompileResult(proc.returncode, proc.stdout, proc.stderr)


def route_a(copybook: Path, workdir: Path) -> Tuple[List[ListingEntry], str]:
    """Compile a bare driver and return ``(listing entries, listing text)``."""
    require_cobc()
    stem = copybook.stem
    driver = workdir / f"a_{stem}.cbl"
    listing = workdir / f"a_{stem}.lst"
    driver.write_text(
        "       IDENTIFICATION DIVISION.\n"
        f"       PROGRAM-ID. A{_program_suffix(stem)}.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        f'           COPY "{copybook.name}".\n'
        "       PROCEDURE DIVISION.\n"
        "           GOBACK.\n",
        encoding="utf-8",
    )
    result = _run_cobc(
        ["-x", "-t", listing.name, "-ftsymbols", "-I", str(copybook.parent),
         driver.name],
        workdir,
    )
    if result.returncode != 0 or not listing.is_file():
        raise OracleError(
            f"Route A failed for {copybook.name} (exit {result.returncode}). "
            f"The copybook is the defect, not the harness -- fix it before "
            f"sealing.\n{result.stderr.strip()}"
        )
    text = listing.read_text(encoding="utf-8", errors="replace")
    entries = parse_listing(text)
    if not entries:
        raise OracleError(
            f"Route A produced no symbol rows for {copybook.name}; the "
            f"listing format is not what this parser expects"
        )
    return entries, text


def route_b(plan: ProbePlan, copybook: Path, workdir: Path) -> Tuple[
    Dict[int, List[ProbeRow]], Dict[int, int], str, List[str]
]:
    """Emit, compile and run the probe. Returns rows, lengths, source, warnings."""
    require_cobc()
    source = gen_probe.build(plan)
    program = workdir / f"b_{copybook.stem}.cbl"
    program.write_text(source, encoding="utf-8")
    binary = workdir / f"b_{copybook.stem}"
    # -debug enables run-time checking, so an out-of-range subscript aborts
    # loudly instead of reading whatever happens to be next in memory. A probe
    # that silently reads past its group would produce plausible numbers.
    result = _run_cobc(
        ["-x", "-debug", "-o", binary.name, "-I", str(copybook.parent),
         program.name],
        workdir,
    )
    if result.returncode != 0:
        raise OracleError(
            f"Route B failed to compile for {copybook.name} "
            f"(exit {result.returncode}).\n{result.stderr.strip()}"
        )
    run = subprocess.run(
        [str(binary)], cwd=str(workdir), capture_output=True, text=True,
        timeout=_COBC_TIMEOUT_S,
    )
    if run.returncode != 0:
        raise OracleError(
            f"Route B probe for {copybook.name} exited {run.returncode}:\n"
            f"{run.stderr.strip()}"
        )
    rows, lengths = gen_probe.parse_output(run.stdout)
    return rows, lengths, source, result.warnings


def _program_suffix(stem: str) -> str:
    """A COBOL-safe PROGRAM-ID suffix derived from a file stem."""
    return re.sub(r"[^A-Za-z0-9]", "", stem).upper()[:20] or "X"


# --- Planning the probe -----------------------------------------------------


def plan_for(
    copybook: Path, entries: Sequence[ListingEntry]
) -> Tuple[ProbePlan, Node, List[Node]]:
    """Turn Route A's listing into a probe plan."""
    root, renames = build_tree(entries)
    text = copybook.read_text(encoding="utf-8")
    odo_field = gen_probe.find_odo_field(text)
    total = root.entry.size
    if total is None:
        raise OracleError(f"{copybook.name}: Route A gave the 01 group no SIZE")

    targets: List[ProbeTarget] = []
    odo_requires: Dict[str, int] = {}
    odo_node = _odo_node(root)

    for node in iter_nodes(root) + renames:
        if node.entry.is_filler:
            # Not probeable: FILLER cannot receive a MOVE. Recovered as a gap.
            continue
        dims = node.dimensions()
        odo_index = next(
            (i for i, (n, _) in enumerate(dims) if n is odo_node), None
        )
        for subscripts in gen_probe.expand_occurrences([c for _, c in dims]):
            target = ProbeTarget(node.entry.name, subscripts)
            targets.append(target)
            if odo_index is not None:
                odo_requires[target.key] = subscripts[odo_index]

    if odo_node is not None:
        if odo_field is None:
            raise OracleError(
                f"{copybook.name}: Route A reports OCCURS "
                f"{odo_node.entry.occurs_min} TO {odo_node.entry.occurs} but "
                f"no DEPENDING ON clause was found in the source"
            )
        window = WINDOW_REFMOD
        odo_values = (odo_node.entry.occurs_min or 1, odo_node.entry.occurs or 1)
    else:
        window = WINDOW_TABLE
        odo_values = ()
        odo_field = None

    plan = ProbePlan(
        program_id=f"B{_program_suffix(copybook.stem)}",
        copybook_file=copybook.name,
        group=root.entry.name,
        total_len=total,
        targets=targets,
        window=window,
        odo_field=odo_field,
        odo_values=odo_values,
        odo_requires=odo_requires,
    )
    return plan, root, renames


def _odo_node(root: Node) -> Optional[Node]:
    odo = [n for n in iter_nodes(root) if n.entry.occurs_min is not None]
    if len(odo) > 1:
        raise OracleError(
            "more than one OCCURS DEPENDING ON in a single copybook; the "
            "corpus keeps one axis per file"
        )
    return odo[0] if odo else None


def _expected_element_size(node: Node) -> int:
    """Route A's SIZE for ONE occurrence of ``node``.

    A GROUP row's SIZE covers all of its own occurrences; an ELEMENTARY row's
    SIZE covers one. Measured, not assumed -- see the module docstring.
    """
    size = node.entry.size
    if size is None:
        raise OracleError(f"{node.entry.name}: listing gave no SIZE")
    if node.entry.is_group and node.entry.occurs:
        count = node.entry.occurs
        if size % count:
            raise OracleError(
                f"{node.entry.name}: listing SIZE {size} is not divisible by "
                f"its OCCURS {count}, so per-occurrence size cannot be derived"
            )
        return size // count
    return size


# --- Assembling one copybook's oracle --------------------------------------


@dataclass
class Agreement:
    checked: int = 0
    mismatches: List[str] = dc_field(default_factory=list)


def build_copybook(copybook: Path, workdir: Path) -> Tuple[Dict[str, Any], Agreement]:
    """Route A, then Route B, then cross-check, then derive."""
    entries, _listing_text = route_a(copybook, workdir)
    plan, root, renames = plan_for(copybook, entries)
    rows_by_odo, lengths_by_odo, _source, _warnings = route_b(
        plan, copybook, workdir
    )

    by_name: Dict[str, Node] = {
        n.entry.name: n for n in iter_nodes(root) + renames
    }
    agreement = Agreement()
    variants: List[Dict[str, Any]] = []

    for odo_value in sorted(rows_by_odo):
        rows = {r.key: r for r in rows_by_odo[odo_value]}
        group_length = lengths_by_odo[odo_value]
        fields: List[Dict[str, Any]] = []

        for row in rows_by_odo[odo_value]:
            name, subscripts = _split_key(row.key)
            node = by_name[name]
            if not row.contiguous:
                raise OracleError(
                    f"{copybook.name}: {row.key} occupies bytes "
                    f"{row.first}..{row.last} but only {row.count} of them "
                    f"were marked. The field's storage is not one contiguous "
                    f"run; that is a finding about GnuCOBOL 3.1.2, not a "
                    f"number to record as a length."
                )
            expected = _expected_element_size(node)
            # The 01 record of an ODO copybook has no single length: Route A
            # reports the MAXIMUM. Comparing the min-occurrence probe against
            # it would be a gate firing on correct behaviour, so at any ODO
            # value below the maximum the row is cross-checked against the
            # runtime group length instead -- which is still a Route A/Route B
            # agreement, just the honest one for a variable extent.
            variable_extent = (
                node is root
                and plan.odo_field is not None
                and odo_value != max(plan.odo_values)
            )
            agreement.checked += 1
            if variable_extent:
                if row.length != group_length:
                    agreement.mismatches.append(
                        f"{row.key} at ODO {odo_value}: probe length "
                        f"{row.length} != runtime group length {group_length}"
                    )
            elif row.length != expected:
                agreement.mismatches.append(
                    f"{row.key}: probe length {row.length} != listing size "
                    f"{expected}"
                )
            fields.append(
                {
                    "key": row.key,
                    "name": name,
                    "subscripts": list(subscripts),
                    "elementary": node.is_elementary,
                    "redefines": node.entry.redefines,
                    "renames": node.entry.is_rename,
                    "in_tiling": _in_tiling(node),
                    "offset": row.offset,
                    "length": row.length,
                    "marked_bytes": row.count,
                    "source": "probe",
                    "listing": {
                        "level": node.entry.level,
                        "size": node.entry.size,
                        "element_size": expected,
                        "occurs": node.entry.occurs,
                        "occurs_min": node.entry.occurs_min,
                        "picture": node.entry.picture,
                        "category": node.entry.kind,
                        "source": "listing",
                    },
                }
            )

        # The 01 group's own probe row must agree with the runtime group
        # length, which must agree with Route A when the group is fixed.
        listing_group_size = root.entry.size
        at_max = plan.odo_field is None or odo_value == max(plan.odo_values)
        agreement.checked += 1
        if at_max and group_length != listing_group_size:
            agreement.mismatches.append(
                f"group length {group_length} != listing SIZE "
                f"{listing_group_size}"
            )
        if not at_max and group_length >= listing_group_size:
            agreement.mismatches.append(
                f"ODO {odo_value}: runtime group length {group_length} is not "
                f"shorter than the maximum {listing_group_size}, so the "
                f"DEPENDING ON clause changed nothing"
            )

        gaps = _derive_gaps(fields, group_length, copybook.name)
        _check_tiling(fields, gaps, group_length, copybook.name)
        _check_aliases(fields, rows, group_length, copybook.name)

        conditions = [
            {
                "name": condition.name,
                "host": node.entry.name,
                "level": condition.level,
                "offset": None,
                "length": None,
                "source": "listing",
            }
            for node in iter_nodes(root)
            for condition in node.conditions
        ]

        variants.append(
            {
                "odo_value": None if odo_value == 0 else odo_value,
                "group_length": group_length,
                "source": "probe",
                "listing": {
                    "group_size": listing_group_size,
                    "source": "listing",
                },
                "fields": fields,
                "gaps": gaps,
                "conditions": conditions,
                "counts": {
                    "fields": len(fields),
                    "tiling_fields": sum(1 for f in fields if f["in_tiling"]),
                    "gaps": len(gaps),
                    "gap_bytes": sum(g["length"] for g in gaps),
                    "conditions": len(conditions),
                    "source": "derived",
                },
            }
        )

    return (
        {
            "file": copybook.name,
            "sha256": hashlib.sha256(copybook.read_bytes()).hexdigest(),
            "group": root.entry.name,
            "variable_length": plan.odo_field is not None,
            "odo_field": plan.odo_field,
            "window": plan.window,
            "variants": variants,
        },
        agreement,
    )


def _split_key(key: str) -> Tuple[str, Tuple[int, ...]]:
    name, _, subs = key.partition(" (")
    if not subs:
        return name, ()
    return name, tuple(int(s) for s in subs.rstrip(")").split(","))


def _in_tiling(node: Node) -> bool:
    """Membership of the D9 projection: named, elementary, non-aliasing."""
    return (
        node.is_elementary
        and not node.entry.is_filler
        and not node.entry.is_rename
        and not node.redefines_anywhere
    )


def _derive_gaps(
    fields: Sequence[Dict[str, Any]], group_length: int, where: str
) -> List[Dict[str, Any]]:
    """Bytes inside the group that no tiling field claims.

    FILLER and SYNCHRONIZED slack land here. Derived by subtraction and
    labelled as such -- never presented alongside a measurement.
    """
    claimed = bytearray(group_length + 1)
    for row in fields:
        if not row["in_tiling"]:
            continue
        for index in range(row["offset"], row["offset"] + row["length"]):
            if index > group_length:
                raise OracleError(
                    f"{where}: {row['key']} extends to byte {index}, past the "
                    f"group length {group_length}"
                )
            claimed[index] += 1

    gaps: List[Dict[str, Any]] = []
    start: Optional[int] = None
    for index in range(1, group_length + 1):
        if claimed[index] == 0 and start is None:
            start = index
        elif claimed[index] != 0 and start is not None:
            gaps.append(
                {"offset": start, "length": index - start, "source": "gap"}
            )
            start = None
    if start is not None:
        gaps.append(
            {"offset": start, "length": group_length - start + 1,
             "source": "gap"}
        )
    return gaps


def _check_tiling(
    fields: Sequence[Dict[str, Any]],
    gaps: Sequence[Dict[str, Any]],
    group_length: int,
    where: str,
) -> None:
    """D9: the projection plus gaps covers [1, group_length] exactly once."""
    cover = bytearray(group_length + 1)
    for row in fields:
        if not row["in_tiling"]:
            continue
        for index in range(row["offset"], row["offset"] + row["length"]):
            cover[index] += 1
    for gap in gaps:
        for index in range(gap["offset"], gap["offset"] + gap["length"]):
            cover[index] += 1

    overlaps = [i for i in range(1, group_length + 1) if cover[i] > 1]
    uncovered = [i for i in range(1, group_length + 1) if cover[i] == 0]
    if overlaps or uncovered:
        raise OracleError(
            f"{where}: the D9 tiling invariant failed over named, elementary, "
            f"non-redefining, non-renaming fields plus gap spans. "
            f"{len(overlaps)} byte(s) covered more than once "
            f"(first {overlaps[:5]}), {len(uncovered)} byte(s) not covered "
            f"(first {uncovered[:5]}), group length {group_length}. Stop: "
            f"either the copybook is not what it looks like or the gap "
            f"derivation is wrong, and both are worth finding before sealing."
        )


def _check_aliases(
    fields: Sequence[Dict[str, Any]],
    rows: Dict[str, ProbeRow],
    group_length: int,
    where: str,
) -> None:
    """Redefining and renaming fields, asserted separately from the tiling.

    A REDEFINES lies wholly within the extent of what it redefines. A level-66
    RENAMES does NOT -- ``RENAMES A THRU B`` is deliberately longer than ``A``
    -- so it is asserted to START at what it renames and to stay inside the
    record.
    """
    by_key = {row["key"]: row for row in fields}
    for row in fields:
        if row["renames"]:
            if row["offset"] < 1 or row["offset"] + row["length"] - 1 > group_length:
                raise OracleError(
                    f"{where}: level-66 {row['key']} spans "
                    f"{row['offset']}..{row['offset'] + row['length'] - 1}, "
                    f"outside the record [1, {group_length}]"
                )
            continue
        target_name = row["redefines"]
        if not target_name:
            continue
        target = by_key.get(target_name)
        if target is None:
            raise OracleError(
                f"{where}: {row['key']} redefines {target_name}, which has no "
                f"probe row"
            )
        start_ok = row["offset"] >= target["offset"]
        end_ok = (
            row["offset"] + row["length"]
            <= target["offset"] + target["length"]
        )
        if not (start_ok and end_ok):
            raise OracleError(
                f"{where}: {row['key']} spans "
                f"{row['offset']}..{row['offset'] + row['length'] - 1} but "
                f"redefines {target_name}, which spans "
                f"{target['offset']}..{target['offset'] + target['length'] - 1}"
                f" -- a redefinition must lie wholly within its target"
            )


# --- The whole oracle -------------------------------------------------------


def corpus_files(corpus_dir: Path = CORPUS_DIR) -> List[Path]:
    """Every copybook, sorted by POSIX-string filename for determinism."""
    return sorted(corpus_dir.glob("*.cpy"), key=lambda p: p.name)


def build_oracle(
    corpus_dir: Path = CORPUS_DIR, workdir: Optional[Path] = None
) -> Dict[str, Any]:
    """Measure every copybook and assemble the oracle document."""
    require_cobc()
    files = corpus_files(corpus_dir)
    if not files:
        raise OracleError(f"no copybooks found under {corpus_dir}")

    context = (
        tempfile.TemporaryDirectory() if workdir is None else None
    )
    base = Path(context.name) if context is not None else workdir
    try:
        copybooks: List[Dict[str, Any]] = []
        agreements: Dict[str, Agreement] = {}
        for copybook in files:
            entry, agreement = build_copybook(copybook, base)
            copybooks.append(entry)
            agreements[copybook.name] = agreement
    finally:
        if context is not None:
            context.cleanup()

    mismatches = {
        name: a.mismatches for name, a in agreements.items() if a.mismatches
    }
    if mismatches:
        raise OracleError(
            "Route A and Route B disagree, and the tolerance is zero:\n"
            + "\n".join(
                f"  {name}: {'; '.join(items)}"
                for name, items in sorted(mismatches.items())
            )
        )

    document = {
        "schema": SCHEMA,
        "benchmark": "RELIAN-DISCOVERY-BENCH",
        "version": "0.1.0",
        "conventions": {
            "offset_base": OFFSET_BASE,
            "note": (
                "Offsets are 1-based, as measured (WS-CW-ACCOUNT = 1, "
                "WS-CW-NAME = 11). Lengths are byte counts. A level-88 has "
                "no storage and carries null, never zero."
            ),
            "source": "design-constant",
        },
        "source_vocabulary": {
            "probe": "measured directly by the Route B runtime probe",
            "listing": "read from cobc's -ftsymbols symbol table (Route A)",
            "gap": (
                "DERIVED by subtraction, not measured. FILLER and SYNC slack "
                "cannot receive a MOVE and are recoverable only as the bytes "
                "no named field claims."
            ),
            "derived": "computed from other rows in this document",
            "design-constant": "a fixed convention of the format, not a measurement",
        },
        "toolchain": {"cobc": cobc_version()},
        "agreement": {
            "route_a_vs_route_b_checks": sum(
                a.checked for a in agreements.values()
            ),
            "mismatches": 0,
            "tolerance": 0,
            "per_copybook": {
                name: {"checks": a.checked, "mismatches": len(a.mismatches),
                       "source": "derived"}
                for name, a in sorted(agreements.items())
            },
            "source": "derived",
        },
        "copybooks": copybooks,
        "counts": summarise(copybooks),
    }
    return document


def summarise(copybooks: Sequence[Dict[str, Any]]) -> Dict[str, Any]:
    """The integer counts SPEC.md and the manifest both quote.

    Per-copybook figures are taken from the FIRST variant, so an ODO copybook
    is counted once rather than twice; ``probe_rows`` counts every row in every
    variant, because each is a separate measurement.
    """
    elementary = 0
    probe_rows = 0
    gap_rows = 0
    conditions = 0
    variants = 0
    for book in copybooks:
        for index, variant in enumerate(book["variants"]):
            variants += 1
            probe_rows += len(variant["fields"])
            gap_rows += len(variant["gaps"])
            if index == 0:
                elementary += sum(
                    1 for f in variant["fields"] if f["elementary"]
                )
                conditions += len(variant["conditions"])
    return {
        "copybooks": len(copybooks),
        "variants": variants,
        "elementary_fields": elementary,
        "probe_rows": probe_rows,
        "gap_rows": gap_rows,
        "conditions": conditions,
        "source": "derived",
    }


def dumps(document: Dict[str, Any]) -> str:
    """Serialise deterministically: sorted keys, two-space indent, one newline."""
    return json.dumps(document, sort_keys=True, indent=2) + "\n"


def lint(document: Dict[str, Any]) -> List[str]:
    """Every object carrying a number must say where the number came from.

    Acceptance gate ④, mechanised. The rule has no exceptions on purpose: an
    exception list is where an unsourced number eventually hides.
    """
    problems: List[str] = []

    def walk(value: Any, path: str) -> None:
        if isinstance(value, dict):
            has_number = any(
                isinstance(v, (int, float)) and not isinstance(v, bool)
                for v in value.values()
            )
            if has_number:
                source = value.get("source")
                if source is None:
                    problems.append(
                        f"{path}: object holds a numeric value but no `source`"
                    )
                elif source not in SOURCE_VOCABULARY:
                    problems.append(
                        f"{path}: source {source!r} is not one of "
                        f"{list(SOURCE_VOCABULARY)}"
                    )
            for key, item in value.items():
                walk(item, f"{path}.{key}")
        elif isinstance(value, list):
            for index, item in enumerate(value):
                if isinstance(item, (dict, list)):
                    walk(item, f"{path}[{index}]")
                elif isinstance(item, (int, float)) and not isinstance(item, bool):
                    # A bare number in a list is only acceptable inside an
                    # object that has already declared its source.
                    pass

    walk(document, "$")
    return problems


def write_oracle(path: Path = ORACLE_PATH, corpus_dir: Path = CORPUS_DIR) -> Dict[str, Any]:
    document = build_oracle(corpus_dir)
    problems = lint(document)
    if problems:
        raise OracleError(
            "the oracle failed its own source lint:\n  "
            + "\n  ".join(problems)
        )
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(dumps(document), encoding="utf-8")
    return document


def _main(argv: Optional[Sequence[str]] = None) -> int:
    import argparse

    parser = argparse.ArgumentParser(
        description="Regenerate discovery-bench/oracle/oracle.json from cobc."
    )
    parser.add_argument("--out", type=Path, default=ORACLE_PATH)
    parser.add_argument("--corpus", type=Path, default=CORPUS_DIR)
    parser.add_argument(
        "--check", action="store_true",
        help="regenerate in memory and diff against --out instead of writing",
    )
    args = parser.parse_args(argv)

    document = build_oracle(args.corpus)
    problems = lint(document)
    if problems:
        print("ORACLE LINT FAILED:")
        for problem in problems:
            print(f"  {problem}")
        return 1
    rendered = dumps(document)

    if args.check:
        if not args.out.is_file():
            print(f"FAIL: {args.out} does not exist, so there is nothing to "
                  f"diff against. An absent oracle is not a matching one.")
            return 1
        existing = args.out.read_text(encoding="utf-8")
        if existing != rendered:
            print(f"FAIL: regenerated oracle differs from {args.out}.")
            import difflib
            diff = difflib.unified_diff(
                existing.splitlines(keepends=True),
                rendered.splitlines(keepends=True),
                fromfile=str(args.out), tofile="regenerated",
            )
            print("".join(list(diff)[:120]), end="")
            return 1
        print(f"OK: {args.out} is byte-identical to a fresh regeneration.")
        counts = document["counts"]
        print(f"    copybooks {counts['copybooks']}, "
              f"elementary fields {counts['elementary_fields']}, "
              f"probe rows {counts['probe_rows']}, "
              f"gap rows {counts['gap_rows']}, "
              f"conditions {counts['conditions']}")
        return 0

    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(rendered, encoding="utf-8")
    counts = document["counts"]
    print(f"wrote {args.out}")
    print(f"  toolchain        : {document['toolchain']['cobc']}")
    print(f"  copybooks        : {counts['copybooks']}")
    print(f"  variants         : {counts['variants']}")
    print(f"  elementary fields: {counts['elementary_fields']}")
    print(f"  probe rows       : {counts['probe_rows']}")
    print(f"  gap rows         : {counts['gap_rows']}")
    print(f"  conditions       : {counts['conditions']}")
    print(f"  A<->B checks     : "
          f"{document['agreement']['route_a_vs_route_b_checks']}, "
          f"mismatches {document['agreement']['mismatches']}")
    return 0


if __name__ == "__main__":  # pragma: no cover - CLI entry point
    raise SystemExit(_main())
