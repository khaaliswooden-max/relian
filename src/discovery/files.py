"""WP-2.4 §3.2 — SELECT, FD, the three-source join, and the LRECL cross-check.

Three independent sources describe one file, and no two of them are the same
kind of fact:

===========================  ==========================================
``SELECT … ASSIGN TO``       the program's logical file → a **DD name**
``FD`` + its ``01`` record   the DD name → a **record description**
``//<dd> DD DSN=…,DCB=…``    the DD name → a **dataset and an LRECL**
===========================  ==========================================

D28: three fields, not one merged record
----------------------------------------

:class:`FileRecord` keeps ``select_evidence``, ``fd_evidence`` and
``jcl_evidence`` separate, each with a file and a line, each independently
``None``. That is not tidiness — it is the only way two specific findings stay
visible:

* a DD with a dataset that **no** ``SELECT`` in scope names — a dataset the
  job stream allocates and no program we can see declares; and
* a ``SELECT`` with **no** DD — a file the job stream never supplies.

Merge the sources into one row and both collapse into "some fields are empty",
which reads as missing data rather than as a finding.

D29: the cross-check, and why it must not resolve
-------------------------------------------------

The layout engine computes a record length from the copybook. The JCL declares
an ``LRECL`` for the dataset holding that record. Four outcomes:

``AGREE``      the two agree (after any RDW adjustment)
``DISAGREE``   both known, unequal — **both reported, neither chosen**
``NO_LRECL``   a layout with nothing external to check it against
``NO_LAYOUT``  a dataset with no record description we could compute

There is deliberately no method on :class:`CrossCheck` that returns "the"
record length, and no code path that prefers one source over the other. When a
copybook does not describe the data, that is the most expensive thing to
discover during a migration and the cheapest to discover before one; a tool
that silently reconciles the two numbers destroys the finding it exists to
produce. ``tests/test_discovery_files.py`` asserts both values survive into the
report and that no resolving accessor exists.

``RECFM=V``/``VB`` shifts the comparison by the 4-byte Record Descriptor Word,
which z/OS counts inside ``LRECL``. That adjustment is applied **and stated in
the row** (:attr:`CrossCheck.basis`), because an unexplained ±4 reads as a
defect to anyone checking the arithmetic by hand. Where ``RECFM`` is not
declared at all, no adjustment is applied and the row says so — assuming
"fixed" would silently convert an unknown into a claim.

R12: nothing here opens a dataset. Record descriptions come from COBOL source
and copybooks; lengths come from the layout engine's static computation.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Dict, List, Mapping, Optional, Sequence, Tuple

from src.assessment.intake import SKIP_DIRS, read_source

from .copybook import CodeLine, Resolution, code_lines
from .jcl import DDStatement, Evidence, JclInventory, RDW_BYTES
from .layout import Layout, LayoutStatus, compute, compute_text

#: Source extensions scanned for FILE-CONTROL and FILE SECTION.
PROGRAM_SUFFIXES: Tuple[str, ...] = (".cbl", ".cob", ".cobol", ".ccp", ".pco")


# --------------------------------------------------------------------------
# COBOL statement assembly
# --------------------------------------------------------------------------

@dataclass(frozen=True)
class _Sentence:
    """One period-terminated COBOL sentence, with the line it began on."""

    lineno: int
    text: str
    end_lineno: int


def sentences(lines: Sequence[CodeLine]) -> List[_Sentence]:
    """Join code lines into period-terminated sentences.

    A ``SELECT`` spans four or five lines in every corpus we have measured, so
    a per-line scan would find ``ASSIGN TO`` and miss ``ORGANIZATION``,
    ``ACCESS MODE`` and ``RECORD KEY`` — the same truncation shape as the JCL
    continuation trap, one artifact over.
    """
    out: List[_Sentence] = []
    buffer: List[str] = []
    start: Optional[int] = None
    last = 0

    def flush() -> None:
        nonlocal buffer, start
        joined = " ".join((" ".join(buffer)).split())
        if joined and start is not None:
            out.append(_Sentence(start, joined, last))
        buffer = []
        start = None

    for line in lines:
        text = line.text.strip()
        if not text:
            continue
        last = line.lineno
        segment: List[str] = []
        in_quote: Optional[str] = None
        index = 0
        while index < len(text):
            char = text[index]
            if in_quote is not None:
                segment.append(char)
                if char == in_quote:
                    in_quote = None
                index += 1
                continue
            if char in "'\"":
                in_quote = char
                segment.append(char)
                index += 1
                continue
            if char == "." and _terminates(text, index):
                # A period ends a COBOL sentence only when a separator follows.
                # Without that rule `ASSIGN TO 'customer.dat'` splits inside the
                # literal and the DD name comes back as `TO`; with quote
                # tracking as well, a period inside a literal cannot end a
                # sentence even at end of line.
                if start is None:
                    start = line.lineno
                segment_text = "".join(segment).strip()
                if segment_text:
                    buffer.append(segment_text)
                flush()
                segment = []
                index += 1
                continue
            segment.append(char)
            index += 1
        remainder = "".join(segment).strip()
        if remainder:
            if start is None:
                start = line.lineno
            buffer.append(remainder)
    flush()
    return out


def _terminates(text: str, index: int) -> bool:
    """True when the period at ``index`` ends a sentence.

    A period is a separator only when whitespace or end-of-text follows it.
    That is what keeps `1.5` and `customer.dat` intact; the quote tracking in
    :func:`sentences` covers the rest.
    """
    tail = text[index + 1:]
    return tail == "" or tail[:1].isspace()


# --------------------------------------------------------------------------
# SELECT
# --------------------------------------------------------------------------

@dataclass(frozen=True)
class SelectStatement:
    """One ``SELECT`` from FILE-CONTROL. Absent clauses are ``None``."""

    file_name: str
    assign_to: Optional[str]
    organization: Optional[str] = None
    access_mode: Optional[str] = None
    record_key: Optional[str] = None
    alternate_keys: Tuple[str, ...] = ()
    optional: bool = False
    file_status: Optional[str] = None
    assign_literal: bool = False
    evidence: Evidence = Evidence("<text>", 0)
    raw: str = ""

    def to_dict(self) -> Dict[str, object]:
        return {
            "file_name": self.file_name,
            "assign_to": self.assign_to,
            "organization": self.organization,
            "access_mode": self.access_mode,
            "record_key": self.record_key,
            "alternate_keys": list(self.alternate_keys),
            "optional": self.optional,
            "file_status": self.file_status,
            "assign_literal": self.assign_literal,
            "evidence": self.evidence.to_dict(),
        }


_SELECT_RE = re.compile(
    r"^SELECT\s+(?P<optional>OPTIONAL\s+)?(?P<name>[A-Za-z0-9$#@_-]+)\b",
    re.IGNORECASE,
)
_ASSIGN_RE = re.compile(
    r"\bASSIGN\s+(?:TO\s+)?(?P<target>'[^']*'|\"[^\"]*\"|[A-Za-z0-9$#@_.\-]+)",
    re.IGNORECASE,
)
_ORGANIZATION_RE = re.compile(
    r"\bORGANIZATION\s+(?:IS\s+)?(?P<value>[A-Za-z-]+)", re.IGNORECASE
)
_ACCESS_RE = re.compile(
    r"\bACCESS\s+(?:MODE\s+)?(?:IS\s+)?(?P<value>[A-Za-z-]+)", re.IGNORECASE
)
_RECORD_KEY_RE = re.compile(
    r"\bRECORD\s+KEY\s+(?:IS\s+)?(?P<value>[A-Za-z0-9$#@_-]+)", re.IGNORECASE
)
_ALT_KEY_RE = re.compile(
    r"\bALTERNATE\s+RECORD\s+KEY\s+(?:IS\s+)?(?P<value>[A-Za-z0-9$#@_-]+)",
    re.IGNORECASE,
)
_FILE_STATUS_RE = re.compile(
    r"\bFILE\s+STATUS\s+(?:IS\s+)?(?P<value>[A-Za-z0-9$#@_-]+)", re.IGNORECASE
)

#: ``ASSIGN TO`` may name a DD directly (``ASSIGN TO ACCTFILE``) or through an
#: environment-name prefix (``ASSIGN TO UT-S-ACCTFILE``, ``S-ACCTFILE``). The
#: prefix is a device/organization hint, not part of the DD name.
_ASSIGN_PREFIX_RE = re.compile(
    r"^(?:UT|UR|DA|VS|AS)?-?(?:S|I|R|D)?-(?P<dd>[A-Za-z0-9$#@]+)$", re.IGNORECASE
)


def _assign_target(raw: str) -> Tuple[Optional[str], bool]:
    """Return ``(ddname, was_literal)`` for an ``ASSIGN TO`` target.

    A quoted literal is a *filename*, not a DD name, and is reported as such
    rather than being join-matched against the job stream by accident.
    """
    literal = raw[:1] in {"'", '"'}
    value = raw.strip("'\"").strip()
    if literal:
        return value, True
    match = _ASSIGN_PREFIX_RE.match(value)
    if match:
        return match.group("dd").upper(), False
    return value.upper(), False


def parse_file_control(text: str, path: str, *, fixed_format: bool = True) -> Tuple[SelectStatement, ...]:
    """Extract every ``SELECT`` in ``text``'s FILE-CONTROL paragraph."""
    out: List[SelectStatement] = []
    for sentence in sentences(code_lines(text, fixed_format=fixed_format)):
        body = sentence.text
        match = _SELECT_RE.match(body)
        if not match:
            continue
        assign_match = _ASSIGN_RE.search(body)
        assign_to: Optional[str] = None
        assign_literal = False
        if assign_match:
            assign_to, assign_literal = _assign_target(assign_match.group("target"))
        organization = _first(_ORGANIZATION_RE, body)
        access = _first(_ACCESS_RE, body)
        out.append(SelectStatement(
            file_name=match.group("name").upper(),
            assign_to=assign_to,
            organization=organization.upper() if organization else None,
            access_mode=access.upper() if access else None,
            record_key=_upper_or_none(_first(_RECORD_KEY_RE, body)),
            alternate_keys=tuple(
                m.group("value").upper() for m in _ALT_KEY_RE.finditer(body)
            ),
            optional=bool(match.group("optional")),
            file_status=_upper_or_none(_first(_FILE_STATUS_RE, body)),
            assign_literal=assign_literal,
            evidence=Evidence(path, sentence.lineno),
            raw=body,
        ))
    return tuple(out)


def _first(pattern: re.Pattern, text: str) -> Optional[str]:
    match = pattern.search(text)
    return match.group("value") if match else None


def _upper_or_none(value: Optional[str]) -> Optional[str]:
    return value.upper() if value else None


# --------------------------------------------------------------------------
# FD
# --------------------------------------------------------------------------

@dataclass(frozen=True)
class FdEntry:
    """One ``FD`` and the ``01`` record description(s) under it."""

    file_name: str
    record_names: Tuple[str, ...] = ()
    recording_mode: Optional[str] = None
    record_contains: Optional[int] = None
    is_sort: bool = False
    copy_members: Tuple[str, ...] = ()
    evidence: Evidence = Evidence("<text>", 0)
    record_source: str = ""
    record_first_line: Optional[int] = None

    def to_dict(self) -> Dict[str, object]:
        return {
            "file_name": self.file_name,
            "record_names": list(self.record_names),
            "recording_mode": self.recording_mode,
            "record_contains": self.record_contains,
            "is_sort": self.is_sort,
            "copy_members": list(self.copy_members),
            "evidence": self.evidence.to_dict(),
            "record_first_line": self.record_first_line,
        }


_FD_RE = re.compile(r"^(?P<kind>FD|SD)\s+(?P<name>[A-Za-z0-9$#@_-]+)", re.IGNORECASE)
_LEVEL_RE = re.compile(r"^(?P<level>0?[0-9]{1,2})\s+(?P<name>[A-Za-z0-9$#@_-]+)")
_RECORDING_RE = re.compile(
    r"\bRECORDING\s+(?:MODE\s+)?(?:IS\s+)?(?P<value>[A-Za-z]+)", re.IGNORECASE
)
_RECORD_CONTAINS_RE = re.compile(
    r"\bRECORD\s+CONTAINS\s+(?P<value>\d+)", re.IGNORECASE
)
_COPY_RE = re.compile(r"\bCOPY\s+(?P<name>[A-Za-z0-9$#@_-]+)", re.IGNORECASE)
_SECTION_RE = re.compile(
    r"^(WORKING-STORAGE|LOCAL-STORAGE|LINKAGE|COMMUNICATION|REPORT|SCREEN)\s+SECTION",
    re.IGNORECASE,
)
_DIVISION_RE = re.compile(r"^(PROCEDURE|DATA|ENVIRONMENT|IDENTIFICATION)\s+DIVISION",
                         re.IGNORECASE)


def parse_file_section(
    text: str, path: str, *, fixed_format: bool = True
) -> Tuple[FdEntry, ...]:
    """Extract every ``FD``/``SD`` and the ``01`` records described under it.

    The raw source lines of each record description are kept
    (:attr:`FdEntry.record_source`) so the layout engine can compute over the
    *program's own* bytes when the record is declared inline rather than in a
    copybook.
    """
    raw_lines = text.replace("\r\n", "\n").replace("\r", "\n").split("\n")
    lines = code_lines(text, fixed_format=fixed_format)
    entries: List[FdEntry] = []

    current: Optional[Dict[str, object]] = None
    in_file_section = False

    def flush() -> None:
        if current is None:
            return
        first = current["record_first_line"]
        last = current["record_last_line"]
        source = ""
        if first is not None and last is not None:
            source = "\n".join(raw_lines[first - 1:last])
        entries.append(FdEntry(
            file_name=str(current["file_name"]),
            record_names=tuple(current["record_names"]),          # type: ignore[arg-type]
            recording_mode=current["recording_mode"],             # type: ignore[arg-type]
            record_contains=current["record_contains"],           # type: ignore[arg-type]
            is_sort=bool(current["is_sort"]),
            copy_members=tuple(current["copy_members"]),          # type: ignore[arg-type]
            evidence=Evidence(path, int(current["lineno"])),      # type: ignore[arg-type]
            record_source=source,
            record_first_line=first,                              # type: ignore[arg-type]
        ))

    for line in lines:
        body = line.text.strip()
        if not body:
            continue
        upper = body.upper()
        if upper.startswith("FILE SECTION"):
            in_file_section = True
            continue
        if _SECTION_RE.match(upper) or _DIVISION_RE.match(upper):
            flush()
            current = None
            in_file_section = False
            continue
        if not in_file_section:
            continue

        fd_match = _FD_RE.match(body)
        if fd_match:
            flush()
            recording = _first(_RECORDING_RE, body)
            contains = _first(_RECORD_CONTAINS_RE, body)
            current = {
                "file_name": fd_match.group("name").upper(),
                "record_names": [],
                "recording_mode": recording.upper() if recording else None,
                "record_contains": int(contains) if contains else None,
                "is_sort": fd_match.group("kind").upper() == "SD",
                "copy_members": [],
                "lineno": line.lineno,
                "record_first_line": None,
                "record_last_line": None,
            }
            continue

        if current is None:
            continue

        # Clauses can trail onto continuation lines of the FD itself.
        if current["record_first_line"] is None:
            if current["recording_mode"] is None:
                recording = _first(_RECORDING_RE, body)
                if recording:
                    current["recording_mode"] = recording.upper()
            if current["record_contains"] is None:
                contains = _first(_RECORD_CONTAINS_RE, body)
                if contains:
                    current["record_contains"] = int(contains)

        copy_match = _COPY_RE.search(body)
        if copy_match:
            members = current["copy_members"]
            assert isinstance(members, list)
            members.append(copy_match.group("name").upper().strip("'\""))
            if current["record_first_line"] is None:
                current["record_first_line"] = line.lineno
            current["record_last_line"] = line.lineno
            continue

        level_match = _LEVEL_RE.match(body)
        if level_match:
            level = int(level_match.group("level"))
            if level == 1:
                names = current["record_names"]
                assert isinstance(names, list)
                names.append(level_match.group("name").upper())
                if current["record_first_line"] is None:
                    current["record_first_line"] = line.lineno
            if current["record_first_line"] is not None:
                current["record_last_line"] = line.lineno

    flush()
    return tuple(entries)


# --------------------------------------------------------------------------
# Programs
# --------------------------------------------------------------------------

_PROGRAM_ID_RE = re.compile(
    r"\bPROGRAM-ID\s*\.\s*(?P<name>[A-Za-z0-9$#@_-]+)", re.IGNORECASE
)
_OPEN_RE = re.compile(
    r"\bOPEN\s+(?P<rest>.+)$", re.IGNORECASE
)


@dataclass(frozen=True)
class ProgramFiles:
    """Everything one program says about files."""

    program: str
    path: str
    selects: Tuple[SelectStatement, ...] = ()
    fds: Tuple[FdEntry, ...] = ()
    opens: Tuple["OpenStatement", ...] = ()
    has_file_control: bool = False

    def to_dict(self) -> Dict[str, object]:
        return {
            "program": self.program,
            "path": self.path,
            "has_file_control": self.has_file_control,
            "selects": [s.to_dict() for s in self.selects],
            "fds": [f.to_dict() for f in self.fds],
            "opens": [o.to_dict() for o in self.opens],
        }


@dataclass(frozen=True)
class OpenStatement:
    """One ``OPEN`` mode applied to one file. Feeds §3.3 lineage."""

    file_name: str
    mode: str                     # INPUT | OUTPUT | I-O | EXTEND
    evidence: Evidence

    def to_dict(self) -> Dict[str, object]:
        return {
            "file_name": self.file_name,
            "mode": self.mode,
            "evidence": self.evidence.to_dict(),
        }


_OPEN_MODES = ("INPUT", "OUTPUT", "I-O", "EXTEND")


def parse_opens(
    text: str, path: str, *, fixed_format: bool = True
) -> Tuple[OpenStatement, ...]:
    """Extract ``OPEN <mode> <file>…`` pairs.

    One ``OPEN`` may carry several modes and several files per mode
    (``OPEN INPUT A B OUTPUT C``), and the mode in force applies until the next
    mode keyword. Attributing every file to the first mode is a direction error
    on a data-flow edge, which is worse than no edge at all.
    """
    out: List[OpenStatement] = []
    for sentence in sentences(code_lines(text, fixed_format=fixed_format)):
        match = _OPEN_RE.search(sentence.text)
        if not match:
            continue
        tokens = match.group("rest").replace(",", " ").split()
        mode: Optional[str] = None
        for token in tokens:
            upper = token.upper()
            if upper in _OPEN_MODES:
                mode = upper
                continue
            if upper in {"REVERSED", "WITH", "NO", "REWIND", "LOCK"}:
                continue
            if mode is None:
                continue
            if not re.match(r"^[A-Za-z][A-Za-z0-9$#@_-]*$", token):
                break
            out.append(OpenStatement(
                file_name=upper, mode=mode, evidence=Evidence(path, sentence.lineno)
            ))
    return tuple(out)


def parse_program(path: Path, root: Optional[Path] = None) -> ProgramFiles:
    """Read one program and extract its SELECT / FD / OPEN facts."""
    path = Path(path)
    rel = path.as_posix()
    if root is not None:
        try:
            rel = path.relative_to(root).as_posix()
        except ValueError:
            rel = path.as_posix()
    text = read_source(path)
    match = _PROGRAM_ID_RE.search(text)
    name = match.group("name").upper() if match else path.stem.upper()
    return ProgramFiles(
        program=name,
        path=rel,
        selects=parse_file_control(text, rel),
        fds=parse_file_section(text, rel),
        opens=parse_opens(text, rel),
        has_file_control="FILE-CONTROL" in text.upper(),
    )


def scan_programs(root: Path) -> Tuple[ProgramFiles, ...]:
    """Parse every COBOL program under ``root``, sorted by posix path (R8)."""
    root = Path(root)
    if not root.exists():
        raise FileNotFoundError(f"program scan root does not exist: {root}")
    if root.is_file():
        return (parse_program(root, root.parent),)
    found: List[Path] = []
    stack = [root]
    while stack:
        current = stack.pop()
        for child in sorted(current.iterdir(), key=lambda p: p.name):
            if child.is_symlink():
                continue
            if child.is_dir():
                if child.name not in SKIP_DIRS:
                    stack.append(child)
            elif child.is_file() and child.suffix.lower() in PROGRAM_SUFFIXES:
                found.append(child)
    found.sort(key=lambda p: p.as_posix())
    return tuple(parse_program(p, root) for p in found)


# --------------------------------------------------------------------------
# The cross-check (D29)
# --------------------------------------------------------------------------

class CrossCheckOutcome(str, Enum):
    """The four outcomes. Three of them are findings."""

    AGREE = "AGREE"
    DISAGREE = "DISAGREE"
    NO_LRECL = "NO_LRECL"
    NO_LAYOUT = "NO_LAYOUT"


@dataclass(frozen=True)
class CrossCheck:
    """One comparison of a computed record length against a declared LRECL.

    **Both numbers are always present when known, and nothing here picks one.**
    There is no ``record_length`` property, no ``preferred``, no ``resolved``:
    a caller that wants a single number must decide for itself and say so, and
    a report that renders this row shows the customer both figures. See
    ``tests/test_discovery_files.py::test_no_resolving_accessor_exists``.
    """

    outcome: CrossCheckOutcome
    computed_length: Optional[int]
    declared_lrecl: Optional[int]
    recfm: Optional[str]
    rdw_bytes: int
    adjusted_computed: Optional[int]
    delta: Optional[int]
    basis: str
    conflict: Optional[str] = None
    layout_status: Optional[str] = None
    layout_reason: Optional[str] = None
    ddname: Optional[str] = None
    dataset: Optional[str] = None
    record_name: Optional[str] = None
    layout_evidence: Optional[Evidence] = None
    jcl_evidence: Optional[Evidence] = None

    @property
    def is_finding(self) -> bool:
        return self.outcome is not CrossCheckOutcome.AGREE

    def to_dict(self) -> Dict[str, object]:
        return {
            "outcome": self.outcome.value,
            "computed_length": self.computed_length,
            "declared_lrecl": self.declared_lrecl,
            "recfm": self.recfm,
            "rdw_bytes": self.rdw_bytes,
            "adjusted_computed": self.adjusted_computed,
            "delta": self.delta,
            "basis": self.basis,
            "conflict": self.conflict,
            "layout_status": self.layout_status,
            "layout_reason": self.layout_reason,
            "ddname": self.ddname,
            "dataset": self.dataset,
            "record_name": self.record_name,
            "layout_evidence": (
                self.layout_evidence.to_dict() if self.layout_evidence else None
            ),
            "jcl_evidence": (
                self.jcl_evidence.to_dict() if self.jcl_evidence else None
            ),
        }


def _rdw_basis(recfm: Optional[str], computed: int, lrecl: int, rdw: int) -> str:
    if recfm is None:
        return (
            f"RECFM is not declared in any DCB= for this DD, so no Record "
            f"Descriptor Word adjustment is applied: computed {computed} is "
            f"compared with declared LRECL {lrecl} directly. If the dataset is "
            f"in fact variable-length, the declared LRECL would exceed the "
            f"computed length by {RDW_BYTES} bytes for that reason alone."
        )
    if rdw:
        return (
            f"RECFM={recfm}: z/OS counts the {RDW_BYTES}-byte Record Descriptor "
            f"Word inside LRECL, so the computed record length is compared as "
            f"{computed} + {RDW_BYTES} = {computed + rdw} against the declared "
            f"LRECL {lrecl}."
        )
    return (
        f"RECFM={recfm}: fixed-length records carry no Record Descriptor Word, "
        f"so the computed record length {computed} is compared with the "
        f"declared LRECL {lrecl} directly."
    )


def cross_check(
    *,
    computed_length: Optional[int],
    dd: Optional[DDStatement],
    record_name: Optional[str] = None,
    layout_status: Optional[str] = None,
    layout_reason: Optional[str] = None,
    layout_evidence: Optional[Evidence] = None,
) -> CrossCheck:
    """Compare a computed record length with a declared ``LRECL``.

    Never resolves a disagreement. When the two differ, both numbers and the
    conflict are reported and the caller is told, in words, what disagrees.
    """
    dcb = dd.dcb if dd is not None else None
    declared = dcb.lrecl if dcb is not None else None
    recfm = dcb.recfm if dcb is not None else None
    rdw = dcb.rdw_bytes if dcb is not None else 0
    ddname = dd.ddname if dd is not None else None
    dataset = dd.dataset if dd is not None else None
    jcl_evidence = dd.evidence if dd is not None else None

    if computed_length is not None and declared is not None:
        adjusted = computed_length + rdw
        delta = adjusted - declared
        basis = _rdw_basis(recfm, computed_length, declared, rdw)
        if delta == 0:
            return CrossCheck(
                outcome=CrossCheckOutcome.AGREE,
                computed_length=computed_length,
                declared_lrecl=declared,
                recfm=recfm,
                rdw_bytes=rdw,
                adjusted_computed=adjusted,
                delta=0,
                basis=basis,
                layout_status=layout_status,
                layout_reason=layout_reason,
                ddname=ddname,
                dataset=dataset,
                record_name=record_name,
                layout_evidence=layout_evidence,
                jcl_evidence=jcl_evidence,
            )
        conflict = (
            f"The copybook and the job stream disagree about the record length "
            f"of {dataset or ddname or 'this dataset'}. The record description "
            f"{record_name or '(unnamed)'} computes to {computed_length} bytes"
            + (f" ({adjusted} including the {RDW_BYTES}-byte RDW)" if rdw else "")
            + f"; the JCL declares LRECL={declared}. Difference: {delta:+d} "
            f"bytes. Relian does not choose between these two numbers — both "
            f"come from the customer's own artifacts, and which one describes "
            f"the data on the volume cannot be determined from source. Resolve "
            f"this before migrating: a copybook that does not describe the data "
            f"is the most expensive defect to discover after cutover."
        )
        return CrossCheck(
            outcome=CrossCheckOutcome.DISAGREE,
            computed_length=computed_length,
            declared_lrecl=declared,
            recfm=recfm,
            rdw_bytes=rdw,
            adjusted_computed=adjusted,
            delta=delta,
            basis=basis,
            conflict=conflict,
            layout_status=layout_status,
            layout_reason=layout_reason,
            ddname=ddname,
            dataset=dataset,
            record_name=record_name,
            layout_evidence=layout_evidence,
            jcl_evidence=jcl_evidence,
        )

    if computed_length is not None:
        return CrossCheck(
            outcome=CrossCheckOutcome.NO_LRECL,
            computed_length=computed_length,
            declared_lrecl=None,
            recfm=recfm,
            rdw_bytes=rdw,
            adjusted_computed=None,
            delta=None,
            basis=(
                f"The record description computes to {computed_length} bytes. "
                f"No DCB= in any JCL for this DD declares an LRECL, so this "
                f"layout is unverified against any source outside the copybook "
                f"itself."
            ),
            layout_status=layout_status,
            layout_reason=layout_reason,
            ddname=ddname,
            dataset=dataset,
            record_name=record_name,
            layout_evidence=layout_evidence,
            jcl_evidence=jcl_evidence,
        )

    if declared is not None:
        return CrossCheck(
            outcome=CrossCheckOutcome.NO_LAYOUT,
            computed_length=None,
            declared_lrecl=declared,
            recfm=recfm,
            rdw_bytes=rdw,
            adjusted_computed=None,
            delta=None,
            basis=(
                f"The JCL declares LRECL={declared} for this dataset, but no "
                f"record description was computed for it"
                + (f": {layout_reason}" if layout_reason else "")
                + ". A dataset with a declared length and no description is a "
                "dataset nothing in scope explains."
            ),
            layout_status=layout_status,
            layout_reason=layout_reason,
            ddname=ddname,
            dataset=dataset,
            record_name=record_name,
            layout_evidence=layout_evidence,
            jcl_evidence=jcl_evidence,
        )

    return CrossCheck(
        outcome=CrossCheckOutcome.NO_LAYOUT,
        computed_length=None,
        declared_lrecl=None,
        recfm=recfm,
        rdw_bytes=rdw,
        adjusted_computed=None,
        delta=None,
        basis=(
            "Neither a computed record length nor a declared LRECL is "
            "available for this file"
            + (f": {layout_reason}" if layout_reason else "")
            + ". This row checks nothing, and is reported so that it is not "
            "mistaken for a check that passed."
        ),
        layout_status=layout_status,
        layout_reason=layout_reason,
        ddname=ddname,
        dataset=dataset,
        record_name=record_name,
        layout_evidence=layout_evidence,
        jcl_evidence=jcl_evidence,
    )


# --------------------------------------------------------------------------
# The join (D28)
# --------------------------------------------------------------------------

class FindingKind(str, Enum):
    """Findings that only exist because the three sources are kept apart."""

    DATASET_NOT_DECLARED = "DATASET_NOT_DECLARED"
    FILE_NOT_SUPPLIED = "FILE_NOT_SUPPLIED"
    SELECT_WITHOUT_FD = "SELECT_WITHOUT_FD"
    FD_WITHOUT_SELECT = "FD_WITHOUT_SELECT"
    ASSIGN_TO_LITERAL = "ASSIGN_TO_LITERAL"


@dataclass(frozen=True)
class Finding:
    kind: FindingKind
    detail: str
    evidence: Optional[Evidence] = None

    def to_dict(self) -> Dict[str, object]:
        return {
            "kind": self.kind.value,
            "detail": self.detail,
            "evidence": self.evidence.to_dict() if self.evidence else None,
        }


@dataclass(frozen=True)
class FileRecord:
    """One logical file, assembled from up to three independent sources.

    The three ``*_evidence`` fields are independently ``None``. A record with
    only ``jcl_evidence`` is a dataset no program in scope declares; a record
    with only ``select_evidence``/``fd_evidence`` is a file the job stream
    never supplies. Both are findings, and both are invisible in a design that
    merges the sources into one row.
    """

    ddname: Optional[str]
    program: Optional[str] = None
    file_name: Optional[str] = None
    select: Optional[SelectStatement] = None
    fd: Optional[FdEntry] = None
    dd: Optional[DDStatement] = None
    select_evidence: Optional[Evidence] = None
    fd_evidence: Optional[Evidence] = None
    jcl_evidence: Optional[Evidence] = None
    layout: Optional[Layout] = None
    layout_origin: Optional[str] = None
    layout_reason: Optional[str] = None
    findings: Tuple[Finding, ...] = ()
    check: Optional[CrossCheck] = None

    @property
    def sources(self) -> Tuple[str, ...]:
        """Which of the three sources contributed. Never collapsed to a count."""
        present = []
        if self.select_evidence is not None:
            present.append("SELECT")
        if self.fd_evidence is not None:
            present.append("FD")
        if self.jcl_evidence is not None:
            present.append("JCL")
        return tuple(present)

    def to_dict(self) -> Dict[str, object]:
        return {
            "ddname": self.ddname,
            "program": self.program,
            "file_name": self.file_name,
            "sources": list(self.sources),
            "select": self.select.to_dict() if self.select else None,
            "fd": self.fd.to_dict() if self.fd else None,
            "dd": self.dd.to_dict() if self.dd else None,
            "select_evidence": (
                self.select_evidence.to_dict() if self.select_evidence else None
            ),
            "fd_evidence": self.fd_evidence.to_dict() if self.fd_evidence else None,
            "jcl_evidence": (
                self.jcl_evidence.to_dict() if self.jcl_evidence else None
            ),
            "layout_origin": self.layout_origin,
            "layout_reason": self.layout_reason,
            "layout_group_length": (
                self.layout.group_length if self.layout else None
            ),
            "layout_status": (
                self.layout.status.value if self.layout else None
            ),
            "findings": [f.to_dict() for f in self.findings],
            "cross_check": self.check.to_dict() if self.check else None,
        }


@dataclass(frozen=True)
class FileInventory:
    """Every file record, plus counts recomputed on demand (R1)."""

    records: Tuple[FileRecord, ...] = ()
    programs_scanned: int = 0
    programs_with_file_control: int = 0

    def outcomes(self) -> Dict[str, int]:
        counts = {o.value: 0 for o in CrossCheckOutcome}
        for record in self.records:
            if record.check is not None:
                counts[record.check.outcome.value] += 1
        return counts

    def findings(self) -> Tuple[Finding, ...]:
        return tuple(f for r in self.records for f in r.findings)

    def disagreements(self) -> Tuple[FileRecord, ...]:
        return tuple(
            r for r in self.records
            if r.check is not None
            and r.check.outcome is CrossCheckOutcome.DISAGREE
        )

    def to_dict(self) -> Dict[str, object]:
        return {
            "programs_scanned": self.programs_scanned,
            "programs_with_file_control": self.programs_with_file_control,
            "outcomes": self.outcomes(),
            "records": [r.to_dict() for r in self.records],
        }


def _layout_for_fd(
    fd: Optional[FdEntry], resolution: Optional[Resolution]
) -> Tuple[Optional[Layout], Optional[str], Optional[str]]:
    """Resolve an ``FD``'s record to a layout.

    Returns ``(layout, origin, reason)``. ``reason`` is filled whenever a
    layout could NOT be produced, and says which of the several ways it failed
    — an absent layout with no reason is indistinguishable from a file nobody
    looked at.
    """
    if fd is None:
        return None, None, "no FD in scope describes this file"
    if fd.copy_members:
        member = fd.copy_members[0]
        if resolution is None:
            return None, None, (
                f"the record is supplied by COPY {member} and no copybook "
                f"resolution was provided to this scan"
            )
        layout = compute(member, resolution)
        if layout is None:
            return None, None, (
                f"COPY {member} resolved but contains no 01/77 record"
            )
        if layout.status is LayoutStatus.NONE:
            return layout, f"copybook:{member}", (
                "; ".join(layout.reasons) or f"COPY {member} is unresolved"
            )
        return layout, f"copybook:{member}", None
    if not fd.record_source.strip():
        return None, None, (
            f"FD {fd.file_name} declares no 01 record description"
        )
    layouts = compute_text(fd.record_source, origin=fd.evidence.file)
    if not layouts:
        return None, None, (
            f"FD {fd.file_name}'s record description contains no 01/77 record"
        )
    layout = layouts[0]
    reason = None
    if layout.group_length is None:
        reason = "; ".join(layout.reasons) or "the record length was not computed"
    return layout, "inline", reason


def _record_name(fd: Optional[FdEntry], layout: Optional[Layout]) -> Optional[str]:
    """Name the record a cross-check row is about.

    The FD's own ``01`` name where there is one. When the record arrives
    through ``COPY``, the FD names no ``01`` at all — the name is inside the
    copybook — so the computed layout's group is used instead.

    Without the fallback, every COPY-supplied record produced a row reading
    ``record: (unnamed)``. Measured on CardDemo: four of the thirteen AGREE
    rows are COPY-supplied, so a third of the strongest evidence the
    cross-check produces could not say what it was about.
    """
    if fd is not None and fd.record_names:
        return fd.record_names[0]
    if layout is not None and layout.group:
        return layout.group
    return None


def build_inventory(
    programs: Sequence[ProgramFiles],
    jcl: Optional[JclInventory] = None,
    resolution: Optional[Resolution] = None,
) -> FileInventory:
    """Join SELECT, FD and JCL into one record per file, keeping all three.

    The join key is the DD name, reached from the COBOL through
    ``SELECT … ASSIGN TO`` and from the JCL through the step's ``EXEC PGM=``.
    """
    dd_by_program: Mapping[str, Tuple[DDStatement, ...]] = (
        jcl.by_program() if jcl is not None else {}
    )
    records: List[FileRecord] = []
    matched: set = set()

    for program in sorted(programs, key=lambda p: (p.program, p.path)):
        fds = {f.file_name: f for f in program.fds}
        program_dds = dd_by_program.get(program.program, ())
        dd_index: Dict[str, DDStatement] = {}
        for dd in program_dds:
            dd_index.setdefault(dd.ddname, dd)

        seen_fd_names: set = set()
        for select in program.selects:
            fd = fds.get(select.file_name)
            if fd is not None:
                seen_fd_names.add(fd.file_name)
            dd = dd_index.get(select.assign_to) if select.assign_to else None
            findings: List[Finding] = []
            if select.assign_literal:
                findings.append(Finding(
                    FindingKind.ASSIGN_TO_LITERAL,
                    f"{program.program}: SELECT {select.file_name} assigns to "
                    f"the literal {select.assign_to!r}, which names a file "
                    f"directly rather than a DD. No JCL join is attempted.",
                    select.evidence,
                ))
            elif dd is None:
                findings.append(Finding(
                    FindingKind.FILE_NOT_SUPPLIED,
                    f"{program.program}: SELECT {select.file_name} assigns to "
                    f"DD {select.assign_to}, and no JCL step running "
                    f"{program.program} supplies a DD of that name. The job "
                    f"stream never provides this file.",
                    select.evidence,
                ))
            if fd is None:
                findings.append(Finding(
                    FindingKind.SELECT_WITHOUT_FD,
                    f"{program.program}: SELECT {select.file_name} has no FD, "
                    f"so the file has no record description in this program.",
                    select.evidence,
                ))
            if dd is not None:
                matched.add((program.program, dd.ddname))

            layout, origin, reason = _layout_for_fd(fd, resolution)
            record_name = _record_name(fd, layout)
            check = cross_check(
                computed_length=layout.group_length if layout else None,
                dd=dd,
                record_name=record_name,
                layout_status=layout.status.value if layout else None,
                layout_reason=reason,
                layout_evidence=fd.evidence if fd is not None else None,
            )
            records.append(FileRecord(
                ddname=select.assign_to,
                program=program.program,
                file_name=select.file_name,
                select=select,
                fd=fd,
                dd=dd,
                select_evidence=select.evidence,
                fd_evidence=fd.evidence if fd is not None else None,
                jcl_evidence=dd.evidence if dd is not None else None,
                layout=layout,
                layout_origin=origin,
                layout_reason=reason,
                findings=tuple(findings),
                check=check,
            ))

        for fd in program.fds:
            if fd.file_name in seen_fd_names or fd.is_sort:
                continue
            layout, origin, reason = _layout_for_fd(fd, resolution)
            records.append(FileRecord(
                ddname=None,
                program=program.program,
                file_name=fd.file_name,
                fd=fd,
                fd_evidence=fd.evidence,
                layout=layout,
                layout_origin=origin,
                layout_reason=reason,
                findings=(Finding(
                    FindingKind.FD_WITHOUT_SELECT,
                    f"{program.program}: FD {fd.file_name} has no SELECT, so "
                    f"the record description is not bound to any DD name and "
                    f"cannot be joined to the job stream.",
                    fd.evidence,
                ),),
                check=cross_check(
                    computed_length=layout.group_length if layout else None,
                    dd=None,
                    record_name=_record_name(fd, layout),
                    layout_status=layout.status.value if layout else None,
                    layout_reason=reason,
                    layout_evidence=fd.evidence,
                ),
            ))

    # Datasets the job stream allocates that no SELECT in scope declares.
    if jcl is not None:
        for dd in jcl.dd_statements:
            if dd.program is None or dd.dsn is None:
                continue
            if (dd.program, dd.ddname) in matched:
                continue
            records.append(FileRecord(
                ddname=dd.ddname,
                program=dd.program,
                dd=dd,
                jcl_evidence=dd.evidence,
                findings=(Finding(
                    FindingKind.DATASET_NOT_DECLARED,
                    f"DD {dd.ddname} in step {dd.step or '(unnamed)'} supplies "
                    f"dataset {dd.dsn} to program {dd.program}, and no SELECT "
                    f"in any program in scope assigns to that DD name. Either "
                    f"the program is outside the scanned tree or the dataset "
                    f"is read by something other than the COBOL we can see.",
                    dd.evidence,
                ),),
                check=cross_check(
                    computed_length=None,
                    dd=dd,
                    layout_reason=(
                        "no program in scope declares this DD, so no record "
                        "description was found for it"
                    ),
                ),
            ))

    return FileInventory(
        records=tuple(records),
        programs_scanned=len(programs),
        programs_with_file_control=len(
            [p for p in programs if p.has_file_control]
        ),
    )
