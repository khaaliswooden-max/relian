"""WP-2.4 §3.1 — JCL parsing: DD statements, datasets, and their DCB attributes.

This module reads **JCL text only**. It opens no dataset, decodes no customer
record, and imports nothing that could (R12; asserted by
``tests/test_discovery_reads_no_data.py``). Everything it reports comes from
job-control source that the customer already hands us alongside the COBOL.

Why JCL is a separate source rather than a detail of the COBOL scan
------------------------------------------------------------------

A COBOL program names a *DD name*: ``SELECT ACCTFILE-FILE ASSIGN TO ACCTFILE``.
It has no idea what dataset that DD is bound to, and it cannot: the binding is
made at run time by the job stream. JCL is the only artifact that says
``ACCTFILE`` is ``AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS``, and — crucially for
§3.2 — the only artifact that declares an ``LRECL`` for it. That declared
length is what makes the copybook's computed length checkable against
something outside itself.

JCL parsing is independent of the COBOL preprocessor (V11/D17). Nothing here
imports :mod:`src.discovery.copybook`, and nothing here needs a ``COPY``
resolved. If that ever stops being true the design has drifted and WP-2.5
escalation applies.

The continuation trap
---------------------

This is the parsing hazard the work package names, and it is worth stating
precisely because the failure is silent.

A JCL statement is a *card*: columns 1-71 carry the statement, column 72 is the
continuation indicator, and columns 73-80 are an identification field that is
**not part of the operand**. CardDemo uses that field — measured: 102 records
carry a sequence number there, e.g.::

    //            DCB=(LRECL=100,BLKSIZE=0,RECFM=FB),                   00640019

A reader that takes whole lines swallows ``00640019`` as operand text.

Worse, an operand may continue onto the next record, and a ``DCB=`` sub-
parameter list is free to split across that boundary::

    //OUTFILE  DD DSN=AWS.M2.X,
    //            DCB=(LRECL=107,RECFM=FB,
    //            DSORG=PS,BLKSIZE=0),
    //            UNIT=SYSAD

A naive per-line scan for ``DCB=\\(([^)]*)\\)`` finds nothing on record 2 (no
closing paren) or, with a laxer pattern, finds ``LRECL=107,RECFM=FB`` and stops
— **a parameter set that looks complete and is not**. It reports ``DSORG`` and
``BLKSIZE`` as absent, which is indistinguishable from a DD that genuinely
declares neither. That is a wrong answer wearing the costume of a right one,
and no downstream check catches it because ``LRECL`` — the field the §3.2
cross-check turns on — happened to survive.

``tests/test_discovery_jcl.py`` plants that red: it implements the naive scan,
proves it truncates the split fixture, and then proves this parser recovers the
full set. The guard is kept afterwards.

Two continuation rules are honoured, because real JCL uses both:

* **the comma rule** — an operand field ending in ``,`` continues; and
* **the column-72 rule** — any non-blank in column 72 continues,

and a continuation record must itself begin ``//`` with a blank name field.
Where a continuation is *interrupted* — by a comment, or by a record that is
plainly a new statement — the statement ends and the interruption is recorded
in :attr:`JclMember.notes` rather than absorbed. A parser that guesses through
an interruption is inventing operands.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field as _dc_field
from pathlib import Path
from typing import Dict, List, Mapping, Optional, Sequence, Tuple

# --------------------------------------------------------------------------
# Card geometry
# --------------------------------------------------------------------------

#: Columns 1-71 hold the statement. Slice bound, 0-based.
STATEMENT_END_COL = 71

#: Column 72 (0-based index 71): non-blank means "continued".
CONTINUATION_COL = 71

#: Columns 73-80: the identification/sequence field. Never operand text.
IDENTIFICATION_START_COL = 72

#: Where a continuation record's operand may resume (columns 4-16). Used only
#: to explain a rejected continuation, never to reposition text.
CONTINUATION_OPERAND_COLS = (3, 15)


# --------------------------------------------------------------------------
# Shared evidence record
# --------------------------------------------------------------------------

@dataclass(frozen=True)
class Evidence:
    """Where a fact was read from. Shared by all three WP-2.4 sources (D28).

    ``file`` is posix-form and relative to the scan root where one exists (R8).
    A fact with no evidence is not reported at all — the alternative is a
    finding a customer cannot check, which is an assertion rather than a
    finding.
    """

    file: str
    line: int

    def to_dict(self) -> Dict[str, object]:
        return {"file": self.file, "line": self.line}

    def __str__(self) -> str:                      # pragma: no cover - trivial
        return f"{self.file}:{self.line}"


# --------------------------------------------------------------------------
# DCB
# --------------------------------------------------------------------------

#: RECFM letters that mean "variable-length records, each prefixed by a 4-byte
#: Record Descriptor Word". The RDW is counted inside LRECL by z/OS, which is
#: why §3.2 must adjust for it rather than compare raw numbers.
_VARIABLE_RECFM_PREFIXES = ("V",)

#: Bytes in a Record Descriptor Word. Fixed by the architecture, not a choice.
RDW_BYTES = 4


@dataclass(frozen=True)
class DCB:
    """``DCB=`` sub-parameters, as declared. Absent sub-parameters are ``None``.

    ``None`` here means "the JCL did not say", never "zero" and never a
    default (R1). ``BLKSIZE=0`` in CardDemo means *let the system choose*, and
    is recorded as the ``0`` it literally is — that is a declared value, not a
    missing one, and the distinction matters to anyone sizing a target.
    """

    lrecl: Optional[int] = None
    recfm: Optional[str] = None
    dsorg: Optional[str] = None
    blksize: Optional[int] = None
    referback: Optional[str] = None
    raw: str = ""
    unparsed: Tuple[str, ...] = ()

    @property
    def is_variable(self) -> bool:
        """True when RECFM declares variable-length records (V, VB, VBS...)."""
        if not self.recfm:
            return False
        return self.recfm.upper().startswith(_VARIABLE_RECFM_PREFIXES)

    @property
    def rdw_bytes(self) -> int:
        """4 for a variable-length RECFM, 0 for fixed, 0 when RECFM is unknown.

        Unknown returns 0 *and* :attr:`is_variable` returns False, so a caller
        that needs to distinguish "fixed" from "not stated" must look at
        ``recfm`` — §3.2 does exactly that and refuses to adjust silently.
        """
        return RDW_BYTES if self.is_variable else 0

    def to_dict(self) -> Dict[str, object]:
        return {
            "lrecl": self.lrecl,
            "recfm": self.recfm,
            "dsorg": self.dsorg,
            "blksize": self.blksize,
            "referback": self.referback,
            "raw": self.raw,
            "unparsed": list(self.unparsed),
        }


# --------------------------------------------------------------------------
# Statements
# --------------------------------------------------------------------------

@dataclass(frozen=True)
class Step:
    """One ``EXEC`` step. ``program`` is what ties a DD to COBOL (§3.2)."""

    name: Optional[str]
    program: Optional[str]
    proc: Optional[str]
    evidence: Evidence
    operand: str = ""

    def to_dict(self) -> Dict[str, object]:
        return {
            "name": self.name,
            "program": self.program,
            "proc": self.proc,
            "evidence": self.evidence.to_dict(),
        }


@dataclass(frozen=True)
class DDStatement:
    """One ``DD`` statement, with the step and program it sits under.

    ``concatenation_index`` is 0 for a named DD and 1, 2, ... for the unnamed
    ``//    DD`` records that concatenate onto it. They are kept as separate
    rows rather than merged: each names its own dataset, and a concatenation is
    exactly the case where one DD name maps to several datasets.
    """

    ddname: str
    step: Optional[str]
    program: Optional[str]
    proc_step: Optional[str] = None
    dsn: Optional[str] = None
    disp: Optional[str] = None
    dcb: Optional[DCB] = None
    sysout: Optional[str] = None
    in_stream: bool = False
    in_stream_kind: Optional[str] = None
    dummy: bool = False
    concatenation_index: int = 0
    gdg_relative: Optional[str] = None
    member: Optional[str] = None
    evidence: Evidence = Evidence("<text>", 0)
    lines: Tuple[int, ...] = ()
    operand: str = ""
    parameters: Tuple[Tuple[str, str], ...] = ()

    @property
    def continued(self) -> bool:
        """True when this statement was assembled from more than one record."""
        return len(self.lines) > 1

    @property
    def dataset(self) -> Optional[str]:
        """The DSN with any GDG relative or member suffix stripped."""
        if self.dsn is None:
            return None
        return _base_dsn(self.dsn)

    def to_dict(self) -> Dict[str, object]:
        return {
            "ddname": self.ddname,
            "step": self.step,
            "program": self.program,
            "proc_step": self.proc_step,
            "dsn": self.dsn,
            "dataset": self.dataset,
            "disp": self.disp,
            "dcb": self.dcb.to_dict() if self.dcb else None,
            "sysout": self.sysout,
            "in_stream": self.in_stream,
            "in_stream_kind": self.in_stream_kind,
            "dummy": self.dummy,
            "concatenation_index": self.concatenation_index,
            "gdg_relative": self.gdg_relative,
            "member": self.member,
            "evidence": self.evidence.to_dict(),
            "lines": list(self.lines),
            "continued": self.continued,
        }


@dataclass(frozen=True)
class JclMember:
    """One parsed JCL member.

    ``notes`` carries every place the parser declined to guess: an interrupted
    continuation, an operand it could not split, an in-stream block that ran to
    end-of-file. They are output, not logging — R2 says an unsupported shape is
    reported, never silently approximated.
    """

    path: str
    name: str
    job: Optional[str] = None
    steps: Tuple[Step, ...] = ()
    dd_statements: Tuple[DDStatement, ...] = ()
    notes: Tuple[str, ...] = ()
    records_scanned: int = 0
    in_stream_records: int = 0

    def dd_by_name(self, ddname: str) -> Tuple[DDStatement, ...]:
        upper = ddname.upper()
        return tuple(d for d in self.dd_statements if d.ddname == upper)

    def programs(self) -> Tuple[str, ...]:
        seen: List[str] = []
        for step in self.steps:
            if step.program and step.program not in seen:
                seen.append(step.program)
        return tuple(seen)

    def to_dict(self) -> Dict[str, object]:
        return {
            "path": self.path,
            "name": self.name,
            "job": self.job,
            "steps": [s.to_dict() for s in self.steps],
            "dd_statements": [d.to_dict() for d in self.dd_statements],
            "notes": list(self.notes),
            "records_scanned": self.records_scanned,
            "in_stream_records": self.in_stream_records,
        }


@dataclass(frozen=True)
class JclInventory:
    """Every JCL member under one root, with counts computed on demand (R1)."""

    root: str
    members: Tuple[JclMember, ...] = ()

    @property
    def dd_statements(self) -> Tuple[DDStatement, ...]:
        return tuple(d for m in self.members for d in m.dd_statements)

    def counts(self) -> Dict[str, int]:
        """The §0.4 rows, recomputed here rather than quoted from the log."""
        dds = self.dd_statements
        with_dsn = [d for d in dds if d.dsn is not None]
        with_dcb = [d for d in dds if d.dcb is not None]
        return {
            "members": len(self.members),
            "members_with_dd": len({
                m.path for m in self.members if m.dd_statements
            }),
            "dd_statements": len(dds),
            "dd_named": len([d for d in dds if d.concatenation_index == 0]),
            "dd_concatenated": len([d for d in dds if d.concatenation_index]),
            "dd_with_dsn": len(with_dsn),
            "members_with_dsn": len({d.evidence.file for d in with_dsn}),
            "dd_with_dcb": len(with_dcb),
            "members_with_dcb": len({d.evidence.file for d in with_dcb}),
            "dd_with_lrecl": len([
                d for d in dds if d.dcb is not None and d.dcb.lrecl is not None
            ]),
            "dd_continued": len([d for d in dds if d.continued]),
        }

    def by_program(self) -> Dict[str, Tuple[DDStatement, ...]]:
        """program -> its DD statements. The join key for §3.2."""
        out: Dict[str, List[DDStatement]] = {}
        for dd in self.dd_statements:
            if dd.program:
                out.setdefault(dd.program, []).append(dd)
        return {k: tuple(v) for k, v in sorted(out.items())}

    def to_dict(self) -> Dict[str, object]:
        return {
            "root": self.root,
            "counts": self.counts(),
            "members": [m.to_dict() for m in self.members],
        }


# --------------------------------------------------------------------------
# Record classification
# --------------------------------------------------------------------------

_SIMPLE_NAME_RE = r"[A-Za-z$@#][A-Za-z0-9$@#]{0,7}"
#: A DD name may be QUALIFIED as ``procstep.ddname`` when a job overrides a
#: DD inside an invoked procedure -- CardDemo does this at
#: ``app/jcl/PRTCATBL.jcl:31`` (``//PRC001.FILEIN DD ...``). An unqualified
#: pattern does not merely mis-name that statement, it fails to recognise it
#: as a statement at all, and the record after it is then read as an orphan.
#: Measured before the fix: 5 CardDemo members lost DD statements this way.
_NAME_RE = rf"{_SIMPLE_NAME_RE}(?:\.{_SIMPLE_NAME_RE})?"
_STATEMENT_RE = re.compile(rf"^//(?P<name>{_NAME_RE})?\s")
_STATEMENT_BARE_RE = re.compile(rf"^//(?P<name>{_NAME_RE})?$")


def statement_field(record: str) -> str:
    """Columns 1-71 of ``record``, right-stripped. The identification field in
    73-80 is discarded here and nowhere else, so no operand can ever contain a
    sequence number."""
    return record[:STATEMENT_END_COL].rstrip()


def continuation_flag(record: str) -> bool:
    """True when column 72 is non-blank."""
    if len(record) <= CONTINUATION_COL:
        return False
    return bool(record[CONTINUATION_COL].strip())


def _is_comment(field: str) -> bool:
    return field.startswith("//*")


def _is_delimiter(field: str) -> bool:
    return field.startswith("/*")


def _is_null_statement(field: str) -> bool:
    return field.rstrip() == "//"


def _is_statement(field: str) -> bool:
    if not field.startswith("//") or _is_comment(field) or _is_null_statement(field):
        return False
    return bool(_STATEMENT_RE.match(field) or _STATEMENT_BARE_RE.match(field))


def _is_continuation_record(field: str) -> bool:
    """A continuation record is ``//`` followed by blanks, then the operand.

    The name field must be blank: ``//SYSIN   DD ...`` is a new statement even
    when the previous record asked to be continued, and treating it as operand
    text would fabricate a parameter.
    """
    if not field.startswith("//") or _is_comment(field):
        return False
    rest = field[2:]
    if not rest.strip():
        return False
    return rest[:1].isspace()


# --------------------------------------------------------------------------
# Operand splitting
# --------------------------------------------------------------------------

def split_operands(operand: str) -> Tuple[Tuple[str, ...], Tuple[str, ...]]:
    """Split an operand field on commas at paren depth 0, outside quotes.

    Returns ``(items, problems)``. ``problems`` names an unbalanced paren or an
    unterminated quote instead of silently returning a mangled split — a
    truncated operand is precisely the failure mode this module exists to
    avoid.
    """
    items: List[str] = []
    problems: List[str] = []
    depth = 0
    in_quote = False
    current: List[str] = []
    for ch in operand:
        if in_quote:
            current.append(ch)
            if ch == "'":
                in_quote = False
            continue
        if ch == "'":
            in_quote = True
            current.append(ch)
        elif ch == "(":
            depth += 1
            current.append(ch)
        elif ch == ")":
            depth -= 1
            current.append(ch)
            if depth < 0:
                problems.append(
                    "unbalanced ')' in operand; the split is reported as-is "
                    "rather than repaired"
                )
                depth = 0
        elif ch == "," and depth == 0:
            items.append("".join(current))
            current = []
        else:
            current.append(ch)
    tail = "".join(current)
    if tail:
        items.append(tail)
    if depth > 0:
        problems.append(
            f"unclosed '(' in operand (depth {depth} at end); a DCB= or DISP= "
            f"list may be truncated"
        )
    if in_quote:
        problems.append("unterminated quoted string in operand")
    return tuple(i.strip() for i in items if i.strip()), tuple(problems)


def _key_value(item: str) -> Tuple[str, str]:
    if "=" not in item:
        return ("", item)
    key, _, value = item.partition("=")
    return (key.strip().upper(), value.strip())


def parse_dcb(value: str) -> DCB:
    """Parse a ``DCB=`` value into its sub-parameters.

    Accepts the bare form (``DCB=BLKSIZE=800``), the parenthesised list, and
    the model-dataset referback (``DCB=(*.SORTIN)`` / ``DCB=*.step.dd``).
    A referback is recorded and **not followed**: the attributes it inherits
    live in another DD, and copying them here would present an inference as a
    declaration.
    """
    raw = value.strip()
    inner = raw
    if inner.startswith("(") and inner.endswith(")"):
        inner = inner[1:-1]
    items, problems = split_operands(inner)
    lrecl = recfm = dsorg = blksize = referback = None
    unparsed: List[str] = list(problems)
    for item in items:
        key, val = _key_value(item)
        if key == "LRECL":
            lrecl = _as_int(val, "LRECL", unparsed)
        elif key == "RECFM":
            recfm = val.upper()
        elif key == "DSORG":
            dsorg = val.upper()
        elif key == "BLKSIZE":
            blksize = _as_int(val, "BLKSIZE", unparsed)
        elif key == "" and val.startswith("*."):
            referback = val
        elif key == "":
            # A positional model dataset name, or a keyword this version does
            # not model. Recorded, never guessed at.
            unparsed.append(item)
        else:
            unparsed.append(item)
    return DCB(
        lrecl=lrecl, recfm=recfm, dsorg=dsorg, blksize=blksize,
        referback=referback, raw=raw, unparsed=tuple(unparsed),
    )


def _as_int(value: str, name: str, sink: List[str]) -> Optional[int]:
    """Integer or ``None`` — never a default. A non-numeric ``LRECL`` (a
    symbolic parameter, say) is reported unparsed rather than coerced."""
    try:
        return int(value)
    except (TypeError, ValueError):
        sink.append(f"{name}={value!r} is not an integer")
        return None


_GDG_RE = re.compile(r"\((?P<rel>[+-]?\d+)\)$")
_MEMBER_RE = re.compile(r"\((?P<member>[A-Za-z$@#][A-Za-z0-9$@#]{0,7})\)$")


def _base_dsn(dsn: str) -> str:
    return _GDG_RE.sub("", _MEMBER_RE.sub("", dsn)).strip()


# --------------------------------------------------------------------------
# The parser
# --------------------------------------------------------------------------

def _expects_continuation(raw_lines: Sequence[str], index: int) -> bool:
    """Did the nearest preceding non-comment record ask to be continued?

    Used only to tell an orphaned continuation from a legitimate one. It never
    joins anything -- :func:`_assemble` owns joining, and this must not become
    a second, divergent implementation of the continuation rule.
    """
    cursor = index - 1
    while cursor >= 0:
        field = statement_field(raw_lines[cursor])
        if _is_comment(field) or not field.strip():
            return False
        return (
            field.rstrip().endswith(",")
            or (continuation_flag(raw_lines[cursor]) and not _is_comment(field))
        )
    return False


def parse_text(text: str, path: str = "<text>") -> JclMember:
    """Parse one JCL member from text."""
    raw_lines = text.splitlines()
    notes: List[str] = []
    steps: List[Step] = []
    dds: List[DDStatement] = []
    job: Optional[str] = None
    current_step: Optional[Step] = None
    last_dd_name: Optional[str] = None
    concat_index = 0
    in_stream_kind: Optional[str] = None
    in_stream_records = 0

    index = 0
    total = len(raw_lines)
    while index < total:
        record = raw_lines[index]
        lineno = index + 1
        field = statement_field(record)

        # -- in-stream data ------------------------------------------------
        if in_stream_kind is not None:
            if _is_delimiter(field):
                in_stream_kind = None
                index += 1
                continue
            # `DD *` ends at the next JCL statement; `DD DATA` does not, so a
            # `//` inside DATA is data. Getting this backwards silently turns
            # IDCAMS control cards into phantom DD statements.
            if in_stream_kind == "*" and field.startswith("//"):
                in_stream_kind = None
                continue                     # re-handle this record as JCL
            in_stream_records += 1
            index += 1
            continue

        if not field.strip() or _is_comment(field) or _is_delimiter(field):
            index += 1
            continue
        if _is_null_statement(field):
            index += 1
            continue
        if not _is_statement(field):
            index += 1
            continue

        # -- assemble the statement across its continuations ---------------
        start_index = index
        consumed, operand_text, name, operation, stmt_notes = _assemble(
            raw_lines, index, path
        )
        notes.extend(stmt_notes)
        lines = tuple(range(lineno, lineno + consumed))
        index += consumed

        if operation == "JOB":
            job = name
            continue

        if operation == "EXEC":
            current_step = _build_step(name, operand_text, path, lineno)
            steps.append(current_step)
            last_dd_name = None
            concat_index = 0
            continue

        if operation == "DD":
            if name:
                last_dd_name = name
                concat_index = 0
            else:
                if last_dd_name is None:
                    notes.append(
                        f"{path}:{lineno}: unnamed DD with no preceding named "
                        f"DD to concatenate onto; recorded under '' rather "
                        f"than attributed by guess"
                    )
                    last_dd_name = ""
                    concat_index = 0
                else:
                    concat_index += 1
            dd, dd_notes, starts_in_stream = _build_dd(
                ddname=last_dd_name,
                step=current_step,
                operand=operand_text,
                path=path,
                lineno=lineno,
                lines=lines,
                concatenation_index=concat_index,
            )
            notes.extend(dd_notes)
            dds.append(dd)
            if starts_in_stream is not None:
                in_stream_kind = starts_in_stream
            continue

        # PROC, PEND, SET, IF, INCLUDE, OUTPUT ... recorded as seen, not
        # modelled. Silence here would read as "this member had none".
        if operation in {"PROC", "PEND", "SET", "IF", "ELSE", "ENDIF",
                         "INCLUDE", "OUTPUT", "JCLLIB", "CNTL", "ENDCNTL",
                         "XMIT", "COMMAND", "NOTIFY", "EXPORT"}:
            continue
        if not name and not _expects_continuation(raw_lines, start_index):
            # A blank name field plus a token that is not a JCL operation is a
            # stranded operand, not a statement. Reporting it as an unmodelled
            # OPERATION would name `DSN=AWS.M2...` as a JCL verb -- nonsense
            # dressed as a finding. Measured at app/jcl/CREASTMT.JCL:88, which
            # follows a record the corpus itself has corrupted.
            notes.append(
                f"{path}:{lineno}: orphaned continuation record -- the "
                f"preceding statement did not ask to be continued (no trailing "
                f"comma, no column-72 flag), so these operands belong to no "
                f"statement and are not attributed to one"
            )
            continue
        notes.append(
            f"{path}:{lineno}: operation {operation!r} is not modelled by "
            f"this version; its operands are not reported"
        )

    if in_stream_kind is not None:
        notes.append(
            f"{path}: in-stream data block ran to end of file without a "
            f"delimiter; {in_stream_records} record(s) treated as data"
        )

    return JclMember(
        path=path,
        name=Path(path).stem.upper(),
        job=job,
        steps=tuple(steps),
        dd_statements=tuple(dds),
        notes=tuple(notes),
        records_scanned=total,
        in_stream_records=in_stream_records,
    )


def _assemble(
    raw_lines: Sequence[str], start: int, path: str
) -> Tuple[int, str, Optional[str], str, List[str]]:
    """Join a statement with its continuation records.

    Returns ``(records_consumed, operand, name, operation, notes)``.
    """
    notes: List[str] = []
    field = statement_field(raw_lines[start])
    body = field[2:]
    match = _STATEMENT_RE.match(field) or _STATEMENT_BARE_RE.match(field)
    name = (match.group("name") or "").upper() or None if match else None
    rest = body[len(name):] if name else body
    tokens = rest.strip().split(None, 1)
    operation = tokens[0].upper() if tokens else ""
    operand = tokens[1].strip() if len(tokens) > 1 else ""

    consumed = 1
    cursor = start
    while True:
        record = raw_lines[cursor]
        current_field = statement_field(record)
        by_comma = current_field.rstrip().endswith(",")
        by_column = continuation_flag(record) and not _is_comment(current_field)
        if not (by_comma or by_column):
            break
        nxt = cursor + 1
        if nxt >= len(raw_lines):
            notes.append(
                f"{path}:{cursor + 1}: statement asks to be continued but the "
                f"member ends here; operands after this point are missing"
            )
            break
        next_field = statement_field(raw_lines[nxt])
        if _is_comment(next_field):
            notes.append(
                f"{path}:{nxt + 1}: continuation interrupted by a comment "
                f"record; the statement beginning at line {start + 1} is "
                f"reported with the operands recovered so far"
            )
            break
        if not _is_continuation_record(next_field):
            notes.append(
                f"{path}:{nxt + 1}: expected a continuation record (// then a "
                f"blank name field, operand in columns "
                f"{CONTINUATION_OPERAND_COLS[0] + 1}-"
                f"{CONTINUATION_OPERAND_COLS[1] + 1}) but found a new "
                f"statement; the one beginning at line {start + 1} is "
                f"reported with the operands recovered so far"
            )
            break
        addition = next_field[2:].strip()
        operand = f"{operand}{addition}" if operand else addition
        consumed += 1
        cursor = nxt

    return consumed, operand, name, operation, notes


def _build_step(
    name: Optional[str], operand: str, path: str, lineno: int
) -> Step:
    items, _problems = split_operands(operand)
    program = proc = None
    for item in items:
        key, value = _key_value(item)
        if key == "PGM":
            program = value.upper()
        elif key == "PROC":
            proc = value.upper()
        elif key == "" and proc is None and program is None and value:
            # `EXEC MYPROC` — a bare procedure invocation.
            proc = value.upper()
    return Step(
        name=name, program=program, proc=proc,
        evidence=Evidence(path, lineno), operand=operand,
    )


def _build_dd(
    *,
    ddname: str,
    step: Optional[Step],
    operand: str,
    path: str,
    lineno: int,
    lines: Tuple[int, ...],
    concatenation_index: int,
) -> Tuple[DDStatement, List[str], Optional[str]]:
    items, problems = split_operands(operand)
    notes = [f"{path}:{lineno}: {p}" for p in problems]
    # `//PRC001.FILEIN DD ...` overrides FILEIN inside procedure step PRC001.
    # The DD NAME is what a COBOL `ASSIGN TO` names, so that is what `ddname`
    # holds; the qualifier is kept beside it rather than folded in, because a
    # join on `PRC001.FILEIN` would match no SELECT anywhere.
    proc_step: Optional[str] = None
    if "." in ddname:
        proc_step, _, ddname = ddname.partition(".")
    dsn = disp = sysout = None
    dcb: Optional[DCB] = None
    in_stream = False
    in_stream_kind: Optional[str] = None
    dummy = False
    parameters: List[Tuple[str, str]] = []

    for item in items:
        key, value = _key_value(item)
        parameters.append((key, value))
        if key in {"DSN", "DSNAME"}:
            dsn = value.upper()
        elif key == "DISP":
            disp = value.upper()
        elif key == "DCB":
            dcb = parse_dcb(value)
        elif key == "SYSOUT":
            sysout = value
        elif key == "" and value == "*":
            in_stream = True
            in_stream_kind = "*"
        elif key == "" and value.upper() == "DATA":
            in_stream = True
            in_stream_kind = "DATA"
        elif key == "" and value.upper() == "DUMMY":
            dummy = True

    gdg = None
    member = None
    if dsn:
        gdg_match = _GDG_RE.search(dsn)
        if gdg_match:
            gdg = gdg_match.group("rel")
        else:
            member_match = _MEMBER_RE.search(dsn)
            if member_match:
                member = member_match.group("member")

    dd = DDStatement(
        ddname=ddname,
        step=step.name if step else None,
        program=step.program if step else None,
        proc_step=proc_step,
        dsn=dsn,
        disp=disp,
        dcb=dcb,
        sysout=sysout,
        in_stream=in_stream,
        in_stream_kind=in_stream_kind,
        dummy=dummy,
        concatenation_index=concatenation_index,
        gdg_relative=gdg,
        member=member,
        evidence=Evidence(path, lineno),
        lines=lines,
        operand=operand,
        parameters=tuple(parameters),
    )
    return dd, notes, in_stream_kind


# --------------------------------------------------------------------------
# Tree scan
# --------------------------------------------------------------------------

#: Extensions treated as JCL. Deliberately narrow: this module must never be
#: pointed at a data file, and an over-broad glob is one way that happens.
JCL_SUFFIXES: Tuple[str, ...] = (".jcl", ".jcls", ".job")

#: Extensions treated as PROC members. Parsed with the same reader.
PROC_SUFFIXES: Tuple[str, ...] = (".prc", ".proc")


def parse_member(path: Path, root: Optional[Path] = None) -> JclMember:
    """Parse one member from disk. Text only — never opened in binary mode."""
    path = Path(path)
    rel = path.as_posix()
    if root is not None:
        try:
            rel = path.relative_to(root).as_posix()
        except ValueError:
            rel = path.as_posix()
    text = path.read_text(encoding="utf-8", errors="replace")
    return parse_text(text, rel)


def scan(root: Path, include_procs: bool = False) -> JclInventory:
    """Parse every JCL member under ``root``, sorted by posix path (R8)."""
    root = Path(root)
    if not root.exists():
        raise FileNotFoundError(f"JCL scan root does not exist: {root}")
    suffixes = JCL_SUFFIXES + (PROC_SUFFIXES if include_procs else ())
    if root.is_file():
        candidates = [root]
        base = root.parent
    else:
        candidates = [
            p for p in root.rglob("*")
            if p.is_file()
            and p.suffix.lower() in suffixes
            and ".git" not in p.parts
        ]
        base = root
    members = tuple(
        parse_member(p, base)
        for p in sorted(candidates, key=lambda p: p.as_posix())
    )
    return JclInventory(root=root.as_posix(), members=members)
