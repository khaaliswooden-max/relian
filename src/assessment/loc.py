"""WP-1.4 — LOC inventory.

Pricing reads these numbers, so the counting rules are stated exactly and are
reproduced verbatim in the report appendix. Every rule below is implemented in
:func:`count`; nothing is rounded, estimated, or inferred.

COUNTING RULES
--------------

**physical**
    Every line in the file, counted after normalising ``\\r\\n`` and bare
    ``\\r`` to ``\\n``. A trailing newline does not create an extra line: a
    file ending ``"…RUN.\\n"`` has the same physical count as one ending
    ``"…RUN."``. Line-ending style therefore never changes the price (R8).

**comment**
    A line whose indicator column (column 7 in fixed format) is ``*`` or ``/``,
    or, in free format, whose first non-blank characters are ``*`` or ``*>``.

**blank**
    A line that is not a comment and whose code area contains only whitespace.
    In fixed format the code area is columns 8–72, so a line carrying only a
    sequence number in columns 1–6 is **blank**, not code.

**code**
    ``physical − comment − blank``. Reported implicitly; the three counted
    categories partition the file exactly, and this is asserted by a test.

**logical**
    The number of COBOL *statements*, taken from the same statement extraction
    the coverage analyzer uses — the ANTLR tree when the program parses
    cleanly, otherwise the documented token scan. It is **not** a regex over
    lines, and it is **not** a count of periods. ``logical_method`` records
    which extraction produced it, and ``logical`` is ``None`` when neither
    could recover statements (R1).

**dead_paragraphs**
    Paragraphs that no control-flow construct can reach. A paragraph is
    considered *reached* if:

    * it is the first paragraph of the PROCEDURE DIVISION (the entry point), or
    * its name appears as a target of ``PERFORM`` or ``GO TO`` anywhere in the
      program, including inside a ``PERFORM … THRU …`` span, in which case every
      paragraph textually between the two endpoints is reached, or
    * it is reachable by fall-through from a reached paragraph — that is, the
      previous paragraph is reached and does not end in an unconditional
      transfer (``GO TO``, ``STOP RUN``, ``GOBACK``, ``EXIT PROGRAM``).

    Reachability is computed to a fixed point, so a chain of PERFORMs is
    followed. ``ALTER`` defeats this analysis by design: a program containing
    ``ALTER`` can redirect a ``GO TO`` at run time, so when ``ALTER`` is present
    no paragraph is reported dead and ``note`` says why. Reporting a paragraph
    dead when ``ALTER`` could reach it would be a guess presented as a finding.
"""

from __future__ import annotations

import re
from typing import Dict, List, Optional, Set, Tuple

from .coverage import (
    GOTO_TARGETS_RE,
    ScannedSource,
    StatementHit,
    _antlr_parse,
    goto_targets,
    scan_source,
    statements_by_antlr,
    statements_by_token_scan,
    word_re,
)
from .models import LocResult

_PERFORM_RE = word_re(
    r"PERFORM\s+([A-Z0-9][A-Z0-9\-]*)(?:\s+(?:THRU|THROUGH)\s+([A-Z0-9][A-Z0-9\-]*))?"
)
_GOTO_RE = GOTO_TARGETS_RE
_ALTER_RE = word_re(r"ALTER\s+([A-Z0-9][A-Z0-9\-]*)")
_TERMINATORS = ("STOP RUN", "GOBACK", "EXIT PROGRAM")

# PERFORM VARYING/UNTIL/TIMES take a control variable or count, not a paragraph.
_PERFORM_NON_TARGETS = frozenset({"VARYING", "UNTIL", "WITH", "TEST", "FOREVER"})


def _statement_hits(source: str, scanned: ScannedSource) -> Tuple[Tuple[StatementHit, ...], str]:
    """Statements plus the method that produced them (mirrors coverage.analyze)."""
    try:
        tree, errors, parser_mod = _antlr_parse(scanned.antlr_source())
    except Exception:
        tree, errors, parser_mod = None, ("parser raised",), None
    if tree is not None and parser_mod is not None and not errors:
        try:
            hits = statements_by_antlr(tree, parser_mod)
            if hits:
                return hits, "antlr_tree"
        except Exception:
            pass
    hits = statements_by_token_scan(scanned)
    return hits, "token_scan" if hits else "none"


def _paragraph_order(scanned: ScannedSource) -> Tuple[str, ...]:
    return scanned.paragraph_names()


def _referenced_paragraphs(
    scanned: ScannedSource, order: Tuple[str, ...]
) -> Tuple[Set[str], bool]:
    """Paragraph names named by PERFORM / GO TO, and whether ALTER is present."""
    index = {name: i for i, name in enumerate(order)}
    referenced: Set[str] = set()
    alter_present = False

    for line in scanned.procedure_lines():
        if line.is_comment or not line.code.strip():
            continue
        code = line.code.upper()
        if _ALTER_RE.search(code):
            alter_present = True
        for m in _PERFORM_RE.finditer(code):
            first, last = m.group(1), m.group(2)
            if first in _PERFORM_NON_TARGETS:
                continue
            referenced.add(first)
            if last:
                referenced.add(last)
                if first in index and last in index:
                    lo, hi = sorted((index[first], index[last]))
                    referenced.update(order[lo:hi + 1])
        referenced.update(goto_targets(code))
    return referenced, alter_present


def _falls_through(scanned: ScannedSource, paragraph: str) -> bool:
    """Does control run off the end of this paragraph into the next one?"""
    body = [l for l in scanned.procedure_lines()
            if l.paragraph == paragraph and not l.is_comment and l.code.strip()]
    for line in reversed(body):
        code = " ".join(line.code.upper().split())
        if not code or code == f"{paragraph}.":
            continue
        if any(t in code for t in _TERMINATORS) or _GOTO_RE.search(code):
            return False
        return True
    return True


def dead_paragraphs(scanned: ScannedSource) -> Tuple[Tuple[str, ...], Optional[str]]:
    """Unreachable paragraphs, or ``()`` plus a note explaining why not computed."""
    order = _paragraph_order(scanned)
    if not order:
        return (), None
    referenced, alter_present = _referenced_paragraphs(scanned, order)
    if alter_present:
        return (), ("ALTER present: GO TO targets can be rewritten at run time, so "
                    "reachability is not decidable from source — no paragraph is "
                    "reported dead")

    reached: Set[str] = {order[0]}
    changed = True
    while changed:                      # fixed point over PERFORM/GO TO + fall-through
        changed = False
        for i, name in enumerate(order):
            if name in reached:
                continue
            if name in referenced:
                reached.add(name)
                changed = True
                continue
            if i > 0 and order[i - 1] in reached and _falls_through(scanned, order[i - 1]):
                reached.add(name)
                changed = True
    return tuple(n for n in order if n not in reached), None


def count(source: str, kind: str = "program", program_id: str = "") -> LocResult:
    """Full LOC inventory for one source file. See module docstring for rules."""
    normalised = source.replace("\r\n", "\n").replace("\r", "\n")
    scanned = scan_source(normalised)

    lines = list(scanned.lines)
    # A trailing newline produces a final empty element; it is not a line.
    if lines and normalised.endswith("\n"):
        lines = lines[:-1]

    physical = len(lines)
    comment = sum(1 for l in lines if l.is_comment)
    blank = sum(1 for l in lines if l.is_blank)

    if kind == "program":
        hits, method = _statement_hits(normalised, scanned)
        logical: Optional[int] = len(hits) if method != "none" else None
        dead, note = dead_paragraphs(scanned)
    else:
        # Copybooks and JCL have no PROCEDURE DIVISION; counting statements in
        # them would be counting something that is not there.
        hits, method, logical, dead = (), "not_applicable", None, ()
        note = f"logical statement count not applicable to kind={kind}"

    return LocResult(
        program_id=program_id,
        physical=physical,
        logical=logical,
        comment=comment,
        blank=blank,
        dead_paragraphs=dead,
        logical_method=method,
        note=note,
    )


def portfolio_totals(results: List[LocResult]) -> Dict[str, Optional[int]]:
    """Sums, with ``logical`` None-safe: unmeasured programs are excluded and
    counted separately rather than silently treated as zero."""
    measurable = [r for r in results if r.logical is not None]
    return {
        "physical": sum(r.physical for r in results),
        "comment": sum(r.comment for r in results),
        "blank": sum(r.blank for r in results),
        "code": sum(r.physical - r.comment - r.blank for r in results),
        "logical": sum(r.logical or 0 for r in measurable) if measurable else None,
        "logical_programs_measured": len(measurable),
        "logical_programs_unmeasured": len(results) - len(measurable),
    }
