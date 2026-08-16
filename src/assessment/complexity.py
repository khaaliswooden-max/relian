"""WP-1.5 — complexity metrics.

Every formula is stated here and reproduced verbatim in the report appendix.
**No thresholds live in this module** — nothing here decides whether a number
is good or bad. Thresholds are policy and belong to :mod:`risk`.

FORMULAS
--------

``decision_points``
    Count of branch-introducing constructs in the PROCEDURE DIVISION:
    ``IF``, ``WHEN`` (each ``EVALUATE`` branch, including ``WHEN OTHER``),
    ``UNTIL``, ``VARYING``, ``TIMES``, ``AT END``, ``NOT AT END``,
    ``INVALID KEY``, ``NOT INVALID KEY``, ``ON SIZE ERROR``,
    ``ON OVERFLOW``, ``ON EXCEPTION``, and each ``AND`` / ``OR`` appearing in a
    condition. ``ELSE`` is **not** counted: it is the other side of a branch
    already counted at its ``IF``.

``cyclomatic``
    ``decision_points + 1``. This is McCabe's formula for a single connected
    unit. It is computed per paragraph and, separately, for the whole program
    from the program's own decision points — the program figure is
    ``program_decision_points + 1``, not the sum of the paragraph figures,
    because summing would count each paragraph's ``+1`` again.

``goto_count``
    Occurrences of ``GO TO``. ``GO TO … DEPENDING ON`` counts once per target,
    because each target is a distinct edge.

``goto_density``
    ``goto_count / statements``. ``None`` when the statement count could not be
    measured — never 0.0 as a stand-in.

``alter_present``
    Whether any ``ALTER`` statement appears. Boolean, not a count, because one
    ``ALTER`` is already enough to make static control flow undecidable.

``perform_thru_spans``
    Each ``PERFORM x THRU y`` as the string ``"x THRU y"``. These spans are why
    paragraph boundaries cannot be treated as function boundaries.

``exec_cics_count`` / ``exec_sql_count``
    ``EXEC CICS`` and ``EXEC SQL`` statement occurrences.

``copybook_fan_out``
    Distinct ``COPY`` targets named by this program, quotes stripped. Fan-**in**
    is a portfolio-level inversion of this map and is computed by
    :func:`copybook_fan_in` over all programs.

``call_targets``
    Distinct ``CALL`` targets. Literal targets are recorded as written;
    identifier targets (dynamic CALL) are recorded as the identifier name.

``max_nesting_depth``
    Maximum depth of open scopes, tracked with a **stack** rather than a
    counter. A scope is opened by ``IF``, ``EVALUATE``, ``SEARCH``, or an
    *inline* ``PERFORM`` — one whose loop body is written in place, recognised
    as ``PERFORM UNTIL``, ``PERFORM VARYING``, ``PERFORM WITH TEST``,
    ``PERFORM FOREVER``, or ``PERFORM <n> TIMES``. A ``PERFORM <paragraph>``
    transfers control elsewhere and opens no scope here, so it does not count.

    A scope is closed by its own ``END-…`` terminator, and by nothing else: an
    ``END-…`` whose opener is not on the stack is **ignored** rather than
    decrementing the depth. That distinction is load-bearing — with a plain
    counter, an ``END-PERFORM`` or ``END-READ`` sitting inside an outer ``IF``
    cancels the ``IF``'s own depth, and every construct nested after it in that
    ``IF`` is undercounted. ``END-READ``, ``END-CALL``, ``END-STRING`` and
    ``END-UNSTRING`` are therefore inert here, because the statements they
    terminate are not counted as opening a scope in the first place.

    Openers and closers are processed in the order they appear on the line, so
    a scope opened and closed on one line still registers its depth. A period
    ends the sentence and closes every scope still open.
"""

from __future__ import annotations

import re
from typing import Dict, List, Optional, Sequence, Tuple

from .coverage import (
    ScannedSource,
    goto_targets as _goto_targets,
    scan_source,
    statements_by_token_scan,
    word_re,
)
from .models import ComplexityResult, Measured, ParagraphComplexity

_DECISION_PATTERNS: Tuple[Tuple[str, re.Pattern], ...] = (
    ("IF", word_re("IF")),
    ("WHEN", word_re("WHEN")),
    ("UNTIL", word_re("UNTIL")),
    ("VARYING", word_re("VARYING")),
    ("TIMES", word_re("TIMES")),
    ("AT END", word_re(r"(?:NOT\s+)?AT\s+END")),
    ("INVALID KEY", word_re(r"(?:NOT\s+)?INVALID\s+KEY")),
    ("ON SIZE ERROR", word_re(r"(?:NOT\s+)?ON\s+SIZE\s+ERROR")),
    ("ON OVERFLOW", word_re(r"(?:NOT\s+)?ON\s+OVERFLOW")),
    ("ON EXCEPTION", word_re(r"(?:NOT\s+)?ON\s+EXCEPTION")),
    ("AND", word_re("AND")),
    ("OR", word_re("OR")),
)

_ALTER_RE = word_re("ALTER")
_THRU_RE = word_re(r"PERFORM\s+([A-Z0-9][A-Z0-9\-]*)\s+(?:THRU|THROUGH)\s+([A-Z0-9][A-Z0-9\-]*)")
_COPY_RE = word_re(r"COPY\s+[\"']?([A-Z0-9][A-Z0-9\-_]*)[\"']?")
_CALL_RE = word_re(r"CALL\s+[\"']?([A-Z0-9][A-Z0-9\-_]*)[\"']?")
_EXEC_RE = word_re(r"EXEC\s+(CICS|SQL|DLI)")

# An *inline* PERFORM writes its loop body in place and is closed by
# END-PERFORM. `PERFORM SOME-PARA` transfers control instead and opens nothing,
# so treating every PERFORM as an opener would count ordinary paragraph calls
# as nesting.
_INLINE_PERFORM = word_re(
    r"PERFORM\s+(?:UNTIL|VARYING|FOREVER|WITH\s+TEST)"
    r"|PERFORM\s+[A-Z0-9][A-Z0-9\-]*\s+TIMES"
)

# Constructs that open a scope, and the terminator that closes each one.
_SCOPE_OPENERS: Tuple[Tuple[str, re.Pattern], ...] = (
    ("IF", word_re("IF")),
    ("EVALUATE", word_re("EVALUATE")),
    ("SEARCH", word_re("SEARCH")),
    ("PERFORM", _INLINE_PERFORM),
)
_SCOPE_CLOSER = word_re(r"END-(IF|EVALUATE|SEARCH|PERFORM|READ|CALL|STRING|UNSTRING)")


def _decision_points(code: str) -> int:
    total = 0
    for _, pattern in _DECISION_PATTERNS:
        total += len(pattern.findall(code))
    return total


def _procedure_text(scanned: ScannedSource) -> str:
    return "\n".join(
        l.code.upper() for l in scanned.procedure_lines()
        if not l.is_comment and l.code.strip()
    )


def _max_nesting_of_lines(lines: Sequence[str]) -> int:
    """Deepest stack of open scopes. See ``max_nesting_depth`` in the docstring."""
    stack: List[str] = []
    max_depth = 0
    for code in lines:
        events: List[Tuple[int, str, str]] = []
        for name, pattern in _SCOPE_OPENERS:
            events.extend((m.start(), "open", name) for m in pattern.finditer(code))
        events.extend(
            (m.start(), "close", m.group(1).upper()) for m in _SCOPE_CLOSER.finditer(code)
        )
        for _, kind, name in sorted(events, key=lambda e: (e[0], e[1])):
            if kind == "open":
                stack.append(name)
                max_depth = max(max_depth, len(stack))
            elif name in stack:
                while stack and stack.pop() != name:
                    pass          # an unterminated inner scope closes with its parent
            # a terminator whose opener was never counted is inert
        if code.rstrip().endswith("."):
            stack.clear()         # a period closes every scope still open
    return max_depth


def _max_nesting(scanned: ScannedSource) -> int:
    return _max_nesting_of_lines([
        line.code.upper()
        for line in scanned.procedure_lines()
        if not line.is_comment and line.code.strip()
    ])


def _paragraph_metrics(scanned: ScannedSource) -> Tuple[ParagraphComplexity, ...]:
    by_para: Dict[str, List[str]] = {}
    for line in scanned.procedure_lines():
        if line.is_comment or not line.code.strip() or line.paragraph is None:
            continue
        by_para.setdefault(line.paragraph, []).append(line.code.upper())

    scanned_hits = statements_by_token_scan(scanned)
    stmt_counts: Dict[str, int] = {}
    for h in scanned_hits:
        if h.paragraph:
            stmt_counts[h.paragraph] = stmt_counts.get(h.paragraph, 0) + 1

    out: List[ParagraphComplexity] = []
    for name in scanned.paragraph_names():
        text = "\n".join(by_para.get(name, []))
        dp = _decision_points(text)
        out.append(
            ParagraphComplexity(
                name=name,
                statements=stmt_counts.get(name, 0),
                decision_points=dp,
                cyclomatic=dp + 1,
                goto_count=len(_goto_targets(text)),
                max_nesting=_max_nesting_of_text(text),
            )
        )
    return tuple(out)


def _max_nesting_of_text(text: str) -> int:
    return _max_nesting_of_lines(text.splitlines())


def analyze(source: str, program_id: str = "", statements: Optional[int] = None) -> ComplexityResult:
    """Complexity metrics for one program. All formulas in the module docstring."""
    scanned = scan_source(source)
    text = _procedure_text(scanned)
    whole = "\n".join(
        l.code.upper() for l in scanned.lines if not l.is_comment and l.code.strip()
    )

    dp = _decision_points(text)
    gotos = _goto_targets(text)
    if statements is None:
        statements = len(statements_by_token_scan(scanned))

    density: Optional[Measured] = None
    if statements:
        density = Measured(
            round(len(gotos) / statements, 4),
            f"{len(gotos)} GO TO targets / {statements} statements in {program_id or 'program'}",
            "VERIFIED",
        )

    spans = tuple(sorted({f"{m.group(1).upper()} THRU {m.group(2).upper()}"
                          for m in _THRU_RE.finditer(text)}))
    execs = [m.group(1).upper() for m in _EXEC_RE.finditer(text)]

    return ComplexityResult(
        program_id=program_id,
        method="token_scan",
        cyclomatic=dp + 1,
        decision_points=dp,
        statements=statements,
        goto_count=len(gotos),
        goto_density=density,
        alter_present=bool(_ALTER_RE.search(text)),
        perform_thru_spans=spans,
        exec_cics_count=execs.count("CICS"),
        exec_sql_count=execs.count("SQL"),
        copybook_fan_out=tuple(sorted({m.group(1).upper() for m in _COPY_RE.finditer(whole)})),
        call_targets=tuple(sorted({m.group(1).upper() for m in _CALL_RE.finditer(text)})),
        max_nesting_depth=_max_nesting(scanned),
        paragraphs=_paragraph_metrics(scanned),
    )


def copybook_fan_in(results: Sequence[ComplexityResult]) -> Dict[str, List[str]]:
    """Invert fan-out: copybook -> the programs that COPY it, sorted."""
    fan_in: Dict[str, List[str]] = {}
    for r in results:
        for book in r.copybook_fan_out:
            fan_in.setdefault(book, []).append(r.program_id)
    return {book: sorted(set(progs)) for book, progs in sorted(fan_in.items())}


def external_interfaces(result: ComplexityResult) -> Tuple[str, ...]:
    """Every external touchpoint this program has, as a sorted tuple."""
    out: List[str] = []
    if result.exec_cics_count:
        out.append(f"EXEC CICS x{result.exec_cics_count}")
    if result.exec_sql_count:
        out.append(f"EXEC SQL x{result.exec_sql_count}")
    out.extend(f"CALL {t}" for t in result.call_targets)
    return tuple(sorted(out))
