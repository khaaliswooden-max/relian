"""WP-1.3 — construct coverage: what fraction of a program C1 can transpile.

Two analysis methods, and the result always says which one produced it.

``antlr_tree`` (graded VERIFIED)
    ``src/parsers/antlr/cobol`` is walked and every ``StatementContext`` is
    classified. Used **only** when the parse produced zero syntax errors, so a
    tree assembled by error recovery is never passed off as a clean parse.

``token_scan`` (graded PLAUSIBLE)
    A documented lexical scan, used when the ANTLR parse reports errors.

The grammar bundled in this repo (``src/parsers/grammars/Cobol85.g4``) is the
ProLeap COBOL-85 grammar vendored from ``antlr/grammars-v4``; its provenance,
licence and pinned upstream commit are recorded in
``docs/GRAMMAR_PROVENANCE.md``. It covers the COBOL-85 standard rather than a
subset, and the bench corpus parses cleanly under it — but the fallback is not
therefore obsolete, because real COBOL routinely is not COBOL-85:

* **Dialect extensions.** ``EXIT PERFORM`` (COBOL-2002), GnuCOBOL's
  ``BINARY-LONG``, and compiler directives before the IDENTIFICATION DIVISION
  are all outside the standard and are syntax errors under a COBOL-85 grammar,
  correctly.
* **Comment entries.** The free text after ``AUTHOR.`` or ``INSTALLATION.`` is
  reachable only through a ``*>CE`` marker that upstream's preprocessor
  inserts; this repo does not run that preprocessor.
* **COPY and REPLACE.** ``COPY`` is a lexer token in this grammar that no
  parser rule references — upstream consumes it in the separate
  ``Cobol85Preprocessor.g4``, vendored here but not yet run. A COPY-bearing
  program cannot parse cleanly, by construction.

So both methods exist, every result is labelled with the one that ran, and only
the tree path is graded VERIFIED (R1/R9). A program that reports syntax errors
falls to ``token_scan`` and is graded PLAUSIBLE rather than being reported as
having no constructs.

Token-scan counting rules (reproduced verbatim in the report appendix):

1. Source format is detected per file: **fixed** if any line carries ``*`` or
   ``/`` in column 7, or if at least 80% of non-blank lines are at least 7
   characters long with columns 1–6 either blank or all digits (a sequence
   number) and column 7 blank, ``*``, ``/`` or ``-``; otherwise **free**. In
   fixed format the code area is columns 8–72 and column 7 is the indicator;
   in free format the whole line is code.
2. A line is a comment if its indicator column is ``*`` or ``/``, or if the
   line's first non-blank characters are ``*>``.
3. Only the PROCEDURE DIVISION is scanned for statements.
4. A statement is counted at each verb token that appears in a
   *statement-start position*: the first token of a line, or a token
   immediately following ``.``, ``THEN``, ``ELSE``, or an ``END-…`` scope
   terminator. This deliberately under-counts verbs buried mid-clause (e.g.
   ``WHEN 1 DISPLAY X``); under-counting a construct is a smaller lie than
   guessing at one, and the grade says PLAUSIBLE. A verb is classified
   supported if the dispatch table holds the bare verb or its qualified
   two-word form (``EXIT PROGRAM``); a qualified-only verb whose qualifier
   is absent or unrecovered counts unsupported, in the same under-counting
   direction.
5. ``EXEC CICS`` / ``EXEC SQL`` / ``EXEC DLI`` count as one statement with verb
   ``EXEC`` and the product recorded as its context.
6. A paragraph label is a line whose code area is a single name followed by a
   period; a section header additionally has ``SECTION`` before the period.

ANTLR-tree counting rules:

7. A statement is counted at each ``statement`` context in the parse tree.
   Nested statements count in their own right — the statements inside an
   ``IF``'s THEN branch are counted as well as the ``IF`` — so the tree and the
   scan measure comparable things.
8. The verb reported for a statement is read from an explicit table,
   ``_STATEMENT_VERBS``, with one row per alternative of the grammar's
   ``statement`` rule. The table is checked against the generated parser on
   every walk, and a mismatch raises rather than silently dropping statements
   from the count (R2).
9. Scope terminators (``END-IF``, ``END-PERFORM``, …), ``ELSE`` and ``WHEN``
   are counted by the token scan, which is line-oriented, but not by the tree,
   where they are part of their enclosing statement rather than statements
   themselves. The two methods therefore report different *totals* for the same
   program; each ratio is internally consistent and is labelled with the method
   that produced it.
10. As with rule 4, a two-word verb is resolved where the tree makes it visible:
    ``PERFORM VARYING`` and ``EXIT PROGRAM`` are distinguished from out-of-line
    ``PERFORM`` and paragraph ``EXIT`` by the statement's second token.

A statement is SUPPORTED iff its verb is in
:func:`src.assessment.supported.supported_verbs`, which reads the transpiler's
dispatch table. Nothing here maintains its own opinion of what C1 supports.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Dict, FrozenSet, List, Optional, Sequence, Tuple

from .models import (
    ConstructHit,
    CoverageResult,
    DataFeatureHit,
    FileRecord,
    Measured,
)
from .supported import (
    STATUS_SUPPORTED,
    data_feature_provenance,
    registry_provenance,
    supported_data_features,
    supported_verbs,
)

# COBOL-85 procedural verbs. This is a fact about the *language*, not a claim
# about Relian's capabilities — it defines what counts as a statement, while
# supported_verbs() decides which of those Relian can transpile.
COBOL_VERBS = frozenset("""
ACCEPT ADD ALTER CALL CANCEL CLOSE COMPUTE CONTINUE DELETE DISPLAY DIVIDE
ENTER ENTRY EVALUATE EXHIBIT EXIT GENERATE GO GOBACK IF INITIALIZE INITIATE
INSPECT MERGE MOVE MULTIPLY OPEN PERFORM READ RELEASE RETURN REWRITE SEARCH
SET SORT START STOP STRING SUBTRACT SUPPRESS TERMINATE UNSTRING UNLOCK USE
WRITE EXEC COPY REPLACE SERVICE CHAIN
""".split())

# Tokens after which a verb begins a new statement.
_START_CONTEXTS = frozenset({".", "THEN", "ELSE", "OTHERWISE"})

_DIVISION_RE = re.compile(r"^\s*(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\b", re.I)
_PARAGRAPH_NAME_RE = re.compile(r"^\s*([A-Z0-9][A-Z0-9\-]*)\s*\.\s*$", re.I)
_SECTION_RE = re.compile(r"^\s*([A-Z0-9][A-Z0-9\-]*)\s+SECTION\s*\.\s*$", re.I)
_TOKEN_RE = re.compile(r"[A-Z0-9][A-Z0-9\-]*|\.")


def word_re(pattern: str) -> re.Pattern:
    """Match ``pattern`` as a whole COBOL word.

    ``\\b`` is wrong for COBOL: a hyphen is a non-word character, so ``\\bIF\\b``
    matches the ``IF`` inside ``END-IF`` and every scope terminator inflates the
    decision-point count. COBOL words may contain hyphens, so the boundary must
    exclude ``-`` on both sides.
    """
    return re.compile(rf"(?<![\w-])(?:{pattern})(?![\w-])", re.I)


# GO TO target list. Stops at DEPENDING so that `GO TO A B C DEPENDING ON N`
# yields three targets, not "…DEPENDING ON N" as well.
GOTO_TARGETS_RE = re.compile(
    r"(?<![\w-])GO\s+TO\s+((?:(?!DEPENDING(?![\w-]))[A-Z0-9][A-Z0-9\-]*\s*)+)", re.I
)


def goto_targets(code: str) -> List[str]:
    out: List[str] = []
    for m in GOTO_TARGETS_RE.finditer(code):
        out.extend(name.rstrip(".") for name in m.group(1).split())
    return out


# Reserved words that can legally stand alone on a line followed by a period,
# and would otherwise be misread as paragraph names.
_NOT_PARAGRAPH_NAMES = frozenset({"ELSE", "THEN", "OTHER", "CONTINUE", "NEXT"})


def _paragraph_label(code: str) -> Optional[str]:
    """A lone ``NAME.`` line — unless NAME is a reserved word.

    ``GOBACK.``, ``EXIT.`` and ``END-IF.`` are lexically indistinguishable from
    a paragraph label. A paragraph may not be named after a reserved word, so
    the reserved reading is the correct one. Without this check ``GOBACK`` and
    ``EXIT`` vanish from the construct inventory, and every scope terminator on
    its own line becomes a phantom paragraph that then pollutes the
    per-paragraph complexity table and the dead-paragraph analysis.
    """
    m = _PARAGRAPH_NAME_RE.match(code)
    if not m:
        return None
    name = m.group(1).upper()
    if name in COBOL_VERBS or name in _NOT_PARAGRAPH_NAMES or name.startswith("END-"):
        return None
    return name


# --------------------------------------------------------------------------
# Shared lexical model (also used by loc.py and complexity.py)
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class CodeLine:
    lineno: int          # 1-based
    raw: str
    code: str            # code area only, trailing whitespace stripped
    is_comment: bool
    is_blank: bool
    division: Optional[str]
    paragraph: Optional[str]
    section: Optional[str]
    # Column 7 in fixed format, " " in free format. Kept rather than recomputed
    # so that the ANTLR pre-pass (`ScannedSource.antlr_source`) and the token
    # scan read the same classification of the same character.
    indicator: str = " "


@dataclass(frozen=True)
class ScannedSource:
    source_format: str               # "fixed" | "free"
    lines: Tuple[CodeLine, ...]

    def procedure_lines(self) -> Tuple[CodeLine, ...]:
        return tuple(l for l in self.lines if l.division == "PROCEDURE")

    def data_lines(self) -> Tuple[CodeLine, ...]:
        return tuple(l for l in self.lines if l.division == "DATA")

    def paragraph_names(self) -> Tuple[str, ...]:
        seen: List[str] = []
        for l in self.procedure_lines():
            if l.is_comment:
                continue
            name = _paragraph_label(l.code)
            if name and not _SECTION_RE.match(l.code) and name not in seen:
                seen.append(name)
        return tuple(seen)

    def antlr_source(self) -> str:
        """Code area only, with the indicator column applied. The pre-pass.

        ``Cobol85.g4`` is a grammar for the *code area*. It has no rule for a
        sequence number, a comment line, a debugging line or a continuation —
        upstream those are removed by ``Cobol85Preprocessor.g4`` before the
        parser ever runs. Feeding raw fixed-format text to it therefore
        guarantees syntax errors that are artifacts of the card layout rather
        than facts about the program. This method is that missing pre-pass, and
        it is deliberately the *only* transformation applied: what ANTLR sees
        is the customer's code area and nothing invented.

        In **fixed** format, for each line:

        * columns 1–6 (the sequence-number area) are dropped;
        * column 7 is the indicator and is dropped after being acted on;
        * columns 8–72 are the code area and are kept **unstripped**, because
          trailing spaces inside a continued literal are part of that literal;
        * columns 73+ (the identification area) are dropped.

        Indicator semantics, all four of them:

        ``*`` / ``/``
            Comment (``/`` also form-feeds). Emitted as an empty line, never
            deleted — the line must keep its number so an ANTLR diagnostic
            points at the right line of the customer's file.
        ``D`` / ``d``
            Debugging line. Compiled only under ``WITH DEBUGGING MODE``, which
            this pre-pass does not assume, so it is treated as a comment — the
            same reading a compiler gives it by default. Blanked, not deleted.
        ``-``
            Continuation. The code area is appended to the last emitted code
            line and an empty line is left in its place, so the continued
            statement keeps the line number of where it *starts*, which is what
            ``StatementHit.line`` and the parse diagnostics report. When the
            line being continued has an unterminated literal and the
            continuation opens with the matching quote, that quote is the
            resumption marker rather than literal content and is dropped;
            otherwise the two code areas are joined with no separator, which is
            how a word split across a card boundary rejoins.
        anything else (normally a space)
            Ordinary code area.

        In **free** format the whole line is code and there is no indicator
        column, so only the comment rule applies.
        """
        out: List[str] = []
        last_code: Optional[int] = None      # index in `out` of the last code line
        for line in self.lines:
            if line.is_comment or line.indicator in "Dd":
                out.append("")
                continue
            code = self._code_area(line)
            if line.indicator == "-" and last_code is not None:
                out[last_code] = _join_continuation(out[last_code], code)
                out.append("")
                continue
            out.append(code)
            last_code = len(out) - 1
        return "\n".join(out)

    def _code_area(self, line: "CodeLine") -> str:
        """Columns 8–72 with trailing spaces intact (see `antlr_source`)."""
        if self.source_format != "fixed":
            return line.code
        return line.raw[7:72] if len(line.raw) > 7 else ""


def _join_continuation(head: str, tail: str) -> str:
    """Append a continuation line's code area to the line it continues.

    Leading spaces on the continuation are card padding, not content, so they
    are dropped. If ``head`` has an unterminated literal and ``tail`` opens
    with that literal's quote character, the quote marks where the literal
    resumes and is not itself part of the literal — it goes. Otherwise the two
    areas are concatenated with nothing between them, which is what rejoins a
    COBOL word split across a card boundary.
    """
    stripped = tail.lstrip()
    for quote in ('"', "'"):
        if head.count(quote) % 2 == 1 and stripped.startswith(quote):
            return head + stripped[1:]
    return head + stripped


def detect_format(raw_lines: Sequence[str]) -> str:
    """Fixed vs free format. See rule 1 in the module docstring."""
    non_blank = [l for l in raw_lines if l.strip()]
    if not non_blank:
        return "fixed"
    if any(len(l) > 6 and l[6] in "*/" for l in non_blank):
        return "fixed"

    def looks_fixed(line: str) -> bool:
        if len(line) < 7:
            return False
        seq = line[:6]
        return (seq.strip() == "" or seq.strip().isdigit()) and line[6] in " */-"

    return "fixed" if sum(map(looks_fixed, non_blank)) >= 0.8 * len(non_blank) else "free"


def scan_source(source: str) -> ScannedSource:
    """Split a COBOL source into classified lines. Purely lexical, no parse."""
    raw_lines = source.replace("\r\n", "\n").replace("\r", "\n").split("\n")
    fmt = detect_format(raw_lines)

    out: List[CodeLine] = []
    division: Optional[str] = None
    paragraph: Optional[str] = None
    section: Optional[str] = None

    for idx, raw in enumerate(raw_lines, start=1):
        if fmt == "fixed":
            indicator = raw[6] if len(raw) > 6 else " "
            code = raw[7:72] if len(raw) > 7 else ""
            is_comment = indicator in "*/"
        else:
            indicator = " "
            code = raw
            is_comment = raw.lstrip().startswith("*>") or raw.lstrip().startswith("*")
        code = code.rstrip()
        is_blank = not code.strip() and not is_comment

        if not is_comment:
            dm = _DIVISION_RE.match(code)
            if dm:
                division = dm.group(1).upper()
                paragraph = None
                section = None
            elif division == "PROCEDURE":
                sm = _SECTION_RE.match(code)
                pm = _paragraph_label(code)
                if sm:
                    section = sm.group(1).upper()
                    paragraph = None
                elif pm:
                    paragraph = pm

        out.append(
            CodeLine(
                lineno=idx,
                raw=raw,
                code=code,
                is_comment=is_comment,
                is_blank=is_blank,
                division=division,
                paragraph=paragraph,
                section=section,
                indicator=indicator,
            )
        )
    return ScannedSource(source_format=fmt, lines=tuple(out))


# --------------------------------------------------------------------------
# Statement extraction
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class StatementHit:
    verb: str
    line: int
    paragraph: Optional[str]
    context: Optional[str]
    # Token immediately following the verb on the same line, if any. Needed
    # because the transpiler's dispatch table may support only a QUALIFIED
    # form of a verb (WP-1.5.5: "EXIT PROGRAM" is supported, bare EXIT --
    # paragraph exit -- is not). Without the qualifier every paragraph EXIT
    # in a customer codebase would count as supported, which is an overclaim.
    next_tok: Optional[str] = None


def _exec_product(code_upper: str, pos: int) -> str:
    tail = code_upper[pos:].split()
    return f"EXEC {tail[1]}" if len(tail) > 1 else "EXEC"


def statements_by_token_scan(scanned: ScannedSource) -> Tuple[StatementHit, ...]:
    """Rule 4 of the module docstring."""
    hits: List[StatementHit] = []
    for line in scanned.procedure_lines():
        if line.is_comment or not line.code.strip():
            continue
        code = line.code.upper()
        if _paragraph_label(line.code) or _SECTION_RE.match(line.code):
            continue
        prev: Optional[str] = None          # None == start of line
        toks = [m.group(0) for m in _TOKEN_RE.finditer(code)]
        starts = [m.start() for m in _TOKEN_RE.finditer(code)]
        for k, tok in enumerate(toks):
            at_start = prev is None or prev in _START_CONTEXTS or prev.startswith("END-")
            if tok in COBOL_VERBS and at_start:
                context = _exec_product(code, starts[k]) if tok == "EXEC" else None
                nxt = toks[k + 1] if k + 1 < len(toks) and toks[k + 1] != "." else None
                hits.append(StatementHit(tok, line.lineno, line.paragraph, context, nxt))
            prev = tok
    return tuple(hits)


_EXPECTING_SET_RE = re.compile(r"expecting \{[^}]*\}")


def _normalise_parse_error(msg: str) -> str:
    """Elide ANTLR's expected-token set from a syntax-error message.

    ANTLR's ALL(*) prediction caches DFA state on the parser's ATN simulator,
    which is shared across parses in a process. The *token* it reports is
    stable, but the ``expecting {…}`` set it prints is not: it grows as the
    cache warms, so the same file parsed twice in one run yields two different
    message strings. Since the ledger artifact is hashed, that difference is a
    determinism bug (R8). The expected-token set is a parser-internal artifact
    rather than a fact about the customer's code, so it is elided and the
    diagnostic — position and offending token — is kept.
    """
    return _EXPECTING_SET_RE.sub("expecting {...}", msg)


def _antlr_parse(source: str):
    """Parse with the bundled ANTLR grammar. Returns (tree, errors)."""
    import antlr4
    from antlr4.error.ErrorListener import ErrorListener

    from src.parsers.antlr.cobol.Cobol85Lexer import Cobol85Lexer
    from src.parsers.antlr.cobol.Cobol85Parser import Cobol85Parser

    class _Collect(ErrorListener):
        def __init__(self) -> None:
            self.errors: List[str] = []

        def syntaxError(self, recognizer, offending, line, column, msg, e):  # noqa: N802
            if len(self.errors) < 50:
                self.errors.append(f"line {line}:{column} {_normalise_parse_error(msg)}")

    collector = _Collect()
    lexer = Cobol85Lexer(antlr4.InputStream(source))
    lexer.removeErrorListeners()
    lexer.addErrorListener(collector)
    parser = Cobol85Parser(antlr4.CommonTokenStream(lexer))
    parser.removeErrorListeners()
    parser.addErrorListener(collector)
    # `startRule: compilationUnit EOF` is the grammar's declared entry point.
    # Entering at `compilationUnit` instead would let a file whose tail the
    # grammar cannot parse report zero errors over its prefix and be graded
    # VERIFIED on a tree covering only part of the program.
    tree = parser.startRule()
    return tree, tuple(collector.errors), Cobol85Parser


# --------------------------------------------------------------------------
# The verb -> context map
# --------------------------------------------------------------------------

# One row per alternative of the vendored grammar's `statement` rule, keyed by
# the generated context class name. This is a table on purpose: it is the whole
# interface between `Cobol85.g4` and this analyzer, and it has to be auditable
# by reading it rather than by tracing conditionals. `_verify_statement_map`
# below asserts, at import time, that it covers the grammar exactly — so a
# grammar upgrade that adds or renames a statement fails loudly here instead of
# silently dropping those statements out of every coverage figure (R2).
#
# The value is the verb the transpiler's dispatch table is keyed by, which is
# not always the context's first token:
#
#   * `EXEC CICS` / `EXEC SQL` / `EXEC SQL IMS` collapse to the verb `EXEC`
#     with the product carried separately as the hit's context, matching rule 5
#     of the token scan so the two methods count the same thing.
#   * `GO TO` is two words and is reported as `GO TO`, because a bare `GO` is
#     not what a reader (or the dispatch table) means.
#   * Every other row is the single leading keyword.
#
# Qualified two-word forms that are *optional* in the grammar — `EXIT PROGRAM`
# vs a bare paragraph `EXIT`, `PERFORM VARYING` vs an out-of-line `PERFORM` —
# are NOT resolved here, because which one it is depends on the parse, not on
# the rule. They are resolved per hit by `_qualifier`, below.
_STATEMENT_VERBS: Dict[str, str] = {
    "AcceptStatementContext": "ACCEPT",
    "AddStatementContext": "ADD",
    "AlterStatementContext": "ALTER",
    "CallStatementContext": "CALL",
    "CancelStatementContext": "CANCEL",
    "CloseStatementContext": "CLOSE",
    "ComputeStatementContext": "COMPUTE",
    "ContinueStatementContext": "CONTINUE",
    "DeleteStatementContext": "DELETE",
    "DisableStatementContext": "DISABLE",
    "DisplayStatementContext": "DISPLAY",
    "DivideStatementContext": "DIVIDE",
    "EnableStatementContext": "ENABLE",
    "EntryStatementContext": "ENTRY",
    "EvaluateStatementContext": "EVALUATE",
    "ExhibitStatementContext": "EXHIBIT",
    "ExecCicsStatementContext": "EXEC",
    "ExecSqlStatementContext": "EXEC",
    "ExecSqlImsStatementContext": "EXEC",
    "ExitStatementContext": "EXIT",
    "GenerateStatementContext": "GENERATE",
    "GobackStatementContext": "GOBACK",
    "GoToStatementContext": "GO TO",
    "IfStatementContext": "IF",
    "InitializeStatementContext": "INITIALIZE",
    "InitiateStatementContext": "INITIATE",
    "InspectStatementContext": "INSPECT",
    "MergeStatementContext": "MERGE",
    "MoveStatementContext": "MOVE",
    "MultiplyStatementContext": "MULTIPLY",
    "OpenStatementContext": "OPEN",
    "PerformStatementContext": "PERFORM",
    "PurgeStatementContext": "PURGE",
    "ReadStatementContext": "READ",
    "ReceiveStatementContext": "RECEIVE",
    "ReleaseStatementContext": "RELEASE",
    "ReturnStatementContext": "RETURN",
    "RewriteStatementContext": "REWRITE",
    "SearchStatementContext": "SEARCH",
    "SendStatementContext": "SEND",
    "SetStatementContext": "SET",
    "SortStatementContext": "SORT",
    "StartStatementContext": "START",
    "StopStatementContext": "STOP",
    "StringStatementContext": "STRING",
    "SubtractStatementContext": "SUBTRACT",
    "TerminateStatementContext": "TERMINATE",
    "UnstringStatementContext": "UNSTRING",
    "WriteStatementContext": "WRITE",
}

# The `EXEC` rows above lose which middleware it was, and that is the single
# most load-bearing distinction in the unsupported inventory (CICS and SQL are
# different migration problems). It is carried in the hit's `context` instead.
_EXEC_PRODUCTS: Dict[str, str] = {
    "ExecCicsStatementContext": "CICS",
    "ExecSqlStatementContext": "SQL",
    "ExecSqlImsStatementContext": "SQL IMS",
}


def statement_alternatives(parser_mod) -> FrozenSet[str]:
    """The context classes `statement` can be, read off the generated parser.

    ANTLR gives ``StatementContext`` exactly one accessor method per
    alternative of the ``statement`` rule, and defines them on that class
    rather than inheriting them, so the alternatives can be read back from the
    generated code instead of being re-listed by hand. That matters: the whole
    point of `_STATEMENT_VERBS` is to be checked against the grammar, and a
    check against a second hand-written list would only prove the two lists
    agree with each other.

    Rules elsewhere in the grammar whose names also end in "Statement" —
    ``performProcedureStatement``, ``goToStatementSimple``,
    ``endProgramStatement`` — are sub-clauses reached *inside* an alternative,
    never alternatives themselves, and are correctly absent here.
    """
    return frozenset(
        name[0].upper() + name[1:] + "Context"
        for name in vars(parser_mod.StatementContext)
        if name.endswith("Statement")
    )


def _verify_statement_map(parser_mod) -> Tuple[str, ...]:
    """Check the table against the grammar. Returns any mismatches, in order."""
    alternatives = statement_alternatives(parser_mod)
    problems = [
        f"{ctx} is a grammar alternative of `statement` with no row in the verb table"
        for ctx in sorted(alternatives - set(_STATEMENT_VERBS))
    ]
    problems += [
        f"{ctx} is in the verb table but is not an alternative of `statement`"
        for ctx in sorted(set(_STATEMENT_VERBS) - alternatives)
    ]
    return tuple(problems)


def _qualifier(ctx) -> Optional[str]:
    """The second word of a statement, when the dispatch table keys on it.

    `EXIT` and `EXIT PROGRAM` are different constructs with different support
    status, and so are `PERFORM VARYING` (inline, supported) and out-of-line
    `PERFORM <paragraph>` (not). The grammar makes both a single rule with an
    optional tail, so the distinction is only visible in the parsed tree. This
    returns that second word so `analyze` can look up the qualified key —
    which is what the token scan already does with its `next_tok`, by the same
    rule and for the same reason.

    Returns None when the statement has no second word, in which case only the
    bare verb is looked up and a qualified-only verb counts unsupported. That
    is the same under-counting direction the token scan takes (rule 4).
    """
    if ctx.getChildCount() < 2:
        return None
    second = ctx.getChild(1)
    # A token carries `symbol`; a nested rule context does not, and its own
    # first token is the word in question — `performStatement`'s second child
    # is `performInlineStatement`, which *starts* with VARYING/UNTIL/TIMES.
    # `getText()` is not usable here: it concatenates the subtree with no
    # separators, so "VARYING I FROM 1" comes back as "VARYINGIFROM1".
    symbol = getattr(second, "symbol", None)
    if symbol is not None:
        return symbol.text.upper()
    start = getattr(second, "start", None)
    return start.text.upper() if start is not None else None


def statements_by_antlr(tree, parser_mod) -> Tuple[StatementHit, ...]:
    """Every statement in the tree, classified through `_STATEMENT_VERBS`.

    Walks `statement` contexts. Nested statements count: the `statement*` inside
    an `ifStatement`'s THEN branch are statements in their own right, which is
    what the token scan counts too, so the two methods stay comparable.
    """
    problems = _verify_statement_map(parser_mod)
    if problems:
        raise RuntimeError(
            "statement verb table does not match the grammar: " + "; ".join(problems)
        )

    hits: List[StatementHit] = []
    statement_ctx = parser_mod.StatementContext
    paragraph_ctx = parser_mod.ParagraphContext
    section_ctx = parser_mod.ProcedureSectionContext

    def walk(node, paragraph: Optional[str]) -> None:
        if isinstance(node, section_ctx):
            # A section header renames the enclosing scope; paragraphs inside
            # it overwrite this again as they are met.
            paragraph = node.procedureSectionHeader().sectionName().getText().upper()
        elif isinstance(node, paragraph_ctx):
            paragraph = node.paragraphName().getText().upper()
        if isinstance(node, statement_ctx) and node.getChildCount():
            inner = node.getChild(0)
            key = type(inner).__name__
            verb = _STATEMENT_VERBS.get(key)
            if verb is not None:
                hits.append(
                    StatementHit(
                        verb=verb,
                        line=inner.start.line,
                        paragraph=paragraph,
                        context=_EXEC_PRODUCTS.get(key, key),
                        next_tok=_qualifier(inner),
                    )
                )
        for i in range(node.getChildCount()):
            child = node.getChild(i)
            if hasattr(child, "getChildCount"):
                walk(child, paragraph)

    walk(tree, None)
    return tuple(hits)


# --------------------------------------------------------------------------
# DATA DIVISION feature detection
# --------------------------------------------------------------------------

# (probed feature name, pattern). The *status* of each feature comes from
# supported.supported_data_features(), i.e. from probing the transpiler;
# only the source-detection pattern lives here.
_DATA_FEATURE_PATTERNS: Tuple[Tuple[str, re.Pattern], ...] = (
    ("USAGE COMP-3 (packed decimal)",
     re.compile(r"\b(COMP-3|COMPUTATIONAL-3|PACKED-DECIMAL)\b", re.I)),
    ("USAGE COMP / BINARY",
     re.compile(r"\b(COMP|COMPUTATIONAL|BINARY)\b(?!-3)", re.I)),
    ("REDEFINES", re.compile(r"\bREDEFINES\b", re.I)),
    ("OCCURS DEPENDING ON (variable size)",
     re.compile(r"\bOCCURS\b.*\bDEPENDING\b", re.I)),
    ("OCCURS fixed size", re.compile(r"\bOCCURS\b", re.I)),
    ("88-level condition name", re.compile(r"^\s*88\s+", re.I)),
    ("VALUE clause on a data item", re.compile(r"^\s*(?!88\b)\d\d\s+.*\bVALUE\b", re.I)),
    ("PIC A alphabetic", re.compile(r"\bPIC(TURE)?\s+(IS\s+)?[^.\s]*A[\s(]", re.I)),
    ("PIC with check protect (*)", re.compile(r"\bPIC(TURE)?\s+(IS\s+)?[^.\s]*\*", re.I)),
    ("PIC with CR / DB sign", re.compile(r"\bPIC(TURE)?\s+(IS\s+)?[^.\s]*(CR|DB)\b", re.I)),
    ("SIGN IS SEPARATE", re.compile(r"\bSIGN\b.*\bSEPARATE\b", re.I)),
    ("FILE SECTION (FD) record", re.compile(r"^\s*FD\s+", re.I)),
)


def data_features_in_source(scanned: ScannedSource, file_id: str) -> Tuple[DataFeatureHit, ...]:
    statuses = supported_data_features()
    hits: List[DataFeatureHit] = []
    for line in scanned.data_lines():
        if line.is_comment or not line.code.strip():
            continue
        matched_depending = False
        for feature, pattern in _DATA_FEATURE_PATTERNS:
            if not pattern.search(line.code):
                continue
            if feature == "OCCURS DEPENDING ON (variable size)":
                matched_depending = True
            if feature == "OCCURS fixed size" and matched_depending:
                continue        # DEPENDING ON already recorded for this line
            hits.append(
                DataFeatureHit(
                    feature=feature,
                    status=statuses.get(feature, "unsupported"),
                    file=file_id,
                    line=line.lineno,
                )
            )
    return tuple(hits)


# --------------------------------------------------------------------------
# analyze / rollup
# --------------------------------------------------------------------------


def analyze(program: FileRecord, source: str) -> CoverageResult:
    """Classify every statement in ``source`` against the C1 supported set."""
    file_id = program.rel_path_posix
    program_id = file_id
    scanned = scan_source(source)

    try:
        tree, parse_errors, parser_mod = _antlr_parse(scanned.antlr_source())
    except Exception as exc:                      # parser blew up entirely
        tree, parse_errors, parser_mod = None, (f"parser raised {type(exc).__name__}: {exc}",), None

    parse_ok = bool(tree is not None and not parse_errors)

    tree_hits: Tuple[StatementHit, ...] = ()
    if tree is not None and parser_mod is not None:
        try:
            tree_hits = statements_by_antlr(tree, parser_mod)
        except Exception as exc:
            parse_errors = parse_errors + (f"tree walk failed: {type(exc).__name__}: {exc}",)

    if parse_ok and tree_hits:
        hits, method, grade = tree_hits, "antlr_tree", "VERIFIED"
    else:
        hits, method, grade = statements_by_token_scan(scanned), "token_scan", "PLAUSIBLE"

    data_hits = data_features_in_source(scanned, file_id)

    if not hits:
        return CoverageResult(
            program_id=program_id,
            parse_ok=parse_ok,
            method="none",
            total_statements=0 if scanned.procedure_lines() else None,
            supported_statements=None,
            coverage_ratio=None,
            data_feature_inventory=data_hits,
            parser_errors=parse_errors,
            error="no statements recovered by either method — "
                  "no coverage ratio is reported (R1)",
        )

    supported = supported_verbs()
    inventory: List[ConstructHit] = []
    n_supported = 0
    for h in hits:
        # A verb counts as supported if the dispatch table has the bare verb
        # OR its qualified two-word form ("EXIT PROGRAM"). Both reads come
        # from SUPPORTED_STATEMENTS -- nothing here is hand-maintained. A
        # qualified-only verb whose qualifier was not recovered (ANTLR path,
        # or line-final EXIT) counts unsupported: under-counting is the
        # smaller lie (rule 4).
        ok = h.verb in supported or (
            h.next_tok is not None and f"{h.verb} {h.next_tok}" in supported)
        n_supported += int(ok)
        if not ok:
            inventory.append(
                ConstructHit(
                    verb=h.verb, supported=False, file=file_id,
                    line=h.line, paragraph=h.paragraph, context=h.context,
                )
            )
    inventory.sort(key=lambda h: (h.line, h.verb))

    total = len(hits)
    provenance = (
        f"{n_supported}/{total} statements supported via {registry_provenance()} "
        f"on {file_id} (sha256:{program.sha256[:16]}); method={method}, "
        f"source_format={scanned.source_format}"
    )
    if method == "token_scan":
        provenance += f"; antlr_syntax_errors={len(parse_errors)}"

    return CoverageResult(
        program_id=program_id,
        parse_ok=parse_ok,
        method=method,
        total_statements=total,
        supported_statements=n_supported,
        coverage_ratio=Measured(round(n_supported / total, 4), provenance, grade),
        unsupported_inventory=tuple(inventory),
        data_feature_inventory=data_hits,
        parser_errors=parse_errors,
    )


def rollup(results: Sequence[CoverageResult], program_id: str = "PORTFOLIO") -> CoverageResult:
    """Portfolio coverage, weighted by statement count.

    Programs from which no statements were recovered contribute nothing to the
    ratio and are named in ``parser_errors`` — they are not silently treated as
    0% or 100%.
    """
    measured = [r for r in results if r.total_statements and r.supported_statements is not None]
    total = sum(r.total_statements or 0 for r in measured)
    supported = sum(r.supported_statements or 0 for r in measured)

    inventory: List[ConstructHit] = []
    data_hits: List[DataFeatureHit] = []
    for r in results:
        inventory.extend(r.unsupported_inventory)
        data_hits.extend(r.data_feature_inventory)
    inventory.sort(key=lambda h: (h.file, h.line, h.verb))
    data_hits.sort(key=lambda h: (h.file, h.line, h.feature))

    skipped = tuple(
        f"{r.program_id}: no statements recovered ({r.error or 'unknown'})"
        for r in results
        if r not in measured
    )

    if not total:
        return CoverageResult(
            program_id=program_id,
            parse_ok=all(r.parse_ok for r in results) if results else False,
            method="none",
            total_statements=0,
            supported_statements=None,
            coverage_ratio=None,
            unsupported_inventory=tuple(inventory),
            data_feature_inventory=tuple(data_hits),
            parser_errors=skipped,
            error="no statements recovered across the portfolio — no ratio (R1)",
        )

    methods = sorted({r.method for r in measured})
    grade = "VERIFIED" if methods == ["antlr_tree"] else "PLAUSIBLE"
    method = methods[0] if len(methods) == 1 else "mixed(" + "+".join(methods) + ")"
    provenance = (
        f"{supported}/{total} statements supported across {len(measured)} program(s) "
        f"via {registry_provenance()}; method={method}"
    )
    if skipped:
        provenance += f"; {len(skipped)} program(s) excluded, no statements recovered"

    return CoverageResult(
        program_id=program_id,
        parse_ok=all(r.parse_ok for r in measured),
        method=method,
        total_statements=total,
        supported_statements=supported,
        coverage_ratio=Measured(round(supported / total, 4), provenance, grade),
        unsupported_inventory=tuple(inventory),
        data_feature_inventory=tuple(data_hits),
        parser_errors=skipped,
    )


def data_feature_summary(results: Sequence[CoverageResult]) -> Dict[str, Dict[str, object]]:
    """Per-feature occurrence counts plus the probed status. Measured."""
    statuses = supported_data_features()
    counts: Dict[str, int] = {}
    for r in results:
        for h in r.data_feature_inventory:
            counts[h.feature] = counts.get(h.feature, 0) + 1
    return {
        feature: {
            "occurrences": counts[feature],
            "status": statuses.get(feature, "unsupported"),
            "status_provenance": data_feature_provenance(),
        }
        for feature in sorted(counts)
    }


def quotable_split(
    results: Sequence[CoverageResult],
) -> Tuple[Optional[int], Optional[int]]:
    """(statements C1 can transpile today, statements needing grammar work)."""
    measured = [r for r in results if r.total_statements and r.supported_statements is not None]
    if not measured:
        return None, None
    supported = sum(r.supported_statements or 0 for r in measured)
    total = sum(r.total_statements or 0 for r in measured)
    return supported, total - supported


__all__ = [
    "COBOL_VERBS",
    "CodeLine",
    "ScannedSource",
    "StatementHit",
    "analyze",
    "data_feature_summary",
    "data_features_in_source",
    "detect_format",
    "quotable_split",
    "rollup",
    "scan_source",
    "statements_by_token_scan",
    "STATUS_SUPPORTED",
]
