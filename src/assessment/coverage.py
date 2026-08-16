"""WP-1.3 — construct coverage: what fraction of a program C1 can transpile.

Two analysis methods, and the result always says which one produced it.

``antlr_tree`` (graded VERIFIED)
    ``src/parsers/antlr/cobol`` is walked and every ``StatementContext`` is
    classified. Used **only** when the parse produced zero syntax errors, so a
    tree assembled by error recovery is never passed off as a clean parse.

``token_scan`` (graded PLAUSIBLE)
    A documented lexical scan, used when the ANTLR parse reports errors.

The fallback is not a nicety. The grammar bundled in this repo
(``src/parsers/grammars/Cobol85.g4``) is a **reduced** COBOL-85 subset, not the
full standard grammar: it requires the ``USAGE`` keyword before ``COMP-3``,
requires ``TIMES`` after ``OCCURS``, has no ``ALTER``/``EXEC``/``ACCEPT`` rules,
and its ``computeStatement`` cannot parse ``COMPUTE X = A + B``. Measured
against this repo's own bench corpus, it reports syntax errors on 5 of 5
programs and recovers **zero** statements from every one of them, because a
DATA DIVISION error resynchronises past the entire PROCEDURE DIVISION. An
analyzer that only used the tree would therefore return "no data" for every
real program. So both methods exist, every result is labelled with the one that
ran, and only the tree path is graded VERIFIED (R1/R9).

Token-scan counting rules (reproduced verbatim in the report appendix):

1. Source format is detected per file: **fixed** if any line carries ``*`` or
   ``/`` in column 7, or if at least 80% of non-blank lines begin with 6 or
   more spaces; otherwise **free**. In fixed format the code area is columns
   8–72 and column 7 is the indicator; in free format the whole line is code.
2. A line is a comment if its indicator column is ``*`` or ``/``, or if the
   line's first non-blank characters are ``*>``.
3. Only the PROCEDURE DIVISION is scanned for statements.
4. A statement is counted at each verb token that appears in a
   *statement-start position*: the first token of a line, or a token
   immediately following ``.``, ``THEN``, ``ELSE``, or an ``END-…`` scope
   terminator. This deliberately under-counts verbs buried mid-clause (e.g.
   ``WHEN 1 DISPLAY X``); under-counting a construct is a smaller lie than
   guessing at one, and the grade says PLAUSIBLE.
5. ``EXEC CICS`` / ``EXEC SQL`` / ``EXEC DLI`` count as one statement with verb
   ``EXEC`` and the product recorded as its context.
6. A paragraph label is a line whose code area is a single name followed by a
   period; a section header additionally has ``SECTION`` before the period.

A statement is SUPPORTED iff its verb is in
:func:`src.assessment.supported.supported_verbs`, which reads the transpiler's
dispatch table. Nothing here maintains its own opinion of what C1 supports.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Dict, List, Optional, Sequence, Tuple

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


def _paragraph_label(code: str) -> Optional[str]:
    """A lone ``NAME.`` line — unless NAME is a COBOL verb.

    ``GOBACK.`` and ``EXIT.`` are single-word statements that are lexically
    indistinguishable from a paragraph label. A paragraph may not be named
    after a reserved verb, so the verb reading is the correct one; without this
    check both statements vanish from the inventory.
    """
    m = _PARAGRAPH_NAME_RE.match(code)
    if not m:
        return None
    name = m.group(1).upper()
    return None if name in COBOL_VERBS else name


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
        """Code area only, comments blanked, line numbers preserved.

        Feeding raw fixed-format text to ANTLR guarantees syntax errors from
        the sequence-number area and comment lines, which would then be
        reported as if the *program* were malformed.
        """
        return "\n".join("" if l.is_comment else l.code for l in self.lines)


def detect_format(raw_lines: Sequence[str]) -> str:
    """Fixed vs free format. See rule 1 in the module docstring."""
    non_blank = [l for l in raw_lines if l.strip()]
    if not non_blank:
        return "fixed"
    if any(len(l) > 6 and l[6] in "*/" for l in non_blank):
        return "fixed"
    indented = sum(1 for l in non_blank if len(l) > 6 and l[:6].strip() == "" and l[6] == " ")
    return "fixed" if indented >= 0.8 * len(non_blank) else "free"


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
        for m in _TOKEN_RE.finditer(code):
            tok = m.group(0)
            at_start = prev is None or prev in _START_CONTEXTS or prev.startswith("END-")
            if tok in COBOL_VERBS and at_start:
                context = _exec_product(code, m.start()) if tok == "EXEC" else None
                hits.append(StatementHit(tok, line.lineno, line.paragraph, context))
            prev = tok
    return tuple(hits)


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
                self.errors.append(f"line {line}:{column} {msg}")

    collector = _Collect()
    lexer = Cobol85Lexer(antlr4.InputStream(source))
    lexer.removeErrorListeners()
    lexer.addErrorListener(collector)
    parser = Cobol85Parser(antlr4.CommonTokenStream(lexer))
    parser.removeErrorListeners()
    parser.addErrorListener(collector)
    tree = parser.compilationUnit()
    return tree, tuple(collector.errors), Cobol85Parser


def statements_by_antlr(tree, Cobol85Parser) -> Tuple[StatementHit, ...]:
    hits: List[StatementHit] = []

    def walk(node, paragraph: Optional[str]) -> None:
        if isinstance(node, Cobol85Parser.ParagraphContext):
            paragraph = node.paragraphName().getText().upper()
        if isinstance(node, Cobol85Parser.StatementContext):
            child = node.getChild(0)
            hits.append(
                StatementHit(
                    verb=node.start.text.upper(),
                    line=node.start.line,
                    paragraph=paragraph,
                    context=type(child).__name__,
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
        ok = h.verb in supported
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
