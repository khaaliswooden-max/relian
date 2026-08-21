"""WP-2.2 §3.1 — the copybook resolver.

Reads fixed-format COBOL, finds every ``COPY`` directive, resolves each one
against a search path, and reports what it could not resolve rather than
raising. Four outputs, all of them first-class (D20):

``Resolution.records``     name -> :class:`CopybookSource` for everything found
``Resolution.unresolved``  every missing name, WITH its referrers and the
                           directories that were searched for it
``Resolution.edges``       the (referrer, referent) fan-in edge list
``Resolution.cycles``      cycles, detected and reported, never followed

**Why unresolved is a capability and not an error** (R2). Real mainframe code
is not self-contained: CardDemo references ``DFHAID`` and ``DFHBMSCA`` from 21
programs each, and six ``CMQ*`` members, none of which ship with the sample —
they come with CICS and IBM MQ. "Here are the copybooks you are missing and
who references them, before you start" is a finding a customer pays for. Most
tools report it as a crash.

**The COPY regex refuses to match the tail of a hyphenated identifier.**
WP-2.0.0 §0.4 measured the naive ``\\bCOPY\\s+`` pattern inventing a copybook
named ``REPLACING`` out of::

    INITIALIZE REQUEST-MSG-COPY  REPLACING NUMERIC BY ZEROES

at ``app/app-vsam-mq/cbl/CODATE01.cbl:294``. The lookbehind in
:data:`_COPY_RE` is what stops it, and that line is a test fixture rather than
a footnote.

This resolver is **independent of the ProLeap preprocessor** (D17).
``Cobol85Preprocessor.g4`` is vendored but not generated, and it blocks
*program* parse; it has nothing to do with reading a ``.cpy`` file. CardDemo's
copybooks resolve today while its programs still wait on WP-2.5.
"""

from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, List, Mapping, Optional, Sequence, Tuple

from src.assessment.intake import SKIP_DIRS, read_source

# --------------------------------------------------------------------------
# Fixed-format source reading
# --------------------------------------------------------------------------

#: Reference format: the sequence area is columns 1-6, the indicator is column
#: 7, area A/B run 8-72, and 73-80 is the identification area. Only 7-72 is
#: program text. CardDemo carries content in BOTH margins -- sequence numbers
#: in 1-6 and a change-id in 73-80 -- so a resolver that reads whole lines
#: reads data that is not code.
COL_INDICATOR = 7
COL_TEXT_END = 72

#: Column-7 indicators that make the line a comment. ``D``/``d`` is a debugging
#: line, which IS program text when WITH DEBUGGING MODE is active; it is not
#: skipped here, and WP-2.0.0 measured zero of them across every corpus.
COMMENT_INDICATORS = frozenset({"*", "/"})

#: Column-7 continuation indicator.
CONTINUATION_INDICATOR = "-"


@dataclass(frozen=True)
class CodeLine:
    """One line of program text, with the source line number it came from."""

    lineno: int          # 1-based, in the original file
    text: str            # columns 7..72, indicator blanked, right-stripped
    indicator: str       # the raw column-7 character (" " or "-")


def code_lines(text: str, *, fixed_format: bool = True) -> List[CodeLine]:
    """Yield the program-text lines of ``text``, comments and margins removed.

    Comment lines (``*`` or ``/`` in column 7) are dropped. Blank lines are
    dropped. Line numbers are preserved so a finding can name a real line.

    ``fixed_format=False`` treats the whole line as program text, for free-form
    sources. It still drops ``*``/``/`` comment lines, which free-form COBOL
    also uses.
    """
    out: List[CodeLine] = []
    for lineno, raw in enumerate(text.replace("\r\n", "\n").replace("\r", "\n").split("\n"), 1):
        if fixed_format:
            if len(raw) < COL_INDICATOR:
                # Columns 1-6 are the sequence area. A line that ends inside it
                # carries no program text at all -- not even an empty
                # statement. Emitting its digits as code is how a blank line
                # with a sequence number becomes a phantom source line.
                continue
            indicator = raw[COL_INDICATOR - 1]
            if indicator in COMMENT_INDICATORS:
                continue
            body = raw[COL_INDICATOR:COL_TEXT_END].rstrip()
        else:
            indicator = " "
            stripped_left = raw.lstrip()
            if stripped_left[:1] in COMMENT_INDICATORS:
                continue
            body = raw.rstrip()
        if not body.strip():
            continue
        out.append(CodeLine(lineno, body, indicator))
    return out


# --------------------------------------------------------------------------
# COPY directive extraction
# --------------------------------------------------------------------------

#: A COBOL user-defined word: letters, digits and hyphens, not starting or
#: ending with a hyphen. ``$`` and ``_`` are accepted because GnuCOBOL does.
_WORD = r"[A-Za-z0-9$_](?:[A-Za-z0-9$_-]*[A-Za-z0-9$_])?"

#: The load-bearing lookbehind. ``(?<![A-Za-z0-9$_-])`` is what makes
#: ``REQUEST-MSG-COPY`` not a COPY directive: the character before ``COPY``
#: there is a hyphen, so the match is refused. ``\bCOPY`` does NOT refuse it --
#: ``\b`` sits happily between ``-`` and ``C``. That one difference is the
#: whole of WP-2.0.0's phantom ``REPLACING`` copybook.
_COPY_RE = re.compile(
    r"(?<![A-Za-z0-9$_-])COPY(?![A-Za-z0-9$_-])\s+"
    r"(?:\"(?P<dq>[^\"]+)\"|'(?P<sq>[^']+)'|(?P<bare>" + _WORD + r"))",
    re.IGNORECASE,
)

_LIB_RE = re.compile(
    r"\A\s*(?:OF|IN)\s+(?:\"(?P<dq>[^\"]+)\"|'(?P<sq>[^']+)'|(?P<bare>" + _WORD + r"))",
    re.IGNORECASE,
)

_REPLACING_RE = re.compile(r"\A\s*REPLACING(?![A-Za-z0-9$_-])", re.IGNORECASE)

#: One REPLACING operand: pseudo-text ``==...==``, a quoted literal, or a word.
_OPERAND_RE = re.compile(
    r"\A\s*(?:==(?P<pseudo>.*?)==|\"(?P<dq>[^\"]*)\"|'(?P<sq>[^']*)'|(?P<bare>[^\s.]+))",
    re.DOTALL,
)

_BY_RE = re.compile(r"\A\s*BY(?![A-Za-z0-9$_-])", re.IGNORECASE)


@dataclass(frozen=True)
class CopyDirective:
    """One ``COPY`` site, with everything needed to act on or report it."""

    name: str                                   # upper-cased copybook name
    referrer: str                               # posix path, relative to root
    lineno: int                                 # 1-based line in the referrer
    library: Optional[str] = None               # OF/IN <library>
    replacing: Tuple[Tuple[str, str], ...] = () # (from, to) pseudo-text pairs
    replacing_parsed: bool = True               # False -> phrase not understood
    raw: str = ""

    def to_dict(self) -> Dict[str, object]:
        return {
            "name": self.name,
            "referrer": self.referrer,
            "lineno": self.lineno,
            "library": self.library,
            "replacing": [list(pair) for pair in self.replacing],
            "replacing_parsed": self.replacing_parsed,
        }


def _statement_after(joined: str, start: int) -> str:
    """Return the text from ``start`` up to the next separator period.

    A separator period is a ``.`` followed by whitespace or end-of-text, and
    not inside a quoted literal or a ``==`` pseudo-text delimiter. ``PIC
    ZZZ,ZZ9.99`` and ``VALUE 0.0625`` therefore do not terminate a statement,
    and neither does the ``.`` inside ``VALUE "A.B"``.
    """
    i = start
    n = len(joined)
    quote: Optional[str] = None
    in_pseudo = False
    while i < n:
        ch = joined[i]
        if quote is not None:
            if ch == quote:
                quote = None
            i += 1
            continue
        if in_pseudo:
            if joined.startswith("==", i):
                in_pseudo = False
                i += 2
                continue
            i += 1
            continue
        if ch in "\"'":
            quote = ch
            i += 1
            continue
        if joined.startswith("==", i):
            in_pseudo = True
            i += 2
            continue
        if ch == "." and (i + 1 >= n or joined[i + 1].isspace()):
            return joined[start:i]
        i += 1
    return joined[start:n]


def _parse_replacing(tail: str) -> Tuple[Tuple[Tuple[str, str], ...], bool]:
    """Parse ``REPLACING a BY b [a BY b]...``. Returns (pairs, fully_parsed)."""
    pairs: List[Tuple[str, str]] = []
    rest = tail
    while rest.strip():
        m_from = _OPERAND_RE.match(rest)
        if not m_from:
            return tuple(pairs), False
        frm = _operand_text(m_from)
        rest = rest[m_from.end():]
        m_by = _BY_RE.match(rest)
        if not m_by:
            # A trailing token with no BY: the phrase is not understood. Say so
            # rather than substituting something half-parsed (R2).
            return tuple(pairs), False
        rest = rest[m_by.end():]
        m_to = _OPERAND_RE.match(rest)
        if not m_to:
            return tuple(pairs), False
        pairs.append((frm, _operand_text(m_to)))
        rest = rest[m_to.end():]
    return tuple(pairs), True


def _operand_text(m: "re.Match[str]") -> str:
    """The one operand a match captured. ``_LIB_RE`` has no ``pseudo`` group,
    so this reads the match's own group dictionary rather than assuming a
    fixed set."""
    groups = m.groupdict()
    for group in ("pseudo", "dq", "sq", "bare"):
        value = groups.get(group)
        if value is not None:
            return value
    return ""


def find_copy_directives(
    text: str, referrer: str = "<text>", *, fixed_format: bool = True
) -> List[CopyDirective]:
    """Extract every ``COPY`` directive from one source file's text.

    Directives may span lines -- CardDemo's 40 ``COPY CSSETATY REPLACING``
    sites put each pseudo-text pair on its own line -- so the program-text
    lines are joined before matching, with a map back to the original line
    numbers so every finding can name a real line.
    """
    lines = code_lines(text, fixed_format=fixed_format)
    if not lines:
        return []

    joined_parts: List[str] = []
    line_index: List[int] = []          # char offset -> index into `lines`
    for idx, line in enumerate(lines):
        chunk = line.text + "\n"
        joined_parts.append(chunk)
        line_index.extend([idx] * len(chunk))
    joined = "".join(joined_parts)

    directives: List[CopyDirective] = []
    for m in _COPY_RE.finditer(joined):
        name = (m.group("dq") or m.group("sq") or m.group("bare") or "").strip()
        if not name:
            continue
        lineno = lines[line_index[m.start()]].lineno
        tail = _statement_after(joined, m.end())

        library: Optional[str] = None
        m_lib = _LIB_RE.match(tail)
        if m_lib:
            library = (_operand_text(m_lib) or "").strip().upper() or None
            tail = tail[m_lib.end():]

        replacing: Tuple[Tuple[str, str], ...] = ()
        parsed = True
        m_rep = _REPLACING_RE.match(tail)
        if m_rep:
            replacing, parsed = _parse_replacing(tail[m_rep.end():])

        directives.append(
            CopyDirective(
                name=name.upper(),
                referrer=referrer,
                lineno=lineno,
                library=library,
                replacing=replacing,
                replacing_parsed=parsed,
                raw=joined[m.start(): m.end() + len(tail)].strip(),
            )
        )
    return directives


# --------------------------------------------------------------------------
# REPLACING — pseudo-text substitution
# --------------------------------------------------------------------------

def _replacing_pattern(pairs: Sequence[Tuple[str, str]]) -> Optional["re.Pattern[str]"]:
    """One alternation over every operand, so substitution is a single pass.

    A single pass matters: with ``==A== BY ==B==`` and ``==B== BY ==C==``
    applied in sequence, the first pair's output would be re-scanned by the
    second and ``A`` would arrive as ``C``. COBOL's REPLACING does not work
    that way and neither does this.

    Matching is word-aware. Whitespace inside an operand matches any run of
    whitespace, and where the operand begins or ends with a COBOL word
    character the match is refused mid-identifier -- so ``==A== BY ==B==``
    does not rewrite ``ACCT-A-FLAG``.
    """
    active = [(frm, to) for frm, to in pairs if frm.strip()]
    if not active:
        return None
    alternatives = []
    for frm, _to in active:
        body = r"\s+".join(re.escape(tok) for tok in frm.split())
        lead = r"(?<![A-Za-z0-9$_-])" if _is_word_char(frm.strip()[0]) else ""
        trail = r"(?![A-Za-z0-9$_-])" if _is_word_char(frm.strip()[-1]) else ""
        alternatives.append(f"{lead}(?:{body}){trail}")
    return re.compile("|".join(f"({alt})" for alt in alternatives), re.IGNORECASE)


def _substitute(pattern: "re.Pattern[str]", pairs: Sequence[Tuple[str, str]], text: str) -> str:
    active = [(frm, to) for frm, to in pairs if frm.strip()]

    def _sub(m: "re.Match[str]") -> str:
        for idx, (_frm, to) in enumerate(active, start=1):
            if m.group(idx) is not None:
                return to
        return m.group(0)          # pragma: no cover - unreachable

    return pattern.sub(_sub, text)


def apply_replacing(
    text: str, pairs: Sequence[Tuple[str, str]], *, fixed_format: bool = True
) -> str:
    """Perform ``COPY ... REPLACING`` pseudo-text substitution on ``text``.

    Substitution happens **before** layout, because it changes the data names
    the layout is built from. CardDemo has 40 such sites, so it is not
    optional.

    In fixed format the substitution touches **columns 7-72 only**. The
    sequence area is preserved; the identification area is dropped from any
    line the substitution changed, because carrying columns 73-80 across a
    length change puts them somewhere they no longer mean anything. Comment
    lines are left exactly as they are.

    Two limitations are *detected and reported* rather than silently absorbed
    -- see :func:`replacing_limitations`.
    """
    pattern = _replacing_pattern(pairs)
    if pattern is None:
        return text
    if not fixed_format:
        return _substitute(pattern, pairs, text)

    out: List[str] = []
    for raw in text.replace("\r\n", "\n").replace("\r", "\n").split("\n"):
        if len(raw) < COL_INDICATOR or raw[COL_INDICATOR - 1] in COMMENT_INDICATORS:
            out.append(raw)
            continue
        head = raw[:COL_INDICATOR]
        body = raw[COL_INDICATOR:COL_TEXT_END]
        tail = raw[COL_TEXT_END:]
        replaced = _substitute(pattern, pairs, body)
        out.append(head + replaced if replaced != body else head + body + tail)
    return "\n".join(out)


#: How wide the program-text area is, once the indicator column is accounted
#: for. A substitution that pushes a line past this loses characters to the
#: margin, which is a silent truncation and therefore reportable.
_TEXT_WIDTH = COL_TEXT_END - COL_INDICATOR


def replacing_limitations(
    text: str, pairs: Sequence[Tuple[str, str]], *, fixed_format: bool = True
) -> List[str]:
    """Report what :func:`apply_replacing` could NOT do to ``text``.

    Two cases, both of which would otherwise be a substitution that quietly
    did not happen -- the worst kind, because the layout that follows looks
    complete (R2):

    * **An operand that spans a line break.** Substitution is per line, since
      program text has margins on both sides and the bytes between two lines
      are not whitespace. An operand whose tokens straddle a break is found by
      re-running the match over the joined program text and comparing counts.
    * **Overflow past column 72.** A replacement longer than what it replaced
      can push program text into the identification area, where the reader
      will not see it.
    """
    pattern = _replacing_pattern(pairs)
    if pattern is None:
        return []
    problems: List[str] = []
    if not fixed_format:
        return problems

    lines = code_lines(text)
    per_line = sum(len(pattern.findall(line.text)) for line in lines)
    joined = " ".join(line.text.strip() for line in lines)
    joined_count = len(pattern.findall(joined))
    if joined_count > per_line:
        problems.append(
            f"{joined_count - per_line} REPLACING operand(s) span a line break; "
            f"substitution is per line and those occurrences were NOT applied"
        )

    for raw in text.replace("\r\n", "\n").replace("\r", "\n").split("\n"):
        if len(raw) < COL_INDICATOR or raw[COL_INDICATOR - 1] in COMMENT_INDICATORS:
            continue
        body = raw[COL_INDICATOR:COL_TEXT_END]
        replaced = _substitute(pattern, pairs, body)
        if replaced != body and len(replaced) > _TEXT_WIDTH:
            problems.append(
                f"a REPLACING substitution pushes program text past column "
                f"{COL_TEXT_END}: {replaced.strip()!r}"
            )
    return problems


def _is_word_char(ch: str) -> bool:
    return ch.isalnum() or ch in "$_-"


# --------------------------------------------------------------------------
# Resolution
# --------------------------------------------------------------------------

#: Extensions a copybook member may carry on disk. A ``COPY XYZ`` names a
#: member, not a filename, so every one of these is tried.
COPYBOOK_SUFFIXES: Tuple[str, ...] = (".cpy", ".CPY", ".copy", ".COPY", ".cbl", ".CBL", "")

#: Files whose COPY directives are collected. Programs and copybooks both.
SOURCE_SUFFIXES = frozenset({".cbl", ".cob", ".cobol", ".cpy", ".copy"})


@dataclass(frozen=True)
class CopybookSource:
    """A copybook that was found on disk."""

    name: str
    path: str                 # posix, relative to the resolution root
    sha256: str
    text: str

    def to_dict(self) -> Dict[str, object]:
        return {"name": self.name, "path": self.path, "sha256": self.sha256}


@dataclass(frozen=True)
class Unresolved:
    """A ``COPY`` target that no search path could satisfy (D20).

    Carries the referrers and the directories actually searched, because
    "missing" without "we looked here" is an accusation rather than a finding.
    """

    name: str
    referenced_by: Tuple[str, ...]
    paths_searched: Tuple[str, ...]
    reference_count: int
    reason: str = "no member of this name under any search path"

    def to_dict(self) -> Dict[str, object]:
        return {
            "name": self.name,
            "referenced_by": list(self.referenced_by),
            "paths_searched": list(self.paths_searched),
            "reference_count": self.reference_count,
            "reason": self.reason,
        }


@dataclass(frozen=True)
class Resolution:
    """Everything the resolver learned about one tree."""

    root: str
    files_scanned: int
    records: Mapping[str, CopybookSource]
    unresolved: Tuple[Unresolved, ...]
    edges: Tuple[Tuple[str, str], ...]
    cycles: Tuple[Tuple[str, ...], ...]
    directives: Tuple[CopyDirective, ...] = ()
    search_paths: Tuple[str, ...] = ()
    unreferenced: Tuple[str, ...] = ()

    # -- derived views, all computed rather than stored (R1) ---------------

    def fan_out(self) -> Dict[str, int]:
        """referrer -> number of DISTINCT copybook names it references."""
        seen: Dict[str, set] = {}
        for referrer, referent in self.edges:
            seen.setdefault(referrer, set()).add(referent)
        return {k: len(v) for k, v in seen.items()}

    def fan_in(self) -> Dict[str, int]:
        """copybook name -> number of DISTINCT files that reference it."""
        seen: Dict[str, set] = {}
        for referrer, referent in self.edges:
            seen.setdefault(referent, set()).add(referrer)
        return {k: len(v) for k, v in seen.items()}

    def referenced_names(self) -> Tuple[str, ...]:
        return tuple(sorted({referent for _r, referent in self.edges}))

    def to_dict(self) -> Dict[str, object]:
        return {
            "root": self.root,
            "files_scanned": self.files_scanned,
            "records": {k: v.to_dict() for k, v in sorted(self.records.items())},
            "unresolved": [u.to_dict() for u in self.unresolved],
            "edges": [list(e) for e in self.edges],
            "cycles": [list(c) for c in self.cycles],
            "search_paths": list(self.search_paths),
            "unreferenced": list(self.unreferenced),
        }


def _index_copybooks(dirs: Sequence[Path]) -> Tuple[Dict[str, Path], Dict[str, str]]:
    """member name (upper) -> file, plus how each entry was recognised.

    Two passes, and the order is load-bearing. A ``COPY XYZ`` names a *member*,
    not a filename, and mainframe members routinely arrive with no extension —
    so extension-less files are indexed too. But a file called ``LICENSE`` is
    not a copybook, and letting the extension-less pass win would let it shadow
    a real ``LICENSE.cpy`` on a tree that had both. So ``.cpy``/``.copy`` are
    indexed first and extension-less files only fill gaps.

    The second return value records which pass supplied each entry, because
    "copybooks present but never referenced" is a real finding (CardDemo has
    three) and drowning it in ``.gitignore`` and ``NOTICE`` would destroy it.
    """
    index: Dict[str, Path] = {}
    kinds: Dict[str, str] = {}
    for pass_name, wanted_suffix in (("copybook", True), ("extensionless", False)):
        for directory in dirs:
            if not directory.is_dir():
                continue
            for child in sorted(directory.iterdir(), key=lambda p: p.name):
                if not child.is_file() or child.is_symlink():
                    continue
                has_copybook_suffix = child.suffix.lower() in {".cpy", ".copy"}
                if wanted_suffix and not has_copybook_suffix:
                    continue
                if not wanted_suffix and child.suffix:
                    continue
                key = child.stem.upper()
                if key not in index:
                    index[key] = child
                    kinds[key] = pass_name
    return index, kinds


def _walk_dirs(root: Path) -> List[Path]:
    dirs = [root]
    stack = [root]
    while stack:
        current = stack.pop()
        for child in sorted(current.iterdir(), key=lambda p: p.name):
            if child.is_symlink() or not child.is_dir():
                continue
            if child.name in SKIP_DIRS:
                continue
            dirs.append(child)
            stack.append(child)
    return dirs


def _source_files(root: Path) -> List[Path]:
    out: List[Path] = []
    stack = [root]
    while stack:
        current = stack.pop()
        for child in sorted(current.iterdir(), key=lambda p: p.name):
            if child.is_symlink():
                continue
            if child.is_dir():
                if child.name not in SKIP_DIRS:
                    stack.append(child)
            elif child.is_file() and child.suffix.lower() in SOURCE_SUFFIXES:
                out.append(child)
    out.sort(key=lambda p: p.as_posix())        # R8
    return out


def resolve(root: Path, search_paths: Sequence[Path] = ()) -> Resolution:
    """Resolve every ``COPY`` under ``root``.

    ``search_paths`` are consulted in order, then every directory under
    ``root`` (sorted, R8). Nothing here raises on a missing copybook: an
    unresolvable ``COPY`` is a :class:`Unresolved` record, which is the normal
    condition of real mainframe code and a reportable capability (D20, R2).
    """
    root = Path(root)
    if not root.exists():
        raise FileNotFoundError(f"discovery root does not exist: {root}")

    explicit = [Path(p) for p in search_paths]
    dirs = explicit + _walk_dirs(root)
    index, index_kinds = _index_copybooks(dirs)

    files = _source_files(root)
    directives: List[CopyDirective] = []
    for path in files:
        rel = path.relative_to(root).as_posix()
        directives.extend(find_copy_directives(read_source(path), rel))

    edges: List[Tuple[str, str]] = []
    referrers: Dict[str, List[str]] = {}
    counts: Dict[str, int] = {}
    for directive in directives:
        edges.append((directive.referrer, directive.name))
        bucket = referrers.setdefault(directive.name, [])
        if directive.referrer not in bucket:
            bucket.append(directive.referrer)
        counts[directive.name] = counts.get(directive.name, 0) + 1

    searched = tuple(_rel_or_abs(d, root) for d in dirs)

    # `records` indexes every member FOUND under the search paths, not only
    # the ones something happened to reference. A copybook nobody COPYs is
    # still a copybook a customer can ask for a layout of, and restricting the
    # index to referenced names would make `compute(name, resolution)` fail on
    # a perfectly resolvable member for a reason that has nothing to do with
    # the member.
    records: Dict[str, CopybookSource] = {}
    for name in sorted(index):
        found = index[name]
        raw = found.read_bytes()
        records[name] = CopybookSource(
            name=name,
            path=_rel_or_abs(found, root),
            sha256=hashlib.sha256(raw).hexdigest(),
            text=read_source(found),
        )

    unresolved: List[Unresolved] = [
        Unresolved(
            name=name,
            referenced_by=tuple(sorted(referrers[name])),
            paths_searched=searched,
            reference_count=counts[name],
        )
        for name in sorted(referrers)
        if name not in index
    ]

    # Only members recognised BY a copybook extension count as "present but
    # never referenced". An extension-less LICENSE that nothing COPYs is not a
    # dead copybook; it is not a copybook.
    unreferenced = tuple(sorted(
        name for name in set(index) - set(referrers)
        if index_kinds.get(name) == "copybook"
    ))
    deduped_edges = tuple(sorted(set(edges)))
    return Resolution(
        root=root.as_posix(),
        files_scanned=len(files),
        records=records,
        unresolved=tuple(unresolved),
        edges=deduped_edges,
        cycles=detect_cycles(deduped_edges),
        directives=tuple(directives),
        search_paths=searched,
        unreferenced=unreferenced,
    )


def _rel_or_abs(path: Path, root: Path) -> str:
    try:
        return path.relative_to(root).as_posix() or "."
    except ValueError:
        return path.as_posix()


def detect_cycles(edges: Iterable[Tuple[str, str]]) -> Tuple[Tuple[str, ...], ...]:
    """Find every cycle in the COPY graph. Detected and reported, never followed.

    Nodes are normalised so a file ``app/cpy/A.cpy`` and a ``COPY A`` from it
    are the same node: an edge's referrer is a path and its referent is a
    member name, and a cycle only exists where the two meet.
    """
    graph: Dict[str, List[str]] = {}
    for referrer, referent in edges:
        graph.setdefault(_node(referrer), []).append(_node(referent))
    for key in graph:
        graph[key] = sorted(set(graph[key]))

    cycles: List[Tuple[str, ...]] = []
    seen_keys: set = set()
    colour: Dict[str, int] = {}      # 0 unvisited, 1 on-stack, 2 done

    def visit(node: str, stack: List[str]) -> None:
        colour[node] = 1
        stack.append(node)
        for nxt in graph.get(node, ()):
            state = colour.get(nxt, 0)
            if state == 1:
                cycle = tuple(stack[stack.index(nxt):])
                key = frozenset(cycle)
                if key not in seen_keys:
                    seen_keys.add(key)
                    cycles.append(_canonical_cycle(cycle))
            elif state == 0:
                visit(nxt, stack)
        stack.pop()
        colour[node] = 2

    for node in sorted(graph):
        if colour.get(node, 0) == 0:
            visit(node, [])
    return tuple(sorted(cycles))


def _node(label: str) -> str:
    """A path becomes its member name; a member name stays itself."""
    stem = label.rsplit("/", 1)[-1]
    if "." in stem:
        stem = stem.rsplit(".", 1)[0]
    return stem.upper()


def _canonical_cycle(cycle: Tuple[str, ...]) -> Tuple[str, ...]:
    """Rotate so the cycle starts at its lexicographically smallest node."""
    if not cycle:
        return cycle
    start = cycle.index(min(cycle))
    return cycle[start:] + cycle[:start]


# --------------------------------------------------------------------------
# Assembling one copybook's text, COPY-expanded
# --------------------------------------------------------------------------

@dataclass
class Assembly:
    """The result of expanding one copybook's nested COPY directives."""

    text: str
    expanded: List[str] = field(default_factory=list)
    missing: List[str] = field(default_factory=list)
    cyclic: List[str] = field(default_factory=list)
    unapplied: List[str] = field(default_factory=list)

    @property
    def complete(self) -> bool:
        """True only when nothing was missing, nothing cycled, and every
        REPLACING operand was actually substituted. A substitution that
        quietly did not happen produces a layout that looks complete and names
        the wrong fields, which is the failure mode R2 exists for."""
        return not self.missing and not self.cyclic and not self.unapplied


def assemble(
    name: str,
    resolution: Resolution,
    replacing: Sequence[Tuple[str, str]] = (),
    _stack: Tuple[str, ...] = (),
) -> Assembly:
    """Return ``name``'s text with nested COPY expanded and REPLACING applied.

    A missing nested member is recorded in ``Assembly.missing`` and the
    directive is left in place as a comment marker rather than dropped
    silently: a layout built over a hole must be able to say where the hole is
    (R2). :func:`src.discovery.layout.compute` refuses to publish a length for
    a record whose assembly is incomplete.
    """
    upper = name.upper()
    if upper in _stack:
        return Assembly(text="", cyclic=[" -> ".join((*_stack, upper))])
    source = resolution.records.get(upper)
    if source is None:
        return Assembly(text="", missing=[upper])

    unapplied = list(replacing_limitations(source.text, replacing)) if replacing else []
    text = apply_replacing(source.text, replacing) if replacing else source.text
    assembly = Assembly(text=text, expanded=[upper], unapplied=unapplied)

    nested = find_copy_directives(text, source.path)
    if not nested:
        return assembly

    lines = text.replace("\r\n", "\n").replace("\r", "\n").split("\n")
    replacements: Dict[int, List[str]] = {}
    for directive in nested:
        child = assemble(directive.name, resolution, directive.replacing, _stack + (upper,))
        assembly.expanded.extend(child.expanded)
        assembly.missing.extend(child.missing)
        assembly.cyclic.extend(child.cyclic)
        assembly.unapplied.extend(child.unapplied)
        body = child.text.replace("\r\n", "\n").replace("\r", "\n").split("\n") if child.text else []
        replacements.setdefault(directive.lineno, []).extend(body)

    out: List[str] = []
    for lineno, line in enumerate(lines, 1):
        if lineno in replacements:
            out.extend(replacements[lineno])
        else:
            out.append(line)
    assembly.text = "\n".join(out)
    return assembly
