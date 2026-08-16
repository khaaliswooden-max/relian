"""WP-1.1 — intake: discover and fingerprint a customer codebase.

Read-only, zero network, zero LLM. Nothing in this module writes to, moves,
or normalises the files it inspects; every fingerprint is taken from the raw
bytes on disk.

Counting and classification rules (reproduced in the report appendix):

* **Discovery** is a recursive walk of ``root``. Directories in
  :data:`SKIP_DIRS` are not descended into — they are tool state, not
  customer code, and including them would make the manifest hash depend on
  whether a build had been run.
* **Kind** is decided by extension first (:data:`EXT_KINDS`), then, only for
  files with no extension, by a content sniff for COBOL division markers.
  A ``.cbl`` file is a program even if its contents are garbage: the manifest
  records what the codebase *claims*, and the coverage analyzer records what
  it *is*.
* **sha256** and **size_bytes** are of the raw bytes, before any decoding.
* **line_ending** counts CRLF and lone-LF occurrences: all-CRLF → ``CRLF``,
  all-LF → ``LF``, both present → ``mixed``, no terminator at all → ``NONE``.
  (``NONE`` is a fourth state the work package did not enumerate; a file with
  no line break genuinely has no line ending, and reporting it as ``LF`` would
  be an invented value.)
* **encoding_guess** is a *guess* and is labelled as such: ``utf-8`` if the
  bytes decode as UTF-8, ``binary`` if a NUL byte is present, otherwise
  ``latin-1`` (a decoding that never fails, so it is a fallback label rather
  than a detection claim).
* **Ordering** is by ``rel_path_posix`` string sort (R8), and the manifest
  hash is over the canonical JSON of the ordered records. Paths are relative,
  so the same codebase at two different absolute locations hashes identically.
"""

from __future__ import annotations

import hashlib
from pathlib import Path
from typing import List

from .models import FileRecord, ProgramInventory, canonical_json

SKIP_DIRS = frozenset({".git", "__pycache__", "node_modules", ".venv", ".mypy_cache",
                       ".pytest_cache", ".idea", ".svn"})

EXT_KINDS = {
    ".cbl": "program",
    ".cob": "program",
    ".cobol": "program",
    ".cpy": "copybook",
    ".copy": "copybook",
    ".jcl": "jcl",
    ".proc": "jcl",
}

DIVISION_MARKERS = ("IDENTIFICATION DIVISION", "PROCEDURE DIVISION",
                    "DATA DIVISION", "ENVIRONMENT DIVISION")

SNIFF_BYTES = 65536


def _classify(path: Path, head: bytes) -> str:
    ext = path.suffix.lower()
    if ext in EXT_KINDS:
        return EXT_KINDS[ext]
    if ext:
        return "other"
    text = head.decode("latin-1", errors="replace").upper()
    if any(marker in text for marker in DIVISION_MARKERS):
        return "program"
    if text.lstrip().startswith("//") and " JOB " in text:
        return "jcl"
    return "other"


def _line_ending(raw: bytes) -> str:
    crlf = raw.count(b"\r\n")
    lf = raw.count(b"\n") - crlf
    if crlf and lf:
        return "mixed"
    if crlf:
        return "CRLF"
    if lf:
        return "LF"
    return "NONE"


def _encoding_guess(raw: bytes) -> str:
    if b"\x00" in raw:
        return "binary"
    try:
        raw.decode("utf-8")
        return "utf-8"
    except UnicodeDecodeError:
        return "latin-1"


def read_source(path: Path) -> str:
    """Decode a source file for analysis, mirroring :func:`_encoding_guess`.

    Line endings are *not* normalised here; analyzers split on ``\\n`` after
    stripping ``\\r`` themselves, so a CRLF codebase and an LF codebase produce
    identical analysis (R8) without this function having to rewrite anything.
    """
    raw = path.read_bytes()
    try:
        return raw.decode("utf-8")
    except UnicodeDecodeError:
        return raw.decode("latin-1")


def ingest(root: Path) -> List[FileRecord]:
    """Recursively fingerprint every file under ``root``.

    Returns records sorted by ``rel_path_posix`` (R8).
    """
    root = Path(root)
    if not root.exists():
        raise FileNotFoundError(f"assessment root does not exist: {root}")
    if root.is_file():
        files = [root]
        base = root.parent
    else:
        files = []
        base = root
        stack = [root]
        while stack:
            current = stack.pop()
            for child in current.iterdir():
                if child.is_symlink():
                    continue          # never follow links out of the perimeter
                if child.is_dir():
                    if child.name not in SKIP_DIRS:
                        stack.append(child)
                elif child.is_file():
                    files.append(child)

    records: List[FileRecord] = []
    for path in files:
        raw = path.read_bytes()
        records.append(
            FileRecord(
                rel_path_posix=path.relative_to(base).as_posix(),
                kind=_classify(path, raw[:SNIFF_BYTES]),      # type: ignore[arg-type]
                sha256=hashlib.sha256(raw).hexdigest(),
                size_bytes=len(raw),
                encoding_guess=_encoding_guess(raw),
                line_ending=_line_ending(raw),                # type: ignore[arg-type]
            )
        )
    records.sort(key=lambda r: r.rel_path_posix)
    return records


def manifest_hash(records: List[FileRecord]) -> str:
    payload = canonical_json([r.to_dict() for r in records])
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def build_inventory(root: Path) -> ProgramInventory:
    records = ingest(root)
    return ProgramInventory(records=tuple(records), manifest_hash=manifest_hash(records))
