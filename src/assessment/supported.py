"""WP-1.2 — what the C1 transpiler actually supports.

Nothing in this module is a hand-maintained list of capabilities. There are
exactly two sources of truth, and both are read from the transpiler at call
time:

1. **Verbs** come from ``transpiler.c1_rulebased.SUPPORTED_STATEMENTS`` — the
   dispatch table ``Transpiler.stmt()`` itself uses. A verb is supported iff
   calling ``stmt()`` on it reaches a handler instead of ``unsupported()``.
   There is no second copy to drift from the first.

2. **DATA DIVISION features** are *probed*: each feature is a minimal COBOL
   program that is actually run through ``Transpiler.__init__`` and the
   resulting ``fields`` model inspected. If someone teaches the transpiler
   ``USAGE COMP-3``, the probe result changes on the next run without anyone
   editing this file. If someone removes support, the same happens.

Data features are three-state, because "accepted" and "supported" are not the
same thing and conflating them is how a migration silently loses semantics:

``supported``
    The clause is parsed *and* represented in the transpiler's model.
``accepted_ignored``
    The declaration parses and the field exists, but the clause itself is
    discarded — the generated Java cannot depend on it. Ignoring a clause is
    not supporting it; this state exists so a report can say so.
``unsupported``
    The declaration does not survive parsing at all; referencing the field
    later raises.
"""

from __future__ import annotations

import hashlib
import subprocess
from functools import lru_cache
from pathlib import Path
from typing import Dict, FrozenSet, Tuple

from transpiler.c1_rulebased import SUPPORTED_STATEMENTS, VERBS, Transpiler

TRANSPILER_PATH = Path(__file__).resolve().parents[2] / "transpiler" / "c1_rulebased.py"

STATUS_SUPPORTED = "supported"
STATUS_ACCEPTED_IGNORED = "accepted_ignored"
STATUS_UNSUPPORTED = "unsupported"


def supported_verbs() -> FrozenSet[str]:
    """Every statement keyword that reaches a handler in the transpiler."""
    return frozenset(SUPPORTED_STATEMENTS)


def boundary_only_tokens() -> FrozenSet[str]:
    """Tokens that split statements but have no handler.

    These are the trap: they look like supported verbs because they appear in
    ``VERBS``, but a statement led by one of them is silently dropped
    (``SUBTRACT``) or consumed by another handler (``AT``, ``END-SEARCH``,
    ``END-UNSTRING``). Reporting them as supported would be a constant
    pretending to be a measurement.
    """
    return frozenset(VERBS) - supported_verbs()


@lru_cache(maxsize=1)
def transpiler_sha256() -> str:
    return hashlib.sha256(TRANSPILER_PATH.read_bytes()).hexdigest()


@lru_cache(maxsize=1)
def git_sha() -> str:
    """Short git SHA of the repo, or ``unknown`` — never a fabricated value."""
    try:
        out = subprocess.run(
            ["git", "-C", str(TRANSPILER_PATH.parent), "rev-parse", "--short", "HEAD"],
            capture_output=True, text=True, timeout=10,
        )
    except Exception:
        return "unknown"
    if out.returncode != 0:
        return "unknown"
    return out.stdout.strip() or "unknown"


def registry_provenance() -> str:
    return f"SUPPORTED_STATEMENTS@{git_sha()} (c1_rulebased.py sha256:{transpiler_sha256()[:16]})"


# --------------------------------------------------------------------------
# DATA DIVISION probes
# --------------------------------------------------------------------------

_PROGRAM_TEMPLATE = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROBE.
       DATA DIVISION.
{file_section}\
       WORKING-STORAGE SECTION.
{ws}
       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
"""


def _probe_fields(ws_lines: Tuple[str, ...], file_section: str = "") -> Dict[str, object]:
    ws = "\n".join("       " + line for line in ws_lines)
    src = _PROGRAM_TEMPLATE.format(ws=ws, file_section=file_section)
    return Transpiler(src, "Probe").fields


def _status(fields: Dict[str, object], name: str, represented) -> str:
    f = fields.get(name)
    if f is None:
        return STATUS_UNSUPPORTED
    return STATUS_SUPPORTED if represented(f) else STATUS_ACCEPTED_IGNORED


# (feature, working-storage lines, probed field name, "is the clause modelled?")
_PROBES = (
    ("PIC 9 unsigned integer",
     ("01 WS-P PIC 9(5).",), "WS-P",
     lambda f: f.kind == "num" and f.intd == 5 and f.dec == 0),
    ("PIC X alphanumeric",
     ("01 WS-P PIC X(10).",), "WS-P",
     lambda f: f.kind == "alpha" and f.length == 10),
    ("PIC S9 signed",
     ("01 WS-P PIC S9(5).",), "WS-P",
     lambda f: f.signed),
    ("PIC 9V9 implied decimal",
     ("01 WS-P PIC 9(3)V99.",), "WS-P",
     lambda f: f.dec == 2),
    ("edited picture (Z / - / .)",
     ("01 WS-P PIC ZZ9.99.",), "WS-P",
     lambda f: f.kind == "edit"),
    ("88-level condition name",
     ("01 WS-P PIC X(2).", '88 WS-YES VALUE "AB".'), "WS-P",
     lambda f: bool(f.conds)),
    ("OCCURS fixed size",
     ("01 WS-T.", "05 WS-E PIC 9(3) OCCURS 5."), "WS-E",
     lambda f: f.occurs == 5),
    ("OCCURS DEPENDING ON (variable size)",
     ("01 WS-N PIC 9(2).", "01 WS-T.",
      "05 WS-E PIC 9(3) OCCURS 5 DEPENDING ON WS-N."), "WS-E",
     lambda f: getattr(f, "depending_on", None) is not None),
    ("USAGE COMP-3 (packed decimal)",
     ("01 WS-P PIC S9(5)V99 COMP-3.",), "WS-P",
     lambda f: getattr(f, "usage", None) is not None),
    ("USAGE COMP / BINARY",
     ("01 WS-P PIC S9(9) COMP.",), "WS-P",
     lambda f: getattr(f, "usage", None) is not None),
    ("REDEFINES",
     ("01 WS-P PIC X(5).", "01 WS-R REDEFINES WS-P PIC 9(5)."), "WS-R",
     lambda f: getattr(f, "redefines", None) is not None),
    ("VALUE clause on a data item",
     ("01 WS-P PIC 9(3) VALUE 42.",), "WS-P",
     lambda f: getattr(f, "value", None) is not None),
    ("PIC A alphabetic",
     ("01 WS-P PIC A(5).",), "WS-P",
     lambda f: f.kind == "alpha"),
    ("PIC with check protect (*)",
     ("01 WS-P PIC ***9.",), "WS-P",
     lambda f: f.kind == "edit"),
    ("PIC with CR / DB sign",
     ("01 WS-P PIC 9(5)CR.",), "WS-P",
     lambda f: getattr(f, "sign_trailing", None) is not None),
    ("SIGN IS SEPARATE",
     ("01 WS-P PIC S9(5) SIGN IS LEADING SEPARATE.",), "WS-P",
     lambda f: getattr(f, "sign_separate", None) is not None),
    ("FILE SECTION (FD) record",
     ("01 WS-P PIC 9(3).",), "CUST-REC",
     lambda f: True),
)

_FILE_SECTION = ("       FILE SECTION.\n"
                 "       FD  CUSTFILE.\n"
                 "       01  CUST-REC PIC X(80).\n")


@lru_cache(maxsize=1)
def supported_data_features() -> Dict[str, str]:
    """Probe the transpiler's DATA DIVISION handling. Measured, not asserted."""
    out: Dict[str, str] = {}
    for feature, ws_lines, probe_name, represented in _PROBES:
        file_section = _FILE_SECTION if "FILE SECTION" in feature else ""
        try:
            fields = _probe_fields(ws_lines, file_section)
        except Exception:
            out[feature] = STATUS_UNSUPPORTED
            continue
        out[feature] = _status(fields, probe_name, represented)
    return dict(sorted(out.items()))


def data_features_by_status(status: str) -> FrozenSet[str]:
    return frozenset(f for f, s in supported_data_features().items() if s == status)


def data_feature_provenance() -> str:
    return (f"probed against Transpiler.__init__/parse_working_storage@{git_sha()} "
            f"(c1_rulebased.py sha256:{transpiler_sha256()[:16]})")
