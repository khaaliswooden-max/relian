"""WP-2.7 D40 / acceptance (9) — read a returned verification run.

Produces, per construct: **confirm**, **contradict**, or **unknown**.

THE RULE THAT GOVERNS EVERYTHING IN THIS FILE
---------------------------------------------
Anything a customer returns is a measurement of **their compiler at their
settings** — never generalised to "IBM". A run from Enterprise COBOL 6.3 with
``LP(64),ARCH(12)`` is evidence about Enterprise COBOL 6.3 at ``LP(64),
ARCH(12)``, and recording it as "IBM behaves like X" would manufacture a
general claim out of one observation. So:

* the compiler version and the options in force are **required**. Without
  them there is no provenance, and a measurement with no provenance is not a
  measurement (R9). :func:`ingest` refuses rather than recording it.
* every produced record names the compiler and options it came from, and the
  digest of the file it was read out of.
* a ``contradict`` is a **finding**, not an error. It updates the rule table
  for that compiler at those settings, and the update is emitted as a proposal
  a human applies -- this module does not rewrite
  :mod:`src.discovery.dialects.ibm_enterprise_cobol` on its own. A rule table
  that edits itself from an inbound file is a rule table with no custody.

WHAT IT READS
-------------
``relian-discovery-verify/layout/v1`` — the JSON that
``discovery-verify/IBMLAYOUT.cbl`` emits, and a **normalised** form for the
fields that program cannot reach on IBM (see ``README.md``: reference
modification is restricted to ``USAGE DISPLAY``, ``DISPLAY-1`` and
``NATIONAL``, so binary and packed rows come back from the compiler's own
``MAP`` listing instead). The normalised form is three fields per row —
``name``, ``offset``, ``length`` — because a listing parser this project
cannot test against a real IBM listing would be unverified code shipped as
though it were verified.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from dataclasses import dataclass, field as dc_field
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

_REPO_ROOT = Path(__file__).resolve().parents[1]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from src.discovery.dialects import (  # noqa: E402
    GNUCOBOL_3_1_2,
    IBM_ENTERPRISE_COBOL,
    analyse_text,
    resolve,
)
from src.discovery.dialects.classify import Classification  # noqa: E402
from src.discovery.layout import (  # noqa: E402
    BINARY_USAGES,
    PACKED_USAGES,
    expand_picture,
    picture_digits,
)

KIT_SCHEMA = "relian-discovery-verify/layout/v1"
NORMALISED_SCHEMA = "relian-discovery-verify/normalised/v1"


class Verdict(str, Enum):
    CONFIRM = "confirm"
    CONTRADICT = "contradict"
    UNKNOWN = "unknown"


class ProvenanceMissing(ValueError):
    """The returned file does not say what measured it.

    Refused, not defaulted. "Probably their production compiler" is not a
    provenance field, and a width recorded without one would be indistinguishable
    from a width we invented.
    """


@dataclass(frozen=True)
class Provenance:
    """What measured this, at what settings, and where it was read from."""

    compiler_version: str
    compiler_options: str
    source_file: str
    source_sha256: str
    returned_by: Optional[str] = None
    returned_at: Optional[str] = None

    def __post_init__(self) -> None:
        for name in ("compiler_version", "compiler_options"):
            if not str(getattr(self, name) or "").strip():
                raise ProvenanceMissing(
                    f"{name} is empty. Anything a customer returns is a "
                    f"measurement of THEIR compiler at THEIR settings; without "
                    f"both, the run cannot be recorded as a measurement of "
                    f"anything (R9, D40)."
                )

    def to_dict(self) -> Dict[str, Optional[str]]:
        return {
            "compiler_version": self.compiler_version,
            "compiler_options": self.compiler_options,
            "source_file": self.source_file,
            "source_sha256": self.source_sha256,
            "returned_by": self.returned_by,
            "returned_at": self.returned_at,
            "generalisable_to_ibm": False,
            "note": (
                "A measurement of this compiler at these options. Not "
                "generalised to IBM Enterprise COBOL as a product."
            ),
        }


def construct_key(picture: Optional[str], usage: str) -> str:
    """Name the CONSTRUCT a field exercises, so verdicts map onto rules.

    Per-field verdicts are not directly useful: a customer confirming
    ``WS-CTR`` tells us nothing reusable, whereas a customer confirming "COMP
    at 1-2 digit positions" is a statement about a rule. The key is therefore
    the usage family plus the digit band the field falls in.
    """
    # The engine stores a bare PICTURE ("S9(04)"); the oracle renders it with
    # the usage appended ("S9(04) COMP"). Both reach this function, and a
    # trailing usage token silently derails the digit count -- measured:
    # "S9(04) COMP" came back as the 1-2 digit band, because the parse gave up
    # and returned no symbols. Strip it rather than trusting the caller.
    bare = (picture or "").strip().split()[0] if picture else ""
    symbols = expand_picture(bare) if bare else None
    digits = picture_digits(symbols) if symbols else 0
    if usage in BINARY_USAGES:
        band = (
            "1-2" if digits <= 2 else
            "3-4" if digits <= 4 else
            "5-9" if digits <= 9 else
            "10-18"
        )
        return f"{usage}/{band} digits"
    if usage in PACKED_USAGES:
        return f"{usage}/{digits} digits"
    return f"{usage}"


@dataclass(frozen=True)
class ConstructVerdict:
    construct: str
    verdict: Verdict
    fields: Tuple[str, ...]
    projected_length: Optional[int]
    returned_length: Optional[int]
    detail: str

    def to_dict(self) -> Dict[str, object]:
        return {
            "construct": self.construct,
            "verdict": self.verdict.value,
            "fields": list(self.fields),
            "projected_length": self.projected_length,
            "returned_length": self.returned_length,
            "detail": self.detail,
        }


@dataclass(frozen=True)
class RuleUpdate:
    """A proposal, not an applied edit."""

    construct: str
    rule: str
    was_projected: Optional[int]
    now_measured: Optional[int]
    provenance: Provenance

    def to_dict(self) -> Dict[str, object]:
        return {
            "construct": self.construct,
            "rule": self.rule,
            "was_projected": self.was_projected,
            "now_measured": self.now_measured,
            "provenance": self.provenance.to_dict(),
            "status": "PROPOSED — a human applies this; nothing self-edits",
        }


def _rows_from(document: Dict[str, object]) -> List[Dict[str, object]]:
    schema = document.get("schema")
    if schema not in (KIT_SCHEMA, NORMALISED_SCHEMA):
        raise ValueError(
            f"unrecognised schema {schema!r}; expected {KIT_SCHEMA!r} (the "
            f"IBMLAYOUT.cbl output) or {NORMALISED_SCHEMA!r} (rows transcribed "
            f"from a MAP listing)"
        )
    rows = document.get("fields")
    if not isinstance(rows, list):
        raise ValueError("the returned document carries no 'fields' list")
    return rows


def ingest(
    returned: Dict[str, object],
    copybook_text: str,
    provenance: Provenance,
    *,
    dialect: str = IBM_ENTERPRISE_COBOL.id,
    odo_value: Optional[int] = None,
    record: Optional[str] = None,
) -> Dict[str, object]:
    """Compare a returned run against the projection, per construct."""
    profile = resolve(dialect)
    report = analyse_text(
        copybook_text, GNUCOBOL_3_1_2, profile,
        odo_value=odo_value, origin="<returned>", record=record,
    )
    if report is None:
        raise ValueError("no record could be parsed from the copybook")

    projected = {f.key: f for f in report.fields}

    # Two indexes, because the two accepted schemas identify a row
    # differently. IBMLAYOUT.cbl emits the engine's expanded `key`
    # ("SM-ENTRY-CTR (1)"); the normalised MAP form documented in README.md
    # supplies a bare `name` ("SM-ENTRY-CTR"), because a MAP listing reports a
    # table member ONCE rather than per occurrence. Looking up only by key
    # meant every MAP row for an OCCURS member went unmatched -- and those are
    # exactly the rows IBM forces down the MAP path, since a COMP item inside
    # a table cannot be reference-modified there.
    rows = _rows_from(returned)
    by_key = {str(r["key"]): r for r in rows if r.get("key")}
    by_name: Dict[str, Dict[str, object]] = {}
    for r in rows:
        name = r.get("name")
        if name and str(name) not in by_name:
            by_name[str(name)] = r

    def returned_for(field) -> Optional[Dict[str, object]]:
        row = by_key.get(field.key)
        if row is not None:
            return row
        # A member width from MAP applies to every occurrence of that member,
        # so a name match is sound for the LENGTH comparison below even though
        # the offsets differ per occurrence.
        return by_name.get(field.name)

    # Group both sides by construct.
    # Grouped per construct, but compared PER FIELD and then aggregated. A
    # construct like DISPLAY holds many different widths (X(10), X(03), ...),
    # so reducing it to one projected number and one returned number collapses
    # both to "several" and then compares them equal — a false confirm on a
    # construct nobody checked. The comparison has to happen where the two
    # sides are actually commensurable, which is the field.
    grouped: Dict[str, List[Tuple[str, Optional[int], Optional[int]]]] = {}
    for key, pf in projected.items():
        if not pf.elementary:
            # A group's length is the sum of its members plus any slack; it is
            # not a construct with a width rule of its own, so folding it in
            # would let one group confirm or contradict a rule it does not
            # exercise.
            continue
        ck = construct_key(pf.picture, pf.usage)
        row = returned_for(pf)
        returned_length = row.get("length") if row is not None else None
        grouped.setdefault(ck, []).append(
            (key, pf.projected_length, returned_length)
        )

    verdicts: List[ConstructVerdict] = []
    updates: List[RuleUpdate] = []
    for ck in sorted(grouped):
        rows = sorted(grouped[ck])
        fields = tuple(k for k, _p, _r in rows)

        missing = [k for k, _p, r in rows if r is None]
        unsourced = [k for k, p, _r in rows if p is None]
        disagreeing = [
            (k, p, r) for k, p, r in rows
            if p is not None and r is not None and p != r
        ]
        agreeing = [
            (k, p, r) for k, p, r in rows
            if p is not None and r is not None and p == r
        ]

        if disagreeing:
            k0, p0, r0 = disagreeing[0]
            verdicts.append(ConstructVerdict(
                ck, Verdict.CONTRADICT, fields, p0, r0,
                f"{len(disagreeing)} of {len(rows)} field(s) disagree — "
                f"{k0} was projected {p0} byte(s) and returned {r0}. The rule "
                f"table is wrong for this compiler at these options.",
            ))
            updates.append(RuleUpdate(
                construct=ck,
                rule="binary_width" if "COMP" in ck and "COMP-3" not in ck
                     else "packed_width" if "COMP-3" in ck else "display_width",
                was_projected=p0,
                now_measured=r0,
                provenance=provenance,
            ))
            continue

        if unsourced:
            verdicts.append(ConstructVerdict(
                ck, Verdict.UNKNOWN, fields, None,
                next((r for _k, _p, r in rows if r is not None), None),
                f"no rule is sourced for {len(unsourced)} field(s) of this "
                f"construct, so a returned measurement has nothing to confirm "
                f"or contradict; it is recorded as new evidence",
            ))
            continue

        if not agreeing:
            # NOTHING came back for this construct. That is absent evidence,
            # so UNKNOWN.
            verdicts.append(ConstructVerdict(
                ck, Verdict.UNKNOWN, fields,
                next((p for _k, p, _r in rows if p is not None), None), None,
                f"the returned run reports no row for any of the "
                f"{len(rows)} field(s) of this construct — most often because "
                f"IBM rejected the reference-modified probe for a binary or "
                f"packed field (see README.md)",
            ))
            continue

        # Some rows came back and every one of them agreed. Reporting UNKNOWN
        # because a SIBLING row was absent would throw away real evidence: the
        # rows that did come back were measured on the customer's compiler and
        # they confirmed the rule. The incomplete coverage is stated instead of
        # being converted into ignorance.
        k0, p0, r0 = agreeing[0]
        coverage = (
            f"all {len(agreeing)} field(s) of this construct returned the "
            f"projected width"
        )
        if missing:
            coverage += (
                f"; {len(missing)} further field(s) returned no row and are "
                f"neither confirmed nor contradicted"
            )
        verdicts.append(ConstructVerdict(
            ck, Verdict.CONFIRM, fields, p0, r0, coverage,
        ))

    counts = {v.value: 0 for v in Verdict}
    for v in verdicts:
        counts[v.verdict.value] += 1

    return {
        "schema": "relian-discovery-verify/ingest/v1",
        "record": report.group,
        "dialect_projected": profile.id,
        "provenance": provenance.to_dict(),
        "counts": counts,
        "verdicts": [v.to_dict() for v in verdicts],
        "rule_updates": [u.to_dict() for u in updates],
        "note": (
            "confirm/contradict is measured against the PROJECTION. A confirm "
            "converts that construct from projected to measured FOR THIS "
            "COMPILER AT THESE OPTIONS only."
        ),
    }


def main(argv: Optional[Sequence[str]] = None) -> int:
    ap = argparse.ArgumentParser(
        prog="ingest_verification",
        description="Read a returned IBMLAYOUT run and report confirm/"
                    "contradict/unknown per construct.",
    )
    ap.add_argument("returned", help="the JSON file the customer sent back")
    ap.add_argument("--copybook", required=True, help="the copybook it was run against")
    ap.add_argument("--compiler-version", required=True,
                    help="e.g. 'IBM Enterprise COBOL for z/OS 6.3.0'")
    ap.add_argument("--compiler-options", required=True,
                    help="the options in force, e.g. 'LP(64),ARCH(12),TRUNC(STD)'")
    ap.add_argument("--returned-by", default=None)
    ap.add_argument("--returned-at", default=None)
    ap.add_argument("--dialect", default=IBM_ENTERPRISE_COBOL.id)
    ap.add_argument("--odo", type=int, default=None)
    args = ap.parse_args(argv)

    raw = Path(args.returned).read_bytes()
    provenance = Provenance(
        compiler_version=args.compiler_version,
        compiler_options=args.compiler_options,
        source_file=Path(args.returned).as_posix(),
        source_sha256=hashlib.sha256(raw).hexdigest(),
        returned_by=args.returned_by,
        returned_at=args.returned_at,
    )
    result = ingest(
        json.loads(raw.decode("utf-8")),
        Path(args.copybook).read_text(encoding="utf-8", errors="replace"),
        provenance,
        dialect=args.dialect,
        odo_value=args.odo,
    )
    print(json.dumps(result, indent=2, sort_keys=True))
    return 0 if result["counts"][Verdict.CONTRADICT.value] == 0 else 3


if __name__ == "__main__":
    raise SystemExit(main())
