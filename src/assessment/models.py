"""Frozen data models for the Relian assessment engine.

Two rules are enforced structurally here rather than by convention:

R1 (measure or None). Any number that reaches a report is a :class:`Measured`.
    ``Measured`` has no constructor that omits provenance or grade, and refuses
    a ``None`` value — an unmeasured quantity is represented by the *absence*
    of a ``Measured`` (the field is ``None``), never by a ``Measured`` holding
    a placeholder.

R8 (determinism). Every model is frozen, every collection is a ``tuple``, and
    ``to_dict()`` emits plain JSON types. :func:`canonical_json` is the single
    serialisation used for hashing: sorted keys, no insignificant whitespace,
    UTF-8. Paths are stored in posix form and sorted by that form.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from enum import Enum
from typing import Any, Dict, Literal, Optional, Tuple

GRADES: Tuple[str, ...] = ("VERIFIED", "PLAUSIBLE", "SPECULATIVE")
Grade = Literal["VERIFIED", "PLAUSIBLE", "SPECULATIVE"]

FileKind = Literal["program", "copybook", "jcl", "other"]
LineEnding = Literal["LF", "CRLF", "mixed", "NONE"]


def canonical_json(obj: Any) -> str:
    """The one serialisation used for every hash and every ledger artifact."""
    return json.dumps(obj, sort_keys=True, separators=(",", ":"), ensure_ascii=False)


def _sorted_dict(d: Dict[str, Any]) -> Dict[str, Any]:
    return {k: d[k] for k in sorted(d)}


@dataclass(frozen=True)
class Measured:
    """A number that was actually measured, with how it was measured.

    There is deliberately no default for ``provenance`` or ``grade``: a number
    without a stated origin cannot be constructed at all.
    """

    value: float
    provenance: str
    grade: Grade

    def __post_init__(self) -> None:
        if self.value is None:
            raise ValueError(
                "Measured cannot hold None — an unmeasured quantity is represented "
                "by the absence of a Measured, not by a Measured wrapping None (R1)"
            )
        if not isinstance(self.value, (int, float)) or isinstance(self.value, bool):
            raise ValueError(f"Measured.value must be numeric, got {type(self.value).__name__}")
        if not self.provenance or not self.provenance.strip():
            raise ValueError("Measured.provenance is required and must be non-empty (R9)")
        if self.grade not in GRADES:
            raise ValueError(f"Measured.grade must be one of {GRADES}, got {self.grade!r}")

    def to_dict(self) -> Dict[str, Any]:
        return _sorted_dict(
            {"value": self.value, "provenance": self.provenance, "grade": self.grade}
        )


def measured_to_dict(m: Optional[Measured]) -> Optional[Dict[str, Any]]:
    return m.to_dict() if m is not None else None


# --------------------------------------------------------------------------
# WP-1.1 intake
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class FileRecord:
    rel_path_posix: str
    kind: FileKind
    sha256: str
    size_bytes: int
    encoding_guess: str
    line_ending: LineEnding

    def to_dict(self) -> Dict[str, Any]:
        return _sorted_dict(
            {
                "rel_path_posix": self.rel_path_posix,
                "kind": self.kind,
                "sha256": self.sha256,
                "size_bytes": self.size_bytes,
                "encoding_guess": self.encoding_guess,
                "line_ending": self.line_ending,
            }
        )


@dataclass(frozen=True)
class ProgramInventory:
    """The manifest: every discovered file plus the hash that seals the list."""

    records: Tuple[FileRecord, ...]
    manifest_hash: str

    def by_kind(self, kind: FileKind) -> Tuple[FileRecord, ...]:
        return tuple(r for r in self.records if r.kind == kind)

    def to_dict(self) -> Dict[str, Any]:
        counts: Dict[str, int] = {}
        for r in self.records:
            counts[r.kind] = counts.get(r.kind, 0) + 1
        return _sorted_dict(
            {
                "records": [r.to_dict() for r in self.records],
                "manifest_hash": self.manifest_hash,
                "file_count": len(self.records),
                "counts_by_kind": _sorted_dict(counts),
            }
        )


# --------------------------------------------------------------------------
# WP-1.3 coverage
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class ConstructHit:
    """One occurrence of one construct, and whether C1 can transpile it."""

    verb: str
    supported: bool
    file: str
    line: int
    paragraph: Optional[str] = None
    context: Optional[str] = None   # ANTLR rule context class, when tree-derived

    def to_dict(self) -> Dict[str, Any]:
        return _sorted_dict(
            {
                "verb": self.verb,
                "supported": self.supported,
                "file": self.file,
                "line": self.line,
                "paragraph": self.paragraph,
                "context": self.context,
            }
        )


@dataclass(frozen=True)
class DataFeatureHit:
    """A DATA DIVISION feature found in source, classified against the
    transpiler's *probed* data-feature support (see supported.py)."""

    feature: str
    status: str          # "supported" | "accepted_ignored" | "unsupported"
    file: str
    line: int

    def to_dict(self) -> Dict[str, Any]:
        return _sorted_dict(
            {"feature": self.feature, "status": self.status,
             "file": self.file, "line": self.line}
        )


@dataclass(frozen=True)
class CoverageResult:
    program_id: str
    parse_ok: bool
    method: str                       # "antlr_tree" | "token_scan" | "none"
    total_statements: Optional[int]
    supported_statements: Optional[int]
    coverage_ratio: Optional[Measured]
    unsupported_inventory: Tuple[ConstructHit, ...] = ()
    data_feature_inventory: Tuple[DataFeatureHit, ...] = ()
    parser_errors: Tuple[str, ...] = ()
    error: Optional[str] = None

    def ranked_unsupported(self) -> Tuple[Tuple[str, int], ...]:
        """Unsupported verbs by frequency, descending; ties broken by verb."""
        counts: Dict[str, int] = {}
        for h in self.unsupported_inventory:
            counts[h.verb] = counts.get(h.verb, 0) + 1
        return tuple(sorted(counts.items(), key=lambda kv: (-kv[1], kv[0])))

    def to_dict(self) -> Dict[str, Any]:
        return _sorted_dict(
            {
                "program_id": self.program_id,
                "parse_ok": self.parse_ok,
                "method": self.method,
                "total_statements": self.total_statements,
                "supported_statements": self.supported_statements,
                "coverage_ratio": measured_to_dict(self.coverage_ratio),
                "unsupported_inventory": [h.to_dict() for h in self.unsupported_inventory],
                "unsupported_ranked": [
                    {"verb": v, "count": c} for v, c in self.ranked_unsupported()
                ],
                "data_feature_inventory": [d.to_dict() for d in self.data_feature_inventory],
                "parser_errors": list(self.parser_errors),
                "error": self.error,
            }
        )


# --------------------------------------------------------------------------
# WP-1.4 LOC
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class LocResult:
    program_id: str
    physical: int
    logical: Optional[int]
    comment: int
    blank: int
    dead_paragraphs: Tuple[str, ...] = ()
    logical_method: str = "none"
    note: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return _sorted_dict(
            {
                "program_id": self.program_id,
                "physical": self.physical,
                "logical": self.logical,
                "logical_method": self.logical_method,
                "comment": self.comment,
                "blank": self.blank,
                "dead_paragraphs": list(self.dead_paragraphs),
                "note": self.note,
            }
        )


# --------------------------------------------------------------------------
# WP-1.5 complexity
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class ParagraphComplexity:
    name: str
    statements: int
    decision_points: int
    cyclomatic: int
    goto_count: int
    max_nesting: int

    def to_dict(self) -> Dict[str, Any]:
        return _sorted_dict(
            {
                "name": self.name,
                "statements": self.statements,
                "decision_points": self.decision_points,
                "cyclomatic": self.cyclomatic,
                "goto_count": self.goto_count,
                "max_nesting": self.max_nesting,
            }
        )


@dataclass(frozen=True)
class ComplexityResult:
    program_id: str
    method: str
    cyclomatic: int
    decision_points: int
    statements: int
    goto_count: int
    goto_density: Optional[Measured]
    alter_present: bool
    perform_thru_spans: Tuple[str, ...]
    exec_cics_count: int
    exec_sql_count: int
    copybook_fan_out: Tuple[str, ...]
    call_targets: Tuple[str, ...]
    max_nesting_depth: int
    paragraphs: Tuple[ParagraphComplexity, ...] = ()

    def to_dict(self) -> Dict[str, Any]:
        return _sorted_dict(
            {
                "program_id": self.program_id,
                "method": self.method,
                "cyclomatic": self.cyclomatic,
                "decision_points": self.decision_points,
                "statements": self.statements,
                "goto_count": self.goto_count,
                "goto_density": measured_to_dict(self.goto_density),
                "alter_present": self.alter_present,
                "perform_thru_spans": list(self.perform_thru_spans),
                "exec_cics_count": self.exec_cics_count,
                "exec_sql_count": self.exec_sql_count,
                "copybook_fan_out": list(self.copybook_fan_out),
                "call_targets": list(self.call_targets),
                "max_nesting_depth": self.max_nesting_depth,
                "paragraphs": [p.to_dict() for p in self.paragraphs],
            }
        )


# --------------------------------------------------------------------------
# WP-1.6 risk
# --------------------------------------------------------------------------


class RiskTier(str, Enum):
    LOW = "LOW"
    MED = "MED"
    HIGH = "HIGH"
    BLOCKED = "BLOCKED"


@dataclass(frozen=True)
class RiskFinding:
    program_id: str
    tier: RiskTier
    rule: str                       # the exact rule string that fired
    inputs: Tuple[Tuple[str, Any], ...] = ()

    def to_dict(self) -> Dict[str, Any]:
        return _sorted_dict(
            {
                "program_id": self.program_id,
                "tier": self.tier.value,
                "rule": self.rule,
                "inputs": _sorted_dict(dict(self.inputs)),
                "grade": "PLAUSIBLE",
                "provenance": "policy: RISK_RULES table in src/assessment/risk.py "
                              "(a published rule, not a measurement); inputs are VERIFIED",
            }
        )


# --------------------------------------------------------------------------
# Bundle
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class ProgramAssessment:
    """Everything measured about one program."""

    program_id: str
    file: FileRecord
    coverage: CoverageResult
    loc: LocResult
    complexity: Optional[ComplexityResult]
    risk: RiskFinding

    def to_dict(self) -> Dict[str, Any]:
        return _sorted_dict(
            {
                "program_id": self.program_id,
                "file": self.file.to_dict(),
                "coverage": self.coverage.to_dict(),
                "loc": self.loc.to_dict(),
                "complexity": self.complexity.to_dict() if self.complexity else None,
                "risk": self.risk.to_dict(),
            }
        )


@dataclass(frozen=True)
class AssessmentBundle:
    schema_version: str
    tool_versions: Tuple[Tuple[str, str], ...]
    inventory: ProgramInventory
    programs: Tuple[ProgramAssessment, ...]
    portfolio_coverage: CoverageResult
    portfolio_risk: RiskFinding
    quotable_loc: Optional[Measured]
    grammar_expansion_loc: Optional[Measured]
    notes: Tuple[str, ...] = ()

    def to_dict(self) -> Dict[str, Any]:
        return _sorted_dict(
            {
                "schema_version": self.schema_version,
                "tool_versions": _sorted_dict(dict(self.tool_versions)),
                "inventory": self.inventory.to_dict(),
                "programs": [p.to_dict() for p in self.programs],
                "portfolio_coverage": self.portfolio_coverage.to_dict(),
                "portfolio_risk": self.portfolio_risk.to_dict(),
                "quotable_loc": measured_to_dict(self.quotable_loc),
                "grammar_expansion_loc": measured_to_dict(self.grammar_expansion_loc),
                "notes": list(self.notes),
            }
        )
