"""WP-2.7 D35/D39 — what a dialect profile *is*, and what a rule must carry.

A profile is three things and no more: a **usage -> width function**, an
**alignment rule**, and **per-rule provenance**. Everything else in this
package is a consequence of those three.

The provenance requirement is the load-bearing part, and it is R9 applied to a
*rule* rather than to a number. "We believe IBM does X" is not a basis for an
offset a customer will load data with, so a rule arrives one of exactly two
ways:

``measured``
    The rule reproduces something RELIAN-DISCOVERY-BENCH v0.1 actually
    measured. Its provenance is the seal, and :mod:`gnucobol_3_1_2` derives it
    from ``oracle.json`` rather than typing it in -- a hand-typed table that
    disagreed with the seal would be a second source of truth, and the seal
    would lose.

``sourced``
    The rule is quoted from published vendor documentation, and it carries the
    manual, the section, and the statement relied on. A rule that cannot be
    sourced **is not written**: the construct is classified ``UNKNOWN`` and the
    GnuCOBOL measurement is reported alone.

:class:`Citation` refuses to exist without those three fields, and
:class:`Rule` refuses to exist without a citation when its kind is
``sourced``. Both raise at **construction** time. Since profiles build their
rule tables at module scope, that means a citation-less rule raises at
**import** -- not at use, where it would already be inside a report a customer
is reading (acceptance (4)).
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Callable, Dict, Mapping, Optional, Tuple


class CitationError(ValueError):
    """A rule was declared ``sourced`` without a usable citation.

    Raised at import, deliberately. The alternative -- discovering it when a
    projection is rendered -- means the defect ships and is found by the
    customer holding the offsets.
    """


class RuleKind(str, Enum):
    """How a rule knows what it claims."""

    MEASURED = "measured"
    SOURCED = "sourced"


class ProfileKind(str, Enum):
    """Whether a profile's output is a measurement or a projection.

    D36. ``MEASURED`` is the sealed oracle's own compiler: the numbers are the
    ones the benchmark measured. ``PROJECTED`` is the layout *implied by*
    applying a sourced rule table to the same parse. The two must never render
    in the same column without a label distinguishing them, which is why this
    distinction is a field on the profile and not a convention in the report.
    """

    MEASURED = "measured"
    PROJECTED = "projected"


@dataclass(frozen=True)
class Citation:
    """One vendor statement, quoted rather than summarised.

    ``statement`` is the sentence the rule *relies on*, verbatim. A paraphrase
    is not a citation: the point of carrying it is that a reader can check
    whether the rule follows from the quote, and a paraphrase has already done
    that reasoning for them invisibly.
    """

    manual: str
    version: str
    section: str
    statement: str
    url: Optional[str] = None
    retrieved: Optional[str] = None

    def __post_init__(self) -> None:
        for name in ("manual", "version", "section", "statement"):
            value = getattr(self, name)
            if not isinstance(value, str) or not value.strip():
                raise CitationError(
                    f"Citation.{name} is empty. A sourced rule carries the "
                    f"manual, the section and the statement relied on (D39); "
                    f"a rule that cannot be sourced is not written -- the "
                    f"construct is classified UNKNOWN instead."
                )

    def render(self) -> str:
        """One-line form for report footnotes."""
        where = f"{self.manual} {self.version}, {self.section}"
        if self.url:
            where = f"{where} ({self.url})"
        return f"{where}: “{self.statement}”"

    def to_dict(self) -> Dict[str, Optional[str]]:
        return {
            "manual": self.manual,
            "version": self.version,
            "section": self.section,
            "statement": self.statement,
            "url": self.url,
            "retrieved": self.retrieved,
        }


@dataclass(frozen=True)
class Rule:
    """A named layout rule plus the provenance that licenses it."""

    name: str
    kind: RuleKind
    summary: str
    citation: Optional[Citation] = None
    #: What the rule was derived from, when ``kind`` is ``MEASURED``.
    derived_from: Optional[str] = None

    def __post_init__(self) -> None:
        if self.kind is RuleKind.SOURCED and self.citation is None:
            raise CitationError(
                f"rule {self.name!r} is declared SOURCED but carries no "
                f"citation. D39: a rule that cannot be sourced is not "
                f"written; classify the construct UNKNOWN instead of "
                f"inferring from another compiler, a forum post, or what "
                f"seems reasonable."
            )
        if self.kind is RuleKind.MEASURED and not self.derived_from:
            raise CitationError(
                f"rule {self.name!r} is declared MEASURED but names nothing it "
                f"was derived from. A measured rule's provenance is the "
                f"artifact that measured it."
            )

    def to_dict(self) -> Dict[str, object]:
        return {
            "name": self.name,
            "kind": self.kind.value,
            "summary": self.summary,
            "derived_from": self.derived_from,
            "citation": self.citation.to_dict() if self.citation else None,
        }


@dataclass(frozen=True)
class Width:
    """The outcome of asking a profile how wide something is.

    ``bytes_`` is ``None`` when the profile has **no sourced rule** for the
    construct. That is not an error and it is not zero: it is ``UNKNOWN``, and
    :mod:`src.discovery.dialects.classify` turns it into that class rather than
    letting a plausible-looking number through (R1, R2).
    """

    bytes_: Optional[int]
    rule: Optional[str] = None
    reason: Optional[str] = None

    @property
    def known(self) -> bool:
        return self.bytes_ is not None


#: A width function takes the digit count (binary/packed) or the character
#: count (display) and answers in bytes.
WidthFn = Callable[[int], Width]


@dataclass(frozen=True)
class DialectProfile:
    """One compiler's storage rules, with provenance attached to each.

    The three width functions and :meth:`sync_boundary` are the whole
    behavioural surface. :attr:`rules` exists so a report can print *why* a
    number is what it is, and so the citation lint can walk every rule a
    profile claims without importing the profile's internals.
    """

    id: str
    label: str
    kind: ProfileKind
    binary: WidthFn
    packed: WidthFn
    display: WidthFn
    #: ``(digits, width) -> boundary`` in bytes, or ``None`` for "no alignment
    #: rule this profile can source". GnuCOBOL aligns to the item's own width;
    #: IBM aligns by digit count, which is *not* the same function and is the
    #: reason D10 sensitivity is coupled to member width rather than
    #: independent of it.
    sync_boundary: Callable[[int, Optional[int]], Optional[int]]
    rules: Mapping[str, Rule] = field(default_factory=dict)
    #: Set for MEASURED profiles: the sealed artifact the widths came from.
    basis: Optional[str] = None

    @property
    def projected(self) -> bool:
        return self.kind is ProfileKind.PROJECTED

    @property
    def measured(self) -> bool:
        return self.kind is ProfileKind.MEASURED

    def citations(self) -> Tuple[Citation, ...]:
        return tuple(
            r.citation for r in self.rules.values() if r.citation is not None
        )

    def to_dict(self) -> Dict[str, object]:
        return {
            "id": self.id,
            "label": self.label,
            "kind": self.kind.value,
            "basis": self.basis,
            "rules": {name: r.to_dict() for name, r in sorted(self.rules.items())},
        }


def assert_every_rule_is_provenanced(profile: DialectProfile) -> None:
    """Re-check at import what :class:`Rule` already checked at construction.

    Belt and braces on purpose. :class:`Rule` guards the normal path, but a
    profile assembled by a loop, a ``dataclasses.replace``, or a test helper
    could hold a ``Rule`` that was built before a later edit emptied its
    citation. This walks the finished table, so the assertion is about the
    object the report will actually read.
    """
    for name, rule in sorted(profile.rules.items()):
        if rule.kind is RuleKind.SOURCED and rule.citation is None:
            raise CitationError(
                f"profile {profile.id!r} rule {name!r} is SOURCED with no "
                f"citation (D39)"
            )
        if rule.kind is RuleKind.MEASURED and not rule.derived_from:
            raise CitationError(
                f"profile {profile.id!r} rule {name!r} is MEASURED but names "
                f"no artifact it was derived from"
            )
