"""WP-2.7 D35/D37 — classify every field, and compute the downstream shift.

Three classes, and every field carries exactly one (acceptance (2)):

``INVARIANT``
    Both profiles allocate the same width at the same offset **by the same
    rule**. Length and offset are as measured, and a customer can load with
    them.

``DIALECT_SENSITIVE``
    The profiles disagree -- either on the number (width or offset) or on the
    *rule* that produced it. The rule case matters even when this record's
    arithmetic happens to agree: GnuCOBOL aligns a SYNC item to its own width
    and IBM aligns an 18-digit COMP to a fullword, and two different functions
    that coincide at one offset are not the same function. Calling such a field
    ``INVARIANT`` would assert the rule is shared, which is false.

``UNKNOWN``
    At least one profile has no rule. Either the sealed oracle never measured
    the construct (``COMP-1``, ``POINTER``, ...) or no IBM statement could be
    sourced for it. Never guessed: the field reports the GnuCOBOL measurement
    and says the other side is undetermined (R2).

**The shift is the hard part and the valuable part.** A one-byte widening at
offset 12 moves every subsequent field in the record, and inside an ``OCCURS``
it multiplies by the occurrence count -- a table of 40 entries whose member
gains a byte gains 40. That is not modelled by hand here: the projection is
produced by re-running the *same placement code* over the *same parse* with the
other profile's rules, so ``OCCURS``, ``REDEFINES``, ``RENAMES`` and ODO are
handled by the code that is already verified against the oracle rather than by
a second implementation that could disagree with it.
"""

from __future__ import annotations

from dataclasses import dataclass, field as dc_field
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

from ..layout import (
    BINARY_USAGES,
    Field,
    Layout,
    compute_text,
    expand_picture,
    picture_digits,
)
from .base import DialectProfile


class Classification(str, Enum):
    INVARIANT = "INVARIANT"
    DIALECT_SENSITIVE = "DIALECT_SENSITIVE"
    UNKNOWN = "UNKNOWN"


class UnclassifiedField(AssertionError):
    """A field came out of the classifier without a class.

    Cannot happen through :func:`classify_field`, which is total. It exists so
    :func:`lint_sensitivity` can say *which* field and *why* if the invariant is
    ever broken by a later edit, rather than letting a ``None`` reach a report.
    """


@dataclass(frozen=True)
class FieldSensitivity:
    """One field, under both profiles."""

    key: str
    name: str
    level: int
    classification: Classification
    #: MEASURED -- the sealed-oracle-verified numbers. Never relabelled.
    measured_offset: Optional[int]
    measured_length: Optional[int]
    #: PROJECTED -- implied by the other profile's sourced rules. Never
    #: rendered in the same column as the measured pair without a label (D36).
    projected_offset: Optional[int]
    projected_length: Optional[int]
    reason: Optional[str] = None
    rules: Tuple[str, ...] = ()
    occurrences: Optional[int] = None
    elementary: bool = True
    #: Carried through so a consumer can name the CONSTRUCT a field exercises
    #: rather than only the field. A verdict about "WS-CTR" is not reusable; a
    #: verdict about "COMP at 1-2 digit positions" is a statement about a rule.
    picture: Optional[str] = None
    usage: str = "DISPLAY"

    @property
    def width_delta(self) -> Optional[int]:
        if self.measured_length is None or self.projected_length is None:
            return None
        return self.projected_length - self.measured_length

    @property
    def offset_delta(self) -> Optional[int]:
        if self.measured_offset is None or self.projected_offset is None:
            return None
        return self.projected_offset - self.measured_offset

    @property
    def shifted(self) -> bool:
        """Does this field's ADDRESS move, whatever its width does?"""
        return bool(self.offset_delta)

    def to_dict(self) -> Dict[str, object]:
        return {
            "key": self.key,
            "name": self.name,
            "shifted": self.shifted,
            "level": self.level,
            "classification": self.classification.value,
            "elementary": self.elementary,
            "picture": self.picture,
            "usage": self.usage,
            "measured": {
                "offset": self.measured_offset,
                "length": self.measured_length,
                "basis": "measured",
            },
            "projected": {
                "offset": self.projected_offset,
                "length": self.projected_length,
                "basis": "projected",
            },
            "width_delta": self.width_delta,
            "offset_delta": self.offset_delta,
            "reason": self.reason,
            "rules": list(self.rules),
            "occurrences": self.occurrences,
        }


def _digits_of(f: Field) -> int:
    if not f.picture:
        return 0
    symbols = expand_picture(f.picture)
    return picture_digits(symbols) if symbols else 0


def _sync_rule_differs(
    f: Field, measured: DialectProfile, projected: DialectProfile
) -> Optional[str]:
    """Do the two profiles align this item by different functions?

    Only asked of ``SYNCHRONIZED`` binary items, because that is the only
    construct in the corpus where the two profiles' *rules* differ in form
    rather than in the constants they feed. Returns the human reason or
    ``None``.
    """
    if not f.sync or f.usage not in BINARY_USAGES:
        return None
    digits = _digits_of(f)
    a = measured.sync_boundary(digits, f.length)
    b = projected.sync_boundary(digits, f.length)
    if a == b:
        return None
    return (
        f"alignment boundary differs: {measured.id} aligns to {a}, "
        f"{projected.id} aligns to {b} (m keyed on width vs on digit count)"
    )


def classify_field(
    measured_field: Field,
    projected_field: Optional[Field],
    measured: DialectProfile,
    projected: DialectProfile,
) -> FieldSensitivity:
    """Total function: every field comes back with exactly one class."""
    m_off, m_len = measured_field.offset, measured_field.length
    p_off = projected_field.offset if projected_field else None
    p_len = projected_field.length if projected_field else None

    common = dict(
        key=measured_field.key,
        name=measured_field.name,
        level=measured_field.level,
        measured_offset=m_off,
        measured_length=m_len,
        projected_offset=p_off,
        projected_length=p_len,
        occurrences=measured_field.occurs,
        elementary=measured_field.elementary,
        picture=measured_field.picture,
        usage=measured_field.usage,
    )

    if projected_field is None:
        return FieldSensitivity(
            classification=Classification.UNKNOWN,
            reason=(
                f"{projected.id} produced no row for this field; nothing is "
                f"claimed for it"
            ),
            **common,
        )

    # UNKNOWN first: an absent rule on either side outranks any comparison,
    # because comparing a measurement against nothing is not a comparison.
    if m_len is None or p_len is None:
        side = []
        if m_len is None:
            side.append(
                measured_field.unmeasured_reason
                or f"not measured by {measured.id} for this construct"
            )
        if p_len is None:
            side.append(
                projected_field.unmeasured_reason
                or f"{projected.id} has no sourced rule for this construct"
            )
        return FieldSensitivity(
            classification=Classification.UNKNOWN,
            reason="; ".join(side),
            **common,
        )

    rule_reason = _sync_rule_differs(measured_field, measured, projected)

    if m_len != p_len:
        bits = [f"width {m_len} -> {p_len} bytes"]
        if m_off != p_off:
            bits.append(
                f"and its own offset moves {m_off} -> {p_off} "
                f"({p_off - m_off:+d}) because an earlier field widened"
            )
        if rule_reason:
            bits.append(rule_reason)
        return FieldSensitivity(
            classification=Classification.DIALECT_SENSITIVE,
            reason="; ".join(bits),
            rules=("binary_width",),
            **common,
        )

    if rule_reason:
        return FieldSensitivity(
            classification=Classification.DIALECT_SENSITIVE,
            reason=(
                f"{rule_reason}. The two boundaries coincide at this offset, so "
                f"the numbers agree here; the rules do not, so they need not "
                f"agree in another record"
            ),
            rules=("sync_alignment",),
            **common,
        )

    # Allocated identically by the same rule. If it still moved, it moved
    # because something BEFORE it widened -- the field is invariant, its
    # address is not, and the report has to say both. Reading "INVARIANT" as
    # "this offset is safe" is precisely the misreading that would put a
    # customer one byte off, so the shift is spelled out here rather than left
    # to be inferred from two columns.
    if m_off != p_off:
        return FieldSensitivity(
            classification=Classification.INVARIANT,
            reason=(
                f"width is invariant, but the field shifts {m_off} -> {p_off} "
                f"({p_off - m_off:+d}) under {projected.id} because an earlier "
                f"field widens; the offset (measured) does not hold there"
            ),
            **common,
        )
    return FieldSensitivity(classification=Classification.INVARIANT, **common)


@dataclass(frozen=True)
class SensitivityReport:
    """D37 — the deliverable. The measured layout, annotated."""

    group: str
    origin: str
    measured_profile: DialectProfile
    projected_profile: DialectProfile
    fields: Tuple[FieldSensitivity, ...]
    measured_length: Optional[int]
    projected_length: Optional[int]

    @property
    def elementary(self) -> Tuple[FieldSensitivity, ...]:
        return tuple(f for f in self.fields if f.elementary)

    def counts(self) -> Dict[str, int]:
        out = {c.value: 0 for c in Classification}
        for f in self.elementary:
            out[f.classification.value] += 1
        return out

    @property
    def sensitive(self) -> Tuple[FieldSensitivity, ...]:
        return tuple(
            f for f in self.elementary
            if f.classification is Classification.DIALECT_SENSITIVE
        )

    @property
    def unknown(self) -> Tuple[FieldSensitivity, ...]:
        return tuple(
            f for f in self.elementary
            if f.classification is Classification.UNKNOWN
        )

    @property
    def record_delta(self) -> Optional[int]:
        if self.measured_length is None or self.projected_length is None:
            return None
        return self.projected_length - self.measured_length

    def shifted_after(self, key: str) -> Tuple[FieldSensitivity, ...]:
        """Fields whose offset moves because something before them widened."""
        anchor = next((f for f in self.elementary if f.key == key), None)
        if anchor is None or anchor.measured_offset is None:
            return ()
        return tuple(
            f for f in self.elementary
            if f.measured_offset is not None
            and f.measured_offset > anchor.measured_offset
            and (f.offset_delta or 0) != 0
        )

    def headline(self) -> str:
        """D37's sentence, with the invariant count as a positive finding.

        The invariant count is stated as a finding rather than as a remainder
        on purpose: "112 fields are invariant across both profiles" is the part
        a migration planner can act on, and burying it as "the rest" makes the
        honest half of the answer look like an afterthought.
        """
        total = len(self.elementary)
        counts = self.counts()
        n_sens = counts[Classification.DIALECT_SENSITIVE.value]
        n_inv = counts[Classification.INVARIANT.value]
        n_unk = counts[Classification.UNKNOWN.value]

        n_shifted = sum(1 for f in self.elementary if f.shifted)
        n_inv_put = sum(
            1 for f in self.elementary
            if f.classification is Classification.INVARIANT and not f.shifted
        )

        parts = [
            f"{n_inv} of {total} fields in {self.group} are INVARIANT across "
            f"{self.measured_profile.id} and {self.projected_profile.id} — "
            f"allocated identically by the same rule."
        ]
        if n_inv > n_inv_put:
            parts.append(
                f"{n_inv_put} of those also keep the same offset (measured); "
                f"the other {n_inv - n_inv_put} are invariant in width but move "
                f"because an earlier field widens."
            )
        if n_sens:
            parts.append(f"{n_sens} are DIALECT_SENSITIVE.")
            # Grouped by NAME, not per row. Inside an OCCURS every occurrence
            # is its own sensitive field, so a sentence per row turns a
            # 40-entry table into forty near-identical sentences and buries the
            # one number that matters -- the total the record grows by.
            by_name: Dict[str, List[FieldSensitivity]] = {}
            for f in self.sensitive:
                by_name.setdefault(f.name, []).append(f)

            for name, group in by_name.items():
                f = group[0]
                if len(group) > 1 and f.width_delta:
                    total = sum(g.width_delta or 0 for g in group)
                    parts.append(
                        f"Under {self.projected_profile.id}, {name} widens "
                        f"{f.measured_length} -> {f.projected_length} bytes in "
                        f"each of its {len(group)} occurrences, adding "
                        f"{total:+d} bytes to the record."
                    )
                    continue
                if f.width_delta:
                    moved = self.shifted_after(f.key)
                    # The shift is stated EXACTLY, and as a range when the
                    # fields after this one do not all move by the same amount.
                    # Reporting only the maximum would be wrong for the fields
                    # that move less -- on D02_binary, D02-COMP-2 shifts +1
                    # while the nine after it shift +2, so "shift by +2" would
                    # put a customer one byte off on exactly one field. That is
                    # the failure mode this work package exists to prevent, so
                    # it may not appear in the summary sentence either.
                    deltas = sorted({m.offset_delta or 0 for m in moved})
                    if not moved:
                        tail = "."
                    elif len(deltas) == 1:
                        tail = (
                            f" and the {len(moved)} field(s) after it shift by "
                            f"{deltas[0]:+d}."
                        )
                    else:
                        tail = (
                            f" and the {len(moved)} field(s) after it shift by "
                            f"{deltas[0]:+d} to {deltas[-1]:+d}, per field."
                        )
                    parts.append(
                        f"Under {self.projected_profile.id}, {f.name} widens "
                        f"{f.measured_length} -> {f.projected_length} bytes"
                        + tail
                    )
        if n_unk:
            parts.append(
                f"{n_unk} are UNKNOWN — no rule is sourced on at least one "
                f"side, so nothing is claimed."
            )
        if self.record_delta is not None:
            parts.append(
                f"Record length {self.measured_length} (measured) -> "
                f"{self.projected_length} (projected), {self.record_delta:+d}."
            )
        else:
            shown = (
                f"{self.measured_length} bytes"
                if self.measured_length is not None else "undetermined"
            )
            parts.append(
                f"Record length is {shown} (measured); the length (projected) "
                f"is undetermined because an UNKNOWN field has no sourced "
                f"width — it is left undetermined rather than estimated."
            )
        return " ".join(parts)

    def to_dict(self) -> Dict[str, object]:
        return {
            "group": self.group,
            "origin": self.origin,
            "measured_profile": self.measured_profile.to_dict(),
            "projected_profile": self.projected_profile.to_dict(),
            "counts": self.counts(),
            "measured_record_length": self.measured_length,
            "projected_record_length": self.projected_length,
            "record_length_delta": self.record_delta,
            "headline": self.headline(),
            "fields": [f.to_dict() for f in self.fields],
        }


def analyse_text(
    text: str,
    measured: DialectProfile,
    projected: DialectProfile,
    *,
    odo_value: Optional[int] = None,
    origin: str = "<text>",
    record: Optional[str] = None,
) -> Optional[SensitivityReport]:
    """Compare a projection against the measured layout of the same parse."""
    measured_layouts = compute_text(text, odo_value=odo_value, origin=origin)
    projected_layouts = compute_text(
        text, odo_value=odo_value, origin=origin, profile=projected
    )
    if not measured_layouts:
        return None

    def pick(layouts: Sequence[Layout]) -> Optional[Layout]:
        if record is None:
            return layouts[0] if layouts else None
        for lay in layouts:
            if lay.group == record.upper():
                return lay
        return None

    m_layout = pick(measured_layouts)
    p_layout = pick(projected_layouts)
    if m_layout is None:
        return None

    p_rows = p_layout.by_key() if p_layout else {}
    rows = tuple(
        classify_field(f, p_rows.get(f.key), measured, projected)
        for f in m_layout.fields
    )
    return SensitivityReport(
        group=m_layout.group,
        origin=origin,
        measured_profile=measured,
        projected_profile=projected,
        fields=rows,
        measured_length=m_layout.group_length,
        projected_length=p_layout.group_length if p_layout else None,
    )


def analyse_path(
    path: Path,
    measured: DialectProfile,
    projected: DialectProfile,
    *,
    odo_value: Optional[int] = None,
    record: Optional[str] = None,
) -> Optional[SensitivityReport]:
    return analyse_text(
        Path(path).read_text(encoding="utf-8", errors="replace"),
        measured,
        projected,
        odo_value=odo_value,
        origin=Path(path).as_posix(),
        record=record,
    )


def lint_sensitivity(report: SensitivityReport) -> List[str]:
    """Acceptance (2)'s lint: no field may reach a report unclassified.

    Same discipline as WP-2.2 (7)'s ``source`` requirement. A field with no
    class is not a cosmetic omission -- it is a row a reader will read as
    "fine", because an unlabelled offset looks exactly like a verified one.
    """
    problems: List[str] = []
    for f in report.fields:
        if not isinstance(f.classification, Classification):
            problems.append(
                f"{f.key}: classification is {f.classification!r}, not one of "
                f"{[c.value for c in Classification]}"
            )
            continue
        if (f.classification is Classification.INVARIANT and f.shifted
                and not f.reason):
            problems.append(
                f"{f.key}: INVARIANT and shifted {f.offset_delta:+d} but "
                f"carries no reason. An invariant width at a moved address is "
                f"the row most likely to be misread as a safe offset."
            )
            continue
        if f.classification is not Classification.INVARIANT and not f.reason:
            problems.append(
                f"{f.key}: classified {f.classification.value} with no reason. "
                f"A non-invariant class that does not say why cannot be acted "
                f"on or checked."
            )
    keys = [f.key for f in report.fields]
    if len(keys) != len(set(keys)):
        dupes = sorted({k for k in keys if keys.count(k) > 1})
        problems.append(f"duplicate field keys in the report: {dupes}")
    return problems
