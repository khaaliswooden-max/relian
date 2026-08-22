"""WP-2.4 §3.4 — PostgreSQL DDL from a COBOL record layout.

D31 — the mapping is a set of disclosed choices, not a derivation
----------------------------------------------------------------

Some of this mapping is forced by the data. ``PIC S9(7)V99 COMP-3`` is
``NUMERIC(9,2)`` and could not honestly be anything else: the precision and the
scale are *in the picture*, and a target type that held fewer digits would lose
data the source guarantees.

Some of it is judgment. ``PIC X(30)`` → ``CHAR(30)`` or ``VARCHAR(30)``?
Fixed-width records mean the trailing spaces are really there, and whether they
are significant depends on what the customer's target application expects. That
is a decision, and hiding it inside a function makes it a decision nobody can
argue with.

So every rule in :data:`MAPPING_RULES` carries a ``basis`` of ``forced`` or
``choice``, and every ``choice`` carries its ``rationale`` and its
``alternative``. :func:`lint_mapping_rules` asserts none is missing, and
:func:`lint_table` asserts every column a table emits cites a rule that is in
the published table. A customer who disagrees with a choice can see it, argue
with it, and override it.

D32 — REDEFINES and ODO have no honest relational mapping
---------------------------------------------------------

``REDEFINES`` is the same bytes under two interpretations. There are three ways
to be wrong about it and no way to be right:

* emit **both** as columns — wrong, because they are not independent; a row
  cannot hold both readings of the same eight bytes;
* emit **one** silently — worse, because an interpretation is presented as
  *the* interpretation; and
* add a **discriminator** column — an invention, because nothing in the source
  says which reading applies to which record.

Decision: emit the primary definition's columns, emit the redefining views as
**commented DDL**, and mark the table ``PARTIAL`` with the overlap named —
start byte, length, and both field sets. ``OCCURS`` becomes numbered columns
for small fixed tables and is ``PARTIAL`` with a child-table alternative for
large ones; ``OCCURS DEPENDING ON`` is ``PARTIAL`` always, because the extent
is a runtime property and no static analysis can supply it.

A ``PARTIAL`` table is honest output. A ``COMPLETE`` table that quietly picked
one reading of a ``REDEFINES`` is the fabrication mode this repository has
deleted three times.

D33 — the DDL is loaded into a real PostgreSQL, or it is not a claim
--------------------------------------------------------------------

Nothing in this module verifies its own output. Generated DDL that has never
been executed is a plausible artifact, not a measured one; and DDL checked by
re-reading the strings this module produced is the WP-2.2 §0 circularity in a
new place. The ``ddl-loads`` CI job executes every statement against a
``postgres:16`` service and then reconciles ``information_schema.columns``
against :func:`mapping_table` — PostgreSQL answering, not us.

R12: no dataset is opened and no customer record is read. Everything here comes
from a :class:`~src.discovery.layout.Layout`, which was computed from copybook
and COBOL text.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field as _dc_field
from enum import Enum
from typing import Dict, List, Optional, Sequence, Tuple

from .layout import (
    BINARY_USAGES,
    DISPLAY,
    PACKED_USAGES,
    UNCOVERED_USAGES,
    Condition,
    Field,
    Layout,
    LayoutStatus,
    expand_picture,
)

#: The ruleset this module implements, named in every DDL header so a
#: generated schema can be traced to the rules that produced it.
RULESET_VERSION = "relian-ddl-map-v1.0"

#: An OCCURS with at most this many occurrences becomes numbered columns. Above
#: it, the table is PARTIAL and a child table is proposed. The number is a
#: judgement about readability, and it is published rather than buried.
OCCURS_COLUMN_LIMIT = 8


class Basis(str, Enum):
    """Why a mapping rule is what it is."""

    FORCED = "forced"
    CHOICE = "choice"


@dataclass(frozen=True)
class MappingRule:
    """One published mapping rule.

    ``rationale`` and ``alternative`` are mandatory for a ``choice`` and are
    checked by :func:`lint_mapping_rules`. If a choice cannot be justified in
    one sentence to a customer, it is not a choice — it is a guess, and the
    table it produces should be ``PARTIAL`` instead.
    """

    rule_id: str
    cobol_shape: str
    postgres_type: str
    basis: Basis
    rationale: str
    alternative: Optional[str] = None

    def to_dict(self) -> Dict[str, object]:
        return {
            "rule_id": self.rule_id,
            "cobol_shape": self.cobol_shape,
            "postgres_type": self.postgres_type,
            "basis": self.basis.value,
            "rationale": self.rationale,
            "alternative": self.alternative,
        }


MAPPING_RULES: Tuple[MappingRule, ...] = (
    MappingRule(
        rule_id="M01",
        cobol_shape="PIC X(n) — alphanumeric, USAGE DISPLAY",
        postgres_type="CHAR(n)",
        basis=Basis.CHOICE,
        rationale=(
            "The source records are fixed-width, so the trailing spaces in an "
            "X(n) field physically exist and some programs compare against "
            "them. CHAR(n) preserves that; VARCHAR(n) would silently change "
            "the value of every short field."
        ),
        alternative=(
            "VARCHAR(n), if the target application treats trailing spaces as "
            "insignificant and prefers not to store them."
        ),
    ),
    MappingRule(
        rule_id="M02",
        cobol_shape="PIC A(n) — alphabetic, USAGE DISPLAY",
        postgres_type="CHAR(n)",
        basis=Basis.CHOICE,
        rationale=(
            "Same fixed-width argument as M01. PostgreSQL has no alphabetic-"
            "only type, so the alphabetic restriction is not carried over."
        ),
        alternative=(
            "VARCHAR(n), or CHAR(n) with a CHECK constraint enforcing letters "
            "and spaces only, if the alphabetic restriction must survive."
        ),
    ),
    MappingRule(
        rule_id="M03",
        cobol_shape="PIC 9(p)V9(s) / S9(p)V9(s), USAGE DISPLAY",
        postgres_type="NUMERIC(p+s, s)",
        basis=Basis.FORCED,
        rationale=(
            "The precision and the scale are stated in the picture. NUMERIC "
            "with those parameters is the only PostgreSQL type that holds "
            "every value the picture admits and no more."
        ),
    ),
    MappingRule(
        rule_id="M04",
        cobol_shape="PIC S9(p)V9(s) COMP-3 / PACKED-DECIMAL",
        postgres_type="NUMERIC(p+s, s)",
        basis=Basis.FORCED,
        rationale=(
            "Packed decimal is a storage format, not a different value domain. "
            "The picture states precision and scale exactly as in M03."
        ),
    ),
    MappingRule(
        rule_id="M05",
        cobol_shape="PIC S9(1-4) COMP / BINARY / COMP-5",
        postgres_type="SMALLINT",
        basis=Basis.CHOICE,
        rationale=(
            "A 2-byte binary field maps onto PostgreSQL's native 2-byte "
            "integer, which indexes and arithmetics faster than NUMERIC."
        ),
        alternative=(
            "NUMERIC(p,0), which additionally enforces the picture's digit "
            "limit — SMALLINT accepts values up to 32767 that PIC S9(4) does "
            "not."
        ),
    ),
    MappingRule(
        rule_id="M06",
        cobol_shape="PIC S9(5-9) COMP / BINARY / COMP-5",
        postgres_type="INTEGER",
        basis=Basis.CHOICE,
        rationale="As M05, for the 4-byte binary width.",
        alternative="NUMERIC(p,0), to enforce the picture's digit limit.",
    ),
    MappingRule(
        rule_id="M07",
        cobol_shape="PIC S9(10-18) COMP / BINARY / COMP-5",
        postgres_type="BIGINT",
        basis=Basis.CHOICE,
        rationale="As M05, for the 8-byte binary width.",
        alternative="NUMERIC(p,0), to enforce the picture's digit limit.",
    ),
    MappingRule(
        rule_id="M08",
        cobol_shape="PIC S9(p)V9(s) COMP with a non-zero scale",
        postgres_type="NUMERIC(p+s, s)",
        basis=Basis.FORCED,
        rationale=(
            "An integer type cannot carry an implied decimal position. The "
            "scale is in the picture and must survive."
        ),
    ),
    MappingRule(
        rule_id="M09",
        cobol_shape="Numeric-edited PICTURE (Z, *, comma, CR, DB, floating $)",
        postgres_type="CHAR(n)",
        basis=Basis.CHOICE,
        rationale=(
            "An edited field stores a formatted string — suppressed zeros, "
            "inserted commas, a trailing CR — not a number. CHAR(n) preserves "
            "the bytes that are actually on the record."
        ),
        alternative=(
            "NUMERIC(p,s) with the editing re-applied in the presentation "
            "layer, if the field is only ever produced for display and the "
            "customer wants an arithmetic type in the database."
        ),
    ),
    MappingRule(
        rule_id="M10",
        cobol_shape="88-level condition names on an elementary field",
        postgres_type="CHECK (column IN (…))",
        basis=Basis.CHOICE,
        rationale=(
            "The 88-levels enumerate the values the field is allowed to take. "
            "A CHECK constraint makes the database enforce a domain the COBOL "
            "source could only test."
        ),
        alternative=(
            "Omit the constraint, if records already on the volume may hold "
            "values outside the enumerated set — a load would then fail on "
            "data the mainframe accepted."
        ),
    ),
    MappingRule(
        rule_id="M11",
        cobol_shape="OCCURS n TIMES with n <= "
                    f"{OCCURS_COLUMN_LIMIT}, fixed extent",
        postgres_type="n numbered columns",
        basis=Basis.CHOICE,
        rationale=(
            "A small fixed array flattens to numbered columns, which keeps the "
            "row a faithful image of the record and needs no join."
        ),
        alternative=(
            "A child table keyed to the parent, which normalises the array and "
            "is the better shape if the target application iterates it."
        ),
    ),
    MappingRule(
        rule_id="M12",
        cobol_shape="Group item (a level with subordinate fields)",
        postgres_type="no column",
        basis=Basis.FORCED,
        rationale=(
            "A group has no storage of its own; its bytes are its children's. "
            "Emitting both the group and its children would map the same bytes "
            "twice."
        ),
    ),
    MappingRule(
        rule_id="M13",
        cobol_shape="FILLER",
        postgres_type="no column",
        basis=Basis.CHOICE,
        rationale=(
            "FILLER is unnamed padding with no business meaning, and a target "
            "schema that carries it forward carries forward the mainframe's "
            "record geometry rather than its data."
        ),
        alternative=(
            "Emit filler_<offset> CHAR(n) columns, if the target must "
            "reproduce the original record byte-for-byte on export."
        ),
    ),
)

MAPPING_RULES_BY_ID: Dict[str, MappingRule] = {r.rule_id: r for r in MAPPING_RULES}


def lint_mapping_rules(
    rules: Sequence[MappingRule] = MAPPING_RULES,
) -> List[str]:
    """Return every incomplete rule. Empty list means clean (acceptance ⑥)."""
    problems: List[str] = []
    seen: set = set()
    for rule in rules:
        if rule.rule_id in seen:
            problems.append(f"{rule.rule_id}: duplicate rule id")
        seen.add(rule.rule_id)
        if not isinstance(rule.basis, Basis):
            problems.append(f"{rule.rule_id}: basis is not forced or choice")
        if not rule.rationale.strip():
            problems.append(f"{rule.rule_id}: no rationale")
        if rule.basis is Basis.CHOICE and not (rule.alternative or "").strip():
            problems.append(
                f"{rule.rule_id}: basis is 'choice' but no alternative is "
                f"stated. A choice with no alternative is a decision the "
                f"customer cannot argue with."
            )
        if rule.basis is Basis.FORCED and rule.alternative:
            problems.append(
                f"{rule.rule_id}: basis is 'forced' but an alternative is "
                f"offered. If there is an alternative, it was a choice."
            )
    return problems


def mapping_table() -> Tuple[Dict[str, object], ...]:
    """The published mapping table, for the report and the CI reconciliation."""
    return tuple(rule.to_dict() for rule in MAPPING_RULES)


# --------------------------------------------------------------------------
# Identifiers
# --------------------------------------------------------------------------

_IDENT_CLEAN_RE = re.compile(r"[^a-z0-9_]+")


def sanitize(name: str) -> str:
    """COBOL name → a lower-case PostgreSQL identifier.

    Every identifier is double-quoted where it is emitted, so a name that
    collides with a reserved word is safe without a keyword list to maintain.
    """
    lowered = name.strip().lower().replace("-", "_")
    cleaned = _IDENT_CLEAN_RE.sub("_", lowered).strip("_")
    if not cleaned:
        cleaned = "unnamed"
    if cleaned[0].isdigit():
        cleaned = f"c_{cleaned}"
    return cleaned[:63]


def quote_ident(name: str) -> str:
    return '"' + name.replace('"', '""') + '"'


def quote_literal(value: str) -> str:
    return "'" + value.replace("'", "''") + "'"


# --------------------------------------------------------------------------
# Type derivation
# --------------------------------------------------------------------------

@dataclass(frozen=True)
class Column:
    """One emitted column, and the rule that produced it."""

    name: str
    pg_type: str
    rule_id: str
    cobol_name: str
    offset: int
    length: int
    picture: Optional[str]
    usage: str
    checks: Tuple[str, ...] = ()
    comment: Optional[str] = None

    @property
    def end(self) -> int:
        return self.offset + self.length - 1

    def to_dict(self) -> Dict[str, object]:
        return {
            "name": self.name,
            "pg_type": self.pg_type,
            "rule_id": self.rule_id,
            "cobol_name": self.cobol_name,
            "offset": self.offset,
            "length": self.length,
            "picture": self.picture,
            "usage": self.usage,
            "checks": list(self.checks),
            "comment": self.comment,
        }


def precision_scale(picture: Optional[str]) -> Optional[Tuple[int, int]]:
    """``(precision, scale)`` from a numeric picture, or ``None``.

    ``None`` when the picture cannot be expanded — never a guessed ``(18, 0)``,
    which would be a fabricated precision that silently truncates nothing today
    and everything the day a wider field arrives (R1).
    """
    if not picture:
        return None
    symbols = expand_picture(picture)
    if symbols is None:
        return None
    digits = [s for s in symbols if s == "9"]
    if not digits:
        return None
    if "V" in symbols:
        index = symbols.index("V")
        scale = len([s for s in symbols[index + 1:] if s == "9"])
    else:
        scale = 0
    return len(digits), scale


def _binary_rule(precision: int) -> Tuple[str, str]:
    if precision <= 4:
        return "SMALLINT", "M05"
    if precision <= 9:
        return "INTEGER", "M06"
    return "BIGINT", "M07"


def column_type(field: Field) -> Tuple[Optional[str], Optional[str], Optional[str]]:
    """``(pg_type, rule_id, refusal)`` for one elementary field.

    A field this ruleset does not cover returns ``(None, None, reason)`` and
    makes its table ``PARTIAL``. Emitting a plausible type for an unmapped
    shape is the failure mode R2 exists to prevent.
    """
    usage = field.usage.upper()
    category = field.category

    if usage in UNCOVERED_USAGES:
        return None, None, (
            f"{field.name}: USAGE {usage} is outside the measured scope of the "
            f"layout engine, so neither its length nor its value domain is "
            f"established here"
        )

    if category == "NUMERIC-EDITED" or category == "ALPHANUMERIC-EDITED":
        if field.length is None:
            return None, None, f"{field.name}: edited field with no computed length"
        return f"CHAR({field.length})", "M09", None

    if category == "NUMERIC":
        parsed = precision_scale(field.picture)
        if parsed is None:
            return None, None, (
                f"{field.name}: numeric field whose PICTURE {field.picture!r} "
                f"could not be expanded, so no precision or scale is available"
            )
        precision, scale = parsed
        if usage in BINARY_USAGES:
            if scale:
                return f"NUMERIC({precision},{scale})", "M08", None
            pg_type, rule_id = _binary_rule(precision)
            return pg_type, rule_id, None
        if usage in PACKED_USAGES:
            return f"NUMERIC({precision},{scale})", "M04", None
        if usage == DISPLAY:
            return f"NUMERIC({precision},{scale})", "M03", None
        return None, None, (
            f"{field.name}: USAGE {usage} is not covered by {RULESET_VERSION}"
        )

    if category == "ALPHANUMERIC":
        if field.length is None:
            return None, None, f"{field.name}: no computed length"
        return f"CHAR({field.length})", "M01", None

    if category == "ALPHABETIC":
        if field.length is None:
            return None, None, f"{field.name}: no computed length"
        return f"CHAR({field.length})", "M02", None

    return None, None, (
        f"{field.name}: PICTURE category {category} is not covered by "
        f"{RULESET_VERSION}"
    )


# --------------------------------------------------------------------------
# 88-level CHECK constraints
# --------------------------------------------------------------------------

_VALUE_TOKEN_RE = re.compile(r"'([^']*)'|\"([^\"]*)\"|([A-Za-z0-9+.\-]+)")


def condition_values(condition: Condition) -> Tuple[Tuple[str, ...], Optional[str]]:
    """Literal values from an 88-level, or a reason they were not usable.

    ``VALUE 'A' THRU 'M'`` is a range, not an enumeration; turning it into an
    IN list would enumerate two values where the source admits thirteen. That
    is reported and no constraint is generated.
    """
    raw = condition.values_raw.strip()
    if not raw:
        return (), f"{condition.name}: no VALUE clause text was captured"
    upper = raw.upper()
    if " THRU " in upper or " THROUGH " in upper:
        return (), (
            f"{condition.name}: VALUE uses THRU/THROUGH, which states a range "
            f"rather than an enumeration; no IN constraint is generated"
        )
    for keyword in ("SPACE", "SPACES", "ZERO", "ZEROS", "ZEROES",
                    "HIGH-VALUE", "HIGH-VALUES", "LOW-VALUE", "LOW-VALUES",
                    "QUOTE", "QUOTES", "ALL"):
        if re.search(rf"\b{keyword}\b", upper):
            return (), (
                f"{condition.name}: VALUE uses the figurative constant "
                f"{keyword}, whose byte value depends on the field's picture "
                f"and length; no IN constraint is generated"
            )
    values: List[str] = []
    for match in _VALUE_TOKEN_RE.finditer(raw):
        token = next(g for g in match.groups() if g is not None)
        values.append(token)
    if not values:
        return (), f"{condition.name}: no literal values recovered from {raw!r}"
    return tuple(values), None


def check_constraints(
    field: Field, conditions: Sequence[Condition]
) -> Tuple[Tuple[str, ...], Tuple[str, ...]]:
    """``(checks, notes)`` for one column's 88-levels (rule M10)."""
    checks: List[str] = []
    notes: List[str] = []
    hosted = [c for c in conditions if c.host == field.name]
    if not hosted:
        return (), ()
    values: List[str] = []
    for condition in hosted:
        recovered, reason = condition_values(condition)
        if reason:
            notes.append(reason)
            continue
        values.extend(recovered)
    if not values:
        return (), tuple(notes)
    if any(reason for reason in notes):
        # A partial enumeration would reject values the source allows.
        notes.append(
            f"{field.name}: some 88-levels on this field could not be "
            f"enumerated, so no CHECK constraint is generated from the rest — "
            f"a partial enumeration would reject values the source admits"
        )
        return (), tuple(notes)
    ordered = sorted(dict.fromkeys(values))
    rendered = ", ".join(quote_literal(v) for v in ordered)
    checks.append(f"CHECK ({quote_ident(sanitize(field.name))} IN ({rendered}))")
    return tuple(checks), tuple(notes)


# --------------------------------------------------------------------------
# Tables (D32)
# --------------------------------------------------------------------------

class TableStatus(str, Enum):
    """What the generator is willing to claim about a table."""

    COMPLETE = "COMPLETE"
    PARTIAL = "PARTIAL"


@dataclass(frozen=True)
class Overlap:
    """Bytes that carry two interpretations. Named, never resolved."""

    start_byte: int
    length: int
    primary_fields: Tuple[str, ...]
    redefining_fields: Tuple[str, ...]
    redefines_name: str
    redefines_target: str

    def describe(self) -> str:
        return (
            f"Bytes {self.start_byte}-{self.start_byte + self.length - 1} "
            f"({self.length} bytes) carry two interpretations. "
            f"{self.redefines_name} REDEFINES {self.redefines_target}. "
            f"Primary: {', '.join(self.primary_fields) or '(none)'}. "
            f"Redefining: {', '.join(self.redefining_fields) or '(none)'}. "
            f"Only the primary definition is emitted as columns; the "
            f"redefining view is emitted as commented DDL. Nothing in the "
            f"source says which reading applies to which record, so this table "
            f"is PARTIAL."
        )

    def to_dict(self) -> Dict[str, object]:
        return {
            "start_byte": self.start_byte,
            "length": self.length,
            "primary_fields": list(self.primary_fields),
            "redefining_fields": list(self.redefining_fields),
            "redefines_name": self.redefines_name,
            "redefines_target": self.redefines_target,
            "description": self.describe(),
        }


@dataclass(frozen=True)
class Table:
    """One generated table."""

    name: str
    record: str
    status: TableStatus
    columns: Tuple[Column, ...] = ()
    overlaps: Tuple[Overlap, ...] = ()
    partial_reasons: Tuple[str, ...] = ()
    commented_ddl: Tuple[str, ...] = ()
    notes: Tuple[str, ...] = ()
    source_origin: Optional[str] = None
    record_length: Optional[int] = None

    def column_by_name(self, name: str) -> Optional[Column]:
        for column in self.columns:
            if column.name == name:
                return column
        return None

    def overlapping_column_pairs(self) -> Tuple[Tuple[str, str], ...]:
        """Emitted columns whose byte ranges intersect.

        Must be empty for every ``COMPLETE`` table — that is acceptance ⑦, and
        it is asserted rather than argued.
        """
        pairs: List[Tuple[str, str]] = []
        ordered = sorted(self.columns, key=lambda c: (c.offset, c.end))
        for i, left in enumerate(ordered):
            for right in ordered[i + 1:]:
                if right.offset > left.end:
                    break
                pairs.append((left.name, right.name))
        return tuple(pairs)

    def to_dict(self) -> Dict[str, object]:
        return {
            "name": self.name,
            "record": self.record,
            "status": self.status.value,
            "ruleset": RULESET_VERSION,
            "record_length": self.record_length,
            "columns": [c.to_dict() for c in self.columns],
            "overlaps": [o.to_dict() for o in self.overlaps],
            "partial_reasons": list(self.partial_reasons),
            "commented_ddl": list(self.commented_ddl),
            "notes": list(self.notes),
            "source_origin": self.source_origin,
        }


def _unique(name: str, taken: Dict[str, int]) -> str:
    """Disambiguate a sanitized name that collides with one already emitted."""
    if name not in taken:
        taken[name] = 1
        return name
    taken[name] += 1
    return f"{name}_{taken[name]}"


def _occurs_suffix(field: Field) -> str:
    return "_".join(str(s) for s in field.subscripts)


def generate_table(
    layout: Layout, *, table_name: Optional[str] = None
) -> Table:
    """Generate one PostgreSQL table from one record layout."""
    columns: List[Column] = []
    partial_reasons: List[str] = []
    notes: List[str] = []
    commented: List[str] = []
    overlaps: List[Overlap] = []
    taken: Dict[str, int] = {}

    if layout.status is LayoutStatus.NONE:
        return Table(
            name=table_name or sanitize(layout.group),
            record=layout.group,
            status=TableStatus.PARTIAL,
            partial_reasons=tuple(
                [f"no layout was computed for {layout.group}"]
                + list(layout.reasons)
            ),
            source_origin=layout.origin,
        )
    if layout.status is LayoutStatus.PARTIAL:
        partial_reasons.extend(layout.reasons)

    # -- REDEFINES: primary emitted, redefining view commented (D32) --------
    by_name = {f.name: f for f in layout.fields}
    for field in layout.fields:
        if not field.redefines:
            continue
        target = by_name.get(field.redefines)
        start = field.offset if field.offset is not None else (
            target.offset if target else None
        )
        length = field.length if field.length is not None else (
            target.length if target else None
        )
        if start is None or length is None:
            partial_reasons.append(
                f"{field.name} REDEFINES {field.redefines}, and the overlap "
                f"could not be located in bytes"
            )
            continue
        end = start + length - 1
        primary = tuple(
            f.name for f in layout.fields
            if f.elementary and f.in_tiling and f.offset is not None
            and f.length is not None
            and f.offset <= end and (f.offset + f.length - 1) >= start
        )
        redefining = tuple(
            f.name for f in layout.fields
            if f.elementary and not f.in_tiling and f.offset is not None
            and f.length is not None
            and f.offset >= start and (f.offset + f.length - 1) <= end
        )
        overlap = Overlap(
            start_byte=start,
            length=length,
            primary_fields=primary,
            redefining_fields=redefining,
            redefines_name=field.name,
            redefines_target=field.redefines,
        )
        overlaps.append(overlap)
        partial_reasons.append(overlap.describe())
        commented.append(_commented_redefines(overlap, layout))

    # -- ODO: always PARTIAL, the extent is a runtime property --------------
    odo_fields = [f for f in layout.fields if f.odo_object]
    if layout.odo_object or odo_fields:
        subject = layout.odo_object or odo_fields[0].odo_object
        partial_reasons.append(
            f"OCCURS DEPENDING ON {subject}: the number of occurrences is a "
            f"runtime property carried in the record itself, so the column "
            f"count of a flattened table is not determined by the source. "
            f"Columns are emitted for the occurrences the layout was computed "
            f"at (odo_value={layout.odo_value}); a child table keyed to the "
            f"parent is the mapping that does not depend on that value."
        )

    # -- Columns ------------------------------------------------------------
    for field in layout.fields:
        if not field.elementary or not field.in_tiling:
            continue
        if field.offset is None or field.length is None:
            partial_reasons.append(
                f"{field.name}: no byte position was computed, so no column is "
                f"emitted for it"
            )
            continue
        if field.occurs and field.occurs > OCCURS_COLUMN_LIMIT:
            if not any("OCCURS " + str(field.occurs) in r for r in partial_reasons):
                partial_reasons.append(
                    f"{field.name} OCCURS {field.occurs} TIMES, above the "
                    f"published limit of {OCCURS_COLUMN_LIMIT} numbered "
                    f"columns (rule M11). Columns are emitted, and a child "
                    f"table keyed to the parent is the alternative mapping."
                )
        pg_type, rule_id, refusal = column_type(field)
        if pg_type is None or rule_id is None:
            partial_reasons.append(
                refusal or f"{field.name}: no mapping rule applies"
            )
            continue
        base = sanitize(field.name)
        if field.subscripts:
            base = f"{base}_{_occurs_suffix(field)}"
        name = _unique(base, taken)
        checks, check_notes = check_constraints(field, layout.conditions)
        notes.extend(check_notes)
        columns.append(Column(
            name=name,
            pg_type=pg_type,
            rule_id=rule_id,
            cobol_name=field.name,
            offset=field.offset,
            length=field.length,
            picture=field.picture,
            usage=field.usage,
            checks=checks,
            comment=(
                f"{field.name} PIC {field.picture} USAGE {field.usage} "
                f"@{field.offset}:{field.length}"
                if field.picture else
                f"{field.name} USAGE {field.usage} @{field.offset}:{field.length}"
            ),
        ))

    status = (
        TableStatus.PARTIAL if partial_reasons else TableStatus.COMPLETE
    )
    return Table(
        name=table_name or sanitize(layout.group),
        record=layout.group,
        status=status,
        columns=tuple(columns),
        overlaps=tuple(overlaps),
        partial_reasons=tuple(partial_reasons),
        commented_ddl=tuple(commented),
        notes=tuple(notes),
        source_origin=layout.origin,
        record_length=layout.group_length,
    )


def _commented_redefines(overlap: Overlap, layout: Layout) -> str:
    """The redefining view, as DDL that is commented out.

    Commented rather than omitted because the alternative reading is real
    information a migration team needs; commented rather than executed because
    the two readings are not independent columns and a row cannot hold both.
    """
    lines = [
        f"-- REDEFINES view: {overlap.redefines_name} REDEFINES "
        f"{overlap.redefines_target}",
        f"-- {overlap.describe()}",
        f"-- CREATE VIEW {quote_ident(sanitize(overlap.redefines_name))} AS SELECT",
    ]
    by_name = {f.name: f for f in layout.fields}
    for name in overlap.redefining_fields:
        field = by_name.get(name)
        if field is None:
            continue
        pg_type, _rule_id, refusal = column_type(field)
        rendered = pg_type or f"/* unmapped: {refusal} */"
        lines.append(
            f"--   substr(record, {field.offset}, {field.length})::{rendered} "
            f"AS {quote_ident(sanitize(name))},"
        )
    lines.append("-- FROM <the source record>;")
    return "\n".join(lines)


# --------------------------------------------------------------------------
# Rendering
# --------------------------------------------------------------------------

def render_table(table: Table, *, schema: Optional[str] = None) -> str:
    """Render one table as executable PostgreSQL DDL.

    The header names the ruleset version and the table's status, so a schema
    that reaches a DBA carries its own provenance and its own caveats.
    """
    qualified = (
        f"{quote_ident(schema)}.{quote_ident(table.name)}"
        if schema else quote_ident(table.name)
    )
    lines = [
        f"-- Relian generated DDL — ruleset {RULESET_VERSION}",
        f"-- Record: {table.record}"
        + (f" (computed length {table.record_length} bytes)"
           if table.record_length is not None else ""),
        f"-- Status: {table.status.value}",
    ]
    if table.status is TableStatus.PARTIAL:
        lines.append(
            "-- PARTIAL: this table does not represent the whole record. "
            "Reasons follow."
        )
        for reason in table.partial_reasons:
            for chunk in _wrap(reason):
                lines.append(f"--   {chunk}")
    for note in table.notes:
        for chunk in _wrap(note):
            lines.append(f"--   note: {chunk}")

    lines.append(f"CREATE TABLE {qualified} (")
    body: List[str] = []
    for column in table.columns:
        body.append(
            f"    {quote_ident(column.name)} {column.pg_type}"
            f"    -- {column.comment} [{column.rule_id}]"
        )
    constraints = [c for column in table.columns for c in column.checks]
    rendered_body = []
    for index, line in enumerate(body):
        code, _, comment = line.partition("    -- ")
        separator = "," if (index < len(body) - 1 or constraints) else ""
        rendered_body.append(f"{code}{separator}  -- {comment}")
    lines.extend(rendered_body)
    for index, constraint in enumerate(constraints):
        separator = "," if index < len(constraints) - 1 else ""
        lines.append(f"    {constraint}{separator}")
    lines.append(");")
    if table.commented_ddl:
        lines.append("")
        lines.extend(table.commented_ddl)
    return "\n".join(lines) + "\n"


def _wrap(text: str, width: int = 72) -> List[str]:
    words = text.split()
    out: List[str] = []
    current: List[str] = []
    for word in words:
        if current and len(" ".join(current + [word])) > width:
            out.append(" ".join(current))
            current = [word]
        else:
            current.append(word)
    if current:
        out.append(" ".join(current))
    return out or [""]


def render_schema(
    tables: Sequence[Table], *, schema: Optional[str] = None
) -> str:
    """Render a whole schema: the mapping table as a header, then the tables."""
    lines = [
        f"-- Relian target-schema DDL — ruleset {RULESET_VERSION}",
        "--",
        "-- Every mapping rule below carries a basis of 'forced' or 'choice'.",
        "-- A 'choice' states its rationale and its alternative, so it can be",
        "-- disagreed with and overridden.",
        "--",
        "-- No customer dataset was opened to produce this schema. Every type",
        "-- here comes from copybook, COBOL and JCL source text (R12).",
        "--",
    ]
    for rule in MAPPING_RULES:
        lines.append(f"-- [{rule.rule_id}] {rule.cobol_shape}")
        lines.append(f"--        -> {rule.postgres_type}  ({rule.basis.value})")
        for chunk in _wrap(rule.rationale, 66):
            lines.append(f"--        {chunk}")
        if rule.alternative:
            for chunk in _wrap(f"alternative: {rule.alternative}", 66):
                lines.append(f"--        {chunk}")
        lines.append("--")
    if schema:
        lines.append(f"CREATE SCHEMA IF NOT EXISTS {quote_ident(schema)};")
    lines.append("")
    for table in tables:
        lines.append(render_table(table, schema=schema))
    return "\n".join(lines)


# --------------------------------------------------------------------------
# Lints (acceptance ⑥ and ⑦)
# --------------------------------------------------------------------------

def lint_table(table: Table) -> List[str]:
    """Return every violation in ``table``. Empty list means clean."""
    problems: List[str] = []
    for column in table.columns:
        if column.rule_id not in MAPPING_RULES_BY_ID:
            problems.append(
                f"{table.name}.{column.name}: cites rule {column.rule_id}, "
                f"which is not in the published mapping table"
            )
    overlapping = table.overlapping_column_pairs()
    if overlapping and table.status is TableStatus.COMPLETE:
        problems.append(
            f"{table.name}: status is COMPLETE but emits overlapping columns "
            f"{overlapping}. Same bytes under two interpretations is exactly "
            f"the case that must be PARTIAL (D32)."
        )
    if table.status is TableStatus.PARTIAL and not table.partial_reasons:
        problems.append(
            f"{table.name}: status is PARTIAL with no reason given. A caveat "
            f"nobody can read is not a caveat."
        )
    if table.overlaps and table.status is not TableStatus.PARTIAL:
        problems.append(
            f"{table.name}: has {len(table.overlaps)} REDEFINES overlap(s) but "
            f"is not marked PARTIAL"
        )
    return problems


def lint_schema(tables: Sequence[Table]) -> List[str]:
    problems = lint_mapping_rules()
    for table in tables:
        problems.extend(lint_table(table))
    return problems
