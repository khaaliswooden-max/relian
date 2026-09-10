"""WP-2.7 D36 — rendering a projection so it cannot be read as a measurement.

Two lints live here, and they guard two different failure modes.

**Acceptance (6) — no unlabelled column.** A projected offset and a measured
offset are both integers, and once they are side by side in a table nothing
about the digits says which is which. So the *structure* is linted, not the
prose: every quantity carries a ``basis`` of ``measured`` or ``projected`` in
the serialised form, and every rendered column header that names a quantity
carries the basis in its title. A column called plain ``Offset`` in a table
that holds both is the defect, and :func:`lint_projection_render` fails on it.

**Acceptance (7) — no measurement vocabulary in a projected context.** The
words *verified*, *measured* and *confirmed* are how this product describes
things it actually observed. Inside a projected block they may appear **only**
as an explicit basis label -- the literal ``(measured)`` -- or as an explicit
negation of measurement (``unmeasured``, ``not measured``, ``no measurement``).
A bare "the measured offset", "verified layout" or "confirmed width" in
projected prose is a violation even when the sentence is technically true,
because the reader takes the adjective from the nearest noun and the nearest
noun is a projection. Each term carries its own negative control in
``tests/test_dialect_projection_labels.py``.

This is deliberately stricter than English requires. The alternative -- a lint
that tries to work out which noun an adjective attaches to -- is not a lint,
it is a parser with opinions, and it would pass the sentence that costs a
customer a byte.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from enum import Enum
from typing import Dict, List, Sequence, Tuple

from .classify import Classification, SensitivityReport

#: The vocabulary this product reserves for things it observed. Inflections are
#: included: a lint that catches "measured" and misses "measurement" is a lint
#: that teaches authors which synonym slips through.
MEASUREMENT_VOCABULARY: Tuple[str, ...] = (
    "verified",
    "verify",
    "verifies",
    "measured",
    "measures",
    "measurement",
    "confirmed",
    "confirm",
    "confirms",
    "confirmation",
)

#: The only forms in which the vocabulary above may appear inside a projected
#: block. ``(measured)`` is the basis label acceptance (6) requires, so banning
#: it outright would make the two acceptance criteria contradict each other.
#: The rest are explicit statements that something was NOT measured, which is
#: the one claim a projection is always allowed to make about itself.
ALLOWED_FORMS: Tuple[str, ...] = (
    "(measured)",
    "(projected)",
    "unmeasured",
    "not measured",
    "not a measurement",
    "no measurement",
    "no measurement was taken",
    "cannot be measured",
    "never measured",
)

#: Quantity words that must never head a column without a basis.
_QUANTITY_WORDS: Tuple[str, ...] = ("offset", "length", "width", "size", "bytes")

_BASIS_WORDS: Tuple[str, ...] = ("measured", "projected")

#: Inline code spans, excluded from the vocabulary scan (see
#: :func:`lint_projected_text`).
_CODE_SPAN = re.compile(r"`[^`]*`")


class BlockKind(str, Enum):
    MEASURED = "measured"
    PROJECTED = "projected"


@dataclass(frozen=True)
class Block:
    """One rendered chunk, carrying the basis of what is inside it.

    The basis is a property of the block rather than something a reader infers
    from position, which is what makes :func:`lint_projection_render` able to
    check it at all.
    """

    kind: BlockKind
    text: str


def _allowed_spans(lowered: str) -> List[Tuple[int, int]]:
    spans: List[Tuple[int, int]] = []
    for form in ALLOWED_FORMS:
        start = 0
        while True:
            at = lowered.find(form, start)
            if at < 0:
                break
            spans.append((at, at + len(form)))
            start = at + 1
    return spans


def lint_projected_text(text: str) -> List[Tuple[str, str]]:
    """``[(term, excerpt), …]`` for measurement vocabulary in projected prose.

    Acceptance (7). Word-boundary matched so ``unmeasured`` is not reported as
    a bare ``measured``, and every hit is returned with an excerpt so the
    failure names the sentence to fix.
    """
    found: List[Tuple[str, str]] = []
    # Backticked spans are identifiers -- file names, flags, rule keys -- not
    # prose making a claim about a value. `discovery-verify/IBMLAYOUT.cbl`
    # contains "verify" and says nothing about any number's basis. They are
    # blanked rather than removed so reported excerpt offsets still line up
    # with the caller's own text.
    scannable = _CODE_SPAN.sub(lambda m: " " * len(m.group(0)), text)
    lowered = scannable.lower()
    allowed = _allowed_spans(lowered)
    for term in MEASUREMENT_VOCABULARY:
        for m in re.finditer(rf"\b{re.escape(term)}\b", lowered):
            at, end = m.span()
            if any(a <= at and end <= b for a, b in allowed):
                continue
            found.append((term, text[max(0, at - 70): end + 70]))
    return found


def lint_projection_render(blocks: Sequence[Block]) -> List[str]:
    """Acceptance (6) and (7) over a rendered artifact.

    Checks, in order:

    1. Every projected block says so in words a reader will see -- the literal
       token ``PROJECTION`` -- so the label does not depend on the caller
       having styled the section correctly.
    2. No table column that names a quantity is missing its basis. This is the
       "same column" rule: the header is where a reader learns which side a
       number came from.
    3. No measurement vocabulary in a projected block outside
       :data:`ALLOWED_FORMS`.
    """
    problems: List[str] = []
    for i, block in enumerate(blocks):
        if block.kind is not BlockKind.PROJECTED:
            continue
        if "PROJECTION" not in block.text:
            problems.append(
                f"block {i} is projected but never says PROJECTION. A "
                f"projection that does not announce itself is a measurement to "
                f"the reader (D36)."
            )
        for line in block.text.splitlines():
            if not line.lstrip().startswith("|"):
                continue
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if all(set(c) <= set("-: ") for c in cells if c):
                continue  # markdown rule row
            for cell in cells:
                low = cell.lower()
                if any(q in low for q in _QUANTITY_WORDS) and not any(
                    b in low for b in _BASIS_WORDS
                ):
                    problems.append(
                        f"block {i} has a column header {cell!r} that names a "
                        f"quantity without a basis. A projected value and a "
                        f"measured one must never share an unlabelled column "
                        f"(acceptance (6))."
                    )
            break  # header is the first table row in the block
        for term, excerpt in lint_projected_text(block.text):
            problems.append(
                f"block {i} describes a projection with the measurement word "
                f"{term!r}: …{excerpt.strip()}… "
                f"(acceptance (7))"
            )
    return problems


def render_sensitivity_blocks(report: SensitivityReport) -> List[Block]:
    """Render the sensitivity section as labelled blocks.

    The measured layout stays the deliverable: it is rendered first, in its own
    block, on its own basis. The projection follows as an annotation on it --
    which is the D37 ordering, and the reason the escalation trigger about
    "shipping a projected layout as the primary deliverable" cannot be tripped
    by this renderer.
    """
    counts = report.counts()
    total = len(report.elementary)
    n_shift = sum(1 for f in report.elementary if f.shifted)

    measured = [
        f"### Record layout — {report.group} (basis: measured)",
        "",
        f"Compiler basis: {report.measured_profile.label}.",
        f"Verified against {report.measured_profile.basis}.",
        f"Record length: {report.measured_length} bytes.",
    ]

    projected: List[str] = [
        f"### Dialect sensitivity — {report.group} "
        f"(PROJECTION under {report.projected_profile.id})",
        "",
        report.headline(),
        "",
        (
            f"The {report.projected_profile.label} column is a **PROJECTION**: "
            f"the layout implied by applying a sourced rule table to the same "
            f"parse. Relian has no IBM system. Equivalence is UNMEASURED until "
            f"a run of `discovery-verify/IBMLAYOUT.cbl` on your own system is "
            f"returned, at which point the result describes *your* compiler at "
            f"*your* settings and is recorded that way — never generalised to "
            f"\"IBM\"."
        ),
        "",
        "| Field | Class | Offset (measured) | Length (measured) "
        "| Offset (projected) | Length (projected) | Shift | Why |",
        "|---|---|---|---|---|---|---|---|",
    ]
    for f in report.elementary:
        # `None` is UNDETERMINED, not zero. A truthiness test collapses both
        # to "0" and prints an UNKNOWN field -- one with no comparable address
        # on either side -- as though it had been checked and found unshifted.
        if f.offset_delta is None:
            shift = "—"
        elif f.offset_delta == 0:
            shift = "0"
        else:
            shift = f"{f.offset_delta:+d}"
        projected.append(
            f"| {f.name} | {f.classification.value} "
            f"| {f.measured_offset if f.measured_offset is not None else '—'} "
            f"| {f.measured_length if f.measured_length is not None else '—'} "
            f"| {f.projected_offset if f.projected_offset is not None else '—'} "
            f"| {f.projected_length if f.projected_length is not None else '—'} "
            f"| {shift} | {f.reason or ''} |"
        )

    projected += [
        "",
        f"**Counts.** INVARIANT {counts[Classification.INVARIANT.value]}, "
        f"DIALECT_SENSITIVE {counts[Classification.DIALECT_SENSITIVE.value]}, "
        f"UNKNOWN {counts[Classification.UNKNOWN.value]}, of {total} fields. "
        f"{n_shift} field(s) change address.",
        "",
        f"**Rules applied for the PROJECTION.**",
    ]
    for name, rule in sorted(report.projected_profile.rules.items()):
        cite = rule.citation.render() if rule.citation else "—"
        projected.append(f"- `{name}` — {rule.summary}. {cite}")

    return [
        Block(BlockKind.MEASURED, "\n".join(measured)),
        Block(BlockKind.PROJECTED, "\n".join(projected)),
    ]


def render_sensitivity_markdown(report: SensitivityReport) -> str:
    return "\n\n".join(b.text for b in render_sensitivity_blocks(report))
