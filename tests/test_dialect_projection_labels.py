"""WP-2.7 acceptance (6) and (7) — a projection cannot be dressed as a measurement.

D36 names this the fabrication mode this repository has deleted four times, and
says a projected offset presented as measured would be the most expensive
instance yet, because a customer would load data with it. Two independent
guards, because there are two independent ways to do it:

**(6) The unlabelled column.** A projected offset and a measured offset are
both integers. Put them side by side and nothing about the digits says which is
which, so the header has to. Every column that names a quantity carries its
basis, and the negative control below strips one to prove the lint bites.

**(7) The borrowed adjective.** Even correctly separated columns can be
narrated with the vocabulary of measurement. Inside a projected block the words
*verified*, *measured* and *confirmed* (and their inflections) may appear ONLY
as an explicit basis label -- the literal ``(measured)`` -- or as an explicit
negation of measurement. Each term gets its own negative control, per
acceptance (7).

The rule is deliberately stricter than English requires. A lint that tried to
work out which noun an adjective attaches to would be a parser with opinions,
and it would pass the sentence that costs a customer a byte.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.discovery.dialects.classify import analyse_path                # noqa: E402
from src.discovery.dialects.gnucobol_3_1_2 import PROFILE as GNUCOBOL   # noqa: E402
from src.discovery.dialects.ibm_enterprise_cobol import PROFILE as IBM  # noqa: E402
from src.discovery.dialects.render import (                             # noqa: E402
    ALLOWED_FORMS,
    Block,
    BlockKind,
    MEASUREMENT_VOCABULARY,
    lint_projected_text,
    lint_projection_render,
    render_sensitivity_blocks,
    render_sensitivity_markdown,
)

CORPUS = REPO_ROOT / "discovery-bench" / "corpus"
FIXTURES = REPO_ROOT / "tests" / "fixtures" / "dialects"

ALL_COPYBOOKS = sorted(p.name for p in CORPUS.glob("*.cpy")) + sorted(
    p.name for p in FIXTURES.glob("*.cpy")
)


def _blocks(name: str):
    path = (CORPUS / name) if (CORPUS / name).is_file() else (FIXTURES / name)
    report = analyse_path(path, GNUCOBOL, IBM)
    assert report is not None
    return render_sensitivity_blocks(report)


# --------------------------------------------------------------------------
# The real artifact is clean
# --------------------------------------------------------------------------

@pytest.mark.parametrize("name", ALL_COPYBOOKS)
def test_the_rendered_sensitivity_section_passes_both_lints(name: str) -> None:
    assert lint_projection_render(_blocks(name)) == []


@pytest.mark.parametrize("name", ALL_COPYBOOKS)
def test_the_projected_block_announces_itself(name: str) -> None:
    projected = [b for b in _blocks(name) if b.kind is BlockKind.PROJECTED]
    assert projected, "nothing was rendered as a projection"
    for b in projected:
        assert "PROJECTION" in b.text
        assert "UNMEASURED" in b.text


def test_the_measured_layout_is_rendered_first_and_on_its_own_basis() -> None:
    """D37 and the escalation trigger: the measured layout is the deliverable
    and the projection is an annotation on it, so the ordering is asserted."""
    blocks = _blocks("D02_binary.cpy")
    assert [b.kind for b in blocks] == [BlockKind.MEASURED, BlockKind.PROJECTED]
    assert "basis: measured" in blocks[0].text
    assert "PROJECTION" not in blocks[0].text


# --------------------------------------------------------------------------
# Acceptance (6) — planted red: strip a column's basis
# --------------------------------------------------------------------------

@pytest.mark.parametrize(
    "header, note",
    [
        ("| Field | Offset | Length (projected) |", "measured side unlabelled"),
        ("| Field | Offset (measured) | Length |", "projected side unlabelled"),
        ("| Field | Offset | Length |", "neither side labelled"),
        ("| Field | Width | Class |", "a quantity by another name"),
    ],
)
def test_a_quantity_column_without_a_basis_fails_the_lint(header: str, note: str) -> None:
    """Acceptance (6)'s planted red."""
    block = Block(
        BlockKind.PROJECTED,
        "### Dialect sensitivity (PROJECTION under ibm-enterprise-cobol)\n"
        "Equivalence is UNMEASURED.\n"
        f"{header}\n|---|---|---|\n| WS-CTR | 1 | 2 |\n",
    )
    problems = lint_projection_render([block])
    assert any("without a basis" in p for p in problems), (note, problems)


def test_a_fully_labelled_table_passes() -> None:
    block = Block(
        BlockKind.PROJECTED,
        "### Dialect sensitivity (PROJECTION under ibm-enterprise-cobol)\n"
        "Equivalence is UNMEASURED.\n"
        "| Field | Offset (measured) | Offset (projected) |\n"
        "|---|---|---|\n| WS-CTR | 1 | 1 |\n",
    )
    assert lint_projection_render([block]) == []


def test_the_serialised_form_labels_every_quantity_with_its_basis() -> None:
    """(6) again, structurally. The JSON is what a downstream tool reads, and a
    tool cannot see a column header."""
    report = analyse_path(CORPUS / "D02_binary.cpy", GNUCOBOL, IBM)
    payload = report.to_dict()
    for row in payload["fields"]:
        assert row["measured"]["basis"] == "measured"
        assert row["projected"]["basis"] == "projected"
    assert payload["measured_profile"]["kind"] == "measured"
    assert payload["projected_profile"]["kind"] == "projected"
    assert set(payload) >= {
        "measured_record_length", "projected_record_length",
        "record_length_delta",
    }, "a record length that does not name its basis in the key"


# --------------------------------------------------------------------------
# Acceptance (7) — each term its own negative control
# --------------------------------------------------------------------------

@pytest.mark.parametrize("term", ["verified", "measured", "confirmed"])
def test_each_measurement_term_is_caught_in_a_projected_context(term: str) -> None:
    """Acceptance (7). Three terms, three controls — a lint exercised on one
    term is a lint with two untested branches."""
    block = Block(
        BlockKind.PROJECTED,
        "### Dialect sensitivity (PROJECTION under ibm-enterprise-cobol)\n"
        "Equivalence is UNMEASURED.\n"
        f"The IBM record length is {term} at 40 bytes.\n",
    )
    problems = lint_projection_render([block])
    assert any(term in p for p in problems), problems


@pytest.mark.parametrize("term", MEASUREMENT_VOCABULARY)
def test_every_inflection_in_the_vocabulary_is_caught(term: str) -> None:
    """A lint that catches "measured" and misses "measurement" teaches authors
    which synonym slips through."""
    found = lint_projected_text(f"The IBM offset is {term} against the manual.")
    assert [t for t, _ in found] == [term], found


@pytest.mark.parametrize("form", ALLOWED_FORMS)
def test_the_allowed_label_and_negation_forms_are_permitted(form: str) -> None:
    """(6) requires the literal ``(measured)`` label, so (7) must not ban it —
    otherwise the two acceptance criteria contradict each other."""
    assert lint_projected_text(f"Record length 38 {form} -> 40 (projected).") == []


def test_a_measurement_word_is_not_excused_by_a_nearby_allowed_form() -> None:
    """The allowed form covers its own span and nothing else."""
    found = lint_projected_text(
        "Record length 38 (measured) -> 40 (projected); the 40 is verified."
    )
    assert [t for t, _ in found] == ["verified"], found


def test_unmeasured_is_not_reported_as_a_bare_measured() -> None:
    """Word-boundary matching, asserted directly: the disclaimer this product
    is required to print must not itself trip the lint."""
    assert lint_projected_text("Equivalence with IBM is UNMEASURED.") == []


def test_a_backticked_identifier_is_not_prose() -> None:
    """``discovery-verify/IBMLAYOUT.cbl`` contains "verify" and says nothing
    about any number's basis. Paths and flags are identifiers."""
    assert lint_projected_text("Run `discovery-verify/IBMLAYOUT.cbl` now.") == []
    assert lint_projected_text("Pass `--dialect` to verify nothing.") != [], (
        "prose outside the code span must still be scanned"
    )


def test_the_vocabulary_is_permitted_in_a_measured_block() -> None:
    """The lint is about *projected* context. The measured layout is entitled
    to say it was measured — that is the whole point of the distinction."""
    block = Block(
        BlockKind.MEASURED,
        "### Record layout (basis: measured)\n"
        "Verified byte-for-byte against GnuCOBOL 3.1.2.0, 186 of 186.\n",
    )
    assert lint_projection_render([block]) == []


@pytest.mark.parametrize("name", ALL_COPYBOOKS)
def test_the_markdown_rendering_never_puts_a_bare_quantity_header_on_a_page(
    name: str,
) -> None:
    """End to end over every copybook: the artifact a reader actually sees."""
    path = (CORPUS / name) if (CORPUS / name).is_file() else (FIXTURES / name)
    report = analyse_path(path, GNUCOBOL, IBM)
    assert report is not None
    text = render_sensitivity_markdown(report)
    assert "Offset (measured)" in text and "Offset (projected)" in text
    assert "| Offset |" not in text
    assert "| Length |" not in text


# --------------------------------------------------------------------------
# Acceptance (6) at the Layout level — the marker travels with the object
# --------------------------------------------------------------------------

def test_a_measured_layout_names_the_compiler_it_was_verified_against() -> None:
    from src.discovery.layout import COMPILER_BASIS, compute_text

    document = compute_text(
        (CORPUS / "D02_binary.cpy").read_text(encoding="utf-8")
    )[0].to_dict()
    assert document["basis"] == "measured"
    assert document["verified_against"] == COMPILER_BASIS
    assert document["projection"] is None
    assert document["projected_under"] is None


def test_a_projected_layout_claims_no_compiler_and_says_so_first() -> None:
    """The hole this closes: ``Layout.to_dict()`` publishes
    ``verified_against``. A caller that serialises a projected Layout directly
    -- bypassing the sensitivity report entirely -- would otherwise emit
    projected offsets labelled as verified against GnuCOBOL. So the marker has
    to live on the object, not on the renderer.
    """
    from src.discovery.layout import compute_text
    from src.discovery.dialects import IBM_ENTERPRISE_COBOL

    document = compute_text(
        (CORPUS / "D02_binary.cpy").read_text(encoding="utf-8"),
        profile=IBM_ENTERPRISE_COBOL,
    )[0].to_dict()

    assert document["basis"] == "projected"
    assert document["verified_against"] is None, (
        "a projected layout was verified against nothing; naming a compiler "
        "here is the untrue claim acceptance (6) forbids"
    )
    assert document["benchmark"] is None
    assert document["projected_under"] == IBM_ENTERPRISE_COBOL.label

    # The projection disclaimer comes FIRST, ahead of the IBM-equivalence
    # limitation, because it changes what every number below is.
    assert document["limitations"][0].startswith("PROJECTION, NOT A MEASUREMENT")
    assert "Relian has no IBM system" in document["limitations"][0]


def test_the_measured_profile_does_not_mark_the_layout_a_projection() -> None:
    """Otherwise "the measured layout" would depend on how the caller asked."""
    from src.discovery.layout import compute_text
    from src.discovery.dialects import GNUCOBOL_3_1_2

    text = (CORPUS / "D10_sync.cpy").read_text(encoding="utf-8")
    default = compute_text(text)[0].to_dict()
    profiled = compute_text(text, profile=GNUCOBOL_3_1_2)[0].to_dict()
    assert profiled == default
    assert profiled["projection"] is None


# --------------------------------------------------------------------------
# Bugbot finding (Medium): a projected layout still asserted verification
# --------------------------------------------------------------------------

def test_a_projected_layout_carries_no_measured_claim_anywhere() -> None:
    """Nulling ``verified_against`` was not enough.

    ``limitations()`` still appended ``IBM_EQUIVALENCE_LIMITATION`` — which
    quotes "Verified byte-for-byte against GnuCOBOL 3.1.2.0 … 186 of 186
    comparisons at tolerance zero" — and ``summary()`` put that same text in
    EVERY number's provenance. So the document contradicted its own
    disclaimer one line further down, and each projected number carried a
    measurement claim in the field designed to hold its provenance.
    """
    from src.discovery.layout import IBM_EQUIVALENCE_LIMITATION, compute_text
    from src.discovery.dialects import IBM_ENTERPRISE_COBOL

    document = compute_text(
        (CORPUS / "D02_binary.cpy").read_text(encoding="utf-8"),
        profile=IBM_ENTERPRISE_COBOL,
    )[0].to_dict()

    limitations = document["limitations"]
    assert IBM_EQUIVALENCE_LIMITATION not in limitations
    assert limitations[0].startswith("PROJECTION, NOT A MEASUREMENT")

    provenances = [
        m["provenance"] for m in document["summary"].values() if m is not None
    ]
    assert provenances, "no summary numbers to check"
    blob = " ".join(limitations) + " " + " ".join(provenances)
    for claim in (
        "Verified byte-for-byte", "186 of 186", "at tolerance zero",
    ):
        assert claim not in blob, (
            f"a projected layout still asserts {claim!r}; the numbers were "
            f"never measured on any compiler"
        )
    for required in ("PROJECTION", "NOT MEASURED"):
        assert required in blob


def test_the_projected_limitations_pass_the_measurement_vocabulary_lint() -> None:
    """The same discipline (7) applies to the Layout's own prose.

    Every use of a measurement word in a projected layout's limitations and
    provenance must be an explicit denial or the ``(measured)`` label — never
    a bare adjective attached to a projected number.
    """
    from src.discovery.layout import compute_text
    from src.discovery.dialects import IBM_ENTERPRISE_COBOL

    document = compute_text(
        (CORPUS / "D10_sync.cpy").read_text(encoding="utf-8"),
        profile=IBM_ENTERPRISE_COBOL,
    )[0].to_dict()
    blob = " ".join(document["limitations"]) + " " + " ".join(
        m["provenance"] for m in document["summary"].values() if m is not None
    )
    assert lint_projected_text(blob) == []


def test_the_measured_layout_keeps_its_verification_claim() -> None:
    """The fix must not strip the measured layout's own, TRUE claim."""
    from src.discovery.layout import IBM_EQUIVALENCE_LIMITATION, compute_text

    document = compute_text(
        (CORPUS / "D02_binary.cpy").read_text(encoding="utf-8")
    )[0].to_dict()
    assert document["limitations"][0] == IBM_EQUIVALENCE_LIMITATION
    assert "186 of 186" in document["summary"]["group_length"]["provenance"]


# --------------------------------------------------------------------------
# Bugbot finding (Medium): an undetermined shift rendered as zero
# --------------------------------------------------------------------------

def test_an_undetermined_shift_renders_as_undetermined_not_zero() -> None:
    """``if f.offset_delta`` collapsed ``None`` and ``0`` to "0".

    An UNKNOWN field has no comparable address on one side, so its shift is
    UNDETERMINED. Printing "0" says it was checked and found unshifted — a
    fabricated zero that survives every type check (D19/R1).
    """
    import dataclasses

    from src.discovery.dialects.classify import Classification

    report = analyse_path(FIXTURES / "UNSOURCED.cpy", GNUCOBOL, IBM)
    assert report is not None
    unknown = next(
        f for f in report.elementary
        if f.classification is Classification.UNKNOWN
    )
    # In the sealed corpus and the fixtures, an UNKNOWN field still gets an
    # offset on both sides (U-FLOAT sits at 5 either way), so `offset_delta`
    # is a real 0 there and no current input reaches the None branch. It
    # becomes reachable the moment a profile lacks a rule the engine HAS --
    # then the projection produces no row for the field and its address is
    # undetermined on one side. So the row is built directly, because the
    # defect is in the RENDERER and testing it needs the input that triggers
    # it rather than the input we happen to have.
    assert unknown.offset_delta == 0
    undetermined = dataclasses.replace(
        unknown, projected_offset=None, projected_length=None,
    )
    assert undetermined.offset_delta is None
    patched = dataclasses.replace(
        report,
        fields=tuple(
            undetermined if f.key == unknown.key else f for f in report.fields
        ),
    )

    text = render_sensitivity_markdown(patched)
    row = next(
        line for line in text.splitlines()
        if line.startswith("|") and undetermined.name in line
    )
    cells = [c.strip() for c in row.strip().strip("|").split("|")]
    assert "—" in cells, f"undetermined shift not marked in {row!r}"
    assert "0" not in cells, (
        f"an undetermined shift was rendered as 0 in {row!r}"
    )
    # And a genuine zero must still print as 0, not be swept into "—".
    invariant = next(
        f for f in report.elementary
        if f.classification is Classification.INVARIANT
    )
    assert invariant.offset_delta == 0
    invariant_row = next(
        line for line in render_sensitivity_markdown(report).splitlines()
        if line.startswith("|") and invariant.name in line
    )
    assert "0" in [c.strip() for c in invariant_row.strip().strip("|").split("|")]
