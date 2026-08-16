"""WP-1.7 template lint.

Two gates, and a proof that each gate has teeth:

1. No template string contains a numeric literal. Section numbers are
   substituted, not typed, so the templates hold no digits at all — a number
   can only reach the page from a measured value.
2. Every table of numbers in the rendered body (sections 1–9) either carries
   ``Grade`` and ``Provenance`` columns, or sits under a ``**Grade:** …
   **Provenance:** …`` line in the same section (R9).

The appendices are exempt from gate 2 by design and the exemption is asserted
rather than assumed: they reproduce rule text, tool versions, and the supported
registry verbatim. Those are not measurements and dressing them with a grade
would dilute what a grade means.
"""

import re
from pathlib import Path

import pytest

from src.assessment import cli as cli_mod
from src.assessment import report as report_mod
from src.assessment.report import FORBIDDEN_VOCABULARY, TEMPLATES

FIXTURES = Path(__file__).parent / "fixtures" / "cobol"

# A standalone number: not part of an identifier (sha256), not part of a
# hyphenated name (COBOL-85), not a version fragment.
_NUMERIC_LITERAL = re.compile(r"(?<![\w.\-])\d+(?:\.\d+)?(?![\w\-])")
# A section number typed directly into a heading, e.g. "## 3. Coverage".
_HEADING_NUMBER = re.compile(r"^#{1,6}\s+\d+(?:\.\d+)*\.?", re.M)


def scan_templates_for_literals(templates) -> list:
    """Return ``[(name, literal), …]`` for every numeric literal in a template."""
    violations = []
    for name, text in sorted(templates.items()):
        stripped = _HEADING_NUMBER.sub("", text)
        stripped = re.sub(r"\{[^}]*\}", "", stripped)      # substitution slots
        violations.extend((name, m.group(0)) for m in _NUMERIC_LITERAL.finditer(stripped))
    return violations


@pytest.fixture(scope="module")
def rendered(tmp_path_factory):
    bundle, by_construct = cli_mod.assess_tree(FIXTURES)
    out = tmp_path_factory.mktemp("report")
    report_mod.render(bundle, out, root_label="fixtures", scope_by_construct=by_construct)
    return (out / "assessment.md").read_text(encoding="utf-8")


# --------------------------------------------------------------------------
# Gate 1 — templates hold no numbers
# --------------------------------------------------------------------------


def test_templates_contain_no_numeric_literals():
    assert scan_templates_for_literals(TEMPLATES) == []


def test_planted_literal_in_a_template_is_caught():
    """The gate must fail on a planted literal, or it is decoration."""
    planted = dict(TEMPLATES)
    planted["planted"] = "Coverage is typically 80% for programs of this size.\n"
    violations = scan_templates_for_literals(planted)
    assert ("planted", "80") in violations


def test_planted_literal_survives_the_placeholder_stripper():
    """A number next to a substitution slot must still be caught."""
    planted = {"planted": "{name} scored 95 out of {total}.\n"}
    assert ("planted", "95") in scan_templates_for_literals(planted)


def test_section_numbers_are_not_flagged():
    assert scan_templates_for_literals({"h": "## 3. Coverage map\n"}) == []


# --------------------------------------------------------------------------
# Gate 2 — every rendered number carries a grade and a provenance
# --------------------------------------------------------------------------


def _body_sections(markdown: str):
    """Split the rendered report into ``## `` sections, dropping the appendices."""
    parts = re.split(r"^## ", markdown, flags=re.M)[1:]
    return [p for p in parts if not p.startswith("10.")]


def _tables(section: str):
    """Yield ``(header_line, rows, text_before_the_table)`` for each table."""
    lines = section.splitlines()
    i = 0
    while i < len(lines):
        if lines[i].startswith("|"):
            start = i
            while i < len(lines) and lines[i].startswith("|"):
                i += 1
            yield lines[start], lines[start:i], "\n".join(lines[:start])
        else:
            i += 1


def test_every_body_table_with_numbers_is_graded(rendered):
    ungraded = []
    for section in _body_sections(rendered):
        for header, rows, before in _tables(section):
            body_rows = rows[2:]
            if not any(_NUMERIC_LITERAL.search(r) for r in body_rows):
                continue                       # no numbers, nothing to grade
            if "Grade" in header and "Provenance" in header:
                continue
            if "**Grade:**" in before and "**Provenance:**" in before:
                continue
            ungraded.append((section.splitlines()[0], header))
    assert ungraded == [], f"tables of numbers with no grade/provenance: {ungraded}"


def test_the_grade_gate_catches_an_ungraded_table(rendered):
    """Plant an ungraded numeric table and confirm the gate rejects it."""
    planted = rendered + (
        "\n## 99. Planted section\n\n"
        "| Measure | Value |\n| --- | --- |\n| Throughput | 42 |\n"
    )
    ungraded = []
    for section in _body_sections(planted):
        for header, rows, before in _tables(section):
            if not any(_NUMERIC_LITERAL.search(r) for r in rows[2:]):
                continue
            if "Grade" in header and "Provenance" in header:
                continue
            if "**Grade:**" in before and "**Provenance:**" in before:
                continue
            ungraded.append(header)
    assert ungraded, "the gate did not notice a planted ungraded numeric table"


def test_every_grade_in_the_report_is_a_real_trutina_grade(rendered):
    for grade in re.findall(r"\*\*Grade:\*\* (\w+)", rendered):
        assert grade in ("VERIFIED", "PLAUSIBLE", "SPECULATIVE")


def test_unmeasured_values_render_as_absent_not_zero(rendered):
    """BROKEN.cbl has no coverage; the cell must be a dash, never 0."""
    row = next(l for l in rendered.splitlines()
               if l.startswith("| BROKEN.cbl |") and "no statements recovered" in l)
    cells = [c.strip() for c in row.strip("|").split("|")]
    assert cells[1] == "—"
    assert "0" not in cells[1]


# --------------------------------------------------------------------------
# R11 — the report makes no commercial commitment
# --------------------------------------------------------------------------


def test_report_states_no_fee_timeline_or_equivalence_claim(rendered):
    lowered = rendered.lower()
    found = [phrase for phrase in FORBIDDEN_VOCABULARY if phrase in lowered]
    assert found == [], f"report contains commercial vocabulary it may not use: {found}"


def test_report_does_not_quote_a_price(rendered):
    assert not re.search(r"[$€£]\s*\d", rendered)
    assert "per LOC" not in rendered
    assert "/LOC" not in rendered


def test_scope_section_says_attribution_is_by_line(rendered):
    assert "Attribution is by source line" in rendered
    assert "does not price the work" in rendered


# --------------------------------------------------------------------------
# Appendices carry the rules verbatim
# --------------------------------------------------------------------------


def test_appendix_reproduces_risk_rules_verbatim(rendered):
    from src.assessment.risk import rule_table
    for rule in rule_table():
        assert rule in rendered, f"RISK_RULES entry missing from the appendix: {rule}"


def test_appendix_reproduces_counting_rules_and_formulas(rendered):
    assert "COUNTING RULES" in rendered
    assert "FORMULAS" in rendered
    assert "Appendix D — coverage method and its limits" in rendered


def test_appendix_names_the_supported_set_and_its_source(rendered):
    from src.assessment.supported import boundary_only_tokens, supported_verbs
    assert "SUPPORTED_STATEMENTS@" in rendered
    for verb in supported_verbs():
        assert f"`{verb}`" in rendered
    for token in boundary_only_tokens():
        assert f"`{token}`" in rendered


def test_appendix_reports_accepted_ignored_features_as_such(rendered):
    assert "accepted_ignored" in rendered
    assert "parses but is discarded" in rendered
