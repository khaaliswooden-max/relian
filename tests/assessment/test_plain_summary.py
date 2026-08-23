"""Section 0 tests — the plain-language layer restates, and never infers.

A plain-language summary is the easiest place in a report for an estimate to
slip in wearing the clothes of a finding, so the tests here are mostly about
what section 0 is *not* allowed to do: introduce a number, disagree with the
graded section it summarises, or hide a truncation.
"""

import re
from pathlib import Path

import pytest

from src.assessment import cli as cli_mod
from src.assessment.report import (
    CONSTRUCT_IN_PLAIN_WORDS,
    TIER_IN_PLAIN_WORDS,
    render_markdown,
)

FIXTURES = Path(__file__).parent / "fixtures" / "cobol"
DEMO = Path(__file__).resolve().parents[2] / "examples" / "demo"

_NUMBER = re.compile(r"(?<![\w.\-])\d+(?:\.\d+)?(?![\w\-])")


def _render(root: Path) -> str:
    bundle, by_construct = cli_mod.assess_tree(root)
    return render_markdown(bundle, root_label=root.as_posix(),
                           scope_by_construct=by_construct)


def _section(markdown: str, number: str) -> str:
    parts = re.split(r"^## ", markdown, flags=re.M)
    return next(p for p in parts if p.startswith(f"{number}."))


@pytest.fixture(scope="module")
def rendered():
    return _render(FIXTURES)


@pytest.fixture(scope="module")
def demo_rendered():
    return _render(DEMO)


@pytest.fixture(scope="module")
def plain(rendered):
    return _section(rendered, "0")


# --------------------------------------------------------------------------
# Placement
# --------------------------------------------------------------------------


def test_section_zero_comes_before_the_technical_report(rendered):
    assert rendered.index("\n## 0.") < rendered.index("\n## 1.")


def test_section_zero_is_titled_for_a_reader_who_wants_the_meaning(plain):
    assert plain.splitlines()[0].strip() == "0. What this means"


# --------------------------------------------------------------------------
# It restates; it does not recompute
# --------------------------------------------------------------------------


def test_tier_counts_match_the_risk_tiers_section(demo_rendered):
    """The same programs-per-tier counts as section 8, reached independently."""
    plain = _section(demo_rendered, "0")
    risk = _section(demo_rendered, "8")

    def counts_from(section, label_of):
        out = {}
        for line in section.splitlines():
            if not line.startswith("|") or "---" in line:
                continue
            cells = [c.strip() for c in line.strip("|").split("|")]
            if len(cells) >= 2 and cells[1].isdigit():
                key = label_of(cells[0])
                if key:
                    out[key] = int(cells[1])
        return out

    labels = {label: tier for tier, (label, _) in TIER_IN_PLAIN_WORDS.items()}
    plain_counts = counts_from(plain, labels.get)
    risk_counts = counts_from(risk, lambda c: c if c in TIER_IN_PLAIN_WORDS else None)

    assert plain_counts, "section 0 rendered no tier counts"
    assert plain_counts == {t: c for t, c in risk_counts.items() if c}


def test_line_counts_are_the_same_measured_values_as_the_scope_section(demo_rendered):
    plain = _section(demo_rendered, "0")
    scope = _section(demo_rendered, "9")

    def value_after(section, label):
        row = next(l for l in section.splitlines() if l.startswith(f"| {label} |"))
        return [c.strip() for c in row.strip("|").split("|")][1]

    assert value_after(plain, "Lines the converter handles today") == \
        value_after(scope, "Quotable-today code lines")
    assert value_after(plain, "Lines needing new converter capability first") == \
        value_after(scope, "Code lines requiring grammar expansion")


def test_construct_counts_match_the_inventory(demo_rendered):
    plain = _section(demo_rendered, "0")
    inventory = _section(demo_rendered, "5")

    inv = {}
    for line in inventory.splitlines():
        if not line.startswith("|") or "---" in line:
            continue
        cells = [c.strip() for c in line.strip("|").split("|")]
        if len(cells) == 2 and cells[1].isdigit():
            inv[cells[0]] = int(cells[1])

    glossed = re.findall(r"- \*\*([A-Z\-]+)\*\* — .*?Appears (\d+) time", plain)
    plain_only = re.findall(r"- \*\*([A-Z\-]+)\*\* — appears (\d+) time", plain)
    listed = glossed + plain_only

    assert listed, "section 0 listed no constructs"
    for construct, count in listed:
        assert inv[construct] == int(count)


def test_every_number_in_section_zero_is_stated_elsewhere(demo_rendered):
    """No figure originates in the plain-language layer (R1).

    The one number that is section 0's own is the count of constructs it did
    not list, which is a count of rows in the inventory below. It is excluded
    here by matching its sentence, not by loosening the rule.
    """
    plain = _section(demo_rendered, "0")
    body = demo_rendered[demo_rendered.index("\n## 1."):]

    truncations = re.findall(r"\+ (\d+) more|and (\d+) further construct", plain)
    exempt = {n for pair in truncations for n in pair if n}

    elsewhere = set(_NUMBER.findall(body))
    for number in _NUMBER.findall(plain):
        assert number in elsewhere or number in exempt, (
            f"section 0 states {number}, which appears nowhere in sections 1-10"
        )


# --------------------------------------------------------------------------
# It says what it left out
# --------------------------------------------------------------------------


def test_a_truncated_construct_list_says_how_many_it_omitted(demo_rendered):
    plain = _section(demo_rendered, "0")
    listed = len(re.findall(r"^- \*\*[A-Z\-]+\*\* — ", plain, flags=re.M))
    inventory = _section(demo_rendered, "5")
    total = sum(
        1 for l in inventory.splitlines()
        if l.startswith("|") and "---" not in l
        and len([c for c in l.strip("|").split("|")]) == 2
        and l.strip("|").split("|")[1].strip().isdigit()
    )
    if total > listed:
        assert re.search(r"and \d+ further construct", plain), \
            "constructs were omitted without saying so"


def test_a_truncated_program_list_says_how_many_it_omitted():
    from src.assessment.report import _name_list

    names = [f"P{i}.cbl" for i in range(9)]
    rendered = _name_list(names, limit=4)
    assert rendered.endswith("+ 5 more")
    assert rendered.count("`") == 8


def test_a_short_program_list_is_shown_whole():
    from src.assessment.report import _name_list

    assert _name_list(["A.cbl", "B.cbl"], limit=4) == "`A.cbl`, `B.cbl`"
    assert _name_list([]) == "—"


# --------------------------------------------------------------------------
# It makes no commitment the graded report does not make
# --------------------------------------------------------------------------


def test_section_zero_states_its_own_limits(plain):
    lowered = plain.lower()
    assert "does not say what the work would cost" in lowered
    assert "how long it would take" in lowered


def test_section_zero_quotes_no_price_or_duration(plain):
    assert not re.search(r"[$€£]\s*\d", plain)
    assert not re.search(r"\b\d+\s*(?:hours?|days?|weeks?|months?)\b", plain, re.I)


def test_section_zero_carries_a_grade_for_every_table(plain):
    """Same R9 rule as the rest of the body, asserted on this section alone."""
    tables = [l for l in plain.splitlines() if l.startswith("|")]
    assert tables, "section 0 rendered no tables"
    assert "**Grade:**" in plain and "**Provenance:**" in plain


# --------------------------------------------------------------------------
# Degenerate inputs
# --------------------------------------------------------------------------


def test_an_empty_corpus_says_so_rather_than_rendering_empty_tables(tmp_path):
    (tmp_path / "notes.txt").write_text("no cobol here\n", encoding="utf-8")
    plain = _section(_render(tmp_path), "0")
    assert "No COBOL programs were found" in plain
    assert not [l for l in plain.splitlines() if l.startswith("|")]


def test_a_fully_supported_corpus_reports_nothing_in_the_way(tmp_path):
    source = (FIXTURES / "FULLSUP.cbl").read_text(encoding="utf-8")
    (tmp_path / "FULLSUP.cbl").write_text(source, encoding="utf-8")
    plain = _section(_render(tmp_path), "0")
    assert "Nothing. Every statement" in plain


# --------------------------------------------------------------------------
# The glossaries are data, and have to stay honest
# --------------------------------------------------------------------------


def test_every_tier_has_a_plain_reading():
    from src.assessment.models import RiskTier

    assert set(TIER_IN_PLAIN_WORDS) == {t.value for t in RiskTier}


def test_tier_labels_are_short_enough_to_read_in_a_table():
    for label, _explanation in TIER_IN_PLAIN_WORDS.values():
        assert len(label) <= 40


def test_construct_glosses_carry_no_numbers_or_commitments():
    """The gloss says what a construct is; measurement is the count beside it."""
    for construct, gloss in CONSTRUCT_IN_PLAIN_WORDS.items():
        assert not _NUMBER.search(gloss), f"{construct} gloss states a number"
        assert construct.isupper()


def test_an_unglossed_construct_renders_without_a_guessed_explanation():
    from src.assessment.report import TEMPLATES

    row = TEMPLATES["plain_blocker_row_plain"].format(construct="ZZZZ", count=1)
    assert row.strip() == "- **ZZZZ** — appears 1 time(s)."


# --------------------------------------------------------------------------
# The structured form — what the API serves and the Markdown renders
# --------------------------------------------------------------------------


@pytest.fixture(scope="module")
def demo_summary():
    from src.assessment.report import plain_summary

    bundle, by_construct = cli_mod.assess_tree(DEMO)
    return plain_summary(bundle, DEMO.as_posix(), by_construct)


def test_plain_summary_is_json_serialisable(demo_summary):
    """It crosses an HTTP boundary, so it may hold no Python-only objects."""
    import json

    assert json.loads(json.dumps(demo_summary)) == demo_summary


def test_measured_values_travel_with_grade_and_provenance(demo_summary):
    """A number reaching the UI carries its own evidence, or it is absent (R9)."""
    for row in demo_summary["how_much"]["rows"]:
        measured = row["measured"]
        if measured is None:
            continue
        assert set(measured) == {"value", "grade", "provenance"}
        assert measured["grade"] in ("VERIFIED", "PLAUSIBLE", "SPECULATIVE")
        assert measured["provenance"].strip()


def test_groups_carry_the_tier_its_label_and_its_programs(demo_summary):
    groups = demo_summary["where_we_stand"]["groups"]
    assert groups, "no tier groups were built"
    for group in groups:
        assert group["tier"] in TIER_IN_PLAIN_WORDS
        label, explanation = TIER_IN_PLAIN_WORDS[group["tier"]]
        assert group["label"] == label
        assert group["explanation"] == explanation
        assert group["programs"] == len(group["program_ids"])


def test_program_ids_are_never_truncated_in_the_data(demo_summary):
    """Each surface discloses its own truncation, so the data carries them all."""
    total = sum(len(g["program_ids"]) for g in demo_summary["where_we_stand"]["groups"])
    bundle, _by_construct = cli_mod.assess_tree(DEMO)
    assert total == len(bundle.programs)


def test_an_omitted_construct_count_accompanies_the_short_list(demo_summary):
    """The construct list *is* cut, so the number dropped travels with it."""
    from src.assessment.report import _CONSTRUCTS_SHOWN

    bundle, by_construct = cli_mod.assess_tree(DEMO)
    in_the_way = demo_summary["in_the_way"]
    assert len(in_the_way["constructs"]) == min(_CONSTRUCTS_SHOWN, len(by_construct))
    assert in_the_way["omitted"] == len(by_construct) - len(in_the_way["constructs"])


def test_an_unglossed_construct_carries_a_null_gloss_not_a_guess(demo_summary):
    for item in demo_summary["in_the_way"]["constructs"]:
        assert item["gloss"] is None or isinstance(item["gloss"], str)
        if item["gloss"] is not None:
            assert item["gloss"] == CONSTRUCT_IN_PLAIN_WORDS[item["construct"].upper()]


def test_an_empty_corpus_returns_nulls_rather_than_empty_blocks(tmp_path):
    from src.assessment.report import plain_summary

    (tmp_path / "notes.txt").write_text("no cobol here\n", encoding="utf-8")
    bundle, by_construct = cli_mod.assess_tree(tmp_path)
    summary = plain_summary(bundle, tmp_path.as_posix(), by_construct)

    assert "No COBOL programs were found" in summary["scope"]
    for key in ("where_we_stand", "in_the_way", "how_much", "limits"):
        assert summary[key] is None, f"{key} should be absent, not an empty block"


# --------------------------------------------------------------------------
# The Markdown shows everything the data carries — neither surface drifts
# --------------------------------------------------------------------------


def test_markdown_renders_every_figure_the_structure_carries(demo_rendered, demo_summary):
    """Whatever the API serves, the report says too — and the reverse."""
    plain = _section(demo_rendered, "0")

    for key in ("intro", "scope"):
        assert demo_summary[key] in plain

    for group in demo_summary["where_we_stand"]["groups"]:
        assert group["label"] in plain
        assert group["explanation"] in plain
        for program_id in group["program_ids"]:
            assert program_id in plain, f"{program_id} is in the data but not the report"

    for item in demo_summary["in_the_way"]["constructs"]:
        assert f"**{item['construct']}**" in plain
        assert f"Appears {item['count']} time" in plain or \
            f"appears {item['count']} time" in plain
    if demo_summary["in_the_way"]["omitted"]:
        assert f"and {demo_summary['in_the_way']['omitted']} further construct" in plain

    for row in demo_summary["how_much"]["rows"]:
        assert row["label"] in plain
        if row["measured"] is not None:
            assert str(row["measured"]["value"]) in plain

    assert demo_summary["limits"] in plain


def test_the_structure_grade_is_the_one_the_markdown_prints(demo_rendered, demo_summary):
    plain = _section(demo_rendered, "0")
    stands = demo_summary["where_we_stand"]
    assert f"**Grade:** {stands['grade']}" in plain
    assert stands["provenance"] in plain
