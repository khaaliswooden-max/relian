"""WP-2.4 §3.4 — the target-schema DDL, its published mapping table, and the
PARTIAL discipline for constructs that have no honest relational mapping.

Three groups of assertion carry the weight.

**Every rule is disclosed (acceptance ⑥).** `lint_mapping_rules` requires a
basis of `forced` or `choice` on every rule, a rationale on every rule, and an
alternative on every `choice` — and refuses an alternative on a `forced` one,
because if there is an alternative it was never forced. The lint is shown to
bite on each of those shapes, since a lint that has never fired is a lint
nobody has measured.

**No COMPLETE table hides an overlap (acceptance ⑦).** REDEFINES is the same
bytes under two readings. The generator emits the primary reading's columns,
emits the redefining view as commented DDL, and marks the table PARTIAL with
the overlap named in bytes. `test_no_complete_table_contains_an_overlapping_
field_pair` asserts this over the whole sealed 15-copybook corpus, not just
over a fixture.

**Nothing here verifies its own output.** The load and the
`information_schema` reconciliation are the `ddl-loads` CI job
(`tools/ddl_load_check.py`), where PostgreSQL answers. Tests in this file check
generation only, and say so.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from src.discovery.ddl import (
    MAPPING_RULES,
    MAPPING_RULES_BY_ID,
    OCCURS_COLUMN_LIMIT,
    RULESET_VERSION,
    Basis,
    MappingRule,
    TableStatus,
    generate_table,
    lint_mapping_rules,
    lint_schema,
    lint_table,
    precision_scale,
    render_schema,
    render_table,
    sanitize,
)
from src.discovery.layout import compute_text

REPO_ROOT = Path(__file__).resolve().parents[1]
SEALED_CORPUS = REPO_ROOT / "discovery-bench" / "corpus"


def record(body: str):
    """Compute a layout from an inline record description."""
    return compute_text(body)[0]


# --------------------------------------------------------------------------
# Acceptance ⑥ — every rule carries a basis; every choice its alternative
# --------------------------------------------------------------------------

def test_the_published_mapping_table_is_clean():
    assert lint_mapping_rules() == []


def test_every_rule_declares_a_basis():
    for rule in MAPPING_RULES:
        assert rule.basis in (Basis.FORCED, Basis.CHOICE), rule.rule_id


def test_every_rule_has_a_rationale():
    for rule in MAPPING_RULES:
        assert rule.rationale.strip(), rule.rule_id


def test_every_choice_states_an_alternative():
    for rule in MAPPING_RULES:
        if rule.basis is Basis.CHOICE:
            assert (rule.alternative or "").strip(), rule.rule_id


def test_no_forced_rule_offers_an_alternative():
    """If there is an alternative, the rule was never forced."""
    for rule in MAPPING_RULES:
        if rule.basis is Basis.FORCED:
            assert not rule.alternative, rule.rule_id


def test_the_lint_bites_on_a_choice_with_no_alternative():
    planted = MappingRule(
        rule_id="X01", cobol_shape="PIC X(n)", postgres_type="TEXT",
        basis=Basis.CHOICE, rationale="because", alternative=None,
    )
    problems = lint_mapping_rules([planted])
    assert any("no alternative is stated" in p for p in problems)


def test_the_lint_bites_on_a_forced_rule_that_offers_an_alternative():
    planted = MappingRule(
        rule_id="X02", cobol_shape="PIC 9(4)", postgres_type="NUMERIC(4,0)",
        basis=Basis.FORCED, rationale="because", alternative="INTEGER",
    )
    problems = lint_mapping_rules([planted])
    assert any("it was a choice" in p for p in problems)


def test_the_lint_bites_on_a_missing_rationale():
    planted = MappingRule(
        rule_id="X03", cobol_shape="PIC X(n)", postgres_type="TEXT",
        basis=Basis.FORCED, rationale="   ",
    )
    assert any("no rationale" in p for p in lint_mapping_rules([planted]))


def test_the_lint_bites_on_a_duplicate_rule_id():
    rule = MAPPING_RULES[0]
    assert any("duplicate rule id" in p for p in lint_mapping_rules([rule, rule]))


def test_every_emitted_column_cites_a_published_rule():
    layout = record("""
       01  R.
           05  A  PIC X(10).
           05  B  PIC S9(7)V99 COMP-3.
           05  C  PIC S9(4) COMP.
    """)
    table = generate_table(layout)
    for column in table.columns:
        assert column.rule_id in MAPPING_RULES_BY_ID, column.rule_id
    assert lint_table(table) == []


# --------------------------------------------------------------------------
# Type mapping
# --------------------------------------------------------------------------

def test_packed_decimal_precision_and_scale_come_from_the_picture():
    """The D31 worked example: PIC S9(7)V99 COMP-3 -> NUMERIC(9,2), forced."""
    layout = record("""
       01  R.
           05  AMOUNT  PIC S9(7)V99 COMP-3.
    """)
    column = generate_table(layout).columns[0]
    assert column.pg_type == "NUMERIC(9,2)"
    assert MAPPING_RULES_BY_ID[column.rule_id].basis is Basis.FORCED


def test_alphanumeric_maps_to_char_as_a_disclosed_choice():
    """PIC X(30) -> CHAR(30), and the rule says why and what the alternative is."""
    layout = record("""
       01  R.
           05  NAME  PIC X(30).
    """)
    column = generate_table(layout).columns[0]
    assert column.pg_type == "CHAR(30)"
    rule = MAPPING_RULES_BY_ID[column.rule_id]
    assert rule.basis is Basis.CHOICE
    # The rule states the SHAPE (VARCHAR(n)); the width comes from the
    # picture at generation time. A rule that hard-coded 30 would be a
    # rule about one field rather than about the mapping.
    assert "VARCHAR(n)" in (rule.alternative or "")


def test_display_numeric_maps_to_numeric():
    layout = record("""
       01  R.
           05  ID  PIC 9(11).
    """)
    assert generate_table(layout).columns[0].pg_type == "NUMERIC(11,0)"


@pytest.mark.parametrize(
    "picture, expected, rule_id",
    [
        ("S9(4)", "SMALLINT", "M05"),
        ("S9(9)", "INTEGER", "M06"),
        ("S9(18)", "BIGINT", "M07"),
    ],
)
def test_binary_width_selects_the_integer_type(picture, expected, rule_id):
    layout = record(f"""
       01  R.
           05  N  PIC {picture} COMP.
    """)
    column = generate_table(layout).columns[0]
    assert (column.pg_type, column.rule_id) == (expected, rule_id)


def test_binary_with_a_scale_cannot_be_an_integer_type():
    """An integer type cannot carry an implied decimal position."""
    layout = record("""
       01  R.
           05  N  PIC S9(5)V99 COMP.
    """)
    column = generate_table(layout).columns[0]
    assert column.pg_type == "NUMERIC(7,2)"
    assert MAPPING_RULES_BY_ID[column.rule_id].basis is Basis.FORCED


def test_edited_pictures_are_char_because_they_store_formatted_text():
    layout = record("""
       01  R.
           05  SHOWN  PIC ZZ,ZZ9.99.
    """)
    column = generate_table(layout).columns[0]
    assert column.pg_type.startswith("CHAR(")
    assert column.rule_id == "M09"


@pytest.mark.parametrize(
    "picture, expected",
    [
        ("9(5)", (5, 0)),
        ("S9(7)V99", (9, 2)),
        ("9(3)V9(4)", (7, 4)),
        ("X(10)", None),
    ],
)
def test_precision_scale_reads_the_picture(picture, expected):
    assert precision_scale(picture) == expected


def test_an_unexpandable_picture_yields_no_precision_rather_than_a_default():
    """A fabricated (18,0) would truncate nothing today and everything later."""
    assert precision_scale(None) is None
    assert precision_scale("") is None


def test_group_items_get_no_column():
    layout = record("""
       01  R.
           05  GRP.
               10  A  PIC X(4).
               10  B  PIC X(4).
    """)
    names = {c.cobol_name for c in generate_table(layout).columns}
    assert names == {"A", "B"}
    assert "GRP" not in names


def test_filler_gets_no_column():
    layout = record("""
       01  R.
           05  A       PIC X(4).
           05  FILLER  PIC X(6).
           05  B       PIC X(4).
    """)
    table = generate_table(layout)
    assert {c.cobol_name for c in table.columns} == {"A", "B"}


def test_an_unmapped_usage_makes_the_table_partial_rather_than_guessing():
    layout = record("""
       01  R.
           05  A  PIC X(4).
           05  F  USAGE COMP-1.
    """)
    table = generate_table(layout)
    assert table.status is TableStatus.PARTIAL
    assert table.partial_reasons


# --------------------------------------------------------------------------
# 88-levels become CHECK constraints (rule M10)
# --------------------------------------------------------------------------

def test_condition_names_become_a_check_constraint():
    """The upgrade the COBOL source could never enforce."""
    layout = record("""
       01  R.
           05  STATUS-CODE  PIC X(01).
               88  ST-ACTIVE  VALUE 'Y'.
               88  ST-CLOSED  VALUE 'N', 'C'.
    """)
    column = generate_table(layout).columns[0]
    assert len(column.checks) == 1
    assert "IN ('C', 'N', 'Y')" in column.checks[0]


def test_a_field_with_no_conditions_gets_no_check():
    layout = record("""
       01  R.
           05  A  PIC X(4).
    """)
    assert generate_table(layout).columns[0].checks == ()


def test_a_thru_range_is_not_flattened_into_an_in_list():
    """VALUE 'A' THRU 'M' admits thirteen values; an IN list would allow two."""
    layout = record("""
       01  R.
           05  GRADE  PIC X(01).
               88  PASSING  VALUE 'A' THRU 'M'.
    """)
    table = generate_table(layout)
    assert table.columns[0].checks == ()
    assert any("THRU" in note for note in table.notes)


def test_a_figurative_constant_is_not_enumerated():
    layout = record("""
       01  R.
           05  MARKER  PIC X(01).
               88  UNSET  VALUE SPACES.
    """)
    table = generate_table(layout)
    assert table.columns[0].checks == ()
    assert any("figurative constant" in note for note in table.notes)


def test_a_partial_enumeration_generates_no_constraint_at_all():
    """Half an enumeration would reject values the source admits."""
    layout = record("""
       01  R.
           05  CODE  PIC X(01).
               88  KNOWN     VALUE 'A', 'B'.
               88  RANGEY    VALUE 'C' THRU 'F'.
    """)
    table = generate_table(layout)
    assert table.columns[0].checks == ()
    assert any("partial enumeration" in note for note in table.notes)


# --------------------------------------------------------------------------
# Acceptance ⑦ — REDEFINES and ODO are PARTIAL, with the overlap named
# --------------------------------------------------------------------------

REDEFINED = """
       01  R.
           05  A          PIC X(4).
           05  RAW-DATE   PIC X(8).
           05  SPLIT-DATE REDEFINES RAW-DATE.
               10  YY     PIC X(4).
               10  MM     PIC X(2).
               10  DD     PIC X(2).
"""


def test_redefines_makes_the_table_partial():
    table = generate_table(record(REDEFINED))
    assert table.status is TableStatus.PARTIAL


def test_the_overlap_names_start_byte_length_and_both_field_sets():
    table = generate_table(record(REDEFINED))
    assert len(table.overlaps) == 1
    overlap = table.overlaps[0]

    assert overlap.start_byte == 5
    assert overlap.length == 8
    assert overlap.primary_fields == ("RAW-DATE",)
    assert overlap.redefining_fields == ("YY", "MM", "DD")
    assert overlap.redefines_name == "SPLIT-DATE"
    assert overlap.redefines_target == "RAW-DATE"


def test_only_the_primary_reading_is_emitted_as_columns():
    """Both would be wrong (not independent); one silently would be worse."""
    table = generate_table(record(REDEFINED))
    names = {c.cobol_name for c in table.columns}
    assert "RAW-DATE" in names
    assert names.isdisjoint({"YY", "MM", "DD"})


def test_the_redefining_view_is_emitted_as_commented_ddl():
    table = generate_table(record(REDEFINED))
    assert table.commented_ddl
    body = "\n".join(table.commented_ddl)
    assert "SPLIT-DATE REDEFINES RAW-DATE" in body
    for line in body.split("\n"):
        assert line.strip().startswith("--"), line


def test_no_discriminator_column_is_invented():
    """Nothing in the source says which reading applies to which record."""
    table = generate_table(record(REDEFINED))
    names = {c.name for c in table.columns}
    for invented in ("record_type", "discriminator", "variant", "redefines_tag"):
        assert invented not in names


def test_odo_is_always_partial():
    """The extent is a runtime property; no static analysis supplies it."""
    layout = record("""
       01  R.
           05  N       PIC 9(2).
           05  ITEM    OCCURS 1 TO 5 TIMES DEPENDING ON N PIC X(4).
    """)
    table = generate_table(layout)
    assert table.status is TableStatus.PARTIAL
    assert any("OCCURS DEPENDING ON" in r for r in table.partial_reasons)


def test_a_small_occurs_becomes_numbered_columns():
    layout = record("""
       01  R.
           05  SLOT  OCCURS 3 TIMES PIC X(2).
    """)
    table = generate_table(layout)
    assert [c.name for c in table.columns] == ["slot_1", "slot_2", "slot_3"]


def test_a_large_occurs_is_partial_with_a_child_table_alternative():
    layout = record(f"""
       01  R.
           05  SLOT  OCCURS {OCCURS_COLUMN_LIMIT + 4} TIMES PIC X(2).
    """)
    table = generate_table(layout)
    assert table.status is TableStatus.PARTIAL
    assert any("child table" in r for r in table.partial_reasons)


def test_a_partial_table_always_says_why():
    layout = record(REDEFINED)
    table = generate_table(layout)
    assert table.partial_reasons
    assert lint_table(table) == []


def test_the_lint_bites_on_a_complete_table_that_overlaps():
    """Planted red for acceptance ⑦'s guard."""
    from dataclasses import replace

    table = generate_table(record(REDEFINED))
    overlapping = replace(
        table,
        status=TableStatus.COMPLETE,
        partial_reasons=(),
        columns=table.columns + (
            replace(table.columns[-1], name="shadow", offset=5, length=8),
        ),
    )
    problems = lint_table(overlapping)
    assert any("overlapping columns" in p for p in problems)


def test_the_lint_bites_on_a_partial_table_with_no_reason():
    from dataclasses import replace

    table = replace(
        generate_table(record(REDEFINED)),
        partial_reasons=(),
        overlaps=(),
    )
    assert any("no reason given" in p for p in lint_table(table))


# --------------------------------------------------------------------------
# Acceptance ⑦ over the sealed corpus, not just a fixture
# --------------------------------------------------------------------------

def _sealed_tables():
    tables = []
    for member in sorted(SEALED_CORPUS.rglob("*.cpy"), key=lambda p: p.as_posix()):
        text = member.read_text(encoding="utf-8", errors="replace")
        for layout in compute_text(text, origin=member.as_posix()):
            tables.append(generate_table(
                layout, table_name=sanitize(f"{member.stem}_{layout.group}")
            ))
    return tables


def test_the_sealed_corpus_generates_tables():
    assert len(_sealed_tables()) >= 15


def test_no_complete_table_contains_an_overlapping_field_pair():
    """Acceptance ⑦, over all fifteen sealed copybooks."""
    for table in _sealed_tables():
        if table.status is TableStatus.COMPLETE:
            assert table.overlapping_column_pairs() == (), table.name


def test_the_sealed_corpus_lints_clean():
    assert lint_schema(_sealed_tables()) == []


def test_every_sealed_column_cites_a_published_rule():
    for table in _sealed_tables():
        for column in table.columns:
            assert column.rule_id in MAPPING_RULES_BY_ID


# --------------------------------------------------------------------------
# Rendering
# --------------------------------------------------------------------------

def test_the_ddl_header_names_the_ruleset_version():
    sql = render_table(generate_table(record("01 R. 05 A PIC X(4).")))
    assert RULESET_VERSION in sql


def test_a_partial_table_carries_its_reasons_into_the_sql():
    sql = render_table(generate_table(record(REDEFINED)))
    assert "Status: PARTIAL" in sql
    assert "two interpretations" in sql


def test_the_schema_header_publishes_the_whole_mapping_table():
    sql = render_schema([generate_table(record("01 R. 05 A PIC X(4)."))])
    for rule in MAPPING_RULES:
        assert f"[{rule.rule_id}]" in sql


def test_the_schema_header_states_that_no_dataset_was_opened():
    """R12, where the customer will actually read it."""
    sql = render_schema([generate_table(record("01 R. 05 A PIC X(4)."))])
    assert "No customer dataset was opened" in sql


def test_identifiers_are_quoted_so_reserved_words_are_safe():
    layout = record("""
       01  R.
           05  ORDER  PIC X(4).
           05  TABLE  PIC X(4).
    """)
    sql = render_table(generate_table(layout))
    assert '"order"' in sql
    assert '"table"' in sql


def test_sanitize_lowercases_and_replaces_hyphens():
    assert sanitize("ACCT-BAL") == "acct_bal"
    assert sanitize("9LIVES") == "c_9lives"
    assert sanitize("") == "unnamed"


def test_duplicate_column_names_are_disambiguated():
    layout = record("""
       01  R.
           05  GRP-A.
               10  VALUE-X  PIC X(2).
           05  GRP-B.
               10  VALUE-X  PIC X(2).
    """)
    names = [c.name for c in generate_table(layout).columns]
    assert len(names) == len(set(names)), names
