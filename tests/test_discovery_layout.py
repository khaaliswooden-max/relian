"""WP-2.2 §3.2 — the layout engine, axis by axis.

``tests/test_layout_roundtrip.py`` grades the engine against the sealed
oracle and is the load-bearing gate. This file does something the round-trip
cannot: it exercises the **rules** in isolation, on inputs the corpus does not
contain, so that a failure names one construct instead of one copybook.

It also owns acceptance ⑦ — the D19 lint. WP-2.1 measured why that matters:
``MOVE HIGH-VALUES`` to a ``PIC ZZZZZZZZ9.99`` compiles, runs, exits 0, and
marks **zero bytes**, with no warning enabled by default. A probe built on it
would have recorded ``offset: 0, length: 0`` for every numeric-edited field in
the corpus, including ``BPR-AMOUNT-DUE``, whose true layout is offset 46,
length 12. The trap here is the same shape on a new surface: a length the
engine cannot compute must come back ``None``, never ``0``, because a
fabricated zero survives every check that inspects types rather than
provenance.

R7 shows up as a *refusal*: a usage the sealed corpus does not measure
(``COMP-1``, ``COMP-2``, ``INDEX``, ``POINTER``, …) yields ``None`` with a
named reason rather than a plausible width. An honest eleven-of-twelve is
shippable; a fudged twelve is not.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from src.discovery.layout import (
    BINARY_WIDTHS,
    LayoutStatus,
    binary_width,
    compute,
    compute_text,
    expand_picture,
    lint_layout,
    packed_width,
    picture_category,
    picture_digits,
    picture_size,
)

REPO_ROOT = Path(__file__).resolve().parents[1]
CORPUS = REPO_ROOT / "discovery-bench" / "corpus"


def _fixed(*lines: str) -> str:
    return "\n".join(f"{i:06d}{line}" for i, line in enumerate(lines, 1)) + "\n"


def _record(*entries: str):
    """One 01 record built from the given entry lines, laid out."""
    layouts = compute_text(_fixed(*entries))
    assert len(layouts) == 1, [layout.group for layout in layouts]
    return layouts[0]


def _sizes(layout):
    return [(f.name, f.offset, f.length) for f in layout.fields if f.elementary]


# ==========================================================================
# PICTURE
# ==========================================================================

@pytest.mark.parametrize(
    "picture,expanded",
    [
        ("9", ["9"]),
        ("9(05)", ["9"] * 5),
        ("S9(03)", ["S", "9", "9", "9"]),
        ("S9(05)V99", ["S"] + ["9"] * 5 + ["V", "9", "9"]),
        ("SV9(04)", ["S", "V"] + ["9"] * 4),
        ("S9(04)PPP", ["S"] + ["9"] * 4 + ["P", "P", "P"]),
        ("SPPP9(04)", ["S", "P", "P", "P"] + ["9"] * 4),
        ("XXBXX", ["X", "X", "B", "X", "X"]),
        ("$$,$$9.99CR", ["$", "$", ",", "$", "$", "9", ".", "9", "9", "CR"]),
    ],
)
def test_picture_expansion(picture: str, expanded: list) -> None:
    assert expand_picture(picture) == expanded


@pytest.mark.parametrize(
    "picture,size",
    [
        ("9(18)", 18),          # the eighteen-digit end of the range
        ("S9(05)", 5),          # an embedded sign occupies no byte of its own
        ("SV9(04)", 4),         # V is an ASSUMED decimal point
        ("S9(04)PPP", 4),       # P scales; it does not occupy
        ("SPPP9(04)", 4),
        ("ZZZ,ZZ9.99", 10),
        ("$$,$$9.99CR", 11),    # CR is TWO character positions
        ("XXX/XX/XXXX", 11),
        ("A(06)", 6),
    ],
)
def test_picture_size_in_character_positions(picture: str, size: int) -> None:
    symbols = expand_picture(picture)
    assert symbols is not None
    assert picture_size(symbols) == size


def test_p_positions_count_as_digits_but_not_as_bytes() -> None:
    """The distinction that makes ``S9(04)PPP`` 4 bytes wide and 7 digits
    deep. Collapsing the two would silently change binary and packed widths."""
    symbols = expand_picture("S9(04)PPP")
    assert symbols is not None
    assert picture_digits(symbols) == 7
    assert picture_size(symbols) == 4


@pytest.mark.parametrize(
    "picture,category",
    [
        ("9(05)", "NUMERIC"),
        ("S9(05)V99", "NUMERIC"),
        ("ZZZ,ZZ9.99", "NUMERIC-EDITED"),
        ("X(10)", "ALPHANUMERIC"),
        ("XXBXX", "ALPHANUMERIC-EDITED"),
        ("A(06)", "ALPHABETIC"),
    ],
)
def test_picture_category(picture: str, category: str) -> None:
    symbols = expand_picture(picture)
    assert symbols is not None
    assert picture_category(symbols) == category


@pytest.mark.parametrize("picture", ["9(0)", "", "9(", "9(4", "X(-2)", "9()"])
def test_a_malformed_picture_yields_none_rather_than_a_guess(picture: str) -> None:
    """An unparseable PICTURE has no size. Returning a plausible one — or a
    zero — is the D19 defect with the symbol table as its disguise."""
    assert expand_picture(picture) is None


# ==========================================================================
# Axis: DISPLAY numeric
# ==========================================================================

def test_display_numeric_axis() -> None:
    layout = _record(
        "   01  R.",
        "       05  UNSIGNED   PIC 9(05).",
        "       05  SIGNED     PIC S9(05).",
        "       05  DECIMAL    PIC 9(05)V99.",
        "       05  FRACTION   PIC SV9(04).",
        "       05  TRAIL      PIC S9(04)PPP.",
        "       05  LEAD       PIC SPPP9(04).",
    )
    assert _sizes(layout) == [
        ("UNSIGNED", 1, 5), ("SIGNED", 6, 5), ("DECIMAL", 11, 7),
        ("FRACTION", 18, 4), ("TRAIL", 22, 4), ("LEAD", 26, 4),
    ]
    assert layout.group_length == 29


# ==========================================================================
# Axis: binary
# ==========================================================================

def test_binary_width_table_is_the_one_the_oracle_measured() -> None:
    """WP-2.2's brief said "1-4 digits -> 2 bytes". The SEALED MEASUREMENT
    says ``D02-COMP-1 PIC S9(01) COMP`` occupies 1 byte, so the table is
    1-2 -> 1, 3-4 -> 2, 5-9 -> 4, 10-18 -> 8. The measurement wins."""
    assert BINARY_WIDTHS == ((2, 1), (4, 2), (9, 4), (18, 8))
    assert [binary_width(d) for d in (1, 2, 3, 4, 5, 9, 10, 18)] == [
        1, 1, 2, 2, 4, 4, 8, 8
    ]


def test_a_binary_item_wider_than_the_measured_range_returns_none() -> None:
    """R7 — 19 digits is outside anything the oracle measured, so no width is
    claimed. Extrapolating the table one step is exactly the kind of plausible
    guess this project keeps deleting."""
    assert binary_width(19) is None
    layout = _record("   01  R.", "       05  TOO-WIDE  PIC S9(19) COMP.")
    field = layout.by_key()["TOO-WIDE"]
    assert field.length is None
    assert field.unmeasured_reason and "1-18" in field.unmeasured_reason
    assert layout.status is LayoutStatus.PARTIAL


@pytest.mark.parametrize("usage", ["COMP", "COMP-4", "BINARY", "COMPUTATIONAL"])
def test_comp_synonyms_all_use_the_binary_table(usage: str) -> None:
    layout = _record("   01  R.", f"       05  N  PIC S9(09) {usage}.")
    assert layout.by_key()["N"].length == 4


def test_comp5_uses_native_binary_widths() -> None:
    layout = _record(
        "   01  R.",
        "       05  A  PIC S9(04) COMP-5.",
        "       05  B  PIC S9(18) COMP-5.",
    )
    assert _sizes(layout) == [("A", 1, 2), ("B", 3, 8)]


# ==========================================================================
# Axis: COMP-3
# ==========================================================================

@pytest.mark.parametrize(
    "digits,width",
    [(1, 1), (2, 2), (3, 2), (4, 3), (5, 3), (6, 4), (7, 4), (8, 5), (9, 5), (18, 10)],
)
def test_packed_width_is_ceil_digits_plus_one_over_two(digits: int, width: int) -> None:
    assert packed_width(digits) == width


def test_packed_decimal_counts_the_scaled_digits_too() -> None:
    """``S9(07)V99`` is nine digits, not seven — the fraction is stored."""
    layout = _record("   01  R.", "       05  N  PIC S9(07)V99 COMP-3.")
    assert layout.by_key()["N"].length == 5


def test_an_unsigned_packed_item_still_carries_a_sign_nibble() -> None:
    """Measured: ``D03-UNSIGNED-3 PIC 9(03) COMP-3`` occupies 2 bytes, which
    is ``ceil((3+1)/2)`` — the same formula, sign or no sign."""
    layout = _record("   01  R.", "       05  N  PIC 9(03) COMP-3.")
    assert layout.by_key()["N"].length == 2


@pytest.mark.parametrize("usage", ["COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL"])
def test_packed_synonyms(usage: str) -> None:
    layout = _record("   01  R.", f"       05  N  PIC S9(05) {usage}.")
    assert layout.by_key()["N"].length == 3


# ==========================================================================
# Axis: SIGN clause
# ==========================================================================

def test_sign_separate_adds_a_byte_and_embedded_does_not() -> None:
    layout = _record(
        "   01  R.",
        "       05  DEFAULT       PIC S9(04).",
        "       05  LEAD-EMB      PIC S9(04) SIGN LEADING.",
        "       05  LEAD-SEP      PIC S9(04) SIGN LEADING SEPARATE.",
        "       05  TRAIL-EMB     PIC S9(04) SIGN TRAILING.",
        "       05  TRAIL-SEP     PIC S9(04) SIGN TRAILING SEPARATE CHARACTER.",
    )
    assert _sizes(layout) == [
        ("DEFAULT", 1, 4), ("LEAD-EMB", 5, 4), ("LEAD-SEP", 9, 5),
        ("TRAIL-EMB", 14, 4), ("TRAIL-SEP", 18, 5),
    ]


def test_the_word_sign_is_optional() -> None:
    layout = _record("   01  R.", "       05  N  PIC S9(04) TRAILING SEPARATE.")
    assert layout.by_key()["N"].length == 5
    assert layout.by_key()["N"].sign == "TRAILING"


# ==========================================================================
# Axis: OCCURS
# ==========================================================================

def test_occurs_on_an_elementary_item_strides_by_its_own_size() -> None:
    layout = _record("   01  R.", "       05  SLOT OCCURS 4 TIMES PIC X(05).")
    assert _sizes(layout) == [
        ("SLOT", 1, 5), ("SLOT", 6, 5), ("SLOT", 11, 5), ("SLOT", 16, 5)
    ]
    assert [f.key for f in layout.fields if f.name == "SLOT"] == [
        "SLOT (1)", "SLOT (2)", "SLOT (3)", "SLOT (4)"
    ]


def test_occurs_on_a_group_strides_by_the_group_size() -> None:
    layout = _record(
        "   01  R.",
        "       05  ROW OCCURS 3 TIMES.",
        "           10  CODE-X  PIC X(02).",
        "           10  AMT     PIC S9(05) COMP-3.",
    )
    rows = layout.by_key()
    assert (rows["ROW (1)"].offset, rows["ROW (1)"].length) == (1, 5)
    assert (rows["ROW (3)"].offset, rows["ROW (3)"].length) == (11, 5)
    assert rows["AMT (3)"].offset == 13


def test_nested_occurs_has_an_inner_stride_that_is_not_the_outer_stride() -> None:
    layout = _record(
        "   01  R.",
        "       05  OUTER OCCURS 3 TIMES",
        "                     INDEXED BY IX-OUTER.",
        "           10  OUTER-KEY  PIC X(02).",
        "           10  INNER OCCURS 2 TIMES",
        "                         INDEXED BY IX-INNER.",
        "               15  INNER-CODE  PIC X(03).",
        "               15  INNER-QTY   PIC S9(03) COMP-3.",
    )
    rows = layout.by_key()
    assert rows["OUTER (2)"].offset == 13          # outer stride 12
    assert rows["INNER (1, 2)"].offset == 8        # inner stride 5
    assert rows["INNER-QTY (3, 2)"].offset == 35
    assert layout.group_length == 36


def test_indexed_by_names_are_captured_and_occupy_no_storage() -> None:
    layout = _record(
        "   01  R.",
        "       05  T OCCURS 2 TIMES INDEXED BY IX-A PIC X(03).",
        "       05  TAIL  PIC X(01).",
    )
    assert layout.by_key()["TAIL"].offset == 7
    assert "IX-A" not in layout.by_key()


# ==========================================================================
# Axis: OCCURS DEPENDING ON
# ==========================================================================

def test_odo_is_computed_at_both_extents_and_names_its_controlling_item() -> None:
    entries = (
        "   01  R.",
        "       05  N       PIC 9(02).",
        "       05  ENTRY-X OCCURS 1 TO 4 TIMES",
        "                     DEPENDING ON N.",
        "           10  E-A  PIC X(05).",
    )
    at_min = compute_text(_fixed(*entries), odo_value=1)[0]
    at_max = compute_text(_fixed(*entries), odo_value=4)[0]
    assert (at_min.group_length, at_max.group_length) == (7, 22)
    assert at_min.odo_object == at_max.odo_object == "N"
    assert at_min.variable_length is True
    assert len([f for f in at_max.fields if f.name == "E-A"]) == 4


def test_an_odo_layout_with_no_extent_given_says_which_extent_it_used() -> None:
    """R2 — a variable-length record reported as one number, with no note of
    which occurrence count produced it, is a measurement with the units filed
    off."""
    layout = _record(
        "   01  R.",
        "       05  N       PIC 9(02).",
        "       05  ENTRY-X OCCURS 1 TO 4 TIMES DEPENDING ON N PIC X(05).",
    )
    assert layout.status is LayoutStatus.PARTIAL
    assert any("MAXIMUM" in reason for reason in layout.reasons)


# ==========================================================================
# Axis: REDEFINES
# ==========================================================================

def test_redefines_takes_the_redefined_items_offset_and_does_not_advance() -> None:
    layout = _record(
        "   01  R.",
        "       05  KEY-X   PIC X(10).",
        "       05  KEY-R REDEFINES KEY-X.",
        "           10  KR-PREFIX  PIC X(03).",
        "           10  KR-BODY    PIC X(07).",
        "       05  TAIL    PIC X(02).",
    )
    rows = layout.by_key()
    assert rows["KEY-R"].offset == 1
    assert rows["KR-PREFIX"].offset == 1 and rows["KR-BODY"].offset == 4
    assert rows["TAIL"].offset == 11, "a REDEFINES must not consume storage"
    assert layout.group_length == 12


def test_a_smaller_redefinition_still_starts_at_the_same_offset() -> None:
    layout = _record(
        "   01  R.",
        "       05  DATE-X        PIC 9(08).",
        "       05  DATE-SHORT REDEFINES DATE-X  PIC X(04).",
        "       05  TAIL          PIC X(01).",
    )
    rows = layout.by_key()
    assert (rows["DATE-SHORT"].offset, rows["DATE-SHORT"].length) == (1, 4)
    assert rows["TAIL"].offset == 9


def test_two_redefinitions_of_one_target_both_land_on_it() -> None:
    layout = _record(
        "   01  R.",
        "       05  GRP.",
        "           10  A  PIC X(04).",
        "           10  B  PIC X(03).",
        "       05  FLAT REDEFINES GRP  PIC X(07).",
        "       05  HEAD REDEFINES GRP  PIC X(02).",
    )
    rows = layout.by_key()
    assert rows["FLAT"].offset == rows["HEAD"].offset == 1
    assert layout.group_length == 7


def test_fields_under_a_redefinition_are_excluded_from_the_tiling_projection() -> None:
    """SPEC.md §6 — the tiling invariant is CONDITIONED, or it is wrong.
    REDEFINES overlaps deliberately, and a gate that fires on correct
    behaviour trains its readers to wave it through."""
    layout = _record(
        "   01  R.",
        "       05  KEY-X   PIC X(10).",
        "       05  KEY-R REDEFINES KEY-X.",
        "           10  KR-PREFIX  PIC X(03).",
        "           10  KR-BODY    PIC X(07).",
    )
    assert [f.name for f in layout.tiling_fields()] == ["KEY-X"]


def test_a_redefines_naming_an_unplaced_item_claims_no_offset() -> None:
    layout = _record(
        "   01  R.",
        "       05  A  PIC X(04).",
        "       05  B REDEFINES NO-SUCH-FIELD  PIC X(04).",
    )
    field = layout.by_key()["B"]
    assert field.offset is None and field.length is None
    assert field.unmeasured_reason
    assert layout.status is LayoutStatus.PARTIAL


# ==========================================================================
# Axis: level-88
# ==========================================================================

def test_conditions_have_no_storage_and_appear_in_no_field_row() -> None:
    layout = _record(
        "   01  R.",
        "       05  STATUS-X  PIC X(01).",
        "           88  IS-ACTIVE   VALUE \"A\".",
        "           88  IS-ANY-OPEN VALUE \"A\", \"B\", \"C\".",
        "       05  TAIL      PIC X(01).",
    )
    assert [c.name for c in layout.conditions] == ["IS-ACTIVE", "IS-ANY-OPEN"]
    assert all(c.offset is None and c.length is None for c in layout.conditions)
    assert all(c.host == "STATUS-X" for c in layout.conditions)
    assert "IS-ACTIVE" not in layout.by_key()
    assert layout.by_key()["TAIL"].offset == 2


def test_a_condition_on_a_packed_host_is_still_storage_free() -> None:
    layout = _record(
        "   01  R.",
        "       05  AMT  PIC S9(05) COMP-3.",
        "           88  AMT-ZERO  VALUE ZERO.",
        "           88  AMT-BAND  VALUE 100 THRU 999.",
    )
    assert layout.group_length == 3
    assert [c.host for c in layout.conditions] == ["AMT", "AMT"]


def test_a_value_clause_containing_a_period_does_not_end_the_statement() -> None:
    """``VALUE 0.0625`` and ``VALUE "MMUD."`` both carry a period that is not
    a separator. Splitting on every ``.`` would truncate the record."""
    layout = _record(
        "   01  R.",
        "       05  RATE  PIC S9V9(04) COMP-3 VALUE 0.0625.",
        "       05  CODE-X  PIC X(04) VALUE \"MM.D\".",
        "       05  TAIL  PIC X(01).",
    )
    assert _sizes(layout) == [("RATE", 1, 3), ("CODE-X", 4, 4), ("TAIL", 8, 1)]


# ==========================================================================
# Axis: SYNCHRONIZED
# ==========================================================================

def test_sync_aligns_to_the_items_own_width_and_records_the_slack() -> None:
    """MEASURED against ``D10_sync.cpy``: 20 declared bytes in a 25-byte
    group, slack at offsets 2, 10 and 16. The rule that reproduces it is
    "align the item to its own width relative to the start of the record"."""
    layout = compute(CORPUS / "D10_sync.cpy")
    assert layout is not None
    assert layout.group_length == 25
    assert sum(f.length for f in layout.tiling_fields()) == 20
    assert [(g.offset, g.length, g.source) for g in layout.gaps] == [
        (2, 3, "slack"), (10, 1, "slack"), (16, 1, "slack")
    ]


def test_sync_on_an_already_aligned_item_inserts_nothing() -> None:
    layout = _record(
        "   01  R.",
        "       05  A  PIC X(04).",
        "       05  B  PIC S9(09) COMP SYNCHRONIZED.",
    )
    assert layout.gaps == ()
    assert layout.by_key()["B"].offset == 5


def test_sync_on_a_display_item_is_not_claimed_and_is_reported() -> None:
    """R2/R7 — GnuCOBOL's handling of SYNC on a non-binary item is not
    measured by the sealed corpus, so nothing is moved and the layout says so
    rather than applying a rule nobody checked."""
    layout = _record(
        "   01  R.",
        "       05  A  PIC X(01).",
        "       05  B  PIC X(04) SYNC.",
    )
    assert layout.by_key()["B"].offset == 2
    assert layout.status is LayoutStatus.PARTIAL
    assert any("SYNCHRONIZED" in reason for reason in layout.reasons)


def test_sync_inside_an_occurs_table_narrows_the_claim_to_partial() -> None:
    """The honest edge of the hardest axis. Slack placement inside a table is
    not measured by v0.1, so a layout containing one is PARTIAL with the
    construct named — not approximated, and not silently claimed."""
    layout = _record(
        "   01  R.",
        "       05  T OCCURS 2 TIMES.",
        "           10  C  PIC X(01).",
        "           10  N  PIC S9(09) COMP SYNC.",
    )
    assert layout.status is LayoutStatus.PARTIAL
    assert any("not measured" in reason for reason in layout.reasons)


# ==========================================================================
# Axis: level-66 RENAMES
# ==========================================================================

def test_renames_is_a_span_and_carries_the_renamed_start_item() -> None:
    layout = _record(
        "   01  R.",
        "       05  ALPHA  PIC X(03).",
        "       05  BETA   PIC X(04).",
        "       05  GAMMA.",
        "           10  G1  PIC X(02).",
        "           10  G2  PIC X(02).",
        "   66  ALPHA-BETA RENAMES ALPHA THRU BETA.",
        "   66  JUST-GAMMA RENAMES GAMMA.",
    )
    rows = layout.by_key()
    assert (rows["ALPHA-BETA"].offset, rows["ALPHA-BETA"].length) == (1, 7)
    assert rows["ALPHA-BETA"].redefines == "ALPHA"
    assert rows["ALPHA-BETA"].renames is True
    assert rows["ALPHA-BETA"].level == 66
    assert (rows["JUST-GAMMA"].offset, rows["JUST-GAMMA"].length) == (8, 4)


def test_renames_thru_may_end_on_a_group() -> None:
    layout = _record(
        "   01  R.",
        "       05  ALPHA  PIC X(03).",
        "       05  BETA   PIC X(04).",
        "       05  GAMMA.",
        "           10  G1  PIC X(02).",
        "           10  G2  PIC X(02).",
        "   66  BETA-GAMMA RENAMES BETA THRU GAMMA.",
    )
    assert (layout.by_key()["BETA-GAMMA"].offset,
            layout.by_key()["BETA-GAMMA"].length) == (4, 8)


def test_a_renames_span_is_excluded_from_the_tiling_projection() -> None:
    layout = _record(
        "   01  R.",
        "       05  ALPHA  PIC X(03).",
        "       05  BETA   PIC X(04).",
        "   66  BOTH RENAMES ALPHA THRU BETA.",
    )
    assert [f.name for f in layout.tiling_fields()] == ["ALPHA", "BETA"]
    assert layout.group_length == 7


def test_a_renames_of_an_unplaced_item_claims_no_span() -> None:
    layout = _record(
        "   01  R.",
        "       05  ALPHA  PIC X(03).",
        "   66  GHOST RENAMES NO-SUCH-FIELD.",
    )
    field = layout.by_key()["GHOST"]
    assert field.offset is None and field.length is None
    assert field.unmeasured_reason


# ==========================================================================
# Axis: edited
# ==========================================================================

def test_edited_axis() -> None:
    layout = compute(CORPUS / "D12_edited.cpy")
    assert layout is not None
    assert layout.group_length == 66
    rows = layout.by_key()
    assert (rows["D12-AMOUNT-EDIT"].offset, rows["D12-AMOUNT-EDIT"].length) == (1, 10)
    assert rows["D12-BWZ-EDITED"].blank_when_zero is True
    assert rows["D12-JUST-X"].justified is True
    assert rows["D12-JUST-A"].justified is True
    assert rows["D12-CREDIT-EDIT"].length == 11
    assert rows["D12-JUST-A"].category == "ALPHABETIC"


def test_blank_when_zero_and_justified_do_not_change_the_length() -> None:
    """The clauses are erased from the listing's PICTURE column (SPEC.md §8),
    which is exactly why the engine must read them from the SOURCE and know
    they cost nothing."""
    plain = _record("   01  R.", "       05  N  PIC ZZZZ9.")
    bwz = _record("   01  R.", "       05  N  PIC ZZZZ9 BLANK WHEN ZERO.")
    just = _record("   01  R.", "       05  N  PIC X(06) JUSTIFIED RIGHT.")
    assert plain.group_length == bwz.group_length == 5
    assert just.group_length == 6


# ==========================================================================
# Acceptance (7) — the D19 lint
# ==========================================================================

@pytest.mark.parametrize(
    "path", sorted(CORPUS.glob("*.cpy")), ids=lambda p: p.name
)
def test_every_corpus_layout_passes_the_lint(path: Path) -> None:
    layout = compute(path)
    assert layout is not None
    assert lint_layout(layout) == []


def test_an_uncomputable_length_is_none_and_never_zero() -> None:
    """U10's trap, on a new surface. A zero here would be indistinguishable
    from a real zero-length answer and would survive every type check."""
    layout = _record("   01  R.", "       05  P  PIC X(04) USAGE POINTER.")
    field = layout.by_key()["P"]
    assert field.length is None
    assert field.length is not 0            # noqa: F632 - the point is identity
    assert field.unmeasured_reason and "not covered by" in field.unmeasured_reason
    assert lint_layout(layout) == []


@pytest.mark.parametrize("usage", ["COMP-1", "COMP-2", "INDEX", "POINTER", "COMP-X"])
def test_a_usage_the_sealed_corpus_does_not_measure_is_refused(usage: str) -> None:
    """R7 — no construct is "supported" until RELIAN-BENCH covers it. These
    have plausible, widely documented widths; the engine still declines to
    state one, because a documented width is not a measured width."""
    layout = _record("   01  R.", f"       05  N  PIC S9(04) {usage}.")
    field = layout.by_key()["N"]
    assert field.length is None
    assert usage in (field.unmeasured_reason or "")
    assert layout.status is LayoutStatus.PARTIAL


def test_the_lint_catches_a_fabricated_zero() -> None:
    """The negative control. A lint that has never rejected anything is a lint
    whose failure path is unproven."""
    import dataclasses

    layout = compute(CORPUS / "MUBCUST.cpy")
    assert layout is not None
    assert lint_layout(layout) == []
    fabricated = dataclasses.replace(layout.fields[1], length=0)
    broken = dataclasses.replace(layout, fields=(layout.fields[0], fabricated))
    problems = lint_layout(broken)
    assert any("length 0" in problem for problem in problems)


def test_the_lint_catches_a_null_length_with_no_stated_reason() -> None:
    import dataclasses

    layout = compute(CORPUS / "MUBCUST.cpy")
    assert layout is not None
    silent = dataclasses.replace(layout.fields[1], length=None, unmeasured_reason=None)
    problems = lint_layout(dataclasses.replace(layout, fields=(silent,)))
    assert any("no unmeasured_reason" in problem for problem in problems)


def test_the_lint_catches_a_condition_that_grew_an_extent() -> None:
    import dataclasses

    layout = compute(CORPUS / "D09_conditions.cpy")
    assert layout is not None
    grown = dataclasses.replace(layout.conditions[0], offset=1, length=1)
    problems = lint_layout(dataclasses.replace(layout, conditions=(grown,)))
    assert any("no storage" in problem for problem in problems)


# ==========================================================================
# Reuse of the assessment models (U14)
# ==========================================================================

def test_summary_numbers_carry_provenance_and_a_grade() -> None:
    """R9 — every externally visible number carries a Trutina grade and a
    provenance field. ``assessment.models.Measured`` is reused rather than
    reimplemented; it refuses to hold ``None`` at all."""
    layout = compute(CORPUS / "D10_sync.cpy")
    assert layout is not None
    summary = layout.summary()
    assert summary["group_length"].value == 25
    assert summary["declared_field_bytes"].value == 20
    assert summary["gap_bytes"].value == 5
    for measured in summary.values():
        assert measured is None or (measured.provenance and measured.grade)


def test_a_partial_layout_reports_no_declared_byte_total() -> None:
    """An absent Measured, not a Measured holding a placeholder."""
    layout = _record("   01  R.", "       05  P  PIC S9(04) INDEX.")
    assert layout.summary()["declared_field_bytes"] is None


def test_the_grade_is_plausible_rather_than_verified() -> None:
    """R11 — the round-trip verifies this engine against GnuCOBOL 3.1.2 only.
    IBM Enterprise COBOL equivalence is UNMEASURED, and SYNC slack in
    particular moves with compiler options, so VERIFIED would overclaim."""
    layout = compute(CORPUS / "MUBCUST.cpy")
    assert layout is not None
    assert layout.summary()["group_length"].grade == "PLAUSIBLE"


# ==========================================================================
# Structural
# ==========================================================================

def test_multiple_records_in_one_copybook_are_returned_separately() -> None:
    layouts = compute_text(_fixed(
        "   01  FIRST-REC.",
        "       05  A  PIC X(04).",
        "   01  SECOND-REC.",
        "       05  B  PIC X(06).",
    ))
    assert [(layout.group, layout.group_length) for layout in layouts] == [
        ("FIRST-REC", 4), ("SECOND-REC", 6)
    ]


def test_a_named_record_can_be_selected() -> None:
    path = CORPUS / "MUBCONS.cpy"
    assert compute(path, record="MUB-CONSTANTS").group == "MUB-CONSTANTS"
    assert compute(path, record="NO-SUCH-RECORD") is None


def test_offsets_are_one_based() -> None:
    """U5. Converting at a boundary is a place to be wrong, so there is no
    conversion anywhere in this engine."""
    layout = compute(CORPUS / "MUBCUST.cpy")
    assert layout is not None
    rows = layout.by_key()
    assert rows["WS-CW-ACCOUNT"].offset == 1
    assert rows["WS-CW-NAME"].offset == 11


def test_a_condition_on_a_table_item_is_reported_once_not_dropped() -> None:
    """A level-88 declared on an OCCURS item must still be reported.

    Emitting conditions only for unsubscripted items would drop the construct
    entirely, and a whole axis vanishing with nothing saying so is worse than
    a wrong number (R2).
    """
    layout = _record(
        "   01  R.",
        "       05  SLOT OCCURS 3 TIMES  PIC X(01).",
        "           88  SLOT-SET  VALUE \"Y\".",
        "       05  TAIL  PIC X(01).",
    )
    assert [(c.name, c.host) for c in layout.conditions] == [("SLOT-SET", "SLOT")]
    assert layout.group_length == 4
