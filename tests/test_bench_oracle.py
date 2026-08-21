"""RELIAN-DISCOVERY-BENCH v0.1 -- the oracle is a measurement, and stays one.

WP-2.1 §3.3. `discovery-bench/oracle/oracle.json` is generated ground truth
that sits INSIDE the seal (D7), so these tests do two different jobs:

  * prove the committed oracle still matches what `cobc` produces today, which
    is what makes the re-derivability claim checkable rather than asserted;
  * prove the invariants the oracle's own structure depends on -- the
    conditioned tiling of D9, the separation of derived `gap` spans from
    measured `probe` rows, the nulls on level-88 -- so a future change that
    quietly fabricates a number goes red here rather than shipping.

`RELIAN_REQUIRE_COBC=1` turns "cobc is missing" from a skip into a failure. CI
sets it. Locally, without GnuCOBOL, the cobc-dependent tests skip and say so;
the pure-JSON tests below still run against the committed oracle, so a machine
with no COBOL toolchain still checks the artifact's internal consistency.

Nothing here writes to `discovery-bench/`. Regeneration goes to `tmp_path`.
"""

from __future__ import annotations

import importlib.util
import json
import os
import sys
import types
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parent.parent
BENCH = REPO_ROOT / "discovery-bench"
CORPUS = BENCH / "corpus"
ORACLE_PATH = BENCH / "oracle" / "oracle.json"
DEMO_COPY = REPO_ROOT / "examples" / "demo" / "copy"

SEEDED = ("MUBBREC.cpy", "MUBCONS.cpy", "MUBCUST.cpy")
AUTHORED = (
    "D01_display.cpy", "D02_binary.cpy", "D03_packed.cpy", "D04_sign.cpy",
    "D05_occurs.cpy", "D06_occurs_nested.cpy", "D07_odo.cpy",
    "D08_redefines.cpy", "D09_conditions.cpy", "D10_sync.cpy",
    "D11_renames.cpy", "D12_edited.cpy",
)


def _load_harness() -> types.ModuleType:
    """Import `discovery-bench/harness` under a legal package name.

    The directory name contains a hyphen, so it cannot be imported normally.
    Loading it by path here keeps the sealed tree's layout free of a
    Python-packaging constraint that has nothing to do with the benchmark.
    """
    if "discovery_bench_harness" in sys.modules:
        return sys.modules["discovery_bench_harness.oracle_layouts"]
    package = types.ModuleType("discovery_bench_harness")
    package.__path__ = [str(BENCH / "harness")]
    sys.modules["discovery_bench_harness"] = package
    for name in ("gen_probe", "oracle_layouts"):
        spec = importlib.util.spec_from_file_location(
            f"discovery_bench_harness.{name}", BENCH / "harness" / f"{name}.py"
        )
        module = importlib.util.module_from_spec(spec)
        sys.modules[f"discovery_bench_harness.{name}"] = module
        spec.loader.exec_module(module)
        setattr(package, name, module)
    return sys.modules["discovery_bench_harness.oracle_layouts"]


oracle_layouts = _load_harness()
gen_probe = sys.modules["discovery_bench_harness.gen_probe"]


def _cobc_reason() -> str:
    return (
        "GnuCOBOL `cobc` is not installed, so the oracle cannot be "
        "re-measured. Set RELIAN_REQUIRE_COBC=1 to turn this skip into a "
        "failure (CI does)."
    )


#: Skip when cobc is absent -- unless the environment forbids skipping.
needs_cobc = pytest.mark.skipif(
    not oracle_layouts.cobc_available()
    and os.environ.get(oracle_layouts.REQUIRE_COBC_ENV) != "1",
    reason=_cobc_reason(),
)


@pytest.fixture(scope="module")
def committed() -> dict:
    return json.loads(ORACLE_PATH.read_text(encoding="utf-8"))


@pytest.fixture(scope="module")
def regenerated() -> dict:
    oracle_layouts.require_cobc()
    return oracle_layouts.build_oracle(CORPUS)


# --- the corpus itself ------------------------------------------------------


def test_the_corpus_is_exactly_fifteen_copybooks():
    files = sorted(p.name for p in CORPUS.glob("*.cpy"))
    assert files == sorted(SEEDED + AUTHORED)
    assert len(files) == 15


def test_seed_copies_are_byte_identical_in_both_directions():
    """D10: two copies of the same bytes, one editable and one frozen.

    Asserted in both directions so a change to EITHER copy goes red rather than
    silent. If the demo copybooks ever need to change, the discovery bench
    re-versions -- that is the intended forcing function, not an obstacle.
    """
    for name in SEEDED:
        sealed = (CORPUS / name).read_bytes()
        source = (DEMO_COPY / name).read_bytes()
        assert sealed == source, f"{name}: discovery-bench copy differs from source"
        assert source == sealed, f"{name}: source differs from discovery-bench copy"


def test_every_authored_copybook_declares_its_axis_and_that_it_is_synthetic():
    for name in AUTHORED:
        text = (CORPUS / name).read_text(encoding="utf-8")
        header = text.split("01 ")[0].upper()
        assert "AXIS:" in header, f"{name}: header names no axis"
        assert "SYNTHETIC" in header, f"{name}: header does not say it is synthetic"
        assert "NOT DERIVED FROM ANY CUSTOMER" in header, (
            f"{name}: header does not disclaim customer provenance"
        )


def test_no_copybook_line_exceeds_column_72():
    """Fixed-format COBOL truncates silently past column 72."""
    for path in sorted(CORPUS.glob("*.cpy")):
        overlong = [
            (n, len(line))
            for n, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1)
            if len(line) > gen_probe.MAX_COLUMN
        ]
        assert not overlong, f"{path.name}: lines past column 72: {overlong}"


# --- the committed oracle, checked without needing cobc --------------------


def test_committed_oracle_is_serialised_deterministically():
    raw = ORACLE_PATH.read_bytes()
    assert raw.endswith(b"\n") and not raw.endswith(b"\n\n")
    document = json.loads(raw.decode("utf-8"))
    assert oracle_layouts.dumps(document).encode("utf-8") == raw, (
        "the committed oracle is not what dumps() produces for its own "
        "content -- sorted keys, indent 2, one trailing newline"
    )


def test_every_number_in_the_oracle_names_its_source(committed):
    """Acceptance gate ④, mechanised. The rule has no exceptions on purpose."""
    assert oracle_layouts.lint(committed) == []


def test_the_lint_actually_bites(committed):
    """A gate that cannot fail is not a gate."""
    import copy

    tampered = copy.deepcopy(committed)
    tampered["copybooks"][0]["variants"][0]["fields"][0].pop("source")
    problems = oracle_layouts.lint(tampered)
    assert problems and "no `source`" in problems[0]

    relabelled = copy.deepcopy(committed)
    relabelled["copybooks"][0]["variants"][0]["gaps"] = [
        {"offset": 1, "length": 1, "source": "guessed"}
    ]
    assert any("not one of" in p for p in oracle_layouts.lint(relabelled))


def test_offsets_are_one_based_and_match_the_measurement_in_the_wp(committed):
    """T14, re-measured: WS-CW-ACCOUNT = 1 and WS-CW-NAME = 11."""
    assert committed["conventions"]["offset_base"] == 1
    fields = _fields(committed, "MUBCUST.cpy")
    assert fields["WS-CW-ACCOUNT"]["offset"] == 1
    assert fields["WS-CW-NAME"]["offset"] == 11
    assert min(f["offset"] for f in fields.values()) == 1


def test_mubcust_reproduces_the_flat_cross_check_from_the_wp(committed):
    """T15: the probe layout tiles 1-54 and the total equals Route A's 00054."""
    variant = _variant(committed, "MUBCUST.cpy")
    assert variant["group_length"] == 54
    assert variant["listing"]["group_size"] == 54
    tiling = [f for f in variant["fields"] if f["in_tiling"]]
    assert sum(f["length"] for f in tiling) == 54
    assert variant["gaps"] == [], "MUBCUST.cpy is flat; it should claim every byte"


def test_every_field_row_carries_a_probe_source_and_gaps_are_kept_apart(committed):
    """D8: a derived span never sits in the same list as a measurement."""
    for book in committed["copybooks"]:
        for variant in book["variants"]:
            for row in variant["fields"]:
                assert row["source"] == "probe", (
                    f"{book['file']}: {row['key']} is in fields[] with source "
                    f"{row['source']!r}; only measurements belong there"
                )
                assert row["offset"] >= 1 and row["length"] >= 1
            for gap in variant["gaps"]:
                assert gap["source"] == "gap"
                assert gap["offset"] >= 1 and gap["length"] >= 1


def test_conditions_carry_nulls_and_appear_in_no_field_row(committed):
    """A level-88 has no storage. Zero is not an honest answer (R1)."""
    total = 0
    for book in committed["copybooks"]:
        for variant in book["variants"]:
            field_names = {row["name"] for row in variant["fields"]}
            for condition in variant["conditions"]:
                total += 1
                assert condition["level"] == 88
                assert condition["offset"] is None
                assert condition["length"] is None
                assert condition["name"] not in field_names, (
                    f"{condition['name']} has no extent but appears in fields[]"
                )
                assert condition["host"] in field_names
    # Nine across all sixteen variants: D09's seven plus MUBCUST's two. The
    # only copybook measured twice is D07, which declares no conditions, so no
    # condition is double-counted here.
    assert total == 9


def test_the_d9_tiling_invariant_holds_for_every_copybook_and_variant(committed):
    """The conditioned invariant, re-asserted against the committed document.

    `oracle_layouts` raises during generation if this fails, so this test is
    the check that the COMMITTED file still satisfies it -- a hand-edited
    oracle would pass generation because generation never ran.
    """
    checked = 0
    for book in committed["copybooks"]:
        for variant in book["variants"]:
            length = variant["group_length"]
            cover = [0] * (length + 1)
            for row in variant["fields"]:
                if not row["in_tiling"]:
                    continue
                for index in range(row["offset"], row["offset"] + row["length"]):
                    cover[index] += 1
            for gap in variant["gaps"]:
                for index in range(gap["offset"], gap["offset"] + gap["length"]):
                    cover[index] += 1
            assert all(cover[i] == 1 for i in range(1, length + 1)), (
                f"{book['file']} (odo={variant['odo_value']}): the projection "
                f"plus gaps does not cover [1, {length}] exactly once"
            )
            checked += 1
    assert checked == 16, f"expected 16 variants, asserted {checked}"


def test_sum_of_tiling_fields_and_gaps_equals_the_route_a_group_size(committed):
    """Zero tolerance, per copybook, at every ODO value."""
    for book in committed["copybooks"]:
        for variant in book["variants"]:
            probed = sum(f["length"] for f in variant["fields"] if f["in_tiling"])
            gapped = sum(g["length"] for g in variant["gaps"])
            assert probed + gapped == variant["group_length"], book["file"]
        # At the maximum extent the runtime length must equal Route A's SIZE.
        widest = max(v["group_length"] for v in book["variants"])
        assert widest == book["variants"][0]["listing"]["group_size"], book["file"]


def test_aliases_are_asserted_separately_from_the_tiling(committed):
    """REDEFINES lies within its target; a level-66 starts at what it renames."""
    redefines_seen = renames_seen = 0
    for book in committed["copybooks"]:
        for variant in book["variants"]:
            by_key = {row["key"]: row for row in variant["fields"]}
            for row in variant["fields"]:
                if row["renames"]:
                    renames_seen += 1
                    assert not row["in_tiling"]
                    assert row["offset"] >= 1
                    assert (
                        row["offset"] + row["length"] - 1
                        <= variant["group_length"]
                    )
                elif row["redefines"]:
                    redefines_seen += 1
                    assert not row["in_tiling"]
                    target = by_key[row["redefines"]]
                    assert row["offset"] >= target["offset"]
                    assert (
                        row["offset"] + row["length"]
                        <= target["offset"] + target["length"]
                    )
    assert redefines_seen >= 4, "D08 declares four redefinitions"
    assert renames_seen >= 3, "D11 declares three level-66 items"


def test_odo_copybook_records_both_lengths_and_they_differ(committed):
    book = next(b for b in committed["copybooks"] if b["file"] == "D07_odo.cpy")
    assert book["variable_length"] is True
    assert book["odo_field"] == "D07-COUNT"
    assert book["window"] == gen_probe.WINDOW_REFMOD, (
        "a variable-length group cannot be redefined; it must use the "
        "reference-modification window"
    )
    lengths = [v["group_length"] for v in book["variants"]]
    assert len(lengths) == 2 and lengths[0] < lengths[1], lengths
    assert [v["odo_value"] for v in book["variants"]] == [1, 4]
    # Per-occurrence offsets do not move when the count does.
    first = {r["key"]: r["offset"] for r in book["variants"][0]["fields"]}
    second = {r["key"]: r["offset"] for r in book["variants"][1]["fields"]}
    shared = set(first) & set(second)
    assert shared and all(first[k] == second[k] for k in shared)


def test_every_other_copybook_is_fixed_length_and_uses_the_redefines_window(
    committed,
):
    for book in committed["copybooks"]:
        if book["file"] == "D07_odo.cpy":
            continue
        assert book["variable_length"] is False, book["file"]
        assert book["window"] == gen_probe.WINDOW_TABLE, book["file"]
        assert len(book["variants"]) == 1, book["file"]


def test_the_sync_axis_recovered_five_slack_bytes_as_gaps(committed):
    """Measured: 20 bytes of declared fields in a 25-byte group."""
    variant = _variant(committed, "D10_sync.cpy")
    assert variant["group_length"] == 25
    declared = sum(f["length"] for f in variant["fields"] if f["in_tiling"])
    assert declared == 20
    assert sum(g["length"] for g in variant["gaps"]) == 5
    assert [(g["offset"], g["length"]) for g in variant["gaps"]] == [
        (2, 3), (10, 1), (16, 1)
    ]


def test_filler_is_recovered_as_gap_and_never_as_a_field(committed):
    variant = _variant(committed, "MUBBREC.cpy")
    assert "FILLER" not in {row["name"] for row in variant["fields"]}
    assert [(g["offset"], g["length"]) for g in variant["gaps"]] == [
        (11, 2), (44, 2), (58, 2)
    ]


def test_the_numeric_edited_field_the_naive_probe_would_have_zeroed(committed):
    """`BPR-AMOUNT-DUE PIC ZZZZZZZZ9.99` -- see SPEC.md §5.

    A plain `MOVE HIGH-VALUES TO <numeric-edited>` writes no high-value bytes
    at all, so the naive probe would have recorded offset 0 / length 0 here and
    in every field of D12. Its real layout is 46/12.
    """
    fields = _fields(committed, "MUBBREC.cpy")
    row = fields["BPR-AMOUNT-DUE"]
    assert (row["offset"], row["length"]) == (46, 12)
    assert row["marked_bytes"] == 12, "every byte was marked, not just the extent"

    for row in _fields(committed, "D12_edited.cpy").values():
        assert row["offset"] > 0 and row["length"] > 0, row["key"]
        assert row["marked_bytes"] == row["length"], row["key"]


def test_the_counts_block_matches_the_rows_it_summarises(committed):
    assert oracle_layouts.summarise(committed["copybooks"]) == committed["counts"]
    assert committed["counts"] == {
        "copybooks": 15,
        "variants": 16,
        "elementary_fields": 124,
        "probe_rows": 170,
        "gap_rows": 6,
        "conditions": 9,
        "source": "derived",
    }


def test_route_a_and_route_b_agreed_on_every_copybook_with_zero_tolerance(committed):
    """Acceptance gate ③. `n` is stated per copybook, not just in total."""
    agreement = committed["agreement"]
    assert agreement["tolerance"] == 0
    assert agreement["mismatches"] == 0
    assert agreement["route_a_vs_route_b_checks"] == 186
    per = agreement["per_copybook"]
    assert set(per) == {p.name for p in CORPUS.glob("*.cpy")}
    assert all(entry["mismatches"] == 0 for entry in per.values())
    assert all(entry["checks"] > 0 for entry in per.values())
    assert sum(e["checks"] for e in per.values()) == 186


def test_spec_states_the_same_integers_the_oracle_does(committed):
    """Gate ①: SPEC.md's counts are the oracle's, not a second opinion."""
    spec = (BENCH / "SPEC.md").read_text(encoding="utf-8")
    counts = committed["counts"]
    for label, value in (
        ("Copybooks", counts["copybooks"]),
        ("Layout variants measured", counts["variants"]),
        ("Elementary fields", counts["elementary_fields"]),
        ("Probe rows", counts["probe_rows"]),
        ("Gap rows", counts["gap_rows"]),
        ("Level-88 conditions", counts["conditions"]),
    ):
        assert f"| {label} | **{value}** |" in spec, (
            f"SPEC.md does not state {label} as {value}"
        )
    assert (
        f"| Route A ↔ Route B agreement checks | **"
        f"{committed['agreement']['route_a_vs_route_b_checks']}** |"
    ) in spec


def test_the_oracle_records_the_toolchain_that_produced_it(committed):
    assert committed["toolchain"]["cobc"] == "cobc (GnuCOBOL) 3.1.2.0", (
        "every offset in this oracle is that compiler's byte layout; an "
        "oracle derived under a different one is a different oracle"
    )


def test_recorded_copybook_hashes_match_the_files_on_disk(committed):
    import hashlib

    for book in committed["copybooks"]:
        actual = hashlib.sha256((CORPUS / book["file"]).read_bytes()).hexdigest()
        assert actual == book["sha256"], book["file"]


# --- regeneration, which needs cobc ----------------------------------------


@needs_cobc
def test_the_committed_oracle_is_byte_identical_to_a_fresh_regeneration(
    regenerated, tmp_path: Path
):
    """Acceptance gate ②, local half. CI runs the same check on a clean runner."""
    fresh = oracle_layouts.dumps(regenerated)
    (tmp_path / "fresh.json").write_text(fresh, encoding="utf-8")
    assert fresh == ORACLE_PATH.read_text(encoding="utf-8"), (
        "the committed oracle no longer matches what cobc produces. Do NOT "
        "edit the oracle to agree: re-version (v0.2, new seal, new tag) or "
        "find what changed."
    )


@needs_cobc
def test_regeneration_is_byte_identical_across_two_consecutive_runs(regenerated):
    """Gate ②, determinism half: no clock, no paths, no unordered iteration."""
    second = oracle_layouts.build_oracle(CORPUS)
    assert oracle_layouts.dumps(regenerated) == oracle_layouts.dumps(second)


@needs_cobc
def test_every_probe_compiles_and_runs_under_the_pinned_cobc(tmp_path: Path):
    """Every copybook must compile INSIDE a probe, not merely on its own.

    A copybook that will not compile in a probe is a copybook defect, and it is
    fixed before sealing; afterwards it would be a v0.2.
    """
    for copybook in oracle_layouts.corpus_files(CORPUS):
        workdir = tmp_path / copybook.stem
        workdir.mkdir()
        entries, _ = oracle_layouts.route_a(copybook, workdir)
        plan, _root, _renames = oracle_layouts.plan_for(copybook, entries)
        rows, lengths, source, _warnings = oracle_layouts.route_b(
            plan, copybook, workdir
        )
        assert rows and lengths, copybook.name
        assert "HEX-OF" not in source.upper(), (
            f"{copybook.name}: FUNCTION HEX-OF does not exist in GnuCOBOL "
            f"3.1.2 and must never be emitted (T12)"
        )
        assert all(
            len(line) <= gen_probe.MAX_COLUMN for line in source.splitlines()
        ), f"{copybook.name}: generated source overruns column 72"


@needs_cobc
def test_both_byte_windows_agree_on_a_fixed_copybook(tmp_path: Path):
    """The ODO fallback is proved equivalent, not assumed equivalent.

    `D07_odo.cpy` must use reference modification because cobc refuses to
    redefine a variable-length group. That is a second measurement path, so it
    is checked against the primary one on a copybook where both are legal.
    """
    copybook = CORPUS / "D05_occurs.cpy"
    entries, _ = oracle_layouts.route_a(copybook, tmp_path)
    plan, _root, _renames = oracle_layouts.plan_for(copybook, entries)
    assert plan.window == gen_probe.WINDOW_TABLE

    table_rows, table_lengths, _s, _w = oracle_layouts.route_b(
        plan, copybook, tmp_path
    )
    plan.window = gen_probe.WINDOW_REFMOD
    refmod_rows, refmod_lengths, _s, _w = oracle_layouts.route_b(
        plan, copybook, tmp_path
    )

    assert table_lengths == refmod_lengths
    # ProbeRow is a frozen dataclass, so equality is structural.
    assert table_rows == refmod_rows
    assert table_rows, "the comparison must not be vacuously true"


@needs_cobc
def test_a_variable_length_group_cannot_be_redefined(tmp_path: Path):
    """The measured reason the second window exists, pinned as a fact."""
    copybook = CORPUS / "D07_odo.cpy"
    entries, _ = oracle_layouts.route_a(copybook, tmp_path)
    plan, _root, _renames = oracle_layouts.plan_for(copybook, entries)
    plan.window = gen_probe.WINDOW_TABLE
    with pytest.raises(ValueError, match="cannot be variable length"):
        gen_probe.build(plan)


@needs_cobc
def test_the_naive_probe_form_silently_measures_nothing_on_a_numeric_edited_item(
    tmp_path: Path,
):
    """The planted red behind SPEC.md §5, executed rather than recounted.

    `MOVE HIGH-VALUES TO <numeric-edited>` compiles, runs, exits zero and marks
    ZERO bytes. This is why the probe reference-modifies. If GnuCOBOL ever
    changes this, the deviation's justification changes with it and this test
    is where that surfaces.
    """
    oracle_layouts.require_cobc()
    program = tmp_path / "naive.cbl"
    program.write_text(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. NAIVE.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01  WS-G.\n"
        "           05  WS-EDIT             PIC ZZZZZZZZ9.99.\n"
        "       01  WS-T REDEFINES WS-G.\n"
        "           05  WS-BYTE OCCURS 12 TIMES PIC X(01).\n"
        "       01  WS-I     PIC 9(05) COMP-5.\n"
        "       01  WS-COUNT PIC 9(05) COMP-5.\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE LOW-VALUES TO WS-T\n"
        "           MOVE HIGH-VALUES TO WS-EDIT\n"
        "           MOVE 0 TO WS-COUNT\n"
        "           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12\n"
        "               IF WS-BYTE (WS-I) = HIGH-VALUE\n"
        "                   ADD 1 TO WS-COUNT\n"
        "               END-IF\n"
        "           END-PERFORM\n"
        '           DISPLAY "NAIVE|" WS-COUNT\n'
        "           MOVE LOW-VALUES TO WS-T\n"
        "           MOVE HIGH-VALUES TO WS-EDIT (1:)\n"
        "           MOVE 0 TO WS-COUNT\n"
        "           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12\n"
        "               IF WS-BYTE (WS-I) = HIGH-VALUE\n"
        "                   ADD 1 TO WS-COUNT\n"
        "               END-IF\n"
        "           END-PERFORM\n"
        '           DISPLAY "REFMOD|" WS-COUNT\n'
        "           GOBACK.\n",
        encoding="utf-8",
    )
    import subprocess

    compiled = subprocess.run(
        ["cobc", "-x", "-o", "naive", "naive.cbl"],
        cwd=str(tmp_path), capture_output=True, text=True, timeout=120,
    )
    assert compiled.returncode == 0, compiled.stderr
    run = subprocess.run(
        [str(tmp_path / "naive")], cwd=str(tmp_path),
        capture_output=True, text=True, timeout=120,
    )
    assert run.returncode == 0, "the naive form does not even fail loudly"
    marked = {
        line.split("|")[0]: int(line.split("|")[1])
        for line in run.stdout.splitlines() if "|" in line
    }
    assert marked["NAIVE"] == 0, (
        "the plain MOVE now marks bytes on a numeric-edited item; SPEC.md §5 "
        "and gen_probe's deviation need revisiting"
    )
    assert marked["REFMOD"] == 12


@needs_cobc
def test_require_cobc_env_turns_a_missing_toolchain_into_a_failure(monkeypatch):
    """Green-by-skip, closed on day one rather than discovered later."""
    monkeypatch.setattr(oracle_layouts, "cobc_available", lambda: False)
    monkeypatch.setenv(oracle_layouts.REQUIRE_COBC_ENV, "1")
    with pytest.raises(oracle_layouts.OracleError, match="must fail rather than skip"):
        oracle_layouts.require_cobc()

    monkeypatch.delenv(oracle_layouts.REQUIRE_COBC_ENV)
    with pytest.raises(oracle_layouts.OracleError, match="not on PATH"):
        oracle_layouts.require_cobc()


@needs_cobc
def test_the_runner_cobc_matches_the_oracle_toolchain_recorded_in_the_seal(
    committed,
):
    """Acceptance gate ⑥, as a test as well as a CI step (T16)."""
    assert oracle_layouts.cobc_version() == committed["toolchain"]["cobc"]


# --- helpers ---------------------------------------------------------------


def _variant(document: dict, filename: str, index: int = 0) -> dict:
    book = next(b for b in document["copybooks"] if b["file"] == filename)
    return book["variants"][index]


def _fields(document: dict, filename: str, index: int = 0) -> dict:
    return {row["key"]: row for row in _variant(document, filename, index)["fields"]}
