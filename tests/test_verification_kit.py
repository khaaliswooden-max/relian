"""WP-2.7 acceptance (8) and (9) — the customer verification kit.

D40. We cannot get an IBM system, but every customer has one. So the kit is a
small COBOL program a customer compiles and runs on their own hardware against
their own copybook, emitting the engine's JSON schema.

**What (8) actually buys.** Proving the kit and the engine agree where we *can*
measure is the only evidence available that they will agree where we cannot. So
the kit is not merely compiled here — it is compiled with the **pinned**
``cobc 3.1.2.0``, run, and its output graded against the sealed oracle on the
same **186 comparisons** the engine itself is graded on: 170 probe rows plus 16
group lengths. A kit that covered fewer rows than the engine claims would agree
for the wrong reason.

**The pin matters.** WP-2.6.1's session installed GnuCOBOL 4.0 and watched two
``test_bench_oracle`` tests fail correctly; the tests were right and the
environment was wrong. So this file asserts the runner's ``cobc`` is the
version the oracle recorded before it grades anything against it.

**What (9) buys.** Anything a customer returns is a measurement of *their
compiler at their settings*. ``ingest_verification.py`` refuses to record a run
without both, and never generalises one to "IBM".
"""

from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))
KIT = REPO_ROOT / "discovery-verify"
if str(KIT) not in sys.path:
    sys.path.insert(0, str(KIT))

from src.discovery.layout import compute_text                            # noqa: E402
from tests.test_layout_roundtrip import CORPUS, EXPECTED_COMPARISONS, _variants, \
    trusted_oracle                                                        # noqa: E402

from gen_ibmlayout import (                                               # noqa: E402
    ColumnOverrun,
    KIT_SCHEMA,
    _Out,
    generate,
    probeable_fields,
)
from ingest_verification import (                                         # noqa: E402
    Provenance,
    ProvenanceMissing,
    Verdict,
    construct_key,
    ingest,
)

_REQUIRE_COBC_ENV = "RELIAN_REQUIRE_COBC"
_TIMEOUT_S = 180


def _cobc_or_skip() -> str:
    """The runner's ``cobc`` version line, or an honestly-labelled skip.

    ``RELIAN_REQUIRE_COBC=1`` turns the skip into a failure, matching
    ``oracle_layouts.require_cobc``: a kit that quietly did not compile is not
    a passing one, and CI pins the skip count so a runner that loses ``cobc``
    fails rather than going green.
    """
    if shutil.which("cobc") is None:
        message = "cobc is not on PATH"
        if os.environ.get(_REQUIRE_COBC_ENV) == "1":
            pytest.fail(f"{message} and {_REQUIRE_COBC_ENV}=1")
        pytest.skip(message)
    proc = subprocess.run(
        ["cobc", "--version"], capture_output=True, text=True, timeout=30
    )
    for stream in (proc.stdout, proc.stderr):
        for line in (stream or "").splitlines():
            if line.strip():
                return line.strip()
    pytest.fail("cobc --version produced no output")


def _compile_and_run(source: str, stem: str, workdir: Path) -> Dict[str, object]:
    cbl = workdir / f"{stem}.cbl"
    cbl.write_text(source, encoding="utf-8")
    exe = workdir / stem
    build = subprocess.run(
        ["cobc", "-x", str(cbl), "-o", str(exe)],
        capture_output=True, text=True, cwd=workdir, timeout=_TIMEOUT_S,
    )
    assert build.returncode == 0, (
        f"{stem} did not compile under the pinned cobc:\n{build.stderr}"
    )
    run = subprocess.run(
        [str(exe)], capture_output=True, text=True, cwd=workdir, timeout=_TIMEOUT_S,
    )
    assert run.returncode == 0, f"{stem} exited {run.returncode}:\n{run.stderr}"
    try:
        return json.loads(run.stdout)
    except json.JSONDecodeError as exc:
        raise AssertionError(
            f"{stem} did not emit valid JSON ({exc}). The kit's whole contract "
            f"is that it speaks the engine's schema.\n{run.stdout[:600]}"
        ) from exc


# --------------------------------------------------------------------------
# The environment is the one the oracle was measured on
# --------------------------------------------------------------------------

def test_the_runner_cobc_is_the_version_the_oracle_recorded() -> None:
    """The WP-2.6.1 trap, asserted before anything is graded.

    ``apt install gnucobol`` now resolves to a 4.x/5.x metapackage. The seal
    pins the PATCH level, so the package to install is ``gnucobol3``.
    """
    measured = _cobc_or_skip()
    recorded = trusted_oracle()["toolchain"]["cobc"]        # type: ignore[index]
    assert measured == recorded, (
        f"the runner's cobc is {measured!r} but the oracle was measured on "
        f"{recorded!r}. The kit must be graded on the pinned compiler; "
        f"install gnucobol3, not gnucobol."
    )


# --------------------------------------------------------------------------
# Acceptance (8) — compiles, runs, and round-trips 186/186
# --------------------------------------------------------------------------

def test_the_kit_round_trips_186_of_186_against_the_sealed_oracle(
    tmp_path: Path,
) -> None:
    """Acceptance (8). The same composition the engine is graded on."""
    _cobc_or_skip()
    compared = 0
    mismatches: List[str] = []

    for label, copybook, variant in _variants():
        text = (CORPUS / str(copybook["file"])).read_text(encoding="utf-8")
        odo = variant["odo_value"]
        layouts = compute_text(text, odo_value=odo)
        assert layouts, f"{label}: the engine produced no record"
        source = generate(layouts[0], copybook_text=text, odo_value=odo)
        stem = label.replace(".cpy", "").replace("@odo=", "_odo")
        document = _compile_and_run(source, stem, tmp_path)

        oracle_rows = {r["key"]: r for r in variant["fields"]}

        compared += 1
        group_row = oracle_rows.get(str(copybook["group"]))
        assert group_row is not None, f"{label}: oracle has no group row"
        if document["group_length"] != group_row["length"]:
            mismatches.append(
                f"{label}: group length kit={document['group_length']} "
                f"oracle={group_row['length']}"
            )

        for row in document["fields"]:
            compared += 1
            oracle_row = oracle_rows.get(row["key"])
            if oracle_row is None:
                mismatches.append(f"{label}: {row['key']} absent from the oracle")
                continue
            if (row["offset"], row["length"]) != (
                oracle_row["offset"], oracle_row["length"]
            ):
                mismatches.append(
                    f"{label}: {row['key']} kit="
                    f"({row['offset']},{row['length']}) oracle="
                    f"({oracle_row['offset']},{oracle_row['length']})"
                )

    assert compared == EXPECTED_COMPARISONS, (
        f"the kit produced {compared} comparisons, not {EXPECTED_COMPARISONS}. "
        f"A kit that covers fewer rows than the engine is graded on agrees for "
        f"the wrong reason."
    )
    assert mismatches == [], (
        f"the kit and the sealed oracle disagree on {len(mismatches)} of "
        f"{compared} comparisons. The kit is the only evidence that our model "
        f"and a real compiler agree, so a disagreement here is a finding, not "
        f"a tolerance."
        + "".join(f"\n  {m}" for m in mismatches[:20])
    )


def test_the_shipped_specimen_compiles_and_matches_the_generator(
    tmp_path: Path,
) -> None:
    """The committed ``IBMLAYOUT.cbl`` must not go stale.

    It is what a customer's team compiles first to check their toolchain, so a
    specimen that no longer matches the generator is a specimen that tests
    something we no longer ship.
    """
    _cobc_or_skip()
    sample = KIT / "SAMPLE.cpy"
    text = sample.read_text(encoding="utf-8")
    layouts = compute_text(text, origin=sample.as_posix())
    assert layouts
    regenerated = generate(layouts[0], copybook_text=text)
    shipped = (KIT / "IBMLAYOUT.cbl").read_text(encoding="utf-8")
    assert regenerated == shipped, (
        "discovery-verify/IBMLAYOUT.cbl is stale. Regenerate it:\n"
        "  python3 discovery-verify/gen_ibmlayout.py "
        "discovery-verify/SAMPLE.cpy --out discovery-verify/IBMLAYOUT.cbl"
    )
    document = _compile_and_run(shipped, "specimen", tmp_path)
    assert document["schema"] == KIT_SCHEMA
    assert document["reads_no_data"] is True
    assert document["record"] == "SAMPLE-REC"


def test_the_specimen_exercises_the_dialect_sensitive_construct() -> None:
    """A specimen that avoided the sensitive construct would prove nothing.

    ``SM-CTR-1 PIC S9(01) COMP`` is the field the whole work package is about:
    one byte under GnuCOBOL, a halfword under IBM's sourced rule.
    """
    text = (KIT / "SAMPLE.cpy").read_text(encoding="utf-8")
    layout = compute_text(text)[0]
    ctr = next(f for f in layout.fields if f.key == "SM-CTR-1")
    assert ctr.picture == "S9(01) COMP" or "S9(01)" in (ctr.picture or "")
    assert ctr.length == 1, "the specimen's sensitive field is not 1 byte here"


# --------------------------------------------------------------------------
# The kit reads no data, and fits Area B
# --------------------------------------------------------------------------

@pytest.mark.parametrize("name", sorted(p.name for p in CORPUS.glob("*.cpy")))
def test_the_generated_program_opens_no_file_and_reads_no_data(name: str) -> None:
    """D34/R12. The only thing that may leave the customer's perimeter is a
    description of their own copybook."""
    text = (CORPUS / name).read_text(encoding="utf-8")
    source = generate(compute_text(text)[0], copybook_text=text).upper()
    body = "\n".join(
        line for line in source.splitlines()
        if len(line) > 6 and line[6] != "*"
    )
    for forbidden in (
        " OPEN ", " READ ", " WRITE ", " REWRITE ", " DELETE ", " START ",
        "FILE SECTION", "FILE-CONTROL", " ACCEPT ", "CALL ",
    ):
        assert forbidden not in body, (
            f"{name}: the generated probe contains {forbidden!r}. It declares "
            f"a copybook and reports storage; it does not touch data."
        )


@pytest.mark.parametrize("name", sorted(p.name for p in CORPUS.glob("*.cpy")))
def test_no_generated_line_overruns_column_72(name: str) -> None:
    """Fixed-format Area B ends at column 72.

    A generator that overruns it produces "continuation character expected" on
    the customer's machine — a defect they cannot fix and we would never see.
    ``gen_probe`` measured this on ``D01-UNSIGNED-18`` at 73 characters, so the
    hazard is real rather than theoretical.
    """
    text = (CORPUS / name).read_text(encoding="utf-8")
    source = generate(compute_text(text)[0], copybook_text=text)
    for number, line in enumerate(source.splitlines(), start=1):
        # Lines from the customer's own copybook pass through unchanged; we do
        # not own that file. Everything this generator writes must fit.
        if line in text.splitlines():
            continue
        assert len(line) <= 72, f"{name}:{number} is {len(line)} columns: {line!r}"


def test_the_emitter_refuses_an_operand_that_cannot_fit() -> None:
    """Planted red: the column guard raises at generation, not at the
    customer's compiler."""
    out = _Out()
    with pytest.raises(ColumnOverrun):
        out.b("X" * 80)
    with pytest.raises(ColumnOverrun):
        out.display("'" + "Y" * 90 + "'")


def test_a_field_the_probe_cannot_mark_reports_null_and_never_zero() -> None:
    """D19/R1. A zero offset that looks like a measurement is worse than a
    null, because it survives every type check."""
    text = (CORPUS / "D12_edited.cpy").read_text(encoding="utf-8")
    source = generate(compute_text(text)[0], copybook_text=text)
    assert '"offset":null,' in source
    assert '"marked":false,' in source
    assert '"marked":true,' in source


def test_filler_is_not_probed() -> None:
    """FILLER cannot be the receiving item of a MOVE, and neither can the slack
    bytes SYNCHRONIZED inserts. Both are recovered downstream by subtraction
    and graded ``derived`` rather than ``measured``."""
    text = (CORPUS / "D10_sync.cpy").read_text(encoding="utf-8")
    fields = probeable_fields(compute_text(text)[0])
    assert fields, "nothing probeable at all"
    assert not any("FILLER" in name.upper() for _k, name, _s, _u in fields)


# --------------------------------------------------------------------------
# Acceptance (9) — confirm / contradict / unknown, with provenance
# --------------------------------------------------------------------------

@pytest.fixture(scope="module")
def sample_run(tmp_path_factory) -> Dict[str, object]:
    """A real kit run over the specimen, produced by the pinned cobc.

    Using a real run rather than a hand-written fixture matters: the ingest is
    being tested against the document the kit actually emits, not against our
    idea of it.
    """
    _cobc_or_skip()
    workdir = tmp_path_factory.mktemp("kitrun")
    text = (KIT / "SAMPLE.cpy").read_text(encoding="utf-8")
    source = generate(compute_text(text)[0], copybook_text=text)
    return _compile_and_run(source, "sample", workdir)


def _provenance() -> Provenance:
    return Provenance(
        compiler_version="GnuCOBOL 3.1.2.0 (stand-in for a returned run)",
        compiler_options="cobc -x, defaults",
        source_file="sample.json",
        source_sha256="0" * 64,
    )


def test_a_returned_run_without_provenance_is_refused() -> None:
    """Acceptance (9)'s planted red, both halves.

    A layout is a fact about a compiler AT SETTINGS. "Probably their production
    compiler" is not a provenance field, and a width recorded without one is
    indistinguishable from a width we invented (R9).
    """
    with pytest.raises(ProvenanceMissing, match="compiler_version"):
        Provenance("", "LP(64)", "f.json", "d")
    with pytest.raises(ProvenanceMissing, match="compiler_options"):
        Provenance("Enterprise COBOL 6.3", "   ", "f.json", "d")


def test_ingest_reports_confirm_contradict_and_unknown_per_construct(
    sample_run: Dict[str, object],
) -> None:
    """Acceptance (9).

    Feeding a **GnuCOBOL** run against the **IBM** projection is the sharpest
    available self-check: the two differ on exactly one sourced rule, so the
    ingest must contradict exactly ``COMP/1-2 digits`` and confirm the rest. If
    it contradicted more, the rule table would be wrong somewhere we think it
    is right.
    """
    text = (KIT / "SAMPLE.cpy").read_text(encoding="utf-8")
    result = ingest(sample_run, text, _provenance())

    contradicted = {
        v["construct"] for v in result["verdicts"]
        if v["verdict"] == Verdict.CONTRADICT.value
    }
    assert contradicted == {"COMP/1-2 digits"}, contradicted

    confirmed = {
        v["construct"] for v in result["verdicts"]
        if v["verdict"] == Verdict.CONFIRM.value
    }
    assert {"COMP/3-4 digits", "COMP/5-9 digits", "DISPLAY"} <= confirmed

    assert result["counts"][Verdict.CONTRADICT.value] == 1
    assert sum(result["counts"].values()) == len(result["verdicts"])


def test_every_construct_verdict_names_the_fields_it_covers(
    sample_run: Dict[str, object],
) -> None:
    """A verdict about a construct nobody can trace to fields is unauditable."""
    text = (KIT / "SAMPLE.cpy").read_text(encoding="utf-8")
    result = ingest(sample_run, text, _provenance())
    for verdict in result["verdicts"]:
        assert verdict["fields"], verdict
        assert verdict["detail"].strip()


def test_a_construct_with_several_widths_is_not_falsely_confirmed(
    sample_run: Dict[str, object],
) -> None:
    """``DISPLAY`` holds X(10), X(03), X(02), X(01) in the specimen.

    Reducing the construct to one projected number and one returned number
    collapses both to "several" and then compares them equal — a confirm on a
    construct nobody checked. The comparison happens per field for that reason.
    """
    text = (KIT / "SAMPLE.cpy").read_text(encoding="utf-8")
    result = ingest(sample_run, text, _provenance())
    display = next(v for v in result["verdicts"] if v["construct"] == "DISPLAY")
    assert display["verdict"] == Verdict.CONFIRM.value
    assert len(display["fields"]) >= 4
    assert display["projected_length"] is not None
    assert display["returned_length"] is not None


def test_a_contradiction_produces_a_rule_update_carrying_its_provenance(
    sample_run: Dict[str, object],
) -> None:
    """D40. A contradiction is a finding that updates the rule table — for
    THAT compiler at THOSE settings, and as a proposal a human applies."""
    text = (KIT / "SAMPLE.cpy").read_text(encoding="utf-8")
    result = ingest(sample_run, text, _provenance())

    updates = result["rule_updates"]
    assert len(updates) == 1, updates
    update = updates[0]
    assert update["construct"] == "COMP/1-2 digits"
    assert update["rule"] == "binary_width"
    assert (update["was_projected"], update["now_measured"]) == (2, 1)
    assert "PROPOSED" in update["status"]

    provenance = update["provenance"]
    assert provenance["compiler_version"].startswith("GnuCOBOL 3.1.2.0")
    assert provenance["compiler_options"] == "cobc -x, defaults"
    assert provenance["generalisable_to_ibm"] is False, (
        "a single returned run must never be generalised to IBM as a product"
    )


def test_a_missing_row_is_unknown_rather_than_a_contradiction(
    sample_run: Dict[str, object],
) -> None:
    """The realistic IBM case: reference modification is rejected for binary
    and packed fields, so those rows never come back. Absent evidence is
    UNKNOWN, not a contradiction — reporting it as a contradiction would
    manufacture a finding out of a compile error.
    """
    text = (KIT / "SAMPLE.cpy").read_text(encoding="utf-8")
    stripped = dict(sample_run)
    stripped["fields"] = [
        row for row in sample_run["fields"]        # type: ignore[union-attr]
        if "COMP" not in str(row.get("usage", ""))
    ]
    result = ingest(stripped, text, _provenance())
    binary = [
        v for v in result["verdicts"] if v["construct"].startswith("COMP")
    ]
    assert binary, "the specimen has no COMP constructs to drop"
    assert all(v["verdict"] == Verdict.UNKNOWN.value for v in binary), binary
    assert result["counts"][Verdict.CONTRADICT.value] == 0


def test_the_construct_key_groups_by_rule_not_by_field_name() -> None:
    """A verdict about ``WS-CTR`` is not reusable; a verdict about "COMP at
    1-2 digit positions" is a statement about a rule."""
    assert construct_key("S9(01) COMP", "COMP") == "COMP/1-2 digits"
    assert construct_key("S9(02) COMP", "COMP") == "COMP/1-2 digits"
    assert construct_key("S9(04) COMP", "COMP") == "COMP/3-4 digits"
    assert construct_key("S9(09) COMP", "COMP") == "COMP/5-9 digits"
    assert construct_key("S9(18) COMP", "COMP") == "COMP/10-18 digits"
    assert construct_key("X(10)", "DISPLAY") == "DISPLAY"


def test_an_unrecognised_schema_is_refused() -> None:
    """A returned file that does not say what produced it is not ingested."""
    text = (KIT / "SAMPLE.cpy").read_text(encoding="utf-8")
    with pytest.raises(ValueError, match="unrecognised schema"):
        ingest({"schema": "something-else", "fields": []}, text, _provenance())


# --------------------------------------------------------------------------
# Bugbot finding (High): the ingest missed name-keyed MAP rows
# --------------------------------------------------------------------------

def _normalised(rows: List[Dict[str, object]]) -> Dict[str, object]:
    from ingest_verification import NORMALISED_SCHEMA

    return {"schema": NORMALISED_SCHEMA, "fields": rows}


def test_a_name_keyed_map_row_matches_every_occurrence_of_the_member() -> None:
    """The realistic IBM path, and it was broken.

    ``IBMLAYOUT.cbl`` emits the engine's expanded ``key`` ("SM-ENTRY-CTR (1)");
    a MAP listing reports a table member ONCE, so the normalised form supplies
    a bare ``name``. Looking up only by key left every MAP row for an OCCURS
    member unmatched — and those are exactly the rows IBM forces down the MAP
    path, since a COMP item inside a table cannot be reference-modified there.
    A member width applies to every occurrence, so the name match is sound for
    the length comparison.
    """
    text = (KIT / "SAMPLE.cpy").read_text(encoding="utf-8")
    document = _normalised([
        {"name": "SM-NAME", "offset": 1, "length": 10},
        {"name": "SM-CTR-1", "offset": 11, "length": 2},
        {"name": "SM-CTR-4", "offset": 13, "length": 2},
        {"name": "SM-AMOUNT", "offset": 15, "length": 5},
        # ONE row for a member that occurs four times.
        {"name": "SM-ENTRY-CTR", "offset": 20, "length": 2},
        {"name": "SM-ENTRY-CODE", "offset": 22, "length": 3},
        {"name": "SM-SYNC-A", "offset": 40, "length": 4},
    ])
    result = ingest(document, text, _provenance())

    binary_1_2 = next(
        v for v in result["verdicts"] if v["construct"] == "COMP/1-2 digits"
    )
    assert binary_1_2["verdict"] == Verdict.CONFIRM.value, binary_1_2
    assert len(binary_1_2["fields"]) == 5, (
        "SM-CTR-1 plus four SM-ENTRY-CTR occurrences must all resolve from the "
        "single name-keyed MAP row"
    )
    assert binary_1_2["returned_length"] == 2
    assert result["counts"][Verdict.UNKNOWN.value] == 0, result["counts"]


def test_partial_coverage_confirms_what_returned_and_says_what_did_not() -> None:
    """A sibling row's absence must not erase the rows that did come back.

    Marking the whole construct UNKNOWN because one field of it was missing
    threw away real evidence: the rows that returned were measured on the
    customer's compiler and they confirmed the rule. Incomplete coverage is
    stated, not converted into ignorance.
    """
    text = (KIT / "SAMPLE.cpy").read_text(encoding="utf-8")
    document = _normalised([
        {"name": "SM-NAME", "offset": 1, "length": 10},
        # SM-ENTRY-CODE deliberately absent, so DISPLAY is partially covered.
    ])
    result = ingest(document, text, _provenance())

    display = next(v for v in result["verdicts"] if v["construct"] == "DISPLAY")
    assert display["verdict"] == Verdict.CONFIRM.value
    assert "neither confirmed nor contradicted" in display["detail"]
    assert display["returned_length"] == 10


def test_a_construct_with_no_returned_row_at_all_is_still_unknown() -> None:
    """The distinction the previous test rests on: SOME evidence confirms,
    NO evidence is unknown."""
    text = (KIT / "SAMPLE.cpy").read_text(encoding="utf-8")
    result = ingest(_normalised([
        {"name": "SM-NAME", "offset": 1, "length": 10},
    ]), text, _provenance())

    for construct in ("COMP/1-2 digits", "COMP/3-4 digits", "COMP-3/9 digits"):
        verdict = next(
            v for v in result["verdicts"] if v["construct"] == construct
        )
        assert verdict["verdict"] == Verdict.UNKNOWN.value, verdict
        assert "no row for any of" in verdict["detail"]
    assert result["counts"][Verdict.CONTRADICT.value] == 0


def test_a_map_row_that_disagrees_still_contradicts() -> None:
    """The name fallback must not soften a real disagreement."""
    text = (KIT / "SAMPLE.cpy").read_text(encoding="utf-8")
    result = ingest(_normalised([
        # IBM's sourced rule says a 1-digit COMP is a halfword; claim 1 byte.
        {"name": "SM-CTR-1", "offset": 11, "length": 1},
    ]), text, _provenance())

    verdict = next(
        v for v in result["verdicts"] if v["construct"] == "COMP/1-2 digits"
    )
    assert verdict["verdict"] == Verdict.CONTRADICT.value
    assert (verdict["projected_length"], verdict["returned_length"]) == (2, 1)
    assert len(result["rule_updates"]) == 1
