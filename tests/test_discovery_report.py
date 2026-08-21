"""WP-2.3 §3.1/§3.3 — the canonical report, its lints, and the leak test.

Five gates, and a proof that each has teeth:

1. **Determinism.** Two builds over the same tree emit byte-identical
   ``report.json``, and the canonical form is asserted on the EMITTED BYTES
   rather than on the dict. A dict that serialises correctly under one code
   path and not another is exactly the defect this assertion exists to see.
2. **R11 vocabulary.** Every term in :data:`FORBIDDEN_VOCABULARY` is its own
   negative control: planted into a template and into generated output, and
   the lint must catch it in both.
3. **R1/R9 grades.** Every quantitative field in the built object carries a
   value, a provenance and a real Trutina grade, or is absent. The gate is
   shown to bite on a planted bare number.
4. **R12 leak.** The countersignature request line is built from a report
   whose every free-text field and whose copybook names and paths are seeded
   with distinctive strings, and none of them appears in the line. A leak has
   to be a test failure, not something a reviewer notices.
5. **Section order and D26.** ``missing_copybooks`` leads the findings, and
   the order is carried by a list rather than by a dict, because canonical
   JSON sorts keys and a mapping cannot express an order at all.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from src.discovery import report as report_mod
from src.discovery.copybook import resolve
from src.discovery.layout import IBM_EQUIVALENCE_LIMITATION, compute
from src.discovery.report import (
    FORBIDDEN_VOCABULARY,
    SECTION_ORDER,
    Report,
    ReportError,
    ReportMeta,
    lint_measured_fields,
    lint_report_text,
    render_markdown,
)
from src.discovery.signing import build_manifest, countersign_request_line, sign_manifest

REPO_ROOT = Path(__file__).resolve().parents[1]
DEMO = REPO_ROOT / "examples" / "demo"

#: A fingerprint-shaped value. Not a real key; the report only prints it.
FAKE_FP = "0123456789abcdef"
FAKE_PUB = "ab" * 32


def _meta(**over) -> ReportMeta:
    base = dict(
        customer="Montana Department of Revenue",
        engagement="Legacy Data Discovery",
        contract_vehicle="MT-DOR-2026-DD-001",
        relian_version="0.1.0-test",
        root_label="examples/demo",
    )
    base.update(over)
    return ReportMeta(**base)


def _build(root: Path = DEMO, *, meta=None, layouts=None, skip=None) -> Report:
    resolution = resolve(root)
    if layouts is None:
        layouts = [
            layout
            for layout in (compute(name, resolution) for name in sorted(resolution.records))
            if layout is not None
        ]
    return Report.build(
        resolution,
        layouts,
        meta or _meta(),
        repo_root=REPO_ROOT,
        instance_fingerprint=FAKE_FP,
        instance_public_key_hex=FAKE_PUB,
        layouts_skipped_reason=skip,
    )


@pytest.fixture(scope="module")
def built() -> Report:
    return _build()


# --------------------------------------------------------------------------
# 1 — determinism and canonical form, asserted on the emitted bytes
# --------------------------------------------------------------------------


def test_two_builds_emit_byte_identical_report_json() -> None:
    """Acceptance ①. Same tree, same version, same bytes."""
    assert _build().to_canonical_json() == _build().to_canonical_json()


def test_the_emitted_bytes_are_sorted_and_separator_canonical(built: Report) -> None:
    """Asserted on the BYTES. A dict compares equal under any separator."""
    blob = built.to_canonical_json()
    text = blob.decode("utf-8")
    assert text.endswith("\n"), "report.json must end with exactly one newline"
    assert not text[:-1].endswith("\n"), "more than one trailing newline"
    body = text[:-1]
    parsed = json.loads(body)
    assert body == json.dumps(
        parsed, sort_keys=True, separators=(",", ":"), ensure_ascii=False
    ), "emitted bytes are not the canonical form"
    # The converse control: the default serialisation is NOT what was emitted,
    # so the assertion above is comparing against something.
    assert body != json.dumps(parsed), "canonical and default forms coincide here"


def test_the_bytes_are_valid_utf8_and_reparse_to_the_same_object(built: Report) -> None:
    assert json.loads(built.to_canonical_json().decode("utf-8")) == built.to_dict()


def test_report_json_contains_no_timestamp(built: Report) -> None:
    """The one ``generated_at`` lives in the manifest, which is where the
    signature is taken. A timestamp here would break ① on its own."""
    text = built.to_canonical_json().decode("utf-8")
    for marker in ("generated_at", "signed_at", "created_at", "timestamp"):
        assert marker not in text, f"report.json carries a {marker} field"


def test_report_id_is_content_derived_not_random() -> None:
    """Two builds agree; a changed engagement produces a different id."""
    assert _build().report_id == _build().report_id
    other = _build(meta=_meta(customer="A Different Agency"))
    assert other.report_id != _build().report_id


# --------------------------------------------------------------------------
# 2 — R11 vocabulary, each term its own negative control
# --------------------------------------------------------------------------


def test_generated_report_json_is_free_of_forbidden_vocabulary(built: Report) -> None:
    found = lint_report_text(built.to_canonical_json().decode("utf-8"))
    assert found == [], f"report.json contains vocabulary R11 forbids: {found}"


def test_generated_markdown_is_free_of_forbidden_vocabulary(built: Report) -> None:
    found = lint_report_text(render_markdown(built))
    assert found == [], f"report.md contains vocabulary R11 forbids: {found}"


def test_every_template_is_free_of_forbidden_vocabulary() -> None:
    for name, text in sorted(report_mod.TEMPLATES.items()):
        assert lint_report_text(text) == [], f"template {name} carries a forbidden term"


@pytest.mark.parametrize("term", FORBIDDEN_VOCABULARY)
def test_the_lint_catches_each_forbidden_term_planted_in_a_template(term: str) -> None:
    """Each term is its own negative control (acceptance ⑥). A vocabulary
    where only one entry has ever been exercised is a vocabulary with one
    tested entry."""
    planted = f"This engagement uses {term} to reach its conclusion.\n"
    found = lint_report_text(planted)
    assert any(hit == term for hit, _excerpt in found), (
        f"the lint did not catch a planted {term!r}"
    )


@pytest.mark.parametrize("term", FORBIDDEN_VOCABULARY)
def test_the_lint_catches_each_forbidden_term_in_any_case(term: str) -> None:
    found = lint_report_text(f"PREFIX {term.upper()} SUFFIX")
    assert any(hit == term for hit, _excerpt in found), (
        f"the lint is case-sensitive and missed {term.upper()!r}"
    )


def test_the_lint_does_not_fire_on_ordinary_report_prose() -> None:
    """The converse control. A lint that flagged everything would pass every
    negative control above and tell you nothing."""
    assert lint_report_text(
        "Every count below was measured in this run and carries its grade."
    ) == []


def test_no_risk_score_of_any_kind_reaches_the_report(built: Report) -> None:
    """Exhibit D's ML gap is not repaired here and must not be implied."""
    text = built.to_canonical_json().decode("utf-8").lower()
    assert "risk_score" not in text
    assert "risk score" not in text.replace(
        "no risk score of any kind is computed", ""
    )


# --------------------------------------------------------------------------
# 3 — R1/R9: no number without a grade, no 0 standing in for a measurement
# --------------------------------------------------------------------------


def test_every_quantitative_field_carries_a_grade_and_a_basis(built: Report) -> None:
    """Acceptance ⑦."""
    assert lint_measured_fields(built.to_dict()) == []


def test_the_grade_gate_catches_a_planted_bare_number(built: Report) -> None:
    planted = built.to_dict()
    planted["sections"] = list(planted["sections"]) + [
        {"id": "planted", "title": "Planted", "throughput": 42}
    ]
    findings = lint_measured_fields(planted)
    assert any("42" in f for f in findings), (
        "the R1 gate did not notice a bare number with no provenance"
    )


def test_the_grade_gate_catches_a_measured_with_no_provenance() -> None:
    planted = {"count": {"value": 7, "provenance": "", "grade": "VERIFIED"}}
    assert any("no provenance" in f for f in lint_measured_fields(planted))


def test_the_grade_gate_catches_a_measured_with_an_invented_grade() -> None:
    planted = {"count": {"value": 7, "provenance": "counted", "grade": "PROBABLY"}}
    assert any("PROBABLY" in f for f in lint_measured_fields(planted))


def test_every_grade_in_the_report_is_a_real_trutina_grade(built: Report) -> None:
    text = built.to_canonical_json().decode("utf-8")
    for grade in json.loads(text) and _grades(json.loads(text)):
        assert grade in ("VERIFIED", "PLAUSIBLE", "SPECULATIVE"), grade


def _grades(node, out=None):
    out = [] if out is None else out
    if isinstance(node, dict):
        if {"value", "provenance", "grade"} <= set(node):
            out.append(node["grade"])
        for v in node.values():
            _grades(v, out)
    elif isinstance(node, list):
        for v in node:
            _grades(v, out)
    return out


def test_max_fan_out_is_absent_rather_than_zero_when_there_is_no_edge(tmp_path) -> None:
    """D19 again, on a new surface: 'the maximum fan-out is zero' and 'nothing
    references anything' are different sentences, and only one is true."""
    (tmp_path / "EMPTY.cpy").write_text("       01  A PIC X.\n", encoding="utf-8")
    built = _build(tmp_path, meta=_meta(root_label="empty"), layouts=[])
    assert built.section("scope")["max_fan_out"] is None


# --------------------------------------------------------------------------
# 4 — R12: the request line leaks nothing (acceptance ②)
# --------------------------------------------------------------------------

#: Distinctive strings seeded into every text-carrying surface of a report.
#: If any reaches the request line, R12 has failed and the design is wrong.
SEEDS = (
    "ZZQCUSTOMERZZQ",
    "ZZQENGAGEMENTZZQ",
    "ZZQVEHICLEZZQ",
    "ZZQROOTLABELZZQ",
    "ZZQFIELDNAMEZZQ",
    "ZZQPATHFRAGMENTZZQ",
)


@pytest.fixture(scope="module")
def seeded(tmp_path_factory):
    """A report built from a tree whose paths, copybook names and field names
    all carry distinctive strings, plus its signed manifest."""
    root = tmp_path_factory.mktemp("seeded") / "tree"
    (root / "ZZQPATHFRAGMENTZZQ").mkdir(parents=True)
    (root / "ZZQPATHFRAGMENTZZQ" / "ZZQFIELDNAMEZZQ.cpy").write_text(
        "       01  ZZQFIELDNAMEZZQ-REC.\n"
        "           05  ZZQFIELDNAMEZZQ-A PIC X(10).\n",
        encoding="utf-8",
    )
    (root / "ZZQPATHFRAGMENTZZQ" / "PROG.cbl").write_text(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. PROG.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           COPY ZZQFIELDNAMEZZQ.\n"
        "           COPY ZZQMISSINGZZQ.\n",
        encoding="utf-8",
    )
    meta = _meta(
        customer="ZZQCUSTOMERZZQ",
        engagement="ZZQENGAGEMENTZZQ",
        contract_vehicle="ZZQVEHICLEZZQ",
        root_label="ZZQROOTLABELZZQ",
    )
    built = _build(root, meta=meta)
    manifest = build_manifest(
        built,
        files=[("report.json", built.to_canonical_json())],
        tool_digests=report_mod.tool_digests(REPO_ROOT),
        input_digest_rows=[],
        instance=_FakeKey(),
        relian_version="0.1.0-test",
        python_version="3.11.0",
    )
    sign_manifest(manifest, _FakeKey())
    return built, manifest


class _FakeKey:
    """An in-memory Ed25519 key, so the leak test needs no filesystem."""

    def __init__(self) -> None:
        self._private = _SHARED_KEY
        self.public_key_hex = _SHARED_PUB
        self.fingerprint = _SHARED_FP
        self.path = "<in-memory>"
        self.created_at = "1970-01-01T00:00:00+00:00"
        self.generated_this_run = False

    def sign(self, payload: bytes) -> str:
        return self._private.sign(payload).hex()

    def record(self):
        return {
            "algorithm": "Ed25519",
            "public_key_hex": self.public_key_hex,
            "fingerprint": self.fingerprint,
            "path": self.path,
            "created_at": self.created_at,
            "generated_this_run": self.generated_this_run,
            "origin": "test key",
        }


def _make_shared():
    import hashlib

    from cryptography.hazmat.primitives import serialization
    from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey

    private = Ed25519PrivateKey.generate()
    raw = private.public_key().public_bytes(
        encoding=serialization.Encoding.Raw, format=serialization.PublicFormat.Raw
    )
    return private, raw.hex(), hashlib.sha256(raw).hexdigest()[:16]


_SHARED_KEY, _SHARED_PUB, _SHARED_FP = _make_shared()


def test_the_seeded_fixture_actually_carries_the_seeds(seeded) -> None:
    """Without this, the leak test could pass because the seeds were never in
    the report to begin with — a control that proves nothing."""
    built, _manifest = seeded
    text = built.to_canonical_json().decode("utf-8")
    missing = [s for s in SEEDS if s not in text]
    assert missing == [], f"the fixture never carried {missing}, so ② is vacuous"


@pytest.mark.parametrize("seed", SEEDS)
def test_the_request_line_carries_no_customer_string(seeded, seed: str) -> None:
    """Acceptance ②, one assertion per seeded string."""
    _built, manifest = seeded
    line = countersign_request_line(manifest)
    assert seed not in line, f"the request line leaked {seed!r} across the perimeter"


def test_the_request_line_is_three_hexadecimal_fields_and_nothing_else(seeded) -> None:
    _built, manifest = seeded
    line = countersign_request_line(manifest)
    prefix, *fields = line.split(" ")
    assert prefix == "relian-countersign-request/1"
    assert len(fields) == 3, fields
    names = []
    for field in fields:
        name, _, value = field.partition("=")
        names.append(name)
        assert value and all(ch in "0123456789abcdef" for ch in value.lower()), field
    assert names == ["manifest_sha256", "report_id", "instance_fingerprint"]


def test_the_request_line_is_one_line(seeded) -> None:
    _built, manifest = seeded
    assert "\n" not in countersign_request_line(manifest)


def test_a_non_hexadecimal_field_is_refused_rather_than_emitted(seeded) -> None:
    """The guard, not just the shape. If a future field ever carried text, the
    line must not be emitted at all — that is R12 failing and it escalates."""
    from src.discovery.signing import SigningError

    _built, manifest = seeded
    tampered = json.loads(json.dumps(manifest))
    tampered["report_id"] = "ZZQCUSTOMERZZQ"
    with pytest.raises(SigningError, match="not hexadecimal"):
        countersign_request_line(tampered)


# --------------------------------------------------------------------------
# 5 — section order, D26, and the top-matter limitation
# --------------------------------------------------------------------------


def test_sections_are_an_ordered_list_not_a_mapping(built: Report) -> None:
    payload = json.loads(built.to_canonical_json().decode("utf-8"))
    assert isinstance(payload["sections"], list)
    assert [s["id"] for s in payload["sections"]] == list(SECTION_ORDER)


def test_missing_copybooks_leads_the_findings(built: Report) -> None:
    """D26. It comes after identification and scope and before everything
    else, because it is the finding a customer cannot get from their own
    team."""
    ids = [s["id"] for s in built.sections]
    assert ids.index("missing_copybooks") == 2
    assert ids.index("missing_copybooks") < ids.index("record_layouts")
    assert ids.index("missing_copybooks") < ids.index("fan_in")


def test_the_ibm_limitation_is_in_the_top_matter_not_only_on_records(built: Report) -> None:
    """WP-2.2's limitation travels into the report's top matter. A limitation
    a reader scrolls past is not disclosed."""
    ident = built.section("identification")
    assert ident["top_matter_limitation"] == IBM_EQUIVALENCE_LIMITATION
    markdown = render_markdown(built)
    head = markdown[: markdown.index("## 2.")]
    assert "IBM Enterprise COBOL is UNMEASURED" in head


def test_the_markdown_names_the_signed_artifact_and_the_verifier(built: Report) -> None:
    markdown = render_markdown(built)
    assert "derived and unsigned" in markdown
    assert "`report.json`" in markdown
    assert "tools/verify_report.py" in markdown


def test_the_markdown_says_the_json_wins_a_disagreement(built: Report) -> None:
    assert "is authoritative and this rendering is the defect" in render_markdown(built)


def test_the_instance_layer_is_never_described_as_a_visionblox_signature(
    built: Report,
) -> None:
    """Escalation trigger 4. Nothing in either artifact may be read as
    *Visionblox signed this* when only the instance layer is present."""
    for text in (built.to_canonical_json().decode("utf-8"), render_markdown(built)):
        assert "does NOT prove identity of signer" in text
        assert "VALID AND UNATTESTED" in text
        assert "Visionblox has never held" in text


def test_an_empty_layout_section_must_state_why_it_is_empty() -> None:
    """R2. An empty section with no reason reads as a clean result."""
    with pytest.raises(ReportError, match="does not say why it is empty"):
        _build(layouts=[])


def test_an_empty_layout_section_with_a_reason_is_accepted_and_records_it() -> None:
    built = _build(layouts=[], skip="resolver-only run; see WP-2.2 §10")
    section = built.section("record_layouts")
    assert section["included"] is False
    assert "WP-2.2" in section["reason_not_included"]


def test_the_missing_table_does_not_guess_where_a_member_came_from(built: Report) -> None:
    """The referrer list is measured; the origin of a member never seen is
    not, and an inference printed beside measurements would read as one."""
    section = built.section("missing_copybooks")
    for row in section["rows"]:
        assert "likely_source" not in row
    assert "does not guess" in section["note"]


# --------------------------------------------------------------------------
# 6 — the R1 gate over EVERY construct the sealed bench covers
# --------------------------------------------------------------------------

BENCH_CORPUS = REPO_ROOT / "discovery-bench" / "corpus"


@pytest.fixture(scope="module")
def bench_report() -> Report:
    """A report over the sealed 12-axis corpus. READ ONLY — nothing under
    ``discovery-bench/`` is written, and rule 4 forbids it in any case."""
    return _build(BENCH_CORPUS, meta=_meta(root_label="discovery-bench/corpus"))


def test_the_bench_fixture_exercises_more_than_the_demo_tree(bench_report) -> None:
    """Without this, the two tests below could pass over three flat copybooks
    and prove nothing about OCCURS, ODO, REDEFINES or SYNC."""
    records = bench_report.section("record_layouts")["records"]
    assert len(records) >= 12, f"only {len(records)} records in the bench fixture"
    assert any(
        f["subscripts"] for r in records for f in r["fields"]
    ), "no OCCURS subscript in the fixture, so the case that caught the bug is absent"
    assert any(r["odo_object"] for r in records), "no ODO record in the fixture"
    assert any(f["redefines"] for r in records for f in r["fields"]), "no REDEFINES"


def test_the_r1_gate_is_clean_over_the_sealed_twelve_axis_corpus(bench_report) -> None:
    """REGRESSION. The first run of this gate over this corpus refused to
    write a report, naming every OCCURS index vector as an ungraded number —
    a subscript is a structural coordinate, not a measurement. ``examples/demo``
    has no OCCURS, so a fixture drawn only from there would have shipped that
    refusal to the first customer with a table in a copybook."""
    assert lint_measured_fields(bench_report.to_dict()) == []


def test_the_forbidden_vocabulary_is_clean_over_the_sealed_corpus(bench_report) -> None:
    assert lint_report_text(bench_report.to_canonical_json().decode("utf-8")) == []
    assert lint_report_text(render_markdown(bench_report)) == []


def test_building_over_the_bench_corpus_writes_nothing_into_it() -> None:
    """Rule 4. The engine reads the corpus; it never writes to the tree it
    reads, and this asserts the digests are unchanged after a full build."""
    before = {
        p.as_posix(): p.stat().st_mtime_ns
        for p in sorted(BENCH_CORPUS.rglob("*")) if p.is_file()
    }
    _build(BENCH_CORPUS, meta=_meta(root_label="discovery-bench/corpus"))
    after = {
        p.as_posix(): p.stat().st_mtime_ns
        for p in sorted(BENCH_CORPUS.rglob("*")) if p.is_file()
    }
    assert before == after and before, "the build touched discovery-bench/corpus"
