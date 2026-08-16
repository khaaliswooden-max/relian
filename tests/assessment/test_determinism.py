"""WP-1.8 — determinism (R8).

Three properties, each asserted end-to-end through the CLI:

1. Two runs over the same tree produce byte-identical JSON and the same
   ``report_hash``.
2. A CRLF copy of the same tree produces the same analysis as the LF original.
   Line-ending style must never move a coverage ratio or a LOC count, because
   it would otherwise move a price.
3. The result does not depend on where the tree lives on disk, nor on the order
   the filesystem happens to hand back directory entries.
"""

import json
import shutil
from pathlib import Path

import pytest

from src.assessment import report as report_mod
from src.assessment.cli import assess_tree, main

FIXTURES = Path(__file__).parent / "fixtures" / "cobol"


def _copy_tree(dst: Path, crlf: bool = False) -> Path:
    dst.mkdir(parents=True, exist_ok=True)
    for src_file in sorted(FIXTURES.iterdir()):
        if not src_file.is_file():
            continue
        raw = src_file.read_bytes()
        if crlf:
            raw = raw.replace(b"\r\n", b"\n").replace(b"\n", b"\r\n")
        (dst / src_file.name).write_bytes(raw)
    return dst


def _run(root: Path, out: Path) -> dict:
    assert main([str(root), "--out", str(out), "--quiet"]) == 0
    return {
        "json": (out / "assessment.json").read_bytes(),
        "hash": (out / "assessment.sha256").read_text().split()[0],
        "md": (out / "assessment.md").read_bytes(),
    }


def test_two_runs_are_byte_identical(tmp_path):
    tree = _copy_tree(tmp_path / "code")
    a = _run(tree, tmp_path / "out_a")
    b = _run(tree, tmp_path / "out_b")
    assert a["json"] == b["json"]
    assert a["hash"] == b["hash"]
    assert a["md"] == b["md"]


def test_report_hash_is_sha256_of_the_json_bytes(tmp_path):
    import hashlib
    tree = _copy_tree(tmp_path / "code")
    a = _run(tree, tmp_path / "out")
    assert a["hash"] == hashlib.sha256(a["json"]).hexdigest()


def test_crlf_copy_produces_the_same_analysis(tmp_path):
    lf = _copy_tree(tmp_path / "lf")
    crlf = _copy_tree(tmp_path / "crlf", crlf=True)

    lf_bundle, _ = assess_tree(lf)
    crlf_bundle, _ = assess_tree(crlf)

    def analysis_only(bundle):
        d = bundle.to_dict()
        # The manifest legitimately differs: CRLF files have different bytes,
        # so different sha256 and size. Everything derived from the *content*
        # must match.
        d.pop("inventory")
        d.pop("tool_versions")
        for p in d["programs"]:
            p.pop("file")
            p["coverage"]["coverage_ratio"] = _strip_sha(p["coverage"]["coverage_ratio"])
        d["portfolio_coverage"]["coverage_ratio"] = _strip_sha(
            d["portfolio_coverage"]["coverage_ratio"]
        )
        return d

    assert analysis_only(lf_bundle) == analysis_only(crlf_bundle)


def _strip_sha(measured):
    """Drop the file sha256 from a provenance string (it is byte-level, and
    CRLF changes bytes); keep the counts, method, and grade."""
    if measured is None:
        return None
    import re
    out = dict(measured)
    out["provenance"] = re.sub(r"sha256:[0-9a-f]+", "sha256:<content-dependent>",
                               out["provenance"])
    return out


def test_crlf_copy_yields_identical_loc_and_coverage_numbers(tmp_path):
    lf_bundle, _ = assess_tree(_copy_tree(tmp_path / "lf"))
    crlf_bundle, _ = assess_tree(_copy_tree(tmp_path / "crlf", crlf=True))
    for a, b in zip(lf_bundle.programs, crlf_bundle.programs):
        assert a.program_id == b.program_id
        assert a.loc.physical == b.loc.physical
        assert a.loc.logical == b.loc.logical
        assert a.loc.comment == b.loc.comment
        assert a.coverage.total_statements == b.coverage.total_statements
        assert a.coverage.supported_statements == b.coverage.supported_statements


def test_result_is_independent_of_where_the_tree_lives(tmp_path):
    a_bundle, _ = assess_tree(_copy_tree(tmp_path / "a" / "deeply" / "nested"))
    b_bundle, _ = assess_tree(_copy_tree(tmp_path / "b"))
    assert a_bundle.inventory.manifest_hash == b_bundle.inventory.manifest_hash


def test_records_are_sorted_by_posix_path(tmp_path):
    bundle, _ = assess_tree(_copy_tree(tmp_path / "code"))
    paths = [r.rel_path_posix for r in bundle.inventory.records]
    assert paths == sorted(paths)
    programs = [p.program_id for p in bundle.programs]
    assert programs == sorted(programs)


def test_json_is_canonical_and_reparses(tmp_path):
    tree = _copy_tree(tmp_path / "code")
    out = tmp_path / "out"
    _run(tree, out)
    text = (out / "assessment.json").read_text(encoding="utf-8")
    data = json.loads(text)
    assert list(data.keys()) == sorted(data.keys())
    assert ": " not in text[:200], "canonical JSON uses compact separators"


def test_json_only_skips_markdown_and_docx(tmp_path):
    tree = _copy_tree(tmp_path / "code")
    out = tmp_path / "out"
    assert main([str(tree), "--out", str(out), "--json-only", "--quiet"]) == 0
    assert (out / "assessment.json").exists()
    assert not (out / "assessment.md").exists()
    assert not (out / "assessment.docx").exists()


def test_docx_is_written_or_its_absence_is_explained(tmp_path):
    tree = _copy_tree(tmp_path / "code")
    bundle, by_construct = assess_tree(tree)
    paths = report_mod.render(bundle, tmp_path / "out", "code", by_construct)
    if paths["docx"] is None:
        assert paths["docx_skipped_reason"], "a missing DOCX must say why"
        assert not (tmp_path / "out" / "assessment.docx").exists(), (
            "no empty file may stand in for the real artifact"
        )
    else:
        assert Path(paths["docx"]).stat().st_size > 0


def test_cli_runs_on_a_tree_with_no_cobol_at_all(tmp_path):
    empty = tmp_path / "nothing"
    empty.mkdir()
    (empty / "README.md").write_text("no cobol here\n")
    bundle, _ = assess_tree(empty)
    assert bundle.programs == ()
    assert bundle.portfolio_coverage.coverage_ratio is None
    assert bundle.quotable_loc is None
    assert any("no files classified as COBOL programs" in n for n in bundle.notes)
    assert main([str(empty), "--out", str(tmp_path / "out"), "--quiet"]) == 0


def test_cli_accepts_a_single_file(tmp_path):
    bundle, _ = assess_tree(FIXTURES / "PARTIAL.cbl")
    assert [p.program_id for p in bundle.programs] == ["PARTIAL.cbl"]


def test_parse_errors_do_not_drift_between_parses_in_one_process():
    """ANTLR's prediction cache warms across parses and changes the
    `expecting {...}` set it prints. That must not reach the hashed ledger."""
    from src.assessment.coverage import analyze
    from src.assessment.intake import ingest, read_source

    record = {r.rel_path_posix: r for r in ingest(FIXTURES)}["PARTIAL.cbl"]
    source = read_source(FIXTURES / "PARTIAL.cbl")
    first = analyze(record, source).parser_errors
    second = analyze(record, source).parser_errors
    assert first == second
    assert first and all("expecting {...}" in e or "expecting" not in e for e in first)
