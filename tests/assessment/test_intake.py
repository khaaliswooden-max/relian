"""WP-1.1 tests — written before intake.py existed."""

import json
from pathlib import Path

import pytest

from src.assessment.intake import ingest, build_inventory
from src.assessment.models import Measured, canonical_json

FIXTURES = Path(__file__).parent / "fixtures"


def _write_tree(root: Path) -> None:
    """Build a mixed-kind tree with deliberate LF/CRLF/mixed line endings.

    Built at runtime rather than committed so that git's line-ending handling
    can never silently change what the test is asserting about (R8).
    """
    (root / "src").mkdir(parents=True)
    (root / "jcl").mkdir()
    (root / "docs").mkdir()

    (root / "src" / "PAYROLL.cbl").write_bytes(
        b"       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PAYROLL.\n"
    )
    (root / "src" / "BENEFIT.COB").write_bytes(
        b"       IDENTIFICATION DIVISION.\r\n       PROGRAM-ID. BENEFIT.\r\n"
    )
    (root / "src" / "COMMON.cpy").write_bytes(b"       01 WS-SHARED PIC X(10).\n")
    (root / "src" / "NOEXT").write_bytes(
        b"       IDENTIFICATION DIVISION.\n       PROCEDURE DIVISION.\n"
    )
    (root / "src" / "MIXEND.cbl").write_bytes(
        b"       IDENTIFICATION DIVISION.\r\n       PROGRAM-ID. MIXEND.\n"
    )
    (root / "jcl" / "RUNJOB.jcl").write_bytes(b"//RUNJOB JOB (ACCT),'PAYROLL'\n")
    (root / "docs" / "notes.txt").write_bytes(b"not cobol\n")


@pytest.fixture()
def tree(tmp_path: Path) -> Path:
    root = tmp_path / "codebase"
    _write_tree(root)
    return root


def test_sorted_by_posix_path(tree: Path):
    records = ingest(tree)
    paths = [r.rel_path_posix for r in records]
    assert paths == sorted(paths)
    assert all("\\" not in p for p in paths)


def test_kind_classification(tree: Path):
    kinds = {r.rel_path_posix: r.kind for r in ingest(tree)}
    assert kinds["src/PAYROLL.cbl"] == "program"
    assert kinds["src/BENEFIT.COB"] == "program"
    assert kinds["src/COMMON.cpy"] == "copybook"
    assert kinds["src/NOEXT"] == "program"          # extensionless + division markers
    assert kinds["jcl/RUNJOB.jcl"] == "jcl"
    assert kinds["docs/notes.txt"] == "other"


def test_line_ending_detection(tree: Path):
    le = {r.rel_path_posix: r.line_ending for r in ingest(tree)}
    assert le["src/PAYROLL.cbl"] == "LF"
    assert le["src/BENEFIT.COB"] == "CRLF"
    assert le["src/MIXEND.cbl"] == "mixed"


def test_sha256_and_size_are_of_raw_bytes(tree: Path):
    import hashlib
    rec = {r.rel_path_posix: r for r in ingest(tree)}["src/PAYROLL.cbl"]
    raw = (tree / "src" / "PAYROLL.cbl").read_bytes()
    assert rec.sha256 == hashlib.sha256(raw).hexdigest()
    assert rec.size_bytes == len(raw)


def test_sha_and_manifest_hash_stable_across_runs(tree: Path):
    a = build_inventory(tree)
    b = build_inventory(tree)
    assert a.manifest_hash == b.manifest_hash
    assert canonical_json(a.to_dict()) == canonical_json(b.to_dict())


def test_manifest_hash_changes_with_content(tree: Path):
    before = build_inventory(tree).manifest_hash
    (tree / "src" / "PAYROLL.cbl").write_bytes(b"       IDENTIFICATION DIVISION.\n")
    assert build_inventory(tree).manifest_hash != before


def test_manifest_hash_is_sha256_of_canonical_manifest(tree: Path):
    import hashlib
    inv = build_inventory(tree)
    payload = canonical_json([r.to_dict() for r in inv.records])
    assert inv.manifest_hash == hashlib.sha256(payload.encode("utf-8")).hexdigest()


def test_manifest_hash_ignores_root_location(tmp_path: Path):
    """Two identical trees at different absolute paths hash identically."""
    a, b = tmp_path / "a", tmp_path / "b"
    _write_tree(a)
    _write_tree(b)
    assert build_inventory(a).manifest_hash == build_inventory(b).manifest_hash


def test_to_dict_is_json_serialisable_with_sorted_keys(tree: Path):
    d = build_inventory(tree).to_dict()
    text = canonical_json(d)
    assert json.loads(text) == d
    assert list(d.keys()) == sorted(d.keys())


def test_ingest_is_read_only(tree: Path):
    before = {p: p.read_bytes() for p in tree.rglob("*") if p.is_file()}
    ingest(tree)
    after = {p: p.read_bytes() for p in tree.rglob("*") if p.is_file()}
    assert before == after


def test_measured_requires_provenance_and_grade():
    with pytest.raises(TypeError):
        Measured(1.0)                       # type: ignore[call-arg]
    with pytest.raises(ValueError):
        Measured(1.0, "", "VERIFIED")
    with pytest.raises(ValueError):
        Measured(1.0, "counted", "PROBABLY")
    with pytest.raises(ValueError):
        Measured(None, "counted", "VERIFIED")   # type: ignore[arg-type]
    m = Measured(0.5, "3/6 via X", "VERIFIED")
    assert m.to_dict() == {"grade": "VERIFIED", "provenance": "3/6 via X", "value": 0.5}
