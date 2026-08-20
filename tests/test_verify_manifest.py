"""Teeth for tools/verify_manifest.py -- all three layers, proven independently.

RULE 4 COMPLIANCE. Nothing in this file writes to `bench/`. Every mutation is
performed on a `shutil.copytree` copy under pytest's `tmp_path`, which is
outside the repository; the real `bench/` is only ever READ. The two tests that
touch the real tree assert on it and do not modify it.

R4 COMPLIANCE. Two tests generate an ephemeral Ed25519 key with
`Ed25519PrivateKey.generate()` to simulate an attacker re-signing a forged
manifest. That key exists only in memory for the duration of the test, is never
written anywhere, and is not the benchmark signing key -- whose custody is the
operator's and which never enters this repository.
"""

import ast
import hashlib
import json
import shutil
from pathlib import Path

import pytest

from tools.verify_manifest import (
    compute_manifest_hash,
    compute_payload_sha256,
    main,
    verify,
)

REPO_ROOT = Path(__file__).resolve().parent.parent
BENCH = REPO_ROOT / "bench"
LEDGER_NAME = "LEDGER_relian-bench-v1.2.json"
LEDGER = BENCH / LEDGER_NAME

# The v1.2 include set, and the two categories of file the sealed manifest
# records but this repository deliberately does not contain:
#   * `**/vectors/heldout.jsonl` -- scoring-only, CI-only (rule 1 / R3)
#   * `harness/gen_vectors.py`   -- generator + seed regenerate the held-out
#                                   set, so it lives only in the private repo
#                                   (rule 6)
# Both are in .gitignore for exactly these reasons.
EXPECT_ABSENT = ("**/vectors/heldout.jsonl", "harness/gen_vectors.py")
INCLUDE_DIRS = ("corpus", "harness")
INCLUDE_FILES = ("SPEC.md",)

# Measured on the sealed v1.2 ledger, not assumed: 29 recorded entries, of
# which 21 exist in this perimeter and hash correctly and 8 are the private
# files above. Pinned so that a change to either number fails loudly.
V12_RECORDED = 29
V12_VERIFIABLE_HERE = 21
V12_DECLARED_ABSENT = 8


def run(root, ledger=None, **overrides):
    """Invoke the verifier with the v1.2 argument set, overridable per test."""
    kwargs = dict(
        ledger=ledger if ledger is not None else Path(root) / LEDGER_NAME,
        root=root,
        include_dirs=INCLUDE_DIRS,
        include_files=INCLUDE_FILES,
        expect_absent=EXPECT_ABSENT,
    )
    kwargs.update(overrides)
    return verify(**kwargs)


@pytest.fixture
def sandbox(tmp_path):
    """A throwaway copy of `bench/`. The real tree is never mutated."""
    dest = tmp_path / "bench"
    shutil.copytree(BENCH, dest)
    return dest


def load(sandbox):
    return json.loads((sandbox / LEDGER_NAME).read_text())


def save(sandbox, manifest):
    (sandbox / LEDGER_NAME).write_text(json.dumps(manifest, indent=2))


def resign(manifest):
    """Re-sign a forged manifest with a freshly generated attacker key.

    This is the failure mode a bare `verify()` cannot see: it trusts the public
    key embedded in the manifest it is checking, so an attacker who re-signs
    with their own key passes the signature check. Layers 1 and 2 are what
    survive that, which is the point these tests are making.
    """
    from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey
    from cryptography.hazmat.primitives import serialization

    priv = Ed25519PrivateKey.generate()  # ephemeral, in memory, never written
    pub = priv.public_key().public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw,
    )
    digest = compute_manifest_hash(manifest)
    manifest["signature"] = {
        "alg": "Ed25519",
        "manifest_sha256": digest,
        "signature_hex": priv.sign(digest.encode("utf-8")).hex(),
        "public_key_hex": pub.hex(),
        "key_fingerprint": hashlib.sha256(pub).hexdigest()[:16],
        "signed_at": manifest["signature"]["signed_at"],
    }
    return manifest


def corpus_file(sandbox):
    return sandbox / "corpus" / "P03_eligibility" / "program.cbl"


def flip_one_byte(path: Path):
    """Change exactly one byte. A one-byte edit is the whole threat model."""
    data = bytearray(path.read_bytes())
    data[0] = data[0] ^ 0x20
    path.write_bytes(bytes(data))


# --- The green path --------------------------------------------------------

def test_sealed_v12_ledger_verifies_on_all_three_layers():
    report = run(BENCH, ledger=LEDGER)
    assert report.ok, report.failed_layers()
    assert [layer.name for layer in report.layers] == ["tree", "payload", "signature"]
    assert all(layer.ok for layer in report.layers)


def test_v12_census_is_21_verified_8_declared_absent_0_mismatched():
    detail = run(BENCH, ledger=LEDGER).layer("tree").detail
    assert detail["recorded"] == V12_RECORDED
    assert detail["verified"] == V12_VERIFIABLE_HERE
    assert detail["declared_absent"] == V12_DECLARED_ABSENT
    assert detail["hash_mismatches"] == 0
    assert detail["missing"] == 0
    assert detail["unrecorded_on_disk"] == 0
    # The unverifiable eight are counted as absent, never folded into
    # `verified`. An unavailable file is not a checked one (R1).
    assert detail["verified"] + detail["declared_absent"] == detail["recorded"]


def test_pinning_the_real_signer_fingerprint_still_passes():
    report = run(BENCH, ledger=LEDGER, key_fingerprint="233bb4406e2de606")
    assert report.ok
    assert report.layer("signature").detail["signer_pinned"] is True


# --- Layer 1 (TREE) has teeth on its own -----------------------------------

def test_corpus_mutation_fails_the_tree_layer_and_only_that_layer(sandbox):
    """One byte changed under corpus/ -- the exact edit the signature misses."""
    target = corpus_file(sandbox)
    flip_one_byte(target)

    report = run(sandbox)
    assert not report.ok
    assert report.failed_layers() == ["tree"]
    # The other two still pass, which is precisely why they are not enough.
    assert report.layer("payload").ok
    assert report.layer("signature").ok

    detail = report.layer("tree").detail
    assert detail["hash_mismatches"] == 1
    assert detail["mismatched_paths"][0]["path"] == "corpus/P03_eligibility/program.cbl"
    assert (detail["mismatched_paths"][0]["actual"]
            != detail["mismatched_paths"][0]["expected"])


def test_file_added_to_the_tree_is_caught_even_though_every_hash_matches(sandbox):
    """Hash-checking only the recorded entries would not notice this."""
    (sandbox / "corpus" / "P03_eligibility" / "extra.cbl").write_text("       *> added\n")

    report = run(sandbox)
    assert not report.ok
    assert report.failed_layers() == ["tree"]
    detail = report.layer("tree").detail
    assert detail["hash_mismatches"] == 0          # every recorded file is fine
    assert detail["unrecorded_on_disk"] == 1
    assert detail["unrecorded_paths"] == ["corpus/P03_eligibility/extra.cbl"]


def test_deleted_recorded_file_is_caught(sandbox):
    corpus_file(sandbox).unlink()
    report = run(sandbox)
    assert not report.ok
    assert report.failed_layers() == ["tree"]
    assert report.layer("tree").detail["missing_paths"] == [
        "corpus/P03_eligibility/program.cbl"
    ]


def test_declared_absent_path_that_is_present_is_reported_not_hashed(sandbox):
    """A held-out vector file appearing here is a finding, not a green tick."""
    planted = sandbox / "corpus" / "P03_eligibility" / "vectors" / "heldout.jsonl"
    planted.write_text('{"not": "real"}\n')

    report = run(sandbox)
    assert not report.ok
    assert report.failed_layers() == ["tree"]
    detail = report.layer("tree").detail
    assert detail["declared_absent_but_present_paths"] == [
        "corpus/P03_eligibility/vectors/heldout.jsonl"
    ]
    # It is not counted as verified, and its content was never read.
    assert detail["verified"] == V12_VERIFIABLE_HERE


def test_file_count_disagreeing_with_files_length_fails_the_tree_layer(sandbox):
    manifest = load(sandbox)
    manifest["file_count"] = manifest["file_count"] + 1
    save(sandbox, resign(manifest))

    report = run(sandbox)
    assert not report.ok
    assert "file_count" in " ".join(report.layer("tree").problems)


# --- Layer 2 (PAYLOAD) has teeth on its own --------------------------------

def test_consistently_edited_files_entry_fails_only_the_payload_layer(sandbox):
    """The attacker does the job properly and still gets caught.

    Change a corpus byte, update that file's recorded sha256 so layer 1 is
    satisfied, then re-sign so layer 3 is satisfied. `payload_sha256` is the
    only thing left standing, and it is the reason it exists.
    """
    target = corpus_file(sandbox)
    flip_one_byte(target)
    new_digest = hashlib.sha256(target.read_bytes()).hexdigest()

    manifest = load(sandbox)
    for entry in manifest["files"]:
        if entry["path"] == "corpus/P03_eligibility/program.cbl":
            entry["sha256"] = new_digest
    save(sandbox, resign(manifest))   # payload_sha256 deliberately left stale

    report = run(sandbox)
    assert not report.ok
    assert report.failed_layers() == ["payload"]
    assert report.layer("tree").ok           # the forged hash satisfied layer 1
    assert report.layer("signature").ok      # the fresh key satisfied layer 3

    detail = report.layer("payload").detail
    assert detail["recomputed"] != detail["recorded"]


def test_payload_recomputation_agrees_with_the_sealed_value():
    """The independent reimplementation matches the sealer, without importing it."""
    manifest = json.loads(LEDGER.read_text())
    assert compute_payload_sha256(manifest["files"]) == manifest["payload_sha256"]


# --- Layer 3 (SIGNATURE) has teeth on its own ------------------------------

def test_signature_mutation_fails_only_the_signature_layer(sandbox):
    manifest = load(sandbox)
    original = manifest["signature"]["signature_hex"]
    # Flip one hex nibble. The signature stays well-formed and correctly sized.
    flipped = ("1" if original[0] != "1" else "2") + original[1:]
    manifest["signature"]["signature_hex"] = flipped
    save(sandbox, manifest)

    report = run(sandbox)
    assert not report.ok
    assert report.failed_layers() == ["signature"]
    assert report.layer("tree").ok
    assert report.layer("payload").ok
    assert report.layer("signature").detail["signature_valid"] is False


def test_manifest_body_edit_is_caught_by_the_signature_layer(sandbox):
    """Lowering a threshold is a body edit, not a files[] edit."""
    manifest = load(sandbox)
    manifest["thresholds"]["ber_heldout_min"] = 0.10
    save(sandbox, manifest)

    report = run(sandbox)
    assert not report.ok
    assert report.failed_layers() == ["signature"]
    detail = report.layer("signature").detail
    assert detail["recomputed_manifest_sha256"] != detail["recorded_manifest_sha256"]


def test_key_fingerprint_pin_rejects_an_otherwise_valid_resigned_manifest(sandbox):
    """Without a pin a re-signed forgery verifies. With one it does not."""
    manifest = load(sandbox)
    manifest["thresholds"]["ber_heldout_min"] = 0.10
    save(sandbox, resign(manifest))

    unpinned = run(sandbox)
    assert unpinned.layer("signature").ok          # self-consistent, self-signed
    assert unpinned.ok

    pinned = run(sandbox, key_fingerprint="233bb4406e2de606")
    assert not pinned.ok
    assert pinned.failed_layers() == ["signature"]
    assert "fingerprint" in " ".join(pinned.layer("signature").problems)


def test_manifest_hash_recomputation_agrees_with_the_sealed_value():
    manifest = json.loads(LEDGER.read_text())
    assert compute_manifest_hash(manifest) == manifest["signature"]["manifest_sha256"]


# --- The walk rules, which the manifest format does not record -------------

def test_excluded_build_artifacts_are_not_reported_as_unrecorded():
    """`payroll01` and `run` are committed COBOL binaries, excluded at sealing."""
    assert (BENCH / "corpus" / "P01_payroll" / "payroll01").is_file()
    assert (BENCH / "corpus" / "P02_interest" / "run").is_file()
    assert run(BENCH, ledger=LEDGER).layer("tree").detail["unrecorded_on_disk"] == 0


def test_dropping_the_binary_exclusion_surfaces_them_as_unrecorded():
    """Proves the exclusion is load-bearing and the reverse walk is really walking."""
    report = run(BENCH, ledger=LEDGER, exclude_basenames=())
    assert not report.ok
    detail = report.layer("tree").detail
    assert detail["unrecorded_on_disk"] == 5
    assert set(detail["unrecorded_paths"]) == {
        "corpus/P01_payroll/payroll01",
        "corpus/P02_interest/run",
        "corpus/P03_eligibility/run",
        "corpus/P04_taxtable/run",
        "corpus/P05_validate/run",
    }


# --- Refusing to certify what it could not measure (R1/R2) -----------------

def test_absent_ledger_is_fatal_and_never_a_pass(tmp_path):
    report = verify(ledger=tmp_path / "nope.json", root=BENCH)
    assert not report.ok
    assert "not found" in report.fatal


def test_unparseable_ledger_is_fatal_and_never_a_pass(tmp_path):
    broken = tmp_path / "broken.json"
    broken.write_text("{not json")
    report = verify(ledger=broken, root=BENCH)
    assert not report.ok
    assert "readable JSON" in report.fatal


def test_absent_root_is_fatal_and_never_a_pass(tmp_path):
    report = verify(ledger=LEDGER, root=tmp_path / "nowhere")
    assert not report.ok
    assert "not a directory" in report.fatal


# --- The command-line surface CI actually invokes --------------------------

def test_cli_exits_zero_on_the_sealed_ledger(capsys):
    code = main([
        "--ledger", str(LEDGER), "--root", str(BENCH),
        "--include-dirs", "corpus,harness", "--include-files", "SPEC.md",
        "--expect-absent", ",".join(EXPECT_ABSENT),
    ])
    assert code == 0
    assert "VERDICT: PASS (3/3 layers)" in capsys.readouterr().out


def test_cli_exits_one_and_names_the_failed_layer(sandbox, capsys):
    flip_one_byte(corpus_file(sandbox))
    code = main([
        "--ledger", str(sandbox / LEDGER_NAME), "--root", str(sandbox),
        "--include-dirs", "corpus,harness", "--include-files", "SPEC.md",
        "--expect-absent", ",".join(EXPECT_ABSENT),
    ])
    assert code == 1
    out = capsys.readouterr().out
    assert "VERDICT: FAIL" in out
    assert "failed: tree" in out
    assert "corpus/P03_eligibility/program.cbl" in out


def test_cli_json_output_is_machine_readable(capsys):
    code = main([
        "--ledger", str(LEDGER), "--root", str(BENCH),
        "--include-dirs", "corpus,harness", "--include-files", "SPEC.md",
        "--expect-absent", ",".join(EXPECT_ABSENT), "--json",
    ])
    assert code == 0
    payload = json.loads(capsys.readouterr().out)
    assert payload["ok"] is True
    assert [layer["name"] for layer in payload["layers"]] == [
        "tree", "payload", "signature"
    ]
    assert all(layer["ok"] for layer in payload["layers"])


# --- The verifier must not depend on the thing it verifies -----------------

def test_verifier_imports_nothing_from_the_repository():
    """A verifier that calls the sealer's code cannot detect the sealer's bugs.

    Checked against the parsed import statements rather than by grepping the
    source, so the module's PROSE may discuss `bench/harness/commit.py` -- it
    has to, that is where the walk rules are documented -- while its CODE
    stays free of it.
    """
    tree = ast.parse((REPO_ROOT / "tools" / "verify_manifest.py").read_text())
    imported = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            imported.update(alias.name.split(".")[0] for alias in node.names)
        elif isinstance(node, ast.ImportFrom):
            # A relative import would make it a package member, not standalone.
            assert not node.level, "verify_manifest.py uses a relative import"
            if node.module:
                imported.add(node.module.split(".")[0])

    allowed = {
        "__future__", "argparse", "dataclasses", "fnmatch", "hashlib", "json",
        "pathlib", "sys", "typing",   # standard library
        "cryptography",               # the one third-party dependency
    }
    assert imported <= allowed, f"unexpected imports: {sorted(imported - allowed)}"


def test_verifier_does_not_manipulate_sys_path():
    """`sys.path.insert(0, 'bench')` is how the other call sites reach the sealer."""
    tree = ast.parse((REPO_ROOT / "tools" / "verify_manifest.py").read_text())
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call) or not isinstance(node.func, ast.Attribute):
            continue
        target = node.func.value
        if (isinstance(target, ast.Attribute) and target.attr == "path"
                and isinstance(target.value, ast.Name) and target.value.id == "sys"):
            raise AssertionError(f"verify_manifest.py mutates sys.path "
                                 f"(sys.path.{node.func.attr})")
