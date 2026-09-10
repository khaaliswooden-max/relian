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
LEDGER_NAME = "LEDGER_relian-bench-v1.3.json"
LEDGER = BENCH / LEDGER_NAME

#: v1.2 stays in the repository as the R7 PROVENANCE ANCHOR. Its `committed_at`
#: predates the grammar merge, and it is the only ledger carrying the held-out
#: vector census: v1.3 records no `vector_counts`, because counting them means
#: reading `heldout.jsonl`, which rule 1 forbids. v1.3 supersedes v1.2 for TREE
#: INTEGRITY ONLY, which is why both are read here and neither is deleted.
V12_LEDGER_NAME = "LEDGER_relian-bench-v1.2.json"
V12_LEDGER = BENCH / V12_LEDGER_NAME

# The include set, and the two categories of file the sealed manifest
# records but this repository deliberately does not contain:
#   * `**/vectors/heldout.jsonl` -- scoring-only, CI-only (rule 1 / R3)
#   * `harness/gen_vectors.py`   -- generator + seed regenerate the held-out
#                                   set, so it lives only in the private repo
#                                   (rule 6)
# Both are in .gitignore for exactly these reasons.
EXPECT_ABSENT = ("**/vectors/heldout.jsonl", "harness/gen_vectors.py")
INCLUDE_DIRS = ("corpus", "harness")
INCLUDE_FILES = ("SPEC.md",)

# Measured on the sealed v1.3 ledger, not assumed: 29 recorded entries, of
# which 21 exist in this perimeter and 8 are the private files above. The
# re-seal moved one recorded HASH, not the file set, so v1.3's census is v1.2's
# -- these are the benchmark's numbers rather than any one ledger's. Pinned so
# that a change to any of them fails loudly.
RECORDED = 29
PRESENT_HERE = 21
DECLARED_ABSENT = 8

#: The sandbox is a whole copy of `bench/`, so post-re-seal its counts ARE the
#: real ones: nothing is excluded from it any more. Kept as separate names
#: because the sandbox tests assert against a deliberately MUTATED tree, and
#: that role should not silently share a constant with the real-tree census.
SANDBOX_VERIFIED = PRESENT_HERE
SANDBOX_DECLARED_ABSENT = DECLARED_ABSENT

# --- WP-2.6, reconciled 2026-09-10 -----------------------------------------
#
# `153f40f` removed `sign()`'s silent-keygen fallback from
# `bench/harness/commit.py`, a file inside the include set of the seal it
# produces. The tree stopped matching v1.2 the moment it was edited, and for
# the interval between that edit and the ceremony this file carried scaffolding
# -- an excluded path, a repaired sandbox baseline, and a skipif on the v1.3
# ledger's existence -- so that the twelve layer-isolation proofs below could
# still say "this mutation breaks the payload layer AND ONLY the payload layer"
# against a baseline that matched its own ledger.
#
# The v1.3 ceremony ran. The sandbox now matches its ledger with no exclusion,
# which is what the re-seal was for, so all of that scaffolding is deleted
# rather than commented out. The skipif is deleted with it: the ledger exists,
# so a gate that can no longer fire is a control that has stopped controlling.

def run(root, ledger=None, **overrides):
    """Invoke the verifier with the real argument set, overridable per test.

    `expect_absent` is the real declared-absent set -- the held-out vectors and
    the generator. There is no longer an excluded path on top of it: v1.3
    records the tree as it stands, so the sandbox copy matches its own ledger.
    """
    kwargs = dict(
        ledger=ledger if ledger is not None else Path(root) / LEDGER_NAME,
        root=root,
        include_dirs=INCLUDE_DIRS,
        include_files=INCLUDE_FILES,
        expect_absent=EXPECT_ABSENT,
    )
    kwargs.update(overrides)
    return verify(**kwargs)


def run_real(root=BENCH, ledger=None, **overrides):
    """Verify the REAL tree, with the real declared-absent set and no exclusion."""
    return run(
        root,
        ledger=ledger if ledger is not None else LEDGER,
        expect_absent=EXPECT_ABSENT,
        **overrides,
    )


@pytest.fixture
def sandbox(tmp_path):
    """A throwaway copy of `bench/`. The real tree is never mutated (rule 4).

    An unmodified copy verifies clean against its own ledger, which is the
    property every layer-isolation test below depends on: a mutation can only
    be blamed for the layer it breaks if nothing else was already broken.
    """
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


# --- The measured state of the real tree against the v1.2 seal -------------
#
# This section used to assert a clean three-layer pass. It cannot, and the
# honest thing is to assert what is actually true rather than skip and look
# green: the tree layer fails on exactly one file, and the other two still
# pass. That is the WP-2.6 finding, and pinning it turns the incident into a
# regression test.


def test_the_v1_2_anchor_manifest_is_still_intact():
    """v1.2 is retained under R7, so its CUSTODY CHAIN is still asserted.

    Its tree layer no longer matches this repository and is not expected to:
    `153f40f` edited `harness/commit.py`, and v1.3 -- not a re-seal of v1.2 --
    is what records the tree as it now stands. What must remain true of the
    anchor is that the anchor itself was never edited, so the payload and
    signature layers are asserted here and the tree layer deliberately is not.

    A failing payload or signature layer on v1.2 would mean the MANIFEST moved,
    which is a tamper finding rather than superseded provenance.
    """
    report = run_real(ledger=V12_LEDGER)
    assert report.layer("payload").ok
    assert report.layer("signature").ok
    assert report.layer("signature").detail["signature_valid"] is True


def test_the_two_ledgers_are_signed_by_the_same_custodian():
    """One custody chain across the re-seal (R4), DERIVED rather than read.

    `key_fingerprint` is not what this trusts. `manifest_hash()` covers
    `manifest minus signature`, so the whole signature block -- that field
    included -- is UNSIGNED: an attacker who re-signs a forged ledger with
    their own key can leave `key_fingerprint` reading 233bb4406e2de606 and a
    string comparison accepts it. bench.yml derives the signer from
    `public_key_hex` for exactly this reason, and so does this test.

    The declared field is then cross-checked against the derived one, which is
    different from being trusted: a manifest disagreeing with its own key is a
    finding rather than a curiosity.
    """
    for path in (V12_LEDGER, LEDGER):
        signature = json.loads(path.read_text())["signature"]
        derived = hashlib.sha256(
            bytes.fromhex(signature["public_key_hex"])
        ).hexdigest()[:16]
        assert derived == "233bb4406e2de606", (
            f"{path.name} was signed by {derived}, not the published "
            f"RELIAN-BENCH key -- the re-seal changed custodian"
        )
        assert signature["key_fingerprint"] == derived, (
            f"{path.name} declares a key_fingerprint its public_key_hex does "
            f"not hash to"
        )


def test_census_accounts_for_all_29_recorded_entries():
    """Nothing appeared, nothing vanished: 21 verified + 0 moved + 8 absent."""
    detail = run_real().layer("tree").detail
    assert detail["recorded"] == RECORDED
    assert detail["declared_absent"] == DECLARED_ABSENT
    assert (
        detail["verified"] + detail["hash_mismatches"] + detail["declared_absent"]
        == detail["recorded"]
    )
    # The unverifiable eight are counted as absent, never folded into
    # `verified`. An unavailable file is not a checked one (R1).
    assert detail["verified"] == PRESENT_HERE


def test_pinning_the_real_signer_fingerprint_still_passes():
    """The custody pin holds on the real ledger, and nothing else is red.

    Before the ceremony this asserted `failed_layers() == ["tree"]` -- the pin
    passing while the tree was stale. Post-re-seal the whole report is green,
    so the assertion is the stronger one: pinning the real fingerprint does not
    cost a layer.
    """
    report = run_real(key_fingerprint="233bb4406e2de606")
    signature = report.layer("signature")
    assert signature.ok
    assert signature.detail["signer_pinned"] is True
    assert report.failed_layers() == []
    assert report.ok


def test_the_v1_3_seal_verifies_the_tree_on_all_three_layers():
    """The post-ceremony green path, now enforcing rather than gated.

    This was skipped on the existence of the v1.3 ledger until the ceremony
    ran. The ledger exists, so the gate is gone and this is the primary claim
    the bench-seal job makes: the tree matches its seal on all three layers.
    """
    report = verify(
        ledger=LEDGER,
        root=BENCH,
        from_manifest=True,
        key_fingerprint="233bb4406e2de606",
    )
    assert report.ok, report.failed_layers()
    assert [layer.name for layer in report.layers] == ["tree", "payload", "signature"]

    detail = report.layer("tree").detail
    assert detail["hash_mismatches"] == 0
    assert detail["missing"] == 0
    assert detail["unrecorded_on_disk"] == 0
    assert detail["recorded"] == RECORDED, (
        "v1.3 must record the same 29 files v1.2 did. A v1.3 recording 21 was "
        "sealed in a public checkout and has silently dropped the held-out "
        "corpus from the benchmark's integrity claim."
    )
    assert detail["declared_absent"] == DECLARED_ABSENT
    assert detail["verified"] == PRESENT_HERE


def test_the_v1_3_seal_carries_v1_2s_baselines_and_thresholds(v12_and_v13):
    """Acceptance ② and ③, re-asserted on the SIGNED artifact after the fact.

    tests/test_seal.py asserts this on the sealer's carry-forward path before
    the ceremony, which is where it has to be caught. This asserts it on the
    ledger the ceremony actually produced, which is where it has to stay true.
    """
    v12_ledger, v13_ledger = v12_and_v13
    canonical = lambda obj: json.dumps(obj, sort_keys=True, separators=(",", ":"))
    for key in ("baselines_recorded", "thresholds"):
        assert canonical(v13_ledger[key]) == canonical(v12_ledger[key]), (
            f"v1.3 {key} is not byte-identical to v1.2's"
        )


@pytest.fixture
def v12_and_v13():
    """The anchor and the current seal, in that order."""
    return (
        json.loads(V12_LEDGER.read_text()),
        json.loads(LEDGER.read_text()),
    )


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
    assert detail["verified"] == SANDBOX_VERIFIED


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
    assert run_real().layer("tree").detail["unrecorded_on_disk"] == 0


def test_dropping_the_binary_exclusion_surfaces_them_as_unrecorded():
    """Proves the exclusion is load-bearing and the reverse walk is really walking."""
    report = run_real(exclude_basenames=())
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

def test_cli_exits_zero_on_the_v1_3_ledger(capsys):
    """The exact invocation `tests.yml` runs, and the exact output it shows.

    This asserted a FAILURE between `153f40f` and the v1.3 ceremony, because
    the failure was the true state and a gate reporting PASS on a tree that
    does not match its seal is the one thing `verify_manifest.py` was built in
    WP-2.0.2 to prevent. The re-seal made the tree true again, so the green is
    asserted once more -- earned this time rather than assumed.
    """
    code = main([
        "--ledger", str(LEDGER), "--root", str(BENCH),
        "--from-manifest", "--pin-fingerprint", "233bb4406e2de606",
    ])
    assert code == 0
    out = capsys.readouterr().out
    assert "VERDICT: PASS (3/3 layers)" in out
    # All three layers are reported by name in the same output, which is how
    # the log shows the pass was three independent checks and not one.
    assert "LAYER 2/3  PAYLOAD" in out
    assert "LAYER 3/3  SIGNATURE" in out


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


def test_cli_json_output_is_machine_readable(sandbox, capsys):
    """`--json` is what the discovery gate parses, so its SHAPE is the contract.

    Asserted on a RED run, because a failing gate that cannot be parsed by the
    step which has to report it is the case that matters. Before the ceremony
    the red came from the real tree being stale against v1.2. Depending on that
    would now mean depending on an accident, so the red is manufactured here:
    one flipped byte in a throwaway copy, which is red for a reason this test
    controls and will stay red for as long as the test wants it to.
    """
    flip_one_byte(corpus_file(sandbox))
    code = main([
        "--ledger", str(sandbox / LEDGER_NAME), "--root", str(sandbox),
        "--include-dirs", "corpus,harness", "--include-files", "SPEC.md",
        "--expect-absent", ",".join(EXPECT_ABSENT), "--json",
    ])
    assert code == 1
    payload = json.loads(capsys.readouterr().out)
    assert payload["ok"] is False
    assert [layer["name"] for layer in payload["layers"]] == [
        "tree", "payload", "signature"
    ]
    failed = [layer["name"] for layer in payload["layers"] if not layer["ok"]]
    assert failed == ["tree"]
    detail = next(l for l in payload["layers"] if l["name"] == "tree")["detail"]
    assert detail["hash_mismatches"] == 1
    assert detail["recorded"] == RECORDED


def test_cli_json_output_is_machine_readable_on_a_green_run(capsys):
    """The same contract on the real, GREEN tree: shape does not depend on verdict."""
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
    detail = next(l for l in payload["layers"] if l["name"] == "tree")["detail"]
    assert detail["hash_mismatches"] == 0
    assert detail["recorded"] == RECORDED
    assert detail["verified"] == PRESENT_HERE
    assert detail["declared_absent"] == DECLARED_ABSENT


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
