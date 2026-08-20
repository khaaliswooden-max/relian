"""`tools/seal.py` -- one test per deliberate difference from `commit.py`.

WP-2.1 §3.1. `seal.py` is written fresh rather than parameterised out of
`bench/harness/commit.py`, because `commit.py` is itself inside the v1.2
manifest and editing it would break the signature it produced. The price of
that duplication is that the five ways the new sealer deliberately behaves
differently have to be PROVED to behave differently, not asserted in a
docstring. That is what this file is.

Two of these are planted reds in the strict sense: they assert that `seal.py`
FAILS where `commit.py` SUCCEEDS, and the second assertion in each -- the one
that pins `commit.py`'s actual behaviour -- is what stops the test from
quietly passing for the wrong reason if `seal.py` ever grew the fallback back.

Nothing here reads, writes or touches the real signing key. Every test that
needs a key generates an ephemeral one in `tmp_path` and pins against ITS
fingerprint, never against `233bb4406e2de606`.
"""

from __future__ import annotations

import json
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parent.parent
if str(REPO_ROOT / "tools") not in sys.path:
    sys.path.insert(0, str(REPO_ROOT / "tools"))

import seal as seal_module  # noqa: E402
import verify_manifest  # noqa: E402

cryptography = pytest.importorskip("cryptography")
from cryptography.hazmat.primitives import serialization  # noqa: E402
from cryptography.hazmat.primitives.asymmetric.ed25519 import (  # noqa: E402
    Ed25519PrivateKey,
)

DISCOVERY_ROOT = REPO_ROOT / "discovery-bench"
DISCOVERY_CONFIG = DISCOVERY_ROOT / "seal.toml"


# --- fixtures ---------------------------------------------------------------


def _write_key(path: Path) -> str:
    """Write an ephemeral Ed25519 key and return its fingerprint."""
    import hashlib

    private = Ed25519PrivateKey.generate()
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(
        private.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption(),
        )
    )
    public = private.public_key().public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw,
    )
    return hashlib.sha256(public).hexdigest()[:16]


@pytest.fixture
def tree(tmp_path: Path) -> Path:
    """A small sealable tree: two include dirs, one include file, one excluded."""
    root = tmp_path / "bench-under-test"
    (root / "corpus").mkdir(parents=True)
    (root / "harness" / "nested").mkdir(parents=True)
    (root / "corpus" / "a.cpy").write_text("01  A PIC X(01).\n", encoding="utf-8")
    (root / "corpus" / "b.cpy").write_text("01  B PIC X(02).\n", encoding="utf-8")
    (root / "harness" / "run.py").write_text("x = 1\n", encoding="utf-8")
    (root / "harness" / "nested" / "deep.py").write_text("y = 2\n", encoding="utf-8")
    (root / "harness" / "stale.pyc").write_bytes(b"\x00\x01")
    (root / "SPEC.md").write_text("# spec\n", encoding="utf-8")
    return root


def _config(root: Path, out: Path, **overrides) -> seal_module.SealConfig:
    config = seal_module.SealConfig(
        name="TEST-BENCH",
        version="0.0.1",
        tag="test-bench-v0.0.1",
        root=root,
        out_path=out,
        include_rules=seal_module.IncludeRules(
            dirs=["corpus", "harness"],
            files=["SPEC.md"],
            exclude_suffix=[".pyc"],
            exclude_names=["__pycache__"],
        ),
        expected_absent=[],
        toolchain_probes={"python": [sys.executable, "--version"]},
        oracle_toolchain_keys=["python"],
    )
    for key, value in overrides.items():
        setattr(config, key, value)
    return config


# --- difference 1: an absent key file raises. No generation fallback. -------


def test_absent_key_raises_and_never_generates(tmp_path: Path, tree: Path):
    """seal.py refuses; commit.py would have written a fresh PEM and succeeded.

    The failure `commit.py` has is silent and the artifact looks valid: a
    sealing run on a machine without the key produces a manifest that verify()
    returns True on, under a fingerprint nobody has ever seen.
    """
    key_path = tmp_path / "absent" / "nokey.pem"
    config = _config(tree, tmp_path / "LEDGER.json")

    with pytest.raises(seal_module.SealError) as excinfo:
        seal_module.seal(config, key_path, sign_it=True)

    message = str(excinfo.value)
    assert "NEVER generates a key" in message
    # The load path must not have created one on the way past.
    assert not key_path.exists(), "seal.py wrote a key file it must never write"
    # And nothing must have been written to the output path either.
    assert not (tmp_path / "LEDGER.json").exists()


def test_commit_py_is_the_behaviour_being_corrected(tmp_path: Path):
    """Pin `commit.py`'s generation fallback, so difference 1 stays meaningful.

    If `commit.py` is ever fixed upstream this test goes red and the docstring
    in seal.py needs rewriting -- which is the correct outcome, because the
    justification for duplicating fifty lines would have changed. `bench/` is
    never written to: the key is generated into tmp_path.
    """
    sys.path.insert(0, str(REPO_ROOT / "bench" / "harness"))
    try:
        import commit  # noqa: PLC0415
    finally:
        sys.path.pop(0)

    key_path = tmp_path / "generated" / "fresh.pem"
    manifest = {"benchmark": "X", "files": []}
    commit.sign(manifest, key_path)

    assert key_path.is_file(), (
        "commit.py's documented generation fallback did not fire; difference 1 "
        "of seal.py may no longer be describing real behaviour"
    )
    assert commit.verify(manifest) is True, (
        "the whole hazard is that the generated-key manifest VERIFIES"
    )
    # ...and it verifies under a fingerprint that is not the published one.
    assert manifest["signature"]["key_fingerprint"] != seal_module.EXPECTED_KEY_FINGERPRINT


# --- difference 2: any UNAVAILABLE toolchain probe raises -------------------


def test_unavailable_toolchain_probe_refuses_to_sign(tmp_path: Path, tree: Path):
    """The v1.2 ledger carries "javac": "UNAVAILABLE" forever. Not again."""
    config = _config(
        tree,
        tmp_path / "LEDGER.json",
        toolchain_probes={
            "python": [sys.executable, "--version"],
            "definitely-not-a-real-tool": ["definitely-not-a-real-tool", "-v"],
        },
    )
    with pytest.raises(seal_module.SealError) as excinfo:
        seal_module.build_manifest(config)
    assert "definitely-not-a-real-tool" in str(excinfo.value)
    assert "UNAVAILABLE" in str(excinfo.value)


def test_the_v1_2_ledger_still_carries_the_unavailable_this_prevents():
    """The concrete precedent, read from the sealed ledger rather than recalled."""
    ledger = json.loads(
        (REPO_ROOT / "bench" / "LEDGER_relian-bench-v1.2.json").read_text(
            encoding="utf-8"
        )
    )
    assert ledger["toolchain"]["javac"] == seal_module.UNAVAILABLE, (
        "v1.2's permanent UNAVAILABLE is the reason difference 2 exists; if it "
        "is gone the ledger was re-sealed and this test should be revisited"
    )


def test_a_complete_toolchain_is_accepted(tree: Path, tmp_path: Path):
    config = _config(tree, tmp_path / "LEDGER.json")
    manifest = seal_module.build_manifest(config)
    assert seal_module.UNAVAILABLE not in manifest["toolchain"].values()
    assert manifest["toolchain"]["python"].startswith("Python ")


# --- difference 3: name, version, tag, include-set, output are parameters ---


def test_identity_and_output_path_are_parameters(tree: Path, tmp_path: Path):
    """`commit.py` hardcodes "1.2.0", its tag and its output path."""
    out = tmp_path / "nested" / "SOMEWHERE_ELSE.json"
    config = _config(
        tree, out, name="OTHER-BENCH", version="9.9.9", tag="other-v9.9.9"
    )
    manifest = seal_module.seal(config, tmp_path / "unused.pem", sign_it=False)
    assert manifest["benchmark"] == "OTHER-BENCH"
    assert manifest["version"] == "9.9.9"
    assert manifest["tag"] == "other-v9.9.9"
    assert out.is_file(), "out_path is a parameter and the parent was created"
    assert json.loads(out.read_text(encoding="utf-8"))["tag"] == "other-v9.9.9"


def test_include_set_is_a_parameter_and_exclusions_apply(tree: Path, tmp_path: Path):
    config = _config(tree, tmp_path / "L.json")
    everything = seal_module.build_manifest(config)
    paths = {entry["path"] for entry in everything["files"]}
    assert paths == {
        "SPEC.md",
        "corpus/a.cpy",
        "corpus/b.cpy",
        "harness/nested/deep.py",
        "harness/run.py",
    }
    assert "harness/stale.pyc" not in paths, "exclude_suffix did not apply"

    narrowed = _config(
        tree,
        tmp_path / "L2.json",
        include_rules=seal_module.IncludeRules(dirs=["corpus"], files=[]),
    )
    assert {e["path"] for e in seal_module.build_manifest(narrowed)["files"]} == {
        "corpus/a.cpy",
        "corpus/b.cpy",
    }


def test_an_empty_include_set_refuses_rather_than_sealing_nothing(
    tree: Path, tmp_path: Path
):
    config = _config(
        tree,
        tmp_path / "L.json",
        include_rules=seal_module.IncludeRules(dirs=["does-not-exist"], files=[]),
    )
    with pytest.raises(seal_module.SealError, match="covered no files"):
        seal_module.build_manifest(config)


def test_config_file_round_trips(tmp_path: Path, tree: Path):
    """The primary parameter form is a TOML file, so it is reviewable in a diff."""
    config_path = tmp_path / "seal.toml"
    config_path.write_text(
        "[seal]\n"
        'name = "FROM-FILE"\n'
        'version = "1.2.3"\n'
        'tag = "from-file-v1.2.3"\n'
        f'root = "{tree.as_posix()}"\n'
        'out = "OUT.json"\n'
        'oracle_toolchain = ["python"]\n'
        "\n[include]\n"
        'dirs = ["corpus"]\n'
        'files = ["SPEC.md"]\n'
        'exclude_suffix = [".pyc"]\n'
        'exclude_names = ["__pycache__"]\n'
        "\nexpected_absent = []\n"
        "\n[toolchain]\n"
        f'python = ["{Path(sys.executable).as_posix()}", "--version"]\n',
        encoding="utf-8",
    )
    config = seal_module.load_config(config_path)
    assert config.name == "FROM-FILE"
    assert config.tag == "from-file-v1.2.3"
    assert config.include_rules.dirs == ["corpus"]
    manifest = seal_module.build_manifest(config)
    assert manifest["benchmark"] == "FROM-FILE"


# --- difference 4: the manifest self-describes (D12) ------------------------


def test_manifest_records_its_own_rules_absences_toolchain_and_counts(
    tree: Path, tmp_path: Path
):
    oracle = tree / "oracle.json"
    oracle.write_text(
        json.dumps({"counts": {"copybooks": 2, "fields": 7, "source": "derived"}}),
        encoding="utf-8",
    )
    config = _config(tree, tmp_path / "L.json", corpus_counts_from=oracle)
    manifest = seal_module.build_manifest(config)

    assert manifest["include_rules"] == {
        "dirs": ["corpus", "harness"],
        "files": ["SPEC.md"],
        "exclude_suffix": [".pyc"],
        "exclude_names": ["__pycache__"],
    }
    assert manifest["expected_absent"] == []
    assert manifest["oracle_toolchain"]["python"].startswith("Python ")
    # Counts are READ from the oracle, not recounted -- and `source` is dropped
    # because the manifest is not the place that grades them.
    assert manifest["corpus_counts"] == {"copybooks": 2, "fields": 7}


def test_v1_2_records_none_of_them_which_is_why_difference_4_exists():
    ledger = json.loads(
        (REPO_ROOT / "bench" / "LEDGER_relian-bench-v1.2.json").read_text(
            encoding="utf-8"
        )
    )
    for key in ("include_rules", "expected_absent", "oracle_toolchain"):
        assert key not in ledger, (
            f"v1.2 unexpectedly records {key}; difference 4's justification has "
            f"changed"
        )


def test_missing_oracle_refuses_rather_than_recording_zero(tree: Path, tmp_path: Path):
    config = _config(
        tree, tmp_path / "L.json", corpus_counts_from=tree / "no-such-oracle.json"
    )
    with pytest.raises(seal_module.SealError, match="Generate the oracle before"):
        seal_module.build_manifest(config)


def test_from_manifest_reproduces_the_argument_mode_result_exactly(
    tree: Path, tmp_path: Path
):
    """D12's whole point: the two modes must agree on the same tree."""
    ledger = tree / "LEDGER.json"
    fingerprint = _write_key(tmp_path / "k.pem")
    config = _config(tree, ledger)
    seal_module.seal(config, tmp_path / "k.pem", sign_it=False)

    by_arguments = verify_manifest.verify(
        ledger=ledger,
        root=tree,
        include_dirs=["corpus", "harness"],
        include_files=["SPEC.md"],
        exclude_suffixes=[".pyc"],
        exclude_path_parts=["__pycache__"],
        exclude_basenames=[],
    )
    from_manifest = verify_manifest.verify(ledger=ledger, from_manifest=True)

    assert by_arguments.layer("tree").detail == from_manifest.layer("tree").detail
    assert by_arguments.layer("payload").ok and from_manifest.layer("payload").ok
    # `root` was not passed to the second call; it came out of the ledger's
    # own location, which is the "no arguments beyond --ledger" claim.
    assert Path(from_manifest.context["root"]) == tree.resolve()
    assert from_manifest.context["rules_source"] == "manifest"
    assert fingerprint  # the key exists; signing is exercised elsewhere


def test_from_manifest_refuses_a_manifest_that_does_not_self_describe():
    """v1.2 is signed and is NOT retrofitted; the flag refuses rather than guesses."""
    report = verify_manifest.verify(
        ledger=REPO_ROOT / "bench" / "LEDGER_relian-bench-v1.2.json",
        from_manifest=True,
    )
    assert report.fatal is not None
    assert "include_rules" in report.fatal
    assert not report.ok


def test_a_file_on_disk_but_absent_from_files_fails(tree: Path, tmp_path: Path):
    ledger = tree / "LEDGER.json"
    seal_module.seal(_config(tree, ledger), tmp_path / "unused.pem", sign_it=False)
    (tree / "corpus" / "sneaked-in.cpy").write_text("01 C PIC X.\n", encoding="utf-8")

    report = verify_manifest.verify(ledger=ledger, from_manifest=True)
    tree_layer = report.layer("tree")
    assert not tree_layer.ok
    assert "corpus/sneaked-in.cpy" in tree_layer.detail["unrecorded_paths"]


def test_expected_absent_must_be_empty_for_the_discovery_bench():
    """Gate ⑤, enforced on the config rather than only on the eventual ledger.

    A layout oracle has no held-out split: the ground truth IS the answer key.
    Anything here means the seal covers something the public repo cannot check.
    """
    config = seal_module.load_config(DISCOVERY_CONFIG)
    assert config.expected_absent == [], (
        "expected_absent acquired an entry. That is the discovery bench "
        "drifting toward v1.2's limitation -- escalate rather than declare."
    )


def test_the_discovery_seal_config_is_buildable_and_records_what_it_should():
    """The real config, dry-run. Proves the ceremony has nothing left to discover."""
    config = seal_module.load_config(DISCOVERY_CONFIG)
    manifest = seal_module.build_manifest(config)

    assert manifest["benchmark"] == "RELIAN-DISCOVERY-BENCH"
    assert manifest["tag"] == "relian-discovery-bench-v0.1"
    assert manifest["expected_absent"] == []
    assert manifest["oracle_toolchain"]["cobc"].startswith("cobc (GnuCOBOL) ")
    assert manifest["corpus_counts"]["copybooks"] == 15
    assert manifest["include_rules"]["dirs"] == ["corpus", "harness", "oracle"]

    paths = {entry["path"] for entry in manifest["files"]}
    assert "oracle/oracle.json" in paths, "the oracle is inside the seal (D7)"
    assert "SPEC.md" in paths
    assert "seal.toml" in paths
    assert sum(1 for p in paths if p.startswith("corpus/")) == 15
    assert not any(p.startswith("results/") for p in paths), (
        "results/ mirrors bench/results and must not be inside the artifact "
        "those runs are scored against"
    )


# --- difference 5: verification pins the expected fingerprint ---------------


def test_seal_verify_requires_and_honours_the_pin(tree: Path, tmp_path: Path):
    key = tmp_path / "k.pem"
    fingerprint = _write_key(key)
    manifest = seal_module.build_manifest(_config(tree, tmp_path / "L.json"))
    seal_module.sign(manifest, key)

    assert seal_module.verify(manifest, expected_fingerprint=fingerprint) is True
    assert seal_module.verify(manifest, expected_fingerprint="0" * 16) is False
    with pytest.raises(seal_module.SealError, match="requires an expected key"):
        seal_module.verify(manifest, expected_fingerprint="")


def test_seal_refuses_to_write_a_manifest_signed_by_an_unpinned_key(
    tree: Path, tmp_path: Path
):
    """The default pin is the published fingerprint, so a stray key stops here."""
    key = tmp_path / "stranger.pem"
    _write_key(key)
    out = tmp_path / "LEDGER.json"
    with pytest.raises(seal_module.SealError, match="not the published signing key"):
        seal_module.seal(_config(tree, out), key, sign_it=True)
    assert not out.exists(), "a manifest that failed its own pin was written anyway"


def test_the_pin_cannot_be_rebound_by_an_importer_at_runtime(
    tree: Path, tmp_path: Path, monkeypatch
):
    """`verify()`'s default pin is bound at definition time, and that is load-bearing.

    Found while rehearsing the ceremony: reassigning `seal.EXPECTED_KEY_FINGERPRINT`
    does NOT move the default, because Python evaluates default arguments once,
    when the function is defined. So a caller that imports this module cannot
    quietly widen what counts as the published signer -- it has to pass a
    different fingerprint explicitly, at the call site, where a reviewer sees it.
    Pinned here so a later refactor to `expected_fingerprint=None` plus an
    in-body lookup does not silently give that property away.
    """
    key = tmp_path / "k.pem"
    fingerprint = _write_key(key)
    manifest = seal_module.sign(
        seal_module.build_manifest(_config(tree, tmp_path / "L.json")), key
    )
    monkeypatch.setattr(seal_module, "EXPECTED_KEY_FINGERPRINT", fingerprint)
    assert seal_module.verify(manifest) is False, (
        "rebinding the module constant moved the default pin; the published "
        "fingerprint must not be swappable by an importer"
    )
    assert seal_module.verify(manifest, expected_fingerprint=fingerprint) is True


def test_resigned_forgery_passes_three_layers_and_fails_the_pin(
    tree: Path, tmp_path: Path
):
    """The hole fingerprint pinning exists to close (T7), demonstrated end to end.

    An attacker edits the tree, re-seals it consistently, and signs with a key
    they generated. Every internal consistency check the verifier can make
    succeeds -- the tree matches files[], payload_sha256 matches files[], and
    the Ed25519 signature verifies against the key the manifest itself carries.
    Only pinning the expected signer catches it.
    """
    ledger = tree / "LEDGER.json"
    honest_key = tmp_path / "honest.pem"
    honest_fingerprint = _write_key(honest_key)
    config = _config(tree, ledger)
    manifest = seal_module.build_manifest(config)
    seal_module.sign(manifest, honest_key)
    ledger.write_text(json.dumps(manifest, indent=2), encoding="utf-8")

    baseline = verify_manifest.verify(
        ledger=ledger, from_manifest=True, key_fingerprint=honest_fingerprint
    )
    assert baseline.ok, f"the honest manifest should verify: {baseline.failed_layers()}"

    # --- the forgery -------------------------------------------------------
    (tree / "corpus" / "a.cpy").write_text(
        "01  A PIC X(99).\n", encoding="utf-8"
    )  # a changed benchmark input
    attacker_key = tmp_path / "attacker.pem"
    attacker_fingerprint = _write_key(attacker_key)
    assert attacker_fingerprint != honest_fingerprint

    forged = seal_module.build_manifest(config)      # rehashes the edited tree
    seal_module.sign(forged, attacker_key)           # re-signs it consistently
    ledger.write_text(json.dumps(forged, indent=2), encoding="utf-8")

    unpinned = verify_manifest.verify(ledger=ledger, from_manifest=True)
    assert unpinned.layer("tree").ok, "layer 1 should be fooled -- that is the point"
    assert unpinned.layer("payload").ok, "layer 2 should be fooled"
    assert unpinned.layer("signature").ok, "layer 3 should be fooled"
    assert unpinned.ok, (
        "all three layers pass on a re-signed forgery. This is not a bug in "
        "the verifier; it is why the pin is mandatory."
    )

    pinned = verify_manifest.verify(
        ledger=ledger, from_manifest=True, key_fingerprint=honest_fingerprint
    )
    assert not pinned.ok
    assert pinned.failed_layers() == ["signature"]
    assert any(
        "does not match the pinned" in problem
        for problem in pinned.layer("signature").problems
    )
    # And seal.py's own verify() says the same thing.
    assert seal_module.verify(forged, expected_fingerprint=honest_fingerprint) is False
    assert seal_module.verify(forged, expected_fingerprint=attacker_fingerprint) is True


def test_pin_fingerprint_and_key_fingerprint_are_the_same_flag(
    tree: Path, tmp_path: Path, capsys
):
    """WP-2.1 asks for --pin-fingerprint; CI already passes --key-fingerprint."""
    ledger = tree / "LEDGER.json"
    key = tmp_path / "k.pem"
    fingerprint = _write_key(key)
    manifest = seal_module.build_manifest(_config(tree, ledger))
    seal_module.sign(manifest, key)
    ledger.write_text(json.dumps(manifest, indent=2), encoding="utf-8")

    base = ["--ledger", str(ledger), "--from-manifest", "--json"]
    assert verify_manifest.main(base + ["--pin-fingerprint", fingerprint]) == 0
    capsys.readouterr()
    assert verify_manifest.main(base + ["--key-fingerprint", fingerprint]) == 0
    capsys.readouterr()
    assert verify_manifest.main(base + ["--pin-fingerprint", "0" * 16]) == 1
    report = json.loads(capsys.readouterr().out)
    assert report["ok"] is False


# --- the two posix conventions, replicated verbatim from commit.py ----------


def test_manifest_paths_are_posix_even_for_nested_files(tree: Path, tmp_path: Path):
    """BUG-AVOIDANCE #1: as_posix() unconditionally, never native separators."""
    manifest = seal_module.build_manifest(_config(tree, tmp_path / "L.json"))
    paths = [entry["path"] for entry in manifest["files"]]
    assert "harness/nested/deep.py" in paths
    assert not any("\\" in path for path in paths)


def test_entries_are_sorted_by_the_posix_string_not_the_path_object(
    tree: Path, tmp_path: Path
):
    """BUG-AVOIDANCE #2: sort by the explicit posix STRING form."""
    manifest = seal_module.build_manifest(_config(tree, tmp_path / "L.json"))
    paths = [entry["path"] for entry in manifest["files"]]
    assert paths == sorted(paths), "files[] is not in posix-string order"


def test_both_conventions_match_commit_py_on_the_same_tree(tree: Path, tmp_path: Path):
    """The duplication is only safe if the two sealers still agree byte for byte.

    Reimplements commit.py's ordering and hashing here rather than importing it,
    then asserts seal.py produces the identical entry list. If either file's
    conventions ever drift, the two benchmarks would hash differently on the
    same machine and this goes red.
    """
    import hashlib

    expected = []
    for directory in ("corpus", "harness"):
        for path in (tree / directory).rglob("*"):
            if path.is_file() and path.suffix not in {".pyc", ".o"}:
                expected.append(path)
    expected.append(tree / "SPEC.md")
    expected.sort(key=lambda p: p.relative_to(tree).as_posix())
    reference = [
        {
            "path": p.relative_to(tree).as_posix(),
            "sha256": hashlib.sha256(p.read_bytes()).hexdigest(),
        }
        for p in expected
    ]

    manifest = seal_module.build_manifest(_config(tree, tmp_path / "L.json"))
    assert manifest["files"] == reference
    assert manifest["payload_sha256"] == seal_module.payload_sha256(reference)


def test_the_manifest_is_deterministic_across_two_runs(tree: Path, tmp_path: Path):
    """Everything except `committed_at`, which is the one deliberate variable."""
    first = seal_module.build_manifest(_config(tree, tmp_path / "a.json"))
    second = seal_module.build_manifest(_config(tree, tmp_path / "b.json"))
    for manifest in (first, second):
        manifest.pop("committed_at")
    assert first == second


def test_the_published_fingerprint_is_the_one_this_repo_already_trusts():
    """D14: two benchmarks, one custody chain, one thing to publish."""
    ledger = json.loads(
        (REPO_ROOT / "bench" / "LEDGER_relian-bench-v1.2.json").read_text(
            encoding="utf-8"
        )
    )
    assert (
        ledger["signature"]["key_fingerprint"]
        == seal_module.EXPECTED_KEY_FINGERPRINT
        == "233bb4406e2de606"
    )


def test_seal_contains_no_key_generation_or_serialisation_call():
    """R4: custody is the operator's. The sealer has no key discovery logic.

    Asserted against the PARSED module rather than its text, because the
    docstring names `Ed25519PrivateKey.generate()` on purpose -- it is
    documenting the behaviour it refuses to have -- and a substring search
    cannot tell prose from code.
    """
    import ast

    tree = ast.parse((REPO_ROOT / "tools" / "seal.py").read_text(encoding="utf-8"))
    called = {
        node.func.attr
        for node in ast.walk(tree)
        if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
    }
    assert "generate" not in called, "seal.py must never generate a key"
    assert "private_bytes" not in called, (
        "seal.py must never serialise a private key"
    )
    # The public half is fine -- it goes into the signature block.
    assert "public_bytes" in called
