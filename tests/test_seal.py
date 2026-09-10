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

import ast
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


def _import_commit_py():
    """Import `bench/harness/commit.py` without leaving it on `sys.path`."""
    sys.path.insert(0, str(REPO_ROOT / "bench" / "harness"))
    try:
        import commit  # noqa: PLC0415

        return commit
    finally:
        sys.path.pop(0)


def test_commit_py_now_refuses_on_an_absent_key(tmp_path: Path):
    """The repointed negative control. It was pinning the OPPOSITE behaviour.

    WHAT THIS TEST USED TO SAY, AND WHY IT WENT RED
    -----------------------------------------------
    Until `153f40f` this was `test_commit_py_is_the_behaviour_being_corrected`,
    and it asserted that `commit.sign()` on an absent key path GENERATED a
    fresh Ed25519 key, wrote it, and produced a manifest that `verify()`
    returned True on under an unpublished fingerprint. Its own docstring
    predicted its death:

        *"If commit.py is ever fixed upstream this test goes red and the
        docstring in seal.py needs rewriting -- which is the correct outcome,
        because the justification for duplicating fifty lines would have
        changed."*

    `153f40f` fixed `commit.py`. The control fired exactly as designed. It is
    REPOINTED rather than deleted, because what it guards is still worth
    guarding -- it just guards the other side of the line now: the fallback
    must not come back.

    `bench/harness/commit.py` is a sealed file and is NOT edited here. This
    test only imports and calls it; the key path it is given is under
    `tmp_path` and is never created, which is the whole point.
    """
    commit = _import_commit_py()

    key_path = tmp_path / "generated" / "fresh.pem"
    manifest = {"benchmark": "X", "files": []}

    # `commit.py` refuses with SystemExit rather than a custom exception; the
    # behaviour under test is the refusal, not its type.
    with pytest.raises(SystemExit) as excinfo:
        commit.sign(manifest, key_path)

    assert not key_path.exists(), (
        "commit.py wrote a key file on the absent-key path; the generation "
        "fallback that 153f40f removed has come back"
    )
    assert "signature" not in manifest, (
        "commit.py signed something despite refusing; a refusal that still "
        "produces a signature is worse than no refusal"
    )
    message = str(excinfo.value)
    assert "REFUSING" in message
    assert seal_module.EXPECTED_KEY_FINGERPRINT in message, (
        "the refusal should name the key the benchmark is actually sealed "
        "with, so the operator knows what to restore rather than generate"
    )


def test_seal_py_records_difference_one_as_retired_and_names_the_sha():
    """Acceptance ⑤: the docstring was narrowed, and it says by what.

    The justification for a second sealer shrank when `153f40f` landed. A
    docstring still claiming five differences after one of them stopped being
    a difference is a stale claim in the file whose entire subject is stale
    claims, so the narrowing is asserted rather than trusted.
    """
    text = (REPO_ROOT / "tools" / "seal.py").read_text(encoding="utf-8")
    head = text.split('"""')[1]

    assert "FOUR DELIBERATE DIFFERENCES" in head, (
        "difference 1 is retired; the count must say so"
    )
    assert "FIVE DELIBERATE DIFFERENCES" not in head
    assert "RETIRED" in head
    assert "153f40f" in head, (
        "the sha that retired difference 1 must be named, so the claim is "
        "checkable against history rather than remembered"
    )
    # The four survivors are still described, each by the phrase that names it.
    for surviving in (
        "UNAVAILABLE",                       # 2
        "are parameters",                    # 3
        "The manifest records",              # 4
        "pins the expected key fingerprint", # 5
    ):
        assert surviving in head, f"surviving difference {surviving!r} lost"


def test_commit_py_is_still_the_file_this_whole_package_exists_because_of():
    """`commit.py` is inside the manifest it produces. That is the trap.

    Not a style observation: it is the mechanism by which a correct one-line
    bugfix (`153f40f`) invalidated the v1.2 seal and turned `main` red. Pinned
    so that anyone tempted to "just parameterise commit.py" for v1.4 meets
    this assertion first.
    """
    ledger = json.loads(
        (REPO_ROOT / "bench" / "LEDGER_relian-bench-v1.2.json").read_text(
            encoding="utf-8"
        )
    )
    recorded = {entry["path"] for entry in ledger["files"]}
    assert "harness/commit.py" in recorded, (
        "commit.py is a recorded entry in the manifest commit.py produces; "
        "if that ever stops being true, seal.py's difference 3 rationale "
        "needs revisiting"
    )
    # And tools/seal.py is not, which is what makes it editable.
    assert not any(path.startswith("tools/") for path in recorded)


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


# --- WP-2.6: the v1.3 re-seal, asserted BEFORE the ceremony -----------------
#
# Every test below runs with no key, signs nothing, and writes nothing under
# `bench/`. They exist so that the operator's key session has nothing left to
# discover: the carry-forward, the include set and the declared absences are
# proved here, on this branch, in CI, before the ledger exists.

BENCH_ROOT = REPO_ROOT / "bench"
BENCH_CONFIG = BENCH_ROOT / "seal.toml"
V12_LEDGER = BENCH_ROOT / "LEDGER_relian-bench-v1.2.json"


def _canonical(obj) -> str:
    """The serialisation the manifest hash is actually taken over.

    `payload_sha256` and `manifest_hash()` both use `sort_keys=True,
    separators=(",", ":")`, so this -- not `==` on dicts, and not the
    indented pretty-print -- is the form in which "byte-identical" is the
    claim that matters.
    """
    return json.dumps(obj, sort_keys=True, separators=(",", ":"))


@pytest.fixture(scope="module")
def v12() -> dict:
    return json.loads(V12_LEDGER.read_text(encoding="utf-8"))


@pytest.fixture(scope="module")
def bench_config() -> seal_module.SealConfig:
    return seal_module.load_config(BENCH_CONFIG)


@pytest.fixture(scope="module")
def carried(bench_config: seal_module.SealConfig) -> dict:
    """What v1.3 will carry, produced by the code path the ceremony runs."""
    return seal_module.load_carry_forward(
        bench_config.carry_forward_from, bench_config.carry_forward_keys
    )


def test_v1_3_carries_v1_2_baselines_recorded_byte_identical(carried, v12):
    """Acceptance ②. The one that can silently destroy the benchmark.

    `baselines_recorded` is the measured floor from BEFORE any solution work
    (ZCS-6 Phase 4). `commit.py.__main__` re-derives it from
    `bench/results/*.json` at seal time. That directory is in no include set,
    so it is unsealed and mutable, and it HAS moved since v1.2 was sealed.

    A v1.3 that re-derived would overwrite a pre-solution floor with a
    post-solution number and still be internally consistent and
    signature-valid under the real key -- undetectable downstream, because
    everything downstream verifies the manifest against itself. This assertion
    is the only place it is catchable, and it runs before the ceremony.
    """
    assert _canonical(carried["baselines_recorded"]) == _canonical(
        v12["baselines_recorded"]
    ), "v1.3 must carry v1.2's pre-solution floor forward unchanged"


def test_v1_3_carries_v1_2_thresholds_byte_identical(carried, v12):
    """Acceptance ③. `bench.yml` reads `ledger['thresholds']` for the R10 gates.

    A changed key here silently moves what passes. Asserting the whole block
    rather than the three numbers catches a renamed or dropped key too -- a
    threshold that disappears does not fail closed, it stops being read.
    """
    assert _canonical(carried["thresholds"]) == _canonical(v12["thresholds"]), (
        "v1.3 must carry v1.2's merge gates forward unchanged"
    )


def test_the_carried_thresholds_are_the_r10_gates_bench_yml_reads(carried):
    """The values themselves, spelled out, so a silent move is visible in a diff."""
    thresholds = carried["thresholds"]
    assert thresholds["ber_heldout_min"] == 0.95
    assert thresholds["build_rate_min"] == 1.00
    assert thresholds["branch_coverage_min"] == 0.80
    assert thresholds["coverage_required_tool"] == "jacoco"


def test_carry_forward_reads_the_ledger_and_never_bench_results(bench_config):
    """It must be a COPY, not a recomputation that happens to agree today.

    Measured while writing this: re-deriving the baselines from
    `bench/results/*.json` right now produces the same three figures v1.2
    recorded, so an equality assertion alone would pass for a recomputing
    sealer and prove nothing. The agreement is a coincidence of timing --
    `bench/results/C1_rulebased.json` changed by 66 lines between the sealing
    commit `e286cb3` and `153f40f` without those particular aggregates moving,
    and the next refresh has no reason to be as kind.

    So the mechanism is asserted, not the outcome: the configured source is
    the signed v1.2 ledger, and `bench/results/` is not consulted at all.
    """
    assert bench_config.carry_forward_from == V12_LEDGER
    assert bench_config.carry_forward_keys == ["baselines_recorded", "thresholds"]

    source = ast.parse(
        (REPO_ROOT / "tools" / "seal.py").read_text(encoding="utf-8")
    )
    function = next(
        node
        for node in ast.walk(source)
        if isinstance(node, ast.FunctionDef) and node.name == "load_carry_forward"
    )
    # The docstring is EXCLUDED on purpose: it explains bench/results at
    # length, because explaining what is not read is the point of it. A
    # substring search that cannot tell prose from code would fail on the
    # documentation of the very guarantee being asserted.
    body = function.body[1:] if ast.get_docstring(function) else function.body
    literals = {
        node.value
        for statement in body
        for node in ast.walk(statement)
        if isinstance(node, ast.Constant) and isinstance(node.value, str)
    }
    assert not any("results" in literal for literal in literals), (
        "load_carry_forward must not reference bench/results; carrying "
        f"forward and re-deriving are the two behaviours being told apart: "
        f"{sorted(literals)}"
    )


def test_carry_forward_refuses_a_missing_source(tmp_path: Path):
    """Silently carrying nothing is the failure this exists to prevent."""
    with pytest.raises(seal_module.SealError) as excinfo:
        seal_module.load_carry_forward(tmp_path / "nope.json", ["thresholds"])
    assert "does not exist" in str(excinfo.value)


def test_carry_forward_refuses_a_source_missing_the_named_key(tmp_path: Path):
    """A block the previous seal recorded must not vanish from the next one."""
    source = tmp_path / "ledger.json"
    source.write_text(json.dumps({"thresholds": {"a": 1}}), encoding="utf-8")
    with pytest.raises(seal_module.SealError) as excinfo:
        seal_module.load_carry_forward(source, ["thresholds", "baselines_recorded"])
    assert "baselines_recorded" in str(excinfo.value)


def test_carried_blocks_share_no_structure_with_the_source(tmp_path: Path):
    """A carried block must not be a live alias into the parsed source."""
    source = tmp_path / "ledger.json"
    source.write_text(json.dumps({"thresholds": {"nested": {"a": 1}}}), encoding="utf-8")
    first = seal_module.load_carry_forward(source, ["thresholds"])
    first["thresholds"]["nested"]["a"] = 999
    second = seal_module.load_carry_forward(source, ["thresholds"])
    assert second["thresholds"]["nested"]["a"] == 1


def test_a_carried_key_cannot_be_overwritten_by_a_computed_block(tree: Path, tmp_path: Path):
    """Ordering is load-bearing: a carry-forward that loses a race is a recompute."""
    source = tmp_path / "ledger.json"
    source.write_text(json.dumps({"tag": "carried-tag"}), encoding="utf-8")
    config = _config(
        tree,
        tmp_path / "out.json",
        carry_forward_from=source,
        carry_forward_keys=["tag"],
    )
    with pytest.raises(seal_module.SealError) as excinfo:
        seal_module.build_manifest(config)
    assert "collides" in str(excinfo.value)


def test_carry_forward_with_keys_but_no_source_is_refused_at_config_time(tmp_path: Path):
    config_path = tmp_path / "seal.toml"
    config_path.write_text(
        '[seal]\nname="X"\nversion="1"\ntag="t"\nroot="."\nout="o.json"\n'
        '[carry_forward]\nkeys=["thresholds"]\n',
        encoding="utf-8",
    )
    with pytest.raises(seal_module.SealError) as excinfo:
        seal_module.load_config(config_path)
    assert "no `from` ledger" in str(excinfo.value)


# --- WP-2.6: the include set and the declared absences ----------------------


def test_the_v1_3_include_set_reproduces_the_v1_2_file_list(bench_config, v12):
    """Acceptance ①, from the sealer's side rather than the verifier's.

    `tools/verify_manifest.py` reports one hash mismatch and nothing else. That
    proves the TREE moved by exactly one file. It does not prove `seal.toml`
    describes the same include set `commit.py`'s module constants do -- and a
    v1.3 sealed from a subtly different include set would be a clean-looking
    manifest covering a different benchmark.

    So the walk is compared against v1.2's own `files[]`: same paths, no
    extras, none missing, once the eight declared-absent entries (which this
    public perimeter does not hold) are set aside.
    """
    walked = {
        entry["path"]
        for entry in seal_module.build_entries(
            bench_config.root, bench_config.include_rules
        )
    }
    absent = set(bench_config.expected_absent)
    recorded_present = {
        entry["path"] for entry in v12["files"] if entry["path"] not in absent
    }

    assert walked - recorded_present == set(), (
        "the v1.3 include set covers files v1.2 did not seal"
    )
    assert recorded_present - walked == set(), (
        "the v1.3 include set drops files v1.2 sealed"
    )
    assert len(walked) == 21
    assert len(v12["files"]) == 29


def test_exactly_one_sealed_file_differs_from_v1_2(bench_config, v12):
    """Acceptance ①. The blast radius, asserted rather than remembered.

    If a second file ever appears in this list it is a different work package,
    and this assertion is where that gets noticed.
    """
    recorded = {entry["path"]: entry["sha256"] for entry in v12["files"]}
    differing = sorted(
        entry["path"]
        for entry in seal_module.build_entries(
            bench_config.root, bench_config.include_rules
        )
        if recorded[entry["path"]] != entry["sha256"]
    )
    assert differing == ["harness/commit.py"], (
        f"expected exactly harness/commit.py to differ from the v1.2 seal, "
        f"got {differing}. More than one means the re-seal's blast radius is "
        f"wider than WP-2.6 scoped and the seal is blocked."
    )


def test_expected_absent_survives_being_written_under_the_include_table():
    """The config-reader defect this package found, pinned so it stays fixed.

    TOML puts a bare key after a table header INSIDE that table, and
    `discovery-bench/seal.toml` writes `expected_absent` below `[include]`.
    `load_config` read it from the TOP level only. That was invisible for the
    discovery bench, whose value is `[]` and whose default was therefore also
    `[]` -- but `bench/seal.toml` declares eight absences, and dropping them
    would seal a v1.3 asserting nothing is absent, which then reports eight
    files MISSING the moment CI verifies it in the public perimeter.
    """
    config = seal_module.load_config(BENCH_CONFIG)
    assert len(config.expected_absent) == 8
    assert all(
        path.endswith("heldout.jsonl") or path.endswith("gen_vectors.py")
        for path in config.expected_absent
    )


def test_expected_absent_declared_twice_with_different_values_is_refused(tmp_path: Path):
    """A config with two answers has no correct one to pick."""
    config_path = tmp_path / "seal.toml"
    # The top-level key must precede every table header, or TOML puts it
    # inside the preceding table -- which is the very confusion being guarded.
    config_path.write_text(
        'expected_absent=["a"]\n'
        '[seal]\nname="X"\nversion="1"\ntag="t"\nroot="."\nout="o.json"\n'
        '[include]\ndirs=["corpus"]\nexpected_absent=["b"]\n',
        encoding="utf-8",
    )
    with pytest.raises(seal_module.SealError) as excinfo:
        seal_module.load_config(config_path)
    assert "declared both" in str(excinfo.value)


def test_a_declared_absence_matching_nothing_recorded_refuses_to_seal():
    """`expected_absent` means absent in the VERIFYING perimeter, not this one.

    If the ceremony runs on a machine that does not hold the held-out corpus,
    the walk never sees those files, they are never recorded, and the manifest
    declares eight absences it does not contain. The seal would cover 21 files
    while claiming to account for 29, `verify_manifest.py` would report
    `declared_absent_but_present: 0` and pass, and the held-out corpus would
    have dropped out of the benchmark's integrity claim entirely.

    Nothing here reads a held-out file. Only recorded PATHS are matched.
    """
    with pytest.raises(seal_module.SealError) as excinfo:
        seal_module.require_declared_absences_are_present(
            ["corpus/P01_payroll/vectors/heldout.jsonl"],
            ["corpus/P01_payroll/program.cbl", "SPEC.md"],
        )
    message = str(excinfo.value)
    assert "expected_absent" in message
    assert "29" in message and "8" in message, (
        "the refusal should name the census it is protecting -- 29 recorded, "
        "8 declared absent -- so the operator knows the ceremony machine is "
        "the problem rather than the config"
    )


def test_the_absence_guard_passes_when_the_files_are_recorded():
    """The complete-benchmark case: recorded, though absent where it verifies."""
    seal_module.require_declared_absences_are_present(
        ["corpus/P01_payroll/vectors/heldout.jsonl", "harness/gen_vectors.py"],
        [
            "corpus/P01_payroll/vectors/heldout.jsonl",
            "harness/gen_vectors.py",
            "SPEC.md",
        ],
    )


def test_no_declared_absences_means_the_guard_does_nothing():
    """RELIAN-DISCOVERY-BENCH declares none and must stay unaffected."""
    seal_module.require_declared_absences_are_present([], [])


def test_the_v1_3_seal_config_is_not_itself_inside_the_seal(bench_config, v12):
    """`bench/seal.toml` must stay OUTSIDE the manifest it configures.

    This is the whole lesson of WP-2.6 applied to the new file. `commit.py` is
    inside the v1.2 manifest, so editing it to seal v1.3 would invalidate v1.3
    -- the loop. A `seal.toml` inside the seal would reproduce that exactly:
    every future version bump would break the ledger it just produced.

    The include set is `corpus`, `harness` and `SPEC.md`; `seal.toml` sits at
    the bench root and is in none of them. Note this deliberately DIFFERS from
    `discovery-bench/seal.toml`, which is inside its own seal -- defensible for
    a v0.1 that is tagged and frozen, and not defensible for a benchmark that
    is on its third version.

    What keeps the rules honest instead is difference 4: `include_rules` is
    recorded inside the SIGNED payload, so the rules are signed as data even
    though the file that supplied them is not.
    """
    walked = {
        entry["path"]
        for entry in seal_module.build_entries(
            bench_config.root, bench_config.include_rules
        )
    }
    assert "seal.toml" not in walked
    assert BENCH_CONFIG.is_file(), "the config must exist to be excluded from"
    assert "seal.toml" not in {entry["path"] for entry in v12["files"]}


def test_the_v1_3_identity_is_what_the_workflows_expect(bench_config):
    """The filename `bench.yml` and `tests.yml` are pointed at."""
    assert bench_config.name == "RELIAN-BENCH"
    assert bench_config.version == "1.3.0"
    assert bench_config.tag == "relian-bench-v1.3"
    assert bench_config.out_path.name == "LEDGER_relian-bench-v1.3.json"
    assert bench_config.out_path.parent == BENCH_ROOT


def test_the_v1_2_ledger_is_still_present_as_the_r7_provenance_anchor():
    """Acceptance ⑧. v1.3 supersedes v1.2 for tree integrity ONLY.

    v1.2's `committed_at` is the R7 anchor that predates the grammar merge.
    Deleting it would delete the evidence that the benchmark was sealed before
    the solution work, which is the claim the whole ordering exists to make.
    `LEDGER_archive_v1.0` and `v1.1` already coexist; the precedent is set.
    """
    assert V12_LEDGER.is_file()
    ledger = json.loads(V12_LEDGER.read_text(encoding="utf-8"))
    assert ledger["committed_at"].startswith("2026-08-16")
    assert ledger["signature"]["key_fingerprint"] == "233bb4406e2de606"


# --- WP-2.6: the passphrase-encrypted key -----------------------------------
#
# The release key was re-wrapped on 2026-09-08 and still signs as
# 233bb4406e2de606. Nothing below touches it. Every key here is ephemeral,
# generated into tmp_path, and pinned against ITS OWN fingerprint.


def _write_encrypted_key(path: Path, passphrase: bytes) -> str:
    import hashlib

    private = Ed25519PrivateKey.generate()
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(
        private.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.BestAvailableEncryption(passphrase),
        )
    )
    public = private.public_key().public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw,
    )
    return hashlib.sha256(public).hexdigest()[:16]


def test_an_encrypted_key_would_have_failed_the_old_password_none_loader(tmp_path: Path):
    """The planted red for the loader fix: prove the hazard was real.

    `load_private_key` passed `password=None` unconditionally. Against the
    re-wrapped release key that raises -- at the ceremony, with the key already
    unlocked on the operator's desk and the session wasted. This asserts the
    old call still fails, so the new path cannot be mistaken for a no-op.
    """
    key_path = tmp_path / "encrypted.pem"
    _write_encrypted_key(key_path, b"correct horse battery staple")

    with pytest.raises((TypeError, ValueError)):
        serialization.load_pem_private_key(key_path.read_bytes(), password=None)


def test_an_encrypted_key_loads_through_the_passphrase_prompt(tmp_path: Path):
    """...and the new path loads the same key when the passphrase is right."""
    key_path = tmp_path / "encrypted.pem"
    fingerprint = _write_encrypted_key(key_path, b"correct horse battery staple")

    asked = []

    def prompt(message: str) -> str:
        asked.append(message)
        return "correct horse battery staple"

    private = seal_module.load_private_key(key_path, prompt=prompt)

    import hashlib

    public = private.public_key().public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw,
    )
    assert hashlib.sha256(public).hexdigest()[:16] == fingerprint
    assert asked and key_path.name in asked[0], (
        "the prompt should name the key file, so an operator with more than "
        "one key knows which passphrase is being asked for"
    )


def test_an_unencrypted_key_is_never_prompted_for(tmp_path: Path):
    """The plain-PEM path must not grow a prompt that would hang unattended CI."""
    key_path = tmp_path / "plain.pem"
    _write_key(key_path)

    def prompt(message: str) -> str:  # pragma: no cover - must not be called
        raise AssertionError("an unencrypted key must not prompt")

    assert seal_module.load_private_key(key_path, prompt=prompt) is not None


def test_a_wrong_passphrase_refuses_without_echoing_anything(tmp_path: Path):
    """R4-adjacent: a failure message must not leak the secret it failed on."""
    key_path = tmp_path / "encrypted.pem"
    secret = "sw0rdf1sh-mnemonic-zebra"
    attempted = "hunter2-not-the-one"
    _write_encrypted_key(key_path, secret.encode("utf-8"))

    with pytest.raises(seal_module.SealError) as excinfo:
        seal_module.load_private_key(key_path, prompt=lambda _: attempted)

    message = str(excinfo.value)
    assert secret not in message, "the refusal echoed the real passphrase"
    assert attempted not in message, "the refusal echoed the attempted one"
    assert "Nothing was signed" in message


def test_the_passphrase_is_never_read_from_argv_or_the_environment():
    """It is a getpass prompt and only a getpass prompt.

    Asserted against the parsed module: no `--passphrase`-shaped CLI flag, and
    no `os.environ` read anywhere in the file. A passphrase in `argv` is in the
    process table; a passphrase in the environment is in every child process.
    """
    source = ast.parse(
        (REPO_ROOT / "tools" / "seal.py").read_text(encoding="utf-8")
    )
    flags = {
        node.value
        for call in ast.walk(source)
        if isinstance(call, ast.Call)
        and isinstance(call.func, ast.Attribute)
        and call.func.attr == "add_argument"
        for node in call.args
        if isinstance(node, ast.Constant) and isinstance(node.value, str)
    }
    assert not any(
        "pass" in flag or "phrase" in flag or "secret" in flag for flag in flags
    ), f"the sealer must not take a passphrase on the command line: {flags}"

    attributes = {
        node.attr for node in ast.walk(source) if isinstance(node, ast.Attribute)
    }
    assert "environ" not in attributes, (
        "the sealer must not read the environment for anything, least of all "
        "a passphrase"
    )


def test_seal_py_still_never_generates_or_serialises_a_private_key():
    """Difference 1's BEHAVIOUR outlives difference 1's status as a contrast.

    `153f40f` retired the contrast with `commit.py`. It did not retire the
    guarantee, and this is the assertion that says so.
    """
    source = ast.parse(
        (REPO_ROOT / "tools" / "seal.py").read_text(encoding="utf-8")
    )
    called = {
        node.func.attr
        for node in ast.walk(source)
        if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
    }
    assert "generate" not in called
    assert "private_bytes" not in called


# --- WP-2.6: vector counts are measured, never carried ----------------------


def test_v1_3_records_no_vector_counts_and_that_is_a_stated_choice(bench_config):
    """The block is omitted on purpose, and the reason is in the config.

    v1.2 records a 14-key `vector_counts` block: public AND held-out counts per
    program, the held-out half summing to 425 -- the denominator of the
    published held-out BER, and the check that the split was not resized
    between seals.

    Measuring the held-out half means opening `heldout.jsonl`, which rule 1 and
    R3 forbid to the agent that authored `bench/seal.toml`. Globbing only
    `public.jsonl` would emit 7 keys where v1.2 has 14, which anyone diffing
    two seals reads as the held-out split having gone to zero -- a partial
    block is worse than no block. So it is omitted, and `seal.toml` says so
    along with the one-line change that includes it.

    Asserted rather than left implicit, because a silently absent block and a
    deliberately absent one look identical in a manifest.
    """
    assert bench_config.vector_counts_glob is None
    text = BENCH_CONFIG.read_text(encoding="utf-8")
    assert "OPERATOR DECISION REQUIRED" in text
    assert "425" in text, "the census the omission gives up must be named"
    # The commented-out form is present so the change is a one-liner, but it
    # must not be live.
    assert "\nvector_counts_glob" not in text


def test_measure_vector_counts_never_reaches_a_held_out_file(bench_config):
    """The capability is kept and tested; rule 1 is enforced by the pattern.

    If the operator opts the block in, this is the assertion that the
    public-only pattern behaves, and the pattern -- not intent -- is what keeps
    held-out files closed.
    """
    counts = seal_module.measure_vector_counts(
        bench_config.root, "corpus/*/vectors/public.jsonl"
    )
    assert len(counts) == 7, "one public vector file per corpus program"
    assert all(name.endswith("public.jsonl") for name in counts)
    assert not any("heldout" in name for name in counts)
    assert all(isinstance(value, int) and value > 0 for value in counts.values())

    for name, value in counts.items():
        lines = (bench_config.root / name).read_text(encoding="utf-8").splitlines()
        assert value == len([line for line in lines if line.strip()])


def test_the_public_counts_still_match_what_v1_2_sealed(bench_config, v12):
    """The corpus did not move, and this is the value-level confirmation.

    verify_manifest reports zero mismatches under `corpus/`, so this should
    hold -- and asserting it means a corpus edit that somehow passed the hash
    check would still be caught by a count. v1.2 keys are relative to
    `corpus/`; the measured ones are root-relative, so the comparison is by
    value under a normalised key.
    """
    measured = {
        name[len("corpus/"):]: value
        for name, value in seal_module.measure_vector_counts(
            bench_config.root, "corpus/*/vectors/public.jsonl"
        ).items()
    }
    sealed_public = {
        name: value
        for name, value in v12["vector_counts"].items()
        if name.endswith("public.jsonl")
    }
    assert measured == sealed_public


def test_v1_2s_held_out_census_is_the_provenance_v1_3_defers_to():
    """v1.2 stays the anchor for the vector census. 425 held-out vectors.

    Read from the sealed ledger, never from a held-out file -- the counts are
    metadata v1.2 recorded, and reading them here opens nothing.
    """
    counts = json.loads(V12_LEDGER.read_text(encoding="utf-8"))["vector_counts"]
    held_out = {k: v for k, v in counts.items() if k.endswith("heldout.jsonl")}
    assert len(held_out) == 7
    assert sum(held_out.values()) == 425, (
        "the held-out denominator the published BER is measured over"
    )


def test_the_oracle_compiler_is_a_sealing_condition(bench_config):
    """`cobc` builds the oracles, so its exact patch version is sealed.

    bench.yml compiles every corpus program with `cobc -x program.cbl -o
    oracle`; the oracle's behaviour IS the ground truth the BER is measured
    against. v1.2 recorded no `oracle_toolchain` block at all.
    """
    assert bench_config.oracle_toolchain_keys == ["cobc"]
    assert "cobc" in bench_config.toolchain_probes
    assert "oracle_toolchain" not in json.loads(
        V12_LEDGER.read_text(encoding="utf-8")
    )


def test_a_vector_glob_matching_nothing_refuses(tmp_path: Path):
    """An empty measured block is indistinguishable from a corpus with none."""
    with pytest.raises(seal_module.SealError) as excinfo:
        seal_module.measure_vector_counts(tmp_path, "corpus/*/vectors/public.jsonl")
    assert "matched no files" in str(excinfo.value)


def test_no_vector_glob_means_no_block_rather_than_an_empty_one(tmp_path: Path):
    assert seal_module.measure_vector_counts(tmp_path, None) == {}


# --- WP-2.6: the signer pin in bench.yml must be DERIVED, not read ----------
#
# Found by Cursor Bugbot on PR #45, verified, and fixed. The first version of
# that pin read `signature.key_fingerprint` and was worthless. These two tests
# are why it cannot silently become worthless again: one proves the hazard on
# the primitive, the other pins the workflow that has to avoid it.

BENCH_WORKFLOW = REPO_ROOT / ".github" / "workflows" / "bench.yml"


def test_a_declared_key_fingerprint_cannot_be_trusted(tmp_path: Path):
    """The planted red. `key_fingerprint` is UNSIGNED and attacker-settable.

    `manifest_hash()` is taken over `manifest` MINUS `signature`, so the whole
    signature block -- `key_fingerprint` included -- is outside what the
    signature commits to. An attacker forges a payload, re-signs with their own
    key, and leaves `key_fingerprint` reading the published value. Both
    `harness.commit.verify()` (which trusts the embedded public key) and any
    string comparison against that field then pass.

    So this test asserts the ATTACK SUCCEEDS against the naive check, and fails
    against the derivation. If the two ever agreed, one of them stopped being
    the thing it claims to be.

    The attacker key is ephemeral and in memory. `bench/` is never written.
    """
    ledger = json.loads(
        (REPO_ROOT / "bench" / "LEDGER_relian-bench-v1.2.json").read_text(
            encoding="utf-8"
        )
    )
    published = seal_module.EXPECTED_KEY_FINGERPRINT

    commit = _import_commit_py()

    # Forge the block bench.yml's scoring step later enforces (R10).
    ledger["thresholds"]["ber_heldout_min"] = 0.10

    attacker = Ed25519PrivateKey.generate()
    public = attacker.public_key().public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw,
    )
    digest = commit.manifest_hash(ledger)
    ledger["signature"] = {
        "alg": "Ed25519",
        "manifest_sha256": digest,
        "signature_hex": attacker.sign(digest.encode("utf-8")).hex(),
        "public_key_hex": public.hex(),
        # The lie. Unsigned, so it costs the attacker nothing.
        "key_fingerprint": published,
        "signed_at": ledger["signature"]["signed_at"],
    }

    import hashlib

    real = hashlib.sha256(public).hexdigest()[:16]
    assert real != published, "ephemeral key collided with the published one"

    # 1. The embedded-key verifier is satisfied. This is T7.
    assert commit.verify(ledger) is True

    # 2. Reading the declared field ACCEPTS the forgery. The hazard is real.
    assert ledger["signature"]["key_fingerprint"] == published

    # 3. Deriving from the key the signature was verified WITH rejects it.
    derived = hashlib.sha256(
        bytes.fromhex(ledger["signature"]["public_key_hex"])
    ).hexdigest()[:16]
    assert derived == real
    assert derived != published


def test_bench_yml_derives_the_signer_and_never_reads_the_declared_field():
    """The workflow's pin, asserted on the script CI actually runs.

    Asserted on text rather than a parsed workflow because PyYAML is not in
    `requirements.lock`, and the literal code of the step is what matters here
    anyway.

    `bench.yml` verifies with `harness.commit.verify()`, which lives inside the
    sealed tree and cannot be edited (rule 4) and which trusts the manifest's
    embedded key. The pin in the workflow is therefore the ONLY thing standing
    between a re-signed forgery and the `thresholds` block the scoring step
    enforces -- so it has to hash `public_key_hex`, not read a field the
    signature does not cover.
    """
    text = BENCH_WORKFLOW.read_text(encoding="utf-8")

    assert "sha256(bytes.fromhex(embedded)).hexdigest()[:16]" in text, (
        "bench.yml must DERIVE the signer from public_key_hex; a comparison "
        "against signature.key_fingerprint is a comparison against an "
        "unsigned, attacker-settable string"
    )
    assert "assert signer == '233bb4406e2de606'" in text, (
        "the derived signer must be pinned to the published fingerprint"
    )
    # The old, broken form must not come back.
    assert "fingerprint = m['signature']['key_fingerprint']" not in text
    assert "assert fingerprint == '233bb4406e2de606'" not in text
    # The declared field may still be CROSS-CHECKED, which is different from
    # being trusted -- a manifest disagreeing with its own key is a finding.
    assert "assert declared == signer" in text


def test_both_workflows_point_at_v1_3():
    """Acceptance ④, and the reason it is one variable rather than two literals.

    `bench.yml` named LEDGER_relian-bench-v1.2.json in two separate steps. A
    re-seal updating one and missing the other verifies a signature-valid v1.2
    ledger against a tree matching v1.3 -- a green gate proving nothing. One
    assignment cannot go half-updated, so both consumers read $BENCH_LEDGER.
    """
    bench_yml = BENCH_WORKFLOW.read_text(encoding="utf-8")
    tests_yml = (
        REPO_ROOT / ".github" / "workflows" / "tests.yml"
    ).read_text(encoding="utf-8")

    assert "V13=bench/LEDGER_relian-bench-v1.3.json" in bench_yml
    assert bench_yml.count("os.environ['BENCH_LEDGER']") == 2, (
        "both the signature step and the scoring step must read the resolved "
        "ledger, not a hardcoded filename"
    )
    assert "LEDGER=bench/LEDGER_relian-bench-v1.3.json" in tests_yml
    assert "TODO(WP-2.6-seal)" in tests_yml, (
        "the pre-ceremony skip must carry its marker"
    )
