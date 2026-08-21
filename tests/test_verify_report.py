"""WP-2.3 §3.4 — ``tools/verify_report.py``, proven to have teeth per layer.

The same discipline WP-2.0.2 applied to ``tools/verify_manifest.py``: a
verifier whose failure paths have never fired is a verifier nobody should
trust. Each of the four layers is planted red on its own and then reverted, so
a green run means the four checks are four checks rather than one check
reported four times.

The layers, and the defect each one exists to catch:

  FILES             an edited ``report.json``. The signature covers the
                    MANIFEST's bytes, not the report's, so an edited report
                    leaves a perfectly valid signature behind and only this
                    layer notices.
  MANIFEST          a CONSISTENTLY edited manifest — one whose ``files[]``
                    was changed and whose recorded digests were updated to
                    match, which passes FILES.
  INSTANCE          a manifest re-signed by a different key, or one whose
                    declared instance fingerprint does not match the key that
                    actually signed it.
  COUNTERSIGNATURE  a forgery re-signed under an attacker-generated key. The
                    pin is the ONLY layer that catches it, which is why
                    ``--pin-fingerprint`` is required and has no default.

Two behaviours get their own tests because they are customer-facing wording
rather than cryptography: the ``VALID AND UNATTESTED`` verdict and its distinct
exit code, and the refusal of any output that could be read as *Visionblox
signed this* when only the instance layer is present.
"""

from __future__ import annotations

import hashlib
import json
import subprocess
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]
DEMO = REPO_ROOT / "examples" / "demo"
VERIFIER = REPO_ROOT / "tools" / "verify_report.py"
COUNTERSIGN = REPO_ROOT / "tools" / "countersign.py"

EXIT_OK = 0
EXIT_FAILED = 1
EXIT_UNREADABLE = 2
EXIT_UNATTESTED = 3


# --------------------------------------------------------------------------
# Fixtures — a real deliverable, and a real countersignature over it
# --------------------------------------------------------------------------


def _build_report(base: Path, customer: str = "Test Agency") -> Path:
    """Build a deliverable under ``base``. Each caller gets its OWN directory:
    two reports sharing one output directory would silently overwrite each
    other's manifest and leave a countersignature pointing at a report that no
    longer exists, which is a fixture bug wearing the costume of a finding."""
    base.mkdir(parents=True, exist_ok=True)
    home = base / "home"
    home.mkdir(exist_ok=True)
    out = base / "delivery"
    proc = subprocess.run(
        [sys.executable, "-m", "src.discovery.cli", "report", "build", str(DEMO),
         "--out", str(out), "--customer", customer,
         "--engagement", "Data Discovery", "--contract-vehicle", "TEST-001",
         "--root-label", "examples/demo", "--home", str(home)],
        cwd=str(REPO_ROOT), capture_output=True, text=True, timeout=300,
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr
    return out


def _make_key(path: Path) -> str:
    from cryptography.hazmat.primitives import serialization
    from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey

    private = Ed25519PrivateKey.generate()
    path.write_bytes(
        private.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption(),
        )
    )
    raw = private.public_key().public_bytes(
        encoding=serialization.Encoding.Raw, format=serialization.PublicFormat.Raw
    )
    return hashlib.sha256(raw).hexdigest()[:16]


def _add_countersignature(delivery: Path, key_path: Path, fingerprint: str) -> None:
    manifest = json.loads((delivery / "report.manifest.json").read_text(encoding="utf-8"))
    proc = subprocess.run(
        [sys.executable, str(COUNTERSIGN),
         "--manifest-hash", manifest["signature"]["manifest_sha256"],
         "--report-id", manifest["report_id"],
         "--instance-fingerprint", manifest["signature"]["key_fingerprint"],
         "--key", str(key_path),
         "--out", str(delivery / "report.countersig.json"),
         "--expect-key-fingerprint", fingerprint],
        cwd=str(REPO_ROOT), capture_output=True, text=True, timeout=120,
    )
    assert proc.returncode == 0, proc.stderr


def _verify(delivery: Path, pin: str, *extra):
    return subprocess.run(
        [sys.executable, str(VERIFIER), "--report-dir", str(delivery),
         "--pin-fingerprint", pin, *extra],
        cwd=str(REPO_ROOT), capture_output=True, text=True, timeout=120,
    )


def _layers(proc) -> dict:
    return {row["layer"]: row["verdict"] for row in json.loads(proc.stdout)["layers"]}


@pytest.fixture()
def unattested(tmp_path) -> Path:
    """Instance-signed, no countersignature. What the customer holds first."""
    return _build_report(tmp_path / "unattested")


@pytest.fixture()
def attested(tmp_path):
    """The full deliverable, countersigned by a stand-in release key.

    The real release key is the operator's and never enters this repository,
    CI, or a test (R4). What is exercised here is the FLOW, under a key
    generated for this test and pinned by its own fingerprint.
    """
    delivery = _build_report(tmp_path / "attested")
    key_path = tmp_path / "release.pem"
    fingerprint = _make_key(key_path)
    _add_countersignature(delivery, key_path, fingerprint)
    return delivery, fingerprint, key_path


# --------------------------------------------------------------------------
# Green, and the "valid and unattested" path (acceptance ④)
# --------------------------------------------------------------------------


def test_all_four_layers_pass_on_a_countersigned_deliverable(attested) -> None:
    delivery, fingerprint, _key = attested
    proc = _verify(delivery, fingerprint, "--json")
    assert proc.returncode == EXIT_OK, proc.stdout + proc.stderr
    assert _layers(proc) == {
        "FILES": "PASS", "MANIFEST": "PASS",
        "INSTANCE": "PASS", "COUNTERSIGNATURE": "PASS",
    }


def test_an_uncountersigned_report_is_valid_and_unattested_in_those_words(
    unattested,
) -> None:
    """Acceptance ④. The wording is the deliverable here, not only the code."""
    proc = _verify(unattested, "91e3a404155ba4dd")
    assert proc.returncode == EXIT_UNATTESTED, proc.stdout + proc.stderr
    assert "VALID AND UNATTESTED" in proc.stdout
    assert "Visionblox has NOT attested to it" in proc.stdout


def test_the_unattested_exit_code_is_distinct_from_both_pass_and_fail(
    attested, unattested
) -> None:
    """Green-by-skip is the failure this separates. 'Unattested' must not
    share an exit code with 'attested', and must not share one with 'broken'."""
    delivery, fingerprint, _key = attested
    assert _verify(delivery, fingerprint).returncode == EXIT_OK
    assert _verify(unattested, fingerprint).returncode == EXIT_UNATTESTED
    (unattested / "report.json").write_bytes(b"{}\n")
    assert _verify(unattested, fingerprint).returncode == EXIT_FAILED
    assert EXIT_UNATTESTED not in (EXIT_OK, EXIT_FAILED)


def test_the_unattested_run_reports_the_countersignature_layer_as_absent(
    unattested,
) -> None:
    """ABSENT, not PASS and not FAIL. Silence about a missing layer is the
    green-by-skip failure on a customer-facing surface."""
    proc = _verify(unattested, "91e3a404155ba4dd", "--json")
    assert _layers(proc)["COUNTERSIGNATURE"] == "ABSENT"
    assert _layers(proc)["INSTANCE"] == "PASS"


def test_no_output_can_be_read_as_visionblox_signed_this(unattested) -> None:
    """Escalation trigger 4. A misrepresentation to a government customer is a
    different category of problem from a bug."""
    proc = _verify(unattested, "91e3a404155ba4dd")
    assert "NOT a Visionblox attestation" in proc.stdout
    assert "Do not read the instance signature as a Visionblox signature" in proc.stdout
    assert "does NOT prove identity of signer" in proc.stdout


def test_the_verifier_prints_its_own_sha256(unattested) -> None:
    """So a customer can confirm against the Technical Delivery Sheet that
    they are running the tool that was shipped to them."""
    proc = _verify(unattested, "91e3a404155ba4dd")
    expected = hashlib.sha256(VERIFIER.read_bytes()).hexdigest()
    assert expected in proc.stdout


# --------------------------------------------------------------------------
# Acceptance ③ — each layer planted red on its own, then reverted
# --------------------------------------------------------------------------


def test_layer_files_fails_alone_on_an_edited_report(attested) -> None:
    delivery, fingerprint, _key = attested
    path = delivery / "report.json"
    original = path.read_bytes()
    path.write_bytes(original.replace(b"Test Agency", b"Other Agency"))
    proc = _verify(delivery, fingerprint, "--json")
    verdicts = _layers(proc)
    assert proc.returncode == EXIT_FAILED
    assert verdicts["FILES"] == "FAIL"
    assert verdicts["MANIFEST"] == "PASS", "an edited report must not break MANIFEST"
    assert verdicts["INSTANCE"] == "PASS", "the signature covers the manifest, not the report"
    assert verdicts["COUNTERSIGNATURE"] == "PASS"

    path.write_bytes(original)                                   # revert
    assert _verify(delivery, fingerprint).returncode == EXIT_OK


def test_layer_files_fails_on_a_recorded_file_that_is_missing(attested) -> None:
    delivery, fingerprint, _key = attested
    path = delivery / "report.md"
    original = path.read_bytes()
    path.unlink()
    proc = _verify(delivery, fingerprint, "--json")
    assert _layers(proc)["FILES"] == "FAIL"
    assert "absent on disk" in proc.stdout

    path.write_bytes(original)                                   # revert
    assert _verify(delivery, fingerprint).returncode == EXIT_OK


def test_layer_manifest_fails_alone_on_a_consistently_edited_manifest(attested) -> None:
    """The attack FILES cannot see: change the report AND update its recorded
    digest so layer 1 is satisfied."""
    delivery, fingerprint, _key = attested
    report_path = delivery / "report.json"
    manifest_path = delivery / "report.manifest.json"
    original_report = report_path.read_bytes()
    original_manifest = manifest_path.read_bytes()

    edited = original_report.replace(b"Test Agency", b"Other Agency")
    report_path.write_bytes(edited)
    manifest = json.loads(original_manifest.decode("utf-8"))
    for entry in manifest["files"]:
        if entry["path"] == "report.json":
            entry["sha256"] = hashlib.sha256(edited).hexdigest()
    manifest_path.write_text(
        json.dumps(manifest, sort_keys=True, separators=(",", ":"),
                   ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    proc = _verify(delivery, fingerprint, "--json")
    verdicts = _layers(proc)
    assert verdicts["FILES"] == "PASS", "the consistent edit did not satisfy layer 1"
    assert verdicts["MANIFEST"] == "FAIL"
    assert proc.returncode == EXIT_FAILED

    report_path.write_bytes(original_report)                     # revert
    manifest_path.write_bytes(original_manifest)
    assert _verify(delivery, fingerprint).returncode == EXIT_OK


def test_layer_instance_fails_alone_on_a_corrupted_signature(attested) -> None:
    delivery, fingerprint, _key = attested
    manifest_path = delivery / "report.manifest.json"
    original = manifest_path.read_bytes()
    manifest = json.loads(original.decode("utf-8"))
    hex_sig = manifest["signature"]["signature_hex"]
    flipped = ("0" if hex_sig[0] != "0" else "1") + hex_sig[1:]
    manifest["signature"]["signature_hex"] = flipped
    manifest_path.write_text(
        json.dumps(manifest, sort_keys=True, separators=(",", ":"),
                   ensure_ascii=False) + "\n", encoding="utf-8",
    )
    proc = _verify(delivery, fingerprint, "--json")
    verdicts = _layers(proc)
    assert verdicts["INSTANCE"] == "FAIL"
    # signature_hex is excluded from nothing -- it lives INSIDE the signature
    # block, which manifest_hash() drops -- so layer 2 stays green and the
    # failure is legible as an instance-signature failure specifically.
    assert verdicts["MANIFEST"] == "PASS"
    assert verdicts["FILES"] == "PASS"

    manifest_path.write_bytes(original)                          # revert
    assert _verify(delivery, fingerprint).returncode == EXIT_OK


def test_layer_instance_fails_when_the_declared_fingerprint_is_not_the_signer(
    attested,
) -> None:
    delivery, fingerprint, _key = attested
    manifest_path = delivery / "report.manifest.json"
    original = manifest_path.read_bytes()
    manifest = json.loads(original.decode("utf-8"))
    manifest["instance_key"]["fingerprint"] = "0" * 16
    manifest_path.write_text(
        json.dumps(manifest, sort_keys=True, separators=(",", ":"),
                   ensure_ascii=False) + "\n", encoding="utf-8",
    )
    proc = _verify(delivery, fingerprint, "--json")
    assert _layers(proc)["INSTANCE"] == "FAIL"
    assert "declares instance key" in proc.stdout

    manifest_path.write_bytes(original)                          # revert
    assert _verify(delivery, fingerprint).returncode == EXIT_OK


def test_layer_countersignature_fails_alone_on_a_countersignature_for_another_report(
    attested, tmp_path
) -> None:
    delivery, fingerprint, key_path = attested
    original = (delivery / "report.countersig.json").read_bytes()
    # A DIFFERENT customer, so the two reports differ in content rather than
    # only in their one-second-resolution generated_at -- otherwise two builds
    # in the same second would produce the same manifest hash and this test
    # would pass or fail on timing.
    other = _build_report(tmp_path / "second", customer="Another Agency")
    other_manifest = json.loads(
        (other / "report.manifest.json").read_text(encoding="utf-8")
    )
    this_manifest = json.loads(
        (delivery / "report.manifest.json").read_text(encoding="utf-8")
    )
    assert (other_manifest["signature"]["manifest_sha256"]
            != this_manifest["signature"]["manifest_sha256"]), (
        "the two fixtures hash identically, so this test proves nothing"
    )
    _add_countersignature(other, key_path, fingerprint)
    (delivery / "report.countersig.json").write_bytes(
        (other / "report.countersig.json").read_bytes()
    )
    proc = _verify(delivery, fingerprint, "--json")
    verdicts = _layers(proc)
    assert verdicts["COUNTERSIGNATURE"] == "FAIL"
    assert verdicts["FILES"] == "PASS"
    assert verdicts["MANIFEST"] == "PASS"
    assert verdicts["INSTANCE"] == "PASS"
    assert "for a DIFFERENT report" in proc.stdout

    (delivery / "report.countersig.json").write_bytes(original)  # revert
    assert _verify(delivery, fingerprint).returncode == EXIT_OK


def test_layer_countersignature_fails_on_a_mismatched_instance_fingerprint(
    attested,
) -> None:
    """`countersign.py` records `instance_fingerprint` and tells the operator
    the verifier checks it against the manifest. It did not — only `report_id`
    was compared — so the mismatch stayed silent. Found by review on PR #31.

    This cannot pass a forgery: the signature covers the manifest hash, which
    already commits to the instance key, so layer 4 was never unsound. What it
    was is a verifier naming a check it did not perform, on the one surface a
    customer is told to trust — and the likeliest real cause of a mismatch, a
    countersignature built from the wrong request line, went unreported.
    """
    delivery, fingerprint, _key = attested
    path = delivery / "report.countersig.json"
    original = path.read_bytes()
    block = json.loads(original.decode("utf-8"))
    assert block["instance_fingerprint"], "the fixture carries no fingerprint to break"
    block["instance_fingerprint"] = "0" * 16
    path.write_text(
        json.dumps(block, sort_keys=True, separators=(",", ":"),
                   ensure_ascii=False) + "\n", encoding="utf-8",
    )
    proc = _verify(delivery, fingerprint, "--json")
    verdicts = _layers(proc)
    assert verdicts["COUNTERSIGNATURE"] == "FAIL"
    assert verdicts["FILES"] == "PASS"
    assert verdicts["MANIFEST"] == "PASS"
    assert verdicts["INSTANCE"] == "PASS"
    assert "do not describe the same installation" in proc.stdout
    assert proc.returncode == EXIT_FAILED

    path.write_bytes(original)                                   # revert
    assert _verify(delivery, fingerprint).returncode == EXIT_OK


def test_countersign_records_exactly_the_advisory_fields_the_verifier_checks() -> None:
    """The pairing that broke. Every advisory field `countersign.py` writes
    must have a comparison in `verify_report.py`, or the comment promising one
    is prose the code does not keep."""
    countersign_source = COUNTERSIGN.read_text(encoding="utf-8")
    verifier_source = VERIFIER.read_text(encoding="utf-8")
    for advisory in ("report_id", "instance_fingerprint"):
        assert f'"{advisory}": {advisory}' in countersign_source, (
            f"{advisory} is no longer recorded by countersign.py"
        )
        assert f'countersig.get("{advisory}")' in verifier_source, (
            f"countersign.py records {advisory} and verify_report.py never "
            f"reads it, so a mismatch would be silent"
        )


# --------------------------------------------------------------------------
# Acceptance ⑤ — the pin is required, and it is the only thing that catches a
# re-signed forgery
# --------------------------------------------------------------------------


def test_pin_fingerprint_is_required_and_has_no_default(unattested) -> None:
    proc = subprocess.run(
        [sys.executable, str(VERIFIER), "--report-dir", str(unattested)],
        cwd=str(REPO_ROOT), capture_output=True, text=True, timeout=120,
    )
    assert proc.returncode != 0
    assert "--pin-fingerprint" in proc.stderr
    assert "required" in proc.stderr


def test_a_resigned_forgery_passes_three_layers_and_fails_the_pin(
    attested, tmp_path
) -> None:
    """Acceptance ⑤, and the WP-2.0.2 finding restated on this surface.

    An attacker who re-signs a deliverable they control has every
    internally-consistent layer green: the files match the manifest, the
    manifest hash recomputes, and the instance signature verifies under the
    key they generated. Only the pin sees it.
    """
    delivery, real_fingerprint, _key = attested
    attacker_key = tmp_path / "attacker.pem"
    attacker_fp = _make_key(attacker_key)

    # Re-sign the manifest end to end under the attacker's key, exactly as a
    # forger would: instance block AND countersignature.
    manifest_path = delivery / "report.manifest.json"
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    _resign(manifest, attacker_key, attacker_fp)
    manifest_path.write_text(
        json.dumps(manifest, sort_keys=True, separators=(",", ":"),
                   ensure_ascii=False) + "\n", encoding="utf-8",
    )
    _add_countersignature(delivery, attacker_key, attacker_fp)

    forged = _verify(delivery, attacker_fp, "--json")
    assert forged.returncode == EXIT_OK, (
        "the forgery is not internally consistent, so this test is not "
        "exercising what it claims to"
    )

    pinned = _verify(delivery, real_fingerprint, "--json")
    verdicts = _layers(pinned)
    assert verdicts["FILES"] == "PASS"
    assert verdicts["MANIFEST"] == "PASS"
    assert verdicts["INSTANCE"] == "PASS"
    assert verdicts["COUNTERSIGNATURE"] == "FAIL"
    assert pinned.returncode == EXIT_FAILED
    assert "does not prove Visionblox did" in pinned.stdout


def _resign(manifest: dict, key_path: Path, fingerprint: str) -> None:
    from cryptography.hazmat.primitives import serialization

    private = serialization.load_pem_private_key(key_path.read_bytes(), password=None)
    public_hex = private.public_key().public_bytes(
        encoding=serialization.Encoding.Raw, format=serialization.PublicFormat.Raw
    ).hex()
    manifest["instance_key"]["fingerprint"] = fingerprint
    manifest["instance_key"]["public_key_hex"] = public_hex
    core = {k: v for k, v in manifest.items() if k != "signature"}
    digest = hashlib.sha256(
        json.dumps(core, sort_keys=True, separators=(",", ":"),
                   ensure_ascii=False).encode("utf-8")
    ).hexdigest()
    manifest["signature"] = dict(manifest["signature"])
    manifest["signature"]["manifest_sha256"] = digest
    manifest["signature"]["public_key_hex"] = public_hex
    manifest["signature"]["key_fingerprint"] = fingerprint
    manifest["signature"]["signature_hex"] = private.sign(
        digest.encode("utf-8")
    ).hex()


def test_the_instance_pin_is_optional_and_checked_when_given(attested) -> None:
    delivery, fingerprint, _key = attested
    manifest = json.loads((delivery / "report.manifest.json").read_text(encoding="utf-8"))
    real = manifest["instance_key"]["fingerprint"]
    assert _verify(delivery, fingerprint,
                   "--pin-instance-fingerprint", real).returncode == EXIT_OK
    proc = _verify(delivery, fingerprint,
                   "--pin-instance-fingerprint", "f" * 16, "--json")
    assert _layers(proc)["INSTANCE"] == "FAIL"
    assert proc.returncode == EXIT_FAILED


def test_without_an_instance_pin_the_verifier_says_what_it_did_not_check(
    unattested,
) -> None:
    """Rather than overclaiming. An instance key has no published fingerprint
    to pin against on a first report, and saying so is better than silence."""
    proc = _verify(unattested, "91e3a404155ba4dd")
    assert "No --pin-instance-fingerprint was given" in proc.stdout


# --------------------------------------------------------------------------
# Refusals — an unreadable deliverable is not a failed one
# --------------------------------------------------------------------------


def test_an_absent_manifest_is_unreadable_not_failed(tmp_path) -> None:
    empty = tmp_path / "empty"
    empty.mkdir()
    proc = _verify(empty, "91e3a404155ba4dd")
    assert proc.returncode == EXIT_UNREADABLE
    assert "UNREADABLE" in proc.stderr


def test_an_unknown_manifest_schema_is_refused_rather_than_guessed(unattested) -> None:
    path = unattested / "report.manifest.json"
    manifest = json.loads(path.read_text(encoding="utf-8"))
    manifest["schema"] = "relian-discovery-report-manifest/99"
    path.write_text(json.dumps(manifest), encoding="utf-8")
    proc = _verify(unattested, "91e3a404155ba4dd")
    assert proc.returncode == EXIT_UNREADABLE
    assert "unknown manifest schema" in proc.stderr


def test_the_verifier_imports_nothing_from_the_relian_source_tree() -> None:
    """D27. A verifier that calls the signer's own code cannot detect a bug in
    the signer's own code — and a customer must be able to run it from a
    directory that holds nothing but the deliverable."""
    import ast

    tree = ast.parse(VERIFIER.read_text(encoding="utf-8"))
    imported = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            imported.update(a.name.split(".")[0] for a in node.names)
        elif isinstance(node, ast.ImportFrom):
            if node.level:
                imported.add(".")
            imported.add((node.module or "").split(".")[0])
    assert not ({"src", "tests", "bench", "tools", "."} & imported), sorted(imported)
    assert imported <= {
        "argparse", "hashlib", "json", "sys", "dataclasses", "datetime",
        "pathlib", "typing", "cryptography", "__future__",
    }, sorted(imported)


def test_the_verifier_reaches_no_network_and_reads_no_credential() -> None:
    source = VERIFIER.read_text(encoding="utf-8").lower()
    for token in ("urllib", "requests", "socket", "http", "os.environ", "getenv"):
        assert token not in source.replace("https://", ""), (
            f"the verifier mentions {token!r}"
        )


# --------------------------------------------------------------------------
# The committed dry runs — a regression gate on the artifacts themselves
# --------------------------------------------------------------------------

DRYRUNS = REPO_ROOT / "docs" / "dryruns"


@pytest.mark.parametrize("name", ["report_demo", "report_carddemo"])
def test_the_committed_dry_run_still_verifies(name: str) -> None:
    """docs/dryruns/ holds two real deliverables. Re-verifying them here means
    an edit to a committed ``report.json``, ``report.md`` or manifest turns the
    build red rather than sitting in the tree looking signed."""
    delivery = DRYRUNS / name
    assert delivery.is_dir(), f"{delivery} is missing"
    proc = _verify(delivery, "91e3a404155ba4dd", "--json")
    assert proc.returncode == EXIT_UNATTESTED, proc.stdout + proc.stderr
    verdicts = _layers(proc)
    assert verdicts["FILES"] == "PASS"
    assert verdicts["MANIFEST"] == "PASS"
    assert verdicts["INSTANCE"] == "PASS"
    # No countersignature is committed, and none should be: that is an
    # operator key session, the same as a benchmark seal (R4).
    assert verdicts["COUNTERSIGNATURE"] == "ABSENT"


def test_no_private_key_is_committed_under_the_dry_runs() -> None:
    """R4. The dry runs publish a fingerprint and nothing else."""
    for path in DRYRUNS.rglob("*"):
        if path.is_file():
            head = path.read_bytes()[:200]
            assert b"PRIVATE KEY" not in head, f"{path} looks like a key"


def test_the_carddemo_dry_run_carries_the_missing_copybook_table() -> None:
    """Acceptance ⑧: populated, with counts measured in the run."""
    report = json.loads(
        (DRYRUNS / "report_carddemo" / "report.json").read_text(encoding="utf-8")
    )
    section = next(s for s in report["sections"] if s["id"] == "missing_copybooks")
    assert section["count"]["value"] == 8
    assert len(section["rows"]) == 8
    assert {r["name"] for r in section["rows"]} == {
        "DFHAID", "DFHBMSCA", "CMQGMOV", "CMQMDV",
        "CMQODV", "CMQPMOV", "CMQTML", "CMQV",
    }
    for row in section["rows"]:
        assert row["referenced_by_count"] >= 1
        assert len(row["referenced_by"]) == row["referenced_by_count"]
        assert row["paths_searched_count"] >= 1


def test_the_carddemo_dry_run_counts_match_the_wp_2_2_measurement() -> None:
    """The resolver has not drifted since WP-2.2 measured this corpus. If it
    ever does, the run is authoritative and this assertion is where it shows."""
    report = json.loads(
        (DRYRUNS / "report_carddemo" / "report.json").read_text(encoding="utf-8")
    )
    counts = next(s for s in report["sections"] if s["id"] == "scope")["counts"]
    assert {k: v["value"] for k, v in counts.items()} == {
        "source_files_scanned": 106,
        "files_with_at_least_one_copy": 40,
        "copy_directive_sites": 346,
        "distinct_names_referenced": 67,
        "resolvable": 59,
        "unresolvable": 8,
        "edges": 306,
        "copy_replacing_sites": 40,
        "cycles": 0,
        "members_present_but_unreferenced": 3,
    }


def test_the_carddemo_dry_run_states_why_it_has_no_layouts() -> None:
    """R2. Empty by decision, and the decision is named in the artifact."""
    report = json.loads(
        (DRYRUNS / "report_carddemo" / "report.json").read_text(encoding="utf-8")
    )
    section = next(s for s in report["sections"] if s["id"] == "record_layouts")
    assert section["included"] is False
    assert "by decision, not by failure" in section["reason_not_included"]
    assert "WP-2.2" in section["reason_not_included"]


def test_the_demo_dry_run_includes_computed_layouts() -> None:
    report = json.loads(
        (DRYRUNS / "report_demo" / "report.json").read_text(encoding="utf-8")
    )
    section = next(s for s in report["sections"] if s["id"] == "record_layouts")
    assert section["included"] is True
    assert section["count"]["value"] == len(section["records"]) == 3
    assert all(r["status"] == "COMPLETE" for r in section["records"])
