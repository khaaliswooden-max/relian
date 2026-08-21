#!/usr/bin/env python3
"""WP-2.3 §3.4 — standalone verifier for a Relian Data Discovery report.

Four layers, each NAMED and each failing INDEPENDENTLY:

  LAYER 1  FILES             every path recorded in the manifest exists under
                             --report-dir and hashes to its recorded SHA-256.
                             This is the layer that catches an edited report:
                             the signature covers the MANIFEST's bytes, not the
                             report's, so a changed report.json still leaves a
                             perfectly valid signature behind.
  LAYER 2  MANIFEST          manifest_sha256, recomputed from the manifest
                             minus its `signature` block, equals the value the
                             signature was taken over. This catches a
                             CONSISTENTLY edited manifest -- one whose files[]
                             was changed and whose digests were updated to
                             match, which passes layer 1.
  LAYER 3  INSTANCE          the Ed25519 instance signature verifies over that
                             manifest hash, and the embedded public key's
                             fingerprint matches the one the manifest records
                             for the instance key.
  LAYER 4  COUNTERSIGNATURE  the detached report.countersig.json verifies over
                             the SAME manifest hash, under a release-key
                             fingerprint the caller PINS; and its two advisory
                             fields -- report id and instance fingerprint --
                             describe THIS report. The advisory fields cannot
                             let a forgery through, because the signature
                             covers the manifest hash and that already commits
                             to both; they are checked because countersign.py
                             says they are, and a verifier that names a check
                             it does not perform is the defect this package
                             exists to avoid.

Every layer is evaluated even when an earlier one fails, so one run tells you
the whole picture rather than the first thing that broke.

WHY --pin-fingerprint IS REQUIRED AND NOT OPTIONAL
---------------------------------------------------
WP-2.0.2 measured this on the benchmark verifier: `verify()` that trusts the
public key embedded in the object it is checking proves self-consistency, not
authorship. Anyone can re-sign an edited artifact with a key they generated,
and every internally-consistent layer passes. Pinning the expected signer is
the ONLY check that catches it. An optional pin is a pin that gets left off, so
there is no default and no way to run without one. `tests/test_verify_report.py`
re-signs a valid deliverable under an ephemeral attacker key and asserts layers
1-3 pass while layer 4 fails on the pin.

The instance layer is pinned differently, and deliberately so. Its key is
per-installation and Visionblox has never held it, so there is no published
fingerprint to pin it against -- the manifest's own record is all there is.
`--pin-instance-fingerprint` is available for a customer who noted the
fingerprint from an earlier report and wants continuity checked; without it the
verdict says what the layer does and does not prove rather than overclaiming.

WHAT A GREEN INSTANCE LAYER WITH NO COUNTERSIGNATURE MEANS
-----------------------------------------------------------
    VALID AND UNATTESTED

in those words, with its own exit code (3). It means the report is intact and
came out of the Relian installation whose fingerprint the manifest records --
and that Visionblox has NOT attested to it. Silence about a missing layer would
be green-by-skip on a customer-facing surface, and wording that let a reader
take the instance layer for a Visionblox signature would be a misrepresentation
to a government customer, which is a different category of problem from a bug.

WHAT THIS FILE IS
------------------
Standalone. Standard library plus `cryptography`. No import from the Relian
source tree -- in particular not from `src/discovery/signing.py`, whose hashing
it reimplements from the format description above, because a verifier that
calls the signer's own code cannot detect a bug in the signer's own code. It
reads no credentials, opens no network connection, and writes nothing.

It prints its own SHA-256 on every run, so a customer can confirm against the
Technical Delivery Sheet that they are running the tool that was shipped to
them rather than one that arrived by another route.

EXIT STATUS
-----------
    0  all four layers passed -- valid and attested by Visionblox
    3  VALID AND UNATTESTED -- layers 1-3 passed, no countersignature present
    1  a named layer failed
    2  the deliverable could not be read at all

USAGE
-----
    python3 tools/verify_report.py --report-dir <dir> \\
        --pin-fingerprint 91e3a404155ba4dd
"""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence

MANIFEST_NAME = "report.manifest.json"
COUNTERSIG_NAME = "report.countersig.json"

#: Schemas this verifier understands. Meeting an unknown one is a refusal, not
#: a best-effort parse: a field that moved between formats would otherwise be
#: read as absent and reported as a pass.
KNOWN_MANIFEST_SCHEMAS = ("relian-discovery-report-manifest/1",)
KNOWN_COUNTERSIG_SCHEMAS = ("relian-discovery-report-countersig/1",)

UNATTESTED_WORDING = "VALID AND UNATTESTED"


# --- canonicalisation, reimplemented from the format description -----------

def canonical(obj: Any) -> str:
    """Sorted keys, no insignificant whitespace, UTF-8, non-ASCII preserved."""
    return json.dumps(obj, sort_keys=True, separators=(",", ":"), ensure_ascii=False)


def manifest_hash(manifest: Dict[str, Any]) -> str:
    core = {k: v for k, v in manifest.items() if k != "signature"}
    return hashlib.sha256(canonical(core).encode("utf-8")).hexdigest()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def fingerprint(public_key_hex: str) -> str:
    return hashlib.sha256(bytes.fromhex(public_key_hex)).hexdigest()[:16]


# --- results ---------------------------------------------------------------


@dataclass
class LayerResult:
    name: str
    ok: bool
    detail: str
    absent: bool = False
    notes: List[str] = field(default_factory=list)

    @property
    def verdict(self) -> str:
        if self.absent:
            return "ABSENT"
        return "PASS" if self.ok else "FAIL"

    def to_dict(self) -> Dict[str, Any]:
        return {
            "layer": self.name,
            "verdict": self.verdict,
            "detail": self.detail,
            "notes": list(self.notes),
        }


# --- the four layers -------------------------------------------------------


def check_files(manifest: Dict[str, Any], report_dir: Path) -> LayerResult:
    entries = manifest.get("files")
    if not isinstance(entries, list) or not entries:
        return LayerResult(
            "FILES", False,
            "the manifest records no files[]; there is nothing to check the "
            "artifacts against, and an empty check is not a passing one",
        )
    problems: List[str] = []
    checked = 0
    for entry in entries:
        rel = entry.get("path")
        recorded = (entry.get("sha256") or "").lower()
        if not rel or not recorded:
            problems.append(f"malformed files[] entry: {entry!r}")
            continue
        path = report_dir / rel
        if not path.is_file():
            problems.append(f"{rel}: recorded in the manifest, absent on disk")
            continue
        actual = sha256_file(path)
        if actual != recorded:
            problems.append(
                f"{rel}: sha256 {actual} on disk, {recorded} in the manifest"
            )
            continue
        checked += 1
    if problems:
        return LayerResult(
            "FILES", False,
            f"{len(problems)} problem(s) across {len(entries)} recorded file(s)",
            notes=problems,
        )
    return LayerResult(
        "FILES", True,
        f"{checked} of {len(entries)} recorded file(s) present and matching",
    )


def check_manifest(manifest: Dict[str, Any]) -> LayerResult:
    block = manifest.get("signature")
    if not isinstance(block, dict):
        return LayerResult(
            "MANIFEST", False,
            "the manifest carries no signature block, so there is no recorded "
            "hash to recompute against",
        )
    recorded = (block.get("manifest_sha256") or "").lower()
    actual = manifest_hash(manifest)
    if not recorded:
        return LayerResult("MANIFEST", False,
                           "the signature block records no manifest_sha256")
    if actual != recorded:
        return LayerResult(
            "MANIFEST", False,
            f"recomputed {actual}, signature was taken over {recorded}: the "
            f"manifest has been edited since it was signed",
        )
    return LayerResult("MANIFEST", True, f"manifest hash {actual} recomputes")


def check_instance(
    manifest: Dict[str, Any], pin: Optional[str] = None
) -> LayerResult:
    from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PublicKey

    block = manifest.get("signature")
    if not isinstance(block, dict):
        return LayerResult("INSTANCE", False, "no instance signature block")
    embedded = block.get("public_key_hex") or ""
    recorded = (block.get("manifest_sha256") or "").lower()
    signature = block.get("signature_hex") or ""
    if not (embedded and recorded and signature):
        return LayerResult(
            "INSTANCE", False,
            "the instance signature block is incomplete (needs public_key_hex, "
            "manifest_sha256 and signature_hex)",
        )
    try:
        actual_fp = fingerprint(embedded)
    except ValueError:
        return LayerResult("INSTANCE", False,
                           "public_key_hex is not valid hexadecimal")

    declared = ((manifest.get("instance_key") or {}).get("fingerprint") or "").lower()
    if declared and declared != actual_fp:
        return LayerResult(
            "INSTANCE", False,
            f"the manifest declares instance key {declared} but the signature "
            f"was made by {actual_fp}",
        )
    if pin and pin.lower() != actual_fp:
        return LayerResult(
            "INSTANCE", False,
            f"pinned instance fingerprint {pin.lower()} does not match the "
            f"signing key {actual_fp}",
        )
    try:
        Ed25519PublicKey.from_public_bytes(bytes.fromhex(embedded)).verify(
            bytes.fromhex(signature), recorded.encode("utf-8")
        )
    except Exception as exc:                           # noqa: BLE001 - reported
        return LayerResult(
            "INSTANCE", False,
            f"the Ed25519 instance signature does not verify: "
            f"{type(exc).__name__}",
        )
    notes = [
        "This layer proves integrity and provenance-of-tool: the report came "
        "out of the Relian installation holding instance key "
        f"{actual_fp} and has not changed since. It does NOT prove identity "
        "of signer, and it is NOT a Visionblox attestation — Visionblox has "
        "never held this key.",
    ]
    if not pin:
        notes.append(
            "No --pin-instance-fingerprint was given, so this layer was "
            "checked against the fingerprint the manifest itself records. "
            "That is all an instance key can be checked against on a first "
            "report; pin it on the next one to check continuity."
        )
    return LayerResult("INSTANCE", True, f"instance key {actual_fp} verifies",
                       notes=notes)


def check_countersignature(
    manifest: Dict[str, Any], countersig: Optional[Dict[str, Any]], pin: str
) -> LayerResult:
    from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PublicKey

    if countersig is None:
        return LayerResult(
            "COUNTERSIGNATURE", False,
            f"no {COUNTERSIG_NAME} in this directory. The report is "
            f"{UNATTESTED_WORDING}: intact and produced by the recorded "
            f"Relian installation, and NOT attested by Visionblox.",
            absent=True,
        )
    schema = countersig.get("schema")
    if schema not in KNOWN_COUNTERSIG_SCHEMAS:
        return LayerResult(
            "COUNTERSIGNATURE", False,
            f"unknown countersignature schema {schema!r}; this verifier "
            f"understands {', '.join(KNOWN_COUNTERSIG_SCHEMAS)}",
        )
    embedded = countersig.get("public_key_hex") or ""
    signed_over = (countersig.get("manifest_sha256") or "").lower()
    signature = countersig.get("signature_hex") or ""
    if not (embedded and signed_over and signature):
        return LayerResult("COUNTERSIGNATURE", False,
                           "the countersignature is incomplete")

    ours = (((manifest.get("signature") or {}).get("manifest_sha256")) or "").lower()
    if ours and signed_over != ours:
        return LayerResult(
            "COUNTERSIGNATURE", False,
            f"this countersignature is for a DIFFERENT report: it covers "
            f"manifest hash {signed_over}, and this report's manifest hashes "
            f"to {ours}",
        )
    # The two advisory fields countersign.py records. Neither is what the
    # signature covers -- the manifest hash is, and it already commits to both
    # -- so a mismatch here cannot let a forgery through. It is checked anyway,
    # because countersign.py TELLS the operator these are checked, and a
    # verifier that names a check it does not perform is the defect this whole
    # package exists to avoid. Silence would also hide the likelier cause: a
    # countersignature built from the wrong request line.
    declared_id = countersig.get("report_id")
    if declared_id and declared_id != manifest.get("report_id"):
        return LayerResult(
            "COUNTERSIGNATURE", False,
            f"this countersignature names report id {declared_id}, and this "
            f"report is {manifest.get('report_id')}",
        )
    declared_instance = (countersig.get("instance_fingerprint") or "").lower()
    ours_instance = (
        ((manifest.get("signature") or {}).get("key_fingerprint")) or ""
    ).lower()
    if declared_instance and ours_instance and declared_instance != ours_instance:
        return LayerResult(
            "COUNTERSIGNATURE", False,
            f"this countersignature names instance key {declared_instance}, "
            f"and this report was instance-signed by {ours_instance}. The "
            f"countersignature request line and this report do not describe "
            f"the same installation.",
        )
    try:
        actual_fp = fingerprint(embedded)
    except ValueError:
        return LayerResult("COUNTERSIGNATURE", False,
                           "public_key_hex is not valid hexadecimal")
    # Pinned BEFORE the cryptographic check: a valid signature by the wrong key
    # is precisely the failure an unpinned verify cannot see.
    if actual_fp != pin.lower():
        return LayerResult(
            "COUNTERSIGNATURE", False,
            f"signed by key {actual_fp}, and you pinned {pin.lower()}. A "
            f"signature by an unpinned key proves only that SOMEONE signed "
            f"this; it does not prove Visionblox did.",
        )
    try:
        Ed25519PublicKey.from_public_bytes(bytes.fromhex(embedded)).verify(
            bytes.fromhex(signature), signed_over.encode("utf-8")
        )
    except Exception as exc:                           # noqa: BLE001 - reported
        return LayerResult(
            "COUNTERSIGNATURE", False,
            f"the Ed25519 countersignature does not verify: {type(exc).__name__}",
        )
    return LayerResult(
        "COUNTERSIGNATURE", True,
        f"Visionblox release key {actual_fp} attests to this report",
    )


# --- driver ----------------------------------------------------------------


def verify(report_dir: Path, pin: str,
           instance_pin: Optional[str] = None) -> Dict[str, Any]:
    manifest_path = report_dir / MANIFEST_NAME
    if not manifest_path.is_file():
        raise FileNotFoundError(f"{manifest_path} not found")
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    if manifest.get("schema") not in KNOWN_MANIFEST_SCHEMAS:
        raise ValueError(
            f"unknown manifest schema {manifest.get('schema')!r}; this "
            f"verifier understands {', '.join(KNOWN_MANIFEST_SCHEMAS)}. "
            f"Refusing to report on a format it may be misreading."
        )
    countersig_path = report_dir / COUNTERSIG_NAME
    countersig = (
        json.loads(countersig_path.read_text(encoding="utf-8"))
        if countersig_path.is_file() else None
    )

    layers = [
        check_files(manifest, report_dir),
        check_manifest(manifest),
        check_instance(manifest, instance_pin),
        check_countersignature(manifest, countersig, pin),
    ]
    instance_ok = all(layer.ok for layer in layers[:3])
    counter = layers[3]
    if instance_ok and counter.ok:
        status, exit_code = "VALID AND ATTESTED", 0
    elif instance_ok and counter.absent:
        status, exit_code = UNATTESTED_WORDING, 3
    else:
        status, exit_code = "FAILED", 1
    return {
        "report_dir": report_dir.as_posix(),
        "report_id": manifest.get("report_id"),
        "verifier_sha256": verifier_sha256(),
        "layers": [layer.to_dict() for layer in layers],
        "status": status,
        "exit_code": exit_code,
    }


def verifier_sha256() -> str:
    """This file's own SHA-256, so a customer can confirm the tool."""
    try:
        return hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
    except OSError:                                    # pragma: no cover
        return "unavailable (this verifier could not read its own bytes)"


def render(result: Dict[str, Any]) -> str:
    lines = [
        "Relian Data Discovery report — verification",
        f"  report directory   {result['report_dir']}",
        f"  report id          {result['report_id']}",
        f"  verifier sha256    {result['verifier_sha256']}",
        "",
    ]
    for layer in result["layers"]:
        lines.append(f"  [{layer['verdict']:<6}] {layer['layer']:<17} {layer['detail']}")
        for note in layer["notes"]:
            lines.append(f"           {note}")
    lines.append("")
    lines.append(f"  STATUS  {result['status']}")
    if result["status"] == UNATTESTED_WORDING:
        lines.append(
            "  The instance layer passed and no Visionblox countersignature is "
            "present. This report is intact and came from the Relian "
            "installation the manifest records. Visionblox has NOT attested to "
            "it. Do not read the instance signature as a Visionblox signature."
        )
    return "\n".join(lines)


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = argparse.ArgumentParser(
        prog="verify_report",
        description="Verify a Relian Data Discovery report. Four layers, each "
                    "named and each failing independently.",
    )
    parser.add_argument("--report-dir", required=True)
    # REQUIRED. See "WHY --pin-fingerprint IS REQUIRED" above.
    parser.add_argument(
        "--pin-fingerprint", required=True,
        help="the Visionblox release-key fingerprint, from the Technical "
             "Delivery Sheet. Required: without a pin, a report re-signed "
             "under any key at all would pass.",
    )
    parser.add_argument(
        "--pin-instance-fingerprint", default=None,
        help="optionally pin the per-installation instance key too, to check "
             "continuity against a fingerprint you noted from an earlier report",
    )
    parser.add_argument("--json", action="store_true",
                        help="machine-readable report on stdout")
    args = parser.parse_args(argv)

    try:
        result = verify(
            Path(args.report_dir), args.pin_fingerprint,
            args.pin_instance_fingerprint,
        )
    except (FileNotFoundError, ValueError, json.JSONDecodeError) as exc:
        print(f"UNREADABLE: {exc}", file=sys.stderr)
        return 2

    print(canonical(result) if args.json else render(result))
    return int(result["exit_code"])


if __name__ == "__main__":       # pragma: no cover - process entry point
    raise SystemExit(main())
