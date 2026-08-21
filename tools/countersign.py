#!/usr/bin/env python3
"""WP-2.3 §3.3 — the operator side of the countersignature.

WHAT CROSSES THE PERIMETER, AND WHAT DOES NOT
----------------------------------------------
The customer's CLI prints one line:

    relian-countersign-request/1 manifest_sha256=<64 hex> report_id=<32 hex> \\
        instance_fingerprint=<16 hex>

Three digests. No file name, no field name, no path fragment, no customer
identifier — R12 holds because there is nothing else in the line to hold. This
tool takes those three values and the operator's release key, and produces a
detached ``report.countersig.json`` the customer drops next to the report.

The operator never sees the customer's source, never sees the report, and does
not need either: the countersignature is over the manifest hash, which is what
the instance signature is over too, so both layers cover exactly the same
object and a verifier can check them independently.

THREE REFUSALS, EACH ONE A MEASURED DEFECT FROM AN EARLIER PACKAGE
-------------------------------------------------------------------
1. **An absent key file raises. There is no generation fallback.** This is the
   WP-2.1 finding about ``bench/harness/commit.py``, whose ``sign()`` falls
   through to ``Ed25519PrivateKey.generate()``: a signing run on a machine
   without the key succeeds and produces an artifact that verifies perfectly
   under a fingerprint nobody has ever seen. ``tests/test_report_signing.py``
   plants that failure and proves this tool refuses.

2. **A hash that is not exactly 64 hexadecimal characters is refused.** The
   request line is transcribed by a human between two machines. A truncated or
   space-mangled hash must fail here, not silently produce a signature over a
   string that is not any report's manifest hash.

3. **The bench key may not sign a report (D23).** ``233bb4406e2de606`` seals
   RELIAN-BENCH and RELIAN-DISCOVERY-BENCH. ``91e3a404155ba4dd`` signs customer
   deliverables. Different claim classes must not share a signer: if the report
   key is rotated or compromised, that event must not put a sealed benchmark in
   question. This tool refuses the bench fingerprint unconditionally and checks
   the loaded key against ``--expect-key-fingerprint``, which defaults to the
   release key.

CUSTODY
-------
This file never generates a key, never writes one, never prints one, and reads
one only from the path it is given (R4). It is not sealed and imports nothing
from this repository — ``cryptography`` and the standard library only.

USAGE
-----
    python3 tools/countersign.py \\
        --manifest-hash <64 hex> \\
        --report-id <hex> \\
        --instance-fingerprint <hex> \\
        --key ~/zil-keys/relian-release.pem \\
        --out report.countersig.json
"""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional, Sequence

#: D23 — what each key signs, and what it may not.
RELEASE_KEY_FINGERPRINT = "91e3a404155ba4dd"      # customer deliverables
BENCH_KEY_FINGERPRINT = "233bb4406e2de606"        # sealed benchmarks, NOT reports

COUNTERSIG_SCHEMA = "relian-discovery-report-countersig/1"

_HEX = frozenset("0123456789abcdef")


class CountersignError(RuntimeError):
    """No honest countersignature is available, so none is produced."""


def _is_hex(value: str, length: Optional[int] = None) -> bool:
    if length is not None and len(value) != length:
        return False
    return bool(value) and all(ch in _HEX for ch in value.lower())


def load_release_key(key_path: Path):
    """Load the operator's Ed25519 release key. NEVER generate one.

    See refusal 1 in the module docstring: a generated key turns "this machine
    does not hold the signing key", which should stop the ceremony, into an
    artifact that verifies under a fingerprint no third party holds.
    """
    from cryptography.hazmat.primitives import serialization
    from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey

    if not key_path.is_file():
        raise CountersignError(
            f"release key not found at {key_path}. Refusing to continue. This "
            f"tool NEVER generates a key: a generated key would produce a "
            f"countersignature that verifies perfectly under a fingerprint no "
            f"customer holds, which is worse than no countersignature at all. "
            f"Custody is the operator's (R4)."
        )
    private = serialization.load_pem_private_key(key_path.read_bytes(), password=None)
    if not isinstance(private, Ed25519PrivateKey):
        raise CountersignError(
            f"the key at {key_path} is not an Ed25519 private key. The report "
            f"format states Ed25519 and every verifier holds it to that."
        )
    return private


def fingerprint_of(private) -> str:
    from cryptography.hazmat.primitives import serialization

    raw = private.public_key().public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw,
    )
    return hashlib.sha256(raw).hexdigest()[:16]


def public_hex_of(private) -> str:
    from cryptography.hazmat.primitives import serialization

    return private.public_key().public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw,
    ).hex()


def countersign(
    manifest_hash: str,
    *,
    report_id: str,
    instance_fingerprint: str,
    key_path: Path,
    expect_fingerprint: str = RELEASE_KEY_FINGERPRINT,
) -> dict:
    """Produce the detached countersignature object. Refusals come first."""
    manifest_hash = manifest_hash.strip().lower()
    if not _is_hex(manifest_hash, 64):
        raise CountersignError(
            f"refusing to sign {manifest_hash!r}: a manifest hash is exactly 64 "
            f"hexadecimal characters. This value is {len(manifest_hash)} "
            f"character(s) and may have been truncated or mangled in transit. "
            f"Signing it would produce a valid signature over a string that is "
            f"not any report's manifest hash."
        )
    if not _is_hex(report_id):
        raise CountersignError(
            f"refusing to sign: report_id {report_id!r} is not hexadecimal. "
            f"Every field of the countersignature request is a digest; a "
            f"non-hex value means something other than a digest crossed the "
            f"customer perimeter, which is R12 failing. Escalate rather than "
            f"working around it."
        )
    if not _is_hex(instance_fingerprint):
        raise CountersignError(
            f"refusing to sign: instance_fingerprint {instance_fingerprint!r} "
            f"is not hexadecimal. See the note on report_id above; escalate."
        )

    private = load_release_key(key_path)
    actual = fingerprint_of(private)
    if actual == BENCH_KEY_FINGERPRINT:
        raise CountersignError(
            f"refusing to sign: {key_path} is the BENCHMARK signing key "
            f"({BENCH_KEY_FINGERPRINT}). Reports and benchmarks are different "
            f"claim classes and must not share a signer (D23) — a report-key "
            f"rotation must never put a sealed benchmark in question. Use the "
            f"release key."
        )
    if actual != expect_fingerprint.lower():
        raise CountersignError(
            f"refusing to sign: the key at {key_path} has fingerprint "
            f"{actual}, and {expect_fingerprint.lower()} was expected. Either "
            f"the wrong key file was given or the published release "
            f"fingerprint is stale; both need a person, not a fallback."
        )

    signature = private.sign(manifest_hash.encode("utf-8"))
    return {
        "schema": COUNTERSIG_SCHEMA,
        "alg": "Ed25519",
        "layer": "countersignature",
        "manifest_sha256": manifest_hash,
        # Advisory, and checked by the verifier against the manifest itself.
        # They are not what the signature covers — the manifest hash is, and it
        # already commits to both — but a mismatch is a named failure rather
        # than a silent one.
        "report_id": report_id,
        "instance_fingerprint": instance_fingerprint,
        "signature_hex": signature.hex(),
        "public_key_hex": public_hex_of(private),
        "key_fingerprint": actual,
        "signed_at": datetime.now(timezone.utc).replace(microsecond=0).isoformat(),
        "attests": (
            "Visionblox attests to the report whose manifest hash appears "
            "above. This signature covers the manifest hash only; the report "
            "content never left the customer perimeter."
        ),
    }


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = argparse.ArgumentParser(
        prog="countersign",
        description="Countersign a Relian Data Discovery report from its "
                    "manifest hash. Operator-only; requires the release key.",
    )
    parser.add_argument("--manifest-hash", required=True,
                        help="the 64-hex manifest hash from the request line")
    parser.add_argument("--report-id", required=True)
    parser.add_argument("--instance-fingerprint", required=True)
    parser.add_argument("--key", required=True,
                        help="path to the Ed25519 release key PEM")
    parser.add_argument("--out", default="report.countersig.json")
    parser.add_argument("--expect-key-fingerprint", default=RELEASE_KEY_FINGERPRINT,
                        help="the fingerprint the key must have (D23)")
    args = parser.parse_args(argv)

    try:
        block = countersign(
            args.manifest_hash,
            report_id=args.report_id,
            instance_fingerprint=args.instance_fingerprint,
            key_path=Path(args.key),
            expect_fingerprint=args.expect_key_fingerprint,
        )
    except CountersignError as exc:
        print(f"REFUSED: {exc}", file=sys.stderr)
        return 2

    out = Path(args.out)
    out.write_text(
        json.dumps(block, sort_keys=True, separators=(",", ":"), ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    print(f"countersignature written to {out}")
    print(f"  manifest_sha256  {block['manifest_sha256']}")
    print(f"  key_fingerprint  {block['key_fingerprint']}")
    print("  the customer drops this file next to report.json and re-runs "
          "tools/verify_report.py")
    return 0


if __name__ == "__main__":       # pragma: no cover - process entry point
    raise SystemExit(main())
