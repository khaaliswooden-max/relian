"""WP-2.3 §3.2/§3.3 — the manifest, the instance signature, and the one line
that crosses the perimeter.

THE CONSTRAINT THIS MODULE EXISTS TO SATISFY
---------------------------------------------
R12 keeps customer source inside the customer perimeter. R4 keeps the
Visionblox private key in the operator's custody. Those are different machines
and neither may travel. So the report is signed where it is produced — by a
per-installation key that never leaves the customer's machine — and the
Visionblox countersignature is taken over a **manifest hash**, not over
content. The manifest records SHA-256 digests of the inputs; the only value
that crosses the perimeter is a 64-character hexadecimal string, together with
the report id and the instance fingerprint, which are also digests.

If anything else ever has to cross — a field name, a path fragment, a file —
that is R12 failing and the design is wrong, not the implementation. The leak
test in ``tests/test_discovery_report.py`` builds a report from a fixture
seeded with distinctive customer strings and asserts none of them reaches
:func:`countersign_request_line`, so such a change is a test failure rather
than something a reviewer has to notice.

TWO LAYERS, ADDED AT DIFFERENT TIMES, VERIFIED INDEPENDENTLY (D22)
-------------------------------------------------------------------
* **Instance.** Ed25519, per installation, generated on first run at
  ``~/.relian/instance-ed25519.pem`` with mode ``0600``. Proves integrity and
  provenance-of-tool. Does **not** prove identity of signer: Visionblox never
  holds this key. The customer therefore has a complete, self-checkable
  artifact the moment the CLI finishes.
* **Countersignature.** The Visionblox release key, added by the operator on
  delivery, detached (``report.countersig.json``). Upgrades the claim rather
  than enabling it.

THE DELIBERATE ASYMMETRY WITH ``bench/harness/commit.py``
----------------------------------------------------------
That sealer generates a signing key when the file is absent, and WP-2.1
recorded it as a defect: it produces a manifest that verifies perfectly under a
fingerprint no third party has ever seen, and the failure is silent. Generation
HERE is the intended behaviour, and the difference is disclosure — the event is
recorded in the manifest, the fingerprint is printed in the report, and
``src.discovery.report.INSTANCE_KEY_DISCLOSURE`` states in the artifact how the
key came to exist. Take the disclosure out and this is the same defect again,
so ``tests/test_report_signing.py`` asserts the disclosure is present in the
emitted report and that the manifest carries the generation record.

What is NOT asymmetric: **failure to obtain a key stops the run**. Relian emits
no report at all rather than an unsigned one, because an unsigned artifact that
looks signed is worse than no artifact.

``cryptography`` IS IMPORTED INSIDE FUNCTIONS, ON PURPOSE
----------------------------------------------------------
``tests/test_discovery_is_compiler_free.py`` (c) imports this package in an
interpreter where ``ctypes``, ``pty`` and their relatives are unimportable, to
prove the layout engine needs no process and no native escape hatch. A
module-level third-party import would drag that library's own import graph into
that probe for no reason — the layout engine does not need it. Function-level
imports are the same convention ``tools/seal.py`` already uses, and they are
ordinary static ``ImportFrom`` statements, not the dynamic-import escape hatch
the guard forbids.
"""

from __future__ import annotations

import hashlib
import json
import os
import stat
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Mapping, Optional, Sequence, Tuple

from src.assessment.models import canonical_json

from .report import (
    COUNTERSIG_ARTIFACT,
    MARKDOWN_ARTIFACT,
    SIGNED_ARTIFACT,
    Report,
)

#: The manifest format. A verifier that meets an unknown schema refuses.
MANIFEST_SCHEMA = "relian-discovery-report-manifest/1"
COUNTERSIG_SCHEMA = "relian-discovery-report-countersig/1"

#: D23. Reports are signed with the RELEASE key; benchmarks with the BENCH key.
#: Different claim classes must not share a signer: a report-key rotation must
#: never put a sealed benchmark in question, and vice versa. Both fingerprints
#: are published in the Technical Delivery Sheet, each labelled with what it
#: signs and what it does not.
RELEASE_KEY_FINGERPRINT = "91e3a404155ba4dd"
BENCH_KEY_FINGERPRINT = "233bb4406e2de606"

#: Where a per-installation key lives, relative to the user's home.
INSTANCE_KEY_RELPATH = Path(".relian") / "instance-ed25519.pem"
INSTANCE_RECORD_RELPATH = Path(".relian") / "instance-key.json"

_KEY_MODE = 0o600
_DIR_MODE = 0o700


class SigningError(RuntimeError):
    """No honest signature is available, so no artifact is emitted."""


# --------------------------------------------------------------------------
# The instance key
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class InstanceKey:
    """A loaded per-installation key and the facts worth disclosing about it."""

    private: Any
    public_key_hex: str
    fingerprint: str
    path: str
    created_at: str
    generated_this_run: bool

    def sign(self, payload: bytes) -> str:
        return self.private.sign(payload).hex()

    def record(self) -> Dict[str, Any]:
        """The manifest's ``instance_key`` block.

        ``generated_this_run`` is recorded here and NOT in ``report.json``:
        the report must be byte-identical across two runs, and "was the key
        made just now" is true of the first run and false of the second. The
        manifest is where per-run facts belong, and it is signed too, so the
        disclosure is still covered.
        """
        return {
            "algorithm": "Ed25519",
            "public_key_hex": self.public_key_hex,
            "fingerprint": self.fingerprint,
            "path": self.path,
            "created_at": self.created_at,
            "generated_this_run": self.generated_this_run,
            "origin": (
                "generated by Relian on first use and disclosed in the report; "
                "never transmitted, never held by Visionblox"
            ),
        }


def _fingerprint(public_raw: bytes) -> str:
    """First 16 hex characters of the SHA-256 over the raw public key.

    Identical to the convention ``tools/seal.py`` uses, so one rule about what
    a Relian key fingerprint IS covers every key in the product.
    """
    return hashlib.sha256(public_raw).hexdigest()[:16]


def _public_hex(private: Any) -> str:
    from cryptography.hazmat.primitives import serialization

    return private.public_key().public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw,
    ).hex()


def load_or_create_instance_key(home: Optional[Path] = None) -> InstanceKey:
    """Load ``~/.relian/instance-ed25519.pem``, creating it on first use.

    Raises :class:`SigningError` on any failure, and the caller must let that
    stop the run. There is no unsigned-report path.
    """
    from cryptography.hazmat.primitives import serialization
    from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey

    base = Path(home) if home is not None else Path.home()
    key_path = base / INSTANCE_KEY_RELPATH
    record_path = base / INSTANCE_RECORD_RELPATH

    if key_path.is_file():
        mode = stat.S_IMODE(key_path.stat().st_mode)
        if mode & 0o077:
            raise SigningError(
                f"instance key at {key_path} has mode {mode:04o}: it is "
                f"readable by someone other than its owner. Refusing to sign "
                f"with it. A key anyone on this machine can read cannot "
                f"support an integrity claim about what this machine "
                f"produced. Restore mode 0600 or remove the file and let "
                f"Relian generate a fresh one."
            )
        try:
            private = serialization.load_pem_private_key(
                key_path.read_bytes(), password=None
            )
        except Exception as exc:                       # noqa: BLE001 - reported
            raise SigningError(
                f"instance key at {key_path} could not be loaded: {exc}. "
                f"Relian does not silently replace an unreadable key with a "
                f"new one — that would change the fingerprint under which "
                f"every previous report was signed, without saying so."
            ) from exc
        if not isinstance(private, Ed25519PrivateKey):
            raise SigningError(
                f"instance key at {key_path} is not an Ed25519 key. Refusing "
                f"to sign: the report format states Ed25519 and a verifier "
                f"holds it to that."
            )
        public_hex = _public_hex(private)
        created_at = _read_created_at(record_path, _fingerprint(bytes.fromhex(public_hex)))
        return InstanceKey(
            private=private,
            public_key_hex=public_hex,
            fingerprint=_fingerprint(bytes.fromhex(public_hex)),
            path=str(key_path),
            created_at=created_at,
            generated_this_run=False,
        )

    # --- first use: generate, and DISCLOSE ---------------------------------
    try:
        key_path.parent.mkdir(parents=True, exist_ok=True)
        os.chmod(key_path.parent, _DIR_MODE)
        private = Ed25519PrivateKey.generate()
        pem = private.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption(),
        )
        # Create with 0600 from the outset rather than writing then chmod-ing:
        # between those two calls the key is world-readable, and a race that
        # only loses occasionally is worse than one that always does.
        fd = os.open(str(key_path), os.O_WRONLY | os.O_CREAT | os.O_EXCL, _KEY_MODE)
        try:
            os.write(fd, pem)
        finally:
            os.close(fd)
        created_at = _utc_now()
        public_hex = _public_hex(private)
        record_path.write_text(
            canonical_json(
                {
                    "created_at": created_at,
                    "fingerprint": _fingerprint(bytes.fromhex(public_hex)),
                    "note": (
                        "Generated by Relian on first use. Recorded so a later "
                        "run reports the same creation time and the report "
                        "stays byte-identical across runs."
                    ),
                }
            ) + "\n",
            encoding="utf-8",
        )
    except SigningError:
        raise
    except Exception as exc:                           # noqa: BLE001 - reported
        raise SigningError(
            f"could not create an instance signing key at {key_path}: {exc}. "
            f"Relian emits NO report rather than an unsigned one: an unsigned "
            f"artifact that looks like a signed one is worse than no artifact."
        ) from exc

    return InstanceKey(
        private=private,
        public_key_hex=public_hex,
        fingerprint=_fingerprint(bytes.fromhex(public_hex)),
        path=str(key_path),
        created_at=created_at,
        generated_this_run=True,
    )


def _read_created_at(record_path: Path, fingerprint: str) -> str:
    """The recorded creation time, or an honest statement that there is none.

    A key that predates the record — restored from a backup, say — has no
    creation time this installation can know. It gets the string below rather
    than a fabricated timestamp (R1).
    """
    unknown = "unknown (key predates this installation's creation record)"
    if not record_path.is_file():
        return unknown
    try:
        data = json.loads(record_path.read_text(encoding="utf-8"))
    except Exception:                                  # noqa: BLE001
        return unknown
    if not isinstance(data, dict) or data.get("fingerprint") != fingerprint:
        return unknown
    value = data.get("created_at")
    return value if isinstance(value, str) and value else unknown


def _utc_now() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat()


# --------------------------------------------------------------------------
# The manifest
# --------------------------------------------------------------------------


def manifest_hash(manifest: Mapping[str, Any]) -> str:
    """The value both signatures are taken over: the manifest minus ``signature``.

    Same shape as ``tools/seal.py``: sorted keys, no insignificant whitespace,
    UTF-8. ``tools/verify_report.py`` reimplements this from the format
    description rather than importing it, because a verifier that calls the
    signer's own code cannot detect a bug in the signer's own code.
    """
    core = {k: v for k, v in manifest.items() if k != "signature"}
    return hashlib.sha256(canonical_json(core).encode("utf-8")).hexdigest()


def input_digests(resolution_records: Mapping[str, Any]) -> List[Dict[str, str]]:
    """``[{name, path, sha256}, …]`` over the copybooks that resolved.

    DIGESTS, NOT CONTENT. The manifest is the object whose hash crosses the
    perimeter, so it must be safe for the hash to be derived from something
    the customer would not transmit — and it is, because nothing here is the
    customer's code. The paths stay in the manifest, which never leaves the
    customer's machine either; only its hash does.
    """
    return [
        {"name": name, "path": src.path, "sha256": src.sha256}
        for name, src in sorted(resolution_records.items())
    ]


def build_manifest(
    report: Report,
    *,
    files: Sequence[Tuple[str, bytes]],
    tool_digests: Sequence[Mapping[str, str]],
    input_digest_rows: Sequence[Mapping[str, str]],
    instance: InstanceKey,
    relian_version: str,
    python_version: str,
    generated_at: Optional[str] = None,
) -> Dict[str, Any]:
    """Assemble the unsigned manifest over already-serialised artifact bytes.

    ``files`` is ``[(relative_posix_path, bytes), …]``. Hashing the bytes the
    caller is about to write — rather than re-reading the files afterwards —
    is what makes the digest a statement about the artifact rather than about
    whatever happened to be on disk a moment later.
    """
    entries = [
        {"path": path, "sha256": hashlib.sha256(blob).hexdigest()}
        for path, blob in sorted(files, key=lambda pair: pair[0])
    ]
    return {
        "schema": MANIFEST_SCHEMA,
        "report_id": report.report_id,
        "report_schema": report.schema,
        # The ONE timestamp in the deliverable, and it is inside the signed
        # object. report.json carries none, which is what makes two runs
        # byte-identical.
        "generated_at": generated_at or _utc_now(),
        "tool_versions": {"relian": relian_version, "python": python_version},
        "tool_digests": [dict(d) for d in tool_digests],
        "files": entries,
        "input_digests": [dict(r) for r in input_digest_rows],
        "instance_key": instance.record(),
        "signed_artifact": SIGNED_ARTIFACT,
        "derived_unsigned_artifacts": [MARKDOWN_ARTIFACT],
        "countersignature_expected_at": COUNTERSIG_ARTIFACT,
    }


def sign_manifest(manifest: Dict[str, Any], instance: InstanceKey) -> Dict[str, Any]:
    """Attach the instance signature block over :func:`manifest_hash`."""
    digest = manifest_hash(manifest)
    manifest["signature"] = {
        "alg": "Ed25519",
        "layer": "instance",
        "manifest_sha256": digest,
        "signature_hex": instance.sign(digest.encode("utf-8")),
        "public_key_hex": instance.public_key_hex,
        "key_fingerprint": instance.fingerprint,
        "signed_at": _utc_now(),
    }
    return manifest


def manifest_bytes(manifest: Mapping[str, Any]) -> bytes:
    return canonical_json(dict(manifest)).encode("utf-8") + b"\n"


# --------------------------------------------------------------------------
# The one line that crosses the perimeter (§3.3)
# --------------------------------------------------------------------------

#: The request format. Three hexadecimal fields and nothing else. Parsed by
#: the operator's eyes and by ``tools/countersign.py``'s arguments; there is no
#: file to transmit, because a file is a thing that can carry a field a line
#: cannot.
COUNTERSIGN_REQUEST_PREFIX = "relian-countersign-request/1"


def countersign_request_line(manifest: Mapping[str, Any]) -> str:
    """The single line the customer sends to the operator.

    Manifest hash, report id, instance fingerprint. All three are digests, so
    the line cannot carry a customer identifier, a field name or a path
    fragment even by accident — but "cannot by construction" is an argument,
    and the leak test is the measurement, so the values are re-checked as
    hexadecimal here before the line is emitted.
    """
    signature = manifest.get("signature")
    if not isinstance(signature, dict):
        raise SigningError(
            "refusing to emit a countersignature request for an unsigned "
            "manifest: there would be nothing for the countersignature to be "
            "taken alongside."
        )
    fields = {
        "manifest_sha256": signature.get("manifest_sha256", ""),
        "report_id": manifest.get("report_id", ""),
        "instance_fingerprint": signature.get("key_fingerprint", ""),
    }
    for name, value in sorted(fields.items()):
        if not isinstance(value, str) or not value:
            raise SigningError(f"countersignature request is missing {name}")
        if any(ch not in "0123456789abcdef" for ch in value.lower()):
            raise SigningError(
                f"refusing to emit a countersignature request: {name} is not "
                f"hexadecimal, so it could carry text out of the customer "
                f"perimeter. That is R12 failing, and the design is wrong "
                f"rather than the implementation. Escalate."
            )
    return (
        f"{COUNTERSIGN_REQUEST_PREFIX} "
        f"manifest_sha256={fields['manifest_sha256']} "
        f"report_id={fields['report_id']} "
        f"instance_fingerprint={fields['instance_fingerprint']}"
    )
