"""
RELIAN-BENCH Phase 4 -- Deterministic Defense.

Freezes the benchmark and makes it tamper-evident. Once committed, the
success criteria cannot be quietly redefined: any future result is
verifiable against this exact bundle by hash.

The commit hash is published BEFORE solution work begins. That ordering is
the whole point -- it is what prevents goalpost-moving and bench-maxing.

Aletheia DAC conventions carried over from zil_sign.py (known cross-platform
bugs, deliberately avoided here):
  1. Use Path.as_posix() UNCONDITIONALLY for manifest paths -- never native
     separators, or Windows and Linux produce different manifest hashes.
  2. Sort by the explicit posix STRING form of the relative path -- never the
     Path object, whose ordering diverges on case-sensitivity between
     Windows and Linux.
"""

import hashlib
import json
import subprocess
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List

ROOT = Path(__file__).resolve().parent.parent

# Everything that defines the benchmark. Candidate outputs are NOT included --
# the benchmark must be independent of anything under test.
INCLUDE_DIRS = ["corpus", "harness"]
INCLUDE_FILES = ["SPEC.md"]
EXCLUDE_SUFFIX = {".pyc", ".o"}
EXCLUDE_NAMES = {"__pycache__", "_classes"}
# Compiled COBOL binaries are excluded: they are build artifacts, not source.
EXCLUDE_BINARIES = {"payroll01", "run"}


def _iter_files() -> List[Path]:
    out: List[Path] = []
    for d in INCLUDE_DIRS:
        for p in (ROOT / d).rglob("*"):
            if not p.is_file():
                continue
            if any(part in EXCLUDE_NAMES for part in p.parts):
                continue
            if p.suffix in EXCLUDE_SUFFIX:
                continue
            if p.name in EXCLUDE_BINARIES:
                continue
            out.append(p)
    for f in INCLUDE_FILES:
        if (ROOT / f).exists():
            out.append(ROOT / f)
    # BUG-AVOIDANCE #2: sort by explicit posix string, not Path objects.
    return sorted(out, key=lambda p: p.relative_to(ROOT).as_posix())


def build_manifest() -> Dict:
    entries = []
    for p in _iter_files():
        digest = hashlib.sha256(p.read_bytes()).hexdigest()
        # BUG-AVOIDANCE #1: as_posix() unconditionally.
        entries.append({"path": p.relative_to(ROOT).as_posix(), "sha256": digest})
    payload = json.dumps(entries, sort_keys=True, separators=(",", ":"))
    payload_hash = hashlib.sha256(payload.encode("utf-8")).hexdigest()

    vec_counts = {}
    for vf in sorted((ROOT / "corpus").rglob("vectors/*.jsonl"),
                     key=lambda p: p.relative_to(ROOT).as_posix()):
        key = vf.relative_to(ROOT / "corpus").as_posix()
        vec_counts[key] = len([l for l in vf.read_text().splitlines() if l.strip()])

    manifest = {
        "benchmark": "RELIAN-BENCH",
        "version": "1.0.0",
        "tag": "relian-bench-v1.0",
        "committed_at": datetime.now(timezone.utc).isoformat(),
        "file_count": len(entries),
        "files": entries,
        "payload_sha256": payload_hash,
        "vector_counts": vec_counts,
        "toolchain": _toolchain(),
        "thresholds": THRESHOLDS,
        "baselines_recorded": {},   # filled by record_baseline()
    }
    return manifest


def _toolchain() -> Dict[str, str]:
    def v(cmd: List[str]) -> str:
        try:
            p = subprocess.run(cmd, capture_output=True, text=True, timeout=20)
            return (p.stdout + p.stderr).strip().splitlines()[0]
        except Exception:
            return "UNAVAILABLE"
    return {
        "cobc": v(["cobc", "--version"]),
        "javac": v(["javac", "-version"]),
        "java": v(["java", "-version"]),
    }


# --- The committed success criteria. Frozen at commit time. -------------
THRESHOLDS = {
    "ber_heldout_min": 0.95,
    "build_rate_min": 1.00,
    "branch_coverage_min": 0.80,
    "coverage_required_tool": "jacoco",
    "notes": (
        "BER is measured on HELD-OUT vectors only. A candidate that fails to "
        "compile scores 0 for that program's full vector count. Any metric "
        "that cannot be measured is reported as null and the run is INVALID; "
        "null is never substituted with a constant."
    ),
}


def manifest_hash(manifest: Dict) -> str:
    core = {k: v for k, v in manifest.items() if k != "signature"}
    blob = json.dumps(core, sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(blob.encode("utf-8")).hexdigest()


def sign(manifest: Dict, key_path: Path) -> Dict:
    from cryptography.hazmat.primitives.asymmetric.ed25519 import (
        Ed25519PrivateKey,
    )
    from cryptography.hazmat.primitives import serialization

    if key_path.exists():
        priv = serialization.load_pem_private_key(key_path.read_bytes(), password=None)
    else:
        priv = Ed25519PrivateKey.generate()
        key_path.parent.mkdir(parents=True, exist_ok=True)
        key_path.write_bytes(priv.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption(),
        ))
    pub = priv.public_key().public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw,
    )
    mh = manifest_hash(manifest)
    sig = priv.sign(mh.encode("utf-8"))
    manifest["signature"] = {
        "alg": "Ed25519",
        "manifest_sha256": mh,
        "signature_hex": sig.hex(),
        "public_key_hex": pub.hex(),
        "key_fingerprint": hashlib.sha256(pub).hexdigest()[:16],
        "signed_at": datetime.now(timezone.utc).isoformat(),
    }
    return manifest


def verify(manifest: Dict) -> bool:
    from cryptography.hazmat.primitives.asymmetric.ed25519 import (
        Ed25519PublicKey,
    )
    sigblock = manifest.get("signature")
    if not sigblock:
        return False
    pub = Ed25519PublicKey.from_public_bytes(bytes.fromhex(sigblock["public_key_hex"]))
    mh = manifest_hash(manifest)
    if mh != sigblock["manifest_sha256"]:
        return False
    try:
        pub.verify(bytes.fromhex(sigblock["signature_hex"]), mh.encode("utf-8"))
        return True
    except Exception:
        return False


if __name__ == "__main__":
    m = build_manifest()
    # Record the measured floor BEFORE any solution work (Phase 4 requirement).
    for name in ("B0_null", "B2_reference"):
        rp = ROOT / "results" / f"{name}.json"
        if rp.exists():
            r = json.loads(rp.read_text())
            m["baselines_recorded"][name] = {
                "ber_overall": r["ber_overall"],
                "build_rate": r["build_rate"],
                "coverage_branch": r["coverage_branch"],
            }
    m = sign(m, Path.home() / "zil-keys" / "relian-bench-v1.pem")
    out = ROOT / "LEDGER_relian-bench-v1.0.json"
    out.write_text(json.dumps(m, indent=2))
    print(f"tag              : {m['tag']}")
    print(f"files            : {m['file_count']}")
    print(f"payload_sha256   : {m['payload_sha256']}")
    print(f"manifest_sha256  : {m['signature']['manifest_sha256']}")
    print(f"key_fingerprint  : {m['signature']['key_fingerprint']}")
    print(f"verify()         : {verify(m)}")
