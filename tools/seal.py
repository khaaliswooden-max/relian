#!/usr/bin/env python3
"""Seal a benchmark tree into a signed, self-describing manifest.

WHY THIS IS A NEW FILE RATHER THAN A PARAMETERISED `commit.py`
--------------------------------------------------------------
`bench/harness/commit.py` seals RELIAN-BENCH v1.2 and does the same job. It is
not reused, imported from, or edited, for one reason: **it is inside the
manifest it produces.** `bench/harness/` is an include dir of the v1.2 seal, so
`commit.py`'s own sha256 is a recorded entry in
`bench/LEDGER_relian-bench-v1.2.json`. Adding a parameter to it -- even a
comment -- changes that hash, which changes `payload_sha256`, which changes
`manifest_sha256`, which invalidates the Ed25519 signature. There is no such
thing as a documentation-only edit to a sealed file, and rule 4 forbids the
edit regardless.

`commit.py` also hardcodes `"1.2.0"`, `"relian-bench-v1.2"` and its own output
path, so it could not seal a second benchmark without being changed.

Fifty duplicated lines is the correct price for leaving the one artifact this
repository can prove untouched. Note what is NOT duplicated: the VERIFIER.
There is one `tools/verify_manifest.py` for both benchmarks, because that is
the surface a third party checks re-derivability against, and it stays unified.

THE TWO POSIX CONVENTIONS, REPLICATED VERBATIM
-----------------------------------------------
Both are copied deliberately from `bench/harness/commit.py`, which carries them
from `zil_sign.py` as known cross-platform bug-avoidance. They are replicated
rather than shared because sharing would mean importing from inside the sealed
tree (see above). If either is ever changed, it must be changed in both files
or the two benchmarks will hash differently on the same machine:

  1. Use `Path.as_posix()` UNCONDITIONALLY for manifest paths -- never native
     separators, or Windows and Linux produce different manifest hashes.
  2. Sort by the explicit posix STRING form of the relative path -- never the
     `Path` object, whose ordering diverges on case-sensitivity between
     Windows and Linux.

FIVE DELIBERATE DIFFERENCES FROM `commit.py`
---------------------------------------------
Each is a defect in the v1.2 sealer that a signed record cannot be edited to
fix, so each is fixed here instead. Each has a test in `tests/test_seal.py`.

  1. **An absent key file raises. There is no generation fallback.**
     `commit.py`'s `sign()` falls through to `Ed25519PrivateKey.generate()` and
     writes a fresh PEM. A sealing run on a machine without the key therefore
     succeeds, and produces a manifest that `verify()` returns True on, under a
     fingerprint nobody has ever seen. The failure is silent and the artifact
     looks valid. `sign()` here raises `SealError`.

  2. **Any `UNAVAILABLE` toolchain probe raises.** The v1.2 ledger carries
     `"javac": "UNAVAILABLE"` permanently, because the sealing machine had no
     JDK and a signed record cannot be edited afterwards. Refusing to sign is
     the only moment at which that is still fixable.

  3. **Name, version, tag, include-set and output path are parameters.**
     Supplied by a TOML config file (`--config`), with CLI flags able to
     override any of them. Rationale for choosing a config file as the primary
     form: the include-set and `expected_absent` are part of what gets SIGNED,
     so they should be reviewable in a diff and identical between the sealing
     run and CI, rather than retyped on a command line where a dropped
     `--include-dir` silently narrows what the seal covers. The CLI overrides
     exist for tests and for one-off verification, not for the ceremony.

  4. **The manifest records `include_rules`, `expected_absent`,
     `oracle_toolchain` and `corpus_counts`** inside the signed payload (D12).
     v1.2 records none of them, which is why `verify_manifest.py` has to be
     told the rules on the command line -- so the person running the check is
     the one deciding what counts as included and what is allowed to be
     missing. That is backwards; absences should be declared by the signer.

  5. **Verification pins the expected key fingerprint** rather than trusting
     the public key embedded in the manifest. `commit.py`'s `verify()` reads
     `public_key_hex` out of the manifest and verifies against it, so anyone
     can re-sign an edited manifest with a key they generated and it returns
     True (T7). `verify()` here requires the fingerprint and compares it.

WHAT THIS FILE IS NOT
---------------------
It is not sealed. It is a tool, not a benchmark artifact. It never reads a
private key from anywhere but the path it is given, never writes one, and never
prints one. Key custody is the operator's alone (R4).
"""

from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
import sys
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence

try:  # Python 3.11+
    import tomllib
except ModuleNotFoundError:  # pragma: no cover - 3.10 and older
    import tomli as tomllib  # type: ignore[no-redef]

#: The one custody chain. Two benchmarks, one fingerprint to publish and one
#: thing to protect (D14). Pinned here so a verification path cannot quietly
#: fall back to trusting whatever key the manifest happens to embed.
EXPECTED_KEY_FINGERPRINT = "233bb4406e2de606"

#: A toolchain probe that could not answer. Never written into a manifest.
UNAVAILABLE = "UNAVAILABLE"

_PROBE_TIMEOUT_S = 30


class SealError(RuntimeError):
    """The seal cannot be produced honestly, so it is not produced."""


# --- Configuration (difference 3) -------------------------------------------


@dataclass
class IncludeRules:
    """What the seal covers. Recorded inside the signed payload (difference 4)."""

    dirs: List[str] = field(default_factory=list)
    files: List[str] = field(default_factory=list)
    exclude_suffix: List[str] = field(default_factory=list)
    exclude_names: List[str] = field(default_factory=list)

    def as_dict(self) -> Dict[str, List[str]]:
        # Sorted so two configs listing the same rules in a different order
        # produce the same manifest bytes.
        return {
            "dirs": sorted(self.dirs),
            "files": sorted(self.files),
            "exclude_suffix": sorted(self.exclude_suffix),
            "exclude_names": sorted(self.exclude_names),
        }


@dataclass
class SealConfig:
    name: str
    version: str
    tag: str
    root: Path
    out_path: Path
    include_rules: IncludeRules
    expected_absent: List[str] = field(default_factory=list)
    #: Command lines to probe, as {label: [argv]}. Every one must answer.
    toolchain_probes: Dict[str, List[str]] = field(default_factory=dict)
    #: Which probe label carries the oracle's compiler.
    oracle_toolchain_keys: List[str] = field(default_factory=list)
    #: Path to the oracle JSON whose counts are recorded, or None.
    corpus_counts_from: Optional[Path] = None


def load_config(path: Path, root_override: Optional[Path] = None) -> SealConfig:
    """Read a seal config. Every key is required except the optional ones."""
    if not path.is_file():
        raise SealError(f"config not found: {path}")
    data = tomllib.loads(path.read_text(encoding="utf-8"))
    seal = data.get("seal")
    if not isinstance(seal, dict):
        raise SealError(f"{path}: no [seal] table")

    missing = [k for k in ("name", "version", "tag", "root", "out") if k not in seal]
    if missing:
        raise SealError(f"{path}: [seal] is missing {', '.join(missing)}")

    base = path.parent
    root = root_override or (base / seal["root"]).resolve()
    include = data.get("include", {})
    rules = IncludeRules(
        dirs=list(include.get("dirs", [])),
        files=list(include.get("files", [])),
        exclude_suffix=list(include.get("exclude_suffix", [])),
        exclude_names=list(include.get("exclude_names", [])),
    )
    probes = {k: list(v) for k, v in (data.get("toolchain", {}) or {}).items()}
    counts_from = seal.get("corpus_counts_from")
    return SealConfig(
        name=seal["name"],
        version=seal["version"],
        tag=seal["tag"],
        root=Path(root),
        out_path=(base / seal["out"]).resolve(),
        include_rules=rules,
        expected_absent=list(data.get("expected_absent", [])),
        toolchain_probes=probes,
        oracle_toolchain_keys=list(seal.get("oracle_toolchain", [])),
        corpus_counts_from=(root / counts_from) if counts_from else None,
    )


# --- The walk ---------------------------------------------------------------


def iter_files(root: Path, rules: IncludeRules) -> List[Path]:
    """Every file the include set covers.

    BUG-AVOIDANCE #2 (origin: bench/harness/commit.py, from zil_sign.py):
    sort by the explicit POSIX STRING form of the relative path, never by the
    Path object -- Path ordering diverges with case sensitivity between Windows
    and Linux, so the same tree would hash differently on the two.
    """
    out: List[Path] = []
    exclude_suffix = set(rules.exclude_suffix)
    exclude_names = set(rules.exclude_names)
    for directory in rules.dirs:
        for path in (root / directory).rglob("*"):
            if not path.is_file():
                continue
            if any(part in exclude_names for part in path.parts):
                continue
            if path.suffix in exclude_suffix:
                continue
            if path.name in exclude_names:
                continue
            out.append(path)
    for name in rules.files:
        candidate = root / name
        if candidate.is_file():
            out.append(candidate)
    return sorted(out, key=lambda p: p.relative_to(root).as_posix())


def build_entries(root: Path, rules: IncludeRules) -> List[Dict[str, str]]:
    """`files[]`: one `{path, sha256}` per covered file.

    BUG-AVOIDANCE #1 (origin: bench/harness/commit.py, from zil_sign.py):
    `as_posix()` UNCONDITIONALLY for manifest paths -- never native separators,
    or Windows and Linux produce different manifest hashes for the same tree.
    """
    entries: List[Dict[str, str]] = []
    for path in iter_files(root, rules):
        entries.append(
            {
                "path": path.relative_to(root).as_posix(),
                "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
            }
        )
    return entries


def payload_sha256(entries: Sequence[Dict[str, str]]) -> str:
    """Hash of the ENTRY LIST, serialised with sorted keys and no whitespace."""
    blob = json.dumps(list(entries), sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(blob.encode("utf-8")).hexdigest()


def manifest_hash(manifest: Dict[str, Any]) -> str:
    """The value the signature is taken over: the manifest minus `signature`."""
    core = {k: v for k, v in manifest.items() if k != "signature"}
    blob = json.dumps(core, sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(blob.encode("utf-8")).hexdigest()


# --- Toolchain (difference 2) -----------------------------------------------


def probe_toolchain(probes: Dict[str, List[str]]) -> Dict[str, str]:
    """First output line of each probe, or `UNAVAILABLE`."""
    result: Dict[str, str] = {}
    for label, argv in sorted(probes.items()):
        try:
            proc = subprocess.run(
                argv, capture_output=True, text=True, timeout=_PROBE_TIMEOUT_S
            )
        except (OSError, subprocess.SubprocessError):
            result[label] = UNAVAILABLE
            continue
        line = None
        for stream in (proc.stdout, proc.stderr):
            for candidate in (stream or "").splitlines():
                if candidate.strip():
                    line = candidate.strip()
                    break
            if line:
                break
        result[label] = line or UNAVAILABLE
    return result


def require_complete_toolchain(toolchain: Dict[str, str]) -> None:
    """Difference 2: refuse to sign when any probe returned `UNAVAILABLE`.

    The v1.2 ledger carries `"javac": "UNAVAILABLE"` forever because the
    sealing machine had no JDK. Nothing can edit that now. This is the only
    moment at which it is still fixable, so it is the moment to fail.
    """
    absent = sorted(k for k, v in toolchain.items() if v == UNAVAILABLE)
    if absent:
        raise SealError(
            "refusing to sign: the toolchain probe could not answer for "
            + ", ".join(absent)
            + ". A signed record cannot be edited afterwards, and the v1.2 "
              "ledger carries \"javac\": \"UNAVAILABLE\" permanently for "
              "exactly this reason. Install the missing tool and re-run."
        )


# --- Signing (difference 1) -------------------------------------------------


def load_private_key(key_path: Path):
    """Load the Ed25519 private key. Difference 1: NEVER generate one.

    `commit.py` falls through to `Ed25519PrivateKey.generate()` when the file
    is absent and writes a fresh PEM. That turns "this machine does not hold
    the signing key" -- which should stop the ceremony -- into a manifest that
    verifies perfectly under a fingerprint no third party has ever seen. There
    is no generation fallback here, and there is not going to be one.
    """
    from cryptography.hazmat.primitives import serialization

    if not key_path.is_file():
        raise SealError(
            f"signing key not found at {key_path}. Refusing to continue. "
            f"This sealer NEVER generates a key: a generated key would produce "
            f"a manifest that verify() returns True on, under a fingerprint no "
            f"third party holds, which is worse than no manifest at all. "
            f"Custody is the operator's (R4)."
        )
    return serialization.load_pem_private_key(
        key_path.read_bytes(), password=None
    )


def sign(manifest: Dict[str, Any], key_path: Path) -> Dict[str, Any]:
    """Attach an Ed25519 signature block over `manifest_hash(manifest)`."""
    from cryptography.hazmat.primitives import serialization

    private = load_private_key(key_path)
    public = private.public_key().public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw,
    )
    fingerprint = hashlib.sha256(public).hexdigest()[:16]
    digest = manifest_hash(manifest)
    signature = private.sign(digest.encode("utf-8"))
    manifest["signature"] = {
        "alg": "Ed25519",
        "manifest_sha256": digest,
        "signature_hex": signature.hex(),
        "public_key_hex": public.hex(),
        "key_fingerprint": fingerprint,
        "signed_at": datetime.now(timezone.utc).isoformat(),
    }
    return manifest


def verify(
    manifest: Dict[str, Any],
    expected_fingerprint: str = EXPECTED_KEY_FINGERPRINT,
) -> bool:
    """Difference 5: verify AND pin the signer.

    `commit.py`'s `verify()` takes the public key out of the manifest and
    checks the signature against it. That proves the manifest is internally
    consistent and self-signed; it does not prove WHO signed it, so a forgery
    re-signed with an attacker-generated key returns True (T7).

    `expected_fingerprint` is required. Passing None is not offered, because an
    optional pin is a pin that gets left off.
    """
    from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PublicKey

    if not expected_fingerprint:
        raise SealError(
            "verify() requires an expected key fingerprint. Verifying against "
            "the manifest's own embedded key proves self-consistency, not "
            "authorship."
        )
    block = manifest.get("signature")
    if not isinstance(block, dict):
        return False
    embedded = block.get("public_key_hex")
    recorded = block.get("manifest_sha256")
    signature = block.get("signature_hex")
    if not (embedded and recorded and signature):
        return False

    # Pinned BEFORE the cryptographic check, because a valid signature by the
    # wrong key is precisely the failure a bare verify() cannot see.
    actual = hashlib.sha256(bytes.fromhex(embedded)).hexdigest()[:16]
    if actual != expected_fingerprint.lower():
        return False
    if manifest_hash(manifest) != recorded:
        return False
    try:
        Ed25519PublicKey.from_public_bytes(bytes.fromhex(embedded)).verify(
            bytes.fromhex(signature), recorded.encode("utf-8")
        )
    except Exception:
        return False
    return True


# --- Building the manifest --------------------------------------------------


def read_corpus_counts(oracle_path: Optional[Path]) -> Dict[str, Any]:
    """The oracle's own derived counts, recorded inside the seal (difference 4).

    Read from the generated oracle rather than recounted here. Two independent
    counts of the same thing is two things to disagree; the oracle is the one
    that CI regenerates and diffs, so it is the one that governs.
    """
    if oracle_path is None:
        return {}
    if not oracle_path.is_file():
        raise SealError(
            f"corpus_counts_from points at {oracle_path}, which does not "
            f"exist. Generate the oracle before sealing -- the manifest "
            f"records measured counts, never estimated ones (R1)."
        )
    document = json.loads(oracle_path.read_text(encoding="utf-8"))
    counts = document.get("counts")
    if not isinstance(counts, dict):
        raise SealError(f"{oracle_path} carries no `counts` block")
    return {k: v for k, v in counts.items() if k != "source"}


def build_manifest(config: SealConfig) -> Dict[str, Any]:
    """Assemble the unsigned manifest. Raises rather than record a guess."""
    if not config.root.is_dir():
        raise SealError(f"root is not a directory: {config.root}")

    entries = build_entries(config.root, config.include_rules)
    if not entries:
        raise SealError(
            f"the include set covered no files under {config.root}. Sealing an "
            f"empty manifest would produce a valid signature over nothing."
        )

    toolchain = probe_toolchain(config.toolchain_probes)
    require_complete_toolchain(toolchain)

    oracle_toolchain = {
        key: toolchain[key]
        for key in config.oracle_toolchain_keys
        if key in toolchain
    }
    missing = [k for k in config.oracle_toolchain_keys if k not in toolchain]
    if missing:
        raise SealError(
            f"oracle_toolchain names {missing} but no probe produced them; "
            f"the exact patch version of the oracle's compiler is a first-"
            f"class sealing condition, not an optional field"
        )

    return {
        "benchmark": config.name,
        "version": config.version,
        "tag": config.tag,
        "committed_at": datetime.now(timezone.utc).isoformat(),
        "file_count": len(entries),
        "files": entries,
        "payload_sha256": payload_sha256(entries),
        "toolchain": toolchain,
        # --- difference 4: the manifest self-describes (D12) ---
        "include_rules": config.include_rules.as_dict(),
        "expected_absent": sorted(config.expected_absent),
        "oracle_toolchain": oracle_toolchain,
        "corpus_counts": read_corpus_counts(config.corpus_counts_from),
    }


def seal(config: SealConfig, key_path: Path, sign_it: bool = True) -> Dict[str, Any]:
    """Build, optionally sign, and write the manifest."""
    manifest = build_manifest(config)
    if sign_it:
        manifest = sign(manifest, key_path)
        if not verify(manifest):
            raise SealError(
                "the manifest this run just signed does not verify under the "
                f"pinned fingerprint {EXPECTED_KEY_FINGERPRINT}. The key at "
                f"{key_path} is not the published signing key."
            )
    config.out_path.parent.mkdir(parents=True, exist_ok=True)
    config.out_path.write_text(json.dumps(manifest, indent=2), encoding="utf-8")
    return manifest


# --- CLI --------------------------------------------------------------------


def _split(value: Optional[str]) -> Optional[List[str]]:
    if value is None:
        return None
    return [item.strip() for item in value.split(",") if item.strip()]


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Seal a benchmark tree into a signed, self-describing manifest. "
            "Configuration comes from a TOML file so the include set -- which "
            "is part of what gets signed -- is reviewable in a diff rather "
            "than retyped on a command line."
        )
    )
    parser.add_argument("--config", required=True, type=Path)
    parser.add_argument("--key", type=Path,
                        default=Path.home() / "zil-keys" / "relian-bench-v1.pem")
    parser.add_argument("--sign", action="store_true",
                        help="sign the manifest (the ceremony; R4, operator only)")
    parser.add_argument("--dry-run", action="store_true",
                        help="build and print the manifest summary, write nothing")
    # Overrides. Present for tests and one-off checks, not for the ceremony.
    parser.add_argument("--name")
    parser.add_argument("--version")
    parser.add_argument("--tag")
    parser.add_argument("--root", type=Path)
    parser.add_argument("--out", type=Path)
    parser.add_argument("--include-dirs")
    parser.add_argument("--include-files")
    args = parser.parse_args(argv)

    try:
        config = load_config(args.config, root_override=args.root)
        for attribute, value in (
            ("name", args.name), ("version", args.version), ("tag", args.tag),
        ):
            if value:
                setattr(config, attribute, value)
        if args.out:
            config.out_path = args.out
        dirs = _split(args.include_dirs)
        if dirs is not None:
            config.include_rules.dirs = dirs
        files = _split(args.include_files)
        if files is not None:
            config.include_rules.files = files

        if args.dry_run:
            manifest = build_manifest(config)
        else:
            manifest = seal(config, args.key, sign_it=args.sign)
    except SealError as exc:
        print(f"SEAL FAILED: {exc}", file=sys.stderr)
        return 1

    print(f"benchmark        : {manifest['benchmark']} {manifest['version']}")
    print(f"tag              : {manifest['tag']}")
    print(f"root             : {config.root}")
    print(f"files            : {manifest['file_count']}")
    print(f"payload_sha256   : {manifest['payload_sha256']}")
    print(f"include_rules    : {manifest['include_rules']}")
    print(f"expected_absent  : {manifest['expected_absent'] or '(none)'}")
    print(f"oracle_toolchain : {manifest['oracle_toolchain']}")
    print(f"corpus_counts    : {manifest['corpus_counts']}")
    if "signature" in manifest:
        print(f"manifest_sha256  : {manifest['signature']['manifest_sha256']}")
        print(f"key_fingerprint  : {manifest['signature']['key_fingerprint']}")
        print(f"verify() pinned  : {verify(manifest)}")
    else:
        print("signature        : (unsigned -- pass --sign)")
    if not args.dry_run:
        print(f"written          : {config.out_path}")
    return 0


if __name__ == "__main__":  # pragma: no cover - CLI entry point
    raise SystemExit(main())
