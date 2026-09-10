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

FOUR DELIBERATE DIFFERENCES FROM `commit.py`
---------------------------------------------
Each is a defect in the v1.2 sealer that a signed record cannot be edited to
fix, so each is fixed here instead. Each has a test in `tests/test_seal.py`.

  **Difference 1 is RETIRED. It was:** *"an absent key file raises; there is no
  generation fallback"* -- true of this file and, at the time, not of
  `commit.py`, whose `sign()` fell through to `Ed25519PrivateKey.generate()`
  and wrote a fresh PEM, so a sealing run on a machine without the key
  succeeded and produced a manifest that `verify()` returned True on under a
  fingerprint nobody had ever seen.

  `153f40f` ("harness: refuse to sign with a silently generated key") removed
  that fallback. **Both files now refuse**, so the difference is no longer a
  difference and is not counted below. The behaviour itself is not retired --
  this sealer still never generates a key, `load_private_key` still raises, and
  `tests/test_seal.py::test_absent_key_raises_and_never_generates` still pins
  it. What is retired is the CONTRAST, and with it that share of the
  justification for a second implementation.

  Note what `153f40f` cost, because it is the reason WP-2.6 exists: `commit.py`
  is inside the v1.2 manifest (`bench/harness/` is an include dir), so fixing
  it broke the seal it produces. Difference 3 below is the escape from that
  loop, and it is why the remaining four are enough on their own.

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

  Differences 3 and 5 are load-bearing ON THEIR OWN, and that is what carries
  the duplication now that difference 1 is gone: `commit.py` hardcodes
  `"1.2.0"`, `"relian-bench-v1.2"` and its own output path, so parameterising
  it is the only way it could seal a second version -- and parameterising it is
  the exact edit that invalidated the v1.2 seal. The duplication survives on
  narrower grounds than it was written on, and this is the narrower ground.

ADDED FOR WP-2.6 (v1.3), BEYOND THE FOUR
-----------------------------------------
  * **`baselines_recorded` and `thresholds` are CARRIED FORWARD, never
    recomputed** (`load_carry_forward`). `commit.py` re-derives the baselines
    from `bench/results/*.json` at seal time; those are the pre-solution floor,
    and `bench/results/` is unsealed and has moved since v1.2. See that
    function's docstring for why the source is the signed ledger rather than
    literals in the config.
  * **`expected_absent` must match something actually recorded**
    (`require_declared_absences_are_present`). It declares files absent in the
    VERIFYING perimeter, not the sealing one; a ceremony run without the
    held-out corpus present would otherwise shrink the seal from 29 files to 21
    and still pass every downstream check.
  * **`vector_counts` can be measured, never carried**
    (`measure_vector_counts`) -- a carried count is a claim about a tree that
    was not the tree being sealed (R1). It is NOT enabled for v1.3: v1.2's
    block counts held-out vectors too (425 of them, the published BER's
    denominator), measuring those means opening `heldout.jsonl` (rule 1, R3),
    and a public-only block would show 7 keys where v1.2 shows 14. See
    `bench/seal.toml` for the omission and the one-line change that reverses
    it.
  * **`oracle_toolchain` records the compiler that builds the oracles.**
    `cobc`'s exact patch version, because the oracle's behaviour is the ground
    truth the BER is measured against. v1.2 recorded no such block.
  * **The passphrase path.** The release key was re-wrapped 2026-09-08;
    `load_private_key` used `password=None` unconditionally and would have
    raised at the ceremony. It now prompts with `getpass` -- never `argv`,
    never an environment variable, never logged.

WHAT THIS FILE IS NOT
---------------------
It is not sealed. It is a tool, not a benchmark artifact. It never reads a
private key from anywhere but the path it is given, never writes one, and never
prints one. Key custody is the operator's alone (R4).
"""

from __future__ import annotations

import argparse
import fnmatch
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
    #: Ledger to copy `carry_forward_keys` out of, verbatim, or None.
    carry_forward_from: Optional[Path] = None
    #: Top-level manifest keys copied from `carry_forward_from` rather than
    #: recomputed. See `load_carry_forward` for why this is not optional.
    carry_forward_keys: List[str] = field(default_factory=list)
    #: Glob, relative to root, of vector files whose line counts are MEASURED
    #: into `vector_counts`. Empty means the manifest carries no such block.
    vector_counts_glob: Optional[str] = None


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
    # `expected_absent` is accepted at the top level OR inside [include],
    # because TOML puts a bare key after a table header INSIDE that table and
    # the existing discovery-bench/seal.toml writes it below `[include]`. That
    # config declares `expected_absent = []`, so reading only the top level
    # returned the same empty list by default and the mismatch was invisible.
    # It would not have stayed invisible: bench/seal.toml declares EIGHT
    # absences (the seven held-out vector files and the generator), and
    # silently dropping them would seal a v1.3 claiming nothing is absent --
    # which then reports 8 files MISSING when CI verifies it in the public
    # perimeter. Discovered before the ceremony rather than during it.
    #
    # Both locations are read; declaring it in both with different values is
    # refused rather than resolved by precedence, because a config with two
    # answers has no correct one to pick.
    absent_top = data.get("expected_absent")
    absent_include = include.get("expected_absent")
    if (
        absent_top is not None
        and absent_include is not None
        and list(absent_top) != list(absent_include)
    ):
        raise SealError(
            f"{path}: expected_absent is declared both at the top level and "
            f"under [include], with different values. Declare it once."
        )
    expected_absent = list(
        absent_top if absent_top is not None else (absent_include or [])
    )

    probes = {k: list(v) for k, v in (data.get("toolchain", {}) or {}).items()}
    counts_from = seal.get("corpus_counts_from")
    carry = data.get("carry_forward", {}) or {}
    carry_from = carry.get("from")
    carry_keys = list(carry.get("keys", []))
    if carry_keys and not carry_from:
        raise SealError(
            f"{path}: [carry_forward] names keys {carry_keys} but no `from` "
            f"ledger to copy them out of. A carry-forward with no source is a "
            f"recomputation wearing its name."
        )
    return SealConfig(
        name=seal["name"],
        version=seal["version"],
        tag=seal["tag"],
        root=Path(root),
        out_path=(base / seal["out"]).resolve(),
        include_rules=rules,
        expected_absent=expected_absent,
        toolchain_probes=probes,
        oracle_toolchain_keys=list(seal.get("oracle_toolchain", [])),
        corpus_counts_from=(root / counts_from) if counts_from else None,
        carry_forward_from=(root / carry_from) if carry_from else None,
        carry_forward_keys=carry_keys,
        vector_counts_glob=seal.get("vector_counts_glob"),
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


def _is_encrypted_pem(pem: bytes) -> bool:
    """True when the PEM is a PKCS#8 ENCRYPTED PRIVATE KEY.

    Read from the armour header rather than by attempting a decrypt, so the
    decision is made without a failed load in between. The same one-line test
    `commit.py` uses since 153f40f, kept identical on purpose: two key loaders
    that disagree about what "encrypted" means is a ceremony that fails at the
    worst possible moment.
    """
    return b"ENCRYPTED" in pem.split(b"\n", 1)[0]


def load_private_key(key_path: Path, prompt: Optional[Any] = None):
    """Load the Ed25519 private key. Difference 1: NEVER generate one.

    Two refusals, not one:

      * **Absent file.** There is no generation fallback here and there is not
        going to be one. A generated key produces a manifest that `verify()`
        returns True on, under a fingerprint no third party holds -- worse than
        no manifest at all. Custody is the operator's (R4).

      * **`password=None` on an encrypted key.** The release key was re-wrapped
        with a passphrase on 2026-09-08 (still signing as
        `233bb4406e2de606`). This loader passed `password=None`
        unconditionally, so every invocation against the real key would have
        raised `TypeError: Password was not given but private key is
        encrypted` -- at the ceremony, with the key already unlocked on the
        operator's desk. The passphrase is read with `getpass`: never from
        `argv`, never from an environment variable, never logged, never
        written, and never carried in this repository.

    `prompt` is injectable for tests ONLY. It defaults to `getpass.getpass`, it
    is never wired to a CLI flag, and no caller in the ceremony path passes it.
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
    pem = key_path.read_bytes()
    password: Optional[bytes] = None
    if _is_encrypted_pem(pem):
        import getpass  # noqa: PLC0415 -- only needed on the encrypted path

        ask = prompt or getpass.getpass
        password = ask(f"Passphrase for {key_path.name}: ").encode("utf-8")
    try:
        return serialization.load_pem_private_key(pem, password=password)
    except (TypeError, ValueError) as exc:
        # Deliberately does not echo the passphrase, the PEM, or any byte of
        # either. The operator knows which key they pointed at; the tool does
        # not need to prove it read one.
        raise SealError(
            f"could not load the signing key at {key_path}: "
            f"{type(exc).__name__}. If the key is passphrase-encrypted the "
            f"passphrase was wrong; if it is not, it is not an Ed25519 "
            f"private key in PEM form. Nothing was signed."
        ) from None


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


def load_carry_forward(
    source: Optional[Path], keys: Sequence[str]
) -> Dict[str, Any]:
    """Copy named top-level blocks out of a previous ledger, VERBATIM.

    WHY THIS EXISTS RATHER THAN A RECOMPUTATION (WP-2.6 U1/U2)
    ----------------------------------------------------------
    `commit.py.__main__` derives `baselines_recorded` by re-reading
    `bench/results/*.json` at seal time and copying out `ber_overall`,
    `build_rate` and `coverage_branch`. Its own comment says what those numbers
    are: *"the measured floor BEFORE any solution work (Phase 4 requirement)"*.

    `bench/results/` is not in any include set, so it is unsealed and mutable,
    and it has in fact moved since v1.2 was sealed -- `C1_rulebased.json`
    changed by 66 lines between `e286cb3` and `153f40f` as P06 and P07 gained
    per-program entries. A sealer that re-derives therefore records whatever
    that file says on the day of the ceremony. The resulting manifest would be
    internally consistent, signature-valid under the real key, and would have
    quietly replaced a pre-solution floor with a post-solution number. Nothing
    downstream could detect it, because every check downstream verifies the
    manifest against itself.

    `thresholds` is the same hazard pointed at the merge gates: `bench.yml`
    reads `ledger['thresholds']` for BER >= 0.95, build_rate 1.00 and branch
    coverage >= 0.80 (R10). A changed key there moves what passes, silently.

    So neither is recomputed. Both are copied out of the ledger named in
    `[carry_forward] from`, which for v1.3 is the SIGNED v1.2 ledger -- the
    artifact whose own Ed25519 signature is what makes the copy trustworthy.

    WHY THE SOURCE IS A LEDGER AND NOT LITERALS IN `seal.toml`
    ----------------------------------------------------------
    Transcribing the blocks into TOML was the obvious alternative and it is not
    merely worse, it is impossible without lying:
    `baselines_recorded.B0_null.coverage_branch` is `null`, and **TOML has no
    null**. Writing it out would force either dropping the key -- changing the
    block -- or substituting a sentinel, which is a constant standing in for an
    unmeasured value and a straight R1 violation. Copying JSON to JSON has
    neither problem, and makes byte-equality structural rather than clerical.

    A missing key raises. A carry-forward that silently records nothing is the
    failure it exists to prevent.
    """
    if not keys:
        return {}
    if source is None:
        raise SealError("carry-forward keys were requested with no source ledger")
    if not source.is_file():
        raise SealError(
            f"carry-forward source {source} does not exist. v1.3 carries "
            f"`baselines_recorded` and `thresholds` forward from the signed "
            f"v1.2 ledger; without it there is nothing to carry and "
            f"recomputing them is precisely what must not happen."
        )
    document = json.loads(source.read_text(encoding="utf-8"))
    missing = [key for key in keys if key not in document]
    if missing:
        raise SealError(
            f"{source} carries no {', '.join(missing)}; refusing to seal a "
            f"manifest that would silently omit a block the previous seal "
            f"recorded"
        )
    # Round-tripped through JSON so the copy shares no mutable structure with
    # the parsed source, and so anything unrepresentable would fail here rather
    # than at signing time.
    return {key: json.loads(json.dumps(document[key])) for key in keys}


def measure_vector_counts(root: Path, glob: Optional[str]) -> Dict[str, int]:
    """Non-blank line counts per vector file, MEASURED from the tree.

    v1.2 records a `vector_counts` block. Dropping it from v1.3 would make the
    newer seal strictly less informative than the one it supersedes, so it is
    kept -- but it is kept by MEASURING the current tree, never by carrying
    v1.2's numbers forward. A carried count is a claim about a tree that was
    not the tree being sealed; that it happens to be true today is not
    something the manifest could assert (R1).

    Held-out vector files are not matched by the configured glob and are never
    read here (rule 1 / R3). The glob names `public.jsonl` explicitly rather
    than `*.jsonl` for exactly that reason.
    """
    if not glob:
        return {}
    counts: Dict[str, int] = {}
    for path in sorted(root.glob(glob), key=lambda p: p.relative_to(root).as_posix()):
        if not path.is_file():
            continue
        text = path.read_text(encoding="utf-8")
        counts[path.relative_to(root).as_posix()] = len(
            [line for line in text.splitlines() if line.strip()]
        )
    if not counts:
        raise SealError(
            f"vector_counts_glob {glob!r} matched no files under {root}. An "
            f"empty measured block is indistinguishable from a corpus with no "
            f"vectors; refusing to record either."
        )
    return counts


def require_declared_absences_are_present(
    patterns: Sequence[str], recorded: Sequence[str]
) -> None:
    """Every `expected_absent` pattern must match something being SEALED.

    `expected_absent` does not mean "absent here". It means "recorded in this
    manifest, and legitimately absent in the perimeter that VERIFIES it" -- the
    seven held-out vector files and the vector generator, which are sealed on
    the operator's machine and are never in the public repository (rule 1,
    rule 6, R3).

    That distinction is load-bearing at seal time. If the ceremony runs on a
    machine where those files are not present, the walk never sees them, they
    are never recorded, and `expected_absent` becomes a declaration about files
    the manifest does not contain. The seal would cover 21 files while claiming
    to account for 29, `verify_manifest.py` would report
    `declared_absent_but_present: 0` and pass, and the held-out corpus would
    have quietly dropped out of the benchmark's integrity claim altogether.

    v1.2 records 29 files, 8 of them declared absent. So must v1.3. This is the
    check that makes the sealing machine's completeness a precondition rather
    than something noticed afterwards. Contents are never read -- only the
    presence of a matching recorded path.
    """
    if not patterns:
        return
    unmatched = [
        pattern
        for pattern in patterns
        if not any(fnmatch.fnmatch(path, pattern) for path in recorded)
    ]
    if unmatched:
        raise SealError(
            "refusing to seal: expected_absent declares "
            + ", ".join(repr(p) for p in unmatched)
            + " but the include walk recorded no file matching them, so the "
              "declaration would describe files this manifest does not "
              "contain. Seal on a machine that holds the complete benchmark "
              "-- v1.2 records 29 files, 8 of them declared absent, and a "
              "v1.3 recording only what a public checkout can see would "
              "silently shrink the seal."
        )


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

    expected_absent = sorted(config.expected_absent)
    require_declared_absences_are_present(
        expected_absent, [entry["path"] for entry in entries]
    )

    manifest = {
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
        "expected_absent": expected_absent,
        "oracle_toolchain": oracle_toolchain,
        "corpus_counts": read_corpus_counts(config.corpus_counts_from),
    }

    vector_counts = measure_vector_counts(config.root, config.vector_counts_glob)
    if vector_counts:
        manifest["vector_counts"] = vector_counts

    # Carried LAST and by assignment, so a carried block can never be
    # overwritten by something this function computed. A carry-forward that
    # loses a race with a recomputation is a recomputation.
    carried = load_carry_forward(config.carry_forward_from, config.carry_forward_keys)
    for key, value in carried.items():
        if key in manifest:
            raise SealError(
                f"carry-forward key {key!r} collides with a block this sealer "
                f"computes. One of the two is wrong and guessing which is not "
                f"this tool's decision."
            )
        manifest[key] = value
    return manifest


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
