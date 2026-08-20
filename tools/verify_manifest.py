#!/usr/bin/env python3
"""Standalone integrity verifier for a RELIAN-BENCH sealed ledger.

WHAT THIS PROVES THAT THE SIGNATURE CHECK DOES NOT
--------------------------------------------------
`.github/workflows/bench.yml` verifies the Ed25519 signature over the
manifest. That proves the *manifest* was not edited. It does not prove the
*tree* still matches the manifest. Those are different claims, and only the
second one catches a silent edit to a file under `bench/`: change a byte in
`corpus/P03_eligibility/program.cbl` and the signature still verifies
perfectly, because the signature covers the manifest's bytes, not the
corpus's.

This script checks three layers INDEPENDENTLY and reports which one failed:

  LAYER 1  TREE       every path in `files[]` exists at `root/<path>` and
                      hashes to its recorded `sha256`; and, walking the
                      include set in reverse, no file exists on disk that
                      `files[]` does not record. Hash-checking only the
                      recorded entries would not notice an ADDED file.
  LAYER 2  PAYLOAD    `payload_sha256`, recomputed from `files[]`, equals the
                      recorded value. This is what catches a *consistently*
                      edited `files[]` -- an attacker who changes a corpus
                      file and updates its recorded hash to match passes
                      layer 1 and fails here.
  LAYER 3  SIGNATURE  the Ed25519 signature verifies over `manifest_hash()`
                      -- the manifest minus its `signature` key.

Each layer is evaluated even when an earlier one fails, so a single run tells
you the full picture rather than the first thing that broke.

WHY IT LIVES IN tools/ AND IS DELIBERATELY NOT SEALED
-----------------------------------------------------
A verifier sealed inside the artifact it verifies is circular: it would be
attesting to its own integrity with the same key it is checking. This file is
outside the sealed set on purpose, so a third party can read it, run it, or
throw it away and write their own from the description above.

It imports NOTHING from this repository -- in particular not
`bench/harness/commit.py`, whose hashing it reimplements from scratch. A
verifier that calls the sealer's own code cannot detect a bug in the sealer's
own code. Dependencies are the standard library plus `cryptography`. It reads
no credentials and touches no network.

A KNOWN LIMITATION OF THE v1.2 MANIFEST FORMAT
----------------------------------------------
The v1.2 manifest records `files[]`, `payload_sha256` and a signature. It does
NOT record the include/exclude rules that produced `files[]`. Those rules live
only in the sealer's source (`bench/harness/commit.py`: INCLUDE_DIRS,
INCLUDE_FILES, EXCLUDE_SUFFIX, EXCLUDE_NAMES, EXCLUDE_BINARIES) and are
therefore outside the signature. A verifier cannot re-walk the tree without
being told them.

That is why they are command-line arguments here rather than being read out of
the manifest, and it is a real weakness worth stating plainly rather than
working around silently: an attacker who can also edit `commit.py` can change
what the *next* seal covers. The defaults below are transcribed from v1.2's
sealer so the common invocation is short, but they are defaults, not
attestations -- pass them explicitly if you want the walk to be your claim
rather than this file's.

This limitation is RECORDED, NOT RETROFITTED. v1.2 is signed; teaching this
verifier to guess what v1.2 meant would be inventing an attestation the signer
never made. The argument path below stays exactly as it was, and stays the only
path available for v1.2.

`--from-manifest`, AND WHY IT IS THE RIGHT WAY ROUND (WP-2.1, D12)
------------------------------------------------------------------
The RELIAN-DISCOVERY-BENCH v0.1 manifest records its own `include_rules` and
`expected_absent` INSIDE the signed payload. `--from-manifest` reads them from
there, so verification needs no argument beyond `--ledger`.

The difference is not ergonomic. With rules on the command line, the person
running the check decides what counts as included and what is allowed to be
missing -- so a dropped `--include-dirs` silently narrows the claim, and an
`--expect-absent` pattern invented at the prompt excuses whatever happens not
to be there. With rules in the payload, both are declared by the SIGNER and
covered by the signature: changing either changes `manifest_sha256` and breaks
it. An absence should be something the signer admitted to, not something the
verifier was told to overlook.

Under `--from-manifest`, `--root` defaults to the directory holding the ledger,
because that is where a self-describing manifest's paths are relative to.

A SECOND LIMITATION, IN THE SIGNATURE ITSELF
--------------------------------------------
Layer 3 verifies with the public key EMBEDDED IN THE MANIFEST. That proves the
manifest is internally consistent and self-signed; it does not prove who signed
it. Anyone can re-sign an edited manifest with a key they generated and it will
verify -- and layers 1 and 2 pass too, because a forger who re-seals a tree
they control has every hash internally consistent. Pinning the expected signer
is the ONLY layer that catches it. Pass `--pin-fingerprint` (equivalently
`--key-fingerprint`, kept for the existing v1.2 invocation) or
`--public-key-hex`; without a pin, layer 3 says so in its output rather than
overclaiming.

`tests/test_seal.py::test_resigned_forgery_passes_three_layers_and_fails_the_pin`
is the proof: it re-signs a valid manifest with an ephemeral attacker key and
asserts all three layers pass while the pin fails.

USAGE
-----
    # v1.2 -- rules on the command line, because the manifest does not carry them
    python3 tools/verify_manifest.py \
        --ledger bench/LEDGER_relian-bench-v1.2.json \
        --root bench --include-dirs corpus,harness --include-files SPEC.md \
        --pin-fingerprint 233bb4406e2de606

    # v0.1 discovery bench -- rules come out of the signed payload
    python3 tools/verify_manifest.py \
        --ledger discovery-bench/LEDGER_relian-discovery-bench-v0.1.json \
        --from-manifest --pin-fingerprint 233bb4406e2de606

Exit status is 0 only if all three layers pass. `--json` writes a
machine-readable report to stdout instead of the human one.
"""

from __future__ import annotations

import argparse
import fnmatch
import hashlib
import json
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Sequence

# --- Defaults transcribed from bench/harness/commit.py at v1.2 -------------
# Not read from the manifest, because the manifest does not record them. See
# "A KNOWN LIMITATION OF THE v1.2 MANIFEST FORMAT" above.
DEFAULT_EXCLUDE_SUFFIXES = (".pyc", ".o")
DEFAULT_EXCLUDE_PATH_PARTS = ("__pycache__", "_classes")
DEFAULT_EXCLUDE_BASENAMES = ("payroll01", "run")


# --- Hashing, reimplemented from the format spec ---------------------------
# Two conventions are load-bearing and are the reason this is not just
# `sha256sum`: paths are hashed in their POSIX string form (never native
# separators) and sorted by that same string (never by Path objects, whose
# ordering diverges with case sensitivity). Get either wrong and a manifest
# sealed on Linux will not verify on Windows.

def sha256_file(path: Path) -> str:
    """SHA-256 of a file's bytes, streamed so a large vector file is fine."""
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def compute_payload_sha256(files: Sequence[Dict[str, str]]) -> str:
    """Recompute `payload_sha256` from a `files[]` list.

    The sealer serialises the ENTRY LIST -- not the whole manifest -- with
    sorted keys and no whitespace, then hashes the UTF-8 bytes.
    """
    blob = json.dumps(files, sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(blob.encode("utf-8")).hexdigest()


def compute_manifest_hash(manifest: Dict[str, Any]) -> str:
    """The value the signature is taken over: the manifest minus `signature`."""
    core = {k: v for k, v in manifest.items() if k != "signature"}
    blob = json.dumps(core, sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(blob.encode("utf-8")).hexdigest()


# --- The walk --------------------------------------------------------------

@dataclass(frozen=True)
class WalkRules:
    include_dirs: Sequence[str]
    include_files: Sequence[str]
    exclude_suffixes: Sequence[str]
    exclude_path_parts: Sequence[str]
    exclude_basenames: Sequence[str]

    def is_excluded(self, path: Path, root: Path) -> bool:
        rel = path.relative_to(root)
        if any(part in self.exclude_path_parts for part in rel.parts):
            return True
        if path.suffix in self.exclude_suffixes:
            return True
        if path.name in self.exclude_basenames:
            return True
        return False


def walk_include_set(root: Path, rules: WalkRules) -> List[str]:
    """Every file the include set covers, as sorted POSIX-relative strings."""
    found: List[str] = []
    for directory in rules.include_dirs:
        base = root / directory
        if not base.is_dir():
            continue
        for path in base.rglob("*"):
            if not path.is_file():
                continue
            if rules.is_excluded(path, root):
                continue
            found.append(path.relative_to(root).as_posix())
    for name in rules.include_files:
        path = root / name
        if path.is_file() and not rules.is_excluded(path, root):
            found.append(path.relative_to(root).as_posix())
    return sorted(set(found))


def matches_any(path: str, patterns: Iterable[str]) -> bool:
    """fnmatch semantics -- note `*` crosses `/`, unlike gitignore."""
    return any(fnmatch.fnmatch(path, pattern) for pattern in patterns)


class SelfDescriptionError(ValueError):
    """The manifest was asked for its own rules and does not carry them."""


def rules_from_manifest(
    manifest: Dict[str, Any]
) -> "tuple[WalkRules, List[str]]":
    """Read `include_rules` and `expected_absent` out of the signed payload.

    Both keys live inside the region covered by `manifest_hash()`, so a
    manifest cannot claim one include set while having been sealed under
    another without breaking its own signature.

    Raises `SelfDescriptionError` for a manifest that does not self-describe --
    v1.2, in particular. That is a refusal, not a fallback: silently reverting
    to the transcribed defaults would let `--from-manifest` report a walk the
    signer never attested to, under a flag whose entire promise is that the
    signer did.
    """
    rules = manifest.get("include_rules")
    if not isinstance(rules, dict):
        raise SelfDescriptionError(
            "this manifest records no `include_rules`, so its include set "
            "cannot be read from the signed payload. That is the v1.2 format "
            "limitation, and it is recorded rather than worked around: pass "
            "--include-dirs / --include-files / --expect-absent explicitly, "
            "and understand that the walk is then YOUR claim, not the "
            "signer's."
        )
    if "expected_absent" not in manifest:
        raise SelfDescriptionError(
            "this manifest records `include_rules` but no `expected_absent`. "
            "An absence that the signer did not declare is not an absence this "
            "verifier will excuse."
        )
    expect_absent = manifest.get("expected_absent") or []
    if not isinstance(expect_absent, list):
        raise SelfDescriptionError("`expected_absent` is not a list")

    return (
        WalkRules(
            include_dirs=tuple(rules.get("dirs", ())),
            include_files=tuple(rules.get("files", ())),
            exclude_suffixes=tuple(rules.get("exclude_suffix", ())),
            exclude_path_parts=tuple(rules.get("exclude_names", ())),
            # The discovery sealer has no basename-exclusion rule; v1.2's
            # EXCLUDE_BINARIES has no counterpart in the recorded format, and
            # inventing one here would be exactly the retrofit this flag
            # exists to avoid.
            exclude_basenames=(),
        ),
        [str(pattern) for pattern in expect_absent],
    )


# --- Layer results ---------------------------------------------------------

@dataclass
class Layer:
    name: str
    title: str
    ok: bool = False
    detail: Dict[str, Any] = field(default_factory=dict)
    problems: List[str] = field(default_factory=list)


@dataclass
class Report:
    layers: List[Layer] = field(default_factory=list)
    context: Dict[str, Any] = field(default_factory=dict)
    fatal: Optional[str] = None

    @property
    def ok(self) -> bool:
        return self.fatal is None and bool(self.layers) and all(l.ok for l in self.layers)

    def __bool__(self) -> bool:
        return self.ok

    def layer(self, name: str) -> Layer:
        for candidate in self.layers:
            if candidate.name == name:
                return candidate
        raise KeyError(name)

    def failed_layers(self) -> List[str]:
        return [l.name for l in self.layers if not l.ok]


# --- The three layers ------------------------------------------------------

def check_tree(manifest, root: Path, rules: WalkRules, expect_absent) -> Layer:
    layer = Layer("tree", "recorded files exist and hash to their recorded sha256")
    recorded = manifest.get("files")
    if not isinstance(recorded, list):
        layer.problems.append("manifest has no `files[]` list")
        return layer

    verified: List[str] = []
    mismatched: List[Dict[str, str]] = []
    missing: List[str] = []
    declared_absent: List[str] = []
    declared_absent_but_present: List[str] = []
    unreadable: List[Dict[str, str]] = []

    for entry in recorded:
        rel = entry.get("path")
        expected = entry.get("sha256")
        if not rel or not expected:
            layer.problems.append(f"malformed files[] entry: {entry!r}")
            continue
        path = root / rel
        exists = path.is_file()
        if matches_any(rel, expect_absent):
            # Declared absent on the command line. It is NOT verified -- its
            # content is not available in this perimeter, and an unverified
            # file is never counted as a verified one. If it turns up anyway,
            # that is itself a finding, and the file is deliberately not read.
            if exists:
                declared_absent_but_present.append(rel)
            else:
                declared_absent.append(rel)
            continue
        if not exists:
            missing.append(rel)
            continue
        try:
            actual = sha256_file(path)
        except OSError as exc:
            unreadable.append({"path": rel, "error": str(exc)})
            continue
        if actual == expected:
            verified.append(rel)
        else:
            mismatched.append({"path": rel, "expected": expected, "actual": actual})

    # The reverse direction. Hash-checking only the recorded entries cannot
    # notice a file that was ADDED to the tree after sealing, so re-walk the
    # include set and diff against what the manifest records.
    on_disk = walk_include_set(root, rules)
    recorded_paths = {e.get("path") for e in recorded if isinstance(e, dict)}
    unrecorded = sorted(p for p in on_disk if p not in recorded_paths)

    layer.detail = {
        "recorded": len(recorded),
        "verified": len(verified),
        "hash_mismatches": len(mismatched),
        "missing": len(missing),
        "declared_absent": len(declared_absent),
        "declared_absent_but_present": len(declared_absent_but_present),
        "unreadable": len(unreadable),
        "unrecorded_on_disk": len(unrecorded),
        "walked_on_disk": len(on_disk),
        "mismatched_paths": mismatched,
        "missing_paths": missing,
        "declared_absent_paths": declared_absent,
        "declared_absent_but_present_paths": declared_absent_but_present,
        "unreadable_paths": unreadable,
        "unrecorded_paths": unrecorded,
    }

    if mismatched:
        layer.problems.append(
            f"{len(mismatched)} recorded file(s) present but hashing to a "
            f"DIFFERENT sha256 than the sealed manifest records"
        )
    if missing:
        layer.problems.append(
            f"{len(missing)} recorded file(s) absent from the tree and not "
            f"declared absent via --expect-absent"
        )
    if declared_absent_but_present:
        layer.problems.append(
            f"{len(declared_absent_but_present)} path(s) declared absent via "
            f"--expect-absent are PRESENT on disk; not hashed, reported as-is"
        )
    if unreadable:
        layer.problems.append(f"{len(unreadable)} recorded file(s) could not be read")
    if unrecorded:
        layer.problems.append(
            f"{len(unrecorded)} file(s) present in the include set but ABSENT "
            f"from files[] -- added since sealing, or outside the sealer's "
            f"exclude rules (see --exclude-* and the format limitation in the "
            f"module docstring)"
        )
    layer.ok = not layer.problems
    return layer


def check_payload(manifest) -> Layer:
    layer = Layer("payload", "payload_sha256 recomputed from files[]")
    recorded = manifest.get("payload_sha256")
    files = manifest.get("files")
    if not isinstance(files, list):
        layer.problems.append("manifest has no `files[]` list to recompute from")
        return layer
    if not recorded:
        layer.problems.append("manifest records no `payload_sha256`")
        return layer
    recomputed = compute_payload_sha256(files)
    layer.detail = {"recorded": recorded, "recomputed": recomputed}
    if recomputed != recorded:
        layer.problems.append(
            f"payload_sha256 recomputed from files[] is {recomputed}, but the "
            f"manifest records {recorded} -- files[] was edited after sealing"
        )
    layer.ok = not layer.problems
    return layer


def check_signature(manifest, key_fingerprint=None, public_key_hex=None) -> Layer:
    layer = Layer("signature", "Ed25519 signature over manifest_hash()")
    block = manifest.get("signature")
    if not isinstance(block, dict):
        layer.problems.append("manifest carries no `signature` block")
        return layer

    alg = block.get("alg")
    recorded_hash = block.get("manifest_sha256")
    signature_hex = block.get("signature_hex")
    embedded_key = block.get("public_key_hex")
    recomputed_hash = compute_manifest_hash(manifest)
    embedded_fingerprint = (
        hashlib.sha256(bytes.fromhex(embedded_key)).hexdigest()[:16]
        if embedded_key else None
    )

    layer.detail = {
        "alg": alg,
        "recorded_manifest_sha256": recorded_hash,
        "recomputed_manifest_sha256": recomputed_hash,
        "public_key_hex": embedded_key,
        "embedded_key_fingerprint": embedded_fingerprint,
        "recorded_key_fingerprint": block.get("key_fingerprint"),
        "signer_pinned": bool(key_fingerprint or public_key_hex),
        "signature_valid": False,
    }

    if alg != "Ed25519":
        layer.problems.append(f"unsupported signature algorithm {alg!r}; expected Ed25519")
        return layer
    if not (recorded_hash and signature_hex and embedded_key):
        layer.problems.append("signature block is missing one of "
                              "manifest_sha256 / signature_hex / public_key_hex")
        return layer

    if recomputed_hash != recorded_hash:
        layer.problems.append(
            f"manifest_hash() recomputed over the manifest minus its signature "
            f"is {recomputed_hash}, but the signature block records "
            f"{recorded_hash} -- the manifest body was edited after signing"
        )

    # The signer pin. Checked BEFORE the cryptographic verification, because a
    # valid signature by the wrong key is the failure mode that a bare
    # verify() cannot see: the key it trusts is the one the attacker supplied.
    if public_key_hex and embedded_key.lower() != public_key_hex.lower():
        layer.problems.append(
            f"embedded public key {embedded_key} does not match the pinned "
            f"--public-key-hex {public_key_hex}"
        )
    if key_fingerprint and embedded_fingerprint != key_fingerprint.lower():
        layer.problems.append(
            f"embedded key fingerprint {embedded_fingerprint} does not match "
            f"the pinned --key-fingerprint {key_fingerprint}"
        )
    if block.get("key_fingerprint") and embedded_fingerprint != block["key_fingerprint"]:
        layer.problems.append(
            f"the manifest's own key_fingerprint {block['key_fingerprint']} is "
            f"not the fingerprint of the public key it embeds "
            f"({embedded_fingerprint})"
        )

    try:
        from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PublicKey
    except ImportError:  # pragma: no cover - environment failure, not a verdict
        layer.problems.append(
            "the `cryptography` package is not installed, so the signature "
            "could not be checked. An unchecked signature is not a valid one."
        )
        return layer

    try:
        Ed25519PublicKey.from_public_bytes(bytes.fromhex(embedded_key)).verify(
            bytes.fromhex(signature_hex), recorded_hash.encode("utf-8")
        )
        layer.detail["signature_valid"] = True
    except Exception as exc:
        layer.problems.append(
            f"Ed25519 verification failed over the recorded manifest_sha256 "
            f"({type(exc).__name__})"
        )

    layer.ok = not layer.problems
    return layer


# --- Orchestration ---------------------------------------------------------

def verify(
    ledger: Path,
    root: Optional[Path] = None,
    include_dirs: Sequence[str] = ("corpus", "harness"),
    include_files: Sequence[str] = ("SPEC.md",),
    exclude_suffixes: Sequence[str] = DEFAULT_EXCLUDE_SUFFIXES,
    exclude_path_parts: Sequence[str] = DEFAULT_EXCLUDE_PATH_PARTS,
    exclude_basenames: Sequence[str] = DEFAULT_EXCLUDE_BASENAMES,
    expect_absent: Sequence[str] = (),
    key_fingerprint: Optional[str] = None,
    public_key_hex: Optional[str] = None,
    from_manifest: bool = False,
) -> Report:
    """Run all three layers. The returned Report is falsy unless all pass.

    With `from_manifest=True` the include rules and `expected_absent` are read
    from the signed payload instead of from these arguments, and `root`
    defaults to the ledger's own directory.
    """
    report = Report()
    ledger = Path(ledger)
    report.context = {
        "ledger": str(ledger),
        "rules_source": "manifest" if from_manifest else "arguments",
    }

    if not ledger.is_file():
        report.fatal = f"ledger not found: {ledger}"
        return report
    try:
        manifest = json.loads(ledger.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        report.fatal = f"ledger is not readable JSON: {type(exc).__name__}: {exc}"
        return report
    if not isinstance(manifest, dict):
        report.fatal = "ledger JSON is not an object"
        return report

    if from_manifest:
        try:
            manifest_rules, manifest_absent = rules_from_manifest(manifest)
        except SelfDescriptionError as exc:
            report.fatal = f"--from-manifest: {exc}"
            return report
        include_dirs = manifest_rules.include_dirs
        include_files = manifest_rules.include_files
        exclude_suffixes = manifest_rules.exclude_suffixes
        exclude_path_parts = manifest_rules.exclude_path_parts
        exclude_basenames = manifest_rules.exclude_basenames
        expect_absent = manifest_absent
        if root is None:
            # A self-describing manifest's paths are relative to the tree it
            # seals, and the ledger is written at that tree's root.
            root = ledger.resolve().parent

    if root is None:
        report.fatal = (
            "--root is required unless --from-manifest can supply it"
        )
        return report
    root = Path(root)
    report.context.update({
        "root": str(root),
        "include_dirs": list(include_dirs),
        "include_files": list(include_files),
        "exclude_suffixes": list(exclude_suffixes),
        "exclude_path_parts": list(exclude_path_parts),
        "exclude_basenames": list(exclude_basenames),
        "expect_absent": list(expect_absent),
    })
    if not root.is_dir():
        report.fatal = f"root is not a directory: {root}"
        return report

    report.context.update({
        "benchmark": manifest.get("benchmark"),
        "version": manifest.get("version"),
        "tag": manifest.get("tag"),
        "committed_at": manifest.get("committed_at"),
        "file_count": manifest.get("file_count"),
        "oracle_toolchain": manifest.get("oracle_toolchain"),
        "corpus_counts": manifest.get("corpus_counts"),
    })

    rules = WalkRules(include_dirs, include_files, exclude_suffixes,
                      exclude_path_parts, exclude_basenames)
    report.layers = [
        check_tree(manifest, root, rules, expect_absent),
        check_payload(manifest),
        check_signature(manifest, key_fingerprint, public_key_hex),
    ]

    # `file_count` is a claim the manifest makes about itself; it is inside the
    # signature, so a mismatch means the sealer disagreed with itself.
    declared = manifest.get("file_count")
    actual = len(manifest.get("files") or [])
    if isinstance(declared, int) and declared != actual:
        tree = report.layer("tree")
        tree.problems.append(
            f"manifest records file_count {declared} but files[] holds {actual}"
        )
        tree.ok = False
    return report


def render(report: Report) -> str:
    ctx = report.context
    out: List[str] = []
    out.append("RELIAN-BENCH manifest verifier (tools/verify_manifest.py)")
    out.append("  This is NOT the signature check in bench.yml. That proves the")
    out.append("  manifest was not edited; this proves the tree still matches it.")
    out.append("")
    out.append(f"  ledger        : {ctx.get('ledger')}")
    out.append(f"  root          : {ctx.get('root')}")
    if report.fatal:
        out.append("")
        out.append(f"FATAL: {report.fatal}")
        out.append("VERDICT: FAIL (no layer could be evaluated)")
        return "\n".join(out)
    out.append(f"  benchmark     : {ctx.get('benchmark')} {ctx.get('version')} "
               f"({ctx.get('tag')})")
    out.append(f"  sealed at     : {ctx.get('committed_at')}")
    out.append(f"  include dirs  : {', '.join(ctx['include_dirs']) or '(none)'}")
    out.append(f"  include files : {', '.join(ctx['include_files']) or '(none)'}")
    out.append(f"  excluded      : suffixes {list(ctx['exclude_suffixes'])}, "
               f"path parts {list(ctx['exclude_path_parts'])}, "
               f"basenames {list(ctx['exclude_basenames'])}")
    out.append(f"  expect absent : {', '.join(ctx['expect_absent']) or '(none)'}")
    if ctx.get("rules_source") == "manifest":
        out.append("  rules source  : THE SIGNED MANIFEST (--from-manifest)")
        out.append("        The four lines above were declared by the signer and are")
        out.append("        covered by the signature, not asserted by this run.")
        if ctx.get("oracle_toolchain"):
            out.append(f"  oracle cobc   : {ctx['oracle_toolchain']}")
        if ctx.get("corpus_counts"):
            out.append(f"  corpus counts : {ctx['corpus_counts']}")
    else:
        out.append("  rules source  : THIS COMMAND LINE")
        out.append("        The v1.2 manifest does not record its own include/exclude")
        out.append("        rules, so the four lines above are arguments to this run,")
        out.append("        not attestations from the signed manifest. That is a")
        out.append("        format limitation of v1.2, recorded rather than retrofitted.")

    for index, layer in enumerate(report.layers, start=1):
        out.append("")
        out.append(f"LAYER {index}/{len(report.layers)}  {layer.name.upper()} "
                   f"-- {layer.title}")
        if layer.name == "tree":
            for key in ("recorded", "verified", "hash_mismatches", "missing",
                        "declared_absent", "declared_absent_but_present",
                        "unreadable", "walked_on_disk", "unrecorded_on_disk"):
                if key in layer.detail:
                    out.append(f"    {key:<28}: {layer.detail[key]}")
        if layer.name == "payload":
            out.append(f"    {'recorded':<28}: {layer.detail.get('recorded')}")
            out.append(f"    {'recomputed':<28}: {layer.detail.get('recomputed')}")
        if layer.name == "signature":
            out.append(f"    {'alg':<28}: {layer.detail.get('alg')}")
            out.append(f"    {'recorded manifest_sha256':<28}: "
                       f"{layer.detail.get('recorded_manifest_sha256')}")
            out.append(f"    {'recomputed manifest_sha256':<28}: "
                       f"{layer.detail.get('recomputed_manifest_sha256')}")
            out.append(f"    {'key fingerprint':<28}: "
                       f"{layer.detail.get('embedded_key_fingerprint')}")
            out.append(f"    {'signature verifies':<28}: "
                       f"{layer.detail.get('signature_valid')}")
            if not layer.detail.get("signer_pinned"):
                out.append("    CAVEAT: no --key-fingerprint / --public-key-hex was")
                out.append("            given, so this layer proves the manifest is")
                out.append("            self-consistent and self-signed, NOT who")
                out.append("            signed it. A re-signed forgery passes.")

        for path_key, label in (
            ("mismatched_paths", "HASH MISMATCH"),
            ("missing_paths", "MISSING"),
            ("declared_absent_but_present_paths", "DECLARED ABSENT BUT PRESENT"),
            ("unreadable_paths", "UNREADABLE"),
            ("unrecorded_paths", "UNRECORDED ON DISK"),
        ):
            for item in layer.detail.get(path_key) or []:
                if isinstance(item, dict) and "expected" in item:
                    out.append(f"    {label}: {item['path']}")
                    out.append(f"        recorded {item['expected']}")
                    out.append(f"        actual   {item['actual']}")
                elif isinstance(item, dict):
                    out.append(f"    {label}: {item['path']} -- {item.get('error')}")
                else:
                    out.append(f"    {label}: {item}")
        for item in layer.detail.get("declared_absent_paths") or []:
            out.append(f"    declared absent (NOT verified): {item}")

        out.append(f"    RESULT: {'PASS' if layer.ok else 'FAIL'}")
        for problem in layer.problems:
            out.append(f"    LAYER FAILED: {problem}")

    out.append("")
    passed = sum(1 for l in report.layers if l.ok)
    if report.ok:
        out.append(f"VERDICT: PASS ({passed}/{len(report.layers)} layers)")
    else:
        out.append(f"VERDICT: FAIL ({passed}/{len(report.layers)} layers passed; "
                   f"failed: {', '.join(report.failed_layers())})")
    return "\n".join(out)


def _split(value: Optional[str]) -> List[str]:
    if not value:
        return []
    return [item.strip() for item in value.split(",") if item.strip()]


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = argparse.ArgumentParser(
        description="Verify a RELIAN-BENCH sealed ledger against the tree it seals.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument("--ledger", required=True, type=Path,
                        help="path to the LEDGER_*.json manifest")
    parser.add_argument("--root", type=Path, default=None,
                        help="directory the manifest's paths are relative to. "
                             "Required unless --from-manifest, which defaults "
                             "it to the ledger's own directory.")
    parser.add_argument("--from-manifest", action="store_true",
                        help="read include_rules and expected_absent out of "
                             "the SIGNED payload instead of from arguments, so "
                             "the include set and the declared absences are the "
                             "signer's claim rather than the caller's. Refuses "
                             "on a manifest that does not self-describe (v1.2) "
                             "rather than silently falling back to defaults.")
    parser.add_argument("--include-dirs", default="corpus,harness",
                        help="comma-separated dirs under --root that the seal covers")
    parser.add_argument("--include-files", default="SPEC.md",
                        help="comma-separated individual files under --root")
    parser.add_argument("--exclude-suffixes",
                        default=",".join(DEFAULT_EXCLUDE_SUFFIXES),
                        help="comma-separated file suffixes the sealer skipped")
    parser.add_argument("--exclude-path-parts",
                        default=",".join(DEFAULT_EXCLUDE_PATH_PARTS),
                        help="comma-separated path components the sealer skipped")
    parser.add_argument("--exclude-basenames",
                        default=",".join(DEFAULT_EXCLUDE_BASENAMES),
                        help="comma-separated file names the sealer skipped")
    parser.add_argument("--expect-absent", default="",
                        help="comma-separated fnmatch patterns for recorded files "
                             "that are expected NOT to exist in this perimeter. "
                             "They are reported as unverified, never as verified, "
                             "and are not read. Note `*` crosses `/`.")
    # --pin-fingerprint is the name WP-2.1 specifies; --key-fingerprint is
    # the spelling the existing v1.2 CI invocation already uses. Same dest, so
    # either works and neither breaks the other.
    parser.add_argument("--pin-fingerprint", "--key-fingerprint",
                        dest="key_fingerprint", default=None,
                        help="pin the expected signer (first 16 hex chars of "
                             "sha256 over the raw public key). Without it, "
                             "layer 3 proves the manifest is self-signed but "
                             "not WHO signed it: a forgery re-signed under an "
                             "attacker's own key passes all three layers.")
    parser.add_argument("--public-key-hex", default=None,
                        help="pin the expected signer's raw Ed25519 public key")
    parser.add_argument("--json", action="store_true",
                        help="emit a machine-readable report instead of text")
    args = parser.parse_args(argv)

    report = verify(
        ledger=args.ledger,
        root=args.root,
        from_manifest=args.from_manifest,
        include_dirs=_split(args.include_dirs),
        include_files=_split(args.include_files),
        exclude_suffixes=_split(args.exclude_suffixes),
        exclude_path_parts=_split(args.exclude_path_parts),
        exclude_basenames=_split(args.exclude_basenames),
        expect_absent=_split(args.expect_absent),
        key_fingerprint=args.key_fingerprint,
        public_key_hex=args.public_key_hex,
    )

    if args.json:
        print(json.dumps({
            "ok": report.ok,
            "fatal": report.fatal,
            "context": report.context,
            "layers": [
                {"name": l.name, "title": l.title, "ok": l.ok,
                 "problems": l.problems, "detail": l.detail}
                for l in report.layers
            ],
        }, indent=2, sort_keys=True))
    else:
        print(render(report))
    return 0 if report.ok else 1


if __name__ == "__main__":
    sys.exit(main())
