"""WP-2.2 §3.3 — the round-trip: engine output against the sealed oracle.

RELIAN-DISCOVERY-BENCH v0.1 was sealed on 2026-08-21, before ``src/discovery/``
existed. Its ``oracle.json`` is GnuCOBOL 3.1.2.0's own byte layout for fifteen
copybooks, measured by compiling probe programs. This file grades the static
engine against it: **186 comparisons, tolerance zero.**

Four properties make that number mean something.

1. **The seal is verified before a single row is trusted.** Not after, and not
   "the file is in git so it must be fine". All three layers of
   ``tools/verify_manifest.py`` run, the signer is pinned to fingerprint
   ``233bb4406e2de606``, and ``oracle.json``'s digest is checked against the
   signed manifest. A valid signature by the wrong key is the failure mode a
   bare verification cannot see.

2. **The engine and the oracle share no code and no process.** ``cobc`` is
   never invoked here or in ``src/discovery/``;
   ``tests/test_discovery_is_compiler_free.py`` walks both ASTs to assert it.
   The oracle is a committed artifact and re-deriving it inside the grading
   step would reintroduce the dependency through the back door (D15).

3. **The comparison count is asserted, not reported.** A harness that silently
   compares fewer rows than the oracle contains is green-by-skip in a new
   place, and this build has found that defect seven times.
   :data:`EXPECTED_COMPARISONS` is 186 and its composition is checked against
   the oracle's own per-copybook ``agreement.checks``.

4. **A mismatch names which artifact it accuses (D16).** 100% agreement is a
   claim about the *pair*, not about the engine alone. ``ACCUSES: engine``
   where the oracle row is probe- or listing-sourced and the seal verifies.
   ``ACCUSES: oracle (ESCALATE)`` where the disagreement is on a derived
   ``gap`` row, since gaps are computed by subtraction and the engine —
   reading the source — may legitimately know more (D18).

**An oracle accusation halts the work package.** It is not repaired by editing
``oracle.json``: rule 4 freezes ``discovery-bench/`` and the signature would
fail anyway. The remedy is a v0.2 re-seal, which is an operator key session.
Finding an oracle defect is a *successful* outcome of WP-2.2 — that is what
committing the benchmark before the solution buys.

**What 186 is made of** — the same composition the oracle uses for its own
Route A / Route B agreement (SPEC.md §2.1): 170 probe field rows, one
comparison each on ``offset`` and ``length`` together, plus one group-length
comparison per variant, 16 of them. Gap projections, level-88 conditions,
REDEFINES containment and the tiling invariant are asserted **in addition**
and counted separately, because the oracle does not count them in its own 186
either — folding them in would move the number for a reason that has nothing
to do with coverage.
"""

from __future__ import annotations

import hashlib
import json
import sys
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.discovery import LayoutStatus, compute                      # noqa: E402
from src.discovery.layout import (                                   # noqa: E402
    Layout,
    assert_redefines_containment,
    assert_renames_within_record,
    assert_tiling,
    lint_layout,
)
from tools.verify_manifest import verify                             # noqa: E402

BENCH = REPO_ROOT / "discovery-bench"
CORPUS = BENCH / "corpus"
ORACLE_PATH = BENCH / "oracle" / "oracle.json"
LEDGER_PATH = BENCH / "LEDGER_relian-discovery-bench-v0.1.json"

#: The published signer (SPEC.md §10, D13). "Any third party can re-derive the
#: ledger" is only true if the third party knows which key to expect.
PINNED_FINGERPRINT = "233bb4406e2de606"

#: The signed manifest hash, from the seal ceremony of 2026-08-21.
PINNED_MANIFEST_SHA256 = (
    "696397e0d4d865a368735404f8a0db52367ad01a66fcd3d4eb22add06a77f265"
)

#: 170 probe field rows + 16 variant group lengths. Asserted, not reported.
EXPECTED_COMPARISONS = 186

#: Oracle ``source`` values that are MEASUREMENTS. A disagreement with one of
#: these accuses the engine.
MEASURED_SOURCES = frozenset({"probe", "listing"})

#: Oracle ``source`` values that are DERIVED. A disagreement with one of these
#: is an escalation, not a bug report against the engine (D18).
DERIVED_SOURCES = frozenset({"gap", "derived"})


# ==========================================================================
# The trust gate — nothing below reads a row until this passes
# ==========================================================================

@lru_cache(maxsize=1)
def seal_report():
    """All three verifier layers, with the signer pinned."""
    return verify(
        LEDGER_PATH,
        from_manifest=True,
        key_fingerprint=PINNED_FINGERPRINT,
    )


@lru_cache(maxsize=1)
def trusted_oracle() -> Dict[str, object]:
    """The oracle document, returned ONLY if the seal verifies.

    Every consumer below goes through this function. There is deliberately no
    path that loads ``oracle.json`` without the seal check: an unverified
    answer key is not an answer key, and "we checked it in another test" is
    exactly how a green-by-skip gets in.
    """
    report = seal_report()
    if not report.ok:
        raise AssertionError(
            "the discovery-bench seal did NOT verify, so no oracle row may be "
            f"trusted. Failed layers: {report.failed_layers()}"
        )
    ledger = json.loads(LEDGER_PATH.read_text(encoding="utf-8"))
    fingerprint = ledger["signature"]["key_fingerprint"]
    if fingerprint != PINNED_FINGERPRINT:
        raise AssertionError(
            f"the ledger is signed by {fingerprint}, not the pinned "
            f"{PINNED_FINGERPRINT}"
        )
    digest = hashlib.sha256(ORACLE_PATH.read_bytes()).hexdigest()
    sealed = {entry["path"]: entry["sha256"] for entry in ledger["files"]}
    if sealed.get("oracle/oracle.json") != digest:
        raise AssertionError(
            "oracle/oracle.json does not match the digest inside the signed "
            f"manifest: on disk {digest}, sealed "
            f"{sealed.get('oracle/oracle.json')}"
        )
    return json.loads(ORACLE_PATH.read_text(encoding="utf-8"))


def _variants() -> List[Tuple[str, Dict[str, object], Dict[str, object]]]:
    out = []
    for copybook in trusted_oracle()["copybooks"]:            # type: ignore[index]
        for variant in copybook["variants"]:
            label = copybook["file"]
            if variant["odo_value"] is not None:
                label = f"{label}@odo={variant['odo_value']}"
            out.append((label, copybook, variant))
    return out


def _variant_ids() -> List[str]:
    """Stable parametrisation ids, computed WITHOUT trusting a row.

    Collection must not depend on the seal check passing — a seal failure
    should surface as a red test, not as a collection error that looks like an
    infrastructure problem.
    """
    try:
        return [label for label, _cb, _v in _variants()]
    except Exception:                       # pragma: no cover - seal failure path
        return ["seal-unverified"]


VARIANT_IDS = _variant_ids()


@lru_cache(maxsize=None)
def engine_layout(label: str) -> Layout:
    """The engine's layout for one oracle variant.

    The corpus file is read from ``discovery-bench/corpus/`` — the same bytes
    the seal covers, checked against the oracle's recorded sha256 by
    :func:`test_corpus_files_match_the_digests_recorded_in_the_oracle`.
    """
    for candidate, copybook, variant in _variants():
        if candidate == label:
            layout = compute(
                CORPUS / str(copybook["file"]), odo_value=variant["odo_value"]
            )
            assert layout is not None, f"the engine produced no record for {label}"
            return layout
    raise AssertionError(f"no oracle variant named {label}")


def _oracle_variant(label: str) -> Tuple[Dict[str, object], Dict[str, object]]:
    for candidate, copybook, variant in _variants():
        if candidate == label:
            return copybook, variant
    raise AssertionError(f"no oracle variant named {label}")


# ==========================================================================
# D16 — the accusation
# ==========================================================================

@dataclass(frozen=True)
class Comparison:
    """One graded comparison. ``kind`` is ``field`` or ``group_length``."""

    kind: str
    copybook: str
    group: str
    key: str
    oracle_offset: Optional[int]
    oracle_length: Optional[int]
    oracle_source: str
    engine_offset: Optional[int]
    engine_length: Optional[int]
    engine_source: str

    @property
    def ok(self) -> bool:
        return (
            self.oracle_offset == self.engine_offset
            and self.oracle_length == self.engine_length
        )


def accusation(
    *,
    copybook: str,
    group: str,
    key: str,
    oracle_line: str,
    engine_line: str,
    oracle_source: str,
    seal_ok: bool,
    extra: str = "",
) -> str:
    """Render the D16 block. The message names which artifact it accuses.

    Defaulting to "the engine is wrong" is the failure mode this replaces: it
    is a claim about the pair, and a harness that cannot express "the answer
    key is wrong" cannot discover an answer key that is.
    """
    if oracle_source in DERIVED_SOURCES:
        verdict = "oracle (ESCALATE)"
        reasoning = (
            f"the oracle row is {oracle_source!r}-sourced — DERIVED by "
            "subtraction, not measured. FILLER and SYNC slack cannot receive a "
            "MOVE, so the probe cannot see them and the oracle recovers them "
            "as the bytes no named field claims (SPEC.md §7). The engine reads "
            "the source and can distinguish explicit FILLER from implicit SYNC "
            "slack, so it may legitimately know more here (D18).\n"
            "  HALT: this work package stops. Do NOT edit oracle.json — "
            "CLAUDE.md rule 4 freezes discovery-bench/ and the Ed25519 "
            "signature over the manifest would fail regardless. The remedy is "
            "a v0.2 re-seal, which is an operator key session. Finding an "
            "oracle defect is a SUCCESSFUL outcome of WP-2.2."
        )
    elif seal_ok:
        verdict = "engine"
        reasoning = (
            f"the oracle row is {oracle_source!r}-sourced — MEASURED by "
            "GnuCOBOL 3.1.2.0 — and the seal verifies (manifest "
            f"{PINNED_MANIFEST_SHA256[:16]}…, key {PINNED_FINGERPRINT}). "
            "Fix src/discovery/, not the answer key."
        )
    else:
        verdict = "neither — the seal did not verify"
        reasoning = (
            "no row in this document may be trusted until "
            "tools/verify_manifest.py passes all three layers."
        )
    block = (
        f"MISMATCH  {copybook} / {group} / {key}\n"
        f"  oracle : {oracle_line}   (sealed, relian-discovery-bench-v0.1)\n"
        f"  engine : {engine_line}\n"
        f"  ACCUSES: {verdict} — {reasoning}"
    )
    return block + (f"\n  {extra}" if extra else "")


def _field_accusation(cmp: Comparison, seal_ok: bool) -> str:
    return accusation(
        copybook=cmp.copybook,
        group=cmp.group,
        key=cmp.key,
        oracle_line=(
            f"offset {cmp.oracle_offset}  length {cmp.oracle_length}  "
            f"source {cmp.oracle_source}"
        ),
        engine_line=(
            f"offset {cmp.engine_offset}  length {cmp.engine_length}  "
            f"source {cmp.engine_source}"
        ),
        oracle_source=cmp.oracle_source,
        seal_ok=seal_ok,
    )


# ==========================================================================
# Building the 186
# ==========================================================================

def comparisons_for(label: str) -> List[Comparison]:
    """Every graded comparison for one variant: its field rows and its group
    length. Nothing is skipped, and a row the engine failed to produce becomes
    a comparison with ``None`` on the engine side rather than vanishing."""
    copybook, variant = _oracle_variant(label)
    layout = engine_layout(label)
    engine_rows = layout.by_key()
    out: List[Comparison] = []

    for row in variant["fields"]:                                # type: ignore[index]
        field = engine_rows.get(row["key"])
        out.append(
            Comparison(
                kind="field",
                copybook=str(copybook["file"]),
                group=str(copybook["group"]),
                key=str(row["key"]),
                oracle_offset=row["offset"],
                oracle_length=row["length"],
                oracle_source=str(row["source"]),
                engine_offset=field.offset if field else None,
                engine_length=field.length if field else None,
                engine_source=(field.source if field else "ABSENT — the engine "
                                                          "emitted no such row"),
            )
        )

    out.append(
        Comparison(
            kind="group_length",
            copybook=str(copybook["file"]),
            group=str(copybook["group"]),
            key=f"{copybook['group']} (group length)",
            oracle_offset=1,
            oracle_length=variant["group_length"],
            oracle_source=str(variant["listing"]["source"]),      # type: ignore[index]
            engine_offset=1,
            engine_length=layout.group_length,
            engine_source="computed",
        )
    )
    return out


@lru_cache(maxsize=1)
def all_comparisons() -> Tuple[Comparison, ...]:
    out: List[Comparison] = []
    for label in VARIANT_IDS:
        out.extend(comparisons_for(label))
    return tuple(out)


# ==========================================================================
# (4) the seal, verified before any row is trusted
# ==========================================================================

def test_oracle_seal_verifies_all_three_layers() -> None:
    report = seal_report()
    assert report.ok, (
        "the discovery-bench seal did not verify; no row below means anything.\n"
        f"failed layers: {report.failed_layers()}"
    )
    assert [layer.name for layer in report.layers] == ["tree", "payload", "signature"]


def test_oracle_signer_is_the_pinned_fingerprint() -> None:
    """A valid signature by the WRONG key is the failure a bare verify cannot
    see: the key it trusts is the one the document supplied."""
    ledger = json.loads(LEDGER_PATH.read_text(encoding="utf-8"))
    assert ledger["signature"]["key_fingerprint"] == PINNED_FINGERPRINT
    embedded = hashlib.sha256(
        bytes.fromhex(ledger["signature"]["public_key_hex"])
    ).hexdigest()[:16]
    assert embedded == PINNED_FINGERPRINT


def test_oracle_manifest_hash_is_the_published_one() -> None:
    ledger = json.loads(LEDGER_PATH.read_text(encoding="utf-8"))
    assert ledger["signature"]["manifest_sha256"] == PINNED_MANIFEST_SHA256


def test_oracle_json_matches_the_digest_inside_the_signed_manifest() -> None:
    """The signature covers the manifest; the manifest covers oracle.json's
    digest. Without this step the chain has a gap exactly where the answers
    are."""
    trusted_oracle()          # raises with the reason if the chain is broken
    ledger = json.loads(LEDGER_PATH.read_text(encoding="utf-8"))
    sealed = {entry["path"]: entry["sha256"] for entry in ledger["files"]}
    assert sealed["oracle/oracle.json"] == hashlib.sha256(
        ORACLE_PATH.read_bytes()
    ).hexdigest()


def test_corpus_files_match_the_digests_recorded_in_the_oracle() -> None:
    """The engine reads the same bytes the oracle measured. If the corpus on
    disk had drifted, a green round-trip would be about two different
    corpora."""
    oracle = trusted_oracle()
    mismatched = []
    for copybook in oracle["copybooks"]:                          # type: ignore[index]
        path = CORPUS / str(copybook["file"])
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        if digest != copybook["sha256"]:
            mismatched.append(f"{copybook['file']}: on disk {digest}, oracle {copybook['sha256']}")
    assert not mismatched, "\n".join(mismatched)


# ==========================================================================
# (2) the count, asserted rather than reported
# ==========================================================================

def test_comparison_count_is_exactly_186() -> None:
    total = len(all_comparisons())
    assert total == EXPECTED_COMPARISONS, (
        f"{total} comparisons were built, not {EXPECTED_COMPARISONS}. A harness "
        "that silently compares fewer rows than the oracle contains is "
        "green-by-skip. Find the rows being skipped BEFORE looking at any pass "
        "rate (WP-2.2 §5)."
    )


def test_comparison_composition_matches_the_oracles_own_agreement_counts() -> None:
    """186 is not a magic number: it is the oracle's own Route A / Route B
    surface, per copybook. Checking the composition rather than the total means
    two offsetting errors cannot cancel."""
    oracle = trusted_oracle()
    per_copybook: Dict[str, int] = {}
    for cmp in all_comparisons():
        per_copybook[cmp.copybook] = per_copybook.get(cmp.copybook, 0) + 1
    expected = {
        name: block["checks"]
        for name, block in oracle["agreement"]["per_copybook"].items()   # type: ignore[index]
    }
    assert per_copybook == expected


def test_probe_row_and_variant_counts_match_the_sealed_corpus_counts() -> None:
    oracle = trusted_oracle()
    counts = oracle["counts"]                                     # type: ignore[index]
    field_cmps = [c for c in all_comparisons() if c.kind == "field"]
    group_cmps = [c for c in all_comparisons() if c.kind == "group_length"]
    assert len(field_cmps) == counts["probe_rows"] == 170
    assert len(group_cmps) == counts["variants"] == 16


# ==========================================================================
# (2) the comparison itself — tolerance zero
# ==========================================================================

@pytest.mark.parametrize("label", VARIANT_IDS)
def test_variant_rows_agree_with_the_oracle(label: str) -> None:
    """Field rows: ``offset`` and ``length`` exact, tolerance zero."""
    seal_ok = seal_report().ok
    failures = [
        _field_accusation(cmp, seal_ok)
        for cmp in comparisons_for(label)
        if not cmp.ok
    ]
    assert not failures, "\n\n".join(failures)


@pytest.mark.parametrize("label", VARIANT_IDS)
def test_engine_emits_no_row_the_oracle_does_not_contain(label: str) -> None:
    """The converse of the count assertion. An engine that invented rows would
    pass every comparison above while describing a different record."""
    _copybook, variant = _oracle_variant(label)
    oracle_keys = {row["key"] for row in variant["fields"]}       # type: ignore[index]
    engine_keys = set(engine_layout(label).by_key())
    assert engine_keys == oracle_keys, (
        f"{label}\n  only in engine: {sorted(engine_keys - oracle_keys)}\n"
        f"  only in oracle: {sorted(oracle_keys - engine_keys)}"
    )


@pytest.mark.parametrize("label", VARIANT_IDS)
def test_gap_rows_project_to_the_oracles_gap_spans(label: str) -> None:
    """D18 — compare by PROJECTION, not by label.

    The oracle can only observe "bytes no named field claims" and calls them
    ``gap``. The engine emits the finer ``filler``/``slack``. Comparing labels
    directly would fail on a difference that is extra information rather than
    disagreement, so the assertion is that ``filler ∪ slack`` equals ``gap`` as
    a set of byte ranges.
    """
    _copybook, variant = _oracle_variant(label)
    layout = engine_layout(label)
    oracle_spans = tuple(sorted(
        (g["offset"], g["offset"] + g["length"] - 1)
        for g in variant["gaps"]                                  # type: ignore[index]
    ))
    engine_spans = layout.gap_spans()
    if oracle_spans != engine_spans:
        sources = {g.source for g in layout.gaps} or {"none"}
        pytest.fail(accusation(
            copybook=label,
            group=layout.group,
            key="gap projection (filler ∪ slack)",
            oracle_line=f"spans {list(oracle_spans)}  source gap",
            engine_line=f"spans {list(engine_spans)}  source {sorted(sources)}",
            oracle_source="gap",
            seal_ok=seal_report().ok,
            extra="engine gap detail: " + "; ".join(
                f"{g.offset}+{g.length} {g.source} ({g.cause})" for g in layout.gaps
            ),
        ))


@pytest.mark.parametrize("label", VARIANT_IDS)
def test_conditions_agree_name_for_name_with_both_sides_null(label: str) -> None:
    """Level-88s: name for name, and offset/length null on BOTH sides.

    A zero here would be a fabricated measurement for a thing with no extent
    (R1, U7), and it would sail through any check that compared types.
    """
    _copybook, variant = _oracle_variant(label)
    layout = engine_layout(label)
    oracle_names = sorted(c["name"] for c in variant["conditions"])   # type: ignore[index]
    engine_names = sorted(c.name for c in layout.conditions)
    assert engine_names == oracle_names, f"{label}: {engine_names} != {oracle_names}"

    for cond in variant["conditions"]:                            # type: ignore[index]
        assert cond["offset"] is None and cond["length"] is None, cond
    for cond in layout.conditions:
        assert cond.offset is None and cond.length is None, cond

    oracle_hosts = {c["name"]: c["host"] for c in variant["conditions"]}  # type: ignore[index]
    for cond in layout.conditions:
        assert cond.host == oracle_hosts[cond.name], (
            f"{label}: {cond.name} hosted by {cond.host}, oracle says "
            f"{oracle_hosts[cond.name]}"
        )


def test_odo_is_compared_at_both_min_and_max_extents() -> None:
    """U8 — the ODO copybook is measured twice and BOTH lengths are graded.

    Comparing one extent and calling the axis covered is the same green-by-skip
    the count assertion exists to catch, one level down.
    """
    odo_labels = [label for label in VARIANT_IDS if "odo=" in label]
    assert sorted(odo_labels) == ["D07_odo.cpy@odo=1", "D07_odo.cpy@odo=4"]
    lengths = {label: engine_layout(label).group_length for label in odo_labels}
    assert lengths == {"D07_odo.cpy@odo=1": 17, "D07_odo.cpy@odo=4": 50}
    for label in odo_labels:
        layout = engine_layout(label)
        assert layout.variable_length is True
        assert layout.odo_object == "D07-COUNT"


# ==========================================================================
# Engine-side invariants, asserted INDEPENDENTLY of the oracle
# ==========================================================================

@pytest.mark.parametrize("label", VARIANT_IDS)
def test_redefines_containment_holds_in_engine_output(label: str) -> None:
    """§3.3 — asserted in engine output independently of the oracle, so a
    round-trip that agreed with a wrong answer key would still go red here."""
    problems = assert_redefines_containment(engine_layout(label))
    assert not problems, f"{label}\n  " + "\n  ".join(problems)


@pytest.mark.parametrize("label", VARIANT_IDS)
def test_renames_spans_start_where_they_rename_and_stay_in_the_record(label: str) -> None:
    """Deliberately NOT containment: ``RENAMES A THRU B`` is longer than ``A``,
    which is the whole point of the clause (SPEC.md §6)."""
    problems = assert_renames_within_record(engine_layout(label))
    assert not problems, f"{label}\n  " + "\n  ".join(problems)


@pytest.mark.parametrize("label", VARIANT_IDS)
def test_tiling_invariant_holds_over_the_conditioned_projection(label: str) -> None:
    """SPEC.md §6 / D9 — named, elementary, non-redefining, non-renaming
    fields plus gap spans cover ``[1, group_length]`` exactly once."""
    problems = assert_tiling(engine_layout(label))
    assert not problems, f"{label}\n  " + "\n  ".join(problems)


@pytest.mark.parametrize("label", VARIANT_IDS)
def test_engine_output_passes_the_no_fabricated_zero_lint(label: str) -> None:
    """Acceptance ⑦ / D19 — no engine field carries ``0`` where nothing was
    computed."""
    layout = engine_layout(label)
    assert lint_layout(layout) == []
    assert layout.status is LayoutStatus.COMPLETE, (
        f"{label}: status {layout.status.value} — {layout.reasons}"
    )


def test_renames_rows_use_the_shape_the_seal_committed_to() -> None:
    """A level-66 renames a SPAN; it has no offset of its own.

    The sealed rows express that span as ``offset`` = the renamed start item's
    offset, ``length`` = the span, ``redefines`` = the start item's name and
    ``renames: true``. That shape CAN express a span, so the engine emits it
    rather than inventing a second one and reconciling later.
    """
    _copybook, variant = _oracle_variant("D11_renames.cpy")
    oracle_66 = {r["key"]: r for r in variant["fields"] if r["renames"]}  # type: ignore[index]
    assert set(oracle_66) == {"D11-JUST-GAMMA", "D11-ALPHA-BETA", "D11-BETA-GAMMA"}

    engine = engine_layout("D11_renames.cpy").by_key()
    for key, row in oracle_66.items():
        field = engine[key]
        assert field.renames is True
        assert field.level == 66
        assert field.redefines == row["redefines"], (
            f"{key}: the seal names the renamed start item as "
            f"{row['redefines']!r}; the engine says {field.redefines!r}"
        )
        assert field.in_tiling is False
        assert (field.offset, field.length) == (row["offset"], row["length"])

    # RENAMES ... THRU is longer than what it starts at. If it were not, this
    # axis would be indistinguishable from REDEFINES.
    assert engine["D11-ALPHA-BETA"].length == 7 > engine["D11-ALPHA"].length


def test_sync_slack_is_labelled_more_finely_than_the_oracle_can(label: str = "D10_sync.cpy") -> None:
    """The customer-facing half of D18.

    "Bytes 2, 10 and 16 are SYNC slack, not data" is a different sentence from
    "there is a gap", and only one of them helps someone writing a target
    schema. The oracle cannot say the first; the engine can.
    """
    layout = engine_layout(label)
    assert [g.source for g in layout.gaps] == ["slack", "slack", "slack"]
    assert [g.span() for g in layout.gaps] == [(2, 4), (10, 10), (16, 16)]
    for gap in layout.gaps:
        assert "SYNCHRONIZED" in gap.cause


def test_filler_gaps_are_labelled_filler_not_slack() -> None:
    layout = engine_layout("MUBBREC.cpy")
    assert [g.source for g in layout.gaps] == ["filler", "filler", "filler"]
    assert [g.span() for g in layout.gaps] == [(11, 12), (44, 45), (58, 59)]


# ==========================================================================
# (3) the accusation path, proven
# ==========================================================================

def test_accusation_names_the_engine_on_a_probe_sourced_row() -> None:
    """A planted mismatch on a measured row. This is the D16 example verbatim."""
    planted = Comparison(
        kind="field", copybook="D10_sync.cpy", group="D10-SYNC", key="D10-BIN-A",
        oracle_offset=3, oracle_length=4, oracle_source="probe",
        engine_offset=2, engine_length=4, engine_source="computed",
    )
    assert not planted.ok
    block = _field_accusation(planted, seal_ok=True)
    assert "MISMATCH  D10_sync.cpy / D10-SYNC / D10-BIN-A" in block
    assert "oracle : offset 3  length 4  source probe" in block
    assert "engine : offset 2  length 4  source computed" in block
    assert "ACCUSES: engine" in block
    assert "the seal verifies" in block
    assert "ESCALATE" not in block


def test_accusation_names_the_oracle_and_escalates_on_a_derived_gap_row() -> None:
    """A planted mismatch on a DERIVED row. The engine may legitimately know
    more, so the harness must be able to say so — a gate that can only accuse
    the engine can never discover an oracle defect."""
    block = accusation(
        copybook="D10_sync.cpy", group="D10-SYNC", key="gap projection (filler ∪ slack)",
        oracle_line="spans [(2, 4)]  source gap",
        engine_line="spans [(2, 3)]  source ['slack']",
        oracle_source="gap", seal_ok=True,
    )
    assert "ACCUSES: oracle (ESCALATE)" in block
    assert "DERIVED by subtraction" in block
    assert "HALT" in block
    assert "Do NOT edit oracle.json" in block
    assert "v0.2 re-seal" in block


def test_accusation_blames_neither_artifact_when_the_seal_did_not_verify() -> None:
    """An unverified answer key is not an answer key. Accusing the engine on
    the strength of a document whose signature failed would be worse than
    reporting nothing."""
    block = accusation(
        copybook="X.cpy", group="X", key="X-A",
        oracle_line="offset 1  length 2  source probe",
        engine_line="offset 1  length 3  source computed",
        oracle_source="probe", seal_ok=False,
    )
    assert "ACCUSES: neither — the seal did not verify" in block


def test_a_missing_engine_row_becomes_a_comparison_rather_than_vanishing() -> None:
    """The green-by-skip guard, at row granularity. A row the engine failed to
    emit must FAIL a comparison, not silently reduce the count."""
    planted = Comparison(
        kind="field", copybook="D01_display.cpy", group="D01-DISPLAY", key="D01-GHOST",
        oracle_offset=1, oracle_length=1, oracle_source="probe",
        engine_offset=None, engine_length=None,
        engine_source="ABSENT — the engine emitted no such row",
    )
    assert not planted.ok
    assert "ABSENT" in _field_accusation(planted, seal_ok=True)


# ==========================================================================
# The headline
# ==========================================================================

def test_all_186_comparisons_pass_with_zero_tolerance() -> None:
    """Acceptance ② in one assertion, with the count checked in the same breath."""
    comparisons = all_comparisons()
    assert len(comparisons) == EXPECTED_COMPARISONS
    seal_ok = seal_report().ok
    failures = [_field_accusation(c, seal_ok) for c in comparisons if not c.ok]
    passed = len(comparisons) - len(failures)
    assert not failures, (
        f"{passed} of {len(comparisons)} comparisons passed.\n\n"
        + "\n\n".join(failures)
    )
    assert passed == EXPECTED_COMPARISONS
