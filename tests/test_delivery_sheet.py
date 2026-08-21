"""WP-2.3 D13/D23 — the Technical Delivery Sheet is pinned to what it publishes.

A digest published in a document that can go stale in silence is a digest
nobody should rely on. WP-2.2 established the pattern for this: the "186 of
186" in the published limitation is read back out of the round-trip harness, so
changing one without the other goes red. The same applies here, harder, because
this sheet is what a recipient checks a shipped tool against.

Four things are pinned:

* each tool's own SHA-256, as printed in the sheet, against the file on disk;
* both signing fingerprints, against the constants the code actually uses;
* the exit codes, against the verifier's own documented behaviour;
* the R11 disclaimers, so the sheet cannot drift into claiming something the
  quotable-capability matrix does not support.
"""

from __future__ import annotations

import hashlib
import re
from pathlib import Path

import pytest

from src.discovery.report import FORBIDDEN_VOCABULARY, lint_report_text
from src.discovery.signing import BENCH_KEY_FINGERPRINT, RELEASE_KEY_FINGERPRINT

REPO_ROOT = Path(__file__).resolve().parents[1]
SHEET = REPO_ROOT / "docs" / "TECHNICAL_DELIVERY_SHEET.md"

PINNED_TOOLS = (
    "tools/verify_report.py",
    "tools/countersign.py",
    "tools/verify_manifest.py",
)


@pytest.fixture(scope="module")
def sheet() -> str:
    assert SHEET.is_file(), f"{SHEET} is missing — D13 is not closed"
    return SHEET.read_text(encoding="utf-8")


@pytest.mark.parametrize("tool", PINNED_TOOLS)
def test_the_sheet_publishes_the_tools_actual_sha256(sheet: str, tool: str) -> None:
    actual = hashlib.sha256((REPO_ROOT / tool).read_bytes()).hexdigest()
    assert actual in sheet, (
        f"{tool} hashes to {actual}, which the Technical Delivery Sheet does "
        f"not publish. A customer comparing the digest the tool prints against "
        f"the sheet would find a mismatch and rightly stop. Update the sheet."
    )


def test_the_sheet_publishes_both_fingerprints_with_what_each_signs(sheet: str) -> None:
    """D13. Both, each labelled with what it signs and what it does not."""
    assert BENCH_KEY_FINGERPRINT in sheet
    assert RELEASE_KEY_FINGERPRINT in sheet
    assert "customer deliverables of any kind" in sheet
    assert "benchmark seals" in sheet


def test_the_sheet_grades_the_release_fingerprint_as_derived(sheet: str) -> None:
    """R9. Upgraded from PLAUSIBLE to VERIFIED on 2026-08-21, when the operator
    derived the value from the public key rather than transcribing it from a
    design note. The row must carry the METHOD and the DATE, not just the word:
    a grade whose basis is not stated is a grade with the units filed off."""
    row = next(l for l in sheet.splitlines()
               if RELEASE_KEY_FINGERPRINT in l and "|" in l and "VERIFIED" in l)
    assert "2026-08-21" in row, "the row does not say when it was derived"
    assert "visionblox-release-key-v1.pub" in row, "the row does not say from what"
    assert "SubjectPublicKeyInfo" in row and "Ed25519" in row
    assert "32 bytes" in row and "SHA-256" in row and "16 hexadecimal" in row
    assert "Public material only" in row


def test_the_sheet_still_says_the_private_key_never_enters_this_repository(
    sheet: str,
) -> None:
    """R4 did not change when the grade did. VERIFIED here means the value was
    computed from the PUBLIC key, not that this repository holds the key."""
    assert "never enters this repository, CI, or an agent" in sheet
    assert "It does not mean this repository holds the key" in sheet


def test_the_sheet_publishes_a_recipe_the_recipient_can_run(sheet: str) -> None:
    assert "How to re-derive" in sheet
    assert "load_pem_public_key" in sheet
    assert "fingerprint_from_public_pem" in sheet


def test_the_published_recipe_computes_what_the_tool_computes() -> None:
    """The sheet prints six lines and tells a customer they will reproduce the
    fingerprint. Run those six lines. A published recipe nobody has executed is
    a published recipe that can be wrong, and the customer is the one who finds
    out."""
    import hashlib as _hashlib
    import sys

    from cryptography.hazmat.primitives import serialization
    from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey

    sys.path.insert(0, str(REPO_ROOT / "tools"))
    try:
        import countersign
    finally:
        sys.path.remove(str(REPO_ROOT / "tools"))

    for _ in range(5):
        pem = Ed25519PrivateKey.generate().public_key().public_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PublicFormat.SubjectPublicKeyInfo,
        )
        # --- verbatim from the sheet ---
        raw = serialization.load_pem_public_key(pem).public_bytes(
            encoding=serialization.Encoding.Raw,
            format=serialization.PublicFormat.Raw,
        )
        assert len(raw) == 32
        recipe = _hashlib.sha256(raw).hexdigest()[:16]
        # --- end ---
        assert recipe == countersign.fingerprint_from_public_pem(pem)


def test_the_sheet_records_the_release_key_path_and_its_passphrase_custody(
    sheet: str,
) -> None:
    assert "~/zil-keys/visionblox-release-key-v1.pem" in sheet
    assert "never accepted on the command line" in sheet
    assert "never read from the environment" in sheet


def test_the_sheet_states_every_verifier_exit_code(sheet: str) -> None:
    source = (REPO_ROOT / "tools" / "verify_report.py").read_text(encoding="utf-8")
    for code, phrase in ((0, "VALID AND ATTESTED"), (3, "VALID AND UNATTESTED")):
        assert phrase in sheet, f"the sheet does not state the exit-{code} wording"
        assert phrase in source or phrase.replace("VALID AND ", "") in source
    assert re.search(r"\|\s*3\s*\|", sheet), "exit code 3 is not tabulated"


def test_the_sheet_says_the_pin_is_required(sheet: str) -> None:
    assert "required and has no default" in sheet


def test_the_sheet_does_not_let_exit_3_read_as_a_pass(sheet: str) -> None:
    assert "Exit 3 is not a warning to be ignored" in sheet
    assert "not** attested to it" in sheet or "not attested to it" in sheet


def test_the_sheet_carries_no_forbidden_vocabulary(sheet: str) -> None:
    """R11. The sheet is a customer-facing surface and is linted like one."""
    found = lint_report_text(sheet)
    assert found == [], f"the delivery sheet uses vocabulary R11 forbids: {found}"


@pytest.mark.parametrize("term", FORBIDDEN_VOCABULARY)
def test_the_sheet_lint_would_catch_each_term(sheet: str, term: str) -> None:
    """Negative control per term, on this surface too: a lint that has never
    fired on the document it guards is a lint nobody has tested."""
    assert lint_report_text(sheet + f"\nRelian uses {term} here.\n")


def test_the_sheet_repeats_what_relian_does_not_claim(sheet: str) -> None:
    assert "No risk scoring of any kind" in sheet
    assert "No claim of equivalence with IBM Enterprise COBOL" in sheet
    assert "Ed25519 hash chain" in sheet


# --------------------------------------------------------------------------
# Every LIVE document that publishes a tool digest is pinned, not just the sheet
# --------------------------------------------------------------------------

DRYRUN_NOTE = REPO_ROOT / "docs" / "dryruns" / "REPORT_DRYRUNS.md"


@pytest.mark.parametrize("tool", ["tools/verify_report.py", "tools/countersign.py"])
def test_the_dryrun_note_publishes_the_tools_actual_sha256(tool: str) -> None:
    """WP-2.3.2. The delivery sheet was pinned and the dry-run note was not, so
    a tool change would have turned one red and left the other quietly wrong —
    which is the failure the sheet's pinning exists to prevent, one file over.

    `docs/PHASE2_LOG.md` is deliberately NOT covered here: it is append-only,
    and its entries record what was measured at the time rather than what is
    true now. A log edited to stay current is not a log.
    """
    actual = hashlib.sha256((REPO_ROOT / tool).read_bytes()).hexdigest()
    assert actual in DRYRUN_NOTE.read_text(encoding="utf-8"), (
        f"{tool} hashes to {actual}, which docs/dryruns/REPORT_DRYRUNS.md does "
        f"not publish"
    )
