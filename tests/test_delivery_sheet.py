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


def test_the_sheet_grades_the_release_fingerprint_honestly(sheet: str) -> None:
    """R4/R9. Nothing in this repository has computed the release fingerprint
    from a key, because the key never enters it. Saying VERIFIED would be a
    grade with nothing behind it."""
    row = next(l for l in sheet.splitlines() if RELEASE_KEY_FINGERPRINT in l and "|" in l
               and "PLAUSIBLE" in l)
    assert "never enters this repository" in row


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
