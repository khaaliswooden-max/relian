"""WP-2.7 acceptance (4) — every IBM rule is sourced, and an unsourced one
raises at IMPORT.

D39 is R9 applied to a rule rather than to a number. The distinction this file
defends is between *checking at use* and *checking at import*. A citation-less
rule discovered at use has already been rendered into a report a customer is
holding; the same defect discovered at import stops the build. So the guard
lives in :class:`Rule.__post_init__`, profiles build their tables at module
scope, and the planted red below proves the raise happens on ``import`` rather
than on the first projection.
"""

from __future__ import annotations

import importlib
import sys
import textwrap
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.discovery.dialects.base import (                       # noqa: E402
    Citation,
    CitationError,
    DialectProfile,
    ProfileKind,
    Rule,
    RuleKind,
    assert_every_rule_is_provenanced,
)
from src.discovery.dialects import ibm_enterprise_cobol as ibm   # noqa: E402
from src.discovery.dialects import gnucobol_3_1_2 as gnu         # noqa: E402


# --------------------------------------------------------------------------
# The citation contract
# --------------------------------------------------------------------------

@pytest.mark.parametrize("blank", ["manual", "version", "section", "statement"])
def test_a_citation_missing_any_required_field_raises(blank: str) -> None:
    """Each field its own negative control: a lint that is only exercised on
    one of four fields is a lint with three untested branches."""
    kwargs = dict(
        manual="IBM Enterprise COBOL for z/OS Language Reference",
        version="6.3",
        section="USAGE clause",
        statement="binary items occupy 2 bytes for 1 through 4 digits",
    )
    kwargs[blank] = "   "
    with pytest.raises(CitationError, match=blank):
        Citation(**kwargs)


def test_a_sourced_rule_without_a_citation_raises() -> None:
    with pytest.raises(CitationError, match="SOURCED but carries no citation"):
        Rule(
            name="binary_width",
            kind=RuleKind.SOURCED,
            summary="1-4 digits -> 2 bytes",
            citation=None,
        )


def test_a_measured_rule_that_names_no_artifact_raises() -> None:
    """A measured rule's provenance is the artifact that measured it. "We
    measured it" with nothing named is the same defect wearing the other
    label."""
    with pytest.raises(CitationError, match="names nothing it was derived from"):
        Rule(
            name="binary_width",
            kind=RuleKind.MEASURED,
            summary="1-2 digits -> 1 byte",
            derived_from=None,
        )


# --------------------------------------------------------------------------
# Acceptance (4) — planted red, at import
# --------------------------------------------------------------------------

_CITATIONLESS_MODULE = textwrap.dedent(
    '''
    """A profile that declares a sourced rule it cannot source."""
    from src.discovery.dialects.base import Rule, RuleKind

    RULES = {
        "binary_width": Rule(
            name="binary_width",
            kind=RuleKind.SOURCED,
            summary="we believe IBM allocates a halfword here",
            citation=None,
        ),
    }
    '''
)


def test_a_citation_less_rule_raises_at_import_not_at_use(tmp_path: Path) -> None:
    """Acceptance (4)'s planted red.

    The module is imported, not called. Nothing in it is invoked and no
    projection is rendered — the failure has to happen while the module body
    executes, which is what "raises at import rather than at use" means.
    """
    pkg = tmp_path / "planted_red_pkg"
    pkg.mkdir()
    (pkg / "__init__.py").write_text("")
    (pkg / "unsourced_profile.py").write_text(_CITATIONLESS_MODULE)
    sys.path.insert(0, str(tmp_path))
    try:
        with pytest.raises(CitationError, match="SOURCED but carries no citation"):
            importlib.import_module("planted_red_pkg.unsourced_profile")
    finally:
        sys.path.remove(str(tmp_path))
        sys.modules.pop("planted_red_pkg.unsourced_profile", None)
        sys.modules.pop("planted_red_pkg", None)


def test_the_table_walk_catches_a_rule_that_lost_its_citation_later() -> None:
    """``Rule`` guards construction; this guards the finished table.

    A profile assembled by a loop or a ``replace`` could hold a rule that was
    valid when built and is not now. The assertion has to be about the object
    the report will actually read.
    """
    tampered = dict(ibm.PROFILE.rules)
    tampered["binary_width"] = object.__new__(Rule)   # bypasses __post_init__
    object.__setattr__(tampered["binary_width"], "name", "binary_width")
    object.__setattr__(tampered["binary_width"], "kind", RuleKind.SOURCED)
    object.__setattr__(tampered["binary_width"], "summary", "x")
    object.__setattr__(tampered["binary_width"], "citation", None)
    object.__setattr__(tampered["binary_width"], "derived_from", None)
    profile = DialectProfile(
        id="tampered", label="tampered", kind=ProfileKind.PROJECTED,
        binary=ibm.PROFILE.binary, packed=ibm.PROFILE.packed,
        display=ibm.PROFILE.display, sync_boundary=ibm.PROFILE.sync_boundary,
        rules=tampered,
    )
    with pytest.raises(CitationError, match="SOURCED with no citation"):
        assert_every_rule_is_provenanced(profile)


# --------------------------------------------------------------------------
# Acceptance (4) — the real table
# --------------------------------------------------------------------------

def test_the_ibm_profile_declares_at_least_the_rules_the_corpus_needs() -> None:
    """Nothing in the sealed corpus may be projected by an absent rule."""
    assert {
        "binary_width", "binary_synonyms", "comp5_width",
        "packed_width", "display_width", "sign_separate", "sync_alignment",
    } <= set(ibm.PROFILE.rules)


@pytest.mark.parametrize("name", sorted(ibm.PROFILE.rules))
def test_every_ibm_rule_carries_a_usable_citation(name: str) -> None:
    """Acceptance (4). Manual, section and the statement relied on — and the
    statement long enough to be a quotation rather than a gesture."""
    rule = ibm.PROFILE.rules[name]
    assert rule.kind is RuleKind.SOURCED, (
        f"{name} is {rule.kind.value}; every IBM rule is sourced from published "
        f"documentation or it is not written (D39)"
    )
    cite = rule.citation
    assert cite is not None
    assert cite.manual.strip() and cite.section.strip() and cite.version.strip()
    assert len(cite.statement.strip()) >= 40, (
        f"{name}'s citation statement is {len(cite.statement.strip())} chars. A "
        f"citation carries the statement RELIED ON, verbatim; a paraphrase has "
        f"already done the reasoning the reader is supposed to check."
    )
    assert cite.url and cite.url.startswith("https://"), (
        f"{name} has no retrievable URL; a reader cannot check the quote"
    )


def test_the_ibm_profile_is_projected_and_the_gnucobol_profile_is_measured() -> None:
    """D36's distinction is a field on the profile, not a convention."""
    assert ibm.PROFILE.projected and not ibm.PROFILE.measured
    assert gnu.PROFILE.measured and not gnu.PROFILE.projected
    assert ibm.PROFILE.basis is None, (
        "a projected profile has no measured basis; naming one would make the "
        "projection look like it rests on a measurement"
    )


def test_the_ibm_binary_table_is_the_one_the_citation_states() -> None:
    """The gap itself: 1-4 digits is a halfword on IBM, and 1-2 digits is a
    SINGLE byte on GnuCOBOL. Everything else in the table agrees."""
    assert ibm.PROFILE.binary(1).bytes_ == 2
    assert ibm.PROFILE.binary(2).bytes_ == 2
    assert ibm.PROFILE.binary(4).bytes_ == 2
    assert ibm.PROFILE.binary(5).bytes_ == 4
    assert ibm.PROFILE.binary(9).bytes_ == 4
    assert ibm.PROFILE.binary(10).bytes_ == 8
    assert ibm.PROFILE.binary(18).bytes_ == 8
    assert ibm.PROFILE.binary(19).bytes_ is None
    # And the divergence that is NOT a width: an 18-digit COMP is 8 bytes wide
    # but fullword-aligned, because IBM's slack rule keys m on the digit count.
    assert ibm.PROFILE.sync_boundary(18, 8) == 4
    assert gnu.PROFILE.sync_boundary(18, 8) == 8


def test_no_ibm_rule_exists_for_a_usage_the_oracle_never_measured() -> None:
    """R7/D39. ``COMP-1`` is documented by IBM, but the GnuCOBOL side is
    unmeasured, so a sourced IBM rule alone would be a difference against
    nothing. Those constructs classify UNKNOWN instead."""
    assert not any(
        "comp_1" in n or "comp1" in n or "float" in n
        for n in ibm.PROFILE.rules
    )
