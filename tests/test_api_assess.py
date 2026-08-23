"""The assessment endpoint — what the Assess tab is actually served.

The endpoint mints no figures of its own. These tests pin that: the bundle is
the engine's, the plain-language layer is the report writer's, and the report
hash is over the bundle alone, so adding section 0 to the response cannot move
it.
"""

from pathlib import Path

import pytest

fastapi = pytest.importorskip("fastapi", reason="the API extra is not installed")
pytest.importorskip("httpx", reason="fastapi's TestClient needs httpx")

from fastapi.testclient import TestClient  # noqa: E402

from src.api.main import app  # noqa: E402

REPO_ROOT = Path(__file__).resolve().parents[1]
DEMO = REPO_ROOT / "examples" / "demo"


@pytest.fixture(scope="module")
def payload():
    with TestClient(app) as client:
        response = client.get("/api/v1/assess/demo")
    assert response.status_code == 200, response.text
    return response.json()


def test_the_endpoint_returns_the_plain_language_layer(payload):
    assert "plain_summary" in payload, "the Assess tab has nothing to render"
    assert payload["plain_summary"]["title"]


def test_the_plain_layer_is_the_report_writers_own(payload):
    """Not a second implementation — byte-for-byte what the CLI renders from."""
    from src.assessment.cli import assess_tree
    from src.assessment.report import plain_summary

    bundle, by_construct = assess_tree(DEMO)
    expected = plain_summary(bundle, root_label="examples/demo",
                             scope_by_construct=by_construct)
    assert payload["plain_summary"] == expected


def test_section_zero_does_not_enter_the_hashed_bundle(payload):
    """The report hash is over measurements; prose must not perturb it."""
    import hashlib

    from src.assessment.models import canonical_json

    recomputed = hashlib.sha256(
        canonical_json(payload["bundle"]).encode("utf-8")
    ).hexdigest()
    assert recomputed == payload["report_hash"]
    assert "plain_summary" not in payload["bundle"]


def test_every_number_in_the_plain_layer_carries_its_evidence(payload):
    """R9 across the wire, not just in the report."""
    for row in payload["plain_summary"]["how_much"]["rows"]:
        measured = row["measured"]
        if measured is None:
            continue
        assert measured["grade"] in ("VERIFIED", "PLAUSIBLE", "SPECULATIVE")
        assert measured["provenance"].strip()


def test_tier_counts_agree_with_the_bundle_they_summarise(payload):
    """The layer restates the bundle; a disagreement is a bug in the layer."""
    from collections import Counter

    tiers = Counter(p["risk"]["tier"] for p in payload["bundle"]["programs"])
    groups = {g["tier"]: g["programs"]
              for g in payload["plain_summary"]["where_we_stand"]["groups"]}
    assert groups == {t: c for t, c in tiers.items() if c}


def test_program_ids_in_the_layer_exist_in_the_bundle(payload):
    known = {p["program_id"] for p in payload["bundle"]["programs"]}
    for group in payload["plain_summary"]["where_we_stand"]["groups"]:
        for program_id in group["program_ids"]:
            assert program_id in known


def test_construct_counts_agree_with_the_portfolio_inventory(payload):
    ranked = {r["verb"]: r["count"]
              for r in payload["bundle"]["portfolio_coverage"]["unsupported_ranked"]}
    for item in payload["plain_summary"]["in_the_way"]["constructs"]:
        assert ranked[item["construct"]] == item["count"]


def test_the_endpoint_states_no_price_or_duration(payload):
    """R11 holds on this surface too, not only in the rendered report."""
    import json
    import re

    text = json.dumps(payload["plain_summary"]).lower()
    assert not re.search(r"[$€£]\s*\d", text)
    for phrase in ("fixed price", "engagement fee", "delivery date", "guaranteed"):
        assert phrase not in text
