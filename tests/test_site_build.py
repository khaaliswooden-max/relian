"""tools/build_site.py -- the gates that keep the published site honest.

Every check the builder makes is proven here by PLANTING the failure it exists
to catch and asserting the build refuses, then asserting the unmodified tree
builds green. A gate nobody has watched fail is a gate nobody should trust:
`README.md` carried four rows claiming deleted stages for two work packages
because nothing connected the prose to the tree, and this file is the thing
that connects them for the site.
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / "tools"))

import build_site as bs  # noqa: E402


@pytest.fixture()
def figures() -> dict:
    return bs.load_figures()


# --------------------------------------------------------------------------
# the tree as committed
# --------------------------------------------------------------------------


def test_the_committed_tree_builds(tmp_path: Path) -> None:
    assert bs.build(tmp_path) == 0
    assert (tmp_path / "index.html").is_file()
    assert (tmp_path / "summary.html").is_file()
    # .nojekyll: without it GitHub Pages runs Jekyll, which silently drops any
    # file or directory whose name begins with an underscore.
    assert (tmp_path / ".nojekyll").is_file()


def test_no_figure_disagrees_with_the_repository(figures: dict) -> None:
    assert bs.verify(figures) == []


def test_every_figure_carries_a_grade_and_a_basis(figures: dict) -> None:
    """R9. A number without a stated basis is a number nobody can audit."""
    allowed = {"VERIFIED", "PLAUSIBLE", "SPECULATIVE", "NOT MEASURED", "NO BASIS"}
    for name, spec in figures["figures"].items():
        assert spec.get("grade") in allowed, f"{name}: grade {spec.get('grade')!r}"
        assert spec.get("basis", "").strip(), f"{name}: empty basis"


def test_the_build_is_deterministic(tmp_path: Path) -> None:
    """R8. Same commit in, byte-identical site out -- no wall-clock read."""
    a, b = tmp_path / "a", tmp_path / "b"
    assert bs.build(a) == 0
    assert bs.build(b) == 0
    for name in ("index.html", "summary.html"):
        assert (a / name).read_bytes() == (b / name).read_bytes(), name


# --------------------------------------------------------------------------
# planted red: each gate, proven to fire
# --------------------------------------------------------------------------


def test_drift_gate_fires_when_a_derived_figure_goes_stale(tmp_path, monkeypatch, figures) -> None:
    """The gate this whole file exists for: re-seal the bench, forget the site."""
    poisoned = json.loads(json.dumps(figures))
    poisoned["figures"]["heldout_vectors"]["value"] = "426"
    monkeypatch.setattr(bs, "load_figures", lambda: poisoned)
    assert bs.build(tmp_path) == 1
    assert not (tmp_path / "index.html").exists()


def test_drift_gate_names_the_figure_and_both_values(figures) -> None:
    poisoned = json.loads(json.dumps(figures))
    poisoned["figures"]["suite_passed"]["value"] = "999"
    drift = bs.verify(poisoned)
    assert len(drift) == 1
    assert "suite_passed" in drift[0] and "999" in drift[0]
    assert figures["figures"]["suite_passed"]["value"] in drift[0]


def test_a_volatile_figure_is_stamped_rather_than_pinned(figures) -> None:
    """head_sha changes every commit; pinning it would make the gate cry wolf."""
    stale = json.loads(json.dumps(figures))
    stale["figures"]["head_sha"]["value"] = "0000000"
    assert bs.verify(stale) == []
    assert bs.resolve(stale)["head_sha"] != "0000000"


def test_a_pinned_figure_is_not_stamped(figures) -> None:
    """The converse control. Without it, a bug that stamped EVERY figure would
    pass the test above and silence the drift gate entirely."""
    assert bs.resolve(figures)["heldout_vectors"] == figures["figures"]["heldout_vectors"]["value"]


def test_orphan_gate_fires_when_a_figure_is_displayed_nowhere(tmp_path, monkeypatch, figures) -> None:
    poisoned = json.loads(json.dumps(figures))
    poisoned["figures"]["nobody_shows_this"] = {
        "value": "1", "grade": "VERIFIED", "basis": "planted by a test",
    }
    monkeypatch.setattr(bs, "load_figures", lambda: poisoned)
    assert bs.build(tmp_path) == 1


def test_unknown_placeholder_is_refused(figures) -> None:
    with pytest.raises(bs.BuildError) as excinfo:
        bs.render("<p>{{fig:no_such_figure}}</p>", figures)
    assert "no_such_figure" in str(excinfo.value)


def test_content_loss_check_catches_a_dropped_paragraph() -> None:
    """The guard that found a real renderer bug: a paragraph opening with
    **bold** looked like a bullet to the old paragraph rule and was dropped."""
    src = "A paragraph the renderer must not lose.\n"
    assert bs.verify_no_content_lost(src, "<p>A paragraph the renderer must not lose.</p>") == []
    assert "lose" in bs.verify_no_content_lost(src, "<p>A paragraph the renderer must not</p>")


def test_a_bold_opened_paragraph_survives_rendering() -> None:
    """The regression itself, pinned."""
    out = bs.markdown("**Companion to the Build Atlas.** Built from the ledgers.")
    assert "Companion" in out and "ledgers" in out
    assert out.startswith("<p>")


def test_position_naming_no_stop_is_refused(tmp_path, monkeypatch, figures) -> None:
    poisoned = json.loads(json.dumps(figures))
    poisoned["position"] = "s99"
    monkeypatch.setattr(bs, "load_figures", lambda: poisoned)
    assert bs.build(tmp_path) == 1


# --------------------------------------------------------------------------
# the position flag, stamped into the served markup
# --------------------------------------------------------------------------


def _stamped(tmp_path: Path, monkeypatch, figures: dict, position: str) -> str:
    moved = json.loads(json.dumps(figures))
    moved["position"] = position
    monkeypatch.setattr(bs, "load_figures", lambda: moved)
    assert bs.build(tmp_path) == 0
    return (tmp_path / "index.html").read_text(encoding="utf-8")


@pytest.mark.parametrize(
    "position,bands_done",
    [("s1", 0), ("s2", 1), ("s8", 3), ("s10", 4), ("s11", 4), ("s12", 5)],
)
def test_the_flag_stamps_the_served_markup(tmp_path, monkeypatch, figures, position, bands_done) -> None:
    """With scripting off, or before the script runs, the page must already be
    showing the right stop -- not stop 8 for a moment and then the right one."""
    html = _stamped(tmp_path, monkeypatch, figures, position)

    assert re.search(rf'id="rail" data-current="{position}"', html)

    states = re.findall(r'data-id="(s\d+)" data-state="(\w+)"', html)
    ids = [i for i, _ in states]
    idx = ids.index(position)
    for i, (_, state) in enumerate(states):
        expected = "done" if i < idx else ("current" if i == idx else "future")
        assert state == expected, f"{ids[i]} is {state}, expected {expected}"

    assert re.findall(r'data-target="(s\d+)" aria-current="step"', html) == [position]
    assert re.findall(r'data-for="(s\d+)" data-status="\w+"(?! hidden)', html) == [position]
    assert len(re.findall(r'<div class="band" data-done="true"', html)) == bands_done


def test_every_contents_link_resolves_to_a_heading(tmp_path) -> None:
    """The hand-written contents list broke silently the first time a section
    was retitled. Generated from the headings, it cannot -- and this is the
    assertion that keeps it generated."""
    assert bs.build(tmp_path) == 0
    page = (tmp_path / "summary.html").read_text(encoding="utf-8")
    anchors = re.findall(r'<a href="#([^"]+)">', page)
    ids = set(re.findall(r'<h\d id="([^"]+)"', page))
    assert anchors, "the summary has no contents links at all"
    assert not [a for a in anchors if a not in ids]


def test_a_summary_with_no_sections_is_refused() -> None:
    with pytest.raises(bs.BuildError):
        bs.contents("<p>no headings here</p>")


def test_the_band_list_is_read_from_the_script_not_duplicated() -> None:
    """The phase bands the builder fills in and the ones the script fills in
    come from the same BAND_END literal, so they cannot come to disagree."""
    tpl = (bs.SITE / "atlas.template.html").read_text(encoding="utf-8")
    assert re.search(r"BAND_END\s*=\s*\[[0-9,\s]+\]", tpl)
    with pytest.raises(bs.BuildError):
        bs.stamp_position(tpl.replace("BAND_END", "BAND_ENDS"), "s8")


# --------------------------------------------------------------------------
# what the published pages may say
# --------------------------------------------------------------------------


R11_VOCABULARY = ["solana", "on-chain", "ml risk", "machine learning", "predicted", "estimated"]


@pytest.mark.parametrize("term", R11_VOCABULARY)
def test_r11_vocabulary_is_absent_from_the_published_pages(tmp_path, term) -> None:
    """R11: the site may only reference capabilities in the quotable matrix."""
    assert bs.build(tmp_path) == 0
    for name in ("index.html", "summary.html"):
        body = (tmp_path / name).read_text(encoding="utf-8").lower()
        assert term not in body, f"{name} contains the R11 term {term!r}"


def test_the_r11_lint_would_actually_fire() -> None:
    """Converse control. A lint that never matches anything passes every case
    above and tells you nothing."""
    assert any(t in "this page mentions machine learning".lower() for t in R11_VOCABULARY)


def test_no_held_out_vector_reaches_the_published_site(tmp_path) -> None:
    """R3. The site is built from the ledgers, which name the held-out files
    and hash them -- it must never carry their contents."""
    assert bs.build(tmp_path) == 0
    for name in ("index.html", "summary.html"):
        body = (tmp_path / name).read_text(encoding="utf-8")
        assert "heldout.jsonl" not in body
        assert "relian-bench-private" not in body
