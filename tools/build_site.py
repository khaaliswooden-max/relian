#!/usr/bin/env python3
"""Build the Relian GitHub Pages site from repository facts.

Why this exists rather than a hand-maintained page
--------------------------------------------------
A published page that states a number is a claim, and a claim that can go
stale in silence is a claim nobody should rely on. ``README.md`` demonstrated
exactly that failure on 2026-08-22: four capability rows still advertised
stages that had been deleted two work packages earlier, because nothing
connected the prose to the tree.

So every figure the site displays lives in ``site/figures.json`` with a grade
and a basis (R9), and every figure that CAN be recomputed from this repository
carries a ``derive`` key naming the derivation. The build recomputes each one
and **fails** if the declared value and the repository disagree. Advancing the
build then has exactly one failure mode left -- forgetting to update
``figures.json`` -- and that mode is red, not silent.

What is derived, and what is merely declared
--------------------------------------------
Derived (recomputed here, every build):
    the two sealed ledgers -- versions, tags, digests, fingerprints, vector
    counts, thresholds, the pre-implementation baselines, the discovery corpus
    counts; the CI gate's own pinned pass/skip counts; the commit under build.

Declared (a value plus its basis, checked by nobody but a human):
    figures produced by a CI run or a dry run whose output is not committed as
    machine-readable JSON -- the held-out triple, the demo figures, the
    third-party construct coverage. These carry the run that produced them in
    their ``basis`` string. They are NOT derived, and pretending otherwise
    would be the same dishonesty this script exists to prevent.

Determinism (R8)
----------------
No wall-clock read. The build stamps the HEAD commit's own date, so the same
commit renders byte-identical output on any machine and at any hour. Paths are
hashed and sorted in ``Path.as_posix()`` form.

Usage
-----
    python3 tools/build_site.py                 # -> _site/
    python3 tools/build_site.py --out <dir>
    python3 tools/build_site.py --check         # verify only, write nothing
"""

from __future__ import annotations

import argparse
import html
import json
import re
import subprocess
import sys
from pathlib import Path
from typing import Callable, Dict, List, Optional

REPO = Path(__file__).resolve().parents[1]
SITE = REPO / "site"
FIGURES_JSON = SITE / "figures.json"
SUMMARY_MD = SITE / "summary.md"

PLACEHOLDER = re.compile(r"\{\{fig:([a-z0-9_]+)\}\}")


class BuildError(RuntimeError):
    """The build refuses to emit a page it cannot stand behind."""


# --------------------------------------------------------------------------
# repository readers
# --------------------------------------------------------------------------


def _json(path: Path) -> dict:
    if not path.exists():
        raise BuildError(f"required repository file is missing: {path.relative_to(REPO).as_posix()}")
    return json.loads(path.read_text(encoding="utf-8"))


def _bench_ledger() -> dict:
    """The newest sealed RELIAN-BENCH ledger, chosen by version, not by name."""
    candidates = sorted(
        (p for p in (REPO / "bench").glob("LEDGER_relian-bench-v*.json")),
        key=lambda p: p.as_posix(),
    )
    if not candidates:
        raise BuildError("no sealed RELIAN-BENCH ledger found under bench/")
    best, best_key = None, None
    for path in candidates:
        data = _json(path)
        key = tuple(int(n) for n in str(data["version"]).split("."))
        if best_key is None or key > best_key:
            best, best_key = data, key
    return best  # type: ignore[return-value]


def _discovery_ledger() -> dict:
    matches = sorted((REPO / "discovery-bench").glob("LEDGER_*.json"), key=lambda p: p.as_posix())
    if not matches:
        raise BuildError("no sealed RELIAN-DISCOVERY-BENCH ledger found")
    return _json(matches[0])


def _workflow_pin(name: str) -> str:
    """Read a pinned expectation out of the unit-test gate.

    The site's test triple is not retyped here: it is read from the same
    workflow variable CI asserts against, so the page and the gate cannot come
    to disagree about how many tests this repository has.
    """
    text = (REPO / ".github" / "workflows" / "tests.yml").read_text(encoding="utf-8")
    match = re.search(rf'^\s*{re.escape(name)}:\s*"(\d+)"\s*$', text, re.MULTILINE)
    if not match:
        raise BuildError(f"{name} not found in .github/workflows/tests.yml")
    return match.group(1)


def _git(*args: str) -> str:
    try:
        return subprocess.run(
            ["git", *args], cwd=REPO, capture_output=True, text=True, check=True
        ).stdout.strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return ""


def _vector_total(ledger: dict, split: str) -> int:
    return sum(v for k, v in ledger["vector_counts"].items() if split in k)


def _fmt(value: float, places: int) -> str:
    return f"{value:.{places}f}"


# --------------------------------------------------------------------------
# derivations: name -> callable returning the string the page must show
# --------------------------------------------------------------------------


def _derivations() -> Dict[str, Callable[[], str]]:
    bench = _bench_ledger()
    disc = _discovery_ledger()
    thresholds = bench["thresholds"]
    c1 = bench["baselines_recorded"]["C1_rulebased"]

    return {
        "bench_version": lambda: "v" + ".".join(str(bench["version"]).split(".")[:2]),
        "bench_tag": lambda: str(bench["tag"]),
        "bench_files": lambda: str(bench["file_count"]),
        "bench_payload": lambda: str(bench["payload_sha256"])[:8],
        "seal_fingerprint": lambda: str(bench["signature"]["key_fingerprint"]),
        "bench_programs": lambda: str(len([k for k in bench["vector_counts"] if "public" in k])),
        "heldout_vectors": lambda: str(_vector_total(bench, "heldout")),
        "public_vectors": lambda: str(_vector_total(bench, "public")),
        "ber_bar": lambda: _fmt(thresholds["ber_heldout_min"], 2),
        "build_bar": lambda: _fmt(thresholds["build_rate_min"], 2),
        "coverage_bar": lambda: _fmt(thresholds["branch_coverage_min"], 2),
        "coverage_tool": lambda: str(thresholds["coverage_required_tool"]),
        "c1_seal_ber": lambda: _fmt(c1["ber_overall"], 4),
        "c1_seal_build": lambda: "{}/{}".format(
            round(c1["build_rate"] * len([k for k in bench["vector_counts"] if "public" in k])),
            len([k for k in bench["vector_counts"] if "public" in k]),
        ),
        "disc_version": lambda: "v" + ".".join(str(disc["version"]).split(".")[:2]),
        "disc_tag": lambda: str(disc["tag"]),
        "disc_copybooks": lambda: str(disc["corpus_counts"]["copybooks"]),
        "disc_fields": lambda: str(disc["corpus_counts"]["elementary_fields"]),
        "disc_probes": lambda: str(disc["corpus_counts"]["probe_rows"]),
        "disc_variants": lambda: str(disc["corpus_counts"]["variants"]),
        "disc_conditions": lambda: str(disc["corpus_counts"]["conditions"]),
        "disc_gaps": lambda: str(disc["corpus_counts"]["gap_rows"]),
        "disc_oracle": lambda: str(disc["oracle_toolchain"]["cobc"]).replace("cobc (GnuCOBOL) ", "GnuCOBOL "),
        "suite_passed": lambda: _workflow_pin("EXPECTED_PASSES"),
        "suite_skipped": lambda: _workflow_pin("EXPECTED_SKIPS"),
        "head_sha": lambda: _git("rev-parse", "--short=7", "HEAD") or "unknown",
        "head_date": lambda: (_git("log", "-1", "--format=%cd", "--date=format:%Y-%m-%d") or "unknown"),
    }


# --------------------------------------------------------------------------
# figures
# --------------------------------------------------------------------------


def load_figures() -> dict:
    data = _json(FIGURES_JSON)
    for key in ("position", "figures"):
        if key not in data:
            raise BuildError(f"site/figures.json is missing required key {key!r}")
    return data


def resolve(figures: dict) -> Dict[str, str]:
    """The value each figure renders as.

    A ``volatile`` figure (the commit under build, its date) is STAMPED from
    the derivation rather than declared, because pinning a value that changes
    on every commit would make the drift gate cry wolf on every commit and be
    switched off within a week. Everything else renders the declared value,
    which :func:`verify` has already checked against the repository.
    """
    derivations = _derivations()
    table: Dict[str, str] = {}
    for name, spec in figures["figures"].items():
        derive = spec.get("derive")
        if spec.get("volatile") and derive in derivations:
            table[name] = derivations[derive]()
        else:
            table[name] = str(spec.get("value", ""))
    return table


def verify(figures: dict) -> List[str]:
    """Recompute every pinned derivable figure. Return the disagreements."""
    derivations = _derivations()
    drift: List[str] = []
    for name, spec in sorted(figures["figures"].items()):
        for required in ("value", "grade", "basis"):
            if required not in spec:
                drift.append(f"{name}: missing required field {required!r} (R9)")
        derive = spec.get("derive")
        if not derive:
            continue
        if derive not in derivations:
            drift.append(f"{name}: derive={derive!r} names no known derivation")
            continue
        if spec.get("volatile"):
            continue  # stamped, not pinned -- see resolve()
        actual = derivations[derive]()
        declared = str(spec.get("value", ""))
        if actual != declared:
            drift.append(
                f"{name}: declared {declared!r} but {derive} recomputes to {actual!r} "
                f"-- the repository moved and site/figures.json did not"
            )
    return drift


# --------------------------------------------------------------------------
# a small, deterministic Markdown renderer
# --------------------------------------------------------------------------

def _slug(text: str) -> str:
    """A stable anchor for a heading. Section numbers are dropped so a
    contents link does not break the day a section is renumbered."""
    bare = re.sub(r"^\d+(\.\d+)*\.?\s+", "", text.strip())
    bare = re.sub(r"[`*_]", "", bare)
    bare = re.sub(r"[^a-z0-9]+", "-", bare.lower()).strip("-")
    return bare


_LIST_ITEM = re.compile(r"^(\s*)([-*]|\d+\.)\s+(.*)$")
_BLOCK_START = re.compile(r"^\s*(#{1,6}\s|>|\|)")
_INLINE_CODE = re.compile(r"`([^`]+)`")
_BOLD = re.compile(r"\*\*([^*]+)\*\*")
_ITALIC = re.compile(r"(?<![*\w])\*([^*\n]+)\*(?!\*)")
_LINK = re.compile(r"\[([^\]]+)\]\(([^)]+)\)")


def _inline(text: str) -> str:
    out = html.escape(text, quote=False)
    out = _INLINE_CODE.sub(lambda m: f"<code>{m.group(1)}</code>", out)
    out = _BOLD.sub(lambda m: f"<strong>{m.group(1)}</strong>", out)
    out = _ITALIC.sub(lambda m: f"<em>{m.group(1)}</em>", out)
    out = _LINK.sub(lambda m: f'<a href="{m.group(2)}">{m.group(1)}</a>', out)
    return out


def _table(rows: List[str]) -> str:
    def cells(line: str) -> List[str]:
        return [c.strip() for c in line.strip().strip("|").split("|")]

    head, body = cells(rows[0]), rows[2:]
    out = ['<div class="tablewrap"><table><thead><tr>']
    out += [f"<th scope=\"col\">{_inline(c)}</th>" for c in head]
    out.append("</tr></thead><tbody>")
    for line in body:
        out.append("<tr>" + "".join(f"<td>{_inline(c)}</td>" for c in cells(line)) + "</tr>")
    out.append("</tbody></table></div>")
    return "".join(out)


def markdown(src: str) -> str:
    """Render the subset of Markdown this repository's summary actually uses.

    Deliberately small. `verify_no_content_lost` below asserts that nothing in
    the source vanished, which is the guarantee that matters: a renderer that
    silently drops a paragraph is worse than one that cannot render it.
    """
    lines = src.replace("\r\n", "\n").split("\n")
    out: List[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        stripped = line.strip()

        if not stripped:
            i += 1
            continue

        if stripped.startswith("|") and i + 1 < len(lines) and set(lines[i + 1].strip()) <= set("|-: "):
            block = []
            while i < len(lines) and lines[i].strip().startswith("|"):
                block.append(lines[i])
                i += 1
            out.append(_table(block))
            continue

        if re.fullmatch(r"-{3,}", stripped):
            out.append('<hr class="rule">')
            i += 1
            continue

        heading = re.match(r"^(#{1,6})\s+(.*)$", stripped)
        if heading:
            level = len(heading.group(1))
            text = heading.group(2)
            out.append(f'<h{level} id="{_slug(text)}">{_inline(text)}</h{level}>')
            i += 1
            continue

        if stripped.startswith(">"):
            block = []
            while i < len(lines) and lines[i].strip().startswith(">"):
                block.append(lines[i].strip().lstrip(">").strip())
                i += 1
            out.append(f"<blockquote><p>{_inline(' '.join(block))}</p></blockquote>")
            continue

        bullet = _LIST_ITEM.match(line)
        if bullet:
            ordered = bool(re.fullmatch(r"\d+\.", bullet.group(2)))
            tag = "ol" if ordered else "ul"
            # §4 of the summary numbers 1..12 straight through four headings,
            # so each <ol> after the first must resume rather than restart.
            start = f' start="{bullet.group(2).rstrip(".")}"' if ordered else ""
            items: List[str] = []
            while i < len(lines):
                m = _LIST_ITEM.match(lines[i])
                if m:
                    items.append(m.group(3))
                    i += 1
                    continue
                if lines[i].startswith((" ", "\t")) and lines[i].strip() and items:
                    items[-1] += " " + lines[i].strip()
                    i += 1
                    continue
                if not lines[i].strip() and i + 1 < len(lines) and _LIST_ITEM.match(lines[i + 1]):
                    i += 1
                    continue
                break
            out.append(f"<{tag}{start}>" + "".join(f"<li>{_inline(it)}</li>" for it in items) + f"</{tag}>")
            continue

        para = []
        while (
            i < len(lines)
            and lines[i].strip()
            and not _LIST_ITEM.match(lines[i])
            and not _BLOCK_START.match(lines[i])
            and not re.fullmatch(r"-{3,}", lines[i].strip())
        ):
            para.append(lines[i].strip())
            i += 1
        if para:
            out.append(f"<p>{_inline(' '.join(para))}</p>")
        else:
            # Nothing consumed this line. Rather than skip it -- which is how a
            # renderer loses a paragraph in silence -- emit it as a paragraph.
            out.append(f"<p>{_inline(lines[i].strip())}</p>")
            i += 1
    return "\n".join(out)


def contents(rendered_html: str) -> str:
    """The summary's contents list, built from the headings it actually has.

    Hand-maintained, this list breaks the first time a section is renamed --
    which it did, silently, the moment section 8 was retitled. Generated, a
    contents entry cannot point at a heading that is not there.
    """
    items = re.findall(r'<h2 id="([^"]+)">(.*?)</h2>', rendered_html, re.DOTALL)
    if not items:
        raise BuildError("the rendered summary has no <h2> sections to build a contents list from")
    links = "".join(
        f'<li><a href="#{anchor}">{re.sub(r"<[^>]+>", "", text)}</a></li>' for anchor, text in items
    )
    return f"<ul>{links}</ul>"


_WORD = re.compile(r"[A-Za-z0-9]+")


def verify_no_content_lost(source_md: str, rendered_html: str) -> List[str]:
    """Every word in the Markdown must survive into the rendered page.

    A renderer that meets an unhandled construct and drops the line would
    otherwise ship a page quietly missing a paragraph. This is the check that
    makes the small renderer above safe to rely on.
    """
    visible = re.sub(r"<[^>]+>", " ", rendered_html)
    visible = html.unescape(visible)
    have = set(_WORD.findall(visible.lower()))
    # An ordered-list marker is structure, not prose: the browser renders it
    # from the <ol start=...>, so it never appears in the text content.
    source_prose = re.sub(r"^\s*\d+\.\s+", "", html.unescape(source_md), flags=re.MULTILINE)
    want = set(_WORD.findall(source_prose.lower()))
    missing = sorted(want - have)
    return missing


# --------------------------------------------------------------------------
# rendering
# --------------------------------------------------------------------------


def render(template: str, figures: dict, extra: Optional[Dict[str, str]] = None) -> str:
    table = resolve(figures)
    table.update(extra or {})
    unknown: List[str] = []

    def sub(match: "re.Match[str]") -> str:
        name = match.group(1)
        if name not in table:
            unknown.append(name)
            return match.group(0)
        return table[name]

    out = PLACEHOLDER.sub(sub, template)
    if unknown:
        raise BuildError(
            "template references figures that site/figures.json does not define: "
            + ", ".join(sorted(set(unknown)))
        )
    return out


def stamp_position(atlas: str, position: str) -> str:
    """Write the flag's position into the SERVED markup, not just the script.

    The page's JavaScript recomputes all of this on load, but a page whose
    first paint disagrees with its own data -- or which renders the wrong stop
    entirely with scripting off -- is a page that is briefly lying. Advancing
    the build is one edit to ``position`` in site/figures.json; everything
    below follows from it deterministically.
    """
    ids = re.findall(r'<li class="stop" data-id="(s\d+)"', atlas)
    if position not in ids:
        raise BuildError(
            f"position {position!r} names no stop on the rail; known stops: {', '.join(ids)}"
        )
    idx = ids.index(position)

    def stop_state(match: "re.Match[str]") -> str:
        i = ids.index(match.group(1))
        state = "done" if i < idx else ("current" if i == idx else "future")
        return f'<li class="stop" data-id="{match.group(1)}" data-state="{state}">'

    atlas = re.sub(r'<li class="stop" data-id="(s\d+)" data-state="[a-z]+">', stop_state, atlas)

    # aria-current lives on exactly one stop button
    atlas = atlas.replace(' aria-current="step"', "")
    atlas = re.sub(
        rf'(<button class="stop-btn" type="button" data-target="{position}")',
        r'\1 aria-current="step"',
        atlas,
        count=1,
    )

    # exactly one state panel is visible
    def panel(match: "re.Match[str]") -> str:
        pid = match.group(1)
        return f'<section class="panel" data-for="{pid}" data-status="{match.group(2)}"' + (
            "" if pid == position else " hidden"
        )

    atlas = re.sub(
        r'<section class="panel" data-for="(s\d+)" data-status="([a-z]+)"(?: hidden)?',
        panel,
        atlas,
    )

    # phase bands fill in from the same list the script uses -- parsed from it,
    # so the two cannot drift
    band_match = re.search(r"BAND_END\s*=\s*\[([0-9,\s]+)\]", atlas)
    if not band_match:
        raise BuildError("BAND_END not found in the atlas template; cannot stamp the phase bands")
    band_end = [int(n) for n in band_match.group(1).split(",") if n.strip()]
    bands = list(re.finditer(r'<div class="band"(?: data-done="true")?( style="[^"]*")>', atlas))
    if len(bands) != len(band_end):
        raise BuildError(
            f"{len(bands)} phase bands in the markup but BAND_END has {len(band_end)} entries"
        )
    for band, end in zip(reversed(bands), reversed(band_end)):
        done = ' data-done="true"' if end <= idx + 1 else ""
        replacement = f'<div class="band"{done}{band.group(1)}>'
        atlas = atlas[: band.start()] + replacement + atlas[band.end():]
    return atlas


def used_placeholders(*templates: str) -> set:
    names: set = set()
    for t in templates:
        names |= {m.group(1) for m in PLACEHOLDER.finditer(t)}
    return names


# --------------------------------------------------------------------------
# main
# --------------------------------------------------------------------------


def build(out_dir: Path, check_only: bool = False) -> int:
    """Every refusal leaves by the same door: print why, return 1, write nothing.

    A caller should not have to know which failures come back as an exit code
    and which come back as an exception.
    """
    try:
        return _build(out_dir, check_only=check_only)
    except BuildError as exc:
        print(f"BUILD REFUSED -- {exc}", file=sys.stderr)
        return 1


def _build(out_dir: Path, check_only: bool = False) -> int:
    figures = load_figures()

    drift = verify(figures)
    if drift:
        print("BUILD REFUSED -- the site disagrees with the repository:\n", file=sys.stderr)
        for item in drift:
            print(f"  * {item}", file=sys.stderr)
        print(
            "\nFix site/figures.json (or the repository) so the two agree. A page that "
            "states a stale number is the defect this build exists to prevent.",
            file=sys.stderr,
        )
        return 1

    style = (SITE / "_style.html").read_text(encoding="utf-8")
    nav = (SITE / "_nav.html").read_text(encoding="utf-8")

    def shell(name: str, active: str) -> str:
        tpl = (SITE / name).read_text(encoding="utf-8")
        page_nav = nav.replace(
            "{{active_" + active + "}}", ' aria-current="page"'
        )
        page_nav = re.sub(r"\{\{active_[a-z]+\}\}", "", page_nav)
        return tpl.replace("{{style}}", style).replace("{{nav}}", page_nav)

    atlas_tpl = shell("atlas.template.html", "atlas")
    summary_tpl = shell("summary.template.html", "summary")

    # every declared figure must actually reach a page; an orphan is a figure
    # someone stopped displaying without noticing.
    declared = set(figures["figures"])
    used = used_placeholders(atlas_tpl, summary_tpl, SUMMARY_MD.read_text(encoding="utf-8"))
    orphans = sorted(declared - used - set(figures.get("appendix_only", [])))
    if orphans:
        print(
            "BUILD REFUSED -- figures declared but displayed nowhere: " + ", ".join(orphans),
            file=sys.stderr,
        )
        return 1

    # The summary is a template too: its figures are placeholders, resolved
    # here, so a number in the prose cannot drift from the same number on the
    # Atlas or from the repository underneath both.
    summary_md = render(SUMMARY_MD.read_text(encoding="utf-8"), figures)
    summary_body = markdown(summary_md)
    missing = verify_no_content_lost(summary_md, summary_body)
    if missing:
        print(
            "BUILD REFUSED -- the Markdown renderer dropped content: "
            + ", ".join(missing[:20]),
            file=sys.stderr,
        )
        return 1

    position = str(figures["position"])
    atlas = render(atlas_tpl, figures)
    atlas = atlas.replace("{{position}}", position)
    atlas = atlas.replace("{{position_num}}", position.lstrip("s").zfill(2))
    atlas = stamp_position(atlas, position)

    summary = render(summary_tpl, figures)
    summary = summary.replace("{{summary_toc}}", contents(summary_body))
    summary = summary.replace("{{summary_body}}", summary_body)

    left = sorted(set(re.findall(r"\{\{[a-z_:0-9]+\}\}", atlas + summary)))
    if left:
        print("BUILD REFUSED -- unresolved placeholders: " + ", ".join(left), file=sys.stderr)
        return 1

    if check_only:
        print(f"OK -- {len(declared)} figures, {len(used)} referenced, no drift.")
        return 0

    out_dir.mkdir(parents=True, exist_ok=True)
    (out_dir / "index.html").write_text(atlas, encoding="utf-8")
    (out_dir / "summary.html").write_text(summary, encoding="utf-8")
    (out_dir / "figures.json").write_text(
        json.dumps(figures, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    (out_dir / ".nojekyll").write_text("", encoding="utf-8")

    print(f"built {out_dir.as_posix()}/index.html and summary.html "
          f"({len(declared)} figures, position {figures['position']})")
    return 0


def main(argv: Optional[List[str]] = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--out", default=str(REPO / "_site"), help="output directory (default: _site/)")
    ap.add_argument("--check", action="store_true", help="verify only; write nothing")
    args = ap.parse_args(argv)
    try:
        return build(Path(args.out), check_only=args.check)
    except BuildError as exc:
        print(f"BUILD REFUSED -- {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
