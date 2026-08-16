"""WP-1.7 — report rendering.

Three artifacts from one bundle:

* **JSON** — canonical (sorted keys, no insignificant whitespace, UTF-8). This
  is the ledger artifact, and ``report_hash`` is ``sha256`` of exactly these
  bytes.
* **Markdown** — the human-readable report.
* **DOCX** — rendered with ``python-docx`` (chosen over a Node ``docx``
  toolchain so the assessment CLI has no JavaScript runtime dependency). If
  ``python-docx`` is not installed, ``render()`` returns no ``docx`` path and
  states the reason; it never emits an empty file to make the output look
  complete.

TEMPLATE DISCIPLINE (linted by ``tests/assessment/test_report_lint.py``)
-----------------------------------------------------------------------

Every template string lives in :data:`TEMPLATES`, and no template contains a
numeric literal other than a section number in a heading. Numbers reach the
page only by substitution from measured values.

Every table of numbers in the rendered body either carries ``Grade`` and
``Provenance`` columns, or is immediately preceded by a ``**Grade:**`` and a
``**Provenance:**`` line covering every number in it (R9). The lint asserts
this over the *rendered* document, so a table added later without provenance
fails the build.

The report never states an engagement fee, a timeline, a delivery date, or a
behavioural-equivalence claim. It reports coverage and complexity; quoting is a
human step (R11). ``tests/assessment/test_report_lint.py`` asserts the absence
of that vocabulary too.
"""

from __future__ import annotations

import hashlib
import platform
import sys
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

from . import complexity as complexity_mod
from . import coverage as coverage_mod
from . import loc as loc_mod
from .models import (
    AssessmentBundle,
    Measured,
    ProgramAssessment,
    canonical_json,
)
from .risk import programs_by_tier, rule_table, tier_counts
from .supported import (
    boundary_only_tokens,
    registry_provenance,
    supported_data_features,
    supported_verbs,
)

SCHEMA_VERSION = "relian-assessment-1"

TEMPLATES: Dict[str, str] = {
    "title": "# Legacy Code Assessment — {root}\n",
    "subtitle": (
        "Schema `{schema}` · manifest `{manifest_hash}`\n\n"
        "Every number in this report is a measurement with a stated origin and a "
        "Trutina grade, or it is absent. Nothing here is a default, an estimate, "
        "or a target reported as a result.\n"
    ),
    "h1": "\n## {n}. {title}\n\n",
    "h2": "\n### {title}\n\n",
    "grade_line": "**Grade:** {grade} · **Provenance:** {provenance}\n\n",
    "para": "{text}\n\n",
    "bullet": "- {text}\n",
    "table_row": "| {cells} |\n",
    "code_block": "```\n{text}\n```\n\n",
    "no_data": "_Not measured — {reason}._\n\n",
}

GRADE_COLUMNS = ("Value", "Grade", "Provenance")

# Vocabulary the report must never contain (R11): commercial commitments the
# tool is not entitled to make.
FORBIDDEN_VOCABULARY = (
    "engagement fee", "fixed price", "we will deliver", "delivery date",
    "timeline", "behavioral equivalence rate", "behavioural equivalence rate",
    "guaranteed", "warranty",
)


# --------------------------------------------------------------------------
# Markdown assembly helpers
# --------------------------------------------------------------------------


def _row(*cells: object) -> str:
    return TEMPLATES["table_row"].format(cells=" | ".join(str(c) for c in cells))


def _table(headers: Sequence[str], rows: Sequence[Sequence[object]]) -> str:
    out = [_row(*headers), _row(*("---" for _ in headers))]
    out.extend(_row(*r) for r in rows)
    return "".join(out) + "\n"


def _m_row(label: str, m: Optional[Measured], absent_reason: str = "not measured") -> List[object]:
    if m is None:
        return [label, "—", "—", absent_reason]
    return [label, m.value, m.grade, m.provenance]


def _grade_line(grade: str, provenance: str) -> str:
    return TEMPLATES["grade_line"].format(grade=grade, provenance=provenance)


# --------------------------------------------------------------------------
# Migration-scope split
# --------------------------------------------------------------------------


def compute_scope_split(
    programs: Sequence[ProgramAssessment],
) -> Tuple[Optional[Measured], Optional[Measured], Tuple[Tuple[str, int], ...]]:
    """Quotable-today LOC vs LOC requiring grammar expansion, plus by-construct counts.

    Attribution is by *source line*: a code line is "requires grammar expansion"
    if it carries at least one construct C1 cannot transpile. Lines are counted
    once even when they carry several such constructs. Programs whose coverage
    could not be measured contribute to neither figure and are named in the
    provenance string — they are not silently counted as quotable.
    """
    measurable = [p for p in programs if p.coverage.coverage_ratio is not None]
    if not measurable:
        return None, None, ()

    total_code = 0
    blocked_lines = 0
    by_construct: Dict[str, int] = {}
    for p in measurable:
        total_code += p.loc.physical - p.loc.comment - p.loc.blank
        lines = {h.line for h in p.coverage.unsupported_inventory}
        blocked_lines += len(lines)
        for h in p.coverage.unsupported_inventory:
            by_construct[h.verb] = by_construct.get(h.verb, 0) + 1

    quotable = total_code - blocked_lines
    excluded = len(programs) - len(measurable)
    grades = {p.coverage.coverage_ratio.grade for p in measurable}
    grade = "VERIFIED" if grades == {"VERIFIED"} else "PLAUSIBLE"
    suffix = f"; {excluded} program(s) excluded (coverage not measured)" if excluded else ""

    quotable_m = Measured(
        quotable,
        f"code lines ({total_code}) minus lines carrying an unsupported construct "
        f"({blocked_lines}) across {len(measurable)} program(s){suffix}",
        grade,
    )
    grammar_m = Measured(
        blocked_lines,
        f"distinct code lines carrying >=1 construct outside {registry_provenance()} "
        f"across {len(measurable)} program(s){suffix}",
        grade,
    )
    ranked = tuple(sorted(by_construct.items(), key=lambda kv: (-kv[1], kv[0])))
    return quotable_m, grammar_m, ranked


def tool_versions() -> Tuple[Tuple[str, str], ...]:
    try:
        import antlr4                                   # noqa: PLC0415
        antlr_version = getattr(antlr4, "__version__", "unknown")
    except Exception:
        antlr_version = "not installed"
    try:
        import docx                                     # noqa: PLC0415
        docx_version = getattr(docx, "__version__", "installed")
    except Exception:
        docx_version = "not installed"
    return tuple(sorted({
        "python": platform.python_version(),
        "platform": platform.system(),
        "antlr4-python3-runtime": antlr_version,
        "python-docx": docx_version,
        "relian_transpiler": registry_provenance(),
        "schema": SCHEMA_VERSION,
        "cli": " ".join(Path(a).name for a in sys.argv[:1]) or "python -m src.assessment.cli",
    }.items()))


# --------------------------------------------------------------------------
# Markdown
# --------------------------------------------------------------------------


def render_markdown(bundle: AssessmentBundle, root_label: str,
                    scope_by_construct: Sequence[Tuple[str, int]] = ()) -> str:
    t = TEMPLATES
    out: List[str] = [
        t["title"].format(root=root_label),
        t["subtitle"].format(schema=bundle.schema_version,
                             manifest_hash=bundle.inventory.manifest_hash),
    ]

    cov = bundle.portfolio_coverage
    loc_totals = loc_mod.portfolio_totals([p.loc for p in bundle.programs])

    # 1. Executive summary
    out.append(t["h1"].format(n=1, title="Executive summary"))
    out.append(_table(
        ("Measure", *GRADE_COLUMNS),
        [
            _m_row("Portfolio construct coverage", cov.coverage_ratio,
                   cov.error or "no statements recovered"),
            _m_row("Quotable-today code lines", bundle.quotable_loc),
            _m_row("Code lines requiring grammar expansion", bundle.grammar_expansion_loc),
        ],
    ))
    out.append(_grade_line(
        "PLAUSIBLE",
        "portfolio risk tier is a policy decision from the RISK_RULES table "
        "reproduced in the appendix; its inputs are VERIFIED measurements",
    ))
    out.append(_table(
        ("Measure", "Value"),
        [
            ["Portfolio risk tier", bundle.portfolio_risk.tier.value],
            ["Rule that fired", f"`{bundle.portfolio_risk.rule}`"],
        ],
    ))

    # 2. Manifest
    out.append(t["h1"].format(n=2, title="Manifest"))
    out.append(_grade_line(
        "VERIFIED",
        "sha256 and size_bytes are of the raw bytes on disk; the manifest hash is "
        "sha256 of the canonical JSON of the sorted record list "
        f"(= {bundle.inventory.manifest_hash})",
    ))
    out.append(_table(
        ("Path", "Kind", "Bytes", "Line ending", "sha256"),
        [
            [r.rel_path_posix, r.kind, r.size_bytes, r.line_ending, f"`{r.sha256[:16]}`"]
            for r in bundle.inventory.records
        ],
    ))

    # 3. LOC inventory
    out.append(t["h1"].format(n=3, title="LOC inventory"))
    out.append(_grade_line(
        "VERIFIED",
        "line categories counted per the rules in appendix A; logical statements "
        "come from the same extraction as the coverage map, and are absent where "
        "no statements could be recovered",
    ))
    out.append(_table(
        ("Program", "Physical", "Comment", "Blank", "Code", "Logical", "Method", "Dead paragraphs"),
        [
            [
                p.program_id, p.loc.physical, p.loc.comment, p.loc.blank,
                p.loc.physical - p.loc.comment - p.loc.blank,
                p.loc.logical if p.loc.logical is not None else "—",
                p.loc.logical_method,
                ", ".join(p.loc.dead_paragraphs) or "—",
            ]
            for p in bundle.programs
        ],
    ))
    out.append(t["para"].format(text=(
        f"Portfolio totals — physical {loc_totals['physical']}, "
        f"code {loc_totals['code']}, comment {loc_totals['comment']}, "
        f"blank {loc_totals['blank']}, logical "
        f"{loc_totals['logical'] if loc_totals['logical'] is not None else '—'} "
        f"({loc_totals['logical_programs_measured']} program(s) measured, "
        f"{loc_totals['logical_programs_unmeasured']} not measured)."
    )))
    notes = [p for p in bundle.programs if p.loc.note]
    if notes:
        out.append(t["h2"].format(title="LOC notes"))
        for p in notes:
            out.append(t["bullet"].format(text=f"`{p.program_id}` — {p.loc.note}"))
        out.append("\n")

    # 4. Coverage map
    out.append(t["h1"].format(n=4, title="Coverage map"))
    out.append(_table(
        ("Program", *GRADE_COLUMNS),
        [_m_row(p.program_id, p.coverage.coverage_ratio,
                p.coverage.error or "no statements recovered")
         for p in bundle.programs],
    ))
    out.append(t["h2"].format(title="Portfolio"))
    out.append(_table(
        ("Measure", *GRADE_COLUMNS),
        [
            _m_row("Coverage ratio", cov.coverage_ratio, cov.error or "not measured"),
        ],
    ))
    if cov.parser_errors:
        out.append(t["h2"].format(title="Programs excluded from the ratio"))
        for e in cov.parser_errors:
            out.append(t["bullet"].format(text=e))
        out.append("\n")

    # 5. Unsupported-construct inventory
    out.append(t["h1"].format(n=5, title="Unsupported-construct inventory"))
    ranked = cov.ranked_unsupported()
    if not ranked:
        out.append(t["para"].format(
            text="No construct outside the supported set was found."))
    else:
        out.append(_grade_line(
            "VERIFIED",
            "occurrence counts of constructs absent from "
            f"{registry_provenance()}, counted over the statements listed in the "
            "coverage map",
        ))
        out.append(_table(
            ("Construct", "Occurrences"),
            [[verb, count] for verb, count in ranked],
        ))
        out.append(t["h2"].format(title="Occurrences"))
        out.append(_table(
            ("File", "Line", "Paragraph", "Construct", "Context"),
            [
                [h.file, h.line, h.paragraph or "—", h.verb, h.context or "—"]
                for h in cov.unsupported_inventory
            ],
        ))

    # 6. DATA DIVISION features
    out.append(t["h1"].format(n=6, title="DATA DIVISION features found"))
    summary = coverage_mod.data_feature_summary([p.coverage for p in bundle.programs])
    if not summary:
        out.append(t["para"].format(text="No classified DATA DIVISION feature was found."))
    else:
        out.append(_grade_line(
            "VERIFIED",
            "occurrence counts from source; each status is probed against the "
            "transpiler itself, not asserted — `accepted_ignored` means the clause "
            "parses but is discarded, so generated code cannot depend on it",
        ))
        out.append(_table(
            ("Feature", "Occurrences", "C1 status"),
            [[f, v["occurrences"], v["status"]] for f, v in summary.items()],
        ))

    # 7. Complexity findings
    out.append(t["h1"].format(n=7, title="Complexity findings"))
    out.append(_grade_line(
        "VERIFIED",
        "computed per the formulas in appendix B; no threshold is applied here",
    ))
    out.append(_table(
        ("Program", "Cyclomatic", "Statements", "GO TO", "GO TO density",
         "ALTER", "EXEC CICS", "EXEC SQL", "Max nesting"),
        [
            [
                p.program_id,
                p.complexity.cyclomatic if p.complexity else "—",
                p.complexity.statements if p.complexity else "—",
                p.complexity.goto_count if p.complexity else "—",
                (p.complexity.goto_density.value
                 if p.complexity and p.complexity.goto_density else "—"),
                ("yes" if p.complexity.alter_present else "no") if p.complexity else "—",
                p.complexity.exec_cics_count if p.complexity else "—",
                p.complexity.exec_sql_count if p.complexity else "—",
                p.complexity.max_nesting_depth if p.complexity else "—",
            ]
            for p in bundle.programs
        ],
    ))
    fan_in = complexity_mod.copybook_fan_in(
        [p.complexity for p in bundle.programs if p.complexity]
    )
    if fan_in:
        out.append(t["h2"].format(title="Copybook fan-in"))
        out.append(_grade_line("VERIFIED", "COPY targets named in program source"))
        out.append(_table(
            ("Copybook", "Used by"),
            [[book, ", ".join(progs)] for book, progs in fan_in.items()],
        ))

    # 8. Risk tiers
    out.append(t["h1"].format(n=8, title="Risk tiers"))
    out.append(_grade_line(
        "PLAUSIBLE",
        "a published policy (RISK_RULES, appendix C), not a measurement; every "
        "input to it is VERIFIED",
    ))
    out.append(_table(
        ("Program", "Tier", "Rule that fired"),
        [[p.program_id, p.risk.tier.value, f"`{p.risk.rule}`"] for p in bundle.programs],
    ))
    out.append(_table(
        ("Tier", "Programs"),
        [[tier, str(count)] for tier, count in
         tier_counts([p.risk for p in bundle.programs]).items()],
    ))

    # 9. Migration-scope recommendation
    out.append(t["h1"].format(n=9, title="Migration-scope recommendation"))
    out.append(_table(
        ("Measure", *GRADE_COLUMNS),
        [
            _m_row("Quotable-today code lines", bundle.quotable_loc),
            _m_row("Code lines requiring grammar expansion", bundle.grammar_expansion_loc),
        ],
    ))
    out.append(t["para"].format(text=(
        "Attribution is by source line: a code line requires grammar expansion "
        "if it carries at least one construct the deterministic transpiler "
        "cannot handle. This report does not price the work and does not state "
        "a schedule."
    )))
    if scope_by_construct:
        out.append(t["h2"].format(title="By construct — what grammar work would unlock"))
        out.append(_grade_line(
            "VERIFIED",
            "occurrences of each unsupported construct across the portfolio",
        ))
        out.append(_table(
            ("Construct", "Occurrences"),
            [[verb, count] for verb, count in scope_by_construct],
        ))

    # 10. Appendices
    out.append(t["h1"].format(n=10, title="Appendices"))

    out.append(t["h2"].format(title="Appendix A — LOC counting rules"))
    out.append(t["code_block"].format(text=(loc_mod.__doc__ or "").strip()))

    out.append(t["h2"].format(title="Appendix B — complexity formulas"))
    out.append(t["code_block"].format(text=(complexity_mod.__doc__ or "").strip()))

    out.append(t["h2"].format(title="Appendix C — RISK_RULES, verbatim and in evaluation order"))
    out.append(t["code_block"].format(text="\n".join(rule_table())))

    out.append(t["h2"].format(title="Appendix D — coverage method and its limits"))
    out.append(t["code_block"].format(text=(coverage_mod.__doc__ or "").strip()))

    out.append(t["h2"].format(title="Appendix E — supported set, read from the transpiler"))
    out.append(t["para"].format(text=f"Registry: `{registry_provenance()}`"))
    out.append(t["para"].format(
        text="Supported statement keywords: " +
             ", ".join(f"`{v}`" for v in sorted(supported_verbs()))))
    out.append(t["para"].format(
        text="Statement-boundary tokens that are **not** supported: " +
             ", ".join(f"`{v}`" for v in sorted(boundary_only_tokens()))))
    out.append(_table(
        ("DATA DIVISION feature", "C1 status"),
        [[f, s] for f, s in supported_data_features().items()],
    ))

    out.append(t["h2"].format(title="Appendix F — tool versions"))
    out.append(_table(("Component", "Version"), [[k, v] for k, v in bundle.tool_versions]))

    if bundle.notes:
        out.append(t["h2"].format(title="Appendix G — notes on this run"))
        for note in bundle.notes:
            out.append(t["bullet"].format(text=note))
        out.append("\n")

    return "".join(out)


# --------------------------------------------------------------------------
# DOCX
# --------------------------------------------------------------------------


def render_docx(markdown: str, path: Path) -> Optional[str]:
    """Write a DOCX rendering of the Markdown. Returns None (and writes nothing)
    when python-docx is unavailable, so a missing dependency never masquerades
    as a delivered artifact."""
    try:
        from docx import Document                       # noqa: PLC0415
    except ImportError:
        return None

    doc = Document()
    pending_table: List[List[str]] = []

    def flush_table() -> None:
        nonlocal pending_table
        if not pending_table:
            return
        rows = [r for r in pending_table if not all(set(c.strip()) <= {"-"} for c in r)]
        if rows:
            table = doc.add_table(rows=len(rows), cols=len(rows[0]))
            table.style = "Table Grid"
            for i, row in enumerate(rows):
                for j, cell in enumerate(row[:len(rows[0])]):
                    table.cell(i, j).text = cell
        pending_table = []

    in_code = False
    code_lines: List[str] = []
    for line in markdown.splitlines():
        if line.startswith("```"):
            if in_code:
                doc.add_paragraph("\n".join(code_lines), style="Intense Quote")
                code_lines = []
            in_code = not in_code
            continue
        if in_code:
            code_lines.append(line)
            continue
        if line.startswith("|"):
            pending_table.append([c.strip() for c in line.strip("|").split("|")])
            continue
        flush_table()
        if line.startswith("### "):
            doc.add_heading(line[4:], level=2)
        elif line.startswith("## "):
            doc.add_heading(line[3:], level=1)
        elif line.startswith("# "):
            doc.add_heading(line[2:], level=0)
        elif line.startswith("- "):
            doc.add_paragraph(line[2:], style="List Bullet")
        elif line.strip():
            doc.add_paragraph(line)
    flush_table()
    doc.save(str(path))
    return str(path)


# --------------------------------------------------------------------------
# render
# --------------------------------------------------------------------------


def render(bundle: AssessmentBundle, out_dir: Path, root_label: str = "",
           scope_by_construct: Sequence[Tuple[str, int]] = (),
           json_only: bool = False, docx: bool = True) -> Dict[str, Optional[str]]:
    """Write the report artifacts. Returns paths plus the ledger hash."""
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    json_text = canonical_json(bundle.to_dict())
    json_bytes = json_text.encode("utf-8")
    report_hash = hashlib.sha256(json_bytes).hexdigest()

    json_path = out_dir / "assessment.json"
    json_path.write_bytes(json_bytes)
    (out_dir / "assessment.sha256").write_text(f"{report_hash}  assessment.json\n")

    result: Dict[str, Optional[str]] = {
        "json": str(json_path),
        "report_hash": report_hash,
        "md": None,
        "docx": None,
        "docx_skipped_reason": None,
    }
    if json_only:
        return result

    markdown = render_markdown(bundle, root_label or "codebase", scope_by_construct)
    md_path = out_dir / "assessment.md"
    md_path.write_text(markdown, encoding="utf-8")
    result["md"] = str(md_path)

    if not docx:
        # DOCX rendering is quadratic-ish in table rows and dominates wall time
        # on large portfolios (measured: it is the bulk of a 44-program run).
        # Skipping it is an explicit choice, recorded as one.
        result["docx_skipped_reason"] = "--no-docx was passed"
        return result

    docx_path = render_docx(markdown, out_dir / "assessment.docx")
    if docx_path is None:
        result["docx_skipped_reason"] = (
            "python-docx is not installed; no DOCX was written. "
            "Install python-docx and re-run to produce it."
        )
    result["docx"] = docx_path
    return result
