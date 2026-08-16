"""WP-1.8 — the assessment CLI.

    python -m src.assessment.cli <root> --out <dir> [--json-only]

Runs WP-1.1 → WP-1.7 in order over a codebase and writes the report artifacts.
Read-only with respect to the codebase under assessment, and offline: nothing
in this package opens a socket or calls a model (R6, R12 — customer source
stays inside the customer perimeter).

Determinism: two runs over the same tree produce byte-identical JSON and the
same ``report_hash``, and a CRLF copy of the same tree produces the same
results as the LF original (R8). ``tests/assessment/test_determinism.py``
asserts all three.
"""

from __future__ import annotations

import argparse
import sys
import time
from pathlib import Path
from typing import List, Optional, Sequence

from . import complexity as complexity_mod
from . import coverage as coverage_mod
from . import loc as loc_mod
from . import report as report_mod
from . import risk as risk_mod
from .intake import build_inventory, read_source
from .models import AssessmentBundle, ProgramAssessment


def assess_tree(root: Path) -> tuple[AssessmentBundle, Sequence[tuple[str, int]]]:
    """Run the full assessment. Pure: no output is written here."""
    root = Path(root)
    inventory = build_inventory(root)

    programs: List[ProgramAssessment] = []
    notes: List[str] = []

    for record in inventory.records:
        if record.kind != "program":
            continue
        path = root / record.rel_path_posix if root.is_dir() else root
        source = read_source(path)

        cov = coverage_mod.analyze(record, source)
        loc_res = loc_mod.count(source, record.kind, record.rel_path_posix)
        comp: Optional[complexity_mod.ComplexityResult]
        if cov.total_statements:
            comp = complexity_mod.analyze(
                source, record.rel_path_posix, statements=cov.total_statements
            )
        else:
            comp = None
            notes.append(
                f"{record.rel_path_posix}: no statements recovered, so complexity "
                f"metrics are absent rather than zero"
            )
        finding = risk_mod.assess(record.rel_path_posix, cov, comp)
        programs.append(
            ProgramAssessment(
                program_id=record.rel_path_posix,
                file=record,
                coverage=cov,
                loc=loc_res,
                complexity=comp,
                risk=finding,
            )
        )

    portfolio_cov = coverage_mod.rollup([p.coverage for p in programs])
    portfolio_risk = risk_mod.portfolio([p.risk for p in programs])
    quotable, grammar_loc, by_construct = report_mod.compute_scope_split(programs)

    if not programs:
        notes.append("no files classified as COBOL programs were found under the root")
    if portfolio_cov.method.startswith("token_scan") or "token_scan" in portfolio_cov.method:
        notes.append(
            "coverage was derived by the documented token scan for at least one "
            "program because the bundled ANTLR grammar could not parse it without "
            "syntax errors; those figures are graded PLAUSIBLE, not VERIFIED"
        )

    bundle = AssessmentBundle(
        schema_version=report_mod.SCHEMA_VERSION,
        tool_versions=report_mod.tool_versions(),
        inventory=inventory,
        programs=tuple(programs),
        portfolio_coverage=portfolio_cov,
        portfolio_risk=portfolio_risk,
        quotable_loc=quotable,
        grammar_expansion_loc=grammar_loc,
        notes=tuple(notes),
    )
    return bundle, by_construct


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = argparse.ArgumentParser(
        prog="python -m src.assessment.cli",
        description="Relian legacy code assessment (read-only, offline).",
    )
    parser.add_argument("root", type=Path, help="codebase to assess")
    parser.add_argument("--out", type=Path, required=True, help="output directory")
    parser.add_argument("--json-only", action="store_true",
                        help="write assessment.json only (skip Markdown and DOCX)")
    parser.add_argument("--no-docx", action="store_true",
                        help="skip the DOCX rendering, which dominates wall time "
                             "on large portfolios")
    parser.add_argument("--quiet", action="store_true")
    args = parser.parse_args(argv)

    started = time.time()
    bundle, by_construct = assess_tree(args.root)
    paths = report_mod.render(
        bundle,
        args.out,
        root_label=args.root.as_posix(),
        scope_by_construct=by_construct,
        json_only=args.json_only,
        docx=not args.no_docx,
    )
    elapsed = time.time() - started

    if not args.quiet:
        cov = bundle.portfolio_coverage.coverage_ratio
        print(f"programs assessed : {len(bundle.programs)}")
        print(f"files in manifest : {len(bundle.inventory.records)}")
        print(f"manifest_hash     : {bundle.inventory.manifest_hash}")
        print(f"coverage          : "
              f"{cov.value if cov else 'not measured'}"
              f"{f' ({cov.grade})' if cov else ''}")
        print(f"portfolio risk    : {bundle.portfolio_risk.tier.value} "
              f"[{bundle.portfolio_risk.rule}]")
        print(f"report_hash       : {paths['report_hash']}")
        print(f"json              : {paths['json']}")
        if paths["md"]:
            print(f"md                : {paths['md']}")
        if paths["docx"]:
            print(f"docx              : {paths['docx']}")
        elif paths["docx_skipped_reason"]:
            print(f"docx              : not written — {paths['docx_skipped_reason']}")
        print(f"wall_seconds      : {elapsed:.2f}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
