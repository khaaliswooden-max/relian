"""WP-2.2 D21 — the discovery CLI, deliberately small.

Two surfaces, both read-only, both emitting JSON on stdout:

``python3 -m src.discovery.cli layout <copybook>``
    the computed record layout — the WP-2.2 deliverable.

``python3 -m src.discovery.cli resolve <root>``
    the copybook fan-in graph and the missing-copybook table (D20), which is
    what the CardDemo dry run needs.

``python3 -m src.discovery.cli report build <root> --out <dir> …``
    the WP-2.3 deliverable: a canonical ``report.json``, an unsigned Markdown
    rendering of it, a manifest of digests, and the instance Ed25519 signature
    over that manifest. **If a signing key cannot be obtained the command
    fails and writes nothing** — an unsigned artifact that looks signed is
    worse than no artifact.

``python3 -m src.discovery.cli report countersign-request --report-dir <dir>``
    the single line that crosses the customer perimeter: manifest hash, report
    id, instance fingerprint. Three digests, no content (R12).

Dictionary rendering, file inventory (``SELECT``/``FD``/JCL DD), lineage and
target-schema DDL are **WP-2.4+**. Nothing here reaches the network, writes to
the tree it reads, or calls a compiler.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any, Dict, Optional, Sequence

from .copybook import resolve
from .layout import (
    COMPILER_BASIS,
    IBM_EQUIVALENCE_LIMITATION,
    LAYOUT_GRADE,
    VERIFIED_AGAINST,
    compute,
    compute_text,
)
from .report import (
    COUNTERSIG_ARTIFACT,
    MANIFEST_ARTIFACT,
    MARKDOWN_ARTIFACT,
    SIGNED_ARTIFACT,
    Report,
    ReportError,
    ReportMeta,
    lint_measured_fields,
    lint_report_text,
    render_markdown,
    tool_digests,
)
from .signing import (
    SigningError,
    build_manifest,
    countersign_request_line,
    input_digests,
    load_or_create_instance_key,
    manifest_bytes,
    sign_manifest,
)

#: Exit codes the report surface uses, named so the CLI, the tests and the
#: documentation cannot drift apart.
EXIT_OK = 0
EXIT_REFUSED = 2


def _layout_command(args: argparse.Namespace) -> int:
    path = Path(args.path)
    if args.root:
        resolution = resolve(Path(args.root))
        layout = compute(path.stem.upper(), resolution, odo_value=args.odo)
        payload = [layout.to_dict()] if layout else []
    else:
        text = path.read_text(encoding="utf-8", errors="replace")
        payload = [
            layout.to_dict()
            for layout in compute_text(text, odo_value=args.odo, origin=path.as_posix())
        ]
    # The limitation is emitted at the TOP of the document as well as on each
    # record. A caveat that only exists one level down is a caveat a reader can
    # scroll past without ever seeing (R9/R11).
    print(json.dumps(
        {
            "source": path.as_posix(),
            "grade": LAYOUT_GRADE,
            "verified_against": COMPILER_BASIS,
            "benchmark": VERIFIED_AGAINST,
            "limitations": [IBM_EQUIVALENCE_LIMITATION],
            "records": payload,
        },
        indent=2, sort_keys=True,
    ))
    return 0 if payload else 1


def _resolve_command(args: argparse.Namespace) -> int:
    resolution = resolve(Path(args.root), [Path(p) for p in args.search_path])
    fan_in = resolution.fan_in()
    fan_out = resolution.fan_out()
    replacing_sites = [d for d in resolution.directives if d.replacing]

    payload = {
        "root": resolution.root,
        # Resolution is dialect-independent -- it reads COPY directives, not
        # storage -- so the IBM-equivalence limitation does not apply here. What
        # DOES apply is that no layout claim is made at all, and saying so is
        # better than leaving a reader to infer it from an absence.
        "scope": (
            "Copybook resolution only. No record layout is computed or claimed "
            "by this command, so no compiler-dialect limitation applies to the "
            "counts below. Use `discovery layout` for layouts, and read its "
            "stated limitations before acting on any offset."
        ),
        "counts": {
            # Every one of these is COUNTED IN THIS RUN. None is transcribed
            # from a previous log: if a run disagrees with the log, the run is
            # authoritative and the log is stale (WP-2.2 §5).
            "source_files_scanned": resolution.files_scanned,
            "files_with_at_least_one_copy": len(
                {d.referrer for d in resolution.directives}
            ),
            "copy_directives": len(resolution.directives),
            "distinct_names_referenced": len(resolution.referenced_names()),
            "resolvable": len(
                [n for n in resolution.referenced_names() if n in resolution.records]
            ),
            "unresolvable": len(resolution.unresolved),
            "edges": len(resolution.edges),
            "copy_replacing_sites": len(replacing_sites),
            "cycles": len(resolution.cycles),
            "members_present_but_unreferenced": len(resolution.unreferenced),
        },
        "max_fan_out": (
            max(((count, name) for name, count in fan_out.items()), default=(0, None))
        ),
        "missing_copybooks": [
            {
                "name": u.name,
                "referenced_by_count": len(u.referenced_by),
                "reference_count": u.reference_count,
                "referenced_by": list(u.referenced_by)[: args.max_referrers],
                "paths_searched_count": len(u.paths_searched),
            }
            for u in sorted(
                resolution.unresolved,
                key=lambda u: (-len(u.referenced_by), u.name),
            )
        ],
        "most_shared": sorted(
            ({"name": n, "referenced_by": c} for n, c in fan_in.items()),
            key=lambda row: (-row["referenced_by"], row["name"]),
        )[: args.top],
        "cycles": [list(c) for c in resolution.cycles],
        "unreferenced": list(resolution.unreferenced),
    }
    print(json.dumps(payload, indent=2, sort_keys=True))
    return 0


def _repo_root() -> Path:
    """The tree this installation was run from, used only for tool digests."""
    return Path(__file__).resolve().parents[2]


def _relian_version() -> str:
    """The version recorded in the report. READ, never typed.

    Installed metadata first, because that is what a customer-perimeter
    install actually has. A source checkout has no metadata, so ``pyproject``
    is read next — that file is what produced the modules whose digests the
    same report records, so it is a measurement of this installation rather
    than a guess about it. If neither answers, the report says so: an
    unknown version is stated, never invented (R1).
    """
    try:
        from importlib.metadata import version

        return version("relian")
    except Exception:                                  # noqa: BLE001
        pass
    try:
        import tomllib

        data = tomllib.loads(
            (_repo_root() / "pyproject.toml").read_text(encoding="utf-8")
        )
        value = data["project"]["version"]
        return f"{value} (from the source checkout's pyproject.toml)"
    except Exception:                                  # noqa: BLE001
        return "unknown (no installed metadata and no readable pyproject.toml)"


def _report_build_command(args: argparse.Namespace) -> int:
    """Build, render, manifest and instance-sign — or fail and write nothing.

    Ordering is deliberate: the key is obtained FIRST, before any file is
    written. A run that cannot sign must not leave a report on disk that a
    reader could mistake for a complete one, and the cheapest way to guarantee
    that is never to have written it.
    """
    out_dir = Path(args.out)
    try:
        instance = load_or_create_instance_key(
            Path(args.home) if args.home else None
        )
    except SigningError as exc:
        print(f"REFUSED: {exc}", file=sys.stderr)
        print(
            "No report was written. Relian does not emit an unsigned report.",
            file=sys.stderr,
        )
        return EXIT_REFUSED

    root = Path(args.root)
    resolution = resolve(root, [Path(p) for p in args.search_path])

    layouts = []
    if not args.skip_layouts:
        for name in sorted(resolution.records):
            layout = compute(name, resolution, odo_value=args.odo)
            if layout is not None:
                layouts.append(layout)

    meta = ReportMeta(
        customer=args.customer,
        engagement=args.engagement,
        contract_vehicle=args.contract_vehicle,
        relian_version=_relian_version(),
        root_label=args.root_label or root.as_posix(),
    )
    try:
        report = Report.build(
            resolution,
            layouts,
            meta,
            repo_root=_repo_root(),
            instance_fingerprint=instance.fingerprint,
            instance_public_key_hex=instance.public_key_hex,
            layouts_skipped_reason=args.skip_layouts,
        )
    except ReportError as exc:
        print(f"REFUSED: {exc}", file=sys.stderr)
        return EXIT_REFUSED

    report_blob = report.to_canonical_json()
    markdown = render_markdown(report)

    # R11 and R1, enforced on the EMITTED bytes rather than on the templates
    # alone. A forbidden term that only reaches the page through an f-string
    # would survive a template-only lint, and a bare number that only appears
    # after rendering would survive a model-only one.
    violations = (
        lint_report_text(report_blob.decode("utf-8"))
        + lint_report_text(markdown)
    )
    if violations:
        print(
            "REFUSED: the generated report contains vocabulary the "
            "quotable-capability matrix does not support (R11):",
            file=sys.stderr,
        )
        for term, excerpt in violations:
            print(f"  {term!r} in …{excerpt}…", file=sys.stderr)
        return EXIT_REFUSED

    ungraded = lint_measured_fields(report.to_dict())
    if ungraded:
        print("REFUSED: a number reached the report without a grade (R1/R9):",
              file=sys.stderr)
        for finding in ungraded:
            print(f"  {finding}", file=sys.stderr)
        return EXIT_REFUSED

    markdown_blob = markdown.encode("utf-8")
    manifest = build_manifest(
        report,
        files=[(SIGNED_ARTIFACT, report_blob), (MARKDOWN_ARTIFACT, markdown_blob)],
        tool_digests=tool_digests(_repo_root()),
        input_digest_rows=input_digests(resolution.records),
        instance=instance,
        relian_version=meta.relian_version,
        python_version=sys.version.split()[0],
    )
    sign_manifest(manifest, instance)

    out_dir.mkdir(parents=True, exist_ok=True)
    (out_dir / SIGNED_ARTIFACT).write_bytes(report_blob)
    (out_dir / MARKDOWN_ARTIFACT).write_bytes(markdown_blob)
    (out_dir / MANIFEST_ARTIFACT).write_bytes(manifest_bytes(manifest))

    print(f"report_id            {report.report_id}")
    print(f"manifest_sha256      {manifest['signature']['manifest_sha256']}")
    print(f"instance_fingerprint {instance.fingerprint}")
    print(f"instance_key         {instance.path} "
          f"({'generated in this run' if instance.generated_this_run else 'existing'})")
    print(f"written              {SIGNED_ARTIFACT}, {MARKDOWN_ARTIFACT}, "
          f"{MANIFEST_ARTIFACT} under {out_dir.as_posix()}")
    print(f"attestation          VALID AND UNATTESTED until "
          f"{COUNTERSIG_ARTIFACT} is added by the operator. The instance "
          f"signature proves integrity and provenance-of-tool, not identity "
          f"of signer; Visionblox has not attested to this report.")
    return EXIT_OK


def _countersign_request_command(args: argparse.Namespace) -> int:
    """Emit the one line that crosses the perimeter, and nothing else."""
    manifest_path = Path(args.report_dir) / MANIFEST_ARTIFACT
    if not manifest_path.is_file():
        print(f"REFUSED: no {MANIFEST_ARTIFACT} at {manifest_path}", file=sys.stderr)
        return EXIT_REFUSED
    manifest: Dict[str, Any] = json.loads(manifest_path.read_text(encoding="utf-8"))
    try:
        print(countersign_request_line(manifest))
    except SigningError as exc:
        print(f"REFUSED: {exc}", file=sys.stderr)
        return EXIT_REFUSED
    return EXIT_OK


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="discovery",
        description="Relian discovery — copybook resolution and record layout, "
                    "computed statically. No compiler is invoked (WP-2.2 D15).",
    )
    sub = parser.add_subparsers(dest="command", required=True)

    layout_p = sub.add_parser("layout", help="compute a record layout as JSON")
    layout_p.add_argument("path", help="a .cpy file")
    layout_p.add_argument("--root", help="resolve nested COPY against this tree")
    layout_p.add_argument("--odo", type=int, default=None,
                          help="OCCURS DEPENDING ON extent to compute at")
    layout_p.set_defaults(func=_layout_command)

    resolve_p = sub.add_parser("resolve", help="copybook fan-in and the missing table")
    resolve_p.add_argument("root", help="the codebase root to scan")
    resolve_p.add_argument("--search-path", action="append", default=[],
                           help="additional copybook directory, repeatable")
    resolve_p.add_argument("--top", type=int, default=10)
    resolve_p.add_argument("--max-referrers", type=int, default=25)
    resolve_p.set_defaults(func=_resolve_command)

    report_p = sub.add_parser("report", help="the signed Data Discovery report")
    report_sub = report_p.add_subparsers(dest="report_command", required=True)

    build_p = report_sub.add_parser(
        "build", help="build, render, manifest and instance-sign a report"
    )
    build_p.add_argument("root", help="the codebase root to scan")
    build_p.add_argument("--out", required=True, help="directory to write into")
    build_p.add_argument("--customer", required=True)
    build_p.add_argument("--engagement", required=True)
    build_p.add_argument("--contract-vehicle", required=True)
    build_p.add_argument("--root-label", default=None,
                         help="how the scanned tree is named in the report")
    build_p.add_argument("--search-path", action="append", default=[])
    build_p.add_argument("--odo", type=int, default=None)
    # A REASON is the argument, not a bare flag. Section 4 may be empty, but it
    # may not be empty silently: an empty section with no stated reason reads
    # as a clean result (R2).
    build_p.add_argument(
        "--skip-layouts", metavar="REASON", default=None,
        help="omit the record-layout section, stating why in the report",
    )
    build_p.add_argument("--home", default=None,
                         help="override the home directory holding the "
                              "instance key (tests and dry runs)")
    build_p.set_defaults(func=_report_build_command)

    request_p = report_sub.add_parser(
        "countersign-request",
        help="print the one line that crosses the customer perimeter",
    )
    request_p.add_argument("--report-dir", required=True)
    request_p.set_defaults(func=_countersign_request_command)
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = build_parser().parse_args(argv)
    return int(args.func(args))


if __name__ == "__main__":       # pragma: no cover - process entry point
    raise SystemExit(main())
