"""WP-2.2 D21 — the discovery CLI, deliberately small.

Two surfaces, both read-only, both emitting JSON on stdout:

``python3 -m src.discovery.cli layout <copybook>``
    the computed record layout — the WP-2.2 deliverable.

``python3 -m src.discovery.cli resolve <root>``
    the copybook fan-in graph and the missing-copybook table (D20), which is
    what the CardDemo dry run needs.

Dictionary rendering, file inventory (``SELECT``/``FD``/JCL DD), lineage,
target-schema DDL and the signed report are **WP-2.3+**. Nothing here reaches
the network, writes to the tree it reads, or calls a compiler.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import List, Optional, Sequence

from .copybook import resolve
from .layout import compute, compute_text


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
    print(json.dumps({"source": path.as_posix(), "records": payload},
                     indent=2, sort_keys=True))
    return 0 if payload else 1


def _resolve_command(args: argparse.Namespace) -> int:
    resolution = resolve(Path(args.root), [Path(p) for p in args.search_path])
    fan_in = resolution.fan_in()
    fan_out = resolution.fan_out()
    replacing_sites = [d for d in resolution.directives if d.replacing]

    payload = {
        "root": resolution.root,
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
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = build_parser().parse_args(argv)
    return int(args.func(args))


if __name__ == "__main__":       # pragma: no cover - process entry point
    raise SystemExit(main())
