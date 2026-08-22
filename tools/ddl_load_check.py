#!/usr/bin/env python3
"""WP-2.4 §3.4 / D33 — execute generated DDL against a real PostgreSQL and
reconcile ``information_schema`` against the mapping table.

Why this is a separate tool and not a test
------------------------------------------

Generated DDL that has never been executed is a plausible artifact, not a
measured one. This tool does two things, and only the second one is a
measurement of the mapping:

1. **Load.** Every generated statement is executed against a live
   ``postgres:16``. Any error fails the run. This proves the DDL is
   syntactically valid and mutually consistent.

2. **Reconcile.** ``information_schema.columns`` is queried and compared,
   column by column, against what this repository *said* it generated. This
   proves PostgreSQL agrees.

Only the second closes the loop. Verifying the DDL by re-reading the strings we
produced would be the WP-2.2 §0 circularity in a new place — the generator
grading its own homework — and it is exactly the shortcut that would make this
gate decorative. So the comparison here parses PostgreSQL's *own* answer
(``data_type``, ``character_maximum_length``, ``numeric_precision``,
``numeric_scale``) and never re-reads the emitted SQL text.

R12: the corpus is copybook source. No dataset is opened and no customer record
is read; nothing is INSERTed, because the schema is the deliverable and the
data never leaves the customer's perimeter.

Usage::

    python3 tools/ddl_load_check.py --dsn postgresql://…  [--corpus DIR]
                                    [--schema NAME] [--json OUT]
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.discovery import ddl                              # noqa: E402
from src.discovery.layout import compute_text              # noqa: E402

DEFAULT_CORPUS = REPO_ROOT / "discovery-bench" / "corpus"
DEFAULT_SCHEMA = "relian_ddl_check"


# --------------------------------------------------------------------------
# What we claim
# --------------------------------------------------------------------------

_CHAR_RE = re.compile(r"^CHAR\((?P<n>\d+)\)$", re.IGNORECASE)
_NUMERIC_RE = re.compile(
    r"^NUMERIC\((?P<p>\d+),(?P<s>\d+)\)$", re.IGNORECASE
)

#: How PostgreSQL names each type we emit, in ``information_schema.data_type``.
_EXPECTED_DATA_TYPE = {
    "SMALLINT": "smallint",
    "INTEGER": "integer",
    "BIGINT": "bigint",
}


def claimed_shape(pg_type: str) -> Dict[str, object]:
    """Decompose the type this repository claims to have generated.

    Returned as the same fields ``information_schema`` reports, so the
    comparison is like-for-like rather than a string match against our own SQL.
    """
    match = _CHAR_RE.match(pg_type.strip())
    if match:
        return {
            "data_type": "character",
            "character_maximum_length": int(match.group("n")),
            "numeric_precision": None,
            "numeric_scale": None,
        }
    match = _NUMERIC_RE.match(pg_type.strip())
    if match:
        return {
            "data_type": "numeric",
            "character_maximum_length": None,
            "numeric_precision": int(match.group("p")),
            "numeric_scale": int(match.group("s")),
        }
    simple = _EXPECTED_DATA_TYPE.get(pg_type.strip().upper())
    if simple:
        return {
            "data_type": simple,
            "character_maximum_length": None,
            "numeric_precision": None,     # not compared for integer types
            "numeric_scale": None,
        }
    raise SystemExit(
        f"FAIL: {pg_type!r} is emitted by the generator but this tool does not "
        f"know what PostgreSQL should report for it. A type the reconciliation "
        f"cannot check is a type the gate does not cover — add it here "
        f"deliberately rather than letting it pass unchecked."
    )


def build_tables(corpus: Path) -> Tuple[List[ddl.Table], List[str]]:
    """Generate one table per 01 record in every copybook under ``corpus``."""
    tables: List[ddl.Table] = []
    notes: List[str] = []
    members = sorted(
        (p for p in corpus.rglob("*") if p.suffix.lower() in {".cpy", ".cbl"}),
        key=lambda p: p.as_posix(),
    )
    if not members:
        raise SystemExit(f"FAIL: no copybooks found under {corpus}")
    taken: Dict[str, int] = {}
    for member in members:
        text = member.read_text(encoding="utf-8", errors="replace")
        layouts = compute_text(text, origin=member.as_posix())
        if not layouts:
            notes.append(f"{member.as_posix()}: no 01/77 record")
            continue
        for layout in layouts:
            base = ddl.sanitize(f"{member.stem}_{layout.group}")
            if base in taken:
                taken[base] += 1
                base = f"{base}_{taken[base]}"
            else:
                taken[base] = 1
            tables.append(ddl.generate_table(layout, table_name=base))
    return tables, notes


# --------------------------------------------------------------------------
# The run
# --------------------------------------------------------------------------

def split_statements(sql: str) -> List[str]:
    """Split rendered DDL into executable statements, dropping comment lines.

    Comment-only lines are removed rather than sent, so a failure names the
    statement that failed instead of a comment block that happened to precede
    it.
    """
    statements: List[str] = []
    current: List[str] = []
    for line in sql.split("\n"):
        stripped = line.strip()
        if stripped.startswith("--") or not stripped:
            continue
        # Strip trailing inline comments; they are provenance, not SQL.
        code = line.split("  -- ")[0].rstrip()
        if not code.strip():
            continue
        current.append(code)
        if code.rstrip().endswith(";"):
            statements.append("\n".join(current))
            current = []
    if current:
        statements.append("\n".join(current))
    return statements


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dsn", required=True, help="PostgreSQL connection DSN")
    parser.add_argument("--corpus", default=str(DEFAULT_CORPUS), type=Path)
    parser.add_argument("--schema", default=DEFAULT_SCHEMA)
    parser.add_argument("--json", default=None, type=Path)
    args = parser.parse_args(argv)

    import psycopg2                                        # noqa: PLC0415

    print(f"--- generating from {Path(args.corpus).as_posix()} ---")
    tables, notes = build_tables(Path(args.corpus))
    for note in notes:
        print(f"  note: {note}")

    lint = ddl.lint_schema(tables)
    if lint:
        for problem in lint:
            print(f"GATE FAILED (lint): {problem}")
        return 1
    print(f"  lint clean over {len(tables)} table(s)")

    complete = [t for t in tables if t.status is ddl.TableStatus.COMPLETE]
    partial = [t for t in tables if t.status is ddl.TableStatus.PARTIAL]
    print(f"  {len(complete)} COMPLETE, {len(partial)} PARTIAL")

    sql = ddl.render_schema(tables, schema=args.schema)
    statements = split_statements(sql)
    print(f"  {len(statements)} statement(s) to execute")

    # -- 1. LOAD --------------------------------------------------------
    executed = 0
    connection = psycopg2.connect(args.dsn)
    connection.autocommit = True
    try:
        with connection.cursor() as cursor:
            cursor.execute(
                f'DROP SCHEMA IF EXISTS "{args.schema}" CASCADE'
            )
            print("--- executing against PostgreSQL ---")
            for statement in statements:
                try:
                    cursor.execute(statement)
                except Exception as error:                 # noqa: BLE001
                    print("GATE FAILED (load): a generated statement did not "
                          "execute.")
                    print(f"  error: {error}")
                    print("  statement:")
                    for line in statement.split("\n"):
                        print(f"    {line}")
                    return 1
                executed += 1
            print(f"  {executed} statement(s) executed, 0 errors")

            # -- 2. RECONCILE against PostgreSQL's own answer -------------
            cursor.execute(
                """
                SELECT table_name, column_name, data_type,
                       character_maximum_length, numeric_precision,
                       numeric_scale
                  FROM information_schema.columns
                 WHERE table_schema = %s
                 ORDER BY table_name, ordinal_position
                """,
                (args.schema,),
            )
            observed: Dict[Tuple[str, str], Dict[str, object]] = {}
            for row in cursor.fetchall():
                observed[(row[0], row[1])] = {
                    "data_type": row[2],
                    "character_maximum_length": row[3],
                    "numeric_precision": row[4],
                    "numeric_scale": row[5],
                }
    finally:
        connection.close()

    print(f"--- reconciling information_schema ({len(observed)} column(s)) ---")
    problems: List[str] = []
    checked = 0
    for table in tables:
        for column in table.columns:
            key = (table.name, column.name)
            actual = observed.get(key)
            if actual is None:
                problems.append(
                    f"{table.name}.{column.name}: this repository claims to "
                    f"have generated it; PostgreSQL has no such column"
                )
                continue
            expected = claimed_shape(column.pg_type)
            checked += 1
            if actual["data_type"] != expected["data_type"]:
                problems.append(
                    f"{table.name}.{column.name}: claimed {column.pg_type} "
                    f"(data_type {expected['data_type']}), PostgreSQL reports "
                    f"data_type {actual['data_type']}"
                )
                continue
            for attribute in ("character_maximum_length", "numeric_precision",
                              "numeric_scale"):
                want = expected[attribute]
                if want is None:
                    continue
                if actual[attribute] != want:
                    problems.append(
                        f"{table.name}.{column.name}: claimed {column.pg_type} "
                        f"({attribute}={want}), PostgreSQL reports "
                        f"{attribute}={actual[attribute]}"
                    )

    claimed_keys = {(t.name, c.name) for t in tables for c in t.columns}
    for key in sorted(observed):
        if key not in claimed_keys:
            problems.append(
                f"{key[0]}.{key[1]}: PostgreSQL has this column and this "
                f"repository did not claim it"
            )

    result = {
        "tables": len(tables),
        "tables_complete": len(complete),
        "tables_partial": len(partial),
        "statements_executed": executed,
        "columns_claimed": len(claimed_keys),
        "columns_observed": len(observed),
        "columns_reconciled": checked,
        "problems": problems,
        "ruleset": ddl.RULESET_VERSION,
    }
    if args.json:
        Path(args.json).write_text(
            json.dumps(result, indent=2, sort_keys=True), encoding="utf-8"
        )

    if problems:
        for problem in problems:
            print(f"GATE FAILED (reconcile): {problem}")
        return 1

    print(
        f"GATE MET: {executed} statement(s) executed with 0 errors; "
        f"{checked} column(s) reconciled against information_schema; "
        f"{len(observed)} observed, {len(claimed_keys)} claimed, 0 mismatches."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
