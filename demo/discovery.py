"""Relian demo — the discovery track.

The transform track (:mod:`demo.pipeline`) answers *does the migration behave
like the original*. It cannot answer the question a migration is actually
scoped on: **what is in the estate, and what does the data look like.** That is
what WP-2.1 → WP-2.4 built, and until this module existed none of it appeared
in the demo — the walkthrough showed a Phase 1 product while the build stood at
Phase 3.

Seven stages, run over one tree, in this order:

  1. **Resolve**    ``COPY`` fan-in and the missing-copybook table.
  2. **Layout**     byte offsets and lengths, computed from source text alone.
  3. **Oracle**     those layouts against RELIAN-DISCOVERY-BENCH's *sealed*
                    GnuCOBOL-derived answer key. Two artifacts that share no
                    code and no process.
  4. **Inventory**  ``SELECT`` ∪ ``FD`` ∪ the job stream's ``DD``, with the
                    computed record length cross-checked against the declared
                    ``LRECL``.
  5. **Lineage**    program→dataset edges, with the bound on their coverage
                    printed next to them rather than in a footnote.
  6. **DDL**        a PostgreSQL target schema, executed against a real server
                    when one is reachable and reported ``NOT MEASURED`` when
                    not.
  7. **Report**     the canonical ``report.json``, its manifest, the Ed25519
                    instance signature — then the *recipient's* verifier run
                    against the result.

WHAT THIS TRACK MAY NOT DO
--------------------------
Every number here is produced by a call into ``src/discovery/`` in this run.
There is no stored expectation anywhere in this file: the counts a reader sees
are recomputed, and a stage that could not run reports why instead of
contributing a zero (R1, R2).

Three specific temptations, each closed deliberately:

* **The oracle is not read until its seal verifies.** All three verifier layers
  and the pinned signer, exactly as ``tests/test_layout_roundtrip.py`` does it.
  An unverified answer key is not an answer key, and "another test checks the
  seal" is how a green-by-skip gets in. A copybook the oracle does not cover is
  counted as *not covered*; it never lands in the numerator.
* **No dataset is opened.** The whole track runs on source text — copybooks,
  COBOL and JCL. That is not a promise made here, it is what
  ``src/discovery/`` is built to do and what
  ``tests/test_discovery_reads_no_data.py`` asserts under an audit hook.
* **The instance key is created inside the demo's workdir**, never in the
  operator's ``~/.relian``. A demo that silently provisions a signing identity
  in someone's home directory is doing something it did not announce.
"""

from __future__ import annotations

import contextlib
import hashlib
import io
import json
import os
import shutil
import subprocess
import sys
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:                              # pragma: no cover
    sys.path.insert(0, str(REPO_ROOT))

from src.assessment.models import Measured                      # noqa: E402
from src.discovery import (                                     # noqa: E402
    LayoutStatus,
    TableStatus,
    build_graph,
    build_inventory,
    compute,
    generate_table,
    lint_schema,
    render_schema,
    resolve,
    scan_programs,
)
from src.discovery import jcl as jcl_mod                        # noqa: E402

# --------------------------------------------------------------------------
# Stage verdicts. The same three-state vocabulary the transform track uses:
# a stage that did not run is NOT a stage that ran and found nothing.
# --------------------------------------------------------------------------

MEASURED = "MEASURED"
NOT_MEASURED = "NOT_MEASURED"
FAILED = "FAILED"

#: The tree the demo discovers. Synthetic, committed, and deliberately not the
#: benchmark corpus: ``examples/demo`` carries copybooks, a job stream and a
#: program with real FILE-CONTROL, which is the shape discovery needs and the
#: flat bench corpus does not have.
DEFAULT_ROOT = REPO_ROOT / "examples" / "demo"

DISCOVERY_BENCH = REPO_ROOT / "discovery-bench"
DISCOVERY_LEDGER = DISCOVERY_BENCH / "LEDGER_relian-discovery-bench-v0.1.json"
DISCOVERY_ORACLE = DISCOVERY_BENCH / "oracle" / "oracle.json"

#: The published signer of RELIAN-DISCOVERY-BENCH v0.1. Pinned rather than read
#: out of the document: a valid signature by the wrong key is the failure a
#: bare verify cannot see, because the key it trusts is the one the document
#: supplied.
BENCH_FINGERPRINT = "233bb4406e2de606"

#: The manifest hash from the seal ceremony of 2026-08-21.
BENCH_MANIFEST_SHA256 = (
    "696397e0d4d865a368735404f8a0db52367ad01a66fcd3d4eb22add06a77f265"
)

#: The published Visionblox release key. The report verifier REQUIRES a pin and
#: has no default, because without one a report re-signed under any key at all
#: passes every internally-consistent check. Nothing here holds the private
#: half (R4) — this is the fingerprint a recipient would pin.
RELEASE_FINGERPRINT = "91e3a404155ba4dd"

#: Set this to a PostgreSQL DSN and stage 6 executes the generated DDL against
#: it. Unset, the stage reports NOT MEASURED and says so — it never reports a
#: schema as loadable on the strength of having generated it.
DDL_DSN_ENV = "RELIAN_DDL_DSN"


def _measured_to_dict(m: Optional[Measured]) -> Optional[Dict[str, Any]]:
    return m.to_dict() if m is not None else None


def _label(path: Path) -> str:
    """Repo-relative posix form where possible (R8), absolute otherwise.

    The absolute path of somebody's checkout is not part of the finding, and
    printing it makes two runs of the same tree look like two different trees.
    """
    try:
        return path.resolve().relative_to(REPO_ROOT).as_posix()
    except ValueError:
        return path.as_posix()


# ==========================================================================
# 1. Resolve
# ==========================================================================

@dataclass(frozen=True)
class ResolveStage:
    status: str
    files_scanned: int = 0
    resolved: Tuple[str, ...] = ()
    unresolved: Tuple[str, ...] = ()
    copy_edges: int = 0
    cycles: int = 0
    reason: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "status": self.status,
            "files_scanned": self.files_scanned,
            "resolved": list(self.resolved),
            "unresolved": list(self.unresolved),
            "copy_edges": self.copy_edges,
            "cycles": self.cycles,
            "reason": self.reason,
        }


# ==========================================================================
# 2. Layout
# ==========================================================================

@dataclass(frozen=True)
class LayoutRow:
    copybook: str
    record: str
    status: str
    group_length: Optional[int]
    fields: int

    def to_dict(self) -> Dict[str, Any]:
        return {
            "copybook": self.copybook,
            "record": self.record,
            "status": self.status,
            "group_length": self.group_length,
            "fields": self.fields,
        }


# ==========================================================================
# 3. The sealed oracle
# ==========================================================================

@dataclass(frozen=True)
class OracleCheck:
    """The layout engine against the sealed answer key.

    ``agreement`` is a rate over comparisons that were actually made. A
    copybook the oracle does not cover contributes to :attr:`not_covered` and
    to neither side of that rate — the alternative is a denominator that
    silently shrinks until any engine looks perfect.
    """

    status: str
    seal_ok: bool = False
    seal_layers: Tuple[str, ...] = ()
    failed_layers: Tuple[str, ...] = ()
    comparisons: int = 0
    agreed: int = 0
    not_covered: Tuple[str, ...] = ()
    digest_mismatches: Tuple[str, ...] = ()
    mismatches: Tuple[str, ...] = ()
    agreement: Optional[Measured] = None
    reason: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "status": self.status,
            "seal_ok": self.seal_ok,
            "seal_layers": list(self.seal_layers),
            "failed_layers": list(self.failed_layers),
            "comparisons": self.comparisons,
            "agreed": self.agreed,
            "not_covered": list(self.not_covered),
            "digest_mismatches": list(self.digest_mismatches),
            "mismatches": list(self.mismatches),
            "agreement": _measured_to_dict(self.agreement),
            "reason": self.reason,
        }


# ==========================================================================
# 4. File inventory
# ==========================================================================

@dataclass(frozen=True)
class InventoryRow:
    program: Optional[str]
    file_name: Optional[str]
    ddname: Optional[str]
    dataset: Optional[str]
    outcome: Optional[str]
    computed_length: Optional[int]
    declared_lrecl: Optional[int]

    def to_dict(self) -> Dict[str, Any]:
        return {
            "program": self.program,
            "file_name": self.file_name,
            "ddname": self.ddname,
            "dataset": self.dataset,
            "outcome": self.outcome,
            "computed_length": self.computed_length,
            "declared_lrecl": self.declared_lrecl,
        }


@dataclass(frozen=True)
class InventoryStage:
    status: str
    programs_scanned: int = 0
    programs_with_file_control: int = 0
    jcl_members: int = 0
    dd_statements: int = 0
    dd_with_lrecl: int = 0
    outcomes: Dict[str, int] = field(default_factory=dict)
    rows: Tuple[InventoryRow, ...] = ()
    findings: Tuple[Tuple[str, str], ...] = ()
    reason: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "status": self.status,
            "programs_scanned": self.programs_scanned,
            "programs_with_file_control": self.programs_with_file_control,
            "jcl_members": self.jcl_members,
            "dd_statements": self.dd_statements,
            "dd_with_lrecl": self.dd_with_lrecl,
            "outcomes": dict(self.outcomes),
            "rows": [r.to_dict() for r in self.rows],
            "findings": [{"kind": k, "detail": d} for k, d in self.findings],
            "reason": self.reason,
        }


# ==========================================================================
# 5. Lineage
# ==========================================================================

@dataclass(frozen=True)
class LineageStage:
    status: str
    edges: Tuple[Tuple[str, str, str], ...] = ()
    datasets: Tuple[str, ...] = ()
    coverage_statement: Optional[str] = None
    reason: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "status": self.status,
            "edges": [
                {"program": p, "direction": d, "dataset": ds}
                for p, d, ds in self.edges
            ],
            "datasets": list(self.datasets),
            "coverage_statement": self.coverage_statement,
            "reason": self.reason,
        }


# ==========================================================================
# 6. Target-schema DDL
# ==========================================================================

@dataclass(frozen=True)
class DdlStage:
    status: str
    tables: int = 0
    tables_complete: int = 0
    tables_partial: int = 0
    columns: int = 0
    lint_problems: Tuple[str, ...] = ()
    statements: int = 0
    #: Execution against a real server. ``None`` until one is reached — never
    #: 0-with-no-errors, which is what a generated-but-unexecuted schema would
    #: report if this field defaulted.
    executed_status: str = NOT_MEASURED
    executed_reason: Optional[str] = None
    statements_executed: Optional[int] = None
    columns_reconciled: Optional[int] = None
    execution_problems: Tuple[str, ...] = ()

    def to_dict(self) -> Dict[str, Any]:
        return {
            "status": self.status,
            "tables": self.tables,
            "tables_complete": self.tables_complete,
            "tables_partial": self.tables_partial,
            "columns": self.columns,
            "lint_problems": list(self.lint_problems),
            "statements": self.statements,
            "execution": {
                "status": self.executed_status,
                "reason": self.executed_reason,
                "statements_executed": self.statements_executed,
                "columns_reconciled": self.columns_reconciled,
                "problems": list(self.execution_problems),
            },
        }


# ==========================================================================
# 7. The signed report, and the recipient's verifier
# ==========================================================================

@dataclass(frozen=True)
class ReportStage:
    status: str
    report_id: Optional[str] = None
    manifest_sha256: Optional[str] = None
    instance_fingerprint: Optional[str] = None
    artifacts: Tuple[str, ...] = ()
    verify_status: Optional[str] = None
    #: ``(layer, verdict, detail)``. The verdict is the verifier's own three-state
    #: word — ``PASS`` / ``FAIL`` / ``ABSENT`` — not a boolean. A missing
    #: countersignature is absent, not failed, and collapsing the two would turn
    #: the expected state of every report Relian has ever produced into a red row.
    verify_layers: Tuple[Tuple[str, str, str], ...] = ()
    verifier_sha256: Optional[str] = None
    countersign_request: Optional[str] = None
    #: Byte-reproducibility of the FINDINGS. The manifest is deliberately
    #: excluded: it timestamps, so it cannot be byte-stable and does not claim
    #: to be.
    report_reproducible: Optional[bool] = None
    manifest_differs_reason: Optional[str] = None
    reason: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "status": self.status,
            "report_id": self.report_id,
            "manifest_sha256": self.manifest_sha256,
            "instance_fingerprint": self.instance_fingerprint,
            "artifacts": list(self.artifacts),
            "verify_status": self.verify_status,
            "verify_layers": [
                {"layer": name, "verdict": verdict, "detail": detail}
                for name, verdict, detail in self.verify_layers
            ],
            "verifier_sha256": self.verifier_sha256,
            "countersign_request": self.countersign_request,
            "report_reproducible": self.report_reproducible,
            "manifest_differs_reason": self.manifest_differs_reason,
            "reason": self.reason,
        }


# ==========================================================================
# The whole track
# ==========================================================================

@dataclass(frozen=True)
class DiscoveryResult:
    root: str
    resolve: ResolveStage
    layouts: Tuple[LayoutRow, ...]
    oracle: OracleCheck
    inventory: InventoryStage
    lineage: LineageStage
    ddl: DdlStage
    report: ReportStage
    wall_seconds: float = 0.0

    def failures(self) -> List[str]:
        """Defects in Relian — not findings about the tree it was pointed at.

        The distinction is the whole point of the track. ``NO_LRECL``,
        ``NO_LAYOUT`` and even ``DISAGREE`` are *discoveries*: the copybook and
        the job stream disagree, and reporting that is the product working. A
        layout that contradicts the sealed oracle, a seal that will not verify,
        DDL that fails its own lint, or a report the shipped verifier rejects
        are defects, and only those reach the exit status.
        """
        bad: List[str] = []
        if self.oracle.status == FAILED:
            bad.append(f"oracle cross-check: {self.oracle.reason}")
        if self.oracle.mismatches:
            bad.append(
                f"{len(self.oracle.mismatches)} layout row(s) disagree with the "
                f"sealed oracle"
            )
        if self.oracle.digest_mismatches:
            bad.append(
                "a copybook on disk does not match the digest the oracle "
                "measured, so a comparison would be about two different files"
            )
        if self.ddl.lint_problems:
            bad.append(f"{len(self.ddl.lint_problems)} DDL lint violation(s)")
        if self.ddl.execution_problems:
            bad.append(
                f"{len(self.ddl.execution_problems)} DDL execution/reconciliation "
                f"problem(s)"
            )
        if self.report.status == FAILED:
            bad.append(f"signed report: {self.report.reason}")
        # UNATTESTED is the correct status, not a failure: no countersignature
        # has ever been issued and the demo does not ship a simulated one (R5).
        if self.report.verify_status not in (None, "VALID AND UNATTESTED",
                                             "VALID AND ATTESTED"):
            bad.append(f"report verification: {self.report.verify_status}")
        if self.report.report_reproducible is False:
            bad.append("two runs over the same tree produced different report.json")
        return bad

    def to_dict(self) -> Dict[str, Any]:
        return {
            "root": self.root,
            "resolve": self.resolve.to_dict(),
            "layouts": [row.to_dict() for row in self.layouts],
            "oracle": self.oracle.to_dict(),
            "inventory": self.inventory.to_dict(),
            "lineage": self.lineage.to_dict(),
            "ddl": self.ddl.to_dict(),
            "report": self.report.to_dict(),
            "failures": self.failures(),
            "wall_seconds": round(self.wall_seconds, 2),
        }


# --------------------------------------------------------------------------
# Stages
# --------------------------------------------------------------------------

def _run_resolve(root: Path) -> Tuple[ResolveStage, Any]:
    resolution = resolve(root, [])
    stage = ResolveStage(
        status=MEASURED,
        files_scanned=resolution.files_scanned,
        resolved=tuple(sorted(resolution.records)),
        unresolved=tuple(sorted({u.name for u in resolution.unresolved})),
        copy_edges=len(resolution.edges),
        cycles=len(resolution.cycles),
    )
    return stage, resolution


def _run_layouts(resolution: Any) -> Tuple[Tuple[LayoutRow, ...], Dict[str, Any]]:
    rows: List[LayoutRow] = []
    layouts: Dict[str, Any] = {}
    for name in sorted(resolution.records):
        layout = compute(name, resolution)
        if layout is None:
            # Not an error and not a zero: the copybook resolved and carries no
            # 01/77 record. Saying so is the honest row.
            rows.append(LayoutRow(name, "—", "NO_RECORD", None, 0))
            continue
        layouts[name] = layout
        rows.append(
            LayoutRow(
                copybook=name,
                record=layout.group,
                status=(layout.status.value
                        if isinstance(layout.status, LayoutStatus)
                        else str(layout.status)),
                group_length=layout.group_length,
                fields=len(layout.fields),
            )
        )
    return tuple(rows), layouts


def _trusted_oracle() -> Dict[str, Any]:
    """The oracle document, returned ONLY if all three layers and both pins
    hold. There is deliberately no path that reads a row without this."""
    from tools.verify_manifest import verify                   # noqa: PLC0415

    report = verify(
        DISCOVERY_LEDGER, from_manifest=True, key_fingerprint=BENCH_FINGERPRINT
    )
    if not report.ok:
        raise ValueError(
            "the discovery-bench seal did NOT verify, so no oracle row may be "
            f"trusted. Failed layers: {report.failed_layers()}"
        )
    ledger = json.loads(DISCOVERY_LEDGER.read_text(encoding="utf-8"))
    signature = ledger["signature"]
    if signature["key_fingerprint"] != BENCH_FINGERPRINT:
        raise ValueError(
            f"the ledger is signed by {signature['key_fingerprint']}, not the "
            f"pinned {BENCH_FINGERPRINT}"
        )
    if signature["manifest_sha256"] != BENCH_MANIFEST_SHA256:
        raise ValueError(
            "the ledger's manifest hash is not the published one: "
            f"{signature['manifest_sha256']} vs {BENCH_MANIFEST_SHA256}"
        )
    digest = hashlib.sha256(DISCOVERY_ORACLE.read_bytes()).hexdigest()
    sealed = {entry["path"]: entry["sha256"] for entry in ledger["files"]}
    if sealed.get("oracle/oracle.json") != digest:
        raise ValueError(
            "oracle/oracle.json does not match the digest inside the signed "
            f"manifest: on disk {digest}, sealed {sealed.get('oracle/oracle.json')}"
        )
    return json.loads(DISCOVERY_ORACLE.read_text(encoding="utf-8"))


def _run_oracle_check(root: Path, resolution: Any,
                      layouts: Dict[str, Any]) -> OracleCheck:
    """Compare this run's layouts against the sealed GnuCOBOL answer key.

    The engine never invokes a compiler and the oracle is nothing but a
    compiler's own byte layout, so agreement here is two independent artifacts
    reaching the same numbers — not a tool grading itself.
    """
    from tools.verify_manifest import verify                   # noqa: PLC0415

    seal = verify(
        DISCOVERY_LEDGER, from_manifest=True, key_fingerprint=BENCH_FINGERPRINT
    )
    seal_layers = tuple(layer.name for layer in seal.layers)

    try:
        oracle = _trusted_oracle()
    except (ValueError, OSError, KeyError, json.JSONDecodeError) as exc:
        return OracleCheck(
            status=FAILED,
            seal_ok=bool(seal.ok),
            seal_layers=seal_layers,
            failed_layers=tuple(seal.failed_layers()),
            reason=str(exc),
        )

    by_file = {cb["file"]: cb for cb in oracle["copybooks"]}
    comparisons = agreed = 0
    not_covered: List[str] = []
    digest_mismatches: List[str] = []
    mismatches: List[str] = []

    for name in sorted(layouts):
        entry = by_file.get(f"{name}.cpy")
        if entry is None:
            not_covered.append(name)
            continue

        # The engine must have read the same bytes the oracle measured. If the
        # copy under examples/ had drifted from the sealed corpus, agreement
        # would be a statement about two different files.
        source_path = root / resolution.records[name].path
        on_disk = hashlib.sha256(source_path.read_bytes()).hexdigest()
        if on_disk != entry["sha256"]:
            digest_mismatches.append(
                f"{name}: {source_path.as_posix()} is {on_disk[:16]}…, the "
                f"oracle measured {str(entry['sha256'])[:16]}…"
            )
            continue

        layout = layouts[name]
        rows = layout.by_key()
        for variant in entry["variants"]:
            for row in variant["fields"]:
                comparisons += 1
                field_row = rows.get(row["key"])
                if (field_row is not None
                        and field_row.offset == row["offset"]
                        and field_row.length == row["length"]):
                    agreed += 1
                else:
                    observed = (
                        f"offset {field_row.offset} length {field_row.length}"
                        if field_row is not None
                        else "ABSENT — the engine emitted no such row"
                    )
                    mismatches.append(
                        f"{name}/{row['key']}: oracle offset {row['offset']} "
                        f"length {row['length']} ({row['source']}), engine {observed}"
                    )
            comparisons += 1
            if layout.group_length == variant["group_length"]:
                agreed += 1
            else:
                mismatches.append(
                    f"{name}/{entry['group']} (group length): oracle "
                    f"{variant['group_length']}, engine {layout.group_length}"
                )

    if not comparisons:
        return OracleCheck(
            status=NOT_MEASURED,
            seal_ok=bool(seal.ok),
            seal_layers=seal_layers,
            not_covered=tuple(not_covered),
            digest_mismatches=tuple(digest_mismatches),
            reason=(
                "no copybook in this tree is covered by RELIAN-DISCOVERY-BENCH "
                "v0.1, so there was nothing to compare against"
            ),
        )

    agreement = Measured(
        value=round(agreed / comparisons, 4),
        provenance=(
            f"{comparisons} offset/length comparison(s) over "
            f"{len(layouts) - len(not_covered)} copybook(s) against the sealed "
            f"RELIAN-DISCOVERY-BENCH v0.1 oracle (GnuCOBOL 3.1.2.0, signer "
            f"{BENCH_FINGERPRINT}), tolerance zero, this run"
        ),
        grade="VERIFIED",
    )
    return OracleCheck(
        status=MEASURED if not mismatches else FAILED,
        seal_ok=bool(seal.ok),
        seal_layers=seal_layers,
        failed_layers=tuple(seal.failed_layers()),
        comparisons=comparisons,
        agreed=agreed,
        not_covered=tuple(not_covered),
        digest_mismatches=tuple(digest_mismatches),
        mismatches=tuple(mismatches),
        agreement=agreement,
        reason=(None if not mismatches
                else f"{len(mismatches)} row(s) disagree with the sealed oracle"),
    )


def _run_inventory(root: Path, resolution: Any) -> Tuple[InventoryStage, Any, Any]:
    programs = scan_programs(root)
    jcl_inventory = jcl_mod.scan(root)
    inventory = build_inventory(programs, jcl_inventory, resolution)
    counts = jcl_inventory.counts()

    rows = tuple(
        InventoryRow(
            program=record.program,
            file_name=record.file_name,
            ddname=getattr(record.dd, "ddname", None),
            dataset=getattr(record.dd, "dataset", None),
            outcome=(record.check.outcome.value if record.check else None),
            computed_length=(record.check.computed_length if record.check else None),
            declared_lrecl=(record.check.declared_lrecl if record.check else None),
        )
        for record in inventory.records
    )
    stage = InventoryStage(
        status=MEASURED,
        programs_scanned=inventory.programs_scanned,
        programs_with_file_control=inventory.programs_with_file_control,
        jcl_members=counts["members"],
        dd_statements=counts["dd_statements"],
        dd_with_lrecl=counts["dd_with_lrecl"],
        outcomes=inventory.outcomes(),
        rows=rows,
        findings=tuple(
            (finding.kind.value, finding.detail) for finding in inventory.findings()
        ),
    )
    return stage, programs, inventory


def _run_lineage(programs: Sequence[Any], inventory: Any) -> LineageStage:
    graph = build_graph(programs, inventory)
    return LineageStage(
        status=MEASURED,
        edges=tuple(
            (edge.program, edge.direction.value, edge.dataset) for edge in graph.edges
        ),
        datasets=graph.datasets(),
        coverage_statement=graph.coverage.statement(),
    )


def _run_ddl(layouts: Dict[str, Any], root: Path, workdir: Path) -> DdlStage:
    tables = [generate_table(layouts[name]) for name in sorted(layouts)]
    if not tables:
        return DdlStage(
            status=NOT_MEASURED,
            executed_reason="no record layout was computed, so no table was generated",
        )

    # The statement count comes from the SHIPPED splitter, not from a second
    # one written here. Two splitters would drift, and the demo would print a
    # number the tool that executes the schema disagrees with.
    from tools.ddl_load_check import split_statements          # noqa: PLC0415

    problems = lint_schema(tables)
    sql = render_schema(tables, schema="relian_demo")
    statements = split_statements(sql)
    (workdir / "target-schema.sql").write_text(sql, encoding="utf-8")

    stage = DdlStage(
        status=MEASURED if not problems else FAILED,
        tables=len(tables),
        tables_complete=len([t for t in tables if t.status is TableStatus.COMPLETE]),
        tables_partial=len([t for t in tables if t.status is TableStatus.PARTIAL]),
        columns=sum(len(t.columns) for t in tables),
        lint_problems=tuple(problems),
        statements=len(statements),
    )

    dsn = os.environ.get(DDL_DSN_ENV)
    if not dsn:
        return _replace_execution(
            stage,
            reason=(
                f"no {DDL_DSN_ENV} is set, so the generated schema was not "
                f"executed. Generating DDL is not evidence that it loads — CI "
                f"runs this against a real postgres:16 and the demo will not "
                f"claim the result of a run that did not happen"
            ),
        )
    try:
        import psycopg2                                        # noqa: F401,PLC0415
    except ImportError as exc:
        return _replace_execution(
            stage, reason=f"{DDL_DSN_ENV} is set but psycopg2 is not installed ({exc})"
        )

    json_path = workdir / "ddl-load-report.json"
    proc = subprocess.run(
        [sys.executable, str(REPO_ROOT / "tools" / "ddl_load_check.py"),
         "--dsn", dsn,
         "--corpus", str(root / "copy"),
         "--schema", "relian_demo_check",
         "--json", str(json_path)],
        capture_output=True, text=True, cwd=str(REPO_ROOT), timeout=300,
    )
    if not json_path.is_file():
        tail = (proc.stderr or proc.stdout or "").strip().splitlines()
        return _replace_execution(
            stage,
            reason=(
                f"the load check did not complete against {DDL_DSN_ENV} "
                f"(exit {proc.returncode}): "
                + (tail[-1] if tail else "no output")
            ),
        )
    result = json.loads(json_path.read_text(encoding="utf-8"))
    execution_problems = tuple(result.get("problems", ()))
    return DdlStage(
        status=FAILED if (problems or execution_problems) else MEASURED,
        tables=stage.tables,
        tables_complete=stage.tables_complete,
        tables_partial=stage.tables_partial,
        columns=stage.columns,
        lint_problems=stage.lint_problems,
        statements=stage.statements,
        executed_status=MEASURED if proc.returncode == 0 else FAILED,
        executed_reason=(
            f"executed against {DDL_DSN_ENV} and reconciled against "
            f"information_schema in this run"
        ),
        statements_executed=result.get("statements_executed"),
        columns_reconciled=result.get("columns_reconciled"),
        execution_problems=execution_problems,
    )


def _replace_execution(stage: DdlStage, *, reason: str) -> DdlStage:
    return DdlStage(
        status=stage.status,
        tables=stage.tables,
        tables_complete=stage.tables_complete,
        tables_partial=stage.tables_partial,
        columns=stage.columns,
        lint_problems=stage.lint_problems,
        statements=stage.statements,
        executed_status=NOT_MEASURED,
        executed_reason=reason,
    )


def _build_report(root: Path, out_dir: Path, home: Path) -> Tuple[int, str]:
    """Invoke the SHIPPED command, not a reimplementation of it."""
    from src.discovery.cli import main as discovery_main       # noqa: PLC0415

    buffer = io.StringIO()
    with contextlib.redirect_stdout(buffer), contextlib.redirect_stderr(buffer):
        code = discovery_main([
            "report", "build", str(root),
            "--out", str(out_dir),
            "--customer", "Meridian Municipal Utility District",
            "--engagement", "Relian demonstration run",
            "--contract-vehicle", "none — demonstration",
            "--root-label", root.name,
            "--home", str(home),
        ])
    return code, buffer.getvalue()


def _run_report(root: Path, workdir: Path) -> ReportStage:
    from src.discovery.cli import main as discovery_main       # noqa: PLC0415
    from tools.verify_report import verify as verify_report    # noqa: PLC0415

    out_dir = workdir / "report"
    # The instance key lives under the demo's own workdir. A demo that
    # provisioned a signing identity in the operator's ~/.relian would be
    # taking an action it never announced.
    home = workdir / "instance-home"
    code, output = _build_report(root, out_dir, home)
    if code != 0:
        return ReportStage(
            status=FAILED,
            reason=(f"`report build` exited {code}: "
                    + (output.strip().splitlines() or ["no output"])[-1]),
        )

    manifest = json.loads((out_dir / "report.manifest.json").read_text(encoding="utf-8"))
    report_json = json.loads((out_dir / "report.json").read_text(encoding="utf-8"))

    verdict = verify_report(out_dir, RELEASE_FINGERPRINT)
    layers = tuple(
        (str(layer["layer"]), str(layer["verdict"]), str(layer.get("detail", "")))
        for layer in verdict["layers"]
    )

    request = io.StringIO()
    with contextlib.redirect_stdout(request):
        discovery_main(["report", "countersign-request", "--report-dir", str(out_dir)])

    # Reproducibility, measured rather than asserted: build a second time with
    # the SAME instance key and compare the bytes of the findings.
    second = workdir / "report-rerun"
    rebuild_code, _ = _build_report(root, second, home)
    reproducible: Optional[bool] = None
    if rebuild_code == 0:
        reproducible = (
            hashlib.sha256((out_dir / "report.json").read_bytes()).hexdigest()
            == hashlib.sha256((second / "report.json").read_bytes()).hexdigest()
        )
    shutil.rmtree(second, ignore_errors=True)

    return ReportStage(
        status=MEASURED,
        report_id=report_json.get("report_id"),
        manifest_sha256=manifest["signature"]["manifest_sha256"],
        instance_fingerprint=manifest["instance_key"]["fingerprint"],
        artifacts=tuple(sorted(p.name for p in out_dir.iterdir())),
        verify_status=verdict["status"],
        verify_layers=layers,
        verifier_sha256=verdict["verifier_sha256"],
        countersign_request=request.getvalue().strip() or None,
        report_reproducible=reproducible,
        manifest_differs_reason=(
            "report.json is byte-identical across runs; report.manifest.json is "
            "not, and does not claim to be — it records generated_at and "
            "signed_at, which are properties of the run rather than of the "
            "findings"
        ),
    )


# --------------------------------------------------------------------------
# The runner
# --------------------------------------------------------------------------

def run(root: Path, workdir: Path, on_stage=None) -> DiscoveryResult:
    """Run all seven stages over ``root``. Never raises for a finding."""
    say = on_stage or (lambda *_a, **_k: None)
    started = time.time()
    workdir.mkdir(parents=True, exist_ok=True)

    if not root.is_dir():
        empty = DiscoveryResult(
            root=_label(root),
            resolve=ResolveStage(status=NOT_MEASURED,
                                 reason=f"{root.as_posix()} is not a directory"),
            layouts=(),
            oracle=OracleCheck(status=NOT_MEASURED, reason="no tree to discover"),
            inventory=InventoryStage(status=NOT_MEASURED, reason="no tree to discover"),
            lineage=LineageStage(status=NOT_MEASURED, reason="no tree to discover"),
            ddl=DdlStage(status=NOT_MEASURED,
                         executed_reason="no tree to discover"),
            report=ReportStage(status=NOT_MEASURED, reason="no tree to discover"),
            wall_seconds=time.time() - started,
        )
        return empty

    say("resolve")
    resolve_stage, resolution = _run_resolve(root)

    say("layout")
    layout_rows, layouts = _run_layouts(resolution)

    say("oracle")
    oracle_check = _run_oracle_check(root, resolution, layouts)

    say("inventory")
    inventory_stage, programs, inventory = _run_inventory(root, resolution)

    say("lineage")
    lineage_stage = _run_lineage(programs, inventory)

    say("ddl")
    ddl_stage = _run_ddl(layouts, root, workdir)

    say("report")
    report_stage = _run_report(root, workdir)

    return DiscoveryResult(
        root=_label(root),
        resolve=resolve_stage,
        layouts=layout_rows,
        oracle=oracle_check,
        inventory=inventory_stage,
        lineage=lineage_stage,
        ddl=ddl_stage,
        report=report_stage,
        wall_seconds=time.time() - started,
    )
