"""Relian end-to-end demo.

    python3 -m demo                      # everything
    python3 -m demo --case P01 --case CUSTUPD
    python3 -m demo --inputs 3           # fewer inputs per program, faster
    python3 -m demo --discovery-only     # the estate, without the migration
    python3 -m demo --html out/relian.html

Two tracks, both over real COBOL and both measured in the run that prints them.

**Transform** runs the shipped assessment engine and the shipped transpiler,
builds the original and the migration, and executes them against each other.

**Discovery** runs the shipped copybook resolver, layout engine, file inventory,
lineage graph, target-schema generator and signed-report surface over a tree
with copybooks and a job stream — the question a migration is actually scoped
on, which behavioural equivalence cannot answer.

Offline: nothing here opens a socket or calls a model, and no dataset is
opened (R6, R12 — customer source never leaves the perimeter).
"""

from __future__ import annotations

import argparse
import json
import sys
import tempfile
from pathlib import Path
from typing import List, Optional

from . import cases as cases_mod
from . import discovery as discovery_mod
from . import oracle, pipeline, report
from .pipeline import (
    BUILD_FAILED,
    DIVERGENCE_MEASURED,
    EQUIVALENCE_MEASURED,
    GAMING_TRIPPED,
    INCOMPLETE_MEASUREMENT,
    NOT_MEASURED,
    REFUSED_UNSUPPORTED,
    TRANSPILE_CRASHED,
    CaseResult,
    RunResult,
)

# ANSI, suppressed when stdout is not a terminal or --no-color is passed.
class Style:
    def __init__(self, enabled: bool) -> None:
        self.on = enabled

    def _w(self, code: str, s: str) -> str:
        return f"\033[{code}m{s}\033[0m" if self.on else s

    def bold(self, s: str) -> str:   return self._w("1", s)
    def dim(self, s: str) -> str:    return self._w("2", s)
    def green(self, s: str) -> str:  return self._w("32", s)
    def red(self, s: str) -> str:    return self._w("31", s)
    def yellow(self, s: str) -> str: return self._w("33", s)
    def cyan(self, s: str) -> str:   return self._w("36", s)


VERDICT_STYLE = {
    EQUIVALENCE_MEASURED: ("green", "EQUIVALENCE MEASURED"),
    DIVERGENCE_MEASURED: ("red", "DIVERGENCE MEASURED"),
    REFUSED_UNSUPPORTED: ("yellow", "REFUSED — UNSUPPORTED"),
    TRANSPILE_CRASHED: ("red", "TRANSPILE CRASHED"),
    BUILD_FAILED: ("red", "BUILD FAILED"),
    NOT_MEASURED: ("yellow", "NOT MEASURED"),
    INCOMPLETE_MEASUREMENT: ("yellow", "INCOMPLETE MEASUREMENT"),
    GAMING_TRIPPED: ("red", "INVALID — GAMING CONTROLS"),
}


def _fmt_measured(m, digits: int = 4) -> str:
    return "not measured" if m is None else f"{m.value:.{digits}f} [{m.grade}]"


def _banner(st: Style, run_oracle: oracle.OracleInfo, n_cases: int, *,
            transform: bool = True, discovery: bool = True) -> None:
    print()
    print(st.bold("  RELIAN — verifiable COBOL→Java migration"))
    if transform:
        print(st.dim("  transform : assess → transpile → build both → execute both "
                     "→ compare"))
    if discovery:
        print(st.dim("  discovery : resolve → layout → sealed oracle → file inventory "
                     "→ lineage → DDL → signed report"))
    print()
    if transform:
        print(f"  legacy oracle    : {run_oracle.label}"
              if run_oracle.available else
              st.yellow("  legacy oracle    : NOT AVAILABLE — equivalence cannot "
                        "be measured"))
        print(f"  programs         : {n_cases}")
    print(st.dim("  every number below was produced by an execution in this run;"))
    print(st.dim("  anything not measured is reported as such, never estimated."))
    print()


def _print_case(st: Style, r: CaseResult) -> None:
    colour, label = VERDICT_STYLE[r.verdict]
    paint = getattr(st, colour)
    print(f"  {st.bold(r.case_id):<34} {paint(label)}")

    a = r.assess
    if a is not None:
        cov = ("not measured" if a.coverage_ratio is None
               else f"{a.coverage_ratio.value:.4f} [{a.coverage_ratio.grade}]")
        print(st.dim(f"      assessed   : {a.total_statements} statements, "
                     f"{a.physical_loc} LOC, cyclomatic {a.cyclomatic}, "
                     f"risk {a.risk_tier}"))
        print(st.dim(f"      C1 coverage: {cov} "
                     f"({a.supported_statements}/{a.total_statements} statements)"))

    if r.verdict in (REFUSED_UNSUPPORTED, TRANSPILE_CRASHED):
        if r.refused_verb:
            where = f"line {r.refused_line}"
            if r.refused_paragraph:
                where += f", paragraph {r.refused_paragraph}"
            print(st.yellow(f"      refused    : {r.refused_verb!r} at {where}"))
        elif r.transpile_error:
            print(st.red(f"      crashed    : {r.transpile_error}"))
            print(st.dim("      this is a DEFECT in the diagnosis path — the engine "
                         "should have named the construct and refused."))
        if a is not None and a.unsupported_ranked:
            inv = ", ".join(f"{v}×{c}" for v, c in a.unsupported_ranked)
            print(st.dim(f"      inventory  : {inv}"))
        print(st.dim("      no Java was emitted and no attestation was issued."))
        if r.prediction_confirmed:
            print(st.dim("      cross-check: the read-only assessment flagged this "
                         "before the transpiler ran."))
        elif r.prediction_confirmed is False:
            print(st.red("      cross-check: the assessment predicted FULL support and "
                         "was WRONG."))
            print(st.dim("      coverage classifies by bare verb, so it over-reports "
                         "where only some forms of a verb are supported."))
        print()
        return

    if r.java_sha256:
        det = ("reproducible" if r.deterministic else
               st.red("NOT reproducible"))
        print(st.dim(f"      transpiled : sha256 {r.java_sha256[:16]}… ({det})"))

    if r.java_build_ok is not None:
        jb = "ok" if r.java_build_ok else "failed"
        cb = ("ok" if r.cobol_build_ok else
              ("failed" if r.cobol_build_ok is False else "not attempted"))
        line = st.dim(f"      built      : javac {jb}, cobc {cb}")
        print(line if r.java_build_ok and r.cobol_build_ok else st.red(line))

    if r.ber is not None:
        rate = f"{r.ber.value:.4f}"
        painted = st.green(rate) if r.ber.value == 1.0 else st.red(rate)
        print(f"      {st.dim('equivalence:')} {painted} "
              f"{st.dim(f'({r.vectors_equivalent}/{r.vectors_executed} executed inputs, stdout + exit code)')}")
    if r.vectors_absent:
        print(st.yellow(f"      not run    : {r.vectors_absent} of {r.vectors_total} "
                        f"input(s) could not be executed on both sides"))
        print(st.dim("      these are absent from the rate above — not counted as "
                     "divergences."))
    if r.oracle_agreement is not None:
        print(st.dim(f"      oracle x-ck: {r.oracle_agreement.value:.4f} vs the "
                     f"sealed public vectors"))

    for o in r.vectors:
        if not o.executed:
            print(st.yellow(f"      not run    : input {o.stdin!r} — "
                            f"cobol {'ok' if o.cobol_stdout is not None else 'no result'}, "
                            f"java {'ok' if o.java_stdout is not None else 'no result'}"))
        elif not o.equivalent:
            print(st.red(f"      diverged   : input {o.stdin!r}"))
            print(st.dim(f"          cobol → {o.cobol_stdout!r} (exit {o.cobol_exit})"))
            print(st.dim(f"          java  → {o.java_stdout!r} (exit {o.java_exit})"))

    gate = (st.green("PASSED") if r.attestation_gate == "PASSED"
            else st.yellow("BLOCKED"))
    print(f"      {st.dim('attestation:')} {gate} {st.dim('— ' + r.attestation_reason)}")
    print()


def _print_discovery(st: Style, d: "discovery_mod.DiscoveryResult") -> None:
    """The discovery track, printed in the order the stages ran."""
    print(st.bold("  ── discovery ───────────────────────────────────────────"))
    print(st.dim(f"  tree: {d.root} — copybooks, COBOL and JCL. No dataset is opened."))
    print()

    # The same courtesy the transform track pays to a missing GnuCOBOL: say it
    # once, loudly, at the top — rather than leaving a reader to infer it from
    # two NOT MEASURED lines further down.
    missing = discovery_mod.signing_backend()
    if missing is not None:
        print(st.yellow(
            "  Ed25519 is unavailable, so two stages did not run: the sealed-oracle\n"
            "  cross-check and the signed report. They report NOT MEASURED rather\n"
            "  than a verdict, and the seal is NOT being called bad — it could not\n"
            "  be checked at all.\n"
            "  Install with:  pip install cryptography\n"))

    r = d.resolve
    if r.status != discovery_mod.MEASURED:
        print(st.yellow(f"  resolve      : NOT MEASURED — {r.reason}"))
        print()
        return
    missing = (st.red(f"{len(r.unresolved)} missing ({', '.join(r.unresolved)})")
               if r.unresolved else "0 missing")
    print(f"  {st.dim('resolve      :')} {len(r.resolved)} copybook(s) resolved, "
          f"{missing}")
    print(st.dim(f"      {r.files_scanned} file(s) scanned, {r.copy_edges} COPY edge(s), "
                 f"{r.cycles} cycle(s)"))

    for row in d.layouts:
        print(st.dim(f"      layout {row.copybook:<10} {row.record:<22} "
                     f"{row.status:<9} {row.fields} field(s), "
                     f"{'—' if row.group_length is None else str(row.group_length) + ' bytes'}"))

    o = d.oracle
    if o.status == discovery_mod.NOT_MEASURED:
        print(st.yellow(f"  oracle x-ck  : NOT MEASURED — {o.reason}"))
    elif o.status == discovery_mod.FAILED:
        print(st.red(f"  oracle x-ck  : FAILED — {o.reason}"))
        for line in o.mismatches[:5]:
            print(st.red(f"      {line}"))
        for line in o.digest_mismatches[:5]:
            print(st.red(f"      {line}"))
    else:
        rate = f"{o.agreement.value:.4f}"
        painted = st.green(rate) if o.agreement.value == 1.0 else st.red(rate)
        print(f"  {st.dim('oracle x-ck  :')} {painted} "
              f"{st.dim(f'({o.agreed}/{o.comparisons} offset+length comparisons, tolerance zero)')}")
        print(st.dim(f"      vs RELIAN-DISCOVERY-BENCH v0.1, seal verified "
                     f"({', '.join(o.seal_layers)}), signer "
                     f"{discovery_mod.BENCH_FINGERPRINT}"))
        print(st.dim("      the engine reads source text and never invokes a "
                     "compiler; the oracle is a compiler's own byte layout."))
    if o.not_covered:
        print(st.dim(f"      not covered by the bench, so not compared: "
                     f"{', '.join(o.not_covered)}"))

    inv = d.inventory
    counts = inv.outcomes
    print(f"  {st.dim('file inv.    :')} {len(inv.rows)} file(s) across "
          f"{inv.programs_with_file_control}/{inv.programs_scanned} program(s) "
          f"with FILE-CONTROL, {inv.jcl_members} JCL member(s), "
          f"{inv.dd_statements} DD statement(s)")
    agree, disagree = counts.get("AGREE", 0), counts.get("DISAGREE", 0)
    print(st.dim(f"      LRECL x-check: {agree} agree, ")
          + (st.red(f"{disagree} DISAGREE") if disagree else st.dim("0 disagree"))
          + st.dim(f", {counts.get('NO_LRECL', 0)} no LRECL declared, "
                   f"{counts.get('NO_LAYOUT', 0)} no resolvable layout"))
    for row in inv.rows:
        if row.outcome == "DISAGREE":
            print(st.red(f"      {row.program}/{row.file_name}: computed "
                         f"{row.computed_length} vs declared LRECL {row.declared_lrecl}"))
    print(st.dim(f"      {len(inv.findings)} finding(s) — a finding is a discovery "
                 f"about the estate, not a fault in Relian."))

    ln = d.lineage
    print(f"  {st.dim('lineage      :')} {len(ln.edges)} edge(s) across "
          f"{len(ln.datasets)} dataset(s)")
    for program, direction, dataset in ln.edges:
        print(st.dim(f"      {program} {direction:<8} {dataset}"))
    if ln.coverage_statement:
        print(st.dim(f"      bound: {ln.coverage_statement}"))

    dd = d.ddl
    print(f"  {st.dim('target DDL   :')} {dd.tables} table(s) "
          f"({dd.tables_complete} COMPLETE, {dd.tables_partial} PARTIAL), "
          f"{dd.columns} column(s), lint "
          + (st.green("clean") if not dd.lint_problems
             else st.red(f"{len(dd.lint_problems)} violation(s)")))
    if dd.executed_status == discovery_mod.MEASURED:
        line = (f"      executed: {dd.statements_executed} statement(s), "
                f"{dd.columns_reconciled} column(s) reconciled against "
                f"information_schema")
        print(st.dim(line) if not dd.execution_problems else st.red(line))
        for problem in dd.execution_problems:
            print(st.red(f"      {problem}"))
    else:
        print(st.yellow(f"      execution: NOT MEASURED — {dd.executed_reason}"))

    rep = d.report
    if rep.status == discovery_mod.NOT_MEASURED:
        # Not a defect and not painted like one: a stage that could not run is
        # yellow, the same as an absent GnuCOBOL on the transform side. Red is
        # reserved for a report the shipped verifier actually rejected.
        print(st.yellow(f"  signed report: NOT MEASURED — {rep.reason}"))
    elif rep.status != discovery_mod.MEASURED:
        print(st.red(f"  signed report: {rep.status} — {rep.reason}"))
    else:
        print(f"  {st.dim('signed report:')} {', '.join(rep.artifacts)}")
        print(st.dim(f"      report_id {rep.report_id}  instance "
                     f"{rep.instance_fingerprint}"))
        for layer, verdict, _detail in rep.verify_layers:
            paint = (st.green if verdict == "PASS"
                     else st.yellow if verdict == "ABSENT" else st.red)
            print(st.dim(f"      verify {layer:<17} ") + paint(verdict))
        status_paint = (st.green if rep.verify_status == "VALID AND ATTESTED"
                        else st.yellow if rep.verify_status == "VALID AND UNATTESTED"
                        else st.red)
        print(f"      {st.dim('status:')} {status_paint(rep.verify_status)} "
              + st.dim(f"(verifier {rep.verifier_sha256[:16]}…, pinned to release "
                       f"key {discovery_mod.RELEASE_FINGERPRINT})"))
        if rep.report_reproducible is True:
            print(st.dim("      reproducible: a second build over the same tree "
                         "produced byte-identical report.json"))
        elif rep.report_reproducible is False:
            print(st.red("      NOT reproducible: a second build produced "
                         "different report.json bytes"))
        print(st.dim(f"      perimeter: {rep.countersign_request}"))
        print(st.dim("      that one line is everything that would cross the "
                     "perimeter — three digests, no source."))
    print()


def _print_summary(st: Style, run: RunResult) -> None:
    print(st.bold("  ── summary ─────────────────────────────────────────────"))
    by_verdict = {}
    for c in run.cases:
        by_verdict.setdefault(c.verdict, []).append(c.case_id)

    for verdict in (EQUIVALENCE_MEASURED, DIVERGENCE_MEASURED, REFUSED_UNSUPPORTED,
                    TRANSPILE_CRASHED, BUILD_FAILED, GAMING_TRIPPED,
                    INCOMPLETE_MEASUREMENT, NOT_MEASURED):
        ids = by_verdict.get(verdict)
        if not ids:
            continue
        colour, label = VERDICT_STYLE[verdict]
        print(f"  {getattr(st, colour)(label):<32} {len(ids)}  {st.dim(', '.join(ids))}")

    pb = run.portfolio_ber()
    print()
    if pb is None:
        print(st.yellow("  portfolio equivalence : not measured — no program was "
                        "executed on both sides"))
    else:
        rate = f"{pb.value:.4f}"
        painted = st.green(rate) if pb.value == 1.0 else st.red(rate)
        print(f"  portfolio equivalence : {painted} [{pb.grade}]")
        print(st.dim(f"      {pb.provenance}"))
    attestable = [c.case_id for c in run.cases if c.attestable]
    print(f"  attestation gate      : {len(attestable)}/{len(run.cases)} passed "
          f"{st.dim('(no signature issued — key custody is operator-only)')}")
    print(st.dim(f"  transpiler sha256     : {run.transpiler_sha256[:16]}…  "
                 f"git {run.git_sha}"))
    print(st.dim(f"  wall time             : {run.wall_seconds:.1f}s"))
    print()


def main(argv: Optional[List[str]] = None) -> int:
    ap = argparse.ArgumentParser(
        prog="python3 -m demo",
        description="Relian end-to-end demo: assess, migrate, and differentially "
                    "verify real COBOL against the migrated Java.",
    )
    ap.add_argument("--case", action="append", default=[], metavar="ID",
                    help="run only cases whose id contains this (repeatable)")
    ap.add_argument("--inputs", type=int, default=None, metavar="N",
                    help="cap the number of inputs per program (default: all)")
    ap.add_argument("--json", type=Path, metavar="PATH",
                    help="write the full measured run here")
    ap.add_argument("--html", type=Path, metavar="PATH",
                    help="write a self-contained HTML report here")
    ap.add_argument("--workdir", type=Path, default=None,
                    help="where to put generated Java and binaries "
                         "(default: a temporary directory)")
    ap.add_argument("--discovery-root", type=Path, default=None, metavar="PATH",
                    help="the tree the discovery track runs over (default: "
                         f"{discovery_mod._label(discovery_mod.DEFAULT_ROOT)})")
    ap.add_argument("--skip-discovery", action="store_true",
                    help="run only the transform track")
    ap.add_argument("--discovery-only", action="store_true",
                    help="run only the discovery track")
    ap.add_argument("--no-color", action="store_true")
    args = ap.parse_args(argv)

    if args.skip_discovery and args.discovery_only:
        print("--skip-discovery and --discovery-only are mutually exclusive",
              file=sys.stderr)
        return 2

    st = Style(enabled=sys.stdout.isatty() and not args.no_color)
    run_transform = not args.discovery_only
    run_discovery = not args.skip_discovery

    selected = []
    if run_transform:
        selected = cases_mod.select(args.case, limit=args.inputs)
        if not selected:
            print(f"no cases matched {args.case!r}", file=sys.stderr)
            return 2

    info = oracle.detect()
    _banner(st, info, len(selected), transform=run_transform,
            discovery=run_discovery)
    if run_transform and not info.available:
        print(st.yellow(
            "  GnuCOBOL is not installed, so the legacy side cannot be executed.\n"
            "  The demo will still assess, transpile and build — and will report\n"
            "  equivalence as NOT MEASURED rather than assuming it.\n"
            "  Install with:  apt-get install -y gnucobol\n"))

    tmp = None
    if args.workdir:
        workdir = args.workdir
        workdir.mkdir(parents=True, exist_ok=True)
    else:
        tmp = tempfile.TemporaryDirectory(prefix="relian-demo-")
        workdir = Path(tmp.name)

    run = None
    disc = None
    try:
        if run_transform:
            run = pipeline.run(selected, workdir,
                               on_case_done=lambda r: _print_case(st, r))
            _print_summary(st, run)

        if run_discovery:
            disc = discovery_mod.run(
                args.discovery_root or discovery_mod.DEFAULT_ROOT,
                workdir / "discovery",
            )
            _print_discovery(st, disc)

        payload = run.to_dict() if run is not None else {}
        payload["discovery"] = disc.to_dict() if disc is not None else None

        if args.json:
            args.json.parent.mkdir(parents=True, exist_ok=True)
            args.json.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")
            print(f"  json  : {args.json}")
        if args.html:
            args.html.parent.mkdir(parents=True, exist_ok=True)
            args.html.write_text(report.render_html(run, disc))
            print(f"  html  : {args.html}")
        if args.json or args.html:
            print()
    finally:
        if tmp is not None:
            tmp.cleanup()

    # Exit non-zero if anything that was supposed to be equivalent was not.
    # A refusal is not a failure — it is the designed behavior. Neither is the
    # deliberately-included crash case, which is a known gap the demo exists to
    # show; it is loudly labelled a defect in the output rather than smuggled
    # into the exit status of a demo that chose to run it.
    # INCOMPLETE_MEASUREMENT counts: the toolchain was there and an input still
    # failed to run, which is an anomaly worth surfacing. So does a total
    # execution outage — both sides built and then every input failed to run.
    # That reaches NOT_MEASURED, but it is a fault, not the supported offline
    # mode (no GnuCOBOL at all), which stays 0.
    #
    # The discovery track contributes the same way, and with the same
    # distinction: a copybook the job stream contradicts is a FINDING — the
    # product working — while a layout that disagrees with the sealed oracle,
    # DDL that fails its own lint, or a report the shipped verifier rejects are
    # defects. Only the second kind reaches the exit status. See
    # DiscoveryResult.failures().
    bad = [c for c in (run.cases if run is not None else [])
           if c.verdict in (DIVERGENCE_MEASURED, BUILD_FAILED, GAMING_TRIPPED,
                            INCOMPLETE_MEASUREMENT)
           or c.execution_outage]
    discovery_failures = disc.failures() if disc is not None else []
    return 1 if (bad or discovery_failures) else 0


if __name__ == "__main__":
    sys.exit(main())
