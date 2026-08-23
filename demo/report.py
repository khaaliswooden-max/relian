"""Render a measured demo run as a self-contained HTML page.

Rules this renderer obeys, because a report is exactly where unmeasured numbers
sneak back in:

* It renders only what the run produced. There is no template value, no
  placeholder percentage, and no "—" standing in for a number that was supposed
  to be there. ``None`` renders as the words *not measured* (R1).
* Every rate is shown with its Trutina grade and its provenance string (R9).
* The attestation block reports a gate decision. It never draws a certificate,
  a signature, a transaction hash or a badge, because none was produced (R4/R5).
"""

from __future__ import annotations

import html
from typing import Optional

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

VERDICT_LABEL = {
    EQUIVALENCE_MEASURED: ("ok", "Equivalence measured"),
    DIVERGENCE_MEASURED: ("bad", "Divergence measured"),
    REFUSED_UNSUPPORTED: ("warn", "Refused — unsupported"),
    TRANSPILE_CRASHED: ("bad", "Transpile crashed"),
    BUILD_FAILED: ("bad", "Build failed"),
    NOT_MEASURED: ("warn", "Not measured"),
    INCOMPLETE_MEASUREMENT: ("warn", "Incomplete measurement"),
    GAMING_TRIPPED: ("bad", "Invalid — gaming controls"),
}

_CSS = """
:root{
  --bg:#fbfbfa; --panel:#fff; --ink:#1a1a18; --muted:#6b6b66; --line:#e4e4e0;
  --ok:#1a7f4b; --okbg:#e8f5ee; --bad:#b3261e; --badbg:#fdecea;
  --warn:#8a6100; --warnbg:#fdf3e0; --accent:#2b5cd9;
  --mono:ui-monospace,SFMono-Regular,"SF Mono",Menlo,Consolas,monospace;
}
@media (prefers-color-scheme:dark){
  :root:not([data-theme="light"]){
    --bg:#16161a; --panel:#1e1e23; --ink:#ececea; --muted:#9a9a95; --line:#32323a;
    --ok:#5fd39b; --okbg:#12291f; --bad:#ff8a80; --badbg:#2c1614;
    --warn:#e8bb63; --warnbg:#2b2213; --accent:#8fb0ff;
  }
}
:root[data-theme="dark"]{
  --bg:#16161a; --panel:#1e1e23; --ink:#ececea; --muted:#9a9a95; --line:#32323a;
  --ok:#5fd39b; --okbg:#12291f; --bad:#ff8a80; --badbg:#2c1614;
  --warn:#e8bb63; --warnbg:#2b2213; --accent:#8fb0ff;
}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--ink);
  font:15px/1.55 ui-sans-serif,system-ui,-apple-system,"Segoe UI",Roboto,sans-serif;}
.wrap{max-width:1000px;margin:0 auto;padding:40px 24px 80px}
h1{font-size:26px;margin:0 0 4px;letter-spacing:-.01em}
h2{font-size:15px;text-transform:uppercase;letter-spacing:.08em;color:var(--muted);
  margin:40px 0 14px;font-weight:600}
.sub{color:var(--muted);margin:0 0 28px}
.meta{display:flex;flex-wrap:wrap;gap:8px 28px;font-size:13px;color:var(--muted);
  padding:14px 0;border-top:1px solid var(--line);border-bottom:1px solid var(--line)}
.meta b{color:var(--ink);font-weight:600}
.tiles{display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));gap:14px;margin:22px 0}
.tile{background:var(--panel);border:1px solid var(--line);border-radius:10px;padding:16px 18px}
.tile .k{font-size:12px;text-transform:uppercase;letter-spacing:.06em;color:var(--muted)}
.tile .v{font-size:26px;font-weight:650;margin-top:6px;font-variant-numeric:tabular-nums}
.tile .n{font-size:12px;color:var(--muted);margin-top:6px;line-height:1.4}
.case{background:var(--panel);border:1px solid var(--line);border-radius:10px;
  padding:18px 20px;margin-bottom:14px}
.case-head{display:flex;align-items:baseline;justify-content:space-between;gap:16px;flex-wrap:wrap}
.case-id{font-family:var(--mono);font-weight:650;font-size:15px}
.case-note{color:var(--muted);font-size:13px;margin:8px 0 14px}
.pill{font-size:12px;font-weight:650;padding:4px 10px;border-radius:999px;white-space:nowrap}
.pill.ok{background:var(--okbg);color:var(--ok)} .pill.bad{background:var(--badbg);color:var(--bad)}
.pill.warn{background:var(--warnbg);color:var(--warn)}
.kv{display:grid;grid-template-columns:minmax(140px,auto) 1fr;gap:6px 18px;font-size:13.5px}
.kv dt{color:var(--muted)} .kv dd{margin:0;font-variant-numeric:tabular-nums}
.mono{font-family:var(--mono);font-size:12.5px}
.prov{font-size:12px;color:var(--muted);margin-top:3px;line-height:1.45}
.gate{margin-top:14px;padding:12px 14px;border-radius:8px;font-size:13px;line-height:1.5}
.gate.ok{background:var(--okbg)} .gate.warn{background:var(--warnbg)} .gate.bad{background:var(--badbg)}
.gate b{display:block;margin-bottom:3px}
.scroll{overflow-x:auto;margin-top:14px}
table{border-collapse:collapse;width:100%;font-size:12.5px;font-family:var(--mono)}
th,td{text-align:left;padding:7px 10px;border-bottom:1px solid var(--line);white-space:nowrap}
th{color:var(--muted);font-weight:600;text-transform:uppercase;font-size:11px;letter-spacing:.05em}
td.eq{color:var(--ok);font-weight:650} td.ne{color:var(--bad);font-weight:650}
.inv{display:flex;flex-wrap:wrap;gap:6px;margin-top:10px}
.inv span{font-family:var(--mono);font-size:12px;background:var(--warnbg);color:var(--warn);
  padding:3px 9px;border-radius:5px}
.note{color:var(--muted);font-size:13px;line-height:1.6}
footer{margin-top:48px;padding-top:20px;border-top:1px solid var(--line);
  color:var(--muted);font-size:12.5px;line-height:1.6}
"""


def _e(s) -> str:
    return html.escape("" if s is None else str(s))


def _measured(m, digits: int = 4) -> str:
    return "not measured" if m is None else f"{m.value:.{digits}f}"


def _grade(m) -> str:
    return "" if m is None else f' <span class="prov">[{_e(m.grade)}]</span>'


def _tile(k: str, v: str, note: str = "") -> str:
    n = f'<div class="n">{note}</div>' if note else ""
    return f'<div class="tile"><div class="k">{_e(k)}</div><div class="v">{v}</div>{n}</div>'


def _case_html(c: CaseResult) -> str:
    cls, label = VERDICT_LABEL[c.verdict]
    parts = [
        '<div class="case">',
        '<div class="case-head">',
        f'<span class="case-id">{_e(c.case_id)}</span>',
        f'<span class="pill {cls}">{_e(label)}</span>',
        "</div>",
    ]
    if c.note:
        parts.append(f'<div class="case-note">{_e(c.note)}</div>')

    rows = []
    a = c.assess
    if a is not None:
        cov = ("not measured" if a.coverage_ratio is None
               else f"{a.coverage_ratio.value:.4f}{_grade(a.coverage_ratio)}")
        sup = (f"{a.supported_statements}/{a.total_statements} statements"
               if a.total_statements is not None else "no statements recovered")
        rows.append(("Source", f'<span class="mono">{_e(c.source_path)}</span>'))
        rows.append(("Size", f"{_e(a.physical_loc)} lines, "
                             f"{_e(a.total_statements)} statements, "
                             f"cyclomatic {_e(a.cyclomatic)}, "
                             f"max nesting {_e(a.max_nesting_depth)}"))
        rows.append(("Transpilable", f"{cov}<div class='prov'>{_e(sup)}; "
                                     f"analysis method {_e(a.parse_method)}</div>"))
        rows.append(("Risk tier", f"{_e(a.risk_tier)}"
                                  f"<div class='prov'>{_e(a.risk_rule)}</div>"))

    if c.verdict in (REFUSED_UNSUPPORTED, TRANSPILE_CRASHED):
        if c.refused_verb:
            where = f"source line {c.refused_line}"
            if c.refused_paragraph:
                where += f", paragraph {c.refused_paragraph}"
            rows.append(("Refused at", f'<span class="mono">{_e(c.refused_verb)}</span> '
                                       f"— {_e(where)}"))
        elif c.transpile_error:
            rows.append(("Crashed with",
                         f'<span class="mono">{_e(c.transpile_error)}</span>'
                         "<div class='prov'>A defect in the diagnosis path: the engine "
                         "should have named the construct and refused. No Java was "
                         "emitted, so the outcome is still safe.</div>"))
        parts.append(f'<dl class="kv">{_rows(rows)}</dl>')
        if a is not None and a.unsupported_ranked:
            chips = "".join(f"<span>{_e(v)} ×{c_}</span>" for v, c_ in a.unsupported_ranked)
            parts.append("<div class='prov' style='margin-top:14px'>Constructs outside "
                         "the committed subset, counted in this source:</div>")
            parts.append(f'<div class="inv">{chips}</div>')
        if c.prediction_confirmed:
            parts.append("<div class='prov' style='margin-top:12px'>Cross-check: the "
                         "read-only assessment flagged this program before the "
                         "transpiler ran. Two independent implementations agreed.</div>")
        elif c.prediction_confirmed is False:
            parts.append('<div class="gate bad" style="margin-top:12px">'
                         "<b>Cross-check failed: the assessment was wrong</b>"
                         "It predicted full transpilability. Coverage classifies "
                         "statements by bare verb, so it over-reports wherever only "
                         "some forms of a verb are supported — as here, where the "
                         "inline PERFORM forms are handled but PERFORM of a named "
                         "paragraph is not.</div>")
        parts.append(_gate_html(c))
        parts.append("</div>")
        return "".join(parts)

    if c.java_sha256:
        det = ("reproducible — a second transpile produced the same bytes"
               if c.deterministic else "NOT reproducible")
        rows.append(("Generated Java",
                     f'<span class="mono">sha256 {_e(c.java_sha256[:24])}…</span>'
                     f"<div class='prov'>{_e(det)}</div>"))
    if c.java_build_ok is not None:
        cb = ("ok" if c.cobol_build_ok else
              ("failed" if c.cobol_build_ok is False else "not attempted"))
        rows.append(("Builds", f"javac {'ok' if c.java_build_ok else 'failed'}, "
                               f"cobc {_e(cb)}"))
    if c.ber is not None:
        rows.append(("Equivalence",
                     f"<b>{_measured(c.ber)}</b>{_grade(c.ber)}"
                     f"<div class='prov'>{_e(c.vectors_equivalent)}/"
                     f"{_e(c.vectors_executed)} executed inputs — "
                     f"{_e(c.ber.provenance)}</div>"))
    if c.vectors_absent:
        rows.append(("Not run",
                     f"{_e(c.vectors_absent)} of {_e(c.vectors_total)} input(s)"
                     "<div class='prov'>Could not be executed on both sides. Absent "
                     "from the rate above — an execution that did not happen is not "
                     "a divergence.</div>"))
    if c.oracle_agreement is not None:
        rows.append(("Oracle cross-check",
                     f"{_measured(c.oracle_agreement)}{_grade(c.oracle_agreement)}"
                     f"<div class='prov'>{_e(c.oracle_agreement.provenance)}</div>"))
    if c.gaming_violations:
        rows.append(("Anti-gaming", "<br>".join(_e(v) for v in c.gaming_violations)))
    if c.java_build_error and not c.java_build_ok:
        rows.append(("javac", f'<span class="mono">{_e(c.java_build_error[:400])}</span>'))

    parts.append(f'<dl class="kv">{_rows(rows)}</dl>')

    if c.vectors:
        parts.append(_vector_table(c))
    parts.append(_gate_html(c))
    parts.append("</div>")
    return "".join(parts)


def _rows(rows) -> str:
    return "".join(f"<dt>{_e(k)}</dt><dd>{v}</dd>" for k, v in rows)


def _vector_table(c: CaseResult) -> str:
    head = ("<tr><th>#</th><th>stdin</th><th>COBOL stdout</th><th>exit</th>"
            "<th>Java stdout</th><th>exit</th><th>equal</th></tr>")
    body = []
    for o in c.vectors:
        if not o.executed:
            cell = '<td>not run</td>'
        elif o.equivalent:
            cell = '<td class="eq">yes</td>'
        else:
            cell = '<td class="ne">NO</td>'
        body.append(
            f"<tr><td>{o.index}</td><td>{_e(o.stdin)}</td>"
            f"<td>{_e(o.cobol_stdout)}</td><td>{_e(o.cobol_exit)}</td>"
            f"<td>{_e(o.java_stdout)}</td><td>{_e(o.java_exit)}</td>{cell}</tr>"
        )
    return ("<div class='prov' style='margin-top:16px'>Every row below is two processes "
            "that ran during this report's generation — the original COBOL and the "
            "migrated Java, on the same input.</div>"
            f'<div class="scroll"><table>{head}{"".join(body)}</table></div>')


def _gate_html(c: CaseResult) -> str:
    if c.attestation_gate == "PASSED":
        cls, title = "ok", "Attestation gate: PASSED"
    elif c.verdict in (DIVERGENCE_MEASURED, BUILD_FAILED, GAMING_TRIPPED):
        cls, title = "bad", "Attestation gate: BLOCKED"
    else:
        cls, title = "warn", "Attestation gate: BLOCKED"
    return (f'<div class="gate {cls}"><b>{title}</b>{_e(c.attestation_reason)}</div>')


def _discovery_html(d) -> str:
    """The discovery track. Every row is a value the run computed."""
    if d is None:
        return ""

    from . import discovery as dm                            # noqa: PLC0415

    o = d.oracle
    if o.status == dm.MEASURED:
        oracle_tile = _tile(
            "Layouts vs sealed oracle", f"{o.agreement.value:.4f}",
            f"{o.agreed}/{o.comparisons} offset+length comparisons, tolerance "
            f"zero — {_e(o.agreement.provenance)}")
    elif o.status == dm.FAILED:
        oracle_tile = _tile("Layouts vs sealed oracle", "FAILED", _e(o.reason))
    else:
        oracle_tile = _tile("Layouts vs sealed oracle", "not measured",
                            _e(o.reason))

    counts = d.inventory.outcomes
    tiles = [
        _tile("Copybooks resolved", f"{len(d.resolve.resolved)}",
              f"{len(d.resolve.unresolved)} unresolved; "
              f"{d.resolve.copy_edges} COPY edge(s) over "
              f"{d.resolve.files_scanned} file(s)"),
        oracle_tile,
        _tile("Record length vs declared LRECL",
              f"{counts.get('AGREE', 0)}/{sum(counts.values())}",
              f"{counts.get('DISAGREE', 0)} disagree, "
              f"{counts.get('NO_LRECL', 0)} no LRECL declared, "
              f"{counts.get('NO_LAYOUT', 0)} no resolvable layout"),
        _tile("Program→dataset edges", f"{len(d.lineage.edges)}",
              f"across {len(d.lineage.datasets)} dataset(s); this graph is not "
              f"offered as complete"),
        _tile("Target-schema tables", f"{d.ddl.tables}",
              f"{d.ddl.columns} column(s); lint "
              + ("clean" if not d.ddl.lint_problems
                 else f"{len(d.ddl.lint_problems)} violation(s)")
              + ("; executed against PostgreSQL in this run"
                 if d.ddl.executed_status == dm.MEASURED
                 else "; not executed — see below")),
        _tile("Signed report",
              _e(d.report.verify_status or d.report.status),
              "verified by the shipped recipient-side tool, pinned to release "
              f"key {dm.RELEASE_FINGERPRINT}"),
    ]

    layout_rows = "".join(
        f"<tr><td class=\"mono\">{_e(r.copybook)}</td>"
        f"<td class=\"mono\">{_e(r.record)}</td><td>{_e(r.status)}</td>"
        f"<td>{_e(r.fields)}</td>"
        f"<td>{'—' if r.group_length is None else _e(r.group_length)}</td></tr>"
        for r in d.layouts
    )
    inv_rows = "".join(
        f"<tr><td class=\"mono\">{_e(r.program)}</td>"
        f"<td class=\"mono\">{_e(r.file_name)}</td>"
        f"<td class=\"mono\">{_e(r.ddname)}</td>"
        f"<td class=\"mono\">{_e(r.dataset)}</td><td>{_e(r.outcome)}</td>"
        f"<td>{'—' if r.computed_length is None else _e(r.computed_length)}</td>"
        f"<td>{'—' if r.declared_lrecl is None else _e(r.declared_lrecl)}</td></tr>"
        for r in d.inventory.rows
    )
    edge_rows = "".join(
        f"<tr><td class=\"mono\">{_e(p)}</td><td>{_e(direction)}</td>"
        f"<td class=\"mono\">{_e(ds)}</td></tr>"
        for p, direction, ds in d.lineage.edges
    )
    findings = "".join(
        f"<li><b>{_e(kind)}</b> — {_e(detail)}</li>"
        for kind, detail in d.inventory.findings
    )

    execution = (
        "<p class=\"note\"><b>Target schema.</b> "
        + (f"Executed against PostgreSQL in this run: "
           f"{_e(d.ddl.statements_executed)} statement(s), "
           f"{_e(d.ddl.columns_reconciled)} column(s) reconciled against "
           f"<span class=\"mono\">information_schema</span>."
           if d.ddl.executed_status == dm.MEASURED
           else f"<b>Not executed.</b> {_e(d.ddl.executed_reason)}")
        + "</p>"
    )

    return f"""
<h2>Discovery — the estate</h2>
<p class="sub">The same run, over <span class="mono">{_e(d.root)}</span>:
copybooks, COBOL and a job stream. Nothing here opens a dataset and nothing here
invokes a compiler — the layouts are computed from source text and then checked
against a compiler's own byte layout, sealed before the engine existed.</p>

<div class="tiles">{"".join(tiles)}</div>

<h3>Record layouts</h3>
<div class="scroll"><table>
<tr><th>Copybook</th><th>Record</th><th>Status</th><th>Fields</th><th>Bytes</th></tr>
{layout_rows}</table></div>

<h3>File inventory — SELECT ∪ FD ∪ DD</h3>
<div class="scroll"><table>
<tr><th>Program</th><th>File</th><th>DD</th><th>Dataset</th><th>Outcome</th>
<th>Computed</th><th>Declared LRECL</th></tr>
{inv_rows}</table></div>
<p class="note">Both numbers are shown and neither is resolved into the other. A
copybook that does not describe the data is the most expensive thing to discover
during a migration and the cheapest to discover before one.</p>

<h3>Data lineage</h3>
<div class="scroll"><table>
<tr><th>Program</th><th>Direction</th><th>Dataset</th></tr>
{edge_rows}</table></div>
<p class="note">{_e(d.lineage.coverage_statement)}</p>

<h3>Findings</h3>
<ul class="note">{findings or "<li>none</li>"}</ul>
<p class="note">A finding is a discovery about the estate, not a fault in Relian.
None of these affects the exit status; a layout that disagreed with the sealed
oracle would.</p>

{execution}
<p class="note"><b>Signed report.</b> The run wrote
<span class="mono">{_e(", ".join(d.report.artifacts))}</span>, then verified them
with the shipped recipient-side tool
(<span class="mono">{_e((d.report.verifier_sha256 or "")[:16])}…</span>):
<b>{_e(d.report.verify_status)}</b>. Exactly one line would cross the customer
perimeter — <span class="mono">{_e(d.report.countersign_request)}</span> — which
is three digests and no source. No countersignature has ever been issued, and
this demo ships no simulated one.</p>
"""


def _discovery_only_html(discovery) -> str:
    """`--discovery-only`: the estate, with no transform claim on the page."""
    body = _discovery_html(discovery)
    if not body:
        body = ("<p class=\"note\">Neither track ran, so this page states "
                "nothing.</p>")
    return f"""<title>Relian Discovery Evidence</title>
<style>{_CSS}</style>
<div class="wrap">
<h1>Relian — discovery evidence</h1>
<p class="sub">The discovery track only. No migration was attempted and no
equivalence is claimed on this page.</p>
{body}
<footer>
Generated by <span class="mono">python3 -m demo --discovery-only</span> from the
Relian repository. Every number carries its provenance and a Trutina grade;
anything unmeasured is reported as unmeasured. &copy; 2025–2026 Zuup, LLC /
Visionblox LLC.
</footer>
</div>
"""


def render_html(run: Optional[RunResult], discovery=None) -> str:
    """The evidence page. ``run`` may be ``None`` when only the discovery track
    was asked for — the transform sections are then absent rather than rendered
    empty, because a zeroed equivalence tile is a measurement nobody took."""
    if run is None:
        return _discovery_only_html(discovery)
    pb = run.portfolio_ber()
    by = {}
    for c in run.cases:
        by.setdefault(c.verdict, []).append(c)

    n_eq = len(by.get(EQUIVALENCE_MEASURED, []))
    n_ref = len(by.get(REFUSED_UNSUPPORTED, []))
    n_crash = len(by.get(TRANSPILE_CRASHED, []))
    attestable = [c for c in run.cases if c.attestable]
    # The tile says "executed on both sides", so it must count executions, not
    # attempts.
    total_executed = sum(c.vectors_executed or 0 for c in run.cases)
    total_absent = sum(c.vectors_absent or 0 for c in run.cases)

    tiles = [
        _tile("Portfolio equivalence",
              "not measured" if pb is None else f"{pb.value:.4f}",
              "" if pb is None else f"{pb.grade} — {_e(pb.provenance)}"),
        _tile("Inputs executed on both sides", f"{total_executed}",
              "each compared on stdout and process exit code"
              + (f"; {total_absent} input(s) could not be run and are excluded"
                 if total_absent else "")),
        # Plain "&" — _tile escapes its label, so pre-escaping double-escapes it.
        _tile("Programs migrated & verified", f"{n_eq}",
              "behavioral equivalence measured on every input"),
        _tile("Programs refused", f"{n_ref}",
              "outside the committed subset — no output, no attestation"),
        _tile("Attestation gate", f"{len(attestable)}/{len(run.cases)}",
              "gate decisions only; no signature is issued in this demo"),
    ]
    if n_crash:
        tiles.append(_tile(
            "Undiagnosed failures", f"{n_crash}",
            "the engine stopped without naming the construct — a defect in the "
            "refusal path, shown rather than hidden"))

    oracle_line = (f"<b>{_e(run.oracle_label)}</b>" if run.oracle_available
                   else "<b>not available</b> — equivalence could not be measured")

    cases_html = "".join(_case_html(c) for c in run.cases)
    discovery_html = _discovery_html(discovery)

    return f"""<title>Relian Migration Evidence</title>
<style>{_CSS}</style>
<div class="wrap">
<h1>Relian — migration evidence</h1>
<p class="sub">Real COBOL, assessed and migrated by the shipped engine, then executed
side by side against the original. Every figure on this page was produced by a
process that ran while this page was generated.</p>

<div class="meta">
  <span>legacy oracle {oracle_line}</span>
  <span>transpiler <b class="mono">{_e(run.transpiler_sha256[:16])}…</b></span>
  <span>git <b class="mono">{_e(run.git_sha)}</b></span>
  <span>wall time <b>{run.wall_seconds:.1f}s</b></span>
</div>

<div class="tiles">{"".join(tiles)}</div>

<h2>Per-program results — transform</h2>
{cases_html}
{discovery_html}

<h2>How to read this</h2>
<p class="note">
<b>Equivalence</b> is the fraction of inputs on which the migrated Java produced
byte-identical stdout <em>and</em> the same process exit code as the original
COBOL, compiled and run here by GnuCOBOL. Output is whitespace-normalised;
numbers are compared exactly, because a rounding divergence is a defect rather
than a formatting difference.
</p>
<p class="note">
<b>Refusal is a result, not an error.</b> A program using constructs outside the
committed COBOL-85 subset produces no Java at all — the engine names the verb
and the line and stops. Emitting plausible-looking output there is the failure
mode this project exists to eliminate.
</p>
<p class="note">
<b>Nothing here is signed.</b> The attestation gate reports whether a migration
<em>would be</em> attestable. Signing keys are operator-custody only, and this
demo ships no simulated attestation, badge, or transaction hash.
</p>
<p class="note">
<b>Scope.</b> These results cover the programs listed above and the COBOL-85
subset they exercise. They are not a claim about arbitrary COBOL. The
<em>transform</em> path does not support CICS, VSAM, JCL, copybooks or embedded
SQL, and does not claim to. The <em>discovery</em> path does read copybooks and
JCL — that is what it is for — and reads them statically, without opening a
dataset or invoking a compiler. Held-out benchmark vectors are scored only in
CI and are not touched by this demo.
</p>

<footer>
Generated by <span class="mono">python3 -m demo</span> from the Relian repository.
Every number carries its provenance and a Trutina grade; anything unmeasured is
reported as unmeasured. &copy; 2025–2026 Zuup, LLC / Visionblox LLC.
</footer>
</div>
"""
