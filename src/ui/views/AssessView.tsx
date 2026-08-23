import { useState } from 'react';
import { api } from '../api';
import { GradeTag, Spinner, StatTile } from '../components';
import type {
    AssessmentResult,
    Measured,
    PlainSummary,
    ProgramAssessment,
    RiskTier,
} from '../types';

// Coverage ratios arrive as a 0..1 fraction; the UI shows a percentage. A null
// ratio renders "Not measured" — never back-filled (R1).
function pct(m: Measured | null | undefined): number | null {
    if (!m || m.value === null || m.value === undefined) return null;
    return Math.round(m.value * 1000) / 10;
}

function TierBadge({ tier }: { tier: RiskTier }): JSX.Element {
    return <span className={`badge badge-${tier.toLowerCase()}`}>{tier}</span>;
}

// The section 0 prose is written by the report writer, in Markdown. Only two
// inline forms occur in it — **strong** and `code` — so they are rendered here
// rather than by pulling in a Markdown parser for two cases. Anything else is
// shown verbatim, which is the safe direction: an unrendered marker is visible
// and gets fixed, a swallowed one is not.
const INLINE_MARKDOWN = /(\*\*[^*]+\*\*|`[^`]+`)/g;

function RichText({ text }: { text: string }): JSX.Element {
    return (
        <>
            {text.split(INLINE_MARKDOWN).map((part, i) => {
                if (part.startsWith('**') && part.endsWith('**')) {
                    return <strong key={i}>{part.slice(2, -2)}</strong>;
                }
                if (part.startsWith('`') && part.endsWith('`')) {
                    return <code key={i}>{part.slice(1, -1)}</code>;
                }
                return <span key={i}>{part}</span>;
            })}
        </>
    );
}

/**
 * Section 0 of the report — the same findings in plain language.
 *
 * Every string and every figure comes from `plain_summary` on the API, which is
 * `report.plain_summary()`: the one source the CLI's Markdown and DOCX render
 * too. This component lays it out and computes nothing — no ratio, no total, no
 * rounding. If a figure is wrong here it is wrong in the report as well, which
 * is the point.
 */
function PlainSummaryCard({ summary }: { summary: PlainSummary }): JSX.Element {
    return (
        <div className="card">
            <h2>{summary.title}</h2>
            <div className="card-sub">
                <RichText text={summary.intro} /> The section names it points at are the
                report's, from <code>python3 -m src.assessment.cli</code>; the cards below
                are this tab's view of the same run.
            </div>

            <p>
                <RichText text={summary.scope} />
            </p>

            {summary.where_we_stand && (
                <>
                    <p>
                        <RichText text={summary.where_we_stand.heading} />{' '}
                        <GradeTag grade={summary.where_we_stand.grade} />
                    </p>
                    <div className="table-wrap">
                        <table>
                            <thead>
                                <tr>
                                    <th>Group</th>
                                    <th>Programs</th>
                                    <th>Which ones</th>
                                </tr>
                            </thead>
                            <tbody>
                                {summary.where_we_stand.groups.map((g) => (
                                    <tr key={g.tier}>
                                        <td>
                                            {g.label} <TierBadge tier={g.tier} />
                                            <div className="muted" style={{ fontSize: '0.72rem' }}>
                                                {g.explanation}
                                            </div>
                                        </td>
                                        <td className="num">{g.programs}</td>
                                        <td>
                                            {g.program_ids.map((id) => (
                                                <div key={id}>
                                                    <code>{id}</code>
                                                </div>
                                            ))}
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                    <div className="muted" style={{ fontSize: '0.72rem', marginTop: '0.35rem' }}>
                        {summary.where_we_stand.provenance}
                    </div>
                </>
            )}

            {summary.in_the_way && (
                <>
                    <p style={{ marginTop: '1rem' }}>
                        <RichText text={summary.in_the_way.heading} />
                    </p>
                    {summary.in_the_way.constructs.length > 0 && (
                        <ul>
                            {summary.in_the_way.constructs.map((c) => (
                                <li key={c.construct}>
                                    <strong>{c.construct}</strong>
                                    {c.gloss ? ` — ${c.gloss}.` : ' —'} Appears {c.count} time(s).
                                </li>
                            ))}
                            {summary.in_the_way.omitted > 0 && (
                                <li className="muted">
                                    …and {summary.in_the_way.omitted} further construct(s), all
                                    listed under <strong>Unsupported constructs</strong> below.
                                </li>
                            )}
                        </ul>
                    )}
                </>
            )}

            {summary.how_much && (
                <>
                    <p style={{ marginTop: '1rem' }}>
                        <RichText text={summary.how_much.heading} />
                    </p>
                    <div className="stat-grid">
                        {summary.how_much.rows.map((row) => (
                            <StatTile
                                key={row.label}
                                title={row.label}
                                value={row.measured ? row.measured.value : null}
                                grade={row.measured ? row.measured.grade : undefined}
                                foot={row.measured ? row.measured.provenance : 'Not measured'}
                            />
                        ))}
                    </div>
                </>
            )}

            {summary.limits && (
                <div className="callout callout-info" style={{ marginTop: '1rem' }}>
                    <span className="callout-icon">ℹ️</span>
                    <div>
                        <RichText text={summary.limits} />
                    </div>
                </div>
            )}
        </div>
    );
}

export function AssessView(): JSX.Element {
    const [result, setResult] = useState<AssessmentResult | null>(null);
    const [loading, setLoading] = useState<boolean>(false);
    const [error, setError] = useState<string | null>(null);

    async function run(): Promise<void> {
        setLoading(true);
        setError(null);
        setResult(null);
        try {
            setResult(await api.assessDemo());
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Assessment failed.');
        } finally {
            setLoading(false);
        }
    }

    const b = result?.bundle;
    const portfolioCovPct = pct(b?.portfolio_coverage.coverage_ratio);
    const ranked = b?.portfolio_coverage.unsupported_ranked ?? [];

    return (
        <div>
            <div className="section-head">
                <h1>Assess — Meridian MUD demo</h1>
                <p>
                    A read-only, offline portfolio assessment of the bundled{' '}
                    <code>examples/demo</code> corpus — a <strong>synthetic</strong> COBOL-85 code set
                    (fictional municipal water utility), not customer or benchmark source. This is the
                    same run as{' '}
                    <code>python3 -m src.assessment.cli examples/demo</code>: nothing is written to
                    disk and no model or network call is made. Every number below is measured this run
                    and carries its own Trutina grade — none is a default or an estimate.
                </p>
            </div>

            <div className="card">
                <div className="card-sub">
                    The corpus is arranged in three tiers so a single run shows the whole honesty
                    spectrum — a clean in-subset migration (LOW), partial coverage (HIGH), and a
                    principled refusal (BLOCKED) — instead of only the happy path.
                </div>
                <div className="btn-row">
                    <button
                        className="btn btn-primary"
                        onClick={run}
                        disabled={loading}
                        type="button"
                    >
                        {loading ? <Spinner /> : '▶'}{' '}
                        {loading ? 'Assessing…' : 'Run demo assessment'}
                    </button>
                    {result && (
                        <span className="muted" style={{ fontSize: '0.82rem' }}>
                            {result.programs_assessed} programs · {result.manifest_files} files ·
                            schema <code>{result.schema_version}</code>
                        </span>
                    )}
                </div>
            </div>

            {error && (
                <div className="callout callout-danger">
                    <span className="callout-icon">⚠️</span>
                    <div>
                        <strong>Could not run the assessment.</strong>
                        <div style={{ marginTop: '0.25rem' }}>{error}</div>
                        <div className="muted" style={{ marginTop: '0.25rem', fontSize: '0.82rem' }}>
                            Start the API with <code>uvicorn src.api.main:app</code> from the repo
                            root.
                        </div>
                    </div>
                </div>
            )}

            {b && result && (
                <>
                    {result.plain_summary && (
                        <PlainSummaryCard summary={result.plain_summary} />
                    )}

                    <div className="card">
                        <h2>
                            Portfolio <GradeTag grade="PLAUSIBLE" />
                        </h2>
                        <div className="card-sub">
                            Roll-up across {b.programs.length} programs. The risk tier is a policy
                            decision from the published <code>RISK_RULES</code> table over VERIFIED
                            inputs; coverage is a token-scan ratio graded PLAUSIBLE where the grammar
                            could not parse a program cleanly.
                        </div>
                        <div className="stat-grid">
                            <StatTile
                                title="Construct coverage"
                                value={portfolioCovPct}
                                unit="%"
                                digits={1}
                                grade={b.portfolio_coverage.coverage_ratio.grade}
                                foot={`${b.portfolio_coverage.supported_statements}/${b.portfolio_coverage.total_statements} statements supported`}
                            />
                            <div className="stat">
                                <div className="stat-title">
                                    Portfolio risk tier <GradeTag grade={b.portfolio_risk.grade} />
                                </div>
                                <div style={{ paddingTop: '0.5rem' }}>
                                    <TierBadge tier={b.portfolio_risk.tier} />
                                </div>
                                <div className="stat-foot">{b.portfolio_risk.rule}</div>
                            </div>
                            <StatTile
                                title="Quotable-today LOC"
                                value={b.quotable_loc.value}
                                grade={b.quotable_loc.grade}
                                foot="Code lines free of any unsupported construct"
                            />
                            <StatTile
                                title="LOC needing grammar work"
                                value={b.grammar_expansion_loc.value}
                                grade={b.grammar_expansion_loc.grade}
                                foot="Lines carrying ≥1 out-of-subset construct"
                            />
                        </div>
                    </div>

                    <div className="card">
                        <h2>Per-program spectrum</h2>
                        <div className="card-sub">
                            Each program's own coverage and risk tier. The BLOCKED row is the honest
                            refusal — the assessor reports it rather than pretending the file migrates.
                        </div>
                        <div className="table-wrap">
                            <table>
                                <thead>
                                    <tr>
                                        <th>Program</th>
                                        <th>Risk tier</th>
                                        <th>Coverage</th>
                                        <th>Grade</th>
                                        <th>Statements</th>
                                        <th>Cyclomatic</th>
                                        <th>Physical LOC</th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {b.programs.map((p: ProgramAssessment) => {
                                        const cp = pct(p.coverage.coverage_ratio);
                                        return (
                                            <tr key={p.program_id}>
                                                <td>
                                                    {p.program_id}
                                                    <div
                                                        className="muted"
                                                        style={{ fontSize: '0.72rem' }}
                                                    >
                                                        {p.risk.rule}
                                                    </div>
                                                </td>
                                                <td>
                                                    <TierBadge tier={p.risk.tier} />
                                                </td>
                                                <td className="num">
                                                    {cp === null ? '—' : `${cp.toFixed(1)}%`}
                                                </td>
                                                <td>
                                                    <GradeTag
                                                        grade={p.coverage.coverage_ratio.grade}
                                                    />
                                                </td>
                                                <td className="num">
                                                    {p.coverage.supported_statements}/
                                                    {p.coverage.total_statements}
                                                </td>
                                                <td className="num">
                                                    {p.complexity ? p.complexity.cyclomatic : '—'}
                                                </td>
                                                <td className="num">{p.loc.physical}</td>
                                            </tr>
                                        );
                                    })}
                                </tbody>
                            </table>
                        </div>
                    </div>

                    {ranked.length > 0 && (
                        <div className="card">
                            <h2>Unsupported constructs (portfolio)</h2>
                            <div className="card-sub">
                                Ranked by occurrence across the corpus. These are why the out-of-subset
                                programs assess MED or BLOCKED rather than transpiling — the inventory
                                is the honest-failure evidence, not an error.
                            </div>
                            <div className="table-wrap">
                                <table>
                                    <thead>
                                        <tr>
                                            <th>Construct</th>
                                            <th>Occurrences</th>
                                        </tr>
                                    </thead>
                                    <tbody>
                                        {ranked.map((r) => (
                                            <tr key={r.verb}>
                                                <td>
                                                    <code>{r.verb}</code>
                                                </td>
                                                <td className="num">{r.count}</td>
                                            </tr>
                                        ))}
                                    </tbody>
                                </table>
                            </div>
                        </div>
                    )}

                    <div className="card">
                        <h2>Provenance</h2>
                        <div className="card-sub">
                            The report hash is <code>sha256</code> of this assessment's canonical
                            JSON. It reproduces for the same corpus within the same runtime. It embeds{' '}
                            <code>tool_versions</code> (invocation, Python, platform), so it is not
                            guaranteed to byte-match a <code>python -m src.assessment.cli</code> run —
                            the corpus measurements are identical either way; only the recorded
                            invocation differs. The manifest hash below is over the source bytes and
                            does match (R8).
                        </div>
                        <div className="table-wrap">
                            <table>
                                <tbody>
                                    <tr>
                                        <td>Root</td>
                                        <td>
                                            <code>{result.root_label}</code>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td>Report hash</td>
                                        <td>
                                            <code>{result.report_hash}</code>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td>Manifest hash</td>
                                        <td>
                                            <code>{b.inventory.manifest_hash}</code>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td>Files by kind</td>
                                        <td className="muted">
                                            {Object.entries(b.inventory.counts_by_kind)
                                                .map(([k, v]) => `${v} ${k}`)
                                                .join(', ')}
                                        </td>
                                    </tr>
                                </tbody>
                            </table>
                        </div>

                        {b.notes.length > 0 && (
                            <div className="callout callout-info" style={{ marginTop: '1rem' }}>
                                <span className="callout-icon">ℹ️</span>
                                <div>
                                    <strong>Measurement notes</strong>
                                    <ul>
                                        {b.notes.map((n, i) => (
                                            <li key={i}>{n}</li>
                                        ))}
                                    </ul>
                                </div>
                            </div>
                        )}
                    </div>
                </>
            )}
        </div>
    );
}
