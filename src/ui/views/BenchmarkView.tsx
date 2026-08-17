import { useEffect, useState } from 'react';
import { api } from '../api';
import { GradeTag, StatTile } from '../components';
import { BENCH_ROWS, BENCH_TARGETS, BENCH_VERSION, LEDGER_PATH } from '../constants';
import type { PlatformMetrics } from '../types';

export function BenchmarkView(): JSX.Element {
    const [metrics, setMetrics] = useState<PlatformMetrics | null>(null);
    const [metricsError, setMetricsError] = useState<string | null>(null);

    useEffect(() => {
        let alive = true;
        api.metrics()
            .then((m) => alive && setMetrics(m))
            .catch((e) => alive && setMetricsError(e instanceof Error ? e.message : 'unavailable'));
        return () => {
            alive = false;
        };
    }, []);

    return (
        <div>
            <div className="section-head">
                <h1>Benchmark &amp; metrics</h1>
                <p>
                    The committed baseline below is a fact of record — measured, then frozen and
                    Ed25519-signed in <code>{LEDGER_PATH}</code> ({BENCH_VERSION}). Session metrics are
                    live from the API. Targets are gates, shown separately from results so a goal is
                    never mistaken for an achievement.
                </p>
            </div>

            <div className="card">
                <h2>
                    Committed baseline <GradeTag grade="VERIFIED" />
                </h2>
                <div className="card-sub">
                    Measured on held-out vectors and committed before solution work. Source:{' '}
                    <code>{LEDGER_PATH}</code>.
                </div>
                <div className="table-wrap">
                    <table>
                        <thead>
                            <tr>
                                <th>Candidate</th>
                                <th>BER (held-out)</th>
                                <th>Build</th>
                                <th>Branch cov.</th>
                                <th>Committed bar</th>
                            </tr>
                        </thead>
                        <tbody>
                            {BENCH_ROWS.map((r) => (
                                <tr key={r.candidate}>
                                    <td>
                                        {r.candidate}
                                        <div className="muted" style={{ fontSize: '0.72rem' }}>
                                            {r.note}
                                        </div>
                                    </td>
                                    <td className="num">{r.ber}</td>
                                    <td className="num">{r.build}</td>
                                    <td className="num">{r.branchCoverage}</td>
                                    <td>
                                        <span className={`badge badge-${r.bar.toLowerCase()}`}>
                                            {r.bar}
                                        </span>
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            </div>

            <div className="card">
                <h2>Success criteria (targets, not results)</h2>
                <div className="card-sub">
                    Frozen in the ledger before any solution work. No target may be reported as an
                    achievement until a candidate clears it on held-out vectors.
                </div>
                <div className="table-wrap">
                    <table>
                        <thead>
                            <tr>
                                <th>Criterion</th>
                                <th>Target gate</th>
                            </tr>
                        </thead>
                        <tbody>
                            {BENCH_TARGETS.map((t) => (
                                <tr key={t.label}>
                                    <td>{t.label}</td>
                                    <td className="num">{t.target}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            </div>

            <div className="card">
                <h2>This session (live)</h2>
                <div className="card-sub">
                    Aggregated from migrations run against this API instance. Averages are computed
                    only over runs whose values were actually measured; with none, they read “Not
                    measured”.
                </div>
                {metricsError ? (
                    <div className="callout callout-info">
                        <span className="callout-icon">🔌</span>
                        <div>
                            Live metrics unavailable — {metricsError}. Start the API with{' '}
                            <code>uvicorn src.api.main:app</code>.
                        </div>
                    </div>
                ) : (
                    <div className="stat-grid">
                        <StatTile title="Migrations (session)" value={metrics?.total_migrations ?? null} />
                        <StatTile
                            title="Completed"
                            value={metrics?.successful_migrations ?? null}
                        />
                        <StatTile
                            title="Avg semantic score"
                            value={metrics?.average_semantic_score ?? null}
                            unit="%"
                            digits={1}
                            grade="VERIFIED"
                            foot="Mean over measured runs only"
                        />
                        <StatTile
                            title="Avg test coverage"
                            value={metrics?.average_test_coverage ?? null}
                            unit="%"
                            digits={1}
                            grade="VERIFIED"
                            foot="Mean over measured runs only"
                        />
                        <StatTile
                            title="LOC processed"
                            value={metrics?.total_loc_processed ?? null}
                        />
                    </div>
                )}
            </div>
        </div>
    );
}
