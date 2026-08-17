import { useState } from 'react';
import { api } from '../api';
import { CodeEditor, GradeTag, RiskIndicator, Spinner } from '../components';
import { DEFAULT_SAMPLE, SAMPLES } from '../samples';
import type { AnalysisResult } from '../types';

export function AnalyzeView(): JSX.Element {
    const [sampleId, setSampleId] = useState<string>(DEFAULT_SAMPLE.id);
    const [source, setSource] = useState<string>(DEFAULT_SAMPLE.code);
    const [result, setResult] = useState<AnalysisResult | null>(null);
    const [loading, setLoading] = useState<boolean>(false);
    const [error, setError] = useState<string | null>(null);

    function pickSample(id: string): void {
        const s = SAMPLES.find((x) => x.id === id) ?? DEFAULT_SAMPLE;
        setSampleId(id);
        setSource(s.code);
        setResult(null);
        setError(null);
    }

    async function analyze(): Promise<void> {
        setLoading(true);
        setError(null);
        setResult(null);
        try {
            setResult(await api.analyze(source));
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Analysis failed.');
        } finally {
            setLoading(false);
        }
    }

    return (
        <div>
            <div className="section-head">
                <h1>Analyze</h1>
                <p>
                    Static parse (COBOL-85 ANTLR) plus a heuristic risk score. This endpoint does not
                    run LLM semantic extraction, so business rules and confidence come back empty —
                    that emptiness is honest, not a blank waiting to be filled with canned text.
                </p>
            </div>

            <div className="card">
                <div className="sample-chips">
                    {SAMPLES.map((s) => (
                        <button
                            key={s.id}
                            className={`chip${sampleId === s.id ? ' active' : ''}`}
                            onClick={() => pickSample(s.id)}
                            type="button"
                        >
                            {s.label}
                        </button>
                    ))}
                </div>

                <CodeEditor
                    value={source}
                    onChange={setSource}
                    language="cobol"
                    hint="Editable — paste your legacy source"
                />

                <div className="btn-row" style={{ marginTop: '1rem' }}>
                    <button
                        className="btn btn-primary"
                        onClick={analyze}
                        disabled={loading || !source.trim()}
                        type="button"
                    >
                        {loading ? <Spinner /> : '🔍'} {loading ? 'Analyzing…' : 'Analyze code'}
                    </button>
                </div>
            </div>

            {error && (
                <div className="callout callout-danger">
                    <span className="callout-icon">⚠️</span>
                    <div>{error}</div>
                </div>
            )}

            {result && (
                <div className="card">
                    <h2>Analysis</h2>
                    <div className="card-sub">{result.purpose}</div>

                    <div className="grid-2">
                        <RiskIndicator score={result.risk_score} />
                        <div className="stat">
                            <div className="stat-title">
                                Confidence <GradeTag grade="SPECULATIVE" />
                            </div>
                            {result.confidence === null ? (
                                <div
                                    className="muted"
                                    style={{ fontStyle: 'italic', paddingTop: '0.6rem' }}
                                >
                                    Not measured (this endpoint does not compute a confidence value)
                                </div>
                            ) : (
                                <div className="stat-value measured">
                                    {result.confidence.toFixed(1)}
                                </div>
                            )}
                            {result.risk_level && (
                                <div className="stat-foot">Heuristic risk level: {result.risk_level}</div>
                            )}
                        </div>
                    </div>

                    <RuleList title="Business rules" items={result.business_rules} />
                    <RuleList title="Decision trees" items={result.decision_trees} />
                    <RuleList title="Edge cases" items={result.edge_cases} />
                    <RuleList
                        title="Recommendations"
                        items={result.recommendations}
                        emptyNote="No heuristic recommendations for this input."
                    />
                </div>
            )}
        </div>
    );
}

function RuleList({
    title,
    items,
    emptyNote,
}: {
    title: string;
    items: string[];
    emptyNote?: string;
}): JSX.Element {
    return (
        <div style={{ marginTop: '1rem' }}>
            <strong style={{ fontSize: '0.9rem' }}>{title}</strong>
            {items.length === 0 ? (
                <div className="muted" style={{ fontSize: '0.85rem', marginTop: '0.25rem' }}>
                    {emptyNote ?? 'Empty — not extracted by this endpoint (shown honestly).'}
                </div>
            ) : (
                <ul style={{ margin: '0.35rem 0 0 1.1rem', fontSize: '0.88rem' }}>
                    {items.map((it, i) => (
                        <li key={i}>{it}</li>
                    ))}
                </ul>
            )}
        </div>
    );
}
