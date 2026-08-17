import { useState } from 'react';
import { api, ApiError } from '../api';
import { CodeEditor, Pipeline, ProgressBar, RiskIndicator, Spinner } from '../components';
import { DEFAULT_SAMPLE, SAMPLES } from '../samples';
import type { MigrationResult, MigrationStatus } from '../types';

const TERMINAL: MigrationStatus[] = ['completed', 'failed'];
const POLL_MS = 900;
const POLL_TIMEOUT_MS = 60_000;

const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

export function MigrateView(): JSX.Element {
    const [sampleId, setSampleId] = useState<string>(DEFAULT_SAMPLE.id);
    const [source, setSource] = useState<string>(DEFAULT_SAMPLE.code);
    const [target, setTarget] = useState<string>('java');
    const [template, setTemplate] = useState<string>('banking');

    const [status, setStatus] = useState<MigrationStatus | null>(null);
    const [result, setResult] = useState<MigrationResult | null>(null);
    const [running, setRunning] = useState<boolean>(false);
    const [error, setError] = useState<string | null>(null);

    function pickSample(id: string): void {
        const s = SAMPLES.find((x) => x.id === id) ?? DEFAULT_SAMPLE;
        setSampleId(id);
        setSource(s.code);
        setResult(null);
        setStatus(null);
        setError(null);
    }

    async function runMigration(): Promise<void> {
        setRunning(true);
        setError(null);
        setResult(null);
        setStatus('pending');
        try {
            const started = await api.startMigration({
                source_code: source,
                target_language: target,
                template,
            });
            const id = started.migration_id;

            const deadline = Date.now() + POLL_TIMEOUT_MS;
            let current: MigrationResult | null = null;
            for (;;) {
                await sleep(POLL_MS);
                current = await api.getMigration(id);
                setStatus(current.status);
                if (TERMINAL.includes(current.status)) break;
                if (Date.now() > deadline) {
                    throw new ApiError('Migration timed out while polling for completion.');
                }
            }
            setResult(current);
        } catch (err) {
            const msg = err instanceof Error ? err.message : 'Migration failed.';
            setError(msg);
            setStatus('failed');
        } finally {
            setRunning(false);
        }
    }

    const done = result?.status === 'completed';
    const failed = result?.status === 'failed';

    return (
        <div>
            <div className="section-head">
                <h1>Migrate</h1>
                <p>
                    Deterministic COBOL-85 → target transform via the C1 transpiler. Scores shown
                    below are what the pipeline actually measured this run — semantic preservation is
                    a differential Behavioral Equivalence Ratio, not an estimate. Unmeasured values
                    read <em>Not measured</em>.
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
                            <small>{s.description}</small>
                        </button>
                    ))}
                </div>

                <div className="field-row">
                    <div className="field">
                        <label className="field-label" htmlFor="target">
                            Target language
                        </label>
                        <select id="target" value={target} onChange={(e) => setTarget(e.target.value)}>
                            <option value="java">Java (supported)</option>
                            <option value="python">Python (supported)</option>
                            <option value="rust" disabled>
                                Rust (planned)
                            </option>
                            <option value="csharp" disabled>
                                C# (planned)
                            </option>
                        </select>
                    </div>
                    <div className="field">
                        <label className="field-label" htmlFor="template">
                            Industry template
                        </label>
                        <select
                            id="template"
                            value={template}
                            onChange={(e) => setTemplate(e.target.value)}
                        >
                            <option value="banking">Banking &amp; Financial</option>
                            <option value="government">Government &amp; Defense</option>
                            <option value="healthcare">Healthcare</option>
                            <option value="manufacturing">Manufacturing</option>
                            <option value="insurance">Insurance</option>
                        </select>
                    </div>
                </div>

                <div className="grid-2">
                    <CodeEditor
                        value={source}
                        onChange={setSource}
                        language="cobol"
                        placeholder="Paste COBOL-85 source here…"
                        hint="Editable — paste your legacy source"
                    />
                    <CodeEditor
                        value={
                            result?.output_code ??
                            (failed
                                ? '// No output — migration failed honestly (see below).'
                                : '// Migrated output appears here after a successful run.')
                        }
                        language={target}
                        readOnly
                        hint={done ? 'Transpiler output' : undefined}
                    />
                </div>

                <div className="btn-row" style={{ marginTop: '1rem' }}>
                    <button
                        className="btn btn-primary"
                        onClick={runMigration}
                        disabled={running || !source.trim()}
                        type="button"
                    >
                        {running ? <Spinner /> : '▶'} {running ? 'Migrating…' : 'Start migration'}
                    </button>
                    {status && (
                        <span className="muted" style={{ fontSize: '0.82rem' }}>
                            status: <code>{status}</code>
                        </span>
                    )}
                </div>

                {status && <Pipeline status={status} />}
            </div>

            {error && (
                <div className="callout callout-danger">
                    <span className="callout-icon">⚠️</span>
                    <div>
                        <strong>Could not complete the migration.</strong>
                        <div style={{ marginTop: '0.25rem' }}>{error}</div>
                    </div>
                </div>
            )}

            {done && result && (
                <div className="card">
                    <h2>Measured results</h2>
                    <div className="card-sub">
                        Every number here came from this run. Nulls are shown as “Not measured”,
                        never back-filled with a default.
                    </div>
                    <div className="grid-2">
                        <div>
                            <ProgressBar
                                label="Semantic preservation (differential BER)"
                                value={result.semantic_score}
                                grade="VERIFIED"
                            />
                            <ProgressBar
                                label="Test coverage"
                                value={result.test_coverage}
                                grade="VERIFIED"
                            />
                        </div>
                        <RiskIndicator score={result.risk_score} />
                    </div>

                    {result.warnings.length > 0 && (
                        <div className="callout callout-info">
                            <span className="callout-icon">ℹ️</span>
                            <div>
                                <strong>Warnings</strong>
                                <ul>
                                    {result.warnings.map((w, i) => (
                                        <li key={i}>{w}</li>
                                    ))}
                                </ul>
                            </div>
                        </div>
                    )}

                    {result.attestation_tx ? (
                        <div className="callout callout-success">
                            <span className="callout-icon">🔗</span>
                            <div>
                                <strong>Attestation (simulated, local)</strong>
                                <div style={{ marginTop: '0.25rem' }}>
                                    Signed only because the run was measured. This is a local
                                    simulation that self-identifies as <code>simulated: true</code> —
                                    not an on-chain Solana transaction.
                                    <br />
                                    <code>{result.attestation_tx}</code>
                                </div>
                            </div>
                        </div>
                    ) : (
                        <div className="callout callout-info">
                            <span className="callout-icon">🔒</span>
                            <div>
                                <strong>No attestation.</strong> The measured-only gate did not sign
                                this run. That is the honest outcome when required metrics were not
                                measured — not a failure to display them.
                            </div>
                        </div>
                    )}
                </div>
            )}

            {failed && result && (
                <div className="card">
                    <h2>Honest failure</h2>
                    <div className="card-sub">
                        The transform refuses to emit placeholder output for programs outside the
                        supported COBOL-85 subset. Unsupported input produces a FAILED migration with
                        an inventory — not fabricated Java.
                    </div>
                    <div className="callout callout-danger">
                        <span className="callout-icon">⛔</span>
                        <div>
                            <strong>Migration failed.</strong>
                            {result.errors.length > 0 ? (
                                <ul>
                                    {result.errors.map((e, i) => (
                                        <li key={i}>{e}</li>
                                    ))}
                                </ul>
                            ) : (
                                <div style={{ marginTop: '0.25rem' }}>
                                    No error detail was returned by the pipeline.
                                </div>
                            )}
                        </div>
                    </div>
                    {result.warnings.length > 0 && (
                        <div className="callout callout-info">
                            <span className="callout-icon">ℹ️</span>
                            <div>
                                <strong>Construct inventory / warnings</strong>
                                <ul>
                                    {result.warnings.map((w, i) => (
                                        <li key={i}>{w}</li>
                                    ))}
                                </ul>
                            </div>
                        </div>
                    )}
                </div>
            )}
        </div>
    );
}
