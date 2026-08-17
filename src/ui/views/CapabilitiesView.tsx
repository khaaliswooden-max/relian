import { useEffect, useState } from 'react';
import { api } from '../api';
import { OUT_OF_SCOPE_CONSTRUCTS, SUPPORTED_CONSTRUCTS } from '../constants';
import type { LanguageMatrix, Template } from '../types';

export function CapabilitiesView(): JSX.Element {
    const [templates, setTemplates] = useState<Template[] | null>(null);
    const [langs, setLangs] = useState<LanguageMatrix | null>(null);
    const [apiError, setApiError] = useState<string | null>(null);

    useEffect(() => {
        let alive = true;
        Promise.all([api.templates(), api.languages()])
            .then(([t, l]) => {
                if (!alive) return;
                setTemplates(t.templates);
                setLangs(l);
            })
            .catch((e) => alive && setApiError(e instanceof Error ? e.message : 'unavailable'));
        return () => {
            alive = false;
        };
    }, []);

    return (
        <div>
            <div className="section-head">
                <h1>Capabilities &amp; scope</h1>
                <p>
                    “Supported” means only what the sealed benchmark corpus exercises. Everything else
                    is “planned”. A language pair is not called supported until RELIAN-BENCH covers it
                    and held-out CI passes.
                </p>
            </div>

            <div className="card">
                <h2>Supported COBOL-85 subset (today)</h2>
                <div className="card-sub">
                    Constructs the deterministic transpiler faithfully migrates. Anything outside this
                    set produces an honest FAILED migration, not approximate output.
                </div>
                <div className="feature-list">
                    {SUPPORTED_CONSTRUCTS.map((c) => (
                        <div className="feature-item" key={c}>
                            <span className="mark mark-yes">✓</span> {c}
                        </div>
                    ))}
                </div>
                <div style={{ marginTop: '1rem' }}>
                    <strong style={{ fontSize: '0.9rem' }}>Explicitly out of scope</strong>
                </div>
                <div className="feature-list">
                    {OUT_OF_SCOPE_CONSTRUCTS.map((c) => (
                        <div className="feature-item" key={c}>
                            <span className="mark mark-no">✗</span> {c}
                        </div>
                    ))}
                </div>
            </div>

            {apiError ? (
                <div className="card">
                    <div className="callout callout-info">
                        <span className="callout-icon">🔌</span>
                        <div>
                            Templates and language matrix are served by the API, which is currently
                            unreachable — {apiError}.
                        </div>
                    </div>
                </div>
            ) : (
                <>
                    <div className="card">
                        <h2>Language matrix</h2>
                        <div className="card-sub">
                            Live from <code>/api/v1/languages</code>. Status badges reflect what is
                            actually wired, not roadmap intent.
                        </div>
                        <div className="grid-2">
                            <LangColumn title="Source languages" entries={langs?.source_languages} />
                            <LangColumn title="Target languages" entries={langs?.target_languages} />
                        </div>
                    </div>

                    <div className="card">
                        <h2>Industry templates</h2>
                        <div className="card-sub">Live from <code>/api/v1/templates</code>.</div>
                        <div className="table-wrap">
                            <table>
                                <thead>
                                    <tr>
                                        <th>Template</th>
                                        <th>Pair</th>
                                        <th>Patterns</th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {(templates ?? []).map((t) => (
                                        <tr key={t.name}>
                                            <td>
                                                {t.description}
                                                <div
                                                    className="muted"
                                                    style={{ fontSize: '0.72rem' }}
                                                >
                                                    {t.name}
                                                </div>
                                            </td>
                                            <td className="num">
                                                {t.source_language} → {t.target_language}
                                            </td>
                                            <td className="muted" style={{ whiteSpace: 'normal' }}>
                                                {t.patterns.join(', ')}
                                            </td>
                                        </tr>
                                    ))}
                                    {templates && templates.length === 0 && (
                                        <tr>
                                            <td colSpan={3} className="muted">
                                                No templates returned.
                                            </td>
                                        </tr>
                                    )}
                                </tbody>
                            </table>
                        </div>
                    </div>
                </>
            )}
        </div>
    );
}

function LangColumn({
    title,
    entries,
}: {
    title: string;
    entries: LanguageMatrix['source_languages'] | undefined;
}): JSX.Element {
    return (
        <div>
            <strong style={{ fontSize: '0.9rem' }}>{title}</strong>
            <div className="feature-list" style={{ gridTemplateColumns: '1fr' }}>
                {(entries ?? []).map((e) => (
                    <div className="feature-item" key={e.code} style={{ justifyContent: 'space-between' }}>
                        <span>{e.name}</span>
                        <span className={`badge badge-${e.status}`}>{e.status}</span>
                    </div>
                ))}
            </div>
        </div>
    );
}
