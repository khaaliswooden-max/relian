import { useState } from 'react';
import { AnalyzeView } from './views/AnalyzeView';
import { BenchmarkView } from './views/BenchmarkView';
import { CapabilitiesView } from './views/CapabilitiesView';
import { MigrateView } from './views/MigrateView';

type View = 'migrate' | 'analyze' | 'benchmark' | 'capabilities';

const NAV: { id: View; label: string }[] = [
    { id: 'migrate', label: 'Migrate' },
    { id: 'analyze', label: 'Analyze' },
    { id: 'benchmark', label: 'Benchmark' },
    { id: 'capabilities', label: 'Capabilities' },
];

function StatusBanner(): JSX.Element {
    return (
        <div className="status-banner" role="note">
            <span className="dot" />
            <div>
                <strong>Pre-MVP research prototype.</strong> No completed customer migrations, no
                customers. Every quality metric shown is measured this run or labeled “Not measured” —
                there are no default or placeholder numbers. Attestation is simulated locally and says
                so.
            </div>
        </div>
    );
}

function Header({ view, onNav }: { view: View; onNav: (v: View) => void }): JSX.Element {
    return (
        <header className="header">
            <div className="header-content">
                <div className="logo">
                    <span className="logo-icon">◈</span>
                    <span className="logo-text">Relian</span>
                    <span className="logo-badge">PRE-MVP</span>
                </div>
                <nav className="nav">
                    {NAV.map((n) => (
                        <button
                            key={n.id}
                            className={`nav-link${view === n.id ? ' active' : ''}`}
                            onClick={() => onNav(n.id)}
                            type="button"
                        >
                            {n.label}
                        </button>
                    ))}
                </nav>
            </div>
        </header>
    );
}

export function App(): JSX.Element {
    const [view, setView] = useState<View>('migrate');

    return (
        <div className="app">
            <Header view={view} onNav={setView} />
            <StatusBanner />
            <main className="main">
                {view === 'migrate' && <MigrateView />}
                {view === 'analyze' && <AnalyzeView />}
                {view === 'benchmark' && <BenchmarkView />}
                {view === 'capabilities' && <CapabilitiesView />}
            </main>
            <footer className="footer">
                <p>
                    Relian™ — Legacy Refactoring Substrate ·{' '}
                    <a href="https://github.com/khaaliswooden-max/relian">source</a>
                </p>
                <p>© 2025–2026 Zuup, LLC / Visionblox LLC. Proprietary. All rights reserved.</p>
            </footer>
        </div>
    );
}
