import React, { useState, useEffect } from 'react';
import { createRoot } from 'react-dom/client';

// ============================================================================
// Types
// ============================================================================

interface Migration {
    migration_id: string;
    status: string;
    semantic_score: number;
    risk_score: number;
    test_coverage: number;
    attestation_tx: string | null;
    output_code: string | null;
}

interface Template {
    name: string;
    description: string;
    source_language: string;
    target_language: string;
}

// ============================================================================
// API Client
// ============================================================================

const API_BASE = 'http://localhost:8000/api/v1';

const api = {
    async startMigration(sourceCode: string, targetLanguage: string, template?: string) {
        const response = await fetch(`${API_BASE}/migrate`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                source_code: sourceCode,
                target_language: targetLanguage,
                template: template,
            }),
        });
        return response.json();
    },

    async getMigrationStatus(migrationId: string) {
        const response = await fetch(`${API_BASE}/migrate/${migrationId}`);
        return response.json();
    },

    async analyzeCode(sourceCode: string) {
        const response = await fetch(`${API_BASE}/analyze`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ source_code: sourceCode }),
        });
        return response.json();
    },

    async getTemplates() {
        const response = await fetch(`${API_BASE}/templates`);
        return response.json();
    },

    async getMetrics() {
        const response = await fetch(`${API_BASE}/metrics`);
        return response.json();
    },
};

// ============================================================================
// Components
// ============================================================================

const Header: React.FC = () => (
    <header className="header">
        <div className="header-content">
            <div className="logo">
                <span className="logo-icon">◈</span>
                <span className="logo-text">Relian</span>
                <span className="logo-badge">BETA</span>
            </div>
            <nav className="nav">
                <a href="#migrate" className="nav-link active">Migrate</a>
                <a href="#analyze" className="nav-link">Analyze</a>
                <a href="#templates" className="nav-link">Templates</a>
                <a href="#docs" className="nav-link">Docs</a>
            </nav>
        </div>
    </header>
);

const MetricsCard: React.FC<{ title: string; value: string | number; unit?: string; trend?: string }> = ({
    title, value, unit, trend
}) => (
    <div className="metrics-card">
        <div className="metrics-title">{title}</div>
        <div className="metrics-value">
            {value}
            {unit && <span className="metrics-unit">{unit}</span>}
        </div>
        {trend && <div className="metrics-trend">{trend}</div>}
    </div>
);

const CodeEditor: React.FC<{
    value: string;
    onChange: (value: string) => void;
    language: string;
    placeholder?: string;
    readOnly?: boolean;
}> = ({ value, onChange, language, placeholder, readOnly }) => (
    <div className="code-editor">
        <div className="code-editor-header">
            <span className="code-language">{language.toUpperCase()}</span>
            {!readOnly && <span className="code-hint">Paste your legacy code here</span>}
        </div>
        <textarea
            className="code-textarea"
            value={value}
            onChange={(e) => onChange(e.target.value)}
            placeholder={placeholder}
            readOnly={readOnly}
            spellCheck={false}
        />
    </div>
);

const RiskIndicator: React.FC<{ score: number }> = ({ score }) => {
    const level = score < 25 ? 'low' : score < 50 ? 'medium' : score < 75 ? 'high' : 'critical';
    return (
        <div className={`risk-indicator risk-${level}`}>
            <span className="risk-label">Risk Level</span>
            <span className="risk-score">{score.toFixed(1)}</span>
            <span className="risk-level">{level.toUpperCase()}</span>
        </div>
    );
};

const ProgressBar: React.FC<{ value: number; label: string }> = ({ value, label }) => (
    <div className="progress-container">
        <div className="progress-label">
            <span>{label}</span>
            <span>{value.toFixed(1)}%</span>
        </div>
        <div className="progress-bar">
            <div className="progress-fill" style={{ width: `${value}%` }} />
        </div>
    </div>
);

const MigrationPanel: React.FC = () => {
    const [sourceCode, setSourceCode] = useState<string>(SAMPLE_COBOL);
    const [targetLanguage, setTargetLanguage] = useState<string>('java');
    const [template, setTemplate] = useState<string>('banking');
    const [migration, setMigration] = useState<Migration | null>(null);
    const [loading, setLoading] = useState<boolean>(false);
    const [error, setError] = useState<string | null>(null);

    const handleMigrate = async () => {
        setLoading(true);
        setError(null);
        try {
            const result = await api.startMigration(sourceCode, targetLanguage, template);
            
            // Poll for completion
            let status = result;
            while (status.status === 'pending' || status.status === 'processing') {
                await new Promise(resolve => setTimeout(resolve, 1000));
                status = await api.getMigrationStatus(result.migration_id);
            }
            
            setMigration(status);
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Migration failed');
        } finally {
            setLoading(false);
        }
    };

    return (
        <div className="migration-panel">
            <div className="panel-header">
                <h2>Legacy Code Migration</h2>
                <p>Transform your legacy systems with AI-powered semantic preservation</p>
            </div>

            <div className="migration-config">
                <div className="config-row">
                    <div className="config-item">
                        <label>Target Language</label>
                        <select value={targetLanguage} onChange={e => setTargetLanguage(e.target.value)}>
                            <option value="java">Java</option>
                            <option value="python">Python</option>
                            <option value="rust">Rust (Coming Soon)</option>
                            <option value="csharp">C# (Coming Soon)</option>
                        </select>
                    </div>
                    <div className="config-item">
                        <label>Industry Template</label>
                        <select value={template} onChange={e => setTemplate(e.target.value)}>
                            <option value="banking">Banking & Financial</option>
                            <option value="government">Government & Defense</option>
                            <option value="healthcare">Healthcare</option>
                            <option value="manufacturing">Manufacturing</option>
                            <option value="insurance">Insurance</option>
                        </select>
                    </div>
                </div>
            </div>

            <div className="code-panels">
                <div className="code-panel">
                    <h3>Source Code (COBOL)</h3>
                    <CodeEditor
                        value={sourceCode}
                        onChange={setSourceCode}
                        language="cobol"
                        placeholder="Paste your COBOL code here..."
                    />
                </div>
                <div className="code-panel">
                    <h3>Migrated Code ({targetLanguage.toUpperCase()})</h3>
                    <CodeEditor
                        value={migration?.output_code || '// Migration output will appear here...'}
                        onChange={() => {}}
                        language={targetLanguage}
                        readOnly
                    />
                </div>
            </div>

            <div className="migration-actions">
                <button
                    className="btn-primary"
                    onClick={handleMigrate}
                    disabled={loading || !sourceCode.trim()}
                >
                    {loading ? 'Migrating...' : 'Start Migration'}
                </button>
            </div>

            {error && (
                <div className="error-message">
                    <span>⚠️</span> {error}
                </div>
            )}

            {migration && migration.status === 'completed' && (
                <div className="migration-results">
                    <h3>Migration Results</h3>
                    <div className="results-grid">
                        <ProgressBar value={migration.semantic_score} label="Semantic Preservation" />
                        <ProgressBar value={migration.test_coverage} label="Test Coverage" />
                        <RiskIndicator score={migration.risk_score} />
                    </div>
                    {migration.attestation_tx && (
                        <div className="attestation-info">
                            <span className="attestation-icon">🔗</span>
                            <span>Blockchain Attestation: </span>
                            <code>{migration.attestation_tx.substring(0, 16)}...</code>
                        </div>
                    )}
                </div>
            )}
        </div>
    );
};

const Dashboard: React.FC = () => {
    const [metrics, setMetrics] = useState<any>(null);

    useEffect(() => {
        api.getMetrics().then(setMetrics).catch(console.error);
    }, []);

    return (
        <div className="dashboard">
            <div className="metrics-grid">
                <MetricsCard title="Total Migrations" value={metrics?.total_migrations || 0} />
                <MetricsCard title="Success Rate" value={metrics?.average_semantic_score || 95} unit="%" />
                <MetricsCard title="Avg Coverage" value={metrics?.average_test_coverage || 80} unit="%" />
                <MetricsCard title="LOC Processed" value={metrics?.total_loc_processed || 0} />
            </div>
        </div>
    );
};

// ============================================================================
// Main App
// ============================================================================

const App: React.FC = () => {
    return (
        <div className="app">
            <Header />
            <main className="main">
                <Dashboard />
                <MigrationPanel />
            </main>
            <footer className="footer">
                <p>© 2025 Zuup, LLC. All rights reserved.</p>
                <p>Relian™ - Universal Legacy Refactoring Substrate</p>
            </footer>
        </div>
    );
};

// ============================================================================
// Sample Data
// ============================================================================

const SAMPLE_COBOL = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEREST-CALC.
       AUTHOR. RELIAN-DEMO.
      *
      * Banking Interest Calculation Program
      * Calculates compound interest for savings accounts
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PRINCIPAL       PIC 9(10)V99 VALUE ZEROS.
       01  WS-RATE            PIC 9(2)V9(4) VALUE ZEROS.
       01  WS-TIME            PIC 9(3) VALUE ZEROS.
       01  WS-INTEREST        PIC 9(12)V99 VALUE ZEROS.
       01  WS-AMOUNT          PIC 9(12)V99 VALUE ZEROS.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "INTEREST CALCULATION PROGRAM".
           MOVE 10000.00 TO WS-PRINCIPAL.
           MOVE 5.25 TO WS-RATE.
           MOVE 12 TO WS-TIME.
           PERFORM CALCULATE-INTEREST.
           DISPLAY "PRINCIPAL: " WS-PRINCIPAL.
           DISPLAY "INTEREST: " WS-INTEREST.
           DISPLAY "TOTAL: " WS-AMOUNT.
           STOP RUN.
           
       CALCULATE-INTEREST.
           COMPUTE WS-AMOUNT = WS-PRINCIPAL * 
               (1 + WS-RATE / 100) ** WS-TIME.
           SUBTRACT WS-PRINCIPAL FROM WS-AMOUNT 
               GIVING WS-INTEREST.
`;

// ============================================================================
// Styles (injected)
// ============================================================================

const styles = `
:root {
    --primary: #6366f1;
    --primary-dark: #4f46e5;
    --secondary: #0ea5e9;
    --success: #10b981;
    --warning: #f59e0b;
    --danger: #ef4444;
    --bg-dark: #0f172a;
    --bg-card: #1e293b;
    --bg-input: #334155;
    --text-primary: #f8fafc;
    --text-secondary: #94a3b8;
    --border: #475569;
    --font-mono: 'JetBrains Mono', 'Fira Code', monospace;
}

* {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
}

body {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
    background: var(--bg-dark);
    color: var(--text-primary);
    line-height: 1.6;
}

.app {
    min-height: 100vh;
    display: flex;
    flex-direction: column;
}

.header {
    background: linear-gradient(135deg, var(--bg-card) 0%, var(--bg-dark) 100%);
    border-bottom: 1px solid var(--border);
    padding: 1rem 2rem;
    position: sticky;
    top: 0;
    z-index: 100;
}

.header-content {
    max-width: 1400px;
    margin: 0 auto;
    display: flex;
    justify-content: space-between;
    align-items: center;
}

.logo {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    font-size: 1.5rem;
    font-weight: 700;
}

.logo-icon {
    color: var(--primary);
    font-size: 2rem;
}

.logo-text {
    background: linear-gradient(135deg, var(--primary) 0%, var(--secondary) 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
}

.logo-badge {
    font-size: 0.6rem;
    padding: 0.2rem 0.5rem;
    background: var(--warning);
    color: var(--bg-dark);
    border-radius: 4px;
    font-weight: 600;
}

.nav {
    display: flex;
    gap: 2rem;
}

.nav-link {
    color: var(--text-secondary);
    text-decoration: none;
    font-weight: 500;
    transition: color 0.2s;
}

.nav-link:hover, .nav-link.active {
    color: var(--primary);
}

.main {
    flex: 1;
    max-width: 1400px;
    margin: 0 auto;
    padding: 2rem;
    width: 100%;
}

.dashboard {
    margin-bottom: 2rem;
}

.metrics-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 1rem;
}

.metrics-card {
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 12px;
    padding: 1.5rem;
    text-align: center;
}

.metrics-title {
    color: var(--text-secondary);
    font-size: 0.875rem;
    margin-bottom: 0.5rem;
}

.metrics-value {
    font-size: 2rem;
    font-weight: 700;
    color: var(--primary);
}

.metrics-unit {
    font-size: 1rem;
    color: var(--text-secondary);
}

.migration-panel {
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 16px;
    padding: 2rem;
}

.panel-header {
    margin-bottom: 2rem;
}

.panel-header h2 {
    font-size: 1.5rem;
    margin-bottom: 0.5rem;
}

.panel-header p {
    color: var(--text-secondary);
}

.migration-config {
    margin-bottom: 1.5rem;
}

.config-row {
    display: flex;
    gap: 1rem;
    flex-wrap: wrap;
}

.config-item {
    flex: 1;
    min-width: 200px;
}

.config-item label {
    display: block;
    margin-bottom: 0.5rem;
    color: var(--text-secondary);
    font-size: 0.875rem;
}

.config-item select {
    width: 100%;
    padding: 0.75rem;
    background: var(--bg-input);
    border: 1px solid var(--border);
    border-radius: 8px;
    color: var(--text-primary);
    font-size: 1rem;
}

.code-panels {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 1rem;
    margin-bottom: 1.5rem;
}

@media (max-width: 900px) {
    .code-panels {
        grid-template-columns: 1fr;
    }
}

.code-panel h3 {
    font-size: 1rem;
    margin-bottom: 0.5rem;
    color: var(--text-secondary);
}

.code-editor {
    background: var(--bg-dark);
    border: 1px solid var(--border);
    border-radius: 8px;
    overflow: hidden;
}

.code-editor-header {
    display: flex;
    justify-content: space-between;
    padding: 0.5rem 1rem;
    background: var(--bg-input);
    border-bottom: 1px solid var(--border);
}

.code-language {
    font-size: 0.75rem;
    font-weight: 600;
    color: var(--secondary);
}

.code-hint {
    font-size: 0.75rem;
    color: var(--text-secondary);
}

.code-textarea {
    width: 100%;
    min-height: 300px;
    padding: 1rem;
    background: transparent;
    border: none;
    color: var(--text-primary);
    font-family: var(--font-mono);
    font-size: 0.875rem;
    resize: vertical;
    outline: none;
}

.migration-actions {
    display: flex;
    gap: 1rem;
    margin-bottom: 1.5rem;
}

.btn-primary {
    padding: 1rem 2rem;
    background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%);
    border: none;
    border-radius: 8px;
    color: white;
    font-size: 1rem;
    font-weight: 600;
    cursor: pointer;
    transition: transform 0.2s, box-shadow 0.2s;
}

.btn-primary:hover:not(:disabled) {
    transform: translateY(-2px);
    box-shadow: 0 4px 12px rgba(99, 102, 241, 0.4);
}

.btn-primary:disabled {
    opacity: 0.6;
    cursor: not-allowed;
}

.error-message {
    background: rgba(239, 68, 68, 0.1);
    border: 1px solid var(--danger);
    border-radius: 8px;
    padding: 1rem;
    color: var(--danger);
    margin-bottom: 1rem;
}

.migration-results {
    background: var(--bg-dark);
    border-radius: 8px;
    padding: 1.5rem;
}

.migration-results h3 {
    margin-bottom: 1rem;
}

.results-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 1.5rem;
    margin-bottom: 1rem;
}

.progress-container {
    margin-bottom: 1rem;
}

.progress-label {
    display: flex;
    justify-content: space-between;
    margin-bottom: 0.5rem;
    font-size: 0.875rem;
}

.progress-bar {
    height: 8px;
    background: var(--bg-input);
    border-radius: 4px;
    overflow: hidden;
}

.progress-fill {
    height: 100%;
    background: linear-gradient(90deg, var(--primary) 0%, var(--success) 100%);
    border-radius: 4px;
    transition: width 0.5s ease;
}

.risk-indicator {
    text-align: center;
    padding: 1rem;
    border-radius: 8px;
    background: var(--bg-input);
}

.risk-indicator.risk-low { border-left: 4px solid var(--success); }
.risk-indicator.risk-medium { border-left: 4px solid var(--warning); }
.risk-indicator.risk-high { border-left: 4px solid #f97316; }
.risk-indicator.risk-critical { border-left: 4px solid var(--danger); }

.risk-label {
    display: block;
    font-size: 0.75rem;
    color: var(--text-secondary);
}

.risk-score {
    display: block;
    font-size: 2rem;
    font-weight: 700;
}

.risk-level {
    display: block;
    font-size: 0.75rem;
    font-weight: 600;
}

.attestation-info {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 1rem;
    background: rgba(16, 185, 129, 0.1);
    border: 1px solid var(--success);
    border-radius: 8px;
    margin-top: 1rem;
}

.attestation-info code {
    font-family: var(--font-mono);
    background: var(--bg-input);
    padding: 0.25rem 0.5rem;
    border-radius: 4px;
}

.footer {
    text-align: center;
    padding: 2rem;
    color: var(--text-secondary);
    font-size: 0.875rem;
    border-top: 1px solid var(--border);
}
`;

// Inject styles
const styleSheet = document.createElement('style');
styleSheet.textContent = styles;
document.head.appendChild(styleSheet);

// Add fonts
const fontLink = document.createElement('link');
fontLink.href = 'https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono&display=swap';
fontLink.rel = 'stylesheet';
document.head.appendChild(fontLink);

// ============================================================================
// Render
// ============================================================================

const container = document.getElementById('root');
if (container) {
    const root = createRoot(container);
    root.render(<App />);
}
