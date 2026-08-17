// Static, provenance-bearing constants for the UI.
//
// The benchmark rows below are MEASURED results, committed and Ed25519-signed
// in bench/LEDGER_relian-bench-v1.0.json and reproduced in README.md. They are
// facts of record, not live values — the UI labels them as such and cites the
// ledger as provenance (R9). Nothing here is a fabricated or "target" number
// dressed up as a measurement (R1).

import type { Grade } from './types';

export const BENCH_VERSION = 'RELIAN-BENCH v1.0';
export const LEDGER_PATH = 'bench/LEDGER_relian-bench-v1.0.json';

export interface BenchRow {
    candidate: string;
    note: string;
    ber: string; // held-out Behavioral Equivalence Ratio
    build: string;
    branchCoverage: string;
    bar: 'PASS' | 'FAIL';
}

// Committed baseline — see README "Measured baseline" table.
export const BENCH_ROWS: BenchRow[] = [
    {
        candidate: 'B0 placeholder',
        note: 'pre-integration',
        ber: '0.0000',
        build: '1.0000',
        branchCoverage: 'n/a',
        bar: 'FAIL',
    },
    {
        candidate: 'B2 hand-written reference',
        note: 'human baseline',
        ber: '1.0000',
        build: '1.0000',
        branchCoverage: '0.8415',
        bar: 'PASS',
    },
    {
        candidate: 'C1 deterministic transpiler',
        note: 'pipeline core',
        ber: '1.0000 (300/300)',
        build: '1.0000',
        branchCoverage: '0.8824',
        bar: 'PASS',
    },
];

// Committed success criteria (targets, frozen BEFORE solution work). These are
// GATES, not achievements — rendered as targets, never as results.
export const BENCH_TARGETS = [
    { label: 'Semantic preservation (held-out BER)', target: '≥ 0.95' },
    { label: 'Build rate (supported scope)', target: '1.00' },
    { label: 'Branch coverage', target: '≥ 0.80' },
    { label: 'Defects', target: '< 5 / KLOC' },
];

// The 7-stage orchestrator pipeline (src/core/orchestrator.py MigrationStatus).
export interface PipelineStage {
    key: string;
    label: string;
    blurb: string;
}

export const PIPELINE_STAGES: PipelineStage[] = [
    { key: 'parsing', label: 'Parse', blurb: 'COBOL-85 ANTLR parser → AST' },
    { key: 'analyzing', label: 'Analyze', blurb: 'Heuristic risk; LLM semantics (informational only)' },
    { key: 'generating_tests', label: 'Tests', blurb: 'Symbolic test gen (empty until KLEE lands)' },
    { key: 'transforming', label: 'Transform', blurb: 'Deterministic C1 transpiler → target' },
    { key: 'validating', label: 'Validate', blurb: 'Differential execution vs. legacy oracle' },
    { key: 'attesting', label: 'Attest', blurb: 'Measured-gated; simulated locally' },
    { key: 'completed', label: 'Done', blurb: 'Attested or honest no-attestation' },
];

// Supported COBOL-85 subset vs. explicitly out-of-scope constructs. Scope
// honesty per README + CLAUDE.md: "supported" means only what the sealed
// corpus exercises (R2, R7).
export const SUPPORTED_CONSTRUCTS = [
    'COMP-3 arithmetic',
    'EVALUATE',
    'PERFORM VARYING',
    'OCCURS / SEARCH',
    'INSPECT',
    'Edited pictures',
];

export const OUT_OF_SCOPE_CONSTRUCTS = ['CICS', 'VSAM', 'JCL', 'Copybooks', 'Embedded SQL'];

// Trutina grade → human explanation. Attached to every visible number (R9).
export const GRADE_META: Record<Grade, { color: string; help: string }> = {
    VERIFIED: { color: 'var(--success)', help: 'Measured directly in this run.' },
    PLAUSIBLE: { color: 'var(--warning)', help: 'Heuristic estimate; not a ground-truth measurement.' },
    SPECULATIVE: { color: 'var(--danger)', help: 'Unvalidated projection.' },
};
