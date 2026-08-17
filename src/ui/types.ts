// Shared types for the Relian frontend.
//
// These mirror the FastAPI response models in src/api/main.py. The cardinal
// rule from RELIAN-BENCH / CLAUDE.md holds here too: any quality metric is
// MEASURED or it is `null`. The `| null` on every score is load-bearing — the
// UI must render "Not measured" for a null, never a fabricated stand-in.

export type MigrationStatus =
    | 'pending'
    | 'processing'
    | 'parsing'
    | 'analyzing'
    | 'generating_tests'
    | 'transforming'
    | 'validating'
    | 'attesting'
    | 'completed'
    | 'failed'
    | 'unknown';

export interface MigrationStart {
    migration_id: string;
    status: MigrationStatus;
    message: string;
}

export interface MigrationResult {
    migration_id: string;
    status: MigrationStatus;
    source_hash: string | null;
    target_hash: string | null;
    // Measured or null. Never default to a number.
    semantic_score: number | null;
    risk_score: number | null;
    test_coverage: number | null;
    attestation_tx: string | null;
    output_code: string | null;
    errors: string[];
    warnings: string[];
}

export interface AnalysisResult {
    purpose: string;
    business_rules: string[];
    decision_trees: string[];
    edge_cases: string[];
    confidence: number | null;
    risk_score: number | null;
    risk_level: string | null;
    recommendations: string[];
}

export interface Template {
    name: string;
    description: string;
    source_language: string;
    target_language: string;
    patterns: string[];
}

export interface LanguageEntry {
    code: string;
    name: string;
    status: 'supported' | 'planned';
}

export interface LanguageMatrix {
    source_languages: LanguageEntry[];
    target_languages: LanguageEntry[];
}

export interface PlatformMetrics {
    total_migrations: number;
    successful_migrations: number;
    average_semantic_score: number | null;
    average_test_coverage: number | null;
    total_loc_processed: number;
}

// Trutina grade (R9): every externally visible number carries a grade.
export type Grade = 'VERIFIED' | 'PLAUSIBLE' | 'SPECULATIVE';
