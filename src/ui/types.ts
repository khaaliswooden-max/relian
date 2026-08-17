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

// ---------------------------------------------------------------------------
// Assessment (src.assessment) — the read-only, offline legacy-code assessment
// that backs the Meridian MUD demo. These mirror the subset of the assessment
// bundle the Assess view renders; the API returns the full bundle, and any
// field the UI does not model is simply carried along untouched.
// ---------------------------------------------------------------------------

// A measured scalar as emitted by src.assessment: value + Trutina grade +
// provenance. `value` is null when nothing could be measured (never a filler).
export interface Measured<T = number> {
    value: T | null;
    grade: Grade;
    provenance: string;
}

export type RiskTier = 'LOW' | 'MED' | 'HIGH' | 'BLOCKED';

export interface CoverageBlock {
    program_id: string;
    coverage_ratio: Measured;
    supported_statements: number;
    total_statements: number;
    method: string;
    parse_ok: boolean;
    unsupported_ranked: { verb: string; count: number }[];
}

export interface RiskBlock {
    tier: RiskTier;
    rule: string;
    grade: Grade;
    provenance: string;
}

export interface ProgramAssessment {
    program_id: string;
    coverage: CoverageBlock;
    risk: RiskBlock;
    loc: { physical: number; logical: number | null; logical_method: string };
    complexity: {
        cyclomatic: number;
        goto_count: number;
        max_nesting_depth: number;
        exec_sql_count: number;
        exec_cics_count: number;
    } | null;
}

export interface AssessmentBundle {
    schema_version: string;
    inventory: {
        file_count: number;
        manifest_hash: string;
        counts_by_kind: Record<string, number>;
    };
    programs: ProgramAssessment[];
    portfolio_coverage: CoverageBlock;
    portfolio_risk: RiskBlock & { inputs: Record<string, number> };
    quotable_loc: Measured;
    grammar_expansion_loc: Measured;
    notes: string[];
}

export interface AssessmentResult {
    root_label: string;
    report_hash: string;
    schema_version: string;
    programs_assessed: number;
    manifest_files: number;
    bundle: AssessmentBundle;
}
