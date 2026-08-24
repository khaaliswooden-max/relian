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

// ---------------------------------------------------------------------------
// Section 0 — the plain-language layer, as data.
//
// Built by report.plain_summary() on the Python side, which is the single
// source the CLI's Markdown and DOCX render too. The wording, the tier labels
// and the construct glosses are decided there and carried here verbatim, so
// the tab and the report cannot describe the same run differently. Nothing in
// here is computed in the browser.
//
// The prose fields carry Markdown inline emphasis (**strong** and `code`),
// because they are the report's own sentences; AssessView renders those two
// forms and shows anything else verbatim.
// ---------------------------------------------------------------------------

export interface PlainGroup {
    tier: RiskTier;
    label: string;
    explanation: string;
    programs: number;
    program_ids: string[];
}

// `gloss` is null for a construct with no plain-language entry — the row then
// shows the count alone rather than a guessed explanation.
export interface PlainConstruct {
    construct: string;
    gloss: string | null;
    count: number;
}

export interface PlainSummary {
    title: string;
    intro: string;
    scope: string;
    // All four are null when no COBOL programs were found; `scope` says so.
    where_we_stand: {
        heading: string;
        grade: Grade;
        provenance: string;
        groups: PlainGroup[];
    } | null;
    in_the_way: {
        heading: string;
        constructs: PlainConstruct[];
        omitted: number;
    } | null;
    how_much: {
        heading: string;
        rows: { label: string; measured: Measured | null }[];
    } | null;
    limits: string | null;
}

export interface AssessmentResult {
    root_label: string;
    report_hash: string;
    schema_version: string;
    programs_assessed: number;
    manifest_files: number;
    bundle: AssessmentBundle;
    plain_summary: PlainSummary;
}
