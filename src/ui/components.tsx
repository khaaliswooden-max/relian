// Reusable presentational components.
//
// The recurring theme: a metric is measured or it is null. `MetricValue` and
// `ProgressBar` render an explicit "Not measured" affordance for null instead
// of substituting a number — the UI never invents a measurement (R1).

import { GRADE_META, PIPELINE_STAGES } from './constants';
import type { Grade, MigrationStatus } from './types';

export function GradeTag({ grade }: { grade: Grade }): JSX.Element {
    const meta = GRADE_META[grade];
    return (
        <span className="grade" style={{ color: meta.color }} title={meta.help}>
            {grade}
        </span>
    );
}

/** Renders a measured number, or an explicit "Not measured" for null. */
export function MetricValue({
    value,
    unit,
    digits = 1,
}: {
    value: number | null | undefined;
    unit?: string;
    digits?: number;
}): JSX.Element {
    if (value === null || value === undefined) {
        return <span className="stat-value unmeasured">Not measured</span>;
    }
    return (
        <span className="stat-value measured">
            {value.toFixed(digits)}
            {unit && <span className="stat-unit">{unit}</span>}
        </span>
    );
}

export function StatTile({
    title,
    value,
    unit,
    digits = 0,
    foot,
    grade,
}: {
    title: string;
    value: number | null | undefined;
    unit?: string;
    digits?: number;
    foot?: string;
    grade?: Grade;
}): JSX.Element {
    return (
        <div className="stat">
            <div className="stat-title">
                {title}
                {grade && <GradeTag grade={grade} />}
            </div>
            <MetricValue value={value} unit={unit} digits={digits} />
            {foot && <div className="stat-foot">{foot}</div>}
        </div>
    );
}

export function ProgressBar({
    label,
    value,
    grade,
}: {
    label: string;
    value: number | null;
    grade?: Grade;
}): JSX.Element {
    const measured = value !== null && value !== undefined;
    return (
        <div className="progress">
            <div className="progress-label">
                <span>
                    {label} {grade && measured && <GradeTag grade={grade} />}
                </span>
                <span className={measured ? '' : 'muted'}>
                    {measured ? `${value.toFixed(1)}%` : 'Not measured'}
                </span>
            </div>
            <div className="progress-track">
                <div
                    className={`progress-fill${measured ? '' : ' unmeasured'}`}
                    style={measured ? { width: `${Math.max(0, Math.min(100, value))}%` } : undefined}
                    title={measured ? undefined : 'No measurement recorded for this run'}
                />
            </div>
        </div>
    );
}

export function RiskIndicator({ score }: { score: number | null }): JSX.Element {
    if (score === null || score === undefined) {
        return (
            <div className="risk">
                <div className="risk-label">Risk score</div>
                <div className="risk-score muted" style={{ fontSize: '1rem', fontStyle: 'italic' }}>
                    Not measured
                </div>
            </div>
        );
    }
    const level = score < 25 ? 'low' : score < 50 ? 'medium' : score < 75 ? 'high' : 'critical';
    return (
        <div className={`risk ${level}`}>
            <div className="risk-label">
                Risk score <GradeTag grade="PLAUSIBLE" />
            </div>
            <div className="risk-score">{score.toFixed(1)}</div>
            <div className="risk-level">{level.toUpperCase()}</div>
        </div>
    );
}

export function CodeEditor({
    value,
    onChange,
    language,
    placeholder,
    readOnly,
    hint,
}: {
    value: string;
    onChange?: (v: string) => void;
    language: string;
    placeholder?: string;
    readOnly?: boolean;
    hint?: string;
}): JSX.Element {
    return (
        <div className="code-editor">
            <div className="code-editor-header">
                <span className="code-language">{language.toUpperCase()}</span>
                {hint && <span className="code-hint">{hint}</span>}
            </div>
            <textarea
                className="code-textarea"
                value={value}
                onChange={(e) => onChange?.(e.target.value)}
                placeholder={placeholder}
                readOnly={readOnly}
                spellCheck={false}
            />
        </div>
    );
}

const STAGE_ORDER = PIPELINE_STAGES.map((s) => s.key);

/** Maps a live status to an index in the stage list. */
function stageIndexForStatus(status: MigrationStatus): number {
    if (status === 'completed') return STAGE_ORDER.length - 1;
    const i = STAGE_ORDER.indexOf(status);
    return i;
}

export function Pipeline({
    status,
    lastActive,
}: {
    status: MigrationStatus | null;
    // The last in-progress stage seen before a terminal status arrived. Used to
    // attribute a failure to a stage, since the API's terminal `failed` status
    // does not itself name the stage that failed.
    lastActive?: MigrationStatus | null;
}): JSX.Element {
    const failed = status === 'failed';
    // Live progress index (not meaningful once failed — `failed` is not a stage).
    const liveIdx = status && !failed ? stageIndexForStatus(status) : -1;
    // Where the failure lands. If we never observed a stage (immediate failure,
    // status went straight to `failed` from `pending`), attribute it to the
    // first stage rather than pretending stages completed.
    let failIdx = -1;
    if (failed) {
        const li = lastActive ? stageIndexForStatus(lastActive) : -1;
        failIdx = li >= 0 ? li : 0;
    }
    return (
        <div className={`pipeline${failed ? ' pipeline-failed' : ''}`}>
            {PIPELINE_STAGES.map((stage, i) => {
                let cls = 'stage';
                if (failed) {
                    // Only stages strictly before the failing one are proven done.
                    // The failing stage is red; later stages stay neutral. Never
                    // paint an unreached or failed stage green (R2: honest failure).
                    if (i < failIdx) cls += ' done';
                    else if (i === failIdx) cls += ' failed';
                } else if (liveIdx >= 0) {
                    if (i < liveIdx) cls += ' done';
                    else if (i === liveIdx) cls += status === 'completed' ? ' done' : ' active';
                }
                return (
                    <div key={stage.key} className={cls} title={stage.blurb}>
                        <div className="stage-top">
                            <span className="stage-dot" />
                            <span className="stage-label">{stage.label}</span>
                        </div>
                        <div className="stage-blurb">{stage.blurb}</div>
                    </div>
                );
            })}
        </div>
    );
}

export function Spinner(): JSX.Element {
    return <span className="spinner" aria-label="loading" />;
}
