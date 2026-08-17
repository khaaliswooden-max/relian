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

export function Pipeline({ status }: { status: MigrationStatus | null }): JSX.Element {
    const failed = status === 'failed';
    const activeIdx = status ? stageIndexForStatus(status) : -1;
    return (
        <div className="pipeline">
            {PIPELINE_STAGES.map((stage, i) => {
                let cls = 'stage';
                if (failed && i <= (activeIdx < 0 ? STAGE_ORDER.length : activeIdx)) {
                    // On failure, mark the reached stages; the failing one is red.
                    cls += activeIdx >= 0 && i === activeIdx ? ' failed' : ' done';
                } else if (activeIdx >= 0) {
                    if (i < activeIdx) cls += ' done';
                    else if (i === activeIdx) cls += status === 'completed' ? ' done' : ' active';
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
