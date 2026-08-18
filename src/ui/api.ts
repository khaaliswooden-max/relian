// Typed API client for the Relian backend (src/api/main.py).
//
// The base URL can be overridden at build time with VITE_API_BASE so the same
// bundle can point at a local dev API or a container network address.

import type {
    AnalysisResult,
    AssessmentResult,
    LanguageMatrix,
    MigrationResult,
    MigrationStart,
    PlatformMetrics,
    Template,
} from './types';

const API_BASE: string =
    (import.meta.env?.VITE_API_BASE as string | undefined) ?? 'http://localhost:8000';

export class ApiError extends Error {
    constructor(
        message: string,
        readonly status?: number,
    ) {
        super(message);
        this.name = 'ApiError';
    }
}

async function request<T>(path: string, init?: RequestInit): Promise<T> {
    let response: Response;
    try {
        response = await fetch(`${API_BASE}${path}`, {
            headers: { 'Content-Type': 'application/json' },
            ...init,
        });
    } catch (err) {
        throw new ApiError(
            `Cannot reach the Relian API at ${API_BASE}. Is the backend running ` +
                `(uvicorn src.api.main:app)?`,
        );
    }
    if (!response.ok) {
        let detail = `${response.status} ${response.statusText}`;
        try {
            const body = (await response.json()) as { detail?: string };
            if (body?.detail) detail = body.detail;
        } catch {
            /* body was not JSON; keep the status text */
        }
        throw new ApiError(detail, response.status);
    }
    return (await response.json()) as T;
}

export const api = {
    health(): Promise<{ status: string; version: string; timestamp: string }> {
        return request('/health');
    },

    startMigration(input: {
        source_code: string;
        source_language?: string;
        target_language: string;
        template?: string;
    }): Promise<MigrationStart> {
        return request('/api/v1/migrate', {
            method: 'POST',
            body: JSON.stringify({ source_language: 'cobol', ...input }),
        });
    },

    getMigration(id: string): Promise<MigrationResult> {
        return request(`/api/v1/migrate/${id}`);
    },

    analyze(source_code: string, language = 'cobol'): Promise<AnalysisResult> {
        return request('/api/v1/analyze', {
            method: 'POST',
            body: JSON.stringify({ source_code, language }),
        });
    },

    // Runs the read-only, offline assessment engine over the bundled Meridian
    // MUD demo corpus (examples/demo) and returns the measured portfolio result.
    assessDemo(): Promise<AssessmentResult> {
        return request('/api/v1/assess/demo');
    },

    templates(): Promise<{ templates: Template[] }> {
        return request('/api/v1/templates');
    },

    languages(): Promise<LanguageMatrix> {
        return request('/api/v1/languages');
    },

    metrics(): Promise<PlatformMetrics> {
        return request('/api/v1/metrics');
    },
};
