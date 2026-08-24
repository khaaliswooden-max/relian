"""Relian API - FastAPI backend for legacy migration platform."""

import os
from pathlib import Path
from typing import Any, Optional
import hashlib
from datetime import datetime, timezone

from fastapi import FastAPI, HTTPException, BackgroundTasks, UploadFile, File
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel, Field

# Repository root (…/relian) resolved from this file's location, so the demo
# assessment endpoint finds the bundled corpus regardless of the CWD uvicorn
# is launched from.
REPO_ROOT = Path(__file__).resolve().parents[2]
DEMO_CORPUS = REPO_ROOT / "examples" / "demo"

app = FastAPI(
    title="Relian API",
    description="Universal Legacy Refactoring Substrate - AI-Powered Migration Platform",
    version="0.1.0",
    docs_url="/docs",
    redoc_url="/redoc",
)

# CORS middleware for frontend
app.add_middleware(
    CORSMiddleware,
    allow_origins=[
        "http://localhost:3000",
        "http://localhost:5173",
        # 127.0.0.1 is a distinct origin from localhost to the browser, so allow
        # both spellings — dev servers bound to either host then work unchanged.
        "http://127.0.0.1:3000",
        "http://127.0.0.1:5173",
    ],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# In-memory storage for demo (use database in production)
migrations_db: dict = {}
analysis_cache: dict = {}


# ============================================================================
# Pydantic Models
# ============================================================================


class HealthResponse(BaseModel):
    """Health check response."""

    status: str
    version: str
    timestamp: str


class MigrationRequest(BaseModel):
    """Request to start a migration."""

    source_code: str = Field(..., description="Source code to migrate")
    source_language: str = Field(default="cobol", description="Source language")
    target_language: str = Field(default="java", description="Target language")
    template: Optional[str] = Field(None, description="Industry template to use")
    enable_blockchain: bool = Field(True, description="Enable blockchain attestation")
    generate_tests: bool = Field(True, description="Generate test suite")


class MigrationResponse(BaseModel):
    """Migration job response."""

    migration_id: str
    status: str
    message: str


class MigrationStatusResponse(BaseModel):
    """Detailed migration status."""

    migration_id: str
    status: str
    source_hash: Optional[str] = None
    target_hash: Optional[str] = None
    # CARDINAL RULE (RELIAN-BENCH): a metric is MEASURED or it is None -- the
    # same correction AnalysisResponse already carries below.
    #
    # These three were `float = 0.0`, which was both an R1 defect and a live
    # bug: run_migration() writes None into the job dict for every unmeasured
    # metric (and on the failure path writes None to all three), so building
    # this model raised a pydantic ValidationError rather than reporting
    # "not measured". WP-2.0.-3 made risk_score None unconditionally, which
    # turned that latent 500 into one on every completed migration.
    semantic_score: Optional[float] = None
    risk_score: Optional[float] = None
    test_coverage: Optional[float] = None
    attestation_tx: Optional[str] = None
    output_code: Optional[str] = None
    errors: list = []
    warnings: list = []


class AnalysisRequest(BaseModel):
    """Request for code analysis."""

    source_code: str
    language: str = "cobol"


class AnalysisResponse(BaseModel):
    """Code analysis response."""

    purpose: str
    business_rules: list
    decision_trees: list
    edge_cases: list
    # CARDINAL RULE (RELIAN-BENCH): a metric is MEASURED or it is None.
    confidence: Optional[float] = None
    risk_score: Optional[float] = None
    risk_level: Optional[str] = None
    recommendations: list = []


class AssessmentResponse(BaseModel):
    """Legacy-code assessment result (read-only, offline).

    `bundle` is the full, measured assessment produced by the assessment
    engine (src.assessment) — the same object the CLI serialises to
    assessment.json. Every number inside it already carries a Trutina grade
    and a provenance string; this endpoint adds no figures of its own.

    `plain_summary` is section 0 of the rendered report — the same findings in
    plain language — as data rather than Markdown. It comes from
    `report.plain_summary()`, the single source the CLI's Markdown and DOCX
    also render, so the tab and the report cannot drift into describing the
    same run differently. It restates the bundle and computes nothing: every
    figure in it is one already present above, and each `measured` value
    carries its own grade and provenance.
    """

    root_label: str
    report_hash: str
    schema_version: str
    programs_assessed: int
    manifest_files: int
    bundle: dict
    plain_summary: dict


class AttestationResponse(BaseModel):
    """Blockchain attestation response."""

    migration_id: str
    transaction_signature: str
    source_hash: str
    target_hash: str
    timestamp: str


# ============================================================================
# API Endpoints
# ============================================================================


@app.get("/", tags=["General"])
async def root():
    """Root endpoint with API information."""
    return {
        "name": "Relian API",
        "description": "Universal Legacy Refactoring Substrate",
        "version": "0.1.0",
        "docs": "/docs",
    }


@app.get("/health", response_model=HealthResponse, tags=["General"])
async def health_check():
    """Health check endpoint."""
    return HealthResponse(
        status="healthy",
        version="0.1.0",
        timestamp=datetime.now(timezone.utc).isoformat(),
    )


@app.post("/api/v1/migrate", response_model=MigrationResponse, tags=["Migration"])
async def start_migration(
    request: MigrationRequest, background_tasks: BackgroundTasks
):
    """
    Start a new migration job.

    This endpoint accepts source code and configuration, then starts
    an asynchronous migration job. Use the returned migration_id to
    check status via GET /api/v1/migrate/{migration_id}.
    """
    import hashlib

    # Generate migration ID
    migration_id = hashlib.sha256(
        f"{request.source_code[:100]}{datetime.now(timezone.utc).isoformat()}".encode()
    ).hexdigest()[:16]

    # Store initial state
    migrations_db[migration_id] = {
        "status": "pending",
        "source_code": request.source_code,
        "source_language": request.source_language,
        "target_language": request.target_language,
        "template": request.template,
        "enable_blockchain": request.enable_blockchain,
        "generate_tests": request.generate_tests,
        "created_at": datetime.now(timezone.utc).isoformat(),
    }

    # Start background migration
    background_tasks.add_task(run_migration, migration_id)

    return MigrationResponse(
        migration_id=migration_id,
        status="pending",
        message="Migration job started. Check status at /api/v1/migrate/{migration_id}",
    )


@app.get(
    "/api/v1/migrate/{migration_id}",
    response_model=MigrationStatusResponse,
    tags=["Migration"],
)
async def get_migration_status(migration_id: str):
    """Get the status of a migration job."""
    if migration_id not in migrations_db:
        raise HTTPException(status_code=404, detail="Migration not found")

    job = migrations_db[migration_id]
    return MigrationStatusResponse(
        migration_id=migration_id,
        status=job.get("status", "unknown"),
        source_hash=job.get("source_hash"),
        target_hash=job.get("target_hash"),
        # No `, 0.0` default: an absent key means the stage never ran, which
        # is "not measured", not "zero" (R1).
        semantic_score=job.get("semantic_score"),
        risk_score=job.get("risk_score"),
        test_coverage=job.get("test_coverage"),
        attestation_tx=job.get("attestation_tx"),
        output_code=job.get("output_code"),
        errors=job.get("errors", []),
        warnings=job.get("warnings", []),
    )


@app.post("/api/v1/analyze", response_model=AnalysisResponse, tags=["Analysis"])
async def analyze_code(request: AnalysisRequest):
    """
    Analyze source code to extract business logic.

    Parses the submitted source and reports that the parse succeeded. It
    returns NO risk figure: this endpoint scores nothing.

    Until WP-2.0.-3 it built `src.ml.risk_scorer.RiskScorer` and returned its
    `overall_score`, `risk_level` and `recommendations`. That scorer was
    deleted under R1 -- its confidence was a hardcoded 0.70 on this path and
    its "200+ metrics" were eighteen fields, several of them derived by
    multiplying another field by a constant. Substituting a different heuristic
    here would repeat the defect with new arithmetic, so the fields are None.

    Measured risk lives at `/api/v1/assess/*`, served by the assessment engine
    (`src.assessment`), where every number carries a Trutina grade and a
    provenance string (R9). See docs/R1_ML_DISPOSITION_2026-08.md.
    """
    try:
        from src.parsers.cobol import COBOLParser

        # Parse code. The parse is real; nothing downstream of it is scored.
        parser = COBOLParser()
        parser.parse_string(request.source_code)

        return AnalysisResponse(
            purpose="Static parse only. This endpoint measures nothing: "
                    "risk scoring was removed under R1 (WP-2.0.-3) and LLM "
                    "semantic analysis under R6 (WP-2.0.-2). For measured "
                    "risk use /api/v1/assess/*.",
            business_rules=[],   # empty is honest; canned strings are not
            decision_trees=[],
            edge_cases=[],
            confidence=None,     # unmeasured -> None, never a constant
            risk_score=None,     # not scored here -- see /api/v1/assess/*
            risk_level=None,
            recommendations=[],  # were generated from the deleted heuristic
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Analysis failed: {str(e)}")


@app.get("/api/v1/attestations/{migration_id}", tags=["Blockchain"])
async def get_attestation(migration_id: str):
    """Get blockchain attestation for a migration."""
    if migration_id not in migrations_db:
        raise HTTPException(status_code=404, detail="Migration not found")

    job = migrations_db[migration_id]
    if not job.get("attestation_tx"):
        raise HTTPException(
            status_code=404, detail="No attestation found for this migration"
        )

    return AttestationResponse(
        migration_id=migration_id,
        transaction_signature=job["attestation_tx"],
        source_hash=job.get("source_hash", ""),
        target_hash=job.get("target_hash", ""),
        timestamp=job.get("attested_at", datetime.now(timezone.utc).isoformat()),
    )


@app.get(
    "/api/v1/assess/demo",
    response_model=AssessmentResponse,
    tags=["Assessment"],
)
async def assess_demo():
    """Assess the bundled Meridian MUD demonstration corpus.

    Runs the read-only, offline assessment engine over ``examples/demo`` — the
    synthetic COBOL-85 code set that ships with this repository — and returns
    the measured portfolio result. This is the API equivalent of

        python3 -m src.assessment.cli examples/demo --out <dir>

    Nothing is written to disk and no network or model call is made (R6, R12):
    customer source stays inside the customer perimeter, and the demo corpus is
    synthetic so it never leaves it. Every figure in ``bundle`` is measured this
    run and carries its own Trutina grade and provenance — the endpoint mints no
    numbers of its own.
    """
    # Imported lazily so the rest of the API loads even if the assessment
    # package (or its optional deps) is unavailable in a given environment.
    from src.assessment.cli import assess_tree
    from src.assessment.models import canonical_json
    from src.assessment.report import plain_summary

    if not DEMO_CORPUS.is_dir():
        raise HTTPException(
            status_code=404,
            detail=f"Demo corpus not found at {DEMO_CORPUS.as_posix()}",
        )
    try:
        bundle, by_construct = assess_tree(DEMO_CORPUS)
    except Exception as e:  # pragma: no cover - surfaced to the client
        raise HTTPException(status_code=500, detail=f"Assessment failed: {e}")

    payload: dict[str, Any] = bundle.to_dict()
    # sha256 of this assessment's canonical JSON — the same construction the CLI
    # uses for assessment.sha256. It reproduces for the same corpus within the
    # same runtime, but the hashed bundle embeds ``tool_versions`` (invocation,
    # Python, platform), so a run under uvicorn need NOT byte-match a
    # ``python -m src.assessment.cli`` run. The corpus-derived measurements are
    # identical across both; only the recorded invocation differs.
    report_hash = hashlib.sha256(
        canonical_json(payload).encode("utf-8")
    ).hexdigest()

    return AssessmentResponse(
        root_label="examples/demo",
        report_hash=report_hash,
        schema_version=bundle.schema_version,
        programs_assessed=len(bundle.programs),
        manifest_files=len(bundle.inventory.records),
        bundle=payload,
        # Built from the same bundle, but deliberately NOT folded into it: the
        # hash above is over the bundle alone, and section 0 is a rendering of
        # that bundle rather than a measurement in it. Adding it to the hashed
        # payload would make the report hash depend on report prose.
        plain_summary=plain_summary(
            bundle, root_label="examples/demo", scope_by_construct=by_construct
        ),
    )


@app.get("/api/v1/templates", tags=["Templates"])
async def list_templates():
    """List available industry templates."""
    return {
        "templates": [
            {
                "name": "banking",
                "description": "Banking & Financial Services (COBOL → Java)",
                "source_language": "cobol",
                "target_language": "java",
                "patterns": [
                    "interest_calculation",
                    "batch_processing",
                    "account_management",
                ],
            },
            {
                "name": "government",
                "description": "Government & Defense (Ada → Rust)",
                "source_language": "ada",
                "target_language": "rust",
                "patterns": ["real_time", "fisma_compliance", "high_reliability"],
            },
            {
                "name": "healthcare",
                "description": "Healthcare Systems (MUMPS → Node.js)",
                "source_language": "mumps",
                "target_language": "nodejs",
                "patterns": ["hl7_messaging", "hipaa_audit", "patient_records"],
            },
            {
                "name": "manufacturing",
                "description": "Manufacturing & Engineering (FORTRAN → C++)",
                "source_language": "fortran",
                "target_language": "cpp",
                "patterns": [
                    "scientific_computing",
                    "numerical_precision",
                    "parallel_processing",
                ],
            },
            {
                "name": "insurance",
                "description": "Insurance Systems (PL/I → C#)",
                "source_language": "pli",
                "target_language": "csharp",
                "patterns": [
                    "actuarial_calculations",
                    "policy_pricing",
                    "regulatory_compliance",
                ],
            },
        ]
    }


@app.get("/api/v1/languages", tags=["Languages"])
async def list_supported_languages():
    """List supported source and target languages."""
    return {
        "source_languages": [
            {"code": "cobol", "name": "COBOL", "status": "supported"},
            {"code": "fortran", "name": "FORTRAN", "status": "planned"},
            {"code": "ada", "name": "Ada", "status": "planned"},
            {"code": "mumps", "name": "MUMPS", "status": "planned"},
            {"code": "pli", "name": "PL/I", "status": "planned"},
        ],
        "target_languages": [
            {"code": "java", "name": "Java", "status": "supported"},
            {"code": "python", "name": "Python", "status": "supported"},
            {"code": "rust", "name": "Rust", "status": "planned"},
            {"code": "csharp", "name": "C#", "status": "planned"},
            {"code": "cpp", "name": "C++", "status": "planned"},
            {"code": "nodejs", "name": "Node.js", "status": "planned"},
        ],
    }


@app.get("/api/v1/metrics", tags=["Metrics"])
async def get_platform_metrics():
    """Get platform-wide metrics and statistics."""
    return {
        "total_migrations": len(migrations_db),
        "successful_migrations": sum(
            1 for m in migrations_db.values() if m.get("status") == "completed"
        ),
        # Averages are computed over jobs whose values were actually
        # measured (non-null). No measured jobs -> null, never a constant.
        "average_semantic_score": (
            round(sum(v) / len(v), 2)
            if (v := [m["semantic_score"] for m in migrations_db.values()
                      if m.get("semantic_score") is not None])
            else None
        ),
        "average_test_coverage": (
            round(sum(c) / len(c), 2)
            if (c := [m["test_coverage"] for m in migrations_db.values()
                      if m.get("test_coverage") is not None])
            else None
        ),
        "total_loc_processed": sum(
            len(m.get("source_code", "").split("\n"))
            for m in migrations_db.values()
        ),
    }


# ============================================================================
# Background Tasks
# ============================================================================


async def run_migration(migration_id: str):
    """Background task: run the REAL migration pipeline.

    This previously fabricated its own results (mock output, hardcoded
    semantic_score=85.0, test_coverage=80.0, fallback 75.0/50.0, and an
    attestation hashed over mock code). It now delegates to
    MigrationOrchestrator, which measures behavioral equivalence via
    differential execution and refuses to attest unmeasured runs.
    Every value stored here is measured or None.
    """
    import tempfile
    from pathlib import Path as _P

    job = migrations_db.get(migration_id)
    if not job:
        return
    job["status"] = "processing"
    try:
        from src.core.orchestrator import MigrationOrchestrator, MigrationConfig

        with tempfile.NamedTemporaryFile(
            "w", suffix=".cbl", delete=False
        ) as tf:
            tf.write(job["source_code"])
            src_path = tf.name
        out_dir = tempfile.mkdtemp(prefix="relian_api_")

        orchestrator = MigrationOrchestrator()
        result = await orchestrator.migrate(MigrationConfig(
            source_path=src_path,
            source_language=job.get("source_language", "cobol"),
            target_language=job.get("target_language", "java"),
            output_dir=out_dir,
        ))

        job["source_hash"] = result.source_hash
        job["semantic_score"] = result.semantic_score      # measured or None
        job["test_coverage"] = result.test_coverage        # measured or None
        job["risk_score"] = result.risk_score              # measured or None
        job["tests_passed"] = result.tests_passed
        job["warnings"] = list(result.warnings)
        if result.output_path and _P(result.output_path).exists():
            job["output_code"] = _P(result.output_path).read_text()
            job["target_hash"] = hashlib.sha256(
                job["output_code"].encode()
            ).hexdigest()
        # Attestation comes ONLY from the orchestrator's measured-gated
        # path. This endpoint never mints its own.
        job["attestation_tx"] = result.attestation_tx
        if result.attestation_tx:
            job["attested_at"] = datetime.now(timezone.utc).isoformat()
        job["status"] = result.status.value
        if result.errors:
            job["error"] = "; ".join(str(e) for e in result.errors)[:500]
    except Exception as e:
        job["status"] = "failed"
        job["error"] = str(e)[:500]
        for k in ("semantic_score", "test_coverage", "risk_score"):
            job[k] = None


# ============================================================================
# Run Server
# ============================================================================

if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=8000)
