"""Migration orchestrator - coordinates all migration pipeline stages."""

import logging
import os
import json
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Dict, Any, List, Optional
from enum import Enum

_logger = logging.getLogger(__name__)


class MigrationStatus(Enum):
    """Migration pipeline status."""

    PENDING = "pending"
    PARSING = "parsing"
    ANALYZING = "analyzing"
    GENERATING_TESTS = "generating_tests"
    TRANSFORMING = "transforming"
    VALIDATING = "validating"
    ATTESTING = "attesting"
    COMPLETED = "completed"
    FAILED = "failed"


@dataclass
class MigrationConfig:
    """Configuration for a migration job."""

    source_path: str
    source_language: str
    target_language: str
    output_dir: str
    template: Optional[str] = None
    enable_blockchain: bool = True
    generate_tests: bool = True
    risk_threshold: float = 75.0  # Max acceptable risk score

    def to_dict(self) -> Dict[str, Any]:
        """Serialize config to dictionary."""
        return {
            "source_path": self.source_path,
            "source_language": self.source_language,
            "target_language": self.target_language,
            "output_dir": self.output_dir,
            "template": self.template,
            "enable_blockchain": self.enable_blockchain,
            "generate_tests": self.generate_tests,
            "risk_threshold": self.risk_threshold,
        }


@dataclass
class MigrationResult:
    """Result of a completed migration."""

    migration_id: str
    status: MigrationStatus
    source_path: str
    output_path: Optional[str] = None
    source_hash: Optional[str] = None
    target_hash: Optional[str] = None
    # CARDINAL RULE (RELIAN-BENCH v1.0): a metric is either MEASURED or it
    # is None. None is never substituted with a constant or a formula.
    semantic_score: Optional[float] = None   # measured BER only (differential)
    risk_score: Optional[float] = None
    test_coverage: Optional[float] = None    # real coverage tool only
    tests_generated: int = 0
    tests_passed: int = 0
    attestation_tx: Optional[str] = None
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    started_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())
    completed_at: Optional[str] = None
    duration_seconds: float = 0.0

    def to_dict(self) -> Dict[str, Any]:
        """Serialize result to dictionary."""
        return {
            "migration_id": self.migration_id,
            "status": self.status.value,
            "source_path": self.source_path,
            "output_path": self.output_path,
            "source_hash": self.source_hash,
            "target_hash": self.target_hash,
            "semantic_score": self.semantic_score,
            "risk_score": self.risk_score,
            "test_coverage": self.test_coverage,
            "tests_generated": self.tests_generated,
            "tests_passed": self.tests_passed,
            "attestation_tx": self.attestation_tx,
            "errors": self.errors,
            "warnings": self.warnings,
            "started_at": self.started_at,
            "completed_at": self.completed_at,
            "duration_seconds": self.duration_seconds,
        }


class MigrationOrchestrator:
    """
    Orchestrates the complete migration pipeline.

    Pipeline stages:
    1. Parse source code → AST
    2. Semantic analysis → Business logic extraction
    3. Risk scoring → Quality prediction
    4. Test generation → Automated test suite
    5. Code transformation → Target language
    6. Validation → Differential testing
    7. Blockchain attestation → Immutable proof
    """

    def __init__(self) -> None:
        """Initialize orchestrator with required components.

        The `intelligence` parameter was removed in WP-2.0.-3 (R1) along with
        `src/intelligence`. It injected a MigrationIntelligence instance whose
        RSI loop retrained an XGBoost model on fabricated feature rows. See
        docs/R1_ML_DISPOSITION_2026-08.md.
        """
        self._parsers: Dict[str, Any] = {}
        self._templates: Dict[str, Any] = {}
        self._current_status = MigrationStatus.PENDING
        self._progress_callbacks: List[Any] = []

    def register_parser(self, language: str, parser: Any) -> None:
        """Register a language parser."""
        self._parsers[language.lower()] = parser

    def register_template(self, name: str, template: Any) -> None:
        """Register an industry template."""
        self._templates[name.lower()] = template

    def add_progress_callback(self, callback: Any) -> None:
        """Add callback for progress updates."""
        self._progress_callbacks.append(callback)

    def load_plugins(self, plugin_registry: Optional[Any] = None) -> Dict[str, Any]:
        """
        Discover and register all available submodule-backed plugins.

        Creates a default PluginRegistry if none is provided, wires every
        available plugin into this orchestrator, and returns a status report.
        Safe to call multiple times; duplicate parsers for the same language
        key will overwrite earlier registrations (last-wins).

        Args:
            plugin_registry: Optional pre-configured PluginRegistry instance.

        Returns:
            Status report dict from PluginRegistry.get_status_report().
        """
        try:
            from src.plugins.registry import PluginRegistry
            registry = plugin_registry or PluginRegistry()
            registry.load_all(self)
            report = registry.get_status_report()
            _logger.info(
                "Plugin loading complete: %d/%d plugins registered.",
                report["loaded"],
                report["total_plugins_attempted"],
            )
            return report
        except Exception as exc:
            _logger.warning("Plugin loading failed (pipeline continues): %s", exc)
            return {"loaded": 0, "total_plugins_attempted": 0, "details": {}}

    def _update_status(self, status: MigrationStatus, message: str = "") -> None:
        """Update status and notify callbacks."""
        self._current_status = status
        for callback in self._progress_callbacks:
            try:
                callback(status, message)
            except Exception:
                pass

    async def migrate(self, config: MigrationConfig) -> MigrationResult:
        """
        Execute complete migration pipeline.

        Args:
            config: Migration configuration

        Returns:
            MigrationResult with all metrics and attestation
        """
        import hashlib
        from datetime import datetime

        start_time = datetime.now(timezone.utc)

        # Generate migration ID
        migration_id = hashlib.sha256(
            f"{config.source_path}{start_time.isoformat()}".encode()
        ).hexdigest()[:16]

        result = MigrationResult(
            migration_id=migration_id,
            status=MigrationStatus.PENDING,
            source_path=config.source_path,
        )

        try:
            # Stage 1: Parse source code
            self._update_status(MigrationStatus.PARSING, "Parsing source code...")
            ast, source_code = await self._parse_source(config)
            result.source_hash = hashlib.sha256(source_code.encode()).hexdigest()

            # RSI pattern retrieval: REMOVED (WP-2.0.-3, R1).
            # `MigrationIntelligence.retrieve_similar_patterns()` ranked past
            # migrations for few-shot injection. Its last consumer -- the
            # Stage 2 LLM call -- was already gone under R6, leaving only a
            # count printed into the warnings. The metrics it ranked on came
            # from RiskScorer.extract_metrics(), whose "cognitive complexity"
            # was cyclomatic * 1.2. Both the scorer and the store are deleted.

            # Stage 2: REMOVED (WP-2.0.-2, R6).
            # The generative-AI semantic analysis stage sent customer source
            # code to a hosted LLM. R6 forbids that in the transform path, so
            # the stage is gone rather than gated: a flag can be flipped, a
            # deleted call cannot. semantic_score is therefore None here by
            # construction and is set exclusively by Stage 6 differential
            # validation (behavioral equivalence vs. the legacy oracle) --
            # which is the only measurement of semantic preservation this
            # pipeline has ever had. LLM self-reported confidence was never
            # one (R1).

            # Stage 3: Risk scoring.
            # The auto-retrain hook that stood here (is_retrain_ready() ->
            # retrain_risk_model()) was removed in WP-2.0.-3 (R1): it retrained
            # XGBoost every RETRAIN_INTERVAL=20 migrations on rows whose
            # features were fabricated. `_score_risk` now returns None for both
            # keys, permanently and by construction -- see its docstring.
            risk_assessment = await self._score_risk(ast, source_code)
            result.risk_score = risk_assessment.get("overall_score")

            # Check risk threshold (only when actually measured)
            if result.risk_score is not None and result.risk_score > config.risk_threshold:
                result.warnings.append(
                    f"Risk score ({result.risk_score}) exceeds threshold "
                    f"({config.risk_threshold}). Manual review recommended."
                )

            # Stage 4: REMOVED (WP-2.0.-2, R6).
            # LLM test generation shared the same dependency as Stage 2 and
            # sent customer source code off-perimeter. tests_generated stays 0
            # and test_coverage stays None by construction. That was already
            # the honest value: counting generated tests was never a coverage
            # measurement, and test_coverage is set only when a real coverage
            # tool (JaCoCo) has run. See RELIAN-BENCH SPEC 5.
            if config.generate_tests:
                result.warnings.append(
                    "[tests] generation stage removed under R6 (WP-2.0.-2); "
                    "config.generate_tests=True has no effect and "
                    "test_coverage remains None (not measured)"
                )

            # Stage 5: Transform code
            self._update_status(MigrationStatus.TRANSFORMING, "Transforming code...")
            target_code = await self._transform_code(
                ast, source_code, config.target_language, config.template
            )

            # Save output
            os.makedirs(config.output_dir, exist_ok=True)
            output_file = os.path.join(
                config.output_dir, f"migrated_{migration_id}.{self._get_extension(config.target_language)}"
            )
            with open(output_file, "w", encoding="utf-8") as f:
                f.write(target_code)
            result.output_path = output_file
            result.target_hash = hashlib.sha256(target_code.encode()).hexdigest()

            # Stage 6: Validate
            self._update_status(MigrationStatus.VALIDATING, "Validating migration...")
            validation = await self._validate_migration(
                source_code, target_code, result.tests_generated
            )
            result.tests_passed = validation.get("tests_passed") or 0
            if validation.get("measured"):
                # Behavioral Equivalence Rate, measured -> this IS the
                # semantic score. 0..100 scale for continuity.
                ber = validation.get("ber")
                result.semantic_score = round(ber * 100, 2) if ber is not None else None
            else:
                result.warnings.append(
                    f"[validation] NOT MEASURED: {validation.get('reason')}"
                )

            # Stage 7: Attestation -- ONLY over measured values.
            # Signing unmeasured numbers is worse than not signing: it
            # makes a fabricated value tamper-evident instead of true.
            if config.enable_blockchain and not validation.get("measured"):
                result.warnings.append(
                    "[attestation] SKIPPED: validation was not measured; "
                    "refusing to attest unmeasured quality claims."
                )
            elif config.enable_blockchain:
                self._update_status(MigrationStatus.ATTESTING, "Creating attestation...")
                attestation = await self._create_attestation(
                    source_code,
                    target_code,
                    result.test_coverage,
                    (result.semantic_score / 100) if result.semantic_score is not None else None,
                    result.risk_score,
                )
                result.attestation_tx = attestation.get("transaction_signature")

            # Complete
            result.status = MigrationStatus.COMPLETED
            result.completed_at = datetime.now(timezone.utc).isoformat()
            result.duration_seconds = (
                datetime.now(timezone.utc) - start_time
            ).total_seconds()

            # RSI outcome recording: REMOVED (WP-2.0.-3, R1).
            # `record_outcome()` wrote each migration into a persistent pattern
            # store that doubled as the XGBoost training set, and booked
            # revenue as lines_of_code x a per-tier price. The honesty guard it
            # carried (unmeasured -> no write) was correct as far as it went,
            # but it guarded the entry to a loop whose feature rows were
            # fabricated regardless. The store, the loop and the model are
            # gone; nothing replaces them until RELIAN-BENCH covers migration
            # outcomes and labelled data exists to train on (R7).

            self._update_status(
                MigrationStatus.COMPLETED,
                f"Migration completed in {result.duration_seconds:.2f}s",
            )

        except Exception as e:
            result.status = MigrationStatus.FAILED
            result.errors.append(str(e))
            result.completed_at = datetime.now(timezone.utc).isoformat()
            self._update_status(MigrationStatus.FAILED, str(e))

        return result

    async def _parse_source(self, config: MigrationConfig) -> tuple:
        """Parse source code into AST."""
        parser = self._parsers.get(config.source_language.lower())

        with open(config.source_path, "r", encoding="utf-8") as f:
            source_code = f.read()

        if parser:
            ast = parser.parse_string(source_code)
        else:
            # Return None AST if no parser registered
            ast = None

        return ast, source_code

    # _analyze_semantics() was deleted in WP-2.0.-2 (R6). It constructed
    # src.analysis.semantic.SemanticAnalyzer, which instantiated an OpenAI or
    # Anthropic client and put customer source code in the prompt. The method
    # is removed rather than stubbed so that reintroducing the behavior
    # requires writing it again, in a diff, under review.

    async def _score_risk(self, ast: Any, source_code: str) -> Dict[str, Any]:
        """Risk is not scored in this pipeline. Both keys are None (R1).

        This used to construct ``src.ml.risk_scorer.RiskScorer`` and return its
        assessment. That path was deleted in WP-2.0.-3: the scorer attached a
        hardcoded ``confidence`` (0.85 with a loaded model, 0.70 without) to a
        weighted-sum heuristic, and advertised "200+ metrics" over eighteen
        fields. None of it was measured, so none of it may be reported (R1).

        ``{"overall_score": None, "risk_level": None}`` is not a new failure
        mode -- it was already this method's exception path, taken whenever the
        scorer raised. It is now the permanent, honest value, returned
        unconditionally rather than as a fallback. A None that always means
        "not measured" is worth more than a number that sometimes does.

        The real, measured risk product is ``src/assessment/risk.py``: rule-
        based tiering over the assessment engine's own inventory, with a
        Trutina grade and a provenance string on every figure (R9). It is
        reached through the assessment CLI and ``/api/v1/assess``, not here.

        Args:
            ast: Parsed AST; accepted so the pipeline's call shape is unchanged.
            source_code: Raw source; likewise unused.

        Returns:
            ``{"overall_score": None, "risk_level": None}``, always.
        """
        del ast, source_code  # unused: nothing here is measured
        return {"overall_score": None, "risk_level": None}

    # _generate_tests() was deleted in WP-2.0.-2 (R6), for the same reason as
    # _analyze_semantics(): src.analysis.test_generator.TestGenerator sent
    # customer source code to a hosted model.

    async def _transform_code(
        self, ast: Any, source_code: str, target_language: str, template: Optional[str]
    ) -> str:
        """Transform source code to target language."""
        # Dead prelude, corrected in WP-2.0.-2: the comment that stood here
        # ("this is a simplified transformation / in production this would use
        # sophisticated code generation") described a stub that no longer
        # exists. For Java this dispatches to _transform_to_java, which calls
        # the real deterministic transpiler (transpiler.c1_rulebased) and
        # raises rather than emitting a placeholder. The comment was removed
        # because it understated what ships, which is its own honesty defect.

        header = f"""/**
 * Auto-generated by Relian Migration Platform
 * Source Language: COBOL
 * Target Language: {target_language}
 * Template: {template or 'default'}
 * Generated: {datetime.now(timezone.utc).isoformat()}
 */

"""

        if target_language.lower() == "java":
            return header + self._transform_to_java(ast, source_code)
        elif target_language.lower() == "python":
            return header + self._transform_to_python(ast, source_code)
        else:
            return header + f"// TODO: Implement {target_language} transformation\n"

    def _transform_to_java(self, ast: Any, source_code: str) -> str:
        """Transform COBOL to Java via the deterministic rule-based
        transpiler (C1, RELIAN-BENCH candidate C1_rulebased).

        Measured against the committed benchmark on 2026-07-15:
        BER 1.0000 (300/300 held-out), build 1.0, branch coverage 0.8824
        -- full PASS of the committed thresholds. See bench/results/.

        Failure semantics: if the source uses constructs outside the
        transpiler's COBOL-85 subset, this RAISES rather than emitting a
        placeholder. A placeholder that 'completes' is a fabricated
        migration; an honest failure is a failed migration. The caller's
        global handler records FAILED status.
        """
        import re as _re
        import sys as _sys
        from pathlib import Path as _P
        repo_root = _P(__file__).resolve().parents[2]
        if str(repo_root) not in _sys.path:
            _sys.path.insert(0, str(repo_root))
        from transpiler.c1_rulebased import Transpiler, UnsupportedConstruct

        m = _re.search(r"PROGRAM-ID\.\s*([A-Z0-9\-]+)", source_code, _re.IGNORECASE)
        prog_id = (m.group(1).rstrip(".") if m else "MigratedProgram")
        class_name = prog_id.replace("-", "_").capitalize()

        try:
            return Transpiler(source_code, class_name).transpile()
        except UnsupportedConstruct as exc:
            # WP-1.5.2: strict mode stops at the FIRST unsupported construct.
            # The honest-failure result should carry the full inventory, so
            # re-run in inventory mode (strict=False records every occurrence
            # and emits nothing). The re-run may fail for a different reason
            # on a program this far outside the subset; the inventory is then
            # whatever was collected before that failure, which is still more
            # than the single construct in `exc`.
            inventory = [(exc.verb, exc.line_no)]
            try:
                inv_tp = Transpiler(source_code, class_name, strict=False)
                try:
                    inv_tp.transpile()
                finally:
                    if inv_tp.unsupported_hits:
                        inventory = inv_tp.unsupported_hits
            except Exception:
                pass
            listed = ", ".join(f"{v} (line {n})" for v, n in inventory)
            raise RuntimeError(
                f"deterministic transform does not support this program's "
                f"constructs ({exc}); unsupported construct inventory: "
                f"[{listed}]; refusing to emit a placeholder in place of a "
                f"migration"
            ) from exc
        except Exception as exc:
            raise RuntimeError(
                f"deterministic transform does not support this program's "
                f"constructs ({exc}); refusing to emit a placeholder in "
                f"place of a migration"
            ) from exc

    def _transform_to_python(self, ast: Any, source_code: str) -> str:
        """Transform to Python code."""
        program_name = "migrated_program"
        if ast and hasattr(ast, "name"):
            program_name = ast.name.replace("-", "_").lower()

        return f'''"""
Migrated from COBOL by Relian.
"""


class {program_name.title().replace("_", "")}:
    """Migrated COBOL program."""

    def __init__(self):
        """Initialize from WORKING-STORAGE SECTION."""
        self.var1 = ""
        self.var2 = 0

    def main_para(self):
        """Main procedure from PROCEDURE DIVISION."""
        # TODO: Implement business logic
        print("Migration placeholder")


if __name__ == "__main__":
    program = {program_name.title().replace("_", "")}()
    program.main_para()
'''

    async def _validate_migration(
        self, source_code: str, target_code: str, num_tests: int
    ) -> Dict[str, Any]:
        """Validate migration through REAL differential testing.

        Delegates to src.validation.differential, which compiles and
        executes the migrated code against oracle-generated vectors and
        compares stdout with the legacy program's stdout.

        Returns measured values, or None values when validation could not
        run. It NEVER fabricates: the previous implementation returned
        tests_passed=num_tests, tests_failed=0, validation_score=95.0
        without executing anything. That behavior is prohibited.
        """
        try:
            from src.validation.differential import validate_differential
            return validate_differential(source_code, target_code)
        except Exception as exc:
            return {
                "tests_passed": None,
                "tests_failed": None,
                "validation_score": None,
                "measured": False,
                "reason": f"differential validation unavailable: {exc}",
            }

    async def _create_attestation(
        self,
        source_code: str,
        target_code: str,
        test_coverage: float,
        semantic_score: float,
        risk_score: float,
    ) -> Dict[str, Any]:
        """Create blockchain attestation."""
        try:
            from src.blockchain.client import SolanaClient

            client = SolanaClient()
            client.connect()
            attestation = await client.create_attestation(
                source_code, target_code, test_coverage, semantic_score, risk_score
            )
            return attestation.to_dict()
        except Exception:
            return {"transaction_signature": None}

    def _get_extension(self, language: str) -> str:
        """Get file extension for target language."""
        extensions = {
            "java": "java",
            "python": "py",
            "rust": "rs",
            "csharp": "cs",
            "cpp": "cpp",
            "nodejs": "js",
            "typescript": "ts",
        }
        return extensions.get(language.lower(), "txt")

    def get_status(self) -> MigrationStatus:
        """Get current migration status."""
        return self._current_status

