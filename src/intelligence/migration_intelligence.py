"""
Migration Intelligence Engine — RSI + Recursive Self-Financing.

Every completed migration teaches the system how to do the next one better,
and each dollar of gross margin compounds into higher compute allocation,
closing the loop:

  Better patterns → Higher quality → Premium pricing
       ↑                                    ↓
  Richer few-shot                    More revenue
       ↑                                    ↓
  Larger context budget  ←←  Reinvestment dividend

The quality of migration N feeds directly into migration N+1 via:
  1. Few-shot retrieval  — similar past successes injected into LLM prompts
  2. Risk model updates  — XGBoost retrained as new labelled examples arrive
  3. Budget allocation   — revenue-to-cost ratio gates model tier selection
"""

import json
import math
import os
import time
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class MigrationPattern:
    """A successful migration stored as a reusable few-shot example."""

    pattern_id: str
    source_language: str
    target_language: str
    template: str
    complexity_tier: str           # "low" | "medium" | "high" | "critical"
    semantic_score: float          # 0–100; stored only if >= QUALITY_THRESHOLD
    risk_score: float
    test_coverage: float
    lines_of_code: int
    cyclomatic_complexity: int
    num_goto_statements: int
    # Distilled transformation knowledge (kept small for prompt injection)
    key_transformations: List[str]  # e.g. "GO TO → structured loop", "FILLER → _"
    business_domain_hints: List[str]  # e.g. "interest calculation", "batch job"
    revenue_usd: float
    cost_usd: float
    recorded_at: str


@dataclass
class BudgetState:
    """Tracks the self-financing pool and spending allocation."""

    total_revenue_usd: float = 0.0
    total_cost_usd: float = 0.0
    reinvestment_pool_usd: float = 0.0
    # Fraction of gross margin channelled back into quality compute
    quality_dividend_rate: float = 0.10
    migrations_completed: int = 0
    migrations_since_last_retrain: int = 0
    # How many tokens we can "spend" on the next LLM call based on budget
    allocated_token_budget: int = 4_000  # starts conservative


@dataclass
class IntelligenceReport:
    """Snapshot of RSI + self-financing health."""

    migrations_in_memory: int
    avg_semantic_score_all_time: float
    avg_semantic_score_last_10: float
    quality_velocity: float        # improvement per migration (∆ score / N)
    gross_margin_pct: float
    reinvestment_pool_usd: float
    current_token_budget: int
    model_tier: str                # "economy" | "standard" | "premium"
    retrain_ready: bool


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

QUALITY_THRESHOLD = 80.0       # minimum semantic score to store a pattern
RETRAIN_INTERVAL = 20           # retrain XGBoost every N new migrations
PRICING_TIERS = {               # USD per line of migrated code
    "low": 0.50,
    "medium": 1.50,
    "high": 3.00,
    "critical": 5.00,
}
LLM_COST_PER_1K_TOKENS = {
    "economy": 0.002,           # e.g. GPT-3.5
    "standard": 0.010,          # e.g. GPT-4o
    "premium": 0.030,           # e.g. GPT-4-turbo / Claude Opus
}
TOKEN_BUDGET_THRESHOLDS = {     # reinvestment_pool → model tier
    "premium": 500.0,
    "standard": 50.0,
    "economy": 0.0,
}
MODEL_TOKEN_BUDGETS = {
    "premium": 32_000,
    "standard": 16_000,
    "economy": 4_000,
}
MAX_FEW_SHOT_EXAMPLES = 5


# ---------------------------------------------------------------------------
# Core engine
# ---------------------------------------------------------------------------

class MigrationIntelligence:
    """
    Persistent memory and budget manager that closes the RSI + self-financing
    loop across all migrations run on this installation.

    State is written to ``~/.relian/intelligence.json`` so knowledge
    accumulates across sessions without requiring a running database.
    """

    def __init__(self, state_path: Optional[str] = None):
        self._state_path = Path(
            state_path or os.path.expanduser("~/.relian/intelligence.json")
        )
        self._state_path.parent.mkdir(parents=True, exist_ok=True)
        self._patterns: List[MigrationPattern] = []
        self._budget = BudgetState()
        self._load_state()

    # ------------------------------------------------------------------
    # RSI — recording & retrieval
    # ------------------------------------------------------------------

    def record_outcome(
        self,
        *,
        migration_id: str,
        source_language: str,
        target_language: str,
        template: Optional[str],
        semantic_score: float,
        risk_score: float,
        test_coverage: float,
        lines_of_code: int,
        cyclomatic_complexity: int,
        num_goto_statements: int,
        key_transformations: List[str],
        business_domain_hints: List[str],
        tokens_used: int,
    ) -> None:
        """
        Called after every migration completes.

        Patterns with semantic_score >= QUALITY_THRESHOLD are added to the
        few-shot library.  Budget is updated unconditionally so the
        self-financing pool reflects all activity.
        """
        complexity_tier = self._classify_complexity(cyclomatic_complexity, lines_of_code)
        revenue = lines_of_code * PRICING_TIERS[complexity_tier]
        model_tier = self._current_model_tier()
        cost = (tokens_used / 1_000) * LLM_COST_PER_1K_TOKENS[model_tier]

        # Update budget
        gross_margin = revenue - cost
        self._budget.total_revenue_usd += revenue
        self._budget.total_cost_usd += cost
        self._budget.reinvestment_pool_usd += gross_margin * self._budget.quality_dividend_rate
        self._budget.migrations_completed += 1
        self._budget.migrations_since_last_retrain += 1
        self._budget.allocated_token_budget = MODEL_TOKEN_BUDGETS[self._current_model_tier()]

        # Store pattern only if quality threshold met
        if semantic_score >= QUALITY_THRESHOLD:
            pattern = MigrationPattern(
                pattern_id=migration_id,
                source_language=source_language.lower(),
                target_language=target_language.lower(),
                template=template or "default",
                complexity_tier=complexity_tier,
                semantic_score=semantic_score,
                risk_score=risk_score,
                test_coverage=test_coverage,
                lines_of_code=lines_of_code,
                cyclomatic_complexity=cyclomatic_complexity,
                num_goto_statements=num_goto_statements,
                key_transformations=key_transformations[:10],
                business_domain_hints=business_domain_hints[:5],
                revenue_usd=revenue,
                cost_usd=cost,
                recorded_at=datetime.now(timezone.utc).isoformat(),
            )
            self._patterns.append(pattern)

        self._save_state()

    def retrieve_similar_patterns(
        self,
        *,
        source_language: str,
        target_language: str,
        template: Optional[str],
        cyclomatic_complexity: int,
        lines_of_code: int,
        n: int = MAX_FEW_SHOT_EXAMPLES,
    ) -> List[MigrationPattern]:
        """
        Return the N most relevant past patterns to inject as few-shot
        examples into the LLM prompt.

        Similarity is scored on language match, template match, and
        proximity in the complexity/size space.  Patterns with higher
        semantic scores are preferred within a tie.
        """
        if not self._patterns:
            return []

        src = source_language.lower()
        tgt = target_language.lower()
        tmpl = (template or "default").lower()
        target_complexity = self._classify_complexity(cyclomatic_complexity, lines_of_code)

        scored: List[Tuple[float, MigrationPattern]] = []
        for p in self._patterns:
            score = 0.0
            if p.source_language == src:
                score += 4.0
            if p.target_language == tgt:
                score += 3.0
            if p.template == tmpl:
                score += 2.0
            if p.complexity_tier == target_complexity:
                score += 1.0
            # Prefer higher quality examples
            score += p.semantic_score / 200.0
            scored.append((score, p))

        scored.sort(key=lambda x: x[0], reverse=True)
        return [p for _, p in scored[:n]]

    def build_few_shot_prompt_section(
        self,
        patterns: List[MigrationPattern],
    ) -> str:
        """
        Format retrieved patterns into a prompt section that instructs
        the LLM to reproduce the same quality of transformation.
        """
        if not patterns:
            return ""

        lines = [
            "## Learned Migration Patterns (few-shot examples from previous "
            f"successful migrations — avg semantic score: "
            f"{sum(p.semantic_score for p in patterns)/len(patterns):.1f}%)\n"
        ]
        for i, p in enumerate(patterns, 1):
            lines.append(
                f"### Example {i}: {p.source_language.upper()} → "
                f"{p.target_language.upper()} "
                f"(score={p.semantic_score:.1f}%, complexity={p.complexity_tier})"
            )
            if p.key_transformations:
                lines.append("Key transformations applied:")
                for t in p.key_transformations:
                    lines.append(f"  - {t}")
            if p.business_domain_hints:
                lines.append(f"Domain context: {', '.join(p.business_domain_hints)}")
            lines.append("")

        lines.append(
            "Apply the same transformation principles to achieve equivalent "
            "or higher semantic preservation.\n"
        )
        return "\n".join(lines)

    # ------------------------------------------------------------------
    # Self-financing — budget & model tier
    # ------------------------------------------------------------------

    def get_token_budget(self) -> int:
        """Return the current token budget for LLM calls."""
        return self._budget.allocated_token_budget

    def get_model_recommendation(self) -> str:
        """
        Recommend the LLM model tier based on the reinvestment pool.

        As successful migrations accumulate revenue, the pool grows and
        unlocks progressively more capable models — this is the
        self-financing mechanism.
        """
        return self._current_model_tier()

    def is_retrain_ready(self) -> bool:
        """True when enough new labelled data has arrived to retrain XGBoost."""
        return self._budget.migrations_since_last_retrain >= RETRAIN_INTERVAL

    def acknowledge_retrain(self) -> None:
        """Reset the retrain counter after the risk model has been retrained."""
        self._budget.migrations_since_last_retrain = 0
        self._save_state()

    def get_report(self) -> IntelligenceReport:
        """Return a snapshot of RSI + self-financing health."""
        scores = [p.semantic_score for p in self._patterns]
        avg_all = sum(scores) / len(scores) if scores else 0.0
        last10 = scores[-10:] if len(scores) >= 10 else scores
        avg_last10 = sum(last10) / len(last10) if last10 else 0.0

        if len(scores) >= 2:
            quality_velocity = (scores[-1] - scores[0]) / len(scores)
        else:
            quality_velocity = 0.0

        revenue = self._budget.total_revenue_usd
        cost = self._budget.total_cost_usd
        gm_pct = ((revenue - cost) / revenue * 100) if revenue > 0 else 0.0

        return IntelligenceReport(
            migrations_in_memory=len(self._patterns),
            avg_semantic_score_all_time=round(avg_all, 2),
            avg_semantic_score_last_10=round(avg_last10, 2),
            quality_velocity=round(quality_velocity, 4),
            gross_margin_pct=round(gm_pct, 2),
            reinvestment_pool_usd=round(self._budget.reinvestment_pool_usd, 2),
            current_token_budget=self._budget.allocated_token_budget,
            model_tier=self._current_model_tier(),
            retrain_ready=self.is_retrain_ready(),
        )

    # ------------------------------------------------------------------
    # XGBoost incremental retraining
    # ------------------------------------------------------------------

    def retrain_risk_model(self, risk_scorer: Any) -> bool:
        """
        Incrementally retrain the XGBoost risk scorer with accumulated
        labelled data (patterns include outcome metrics as labels).

        Returns True if retraining succeeded.
        """
        if len(self._patterns) < RETRAIN_INTERVAL:
            return False

        try:
            import numpy as np
            import xgboost as xgb
        except ImportError:
            return False

        # Build training matrix from stored patterns
        X_rows, y_rows = [], []
        for p in self._patterns:
            # Feature vector mirrors CodeMetrics.to_feature_vector() ordering
            # We use the subset we have; missing fields default to 0
            row = [
                p.lines_of_code,
                p.cyclomatic_complexity,
                int(p.cyclomatic_complexity * 1.2),  # cognitive (approx)
                0,  # max_nesting_depth (not stored — use 0)
                0,  # num_functions
                p.lines_of_code / max(1, 5),
                0,  # num_dependencies
                0,  # fan_in
                0,  # fan_out
                0.0,  # coupling_between_modules
                0.0,  # comment_ratio
                0.0,  # duplicate_code_ratio
                0.0,  # dead_code_ratio
                p.test_coverage / 100.0,
                0,  # num_global_variables
                p.num_goto_statements,
                0,  # num_copy_statements
                0,  # data_division_complexity
            ]
            X_rows.append(row)
            y_rows.append(p.risk_score / 100.0)  # normalised 0–1

        X = np.array(X_rows, dtype=np.float32)
        y = np.array(y_rows, dtype=np.float32)

        dtrain = xgb.DMatrix(X, label=y)
        params = {
            "objective": "reg:squarederror",
            "max_depth": 4,
            "eta": 0.1,
            "subsample": 0.8,
            "colsample_bytree": 0.8,
        }
        model = xgb.train(params, dtrain, num_boost_round=100, verbose_eval=False)
        risk_scorer.model = model
        risk_scorer._is_trained = True

        self.acknowledge_retrain()
        return True

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _current_model_tier(self) -> str:
        pool = self._budget.reinvestment_pool_usd
        if pool >= TOKEN_BUDGET_THRESHOLDS["premium"]:
            return "premium"
        if pool >= TOKEN_BUDGET_THRESHOLDS["standard"]:
            return "standard"
        return "economy"

    @staticmethod
    def _classify_complexity(cyclomatic: int, loc: int) -> str:
        score = cyclomatic + loc / 100
        if score < 15:
            return "low"
        if score < 35:
            return "medium"
        if score < 60:
            return "high"
        return "critical"

    # ------------------------------------------------------------------
    # Persistence
    # ------------------------------------------------------------------

    def _save_state(self) -> None:
        state = {
            "patterns": [asdict(p) for p in self._patterns],
            "budget": asdict(self._budget),
        }
        tmp = self._state_path.with_suffix(".tmp")
        tmp.write_text(json.dumps(state, indent=2))
        tmp.replace(self._state_path)

    def _load_state(self) -> None:
        if not self._state_path.exists():
            return
        try:
            state = json.loads(self._state_path.read_text())
            self._patterns = [MigrationPattern(**p) for p in state.get("patterns", [])]
            budget_data = state.get("budget", {})
            self._budget = BudgetState(**{
                k: budget_data[k]
                for k in BudgetState.__dataclass_fields__
                if k in budget_data
            })
        except Exception as e:
            print(f"[MigrationIntelligence] Warning: could not load state: {e}")
