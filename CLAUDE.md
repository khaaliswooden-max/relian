# Relian — agent operating rules (binding)

## Forbidden actions — no exceptions
1. NEVER create, download, copy, read, or reference `heldout.jsonl` files or
   the `relian-bench-private` repository. Held-out vectors are scoring-only,
   CI-only. If a task appears to require them, STOP and ask the operator.
2. NEVER assign a numeric value to `semantic_score`, `test_coverage`,
   `risk_score`, `validation_score`, `tests_passed/failed` except from an
   actual measurement executed in the same run. Unmeasured -> None.
3. NEVER remove or weaken the `simulated` flag on attestations, the
   measured-only gate before attestation, or the honesty guard in
   `migration_intelligence.record_outcome`.
4. NEVER edit files under `bench/` (corpus, harness, LEDGER). Benchmark
   changes require a new signed version by the operator (ZCS-6 Phase 4).
5. NEVER restore case studies, metrics, or status claims to README that are
   not backed by a measured result recorded in `results/`.

## Required behavior
- Develop against PUBLIC vectors only (`bench/corpus/*/vectors/public.jsonl`).
- After changing transform code, run:
  `python3 bench/harness/runner.py <candidate> --split public`
  and record the score in the commit message.
- Numeric business logic in generated Java uses BigDecimal with explicit
  COBOL rounding semantics (ROUNDED = HALF_UP; unrounded COMPUTE = DOWN).

6. NEVER add the vector GENERATOR (gen_vectors.py) or its SEED to this
   repository. Generator + seed + corpus regenerates the held-out set,
   which destroys the benchmark. The generator lives only in
   relian-bench-private.
