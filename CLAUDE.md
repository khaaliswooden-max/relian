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
4. NEVER edit files under `bench/` (corpus, harness, LEDGER). NEVER edit files
   under `discovery-bench/` once the tag `relian-discovery-bench-v0.1` exists —
   before that tag it is authorable; after it, it is frozen identically to
   `bench/`. Benchmark changes require a new signed version by the operator
   (ZCS-6 Phase 4).
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

## RELIAN ENGINEERING RULES (binding — v1.1, Aug 2026)
R1. A metric is measured or None — never a constant, default, or estimate presented as a measurement. Applies to code, tests, report templates, and UI.
R2. Honest failure is a feature. Unsupported COBOL → explicit no-attestation result with construct inventory. Never silently pass, skip, or approximate.
R3. Never read, regenerate, copy, or reference contents of relian-bench-private. Held-out eval runs only in CI. If a task seems to need the vectors, STOP and escalate to Khaalis.
R4. Private signing keys never enter the repo, .env, CI vars, or this context. Custody is Khaalis-only. Code signs via interfaces.
R5. Nothing simulated ships. No `simulated`/`mock` in mainline paths. Absence of a real integration is stated plainly.
R6. No customer/State source code to any generative-AI model during transformation. Optional local analysis (Ollama) is off by default, disclosed.
R7. Bench-first: no construct or language pair is "supported" until RELIAN-BENCH covers it, is sealed, and CI held-out passes. Bench commit predates grammar merge.
R8. Determinism: core.autocrlf=false; Path.as_posix() for hashed bytes; sort by posix-string form.
R9. Every externally visible number carries a Trutina grade (VERIFIED/PLAUSIBLE/SPECULATIVE) and a provenance field.
R10. Main is protected; thresholds (BER 1.0000 supported scope, build 1.00, branch cov ≥0.80) are merge gates.
R11. Quote/report templates reference only capabilities in the quotable-capability matrix.
R12. Customer source stays in the customer perimeter (CLI/on-prem). Hosted platform stores Visionblox artifacts only.

## PHASE 1 SCOPE (current)
Building `src/assessment/` — the $8K Legacy Code Assessment engine. Read-only over customer code. Imports COBOLParser (src/parsers/cobol.py) and introspects transpiler/c1_rulebased.py. Does NOT modify the transpiler except WP-1.2 (dispatch-table refactor, behavior-preserving, bench-gated).
