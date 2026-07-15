# Relian™ — Legacy Refactoring Substrate

**Status: PRE-MVP RESEARCH PROTOTYPE. Not production. No completed migrations. No customers.**

Relian is an in-development platform for AI-assisted, verifiable migration of
legacy code (initial path: COBOL → Java). It is developed by Zuup Innovation
Lab / Visionblox LLC.

## What exists today (measured, not projected)

| Component | State |
|---|---|
| COBOL-85 ANTLR parser + AST | Working |
| Deterministic transform adapters (OpenRewrite, Piranha, rope, jscodeshift) | Working scaffolds |
| Orchestrator pipeline (7-stage state machine) | Working; transform stage is a stub |
| LLM semantic analysis | Working when API keys present; **informational only** |
| Differential validation vs. legacy oracle | Wired to RELIAN-BENCH harness |
| Test generation (KLEE/symbolic) | Returns empty until KLEE integration lands |
| Risk scoring | Heuristic only; ML model **not yet trained** |
| Attestation | **Simulated locally**; self-identifies as `simulated: true` |

## Measured baseline (RELIAN-BENCH v1.0, committed & Ed25519-signed)

| Candidate | Behavioral Equivalence (held-out) | Build rate |
|---|---|---|
| Current transform output | **0.0000** | 1.0000 |
| Hand-written reference translation | **1.0000** | 1.0000 |

The benchmark ledger (`bench/LEDGER_relian-bench-v1.0.json`) freezes the
success criteria — BER ≥ 0.95 on held-out vectors — **before** solution work.
Held-out vectors are maintained privately and are not in this repository.

**Every quality metric in this codebase is measured or `None`. No constants,
no formulas standing in for observations.** Attestation refuses to sign
unmeasured values.

## Quality targets (NOT results)

Semantic preservation ≥95%, coverage ≥80%, <5 defects/KLOC are **targets**
defined in the committed benchmark. No target may be stated as an achieved
result until a candidate clears it on held-out vectors.

## Quick start

```bash
./scripts/setup/dev-setup.sh
python examples/migrate.py --source examples/cobol/banking-system.cbl --target java --output ./output/
python bench/harness/runner.py <candidate>   # score against public vectors
```

## License

Proprietary — © 2025–2026 Zuup, LLC / Visionblox LLC. All rights reserved.
See LICENSE. Patent status: consult counsel before any public patent-pending
representation.

## Contact

khaalis.wooden@visionblox.com
