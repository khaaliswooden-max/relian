# Relian™ — Legacy Refactoring Substrate

**Status: PRE-MVP RESEARCH PROTOTYPE. Not production. No completed migrations. No customers.**

Relian is an in-development platform for AI-assisted, verifiable migration of
legacy code (initial path: COBOL → Java). It is developed by Zuup Innovation
Lab / Visionblox LLC.

## Architecture and build state

**https://khaaliswooden-max.github.io/relian/** — two tabs, republished on every
push to `main` that touches it:

- **Build Atlas** — system architecture, the production cycle, and the build
  timeline with a movable position flag
- **Technical Summary** — the same ground in prose, with the provenance ledger

It is **one self-contained HTML file** — `docs/architecture/relian-architecture.html`
— that you can also open from disk, email, or project. Both tabs live inside it, so
the summary is there with JavaScript off and when printed. Every figure on it carries
a Trutina grade and a basis in the provenance appendix. See
`docs/architecture/README.md`.

## What exists today (measured, not projected)

| Component | State |
|---|---|
| COBOL-85 ANTLR parser + AST | Working |
| Deterministic COBOL→Java transpiler (C1) | **Working — full committed-bar PASS** (below) |
| Deterministic transform adapters (OpenRewrite, Piranha, rope, jscodeshift) | Working scaffolds |
| Orchestrator pipeline (7-stage state machine) | Working; **5 of the 7 stages execute** — stages 2 and 4 were removed under R6 (rows below). Transform stage is the deterministic C1 core |
| LLM semantic analysis | **Removed** (WP-2.0.-2, R6). It sent customer source code to a hosted model, so it was deleted rather than gated — a flag can be flipped, a deleted call cannot. `semantic_score` is now set by one thing only: differential execution against the legacy oracle |
| Differential validation vs. legacy oracle | Wired to RELIAN-BENCH harness |
| Test generation (KLEE/symbolic) | **Not wired.** The LLM leg was removed with stage 4 (WP-2.0.-2, R6); the symbolic leg was never integrated. `src/generators/tests.py` is an unwired stub whose methods return `[]`. `tests_generated` is 0 and `test_coverage` is `None` by construction |
| Risk scoring | **In the pipeline: none.** The model limb was deleted (WP-2.0.-3, R1) — not left untrained — and `risk_score` is `None` by construction. **As a product:** `src/assessment/risk.py`, a published deterministic tiering rule reached through the assessment CLI and `/api/v1/assess`. The tier is graded PLAUSIBLE (a policy is not a measurement); its inputs are VERIFIED |
| Attestation | **Simulated locally**; self-identifies as `simulated: true` |

## Measured baseline (RELIAN-BENCH v1.0, committed & Ed25519-signed)

| Candidate | BER (held-out) | Build | Branch coverage | Committed bar |
|---|---|---|---|---|
| B0 placeholder (pre-integration) | 0.0000 | 1.0000 | n/a (no branches) | FAIL |
| B2 hand-written reference | 1.0000 | 1.0000 | 0.8415 | PASS |
| **C1 deterministic transpiler — now the pipeline core** | **1.0000** (300/300) | **1.0000** | **0.8824** | **PASS** |

The pipeline end-to-end now reports a MEASURED semantic score of 100.0 on
every corpus program, and FAILS honestly (no placeholder, no attestation)
on programs outside the transpiler's COBOL-85 subset.

The benchmark ledger (`bench/LEDGER_relian-bench-v1.0.json`) freezes the
success criteria — BER ≥ 0.95 on held-out vectors — **before** solution work.
Held-out vectors are maintained privately and are not in this repository.

**Every quality metric in this codebase is measured or `None`. No constants,
no formulas standing in for observations.** Attestation refuses to sign
unmeasured values, and the transform refuses to emit placeholders for
programs it cannot faithfully migrate — unsupported constructs produce a
FAILED migration, not fake output.

Scope honesty: "migrates COBOL" is supported today only for the COBOL-85
subset exercised by the committed corpus (COMP-3 arithmetic, EVALUATE,
PERFORM VARYING, OCCURS/SEARCH, INSPECT, edited pictures). No CICS, VSAM,
JCL, copybooks, or embedded SQL. Corpus v2 (third-party harvested COBOL)
is the next scope-extension gate.

## Quality targets (NOT results)

Semantic preservation ≥95%, coverage ≥80%, <5 defects/KLOC are **targets**
defined in the committed benchmark. No target may be stated as an achieved
result until a candidate clears it on held-out vectors.

## Quick start

```bash
./scripts/setup/dev-setup.sh
python examples/migrate.py --source examples/cobol/banking-system.cbl --target java --output ./output/
python scripts/bench_public.py --out /tmp/c1   # score against PUBLIC vectors
```

## See it work

```bash
apt-get install -y gnucobol      # the legacy oracle
python3 -m demo                  # ~15s
python3 -m demo --html out/relian.html
```

`python3 -m demo` runs the shipped assessment engine and the shipped transpiler
over real COBOL, then builds **both** the original (GnuCOBOL) and the migration
(javac) and executes them against each other input by input. Equivalence
requires byte-identical stdout *and* the same process exit code. Every figure it
prints was produced by a process that ran during that invocation — nothing is
replayed from a file, and where the legacy side cannot be executed it reports
equivalence as *not measured* rather than assuming it.

It also runs two programs it is expected to fail on, because what a migration
tool does outside its depth is the only question that matters. Both get a
diagnosed refusal naming the verb and the line, emit no Java, and issue no
attestation — including one that is 93% transpilable, because 93% is not 100%.
See [`demo/README.md`](demo/README.md).

## License

Proprietary — © 2025–2026 Zuup, LLC / Visionblox LLC. All rights reserved.
See LICENSE. Patent status: consult counsel before any public patent-pending
representation.

## Contact

khaalis.wooden@visionblox.com
