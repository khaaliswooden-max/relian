# RELIAN-BENCH v1.0 — Specification

**Tag:** `relian-bench-v1.0`
**Author:** A. Khaalis Wooden, Sr. (MBA; MSIT Candidate, Southern New Hampshire University) — Visionblox LLC / Zuup Innovation Lab
**Classification:** INTERNAL
**ZCS-6 phases covered:** 2 (build benchmark), 3 (attack it), 4 (defend deterministically)
**Status:** COMMITTED — frozen and Ed25519-signed prior to any solution work.

---

## 1. Why this exists

Relian's pipeline reported quality metrics that were **constants, not observations**:

| Reported | Actual implementation (pre-commit) |
|---|---|
| `test_coverage` | `min(80.0, len(tests) * 10)` |
| `validation_score` | hardcoded `95.0` |
| `tests_failed` | hardcoded `0` |
| `semantic_score` | hardcoded `0.85` on fallback |
| Solana attestation | local SHA256 from `_simulate_transaction()` |

The system returned its **targets as though they were results**, then signed them. This benchmark exists to make every downstream quality claim measured, checkable, and re-derivable by a third party.

## 2. The cardinal rule

> **A metric is either MEASURED or it is `null`. There is no third option.**

No constant, no formula standing in for an observation, no fallback value. If a metric cannot be measured, the run reports `null` and is marked **INVALID**. A benchmark whose harness can fabricate is worthless.

## 3. The oracle

**The compiled legacy COBOL binary is the oracle.** Expected outputs are never hand-authored and never predicted by a model. They are produced by executing the legacy program. If the legacy will not execute for an input, **no vector is emitted** — we do not guess.

This is the load-bearing design choice. It means "did the migration preserve behavior?" becomes an *empirical question with a decidable answer*, not an LLM's self-assessed confidence.

## 4. Corpus

Five COBOL programs, authored to mirror the workloads Montana agencies actually run (payroll, benefits eligibility, taxation, validation). All are I/O-driven (stdin → stdout) so differential comparison is language-agnostic.

| Program | Domain | COBOL semantics exercised |
|---|---|---|
| `P01_payroll` | Gross-to-net payroll | `COMPUTE ROUNDED` (half-up), COMP-3, overtime cliff at 40.00, graduated withholding, negative-taxable floor |
| `P02_interest` | Compound interest | `PERFORM VARYING`, iterative rounding accumulation, **truncation** on unROUNDED COMPUTE |
| `P03_eligibility` | Benefits determination (FPL) | 88-level condition names, `EVALUATE TRUE` **branch-order dependency** |
| `P04_taxtable` | Graduated tax | `OCCURS` table, `SEARCH`, indexes, cumulative bracket arithmetic |
| `P05_validate` | Record validation | `INSPECT TALLYING`, `FUNCTION LENGTH/TRIM`, error-code precedence |

### Deliberate traps

These are where real migrations break, and where a plausible-looking LLM translation silently diverges:

- **Rounding mode.** COBOL `ROUNDED` is HALF_UP (away from zero). Java's `BigDecimal` default and IEEE-754 doubles are not. A float-based translation fails on the cent.
- **Truncation.** `COMPUTE` *without* `ROUNDED` truncates toward zero to the receiving PIC scale. `WS-MRATE PIC 9(3)V9(8)` in P02 truncates at 8dp, then compounds 360 times — the divergence amplifies.
- **Branch order.** In P03 a 70-year-old disabled applicant at 144% FPL matches **both** the DISABILITY and AGED branches. `EVALUATE` order decides. Reorder the branches and `STATUS` stays correct while `CATEGORY` is silently wrong — a defect no smoke test catches.
- **Boundary values.** Vector domains deliberately oversample the cliffs: exactly 40.00 hours, exact bracket ceilings, zero, negative, empty.

## 5. Metrics

| Metric | Definition | Measured how |
|---|---|---|
| **BER** (Behavioral Equivalence Rate) | Fraction of **held-out** vectors where migrated stdout == legacy stdout | Execute both, compare |
| **build_rate** | Fraction of programs whose migration compiles | `javac` exit code |
| **branch_coverage** | Branch coverage of migrated code under the vector suite | JaCoCo — **`null` until wired** |
| **wall_seconds** | End-to-end wall clock | Measured |

**BER is the honest replacement for the unmeasurable claim "95% semantic preservation."** Whitespace is normalized; **numbers are not** — a rounding divergence is a real defect, not a formatting nit.

A program that fails to compile scores **0 over its full vector count**. Failing to build is not an excuse.

## 6. Anti-gaming controls (Phase 3)

The benchmark was attacked before it was committed.

| Attack | Control |
|---|---|
| Overfit to the examples | **Public/held-out split.** 12 public vectors per program may be shown to the migrator; **60 held-out vectors per program are never shipped** and are the sole basis for scoring |
| Shell out to the COBOL binary | Static scan rejects `Runtime.getRuntime`, `ProcessBuilder`, `exec(`, `cobcrun`, `libcob`, binary names, `System.load` |
| Embed the answers | Static scan rejects `heldout`, `/corpus/`, vector filenames |
| Silently soften a threshold | Thresholds are inside the signed manifest; editing them breaks the Ed25519 signature |
| Quietly edit a vector | Vectors are hashed into the manifest; editing one changes `payload_sha256` |

**Trivial-pass floor test:** the null candidate (Relian's current `_transform_to_java()` output) must score ≈0. **It does — 0.0000.** A benchmark that a stub can pass has no discriminating power.

## 7. Committed thresholds

Frozen at commit. Changing these requires a **new versioned benchmark and a written explanation** — never a silent edit.

```
ber_heldout_min      = 0.95      # the real "95% semantic preservation"
build_rate_min       = 1.00
branch_coverage_min  = 0.80
coverage_required_tool = jacoco
```

## 8. Recorded baselines (the floor, measured before solution work)

| Candidate | What it is | BER (held-out) | build_rate | coverage |
|---|---|---|---|---|
| **B0_null** | Relian's actual current output | **0.0000** (0/300) | 1.0000 | `null` |
| **B2_reference** | Faithful hand translation (P01/P03/P05) | **1.0000** (180/180) | 1.0000 | `null` |

**The single most important number here: B0 = 0.0000 while `build_rate` = 1.0000.** Relian's current output *compiles cleanly* and preserves *zero* behavior — while its internal metrics reported `semantic_score=85.0` and `validation_score=95.0`. That gap, 85 reported vs 0 actual, is precisely what this benchmark exists to make impossible.

B2 = 1.0000 proves the benchmark is calibrated in both directions: it is not merely always-zero, and a genuinely correct migration is recognized.

## 9. Deterministic defense (Phase 4)

- Bundle frozen, `sha256` per file, manifest hashed, **Ed25519-signed** (Aletheia DAC pattern).
- Signature verified; both tamper attacks (threshold softening, vector edit) **detected**.
- Cross-platform manifest bugs from `zil_sign.py` explicitly avoided: `Path.as_posix()` used unconditionally; sort by explicit posix **string**, never `Path` objects.
- Candidate outputs are excluded from the manifest — the benchmark must be independent of anything under test.
- **The commit hash precedes any solution timestamp.** That ordering is the goalpost-prevention mechanism.

## 10. Gap analysis — honest

What is **not** solved. Flagged rather than papered over.

- **Branch coverage is `null`.** JaCoCo is not wired. Per the cardinal rule it reports `null`, not a plausible number — this is the design rule demonstrating itself on the very first run. Until wired, the 0.80 coverage threshold is **unenforceable** and no coverage claim may be made externally.
- **Corpus is 5 programs / ~1,100 LOC, not 50K LOC.** It exercises the semantics that break migrations, but it does **not** establish the MVP milestone. No claim of "50K LOC capability" is supported by this benchmark.
- **No CICS, VSAM, JCL, copybooks, or embedded SQL.** Real state mainframes are dominated by these. This benchmark measures computational-core migration only. That is a real scope limit and must be stated in any customer-facing use.
- **Corpus is authored, not harvested.** Programs are representative and trap-rich but are not production artifacts with decades of accreted edge cases.
- **B2_reference is hand-written by an expert**, so it establishes the *ceiling is reachable* — it does not establish that any automated system reaches it.
- **P02/P04 reference translations not yet written**; B2 covers 3 of 5.
- **Velocity (LOC/day) is not meaningfully measurable** at this corpus size. No velocity claim is supported.
- **Defect density (<5/KLOC) is not measured** and remains an uncommitted claim.

## 11. What this authorizes you to say

**Supported today:** "We measure behavioral equivalence against the legacy system on held-out vectors, cryptographically commit the benchmark before building, and can prove our baseline was zero."

**Not supported today:** any semantic-preservation percentage, coverage percentage, velocity, defect density, or LOC-capability claim for Relian.

Until an automated candidate scores against this benchmark, **Relian has no measured quality claims.** That is the honest position, and it is a stronger one than the alternative — because it is the only claim in the field that a state auditor can independently re-derive.

## 12. Reproduce

```bash
apt-get install -y gnucobol default-jdk-headless
cd corpus/P01_payroll && cobc -x program.cbl -o payroll01   # etc.
python3 harness/gen_vectors.py          # regenerate vectors from the oracle
python3 harness/runner.py B0_null       # score a candidate
python3 harness/commit.py               # freeze + sign
```

Verify any result against the committed bundle by hash:
`payload_sha256` and `manifest_sha256` are recorded in `LEDGER_relian-bench-v1.0.json`.
