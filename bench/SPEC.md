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


---

# v1.1 Addendum — first automated candidate (measured)

**Changes in v1.1 (additive; corpus and vectors hash-identical to v1.0):**
JaCoCo 0.8.12 branch coverage wired into the runner. Thresholds unchanged.
Re-versioned and re-signed because harness files changed — ZCS-6 forbids
silent edits to a committed benchmark.

## Results table (all measured, held-out split)

| Candidate | BER | build_rate | branch coverage | Verdict vs committed bar |
|---|---|---|---|---|
| B0_null (Relian today) | 0.0000 (0/300) | 1.0000 | null (zero branches) | FAIL |
| B2_reference (hand-written, now 5/5) | 1.0000 (300/300) | 1.0000 | 0.8415 | **PASS** |
| C1_rulebased (deterministic transpiler) | **1.0000 (300/300)** | 1.0000 | 0.5652 | **FAIL — coverage** |

## C1_rulebased notes

- Deterministic COBOL→Java transpiler (~430 lines Python): PIC-clause-driven
  storage semantics (ROUNDED=HALF_UP, unROUNDED COMPUTE=truncate), verb
  handlers for ACCEPT/UNSTRING/COMPUTE/MOVE/IF/EVALUATE/PERFORM VARYING/
  SEARCH/OCCURS/INSPECT/DISPLAY, shunting-yard expression translation to
  BigDecimal. No LLM. No vector data. Anti-gaming scan: clean.
- Development trace: iteration 1 scored 0.0 public (PIC regex missing digits;
  tokenizer stall on subscripts). Iteration 2: 60/60 public, then 300/300
  held-out. Score progression per commit — the ZCS-6 Phase 5 loop, working.
- **Why it fails the bar:** generated code embeds a generic runtime helper
  whose branches (sign handling, padding paths) the vector suite never
  reaches — 56.5% branch coverage vs the committed 0.80. Dead/unexercised
  code in migration output is a real quality defect. The remediation is a
  slimmer emitted runtime and/or richer vectors — NOT a softer threshold.

## Contamination disclosure (honest)

Corpus, reference translations, and C1 share an author. C1's rules are
structural and vector-blind (inspectable in transpiler/c1_rulebased.py),
but its generality beyond the corpus's COBOL-85 subset is UNPROVEN. The
claim these results support is scoped: "a deterministic rule-based migrator
achieves BER 1.0 on the committed corpus," not "Relian migrates COBOL."
Next de-risk: third-party COBOL (e.g., open-source GnuCOBOL test programs)
added as corpus v2 by someone other than the transpiler's author.

## Updated claims register

Now supported: "A deterministic transpilation core achieved 100% behavioral
equivalence on 300 held-out oracle vectors and failed our own committed
coverage gate at 56.5% — both numbers independently re-derivable."
Still not supported: velocity, defect density, 50K-LOC capability, any
claim beyond the corpus subset.

---

# v1.2 Addendum — exit codes, corpus 5 → 7 (PREPARED, UNSEALED)

**Status: sealing prep only.** This addendum describes the v1.2 content
prepared in-repo (PR "bench: v1.2 sealing prep"); it is **not sealed**
until Khaalis re-hashes and Ed25519-signs `LEDGER_relian-bench-v1.2.json`
(ZCS-6 Phase 4). Until that signature exists, v1.1 remains the benchmark
of record and the CI gate scores against the v1.1 ledger. Runbook:
`docs/SEALING_v1.2.md`.

**Changes in v1.2:**

1. **Behavioral equivalence includes RETURN-CODE** (WP-1.5.0d, merged
   `c7b199f`): the runner captures each process's exit code and a vector
   matches only if stdout AND exit code match. This is the intended
   divergence from v1.1: every public vector now carries an explicit
   `expected_exit` — `0` for all pre-existing vectors (each re-verified
   against a freshly compiled GnuCOBOL 3.1.2 oracle before the rewrite;
   they were only ever recorded against zero-exit runs), measured for
   new vectors. Harness files (`runner.py`, `coverage.py`) changed after
   the v1.1 signature and are re-hashed at sealing — the same
   "harness files changed → re-version" rule the v1.1 addendum states.
2. **Corpus grows 5 → 7** (WP-1.5.4 / WP-1.5.5 drafts promoted):
   - `P06_valinit` (VALINIT01) — VALUE-clause semantics: numeric,
     alphanumeric and COMP-3 VALUE, group-level VALUE over subordinates,
     88-levels with single and multiple values. A zero-initializing
     migration fails on vector 1.
   - `P07_exitflow` (EXITFLW01, rev 3) — CONTINUE (bare / in IF / in
     EVALUATE), EXIT PROGRAM in a main program (a measured no-op), and
     GOBACK with RETURN-CODE 0, 4 and 8. Nonzero-exit vectors are
     legitimate under the WP-1.5.0d scorer; 4 of its 12 public vectors
     expect nonzero exits. No vector depends on the measured GnuCOBOL
     3.1.2 lone-`EXIT PROGRAM`-in-`WHEN` chaining quirk (the program
     structurally excludes it).
   12 public vectors each, oracle-generated with GnuCOBOL 3.1.2 (the CI
   toolchain). Held-out vectors: private generator at sealing (60 each;
   UNSEALED input proposals in `bench/candidates/heldout_proposals_v1.2/`).
3. **P04_taxtable +5 public vectors** in the SEARCH AT-END window
   `(999999999.00, 999999999.99]`, closing the loop-exhaust /
   AT-END-fallback vector-coverage gap (P04 branch coverage 11/16 →
   13/16 measured). **Known limitation:** at the bracket edge the WHEN
   and AT END paths print identical output (sub-cent differences vanish
   in the final ROUNDED HALF_UP), so these vectors prove branch exercise
   and behavioral agreement but cannot alone distinguish a candidate
   that hard-codes bracket 5 as its fallback. A discriminating vector
   (one requiring an observable AT-END-only effect) is flagged as a
   **v1.3 candidate**.
4. **Thresholds unchanged** (`ber_heldout_min 0.95`, `build_rate_min
   1.00`, `branch_coverage_min 0.80`, jacoco).

**Pre-implementation baseline (public split, measured 2026-08-16):**
existing five programs byte-identical Java, BER 1.0 (65/65 incl. P04's
17); P06_valinit compiles but scores 0/12 (C1 discards VALUE);
P07_exitflow fails to transpile (honest uncompilable stub). Aggregate:
BER 0.7303 (65/89), build_rate 0.8571 (6/7). The red is the point —
bench precedes handlers (R7), and the first post-sealing CI run is the
held-out measurement of record.
