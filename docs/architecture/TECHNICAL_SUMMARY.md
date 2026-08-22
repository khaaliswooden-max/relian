# Relian — Technical Summary

**Companion to `relian-architecture.html` (the Build Atlas).**
Compiled 2026-08-22 against `main` at `179fe59`.

> **Status: pre-MVP research prototype.** Not production. Zero completed customer
> migrations. Zero countersigned reports issued. Every figure in this document was
> produced by a process that ran and is traceable to a file in this repository;
> anything not measured is written as `None` or "not measured", never as a default
> (R1). Numbers carry a Trutina grade and a basis (R9) — see §7.

---

## 1. What Relian is

Relian is a **legacy refactoring substrate**: a toolchain that assesses a COBOL estate,
resolves and lays out its data structures, deterministically transpiles the subset it
genuinely supports to Java, and proves the result by executing the original and the
migration against each other — then signs the outcome so a recipient can check it
without trusting Visionblox.

Two properties define the product, and everything else in the architecture is in
service of one of them:

1. **The customer's source code never leaves the customer's machine.** The CLI is
   offline and read-only over the codebase. Exactly one line crosses the perimeter,
   and it carries three digests and no content.
2. **Every quality number is a measurement or it is absent.** There are no constants,
   defaults, or formulas standing in for observations, and the pipeline refuses to
   attest anything it did not measure.

The commercial consequence of (2) is unusual and worth stating up front: Relian will
return *"we could not migrate this"* for programs a less careful tool would hand back
as finished Java. That refusal is the thing being sold.

---

## 2. Architecture

### 2.1 Inside the customer perimeter (R12, R6)

| Component | Path | Role |
|---|---|---|
| Intake | `src/assessment/intake.py` | File discovery, encoding, line endings |
| COBOL-85 parser | `src/parsers/` | ANTLR grammar → AST; `token_scan` fallback when a program will not parse |
| Copybook resolver | `src/discovery/copybook.py` | COPY fan-in graph, missing-copybook table |
| Layout engine | `src/discovery/layout.py` | Offsets, lengths, SYNC, REDEFINES, OCCURS DEPENDING ON, RENAMES |
| Assessment engine | `src/assessment/` | LOC, cyclomatic complexity, construct coverage, risk tier, report, CLI |
| C1 transpiler | `transpiler/c1_rulebased.py` | Dispatch table `SUPPORTED_STATEMENTS` → Java with BigDecimal |
| Orchestrator | `src/core/orchestrator.py` | Seven-stage state machine; stages 2 and 4 are removed, not disabled |
| Differential validation | `src/validation/differential.py` | Builds and executes both sides against the GnuCOBOL oracle |
| Report + signing | `src/discovery/report.py`, `signing.py` | Canonical `report.json`, manifest, Ed25519 instance signature |

Nothing in that column opens a socket or calls a model.

### 2.2 What crosses the perimeter

One line, produced by `report countersign-request`: **manifest hash, report id,
instance fingerprint**. No source, no field names, no record contents, no file paths.
The constraint is enforced by a leak test, not by convention.

### 2.3 Operator side (R4)

`tools/countersign.py` runs only in the operator's custody. The Visionblox release key
is read from disk and its passphrase is taken via `getpass` at the moment it is needed —
never from `argv` (world-readable in `/proc`, and it lands in shell history), never from
the environment (inherited by every child, dumped by crash reporters), and never
printed. Two of those three guards are AST walks over the tool, not greps, and each
asserts that it found something to walk.

**Two keys, two claim classes.** The benchmark seal key `233bb4406e2de606` signs
ledgers and never a customer deliverable. The Visionblox release key
`91e3a404155ba4dd` countersigns reports and never a benchmark. The countersigning tool
refuses the benchmark fingerprint outright. If either key is rotated or compromised,
the event does not put the other's claims in question — which a single key could not
give you.

### 2.4 Recipient side

`tools/verify_report.py --pin-fingerprint <fp>`. Four layers, each failing
independently: files against the manifest, the manifest hash, the instance signature,
the countersignature. The pin is **required and has no default**, because without one
a report re-signed under any key at all passes every internally-consistent check — the
files match the manifest, the manifest recomputes, and the signature verifies under the
key the forger generated. The pin is the only layer that sees it. The tool prints its
own SHA-256 before it says anything else, and that digest is pinned by the test suite.

### 2.5 Verification substrate (CI only, R3)

Two sealed, Ed25519-signed benchmarks whose ledgers CI verifies on every run:

- **RELIAN-BENCH v1.2** — 7 COBOL programs, 425 held-out and 89 public vectors,
  29 files under payload `a8695c2c…`. Thresholds live in the ledger: held-out
  BER ≥ 0.95, build rate 1.00, JaCoCo branch coverage ≥ 0.80.
- **RELIAN-DISCOVERY-BENCH v0.1** — 15 copybooks, 124 elementary fields, 170 probe
  rows, oracle layouts generated by GnuCOBOL 3.1.2.0.

Held-out vectors are not in this repository and are never read by an agent. They are
scored on `main` only. The reason is not runner minutes: a score returned on every push
is a feedback channel, and a feedback channel is something you can fit against one
commit at a time without ever intending to. Nudge a rounding mode, push, read the BER;
nudge it back, push, read it again. Nobody has to see a single held-out vector for the
held-out set to stop measuring generalisation.

### 2.6 What is deliberately absent

| Removed | Under | Why it mattered |
|---|---|---|
| Generative-AI semantic analysis (Stage 2) | R6, WP-2.0.-2 | Sent customer source to a hosted model |
| LLM test generation (Stage 4) | R6, WP-2.0.-2 | Same dependency, same egress |
| Risk-scoring limb (XGBoost) | R1, WP-2.0.-3 | Retrained every 20 migrations on rows whose "cognitive complexity" feature was cyclomatic × 1.2 — a fabricated input producing a fabricated score |

All three were **deleted rather than flagged off**: a flag can be flipped, a deleted
call cannot. As a result `semantic_score` has exactly one source — Stage 6 differential
execution — and `risk_score`, `test_coverage` and `tests_generated` are `None`/`0` by
construction rather than by configuration.

---

## 3. The production cycle

Seven stations. Three of them can end the run with no output.

| # | Station | Can it stop the run? |
|---|---|---|
| 01 | **Intake** — file discovery, encoding, line endings | no |
| 02 | **Assess** — LOC, complexity, construct coverage, risk tier | no |
| 03 | **Discover** — COPY fan-in, record layouts, gaps | signs or writes nothing |
| 04 | **Transpile** — dispatch table → Java; second pass must hash-match the first | **yes** — out of subset |
| 05 | **Validate** — `javac` + `cobc`, then every input through both binaries | **yes** — oracle unavailable |
| 06 | **Gate** — BER ≥ 0.95, build = 1.00, branch ≥ 0.80, all measured | **yes** — threshold missed |
| 07 | **Deliver** — signed, countersigned, recipient-verified | — |

Behavioral equivalence requires **byte-identical stdout *and* the same process exit
code**. A clean build is not evidence of equivalence and is never treated as such.

**The honest-failure rail.** An out-of-subset construct produces a refusal that names
the verb and the line; no Java is written, not even a commented placeholder. If the
oracle cannot be executed, equivalence is reported as *not measured* rather than
assumed. If a threshold is missed, no attestation is issued — signing a fabricated
number does not make it true, it makes it *tamper-evident*, which is worse, because it
lends the number a guarantee it has not earned.

The shipped demo exercises this deliberately: two of its programs are expected to fail,
and **one of them is 93% transpilable**. It still gets a diagnosed refusal and emits no
Java, because 93% is not 100% and a partial migration that looks whole is the failure
mode with a real cost.

---

## 4. Where the build stands

Twelve stops across four phases. The Build Atlas carries a movable flag; the position
of record is the `data-current` attribute on `#rail` in that file.

### Phase 0 — substrate · complete

1. **Bench sealed first.** RELIAN-BENCH's criteria were committed and signed before any
   transform work was scored against them. At v1.2's sealing, C1 was *red on it*
   (BER 0.7176, build 6/7) — which is the receipt that the ordering was real.
2. **C1 clears the bar.** 425/425 held-out vectors, build 1.0000, branch coverage
   0.8854, measured in CI on PR #15 at `8c676e9`.

### Phase 1 — assessment · complete (2026-08-16/17)

3. **Assessment engine.** WP-1.1 → WP-1.9. Read-only, offline, deterministic: two runs
   over the same tree produce byte-identical JSON and the same `report_hash`, and a
   CRLF copy scores identically to its LF original. The capability list is not
   hand-maintained — the analyzer reads `SUPPORTED_STATEMENTS` off the transpiler at
   call time and probes DATA DIVISION features by running programs through it. Features
   are reported three-state (*supported* / *accepted but ignored* / *unsupported*),
   because a clause that parses and is then discarded is not a clause you support.
4. **Honest failure wired.** Before WP-1.2 there was no `else` in the statement dispatch
   and no `unsupported()` anywhere: an unsupported verb that began a statement was
   silently discarded, and one that followed a statement was absorbed into its
   predecessor and surfaced as a misattributed error. The dispatch-table refactor was
   gated on bytes — all five generated Java files regenerated SHA-256-identical.

### Phase 2 — discovery · complete → **current position** (2026-08-20/21)

5. **Fabricated limbs removed**, environment pinned, CI switched to asserting the exact
   test triple rather than "no failures".
6. **RELIAN-DISCOVERY-BENCH v0.1 built and sealed** — before the code it judges (R7).
7. **Copybook resolver + layout engine** — 186/186 against the sealed oracle, tolerance
   zero. Gaps compare by projection rather than by label, so a padding byte cannot be
   renamed into agreement.
8. **The signed Data Discovery report** ← *we are here.* Canonical `report.json`, a
   Markdown rendering that names the JSON as authoritative, a manifest, and an Ed25519
   instance signature. If a key cannot be obtained the command writes nothing. Suite:
   852 passed, 10 skipped, 0 failed.

### Phase 3 — delivery · not built

9. **WP-2.4** — file inventory from SELECT / FD / JCL DD, lineage across programs,
   target-schema DDL, dictionary rendering. Absent today rather than approximated, and
   the shipped report says so on its own face.
10. **First countersigned report.** The flow is proven only under a stand-in key
    generated inside a test. Two things gate it: `visionblox-release-key-v1.pub` —
    public material, the one artifact still missing — reaching the repository so the
    published fingerprint can be checked against the actual key rather than a
    transcription of it; and one real engagement. Until then every report is
    **VALID AND UNATTESTED**, which is the correct label, not a caveat.

### Phase 4 — scope · not built

11. **Corpus v2 and scope extension.** Extending "migrates COBOL" is a benchmark action
    before it is a grammar action (R7): harvest third-party COBOL, seal it, and only
    then attempt CICS, VSAM, JCL, embedded SQL, or copybooks-in-transform. The bench
    commit must predate the grammar merge.
12. **Attestation that is not simulated**, plus KLEE symbolic test generation. Nothing
    simulated ships (R5), so the `simulated: true` flag stays until the integration is
    real. The risk model was deleted, not trained; any re-entry starts from zero.

---

## 5. Scope — what is and is not supported today

**Supported** (the COBOL-85 subset the sealed corpus exercises): COMP-3 arithmetic,
EVALUATE, PERFORM VARYING, OCCURS/SEARCH, INSPECT, edited pictures, VALUE,
CONTINUE / GOBACK / EXIT PROGRAM. Numeric business logic is emitted as BigDecimal with
explicit COBOL rounding semantics — ROUNDED is HALF_UP, an unrounded COMPUTE is DOWN.

**Not supported, and not claimed**: CICS, VSAM, JCL, embedded SQL, copybooks inside the
transform path, and IBM Enterprise COBOL semantics (the oracle is GnuCOBOL 3.1.2.0).

**The distance to a real estate is measured and large.** Across three third-party
corpora the assessment engine found **2,190 unsupported occurrences**, and six
constructs are roughly 70% of everything blocking migration:

| Construct | Occurrences |
|---|---|
| `EXIT` | 368 |
| `EXEC` | 306 |
| `CONTINUE` | 271 |
| `WRITE` | 224 |
| `GO TO` | 186 |
| `CALL` | 183 |

The cheapest third of that backlog is dispatch-table work with no new semantics.

---

## 6. Known limitations and open risks

- **No countersignature has ever been issued.** Both dry runs read
  *VALID AND UNATTESTED*.
- **No customer engagement has been run**, so there is no measured engagement duration,
  cost, or effort figure anywhere — and inventing one would be an R1 violation.
- **The public release key is not in the repository.** Until it is, the published
  fingerprint is re-derivable in principle but not checked against the actual key inside
  the suite. This is the single smallest open item with the largest credibility effect.
- **Third-party construct-coverage figures are graded PLAUSIBLE, not VERIFIED.** *Not
  one* third-party program parsed cleanly under the then-bundled grammar, so all three
  corpora were analysed through the `token_scan` fallback. The grammar was replaced at
  WP-2.0 and these figures **have not been re-measured since** — re-running the dry runs
  is cheap and would either confirm or correct them.
- **`README.md` is stale in one row.** Its capability table still lists "LLM semantic
  analysis — working when API keys present; informational only". That stage was deleted
  under R6 at WP-2.0.-2 and the orchestrator says so in a comment where the call used to
  be. The README row overstates what is present and should be corrected.
- **The transpiler's oracle is GnuCOBOL**, so equivalence is equivalence against
  GnuCOBOL. Any claim about mainframe behaviour requires an IBM oracle that does not
  exist here.

---

## 7. Provenance ledger (R9)

| Figure | Value | Grade | Basis |
|---|---|---|---|
| Held-out BER, C1 on v1.2 | 1.0000 (425/425) | VERIFIED | CI job 95240930271, PR #15 @ `8c676e9` · `docs/PHASE1_LOG.md` |
| Build rate | 1.0000 (7/7) | VERIFIED | same run |
| Branch coverage | 0.8854 | VERIFIED | JaCoCo 0.8.12, the ledger's required tool; bar 0.80 |
| C1 baseline at v1.2 sealing | BER 0.7176, build 6/7 | VERIFIED | `bench/LEDGER_relian-bench-v1.2.json` → `baselines_recorded` |
| v1.2 seal | 29 files, `a8695c2c…`, fp `233bb440…` | VERIFIED | Ed25519 over the manifest; checked by `verify_manifest.py` every CI run |
| Vector counts | 425 held-out, 89 public | VERIFIED | ledger `vector_counts`, summed over 7 programs |
| Discovery-bench corpus | 15 / 124 / 170 | VERIFIED | discovery ledger `corpus_counts` |
| Layout round-trip | 186/186, tolerance 0 | VERIFIED | `docs/PHASE2_LOG.md` WP-2.2 §2 |
| Test suite | 852 passed, 10 skipped, 0 failed | VERIFIED | `.github/workflows/tests.yml` asserts this exact triple |
| Demo, both-sides execution | 89 inputs, 7 programs, 100% | VERIFIED | `python3 -m demo`, ~15 s · `demo/README.md` |
| CardDemo assessment wall time | 4.61 s over 44 programs / 329 files | VERIFIED | WP-1.9 dry run, zero intervention |
| Construct coverage, third-party | 0.8511 / 0.6945 / 0.5968 | **PLAUSIBLE** | CardDemo / OMP / GnuCOBOL — all via `token_scan` fallback; not re-measured since the WP-2.0 grammar swap |
| Unsupported-construct demand signal | 2,190 occurrences | **PLAUSIBLE** | same three corpora, same caveat |
| Countersigned reports | 0 | VERIFIED | both dry runs VALID AND UNATTESTED |
| Completed migrations | 0 | VERIFIED | `README.md` |
| risk_score / test_coverage / tests_generated | None / None / 0 | NOT MEASURED | `None` by construction · `src/core/orchestrator.py` |
| Engagement duration, cost, effort | — | NO BASIS | none has been run; any figure would be an invention |

---

## 8. Using the Build Atlas in a presentation

`relian-architecture.html` is self-contained — no build step, no external assets beyond
Google Fonts, and it degrades to the authored state with JavaScript disabled. Open it in
a browser or project it directly.

**Moving the flag.** Click any stop on Plate III and the flag plants there, the stops
behind it fill in, the phase bands advance, and the record panel below swaps to that
stop's state. Arrow keys move it too. The controls under the panel step it forward and
back, and *Reset* returns it to the authored position.

**Making a move permanent.** The position of record is the `data-current` attribute on
`#rail` in the HTML — change it there and commit, and the file itself records where the
build stands. A click alone is a local view change. (When the page is published as an
artifact, a *Save this position for everyone* control appears for viewers who can write
to it; that path stores an override alongside the page and leaves the authored position
in the markup untouched.)

**As the build advances**, each newly completed stop needs three edits: its
`data-state` in the rail, its panel's status chip and body, and a row in the provenance
appendix. A stop should not move from *not built* to *measured* until a command has been
run and its output recorded — same rule as everywhere else.

---

Relian™ — Legacy Refactoring Substrate. © 2025–2026 Zuup, LLC / Visionblox LLC.
Proprietary. Held-out benchmark vectors are neither reproduced nor referenced in this
document (R3). No private key material appears here (R4).
