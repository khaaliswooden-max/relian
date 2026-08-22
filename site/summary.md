# Relian — Technical Summary

**Companion to the Build Atlas.** Built from `{{fig:bench_tag}}`, `{{fig:disc_tag}}` and the
commit under build — see the header for which commit that is.

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

**Two keys, two claim classes.** The benchmark seal key `{{fig:seal_fingerprint}}` signs
ledgers and never a customer deliverable. The Visionblox release key
`{{fig:release_fingerprint}}` countersigns reports and never a benchmark. The countersigning tool
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

- **RELIAN-BENCH {{fig:bench_version}}** — {{fig:bench_programs}} COBOL programs, {{fig:heldout_vectors}} held-out and {{fig:public_vectors}} public vectors,
  {{fig:bench_files}} files under payload `{{fig:bench_payload}}…`. Thresholds live in the ledger: held-out
  BER ≥ {{fig:ber_bar}}, build rate {{fig:build_bar}}, JaCoCo branch coverage ≥ {{fig:coverage_bar}}.
- **RELIAN-DISCOVERY-BENCH {{fig:disc_version}}** — {{fig:disc_copybooks}} copybooks, {{fig:disc_fields}} elementary fields, {{fig:disc_probes}} probe
  rows, oracle layouts generated by {{fig:disc_oracle}}.

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
| 06 | **Gate** — BER ≥ {{fig:ber_bar}}, build = {{fig:build_bar}}, branch ≥ {{fig:coverage_bar}}, all measured | **yes** — threshold missed |
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
   transform work was scored against them. At {{fig:bench_version}}'s sealing, C1 was *red on it*
   (BER {{fig:c1_seal_ber}}, build {{fig:c1_seal_build}}) — which is the receipt that the ordering was real.
2. **C1 clears the bar.** {{fig:ber_matched}} held-out vectors, build {{fig:build_rate}}, branch coverage
   {{fig:branch_coverage}}, measured in CI on PR #15 at `8c676e9`.

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
6. **RELIAN-DISCOVERY-BENCH {{fig:disc_version}} built and sealed** — before the code it judges (R7).
7. **Copybook resolver + layout engine** — {{fig:layout_roundtrip}} against the sealed oracle, tolerance
   zero. Gaps compare by projection rather than by label, so a padding byte cannot be
   renamed into agreement.
8. **The signed Data Discovery report** ← *we are here.* Canonical `report.json`, a
   Markdown rendering that names the JSON as authoritative, a manifest, and an Ed25519
   instance signature. If a key cannot be obtained the command writes nothing. Suite:
   {{fig:suite_passed}} passed, {{fig:suite_skipped}} skipped, {{fig:suite_failed}} failed.

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
transform path, and IBM Enterprise COBOL semantics (the oracle is {{fig:disc_oracle}}).

**The distance to a real estate is measured and large.** Across three third-party
corpora the assessment engine found **{{fig:demand_total}} unsupported occurrences**, and six
constructs are roughly 70% of everything blocking migration:

| Construct | Occurrences |
|---|---|
| `EXIT` | {{fig:demand_exit}} |
| `EXEC` | {{fig:demand_exec}} |
| `CONTINUE` | {{fig:demand_continue}} |
| `WRITE` | {{fig:demand_write}} |
| `GO TO` | {{fig:demand_goto}} |
| `CALL` | {{fig:demand_call}} |

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
- **`README.md` capability table — corrected 2026-08-22.** Four rows had gone stale
  against the WP-2.0.-2 / WP-2.0.-3 removals: "LLM semantic analysis — working when API
  keys present" (the stage was deleted under R6), test generation and risk scoring (both
  described as pending integration or training when the code paths were deleted), and the
  orchestrator's stage count. All four now state the removals, and the risk row names the
  product that *does* exist — `src/assessment/risk.py`, graded PLAUSIBLE as a policy.
- **The transpiler's oracle is {{fig:disc_oracle}}**, so equivalence is equivalence against
  GnuCOBOL. Any claim about mainframe behaviour requires an IBM oracle that does not
  exist here.

---

## 7. Provenance ledger (R9)

| Figure | Value | Grade | Basis |
|---|---|---|---|
| Held-out BER, C1 on {{fig:bench_version}} | {{fig:ber_heldout}} ({{fig:ber_matched}}) | VERIFIED | CI job 95240930271, PR #15 @ `8c676e9` · `docs/PHASE1_LOG.md` |
| Build rate | {{fig:build_rate}} ({{fig:bench_programs}}/{{fig:bench_programs}}) | VERIFIED | same run |
| Branch coverage | {{fig:branch_coverage}} | VERIFIED | JaCoCo 0.8.12, the ledger's required tool; bar {{fig:coverage_bar}} |
| C1 baseline at {{fig:bench_version}} sealing | BER {{fig:c1_seal_ber}}, build {{fig:c1_seal_build}} | VERIFIED | `bench/LEDGER_{{fig:bench_tag}}.json` → `baselines_recorded` |
| {{fig:bench_version}} seal | {{fig:bench_files}} files, `{{fig:bench_payload}}…`, fp `{{fig:seal_fingerprint}}` | VERIFIED | Ed25519 over the manifest; checked by `verify_manifest.py` every CI run |
| Vector counts | {{fig:heldout_vectors}} held-out, {{fig:public_vectors}} public | VERIFIED | ledger `vector_counts`, summed over 7 programs |
| Discovery-bench corpus | {{fig:disc_copybooks}} / {{fig:disc_fields}} / {{fig:disc_probes}} | VERIFIED | discovery ledger `corpus_counts` |
| Layout round-trip | {{fig:layout_roundtrip}}, tolerance 0 | VERIFIED | `docs/PHASE2_LOG.md` WP-2.2 §2 |
| Test suite | {{fig:suite_passed}} passed, {{fig:suite_skipped}} skipped, {{fig:suite_failed}} failed | VERIFIED | `.github/workflows/tests.yml` asserts this exact triple |
| Demo, both-sides execution | {{fig:demo_inputs}} inputs, {{fig:demo_programs}} programs, 100% | VERIFIED | `python3 -m demo`, ~15 s · `demo/README.md` |
| CardDemo assessment wall time | {{fig:carddemo_seconds}} s over {{fig:carddemo_programs}} programs / {{fig:carddemo_files}} files | VERIFIED | WP-1.9 dry run, zero intervention |
| Construct coverage, third-party | {{fig:cov_carddemo}} / {{fig:cov_omp}} / {{fig:cov_gnucobol}} | **PLAUSIBLE** | CardDemo / OMP / GnuCOBOL — all via `token_scan` fallback; not re-measured since the WP-2.0 grammar swap |
| Unsupported-construct demand signal | {{fig:demand_total}} occurrences | **PLAUSIBLE** | same three corpora, same caveat |
| Countersigned reports | {{fig:countersignatures}} | VERIFIED | both dry runs VALID AND UNATTESTED |
| Completed migrations | {{fig:migrations}} | VERIFIED | `README.md` |
| risk_score / test_coverage / tests_generated | None / None / 0 | NOT MEASURED | `None` by construction · `src/core/orchestrator.py` |
| Engagement duration, cost, effort | — | NO BASIS | none has been run; any figure would be an invention |

---

## 8. Using the Build Atlas, and keeping it true

### The site

Two tabs, one build. **Build Atlas** is the three-plate visual brief; **Technical
Summary** is this document. Both are generated by `tools/build_site.py` from
`site/`, and `.github/workflows/pages.yml` rebuilds and republishes them on every
push to `main`. Nothing is deployed by hand.

    python3 tools/build_site.py            # -> _site/
    python3 tools/build_site.py --check    # verify only, write nothing

### Moving the position flag

Click any stop on Plate III and the flag plants there, the stops behind it fill
in, the phase bands advance, and the record panel swaps to that stop's state.
Arrow keys work too, and the controls under the panel step forward and back.

That is a local view change. **The position of record is one line in
`site/figures.json`:**

    "position": "s8"

Change it, push, and the deployed page moves — the builder stamps the new state
into the served markup, so the page is correct on first paint and with scripting
off, not merely once the script runs.

### Why a figure on this site cannot quietly go stale

Every number both pages display lives in `site/figures.json` with a Trutina grade
and a basis (R9). A figure carrying a `derive` key is **recomputed from this
repository on every build**, and the build **fails** if the declared value and the
repository disagree:

    BUILD REFUSED -- the site disagrees with the repository:
      * heldout_vectors: declared '426' but heldout_vectors recomputes to '425'
        -- the repository moved and site/figures.json did not

Re-seal a benchmark, change the CI gate's expected pass count, or extend the
corpus, and the build goes red until the site is updated to match. Figures that
*cannot* be derived — the held-out triple, the demo timings, the third-party
construct coverage — carry the run that produced them in their `basis` and are
not pretended to be derived, because that pretence is the same dishonesty the
gate exists to catch.

Three further checks run on every build: a figure declared but displayed nowhere
fails it, a placeholder naming an undeclared figure fails it, and every word of
this document must survive into the rendered page — a renderer that silently
dropped a paragraph would otherwise ship one.

### As the build advances

A newly completed stop needs three edits: `position` in `site/figures.json`, the
stop's panel copy in `site/atlas.template.html`, and any figure the stop
introduces. A stop does not move from *not built* to *measured* until a command
has been run and its output recorded — the same rule as everywhere else.

---

Relian™ — Legacy Refactoring Substrate. © 2025–2026 Zuup, LLC / Visionblox LLC.
Proprietary. Held-out benchmark vectors are neither reproduced nor referenced in this
document (R3). No private key material appears here (R4).
