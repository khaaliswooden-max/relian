# Relian — code tour

**Who this is for.** An engineer with repository access who has been told
"look at what's built" and wants the source, not the story. It names files,
says what each one does, and marks what is a stub so nobody spends an
afternoon reading a scaffold and mistaking it for a system.

The architecture narrative lives elsewhere — `README.md`, the Build Atlas at
<https://khaaliswooden-max.github.io/relian/>, and `docs/architecture/`.
This file is the map of the tree.

Every figure below carries its basis in [§8](#8-provenance-of-the-numbers-in-this-file).
Line counts are a size signal, nothing more: they say where the mass is, not
whether the code is any good.

---

## 1. Get it running first

```bash
git clone https://github.com/khaaliswooden-max/relian.git
cd relian
apt-get install -y gnucobol          # the legacy oracle, plus a JDK
pip install antlr4-python3-runtime   # VERIFIED coverage grades
pip install cryptography             # Ed25519, for the discovery track

python3 -m demo --inputs 3           # end-to-end, offline, measured
python3 -m demo --html out/relian.html
```

`python3 -m demo` runs the shipped assessment engine and the shipped
transpiler over real COBOL, then builds **both** the original (GnuCOBOL) and
the migration (javac) and executes them against each other input by input.
Every figure it prints came from a process that ran during that invocation.

It also runs two programs it is expected to fail on, because what a migration
tool does outside its depth is the only question that matters. Both get a
diagnosed refusal naming the verb and the line, emit no Java, and issue no
attestation — including one that is 93% transpilable, because 93% is not 100%.

What degrades without each prerequisite — and what is reported as
`NOT MEASURED` rather than guessed — is in [`demo/README.md`](demo/README.md).

---

## 2. The four things that are actually built

### 2.1 Assessment engine — `src/assessment/` (3,555 lines)

Read-only analysis over customer COBOL. The entry point is
`src/assessment/cli.py`; `GET /api/v1/assess/demo` runs the same engine over
the bundled `examples/demo` corpus and serves the result to the Assess tab.
Read it in pipeline order:

| File | Lines | What it does |
|---|---:|---|
| `intake.py` | 160 | File discovery, encoding, copybook association |
| `supported.py` | 204 | What the transpiler can actually do — **probed by running programs through it**, not asserted from a list |
| `coverage.py` | 1,062 | Dual-method coverage (ANTLR tree / token scan) + analyzer↔transpiler cross-check |
| `report.py` | 884 | Markdown / DOCX / JSON rendering, including section 0 |
| `models.py` | 397 | Shared dataclasses |
| `complexity.py` | 271 | Cyclomatic complexity, nesting depth via a scope stack |
| `loc.py` | 224 | LOC inventory with reachability |
| `risk.py` | 197 | Deterministic risk tiering |
| `cli.py` | 149 | CLI entry point |

**The file to read if you read one:** `coverage.py`. It is where
*measured-or-None* (rule R1) is enforced rather than described. The same
coverage ratio is graded **VERIFIED** when the ANTLR parse succeeded and
**PLAUSIBLE** when it fell back to a token scan — the figure is still
computed, with a weaker basis, and says which one it had.

**The seam to understand:** `supported.py` reads `SUPPORTED_STATEMENTS`
straight out of the transpiler and probes DATA DIVISION features by running
real programs through it. Three states — `supported`, `accepted_ignored`,
`unsupported` — so "it parses" is never reported as "it is supported".

### 2.2 Transpiler — `transpiler/c1_rulebased.py` (966 lines)

COBOL-85 → Java. Deterministic, rule-based, one file, no model in the path.
BigDecimal for all numeric business logic with explicit COBOL rounding
(`ROUNDED` = HALF_UP, unrounded `COMPUTE` = DOWN).

Strict mode is **on by default**: an unsupported verb raises with verb, line
and paragraph. It emits no placeholder Java, ever — an unsupported construct
produces a FAILED migration with a construct inventory.

Measured against RELIAN-BENCH: BER 1.0000 (300/300 held-out), build 1.0000,
branch coverage 0.8824 — the committed bar, cleared. See §8.

### 2.3 Discovery — `src/discovery/` (7,643 lines)

The largest and newest component. Behavioural equivalence answers *does the
migration behave like the original*; discovery answers the question a
migration is actually scoped on — *what is in this estate, and what does the
data look like*.

| File | Lines | What it does |
|---|---:|---|
| `layout.py` | 1,455 | Static record-layout engine |
| `files.py` | 1,170 | File inventory |
| `ddl.py` | 989 | Target-schema DDL generation |
| `jcl.py` | 910 | JCL analysis |
| `report.py` | 871 | The signed Data Discovery report |
| `copybook.py` | 813 | Copybook resolver |
| `signing.py` | 433 | Ed25519 signing |
| `cli.py` | 392 | CLI entry point |
| `lineage.py` | 385 | Field lineage |

### 2.4 Surfaces

| Path | Lines | What it is |
|---|---:|---|
| `demo/` | 2,936 | The end-to-end runnable demo — `discovery.py` (1,004), `pipeline.py` (579), `report.py` (523), `__main__.py` (471) |
| `src/ui/` | 2,068 | React + TypeScript — `AssessView`, `MigrateView`, `AnalyzeView`, `BenchmarkView`, `CapabilitiesView` |
| `tools/` | 2,568 | `verify_manifest.py` (810), `seal.py` (564), `verify_report.py` (495), `countersign.py` (380), `ddl_load_check.py` (319) — the recipient-side verification tools |
| `src/api/main.py` | 564 | FastAPI |
| `src/core/orchestrator.py` | 579 | The 7-stage pipeline state machine; 5 stages execute (see §4) |
| `src/parsers/cobol.py` | 374 | Hand-written wrapper over the generated ANTLR parser |

---

## 3. What to skip

- **`src/parsers/antlr/`** — 65,666 lines of ANTLR-generated Python from the
  ProLeap COBOL-85 grammar. Machine-generated; provenance and licence in
  `docs/GRAMMAR_PROVENANCE.md` and `docs/licenses/`.
- **`bench/` and `discovery-bench/`** — sealed benchmark corpora. Read them,
  never edit them; a change requires a new signed version by the operator.
- **`programs/`, `Anchor.toml`, `Cargo.toml`** — Solana/Anchor scaffolding for
  the attestation path, which is simulated (§4).

---

## 4. What is **not** built

Stated here so nobody discovers it by reading a stub at 11pm.

| Path / capability | State |
|---|---|
| `src/generators/tests.py` (73 lines) | **Unwired stub.** Methods return `[]`. `tests_generated` is 0 and `test_coverage` is `None` by construction. The LLM leg was removed under R6; the symbolic (KLEE) leg was never integrated |
| LLM semantic analysis | **Deleted**, not gated (WP-2.0.-2, R6) — it sent customer source to a hosted model. `semantic_score` is now set by one thing only: differential execution against the legacy oracle |
| ML risk scoring (`src/ml`, `src/intelligence`) | **Deleted** (WP-2.0.-3, R1) — 12 of its 18 features were hardcoded placeholders presented as measurements. Not left untrained: removed. `risk_score` is `None` in the pipeline. The *product* risk tier is `src/assessment/risk.py`, a published deterministic rule graded PLAUSIBLE |
| Attestation (`src/blockchain/`, 222 lines) | **Simulated locally**; self-identifies as `simulated: true` |
| `src/plugins/` (769 lines) | Working **scaffolds** — OpenRewrite, Piranha, rope, jscodeshift adapters |
| `src/storage/` | Empty (`__init__.py`, 0 lines) |
| `src/validation/differential.py` (143 lines) | Real differential validation, wired to the bench harness |
| Orchestrator stages 2 and 4 | Removed under R6. 5 of 7 stages execute |

Scope honesty: "migrates COBOL" holds today only for the COBOL-85 subset the
committed corpus exercises — COMP-3 arithmetic, `EVALUATE`, `PERFORM VARYING`,
`OCCURS`/`SEARCH`, `INSPECT`, edited pictures. No CICS, VSAM, embedded SQL.
The observed subset is enumerated in `docs/C1_SUPPORTED_VERBS_OBSERVED.md`.

---

## 5. Tests

35 test modules, 14,009 lines, under `tests/` mirroring the source tree.

The suite count is a **merge gate**, not a statistic:
`.github/workflows/tests.yml` asserts `EXPECTED_PASSES` mechanically, so a
test that silently stops being collected fails CI rather than disappearing.

```bash
python3 -m pytest tests/ -q
python3 bench/harness/runner.py <candidate> --split public   # public vectors only
```

Held-out vectors are **not in this repository** and are never to be
reconstructed, copied, or referenced (R3). They run in CI only.

---

## 6. Where the evidence lives

| Path | What it holds |
|---|---|
| `docs/dryruns/` | Assessment runs over five real COBOL codebases — AWS CardDemo, GnuCOBOL, the OMP COBOL course, the bench corpus, `examples/cobol` — each with its `.sha256`. Also two signed Data Discovery report runs (`report_carddemo`, `report_demo`) and the CardDemo copybook/JCL runs |
| `docs/TECHNICAL_DELIVERY_SHEET.md` | Signing-key fingerprints, which key signs which claim class, and each verification tool's own SHA-256 |
| `docs/PHASE1_LOG.md`, `docs/PHASE2_LOG.md` | Decisions and defects, work package by work package — the reasoning behind the diffs |
| `examples/demo/README.md` | The Meridian MUD demo corpus: three tiers — clean, partial, principled refusal |
| `bench/LEDGER_relian-bench-v1.2.json` | The sealed benchmark: thresholds (BER ≥ 0.95, build 1.0, branch coverage ≥ 0.80), vector counts, Ed25519 signature |

Commit messages in this repository carry the measured result that justified
the change. `git log --format='%s%n%b' -1 <sha>` is often faster than the diff.

---

## 7. Before you commit

Read [`CLAUDE.md`](CLAUDE.md). Rules R1–R12 are binding on anything that
lands here. The three that bite first:

- **R1** — a metric is measured or `None`. Never a constant, default or
  estimate presented as a measurement.
- **R2** — honest failure is a feature. Unsupported COBOL produces an explicit
  no-attestation result with a construct inventory. Never silently pass, skip
  or approximate.
- **R3** — never read, regenerate, copy or reference `relian-bench-private`.
  Generator + seed + corpus regenerates the held-out set, which destroys the
  benchmark. If a task seems to need those vectors, stop and escalate.

One repository quirk that will cost you an hour otherwise: **there are two
disjoint git roots** — `f5a649b` (2026-07-15 baseline) and `cd6a43c`
(2026-08-16 reseed). Default history simplification prunes the older lineage
at the join, so `git log -- <path>` can silently miss a file's origin commit.
Use `--full-history`, or `--follow` for a single file.

```bash
git log --full-history --no-merges --format='%h %ad %s' --date=short -- src/assessment
```

---

## 8. Provenance of the numbers in this file

Per R9, every externally visible figure carries a grade and a basis.

| Figure | Grade | Basis |
|---|---|---|
| All line counts | VERIFIED | Measured at commit `e06a952` by `git ls-files '<path>' \| xargs wc -l`. Re-derivable on any checkout of that commit |
| 65,666 generated parser lines | VERIFIED | Same command over `src/parsers/antlr/*.py` at `e06a952` |
| 35 test modules, 14,009 test lines | VERIFIED | `git ls-files 'tests/**/test_*.py' 'tests/test_*.py' \| wc -l` and `git ls-files 'tests/*.py' \| xargs wc -l` at `e06a952` |
| C1 BER 1.0000 (300/300), build 1.0000, branch coverage 0.8824 | VERIFIED | Held-out run recorded in commit `4bfa3a0` and in `README.md`; thresholds sealed in `bench/LEDGER_relian-bench-v1.2.json` (Ed25519, fingerprint `233bb4406e2de606`) |
| Suite: 1,149 passed, 10 skipped, 0 failed | VERIFIED | CI run recorded in merge commit `2ba0274` (2026-08-23); asserted mechanically as `EXPECTED_PASSES` in `.github/workflows/tests.yml` |
| Held-out bench 425/425 | VERIFIED | CI run recorded in merge commit `5a93664` (2026-08-23) |
| "93% transpilable and still refused" | VERIFIED | Produced by `python3 -m demo` on the Meridian MUD tier-B program; see `examples/demo/README.md` |
| Risk tier | PLAUSIBLE | A published deterministic policy is not a measurement. Its inputs are VERIFIED |

Figures dated at a commit go stale as the tree moves. Re-derive rather than
trust the table; the commands are given so that you can.
Where the code is, what each part does, and the order to read it in. For an
engineer who has just cloned the repository and has an afternoon.

This is a map, not a specification. Where it states a number, the number was
measured on the tree it describes and the command is given so you can re-run
it. Where a claim belongs to a signed artifact, this file points at the
artifact rather than restating it.

> **Read `README.md` first.** Relian is a **pre-MVP research prototype** — no
> completed migrations, no customers. This tour describes what is built, which
> is not the same as what is shipped.

## The shape of it

| Path | Lines | What |
|---|---|---|
| `src/discovery/` | 7,643 | Data Discovery engine — the estate, and a signed report about it |
| `src/assessment/` | 3,555 | Legacy Code Assessment engine — read-only over customer code |
| `tools/` | 2,568 | Sealing, countersigning, and recipient-side verifiers |
| `transpiler/` | 966 | The COBOL→Java transpiler (C1) |
| `src/core/` | 585 | Seven-stage orchestrator |
| `src/parsers/` | 442 | COBOL-85 parser — **plus 65,666 generated lines, see below** |
| `src/validation/` | 143 | Differential execution against the GnuCOBOL oracle |
| `demo/` | 2,936 | End-to-end runnable demo, both tracks |
| `tests/` | 14,009 | 26 files |
| `src/ui/` | 2,068 | React front end (Phase 1) |
| `src/api/` | 564 | FastAPI service (Phase 1) |

20,469 lines of hand-written product code against 14,000 lines of tests.

```bash
# regenerate the total, excluding the generated ANTLR tree
find src transpiler tools demo -name '*.py' -not -path '*/antlr/*' | xargs wc -l | tail -1
```

### Two things that will mislead you if nobody says them

**`src/parsers/antlr/cobol/` is generated.** 54,007 lines in `Cobol85Parser.py`
alone, from the `.g4` grammars in `src/parsers/grammars/`. Do not read it, do
not count it, do not edit it. The hand-written parser is
`src/parsers/cobol.py` — 374 lines. Anyone who runs `cloc` before reading this
paragraph will conclude the parser is 99% of the project; it is 2%.

**`src/ui/` and `src/api/` do not touch discovery.** They are the Phase-1 web
tier — Analyze, Assess, Benchmark, Capabilities, Migrate. Discovery has never
had a UI or an HTTP endpoint; its only surfaces are its CLI and the demo. If
you are here to evaluate the Discovery work, those two directories are not part
of it.

```bash
# the negative, verifiable: pickaxe finds no commit that ever added the string
git log --all --oneline -S'discovery' -- src/ui src/api
```

## Read it in this order

### 1. `transpiler/c1_rulebased.py` — 966 lines

The whole thesis in one file. `SUPPORTED_STATEMENTS` (line 907) is a dispatch
table from COBOL verb to Java emitter. `Transpiler.stmt` (line 605) looks a
verb up; a miss records an unsupported hit and, under `strict`, raises
`UnsupportedConstruct` — not a fallback, not an approximation, not a silent
skip. Note the two-word keys (`EXIT PROGRAM`): registering the qualified form
keeps paragraph `EXIT` honestly unsupported, in the dispatch table *and* in the
assessment's `supported_verbs()`, which reads these same keys.

Numeric business logic emits `BigDecimal` with explicit COBOL rounding
semantics — `ROUNDED` is HALF_UP on store, an unrounded `COMPUTE` truncates
toward zero (`RoundingMode.DOWN`). That pairing
is the reason the transpiler exists at all; a naive port that uses `double`
gets different money.

Read this first. Everything else in the repository is either feeding it,
checking it, or reporting on it.

### 2. `src/discovery/` — 7,643 lines

The newest and largest subsystem. It answers the question a migration is
actually scoped on — *what is in this estate* — which behavioural equivalence
cannot answer at all. In build order:

| File | Lines | What |
|---|---|---|
| `copybook.py` | 813 | Resolves the `COPY` fan-in graph; publishes the missing-copybook table |
| `layout.py` | 1,455 | Static record layouts: offsets, lengths, `SYNC` padding, `REDEFINES`, `OCCURS DEPENDING ON`, `RENAMES` |
| `files.py` | 1,170 | File inventory from `SELECT`/`FD`/`OPEN` |
| `jcl.py` | 910 | JCL DD statements; record length cross-checked against LRECL |
| `ddl.py` | 989 | PostgreSQL target schema against a published mapping table |
| `lineage.py` | 385 | Program→dataset edges from OPEN modes, with the bound on their coverage published alongside |
| `report.py` | 871 | Canonical `report.json`, Markdown rendering, manifest of digests |
| `signing.py` | 433 | Ed25519 instance signature over the manifest |
| `cli.py` | 392 | `layout`, `resolve`, `report build`, `report request` |

Start with `copybook.py` and `layout.py` — they are the engine. `report.py` and
`signing.py` are where the output becomes something a recipient can check.

The rule that shapes all of it: **if a signing key cannot be obtained,
`report build` writes nothing.** An unsigned artifact that looks signed is
worse than no artifact.

### 3. `src/core/orchestrator.py` — 579 lines

The seven-stage state machine. Note that stages 2 and 4 are *removed, not
disabled*. That distinction is the house style in miniature: a stage that is
switched off is a stage someone will switch back on.

### 4. `src/validation/differential.py` — 143 lines

The smallest file that matters most. Builds and executes both sides — original
COBOL under GnuCOBOL, migrated Java — and compares. Equivalence requires
identical stdout *and* identical exit code. If GnuCOBOL is absent it reports
`NOT MEASURED`; it does not infer, and it does not pass.

### 5. `tools/verify_report.py` and `tools/countersign.py`

Read these last, and read them knowing one deliberate choice: they
**reimplement** the hashing rather than importing from
`src/discovery/signing.py`. A verifier that shares code with the signer proves
only that the code agrees with itself. These let a recipient check a report
without trusting our source tree.

`tools/seal.py` seals a benchmark. `tools/verify_manifest.py` proves the tree,
not just the seal.

## Running it

```bash
git clone https://github.com/khaaliswooden-max/relian
cd relian
pip install -r requirements.lock
pip install -e ".[dev]"          # test runner, linters, cryptography

python3 -m pytest                # the suite
python3 -m demo --inputs 3       # end to end, fast
python3 -m demo --discovery-only # the estate, with no transform claim
```

**Measured on this tree, 2026-08-25:** 1,151 tests collected; **1,121 passed,
30 skipped, 1 failed**. The single failure is
`tests/test_seal.py::test_the_discovery_seal_config_is_buildable_and_records_what_it_should`,
and it fails **because GnuCOBOL is not installed in that container** — the
sealer's toolchain probe cannot answer for `cobc`, so it refuses to sign. That
is the designed behaviour, not a defect: a signed record cannot be edited
afterwards, so the moment before signing is the only moment a missing tool is
still fixable. Install `gnucobol` and it passes.

```bash
apt-get install -y gnucobol      # and a JDK
```

`demo/README.md` has the full table of what each missing dependency costs. The
short version: every prerequisite is optional in the sense that the demo still
runs without it, and none is optional in the sense that the demo will guess at
what it could not measure.

## The rules the code is written under

`CLAUDE.md` at the repository root is binding, not advisory, and reading it
will explain shapes in the code that otherwise look like over-engineering:

- **A metric is measured or `None`.** Never a constant, default, or estimate
  presented as a measurement. `tests/test_no_fabricated_metrics.py` enforces it.
- **Honest failure is a feature.** Unsupported COBOL produces an explicit
  no-attestation result with a construct inventory — never a silent pass.
- **Nothing simulated ships.** Absence of a real integration is stated plainly.
- **No customer source to any generative-AI model.**
  `tests/test_no_generative_ai_in_transform_path.py` fails if an LLM client is
  reintroduced to the dependency list.
- **Bench-first.** No construct is "supported" until the benchmark covers it,
  is sealed, and CI passes against held-out vectors. `bench/` and
  `discovery-bench/` are frozen; changing them requires a new signed version.
- **Every externally visible number carries a Trutina grade** —
  VERIFIED / PLAUSIBLE / SPECULATIVE — and a provenance field.

The commented-out history in `pyproject.toml` is worth ten minutes on its own.
It records, with reasoning, every dependency removed and why — an ML risk
scorer trained on fabricated feature rows, two deep-learning runtimes declared
as hard requirements with zero import sites, several gigabytes carried into
every on-prem install for nothing.

## Where to go deeper

| Document | What |
|---|---|
| `docs/PHASE2_LOG.md` | The build log — decisions and their reasoning, work package by work package |
| `docs/TECHNICAL_DELIVERY_SHEET.md` | The delivery-facing view |
| `docs/architecture/` | The Build Atlas — architecture, production cycle, provenance ledger. Published at https://khaaliswooden-max.github.io/relian/ |
| `docs/R6_AUDIT_2026-08.md` | The generative-AI perimeter audit |
| `docs/R1_ML_DISPOSITION_2026-08.md` | Why the ML risk scorer was deleted rather than fixed |
| `demo/README.md` | Demo quickstart and the missing-dependency table |
