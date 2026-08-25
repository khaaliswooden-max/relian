# Relian — code tour

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
| `tests/` | 14,009 | 35 test modules (39 `.py` files with helpers) |
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

### 3. `src/assessment/` — 3,555 lines

The read-only side: what is in this code, how much of it the transpiler can
take, and what that costs. Reached through `src/assessment/cli.py` and
`GET /api/v1/assess/demo`. In pipeline order:

| File | Lines | What |
|---|---|---|
| `intake.py` | 160 | File discovery, encoding, copybook association |
| `supported.py` | 204 | What the transpiler can actually do — **probed by running programs through it**, not asserted from a list |
| `coverage.py` | 1,062 | Dual-method coverage (ANTLR tree / token scan) plus the analyzer↔transpiler cross-check |
| `loc.py` | 224 | LOC inventory with reachability |
| `complexity.py` | 271 | Cyclomatic complexity; nesting via a scope stack |
| `risk.py` | 197 | Deterministic risk tiering |
| `report.py` | 884 | Markdown / DOCX / JSON, opening with section 0 in plain language |
| `models.py` | 397 | Shared dataclasses |

Two files carry the idea. `coverage.py` grades the *same* coverage ratio
VERIFIED when the ANTLR parse succeeded and PLAUSIBLE when it fell back to a
token scan — the figure is still computed, with a weaker basis, and says which
one it had. `supported.py` reads `SUPPORTED_STATEMENTS` straight out of the
transpiler and probes DATA DIVISION features by running real programs through
it, resolving to `supported` / `accepted_ignored` / `unsupported` so that "it
parses" is never reported as "it is supported".

### 4. `src/core/orchestrator.py` — 579 lines

The seven-stage state machine. Note that stages 2 and 4 are *removed, not
disabled*. That distinction is the house style in miniature: a stage that is
switched off is a stage someone will switch back on.

### 5. `src/validation/differential.py` — 143 lines

The smallest file that matters most. Builds and executes both sides — original
COBOL under GnuCOBOL, migrated Java — and compares. Equivalence requires
identical stdout *and* identical exit code. If GnuCOBOL is absent it reports
`NOT MEASURED`; it does not infer, and it does not pass.

### 6. `tools/verify_report.py` and `tools/countersign.py`

Read these last, and read them knowing one deliberate choice: they
**reimplement** the hashing rather than importing from
`src/discovery/signing.py`. A verifier that shares code with the signer proves
only that the code agrees with itself. These let a recipient check a report
without trusting our source tree.

`tools/seal.py` seals a benchmark. `tools/verify_manifest.py` proves the tree,
not just the seal.

## What is **not** built

Stated here so nobody discovers it by reading a stub at 11pm.

| Path / capability | State |
|---|---|
| `src/generators/tests.py` (73 lines) | **Unwired stub.** Methods return `[]`. `tests_generated` is 0 and `test_coverage` is `None` by construction. The LLM leg was removed under R6; the symbolic (KLEE) leg was never integrated |
| LLM semantic analysis | **Deleted**, not gated (WP-2.0.-2) — it sent customer source to a hosted model. `semantic_score` is now set by one thing only: differential execution against the legacy oracle |
| ML risk scoring (`src/ml`, `src/intelligence`) | **Deleted** (WP-2.0.-3) — 12 of its 18 features were hardcoded placeholders presented as measurements. `risk_score` is `None` in the pipeline. The *product* risk tier is `src/assessment/risk.py`, a published deterministic rule graded PLAUSIBLE. See `docs/R1_ML_DISPOSITION_2026-08.md` |
| Attestation (`src/blockchain/`, 222 lines) | **Simulated locally**; self-identifies as `simulated: true` |
| `src/plugins/` (769 lines) | Working **scaffolds** — OpenRewrite, Piranha, rope, jscodeshift adapters |
| `src/storage/` | Empty (`__init__.py`, 0 lines) |
| Orchestrator stages 2 and 4 | Removed under R6. Five of seven stages execute |

Scope honesty: "migrates COBOL" holds today only for the COBOL-85 subset the
committed corpus exercises — COMP-3 arithmetic, `EVALUATE`, `PERFORM VARYING`,
`OCCURS`/`SEARCH`, `INSPECT`, edited pictures. No CICS, VSAM, embedded SQL. The
observed subset is enumerated in `docs/C1_SUPPORTED_VERBS_OBSERVED.md`.

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

## One quirk of this repository's history

There are **two disjoint git roots** — `f5a649b` (2026-07-15 baseline) and
`cd6a43c` (2026-08-16 reseed). Default history simplification prunes the older
lineage at the join, so `git log -- <path>` can silently miss a file's origin
commit. Use `--full-history`, or `--follow` for a single file:

```bash
git log --full-history --no-merges --format='%h %ad %s' --date=short -- src/assessment
git log --follow -- transpiler/c1_rulebased.py   # its origin is 4bfa3a0, not cd6a43c
```

Commit messages here carry the measured result that justified the change;
`git log --format='%s%n%b' -1 <sha>` is often faster than the diff.

## Where to go deeper

| Document | What |
|---|---|
| `docs/PHASE2_LOG.md` | The build log — decisions and their reasoning, work package by work package |
| `docs/TECHNICAL_DELIVERY_SHEET.md` | The delivery-facing view |
| `docs/architecture/` | The Build Atlas — architecture, production cycle, provenance ledger. Published at https://khaaliswooden-max.github.io/relian/ |
| `docs/R6_AUDIT_2026-08.md` | The generative-AI perimeter audit |
| `docs/R1_ML_DISPOSITION_2026-08.md` | Why the ML risk scorer was deleted rather than fixed |
| `demo/README.md` | Demo quickstart and the missing-dependency table |

---

## Provenance of the numbers in this file

Per R9, every externally visible figure carries a grade and a basis. Figures
dated at a commit go stale as the tree moves — re-derive rather than trust the
table; the commands are given so that you can.

| Figure | Grade | Basis |
|---|---|---|
| Every line count | VERIFIED | Measured at commit `32a60fd` by `git ls-files '<path>' \| xargs wc -l`, and by the `find`/`wc` command in **The shape of it** for the 20,469 total |
| 35 test modules, 14,009 test lines | VERIFIED | `git ls-files 'tests/**/test_*.py' 'tests/test_*.py' \| wc -l` and `git ls-files 'tests/*.py' \| xargs wc -l` at `32a60fd`. The count is 26 if you look only at `tests/*.py` at the top level and miss the subdirectories |
| 65,666 generated parser lines | VERIFIED | Same command over `src/parsers/antlr/*.py` at `32a60fd` |
| 1,151 collected / 1,121 passed / 30 skipped / 1 failed | VERIFIED | Local run on 2026-08-25 in a container **without GnuCOBOL** — the condition is what produces the single failure, and it is stated with the figure above |
| 1,149 passed, 10 skipped, 0 failed | VERIFIED | CI, with the full toolchain present: recorded in merge commit `2ba0274` and asserted mechanically as `EXPECTED_PASSES` / `EXPECTED_SKIPS` in `.github/workflows/tests.yml`, so a test that stops being collected fails the build rather than disappearing |
| C1 BER 1.0000 (300/300), build 1.0000, branch coverage 0.8824 | VERIFIED | Held-out run recorded in commit `4bfa3a0`; thresholds sealed in `bench/LEDGER_relian-bench-v1.2.json` (Ed25519, fingerprint `233bb4406e2de606`) |
| Coverage ratio, VERIFIED vs PLAUSIBLE | — | Set by `src/assessment/coverage.py` per run, from whether the ANTLR parse succeeded or it fell back to a token scan |
| Risk tier | PLAUSIBLE | A published deterministic policy is not a measurement. Its inputs are VERIFIED |
