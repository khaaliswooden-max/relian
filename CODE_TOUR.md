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
