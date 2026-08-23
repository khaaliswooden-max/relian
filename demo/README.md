# Relian demo — end-to-end, measured, offline

```bash
apt-get install -y gnucobol          # transform: the legacy oracle
pip install antlr4-python3-runtime   # transform: the COBOL parser
pip install cryptography             # discovery: Ed25519 (see "Prerequisites")

python3 -m demo                      # both tracks
python3 -m demo --inputs 3           # faster
python3 -m demo --case P01 --case CUSTUPD
python3 -m demo --discovery-only     # the estate, without the migration
python3 -m demo --skip-discovery     # the migration, without the estate
python3 -m demo --html out/relian.html --json out/run.json
```

Two tracks. **Transform** answers *does the migration behave like the
original*. **Discovery** answers the question a migration is actually scoped
on — *what is in this estate, and what does the data look like* — which
behavioural equivalence cannot answer at all.

## Prerequisites, and what happens without each

Every one of these is optional in the sense that the demo still runs without
it. None of them is optional in the sense that the demo will guess at what it
could not measure.

| Missing | Effect |
|---|---|
| `gnucobol` | The legacy side cannot be executed. Equivalence reports `NOT MEASURED`; assess, transpile and build still run. |
| `antlr4-python3-runtime` | The demo will not start — the assessment engine imports the parser at module load. |
| `cryptography` | Ed25519 is unavailable, so the sealed-oracle cross-check and the signed report both report `NOT MEASURED`. The seal is **not** reported as bad: it could not be checked at all, which is a different finding. |
| a PostgreSQL DSN | The generated schema is not executed; see [Executing the generated DDL](#executing-the-generated-ddl). |

`cryptography` is deliberately **not** in `[project.dependencies]` —
`pyproject.toml` keeps it in the `dev` extra, because verifying a benchmark
seal is a CI and third-party-audit activity rather than part of the transform
path a customer perimeter installs. The discovery track needs it anyway, so on
a runtime-only install it is legitimately absent and the demo says so instead
of crashing. `pip install -e ".[dev]"` also supplies it.

## Track 1 — transform

For each COBOL program, in one run:

| Stage | What actually happens |
|---|---|
| **Assess** | The shipped read-only assessment engine (`src/assessment/`) inventories the source: statements, LOC, cyclomatic complexity, risk tier, and the fraction of statements the transpiler's dispatch table can handle. |
| **Transpile** | The shipped transpiler (`transpiler/c1_rulebased.py`) either emits Java or refuses. Its answer is compared against the assessment's prediction — two independent implementations cross-checking each other. |
| **Determinism** | The program is transpiled a second time and the sha256 compared. A migration you cannot reproduce is not evidence. |
| **Build both** | `javac` on the generated Java, `cobc` on the original COBOL. |
| **Execute both** | Every input is fed to *both* binaries. Equivalence requires byte-identical stdout **and** the same process exit code. |
| **Gate** | Decide whether the migration would be attestable. |

Nothing is stubbed. The demo imports the real engine; if the transpiler
regresses, the demo goes red.

### What track 1 proves

Measured on the committed corpus in a single run (`python3 -m demo
--skip-discovery`):

- **89 inputs executed on both sides**, across 7 programs, with 100% behavioral
  equivalence on stdout and exit code.
- The transpile is **reproducible** — a second pass produces identical bytes.
- The locally-built GnuCOBOL binaries **reproduce the sealed public vectors**,
  which is what makes the live oracle trustworthy as an oracle.
- Anti-gaming controls (imported from the sealed bench harness, not restated)
  are clean: the generated Java does not shell out to COBOL or embed answers.

Re-run it and the numbers regenerate from scratch. They are not stored.

## Track 2 — discovery

Seven stages over `examples/demo` — a synthetic municipal-utility batch suite
with copybooks, a job stream and a program that actually declares files. The
transform corpus cannot exercise this: it is flat programs with no `COPY`, no
`SELECT` and no JCL.

| Stage | What actually happens |
|---|---|
| **Resolve** | `src/discovery/copybook.py` walks the `COPY` fan-in and builds the missing-copybook table. |
| **Layout** | `src/discovery/layout.py` computes byte offsets and lengths **from source text alone** — no compiler is invoked, and `tests/test_discovery_is_compiler_free.py` walks the package's AST to assert it. |
| **Oracle** | Those layouts are compared against RELIAN-DISCOVERY-BENCH v0.1's *sealed* answer key, which is GnuCOBOL 3.1.2.0's own byte layout. All three seal layers and the signer are verified **before a single row is read**. |
| **Inventory** | `SELECT` ∪ `FD` ∪ the job stream's `DD`, joined on DD name, with the computed record length cross-checked against the declared `LRECL`. |
| **Lineage** | Program→dataset edges from `OPEN` modes, printed next to the bound on their own coverage. |
| **DDL** | A PostgreSQL target schema against the published mapping table — executed against a real server when one is reachable, `NOT MEASURED` when not. |
| **Report** | The canonical `report.json`, its manifest and an Ed25519 instance signature — then the *recipient's* verifier is run against the result. |

Measured on the committed tree in a single run (`python3 -m demo
--discovery-only`):

- **3 copybooks resolved, 0 missing**, 4 `COPY` edges over 8 files.
- **22 / 22 offset-and-length comparisons** against the sealed oracle,
  tolerance zero, seal verified across all three layers under signer
  `233bb4406e2de606`.
- **6 files inventoried** across 1 of 5 programs with a `FILE-CONTROL`
  paragraph, 12 `DD` statements in 1 job stream: 1 record length agrees with
  its declared `LRECL`, 0 disagree, 3 datasets declare no `LRECL` and 2 have no
  resolvable layout.
- **4 program→dataset edges** across 4 datasets, with the coverage statement
  that refuses to call the graph complete.
- **3 target tables, 16 columns, lint clean.**
- A **signed report** that the shipped recipient-side verifier reads as
  `VALID AND UNATTESTED` — three of four layers `PASS`, the countersignature
  `ABSENT`, which is the correct answer and not a caveat.

Two things the discovery track proves that are easy to skip past:

**The oracle cross-check is not the tool grading itself.** The layout engine
reads source text and never invokes a compiler; the oracle is nothing but a
compiler's own byte layout, sealed and signed before the engine that is graded
against it existed. The two artifacts agree *because* they share no code and no
process. A copybook the oracle does not cover is reported as not covered — it
never lands in the numerator.

**Findings are not failures.** `NO_LRECL`, `NO_LAYOUT` and even `DISAGREE` are
discoveries about the estate, and a demo that went red on them would be red over
any realistic tree. What *does* reach the exit status is a layout that
contradicts the sealed oracle, a seal that will not verify, DDL that fails its
own lint, or a report the shipped verifier rejects.

### Executing the generated DDL

Generating a schema is not evidence that it loads, so the demo will not say it
loads unless it watched that happen:

```bash
RELIAN_DDL_DSN=postgresql://relian:relian@localhost:5432/relian_ddl \
  python3 -m demo --discovery-only
```

With a reachable server the stage executes the schema and reconciles every
column against `information_schema`. Without one — or with one that refuses the
connection — it reports `NOT MEASURED` and names the reason. It never reports
zero errors for a run that did not happen.

### What discovery does not touch

No dataset is opened. The whole track runs on source text, and
`tests/test_discovery_reads_no_data.py` runs the pipeline under an audit hook
with decoy datasets present to prove it (R12). The instance signing key is
created inside the demo's own workdir, never in `~/.relian`.

## What it does not prove

- **This is not the benchmark.** Held-out vectors are scored only in CI and are
  never touched here. The demo uses PUBLIC vector *inputs* only, and takes the
  correct answer from executing the COBOL, not from the vector file. The
  discovery track reads the sealed oracle to *check itself* against it; it
  scores nothing and writes nothing back.
- **It is not a claim about arbitrary COBOL.** The transform track covers the
  COBOL-85 subset these programs exercise: no CICS, VSAM, JCL, copybooks or
  embedded SQL. The discovery track *does* read copybooks and JCL — statically,
  which is what it is for — and its layout claim is scoped to GnuCOBOL 3.1.2.0
  on the fifteen-copybook sealed corpus. IBM Enterprise COBOL equivalence is
  unmeasured.
- **The discovery tree is synthetic.** `examples/demo` is hand-written
  demonstration code, not a customer estate and not derived from one. Nothing
  in it is a claim about how a real estate would score.
- **Nothing is signed.** The attestation gate reports a decision. Signing keys
  are operator-custody only (R4) and this demo ships no simulated attestation,
  badge, or transaction hash (R5).

## The two failure cases, and why they are here

A demo that only shows successes tells you nothing about what the tool does
when it is out of its depth — which, for a migration tool, is the only question
that matters.

**`CUSTUPD.cbl` — designed refusal.** Valid COBOL-85 that GnuCOBOL compiles and
runs, using `SUBTRACT`, `MULTIPLY`, `DIVIDE`, `STRING`, `GO TO` and paragraph
`EXIT`. The transpiler stops at the first one, names the verb, line and
paragraph, emits no Java and issues no attestation. The assessment predicted
the refusal before the transpiler ran.

**`LEDGRPST.cbl` — 93% transpilable, refused anyway.** Its only out-of-subset
construct is `PERFORM` of a named paragraph, the commonest control-flow idiom in
production COBOL. 13 of its 14 statements are transpilable — and Relian refuses
the whole program, naming the verb and the line. There is no partial migration
and no "we did the 93% we could", because Java that silently dropped one
`PERFORM` would compile, run, and post the wrong balance.

This case previously documented two defects, both fixed in PR #16:

1. The transpiler **crashed** here (`AttributeError`) instead of diagnosing.
   `PERFORM` was registered as a bare verb while only the inline `VARYING` form
   had a handler, so a paragraph-name operand made its regex return `None`.
2. The **assessment reported `1.0000` transpilable and was wrong**, because it
   classifies by bare verb and read the same over-broad registration.

Registering the qualified key `PERFORM VARYING` fixed both at once — dispatch
now refuses the unsupported forms by name, and the analyzer picked up the
narrower claim with no hand-edit. The demo still distinguishes a crash from a
refusal; no shipped program triggers `TRANSPILE_CRASHED` now, and a unit test
exercises that path directly.

The correction was not cosmetic. Re-measured on the same input trees, real-world
coverage fell from 0.8511 to 0.7248 (AWS CardDemo), 0.6945 to 0.5287 (OMP
course) and 0.5968 to 0.5444 (GnuCOBOL) — 1,380 statements that had been
counted as migratable and were not. See [`docs/dryruns/README.md`](../docs/dryruns/README.md).

## Without GnuCOBOL

The transform track still assesses, transpiles and builds — and reports
equivalence as `NOT MEASURED` rather than assuming it. It will never substitute
a stored expectation for an execution that did not happen.

**The discovery track needs neither GnuCOBOL nor a JDK.** It is compiler-free by
construction, so `python3 -m demo --discovery-only` runs in full on a machine
with nothing but Python — which is also the point: the product has to run inside
a customer perimeter, and an engine that needs a compiler is not shippable
there.

## Reading the numbers

**Equivalence is a rate over inputs that actually ran.** Each input lands in one
of three states, not two: *equivalent*, *divergent*, or *not run* (a timeout, or
a process that would not launch). An input that could not be executed is
excluded from the denominator and reported separately — it is not a divergence,
because nothing was observed to diverge. If any input goes unrun the program's
verdict is `INCOMPLETE_MEASUREMENT` and the attestation gate blocks, whatever
the executed subset scored: a partial comparison is not an equivalence claim.

Every figure carries a Trutina grade and a provenance string. `VERIFIED` means
a process ran and was observed. `PLAUSIBLE` means a documented method produced
it under stated assumptions — the assessment's coverage ratio is `PLAUSIBLE`
because the bundled ANTLR grammar cannot cleanly parse real COBOL, so the token
scan runs instead. An unmeasured quantity prints as *not measured*; it is never
zero, an average, or a default.

## Exit status

`0` when nothing that was supposed to be equivalent diverged and nothing in the
discovery track contradicted a sealed artifact. Refusals and the
deliberately-included crash case do not affect it — they are what the demo came
to show. Neither do discovery *findings*: a dataset with no declared `LRECL` is
the tool working.

Non-zero means one of these, all of them defects rather than discoveries:

| Track | Condition |
|---|---|
| transform | a divergence, a build failure, an anti-gaming trip, or an input that failed to run on a machine that had both toolchains |
| discovery | a layout contradicting the sealed oracle, a seal that will not verify, a copybook whose bytes drifted from the sealed corpus, DDL failing its own lint or its reconciliation, or a report the shipped verifier rejects |
