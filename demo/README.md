# Relian demo — end-to-end, measured, offline

```bash
apt-get install -y gnucobol          # the legacy oracle (see "Without GnuCOBOL")
pip install antlr4-python3-runtime

python3 -m demo                      # everything, ~15s
python3 -m demo --inputs 3           # faster
python3 -m demo --case P01 --case CUSTUPD
python3 -m demo --html out/relian.html --json out/run.json
```

## What it does

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

## What it proves

Measured on the committed corpus in a single run (`python3 -m demo`):

- **89 inputs executed on both sides**, across 7 programs, with 100% behavioral
  equivalence on stdout and exit code.
- The transpile is **reproducible** — a second pass produces identical bytes.
- The locally-built GnuCOBOL binaries **reproduce the sealed public vectors**,
  which is what makes the live oracle trustworthy as an oracle.
- Anti-gaming controls (imported from the sealed bench harness, not restated)
  are clean: the generated Java does not shell out to COBOL or embed answers.

Re-run it and the numbers regenerate from scratch. They are not stored.

## What it does not prove

- **This is not the benchmark.** Held-out vectors are scored only in CI and are
  never touched here. The demo uses PUBLIC vector *inputs* only, and takes the
  correct answer from executing the COBOL, not from the vector file.
- **It is not a claim about arbitrary COBOL.** It covers the COBOL-85 subset
  these programs exercise. No CICS, VSAM, JCL, copybooks, or embedded SQL.
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

**`LEDGRPST.cbl` — an undiagnosed failure, shown on purpose.** Its only
out-of-subset construct is `PERFORM` of a named paragraph. Two things go wrong:

1. The transpiler **crashes** (`AttributeError`) instead of refusing. `PERFORM`
   has a handler that assumes the inline `UNTIL`/`VARYING` forms, so a
   paragraph-name operand makes its regex return `None`. The outcome is still
   safe — no Java is emitted — but a crash is not a diagnosis, so the demo
   reports `TRANSPILE_CRASHED`, not `REFUSED_UNSUPPORTED`.
2. The **assessment does not catch it in advance**. It classifies statements by
   bare verb, and `PERFORM` is in the dispatch table, so it reports `1.0000`
   transpilable and is wrong. Coverage over-reports wherever only *some* forms
   of a verb are supported.

Both are real, open gaps in this repository. They are in the demo because
hiding them would make the demo a worse tool than the thing it demonstrates.

## Without GnuCOBOL

The demo still assesses, transpiles and builds — and reports equivalence as
`NOT MEASURED` rather than assuming it. It will never substitute a stored
expectation for an execution that did not happen.

## Reading the numbers

Every figure carries a Trutina grade and a provenance string. `VERIFIED` means
a process ran and was observed. `PLAUSIBLE` means a documented method produced
it under stated assumptions — the assessment's coverage ratio is `PLAUSIBLE`
because the bundled ANTLR grammar cannot cleanly parse real COBOL, so the token
scan runs instead. An unmeasured quantity prints as *not measured*; it is never
zero, an average, or a default.

## Exit status

`0` when nothing that was supposed to be equivalent diverged. Refusals and the
deliberately-included crash case do not affect it — they are what the demo came
to show.
