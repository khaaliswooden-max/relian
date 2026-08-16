# Draft oracle candidates — NOT SEALED

Programs proposed for RELIAN-BENCH coverage, written under WP-1.5.4 /
WP-1.5.5. **Nothing in this directory is part of the benchmark.** A draft
becomes a corpus program only after Khaalis seals and signs a new benchmark
version (ZCS-6 Phase 4); the sealed bench commit must predate any grammar or
dispatch-table merge that claims support for the constructs it covers (R7).
The vector generator and seed live only in `relian-bench-private` and are
never part of this repository.

Both drafts were compiled and executed with GnuCOBOL 3.1.2 (`cobc -x`, the
same toolchain the CI gate uses) on 2026-08-16; the sample runs below are
actual program output, not predictions.

## VALINIT01 (WP-1.5.4) — the VALUE clause

VALUE on numeric, alphanumeric and COMP-3 fields, group-level VALUE spread
over subordinate fields, 88-levels with single and multiple values. Every
output line depends on an initial value, so a migration that
zero-initializes fields fails behaviorally on the first vector.

```
$ echo "100.00,A" | ./valinit01
STATE=ACTIVE BAL=1334.56
RATE=0.0725 NAME=ACME LTD G1=AB G2=12 G3=CD
$ echo "-234.56,Z" | ./valinit01
STATE=OTHER BAL=1000.00
RATE=0.0725 NAME=ACME LTD G1=AB G2=12 G3=CD
```

## EXITFLW01 (WP-1.5.5) — EXIT / CONTINUE / GOBACK

Paragraph `EXIT`, `CONTINUE` bare / inside `IF` / inside `EVALUATE`,
`GOBACK` with and without an explicit `MOVE 0 TO RETURN-CODE`.

**Sealing note:** no vector may set a NONZERO `RETURN-CODE` — the harness
treats a nonzero exit as a failed run (`runner._run_java` returns `None`),
so exercising nonzero exit codes needs a harness decision first. Flagged
here rather than smuggled into a draft.

```
$ echo "7,G" | ./exitflw01
TICK=0007 SUM=28 MODE=G
$ echo "3,P" | ./exitflw01
TICK=0003 SUM=6 MODE=P
```
