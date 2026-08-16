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

## EXITFLW01 (WP-1.5.5, rev 2) — EXIT PROGRAM / CONTINUE / GOBACK

`CONTINUE` bare / inside `IF` / inside `EVALUATE`, `EXIT PROGRAM` in a main
program (measured: a no-op that falls through), `GOBACK` with and without an
explicit `MOVE 0 TO RETURN-CODE`. All scaffolding is corpus-proven C1 forms
(inline `PERFORM VARYING … END-PERFORM`, `EVALUATE`/`WHEN OTHER`).

**Rev 2 — Bugbot finding on PR #10, confirmed.** Rev 1 drove its loop with
out-of-line `PERFORM <para>` and tested paragraph `EXIT`. C1's PERFORM
handler supports only the inline VARYING form (out-of-line PERFORM crashes
it), so rev 1 could never gate the three handlers alone. **Paragraph EXIT
is inseparable from performed-paragraph support and is NOT
dispatch-table-only work — moved to the deferred list.**

**Sealing notes:**
1. No vector may set a NONZERO `RETURN-CODE` — the harness treats a nonzero
   exit as a failed run (`runner._run_java` returns `None`); exercising
   nonzero exit codes needs a harness decision first.
2. Measured GnuCOBOL 3.1.2 quirk: a `WHEN` branch whose ONLY statement is
   `EXIT PROGRAM` compiles to an *empty* branch in a main program, and an
   empty `WHEN` chains into the next `WHEN` — `WHEN OTHER` then ALSO runs.
   No reasonable translation reproduces that, so the draft keeps
   `EXIT PROGRAM` inside a multi-statement block and vectors must never
   encode the quirk.
3. Counter fields are wider than the input domain on purpose: with
   `WS-I PIC 9(4)`, input `N=9999` wraps `9999+1` to `0000` and the loop
   never terminates (measured — rev 2 hung on `9999,P` until widened).

```
$ echo "7,G" | ./exitflw01
TICK=7 SUM=28 MODE=G
STATE=DONE
$ echo "3,P" | ./exitflw01
TICK=4 SUM=6 MODE=P
STATE=RETURNED
$ echo "9999,P" | ./exitflw01
TICK=10000 SUM=40504500 MODE=P
STATE=RETURNED
```
(`3,P` → TICK=4: three loop ticks plus the `WHEN OTHER` tick — mode P is
not `"G"`. `9999,P`: SUM caps at Σ1..9000 = 40,504,500 because the
`IF … CONTINUE` arm skips additions above 9000.)
