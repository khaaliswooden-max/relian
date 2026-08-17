       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFPAR01.
      * ORACLE (PROPOSED for RELIAN-BENCH v1.3) -- DRAFT, UNSEALED.
      * This file is a proposal. It is NOT part of the benchmark until
      * Khaalis moves it to bench/corpus/P08_performpara/program.cbl and
      * signs a v1.3 ledger (ZCS-6 Phase 4, R7). The sealed bench commit
      * must predate any dispatch-table or grammar merge claiming these
      * constructs.
      *
      * PURPOSE. Gate out-of-line PERFORM <paragraph> and paragraph
      * EXIT -- together 53.9% of all unsupported-construct occurrences
      * across the three WP-1.9 third-party corpora (PERFORM 1,380 +
      * EXIT 367 of 3,241). P07_exitflow rev 1 tried to carry these and
      * could not (Bugbot, PR #10): they are a control-flow model
      * change, not dispatch-table work, so they need their own program.
      *
      * ISOLATION. Every OTHER construct here is already corpus-proven
      * and measured-supported: ACCEPT, UNSTRING, COMPUTE (incl.
      * ROUNDED), MOVE, IF/ELSE/END-IF, EVALUATE <selector>/WHEN,
      * DISPLAY, GOBACK, MOVE n TO RETURN-CODE, FUNCTION NUMVAL,
      * FUNCTION TRIM, COMP-3, edited pictures. So the delta between a
      * red and a green run on this program is attributable to
      * performed-paragraph support alone.
      *
      * WHAT THE VECTORS PIN, and how a wrong implementation fails:
      *   - RETURN: after PERFORM P, control resumes at the statement
      *     AFTER the PERFORM. An implementation that jumps without
      *     returning loses every later paragraph's effect.
      *   - NO FALL-THROUGH: PERFORM VALIDATE-INPUT executes that
      *     paragraph ONLY. An implementation that runs on into
      *     SET-RATE (the next paragraph in source) computes a rate on
      *     a rejected record and diverges on the reject vectors.
      *   - REUSE: COMPUTE-BONUS is performed from TWO call sites with
      *     different WS-TIER values. An implementation that inlines the
      *     first occurrence and drops the second returns BONUS=0.00 on
      *     the 5-year tier.
      *   - NESTING: APPLY-TIER is itself performed, and performs
      *     COMPUTE-BONUS. A one-level-deep implementation fails.
      *   - PARAGRAPH EXIT: every paragraph ends with a bare EXIT, the
      *     idiom C1 currently refuses. It must be a no-op that returns.
      *   - RETURN-CODE: the reject path exits 8, so a migration that
      *     drops RETURN-CODE semantics fails behaviorally (WP-1.5.0d).
      *
      * Input : SALES,RATECODE,YEARS   e.g. "125000.00,3,12"
      * Output: STATUS=s BASE=b BONUS=n TOTAL=t   (one line)
      * Exit  : 0 accepted, 8 rejected
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RAW        PIC X(80).
       01  WS-F1         PIC X(20).
       01  WS-F2         PIC X(20).
       01  WS-F3         PIC X(20).
       01  WS-SALES      PIC 9(7)V99 COMP-3.
       01  WS-CODE       PIC 9(2).
       01  WS-YEARS      PIC 9(3).
       01  WS-RATE       PIC 9V9(4) COMP-3.
       01  WS-BASE       PIC 9(7)V99 COMP-3.
       01  WS-BONUS      PIC 9(7)V99 COMP-3.
       01  WS-TOTAL      PIC 9(7)V99 COMP-3.
       01  WS-TIER       PIC 9(3).
       01  WS-STATUS     PIC X(6).
       01  WS-O-BASE     PIC Z(6)9.99.
       01  WS-O-BONUS    PIC Z(6)9.99.
       01  WS-O-TOTAL    PIC Z(6)9.99.
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-RAW FROM CONSOLE
           UNSTRING WS-RAW DELIMITED BY ","
               INTO WS-F1 WS-F2 WS-F3
           END-UNSTRING
           COMPUTE WS-SALES = FUNCTION NUMVAL(WS-F1)
           COMPUTE WS-CODE  = FUNCTION NUMVAL(WS-F2)
           COMPUTE WS-YEARS = FUNCTION NUMVAL(WS-F3)
           MOVE 0 TO WS-BASE
           MOVE 0 TO WS-BONUS
           MOVE 0 TO WS-TOTAL
           MOVE 0 TO WS-TIER

           PERFORM VALIDATE-INPUT

           IF WS-STATUS = "REJECT"
               PERFORM FORMAT-LINE
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF

           PERFORM SET-RATE
           PERFORM COMPUTE-BASE
           PERFORM APPLY-TIER
           COMPUTE WS-TOTAL = WS-BASE + WS-BONUS
           PERFORM FORMAT-LINE
           MOVE 0 TO RETURN-CODE
           GOBACK.

      * Performed, and followed in source by another paragraph: an
      * implementation that falls through computes a rate for a
      * rejected record.
       VALIDATE-INPUT.
           MOVE "OK" TO WS-STATUS
           IF WS-SALES > 999999.99
               MOVE "REJECT" TO WS-STATUS
           END-IF
           IF WS-CODE < 1
               MOVE "REJECT" TO WS-STATUS
           END-IF
           IF WS-CODE > 4
               MOVE "REJECT" TO WS-STATUS
           END-IF
           EXIT.

       SET-RATE.
           EVALUATE WS-CODE
               WHEN 1
                   MOVE 0.0200 TO WS-RATE
               WHEN 2
                   MOVE 0.0350 TO WS-RATE
               WHEN 3
                   MOVE 0.0500 TO WS-RATE
               WHEN 4
                   MOVE 0.0750 TO WS-RATE
           END-EVALUATE
           EXIT.

       COMPUTE-BASE.
           COMPUTE WS-BASE ROUNDED = WS-SALES * WS-RATE
           EXIT.

      * Performed, and itself performs COMPUTE-BONUS -- from two
      * distinct call sites, with different WS-TIER.
       APPLY-TIER.
           IF WS-YEARS >= 10
               MOVE 10 TO WS-TIER
               PERFORM COMPUTE-BONUS
           ELSE
               IF WS-YEARS >= 5
                   MOVE 5 TO WS-TIER
                   PERFORM COMPUTE-BONUS
               END-IF
           END-IF
           EXIT.

       COMPUTE-BONUS.
           COMPUTE WS-BONUS ROUNDED = WS-BASE * WS-TIER / 100
           EXIT.

      * Performed from two call sites in MAIN-PARA (accept and reject).
       FORMAT-LINE.
           MOVE WS-BASE TO WS-O-BASE
           MOVE WS-BONUS TO WS-O-BONUS
           MOVE WS-TOTAL TO WS-O-TOTAL
           DISPLAY "STATUS=" FUNCTION TRIM(WS-STATUS)
                   " BASE=" FUNCTION TRIM(WS-O-BASE)
                   " BONUS=" FUNCTION TRIM(WS-O-BONUS)
                   " TOTAL=" FUNCTION TRIM(WS-O-TOTAL)
           EXIT.
