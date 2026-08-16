       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXITFLW01.
      * DRAFT ORACLE CANDIDATE (WP-1.5.5, rev 2) -- NOT SEALED, NOT IN
      * CORPUS. Sealing/signing is Khaalis-only (ZCS-6 Phase 4, R7).
      * Exercises exactly the constructs the WP-1.5.5 handlers would add,
      * on scaffolding C1 already supports (inline PERFORM VARYING,
      * EVALUATE/WHEN/WHEN OTHER, IF/ELSE -- all corpus-proven forms):
      *   - CONTINUE bare, CONTINUE inside IF, CONTINUE inside EVALUATE
      *   - EXIT PROGRAM in a main program (measured with GnuCOBOL 3.1.2:
      *     a no-op that falls through -- both surrounding DISPLAYs run)
      *   - GOBACK without RETURN-CODE, and GOBACK after MOVE 0 TO
      *     RETURN-CODE
      * Rev 2 (Bugbot finding on PR #10, confirmed): rev 1 drove its loop
      * with out-of-line PERFORM <para> and tested paragraph EXIT. C1's
      * PERFORM handler supports only the inline VARYING form, so rev 1
      * could never gate the three handlers alone. Paragraph EXIT is
      * inseparable from performed-paragraph support and moves to the
      * deferred list; it is NOT dispatch-table-only work.
      * SEALING NOTE 1: no vector may set a NONZERO RETURN-CODE. The
      * harness treats a nonzero exit as a failed run (runner._run_java
      * returns None); exercising nonzero exit codes needs a harness
      * decision first. Flagged for the sealing review, not smuggled in.
      * SEALING NOTE 2 (measured, GnuCOBOL 3.1.2): a WHEN branch whose
      * ONLY statement is EXIT PROGRAM compiles to an EMPTY branch in a
      * main program, and an empty WHEN chains into the next WHEN -- the
      * WHEN OTHER branch then ALSO executes. That is an oracle quirk no
      * reasonable translation would reproduce, so EXIT PROGRAM here sits
      * inside a multi-statement IF block, where it is a measured no-op.
      * Input : N,MODE          e.g. "7,G"
      * Output: TICK=t SUM=s MODE=m  (one line, then STATE line)
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RAW      PIC X(80).
       01  WS-F1       PIC X(20).
       01  WS-F2       PIC X(20).
       01  WS-N        PIC 9(4).
      * WS-I/WS-TICK are wider than WS-N on purpose: with WS-I PIC 9(4),
      * an input of N=9999 wraps 9999+1 to 0000 and the loop never
      * terminates (measured -- rev 2 hung on "9999,P"). Counters must be
      * able to exceed the input domain.
       01  WS-I        PIC 9(8).
       01  WS-SUM      PIC 9(8).
       01  WS-TICK     PIC 9(8).
       01  WS-MODE     PIC X.
       01  WS-OUT-SUM  PIC Z(7)9.
       01  WS-OUT-TICK PIC Z(7)9.
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-RAW FROM CONSOLE
           UNSTRING WS-RAW DELIMITED BY "," INTO WS-F1 WS-F2
           END-UNSTRING
           COMPUTE WS-N = FUNCTION NUMVAL(WS-F1)
           MOVE FUNCTION TRIM(WS-F2) TO WS-MODE
           MOVE 0 TO WS-SUM
           MOVE 0 TO WS-TICK
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-N
               IF WS-I > 9000
                   CONTINUE
               ELSE
                   ADD WS-I TO WS-SUM
               END-IF
               ADD 1 TO WS-TICK
           END-PERFORM
           EVALUATE WS-MODE
               WHEN "G"
                   CONTINUE
               WHEN OTHER
                   ADD 1 TO WS-TICK
           END-EVALUATE
           MOVE WS-SUM TO WS-OUT-SUM
           MOVE WS-TICK TO WS-OUT-TICK
           DISPLAY "TICK=" FUNCTION TRIM(WS-OUT-TICK)
                   " SUM=" FUNCTION TRIM(WS-OUT-SUM)
                   " MODE=" WS-MODE
           IF WS-MODE = "P"
               CONTINUE
               EXIT PROGRAM
               DISPLAY "STATE=RETURNED"
               MOVE 0 TO RETURN-CODE
               GOBACK
           END-IF
           DISPLAY "STATE=DONE"
           GOBACK.
