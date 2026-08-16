       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXITFLW01.
      * ORACLE (WP-1.5.5, rev 3) -- promoted to corpus for the v1.2
      * sealing prep. UNSEALED until Khaalis signs the v1.2 ledger
      * (ZCS-6 Phase 4, R7); the sealed bench commit must predate any
      * grammar or dispatch-table merge claiming these constructs.
      * Exercises exactly the constructs the WP-1.5.5 handlers would add,
      * on scaffolding C1 already supports (inline PERFORM VARYING,
      * EVALUATE/WHEN/WHEN OTHER, IF/ELSE -- all corpus-proven forms):
      *   - CONTINUE bare, CONTINUE inside IF, CONTINUE inside EVALUATE
      *   - EXIT PROGRAM in a main program (measured with GnuCOBOL 3.1.2:
      *     a no-op that falls through -- both surrounding DISPLAYs run)
      *   - GOBACK without RETURN-CODE, GOBACK after MOVE 0 TO
      *     RETURN-CODE, and (rev 3) GOBACK after MOVE 4/8 TO RETURN-CODE
      * Rev 2 (Bugbot finding on PR #10, confirmed): rev 1 drove its loop
      * with out-of-line PERFORM <para> and tested paragraph EXIT. C1's
      * PERFORM handler supports only the inline VARYING form, so rev 1
      * could never gate the three handlers alone. Paragraph EXIT is
      * inseparable from performed-paragraph support and moves to the
      * deferred list; it is NOT dispatch-table-only work.
      * Rev 3 (v1.2 sealing prep): rev 2's SEALING NOTE 1 said nonzero
      * RETURN-CODE needed a harness decision first. That decision is
      * WP-1.5.0d (merged c7b199f): the scorer now compares the process
      * exit code against the vector's expected_exit. Modes E and W
      * exercise GOBACK with an explicit nonzero RETURN-CODE (8 and 4,
      * conventional mainframe condition codes), so a migration that
      * drops RETURN-CODE semantics fails behaviorally.
      * SEALING NOTE (measured, GnuCOBOL 3.1.2): a WHEN branch whose
      * ONLY statement is EXIT PROGRAM compiles to an EMPTY branch in a
      * main program, and an empty WHEN chains into the next WHEN -- the
      * WHEN OTHER branch then ALSO executes. That is an oracle quirk no
      * reasonable translation would reproduce, so EXIT PROGRAM here sits
      * inside a multi-statement IF block, where it is a measured no-op,
      * and no vector's expected output may depend on the quirk.
      * Input : N,MODE          e.g. "7,G"
      * Output: TICK=t SUM=s MODE=m  (one line, then STATE line)
      * Exit  : 0 (modes G/P/other), 4 (mode W), 8 (mode E)
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
           IF WS-MODE = "E"
               DISPLAY "STATE=FAULT"
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
           IF WS-MODE = "W"
               DISPLAY "STATE=WARN"
               MOVE 4 TO RETURN-CODE
               GOBACK
           END-IF
           DISPLAY "STATE=DONE"
           GOBACK.
