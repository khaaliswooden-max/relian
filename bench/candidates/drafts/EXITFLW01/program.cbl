       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXITFLW01.
      * DRAFT ORACLE CANDIDATE (WP-1.5.5) -- NOT SEALED, NOT IN CORPUS.
      * Sealing/signing is Khaalis-only (ZCS-6 Phase 4, R7).
      * Exercises the cheap-third dispatch constructs:
      *   - paragraph EXIT (EXIT as the sole statement of a paragraph)
      *   - EXIT PROGRAM (in a mode that then falls through to GOBACK)
      *   - CONTINUE bare, CONTINUE inside IF, CONTINUE inside EVALUATE
      *   - GOBACK without RETURN-CODE, and GOBACK after MOVE 0 TO
      *     RETURN-CODE
      * SEALING NOTE: a NONZERO RETURN-CODE is deliberately absent. The
      * harness treats a nonzero exit as a failed run (runner._run_java
      * returns None), so vectors exercising it would need a harness
      * decision first. Flagged for the sealing review, not smuggled in.
      * Input : N,MODE          e.g. "7,G"
      * Output: TICK=t SUM=s MODE=m
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RAW      PIC X(80).
       01  WS-F1       PIC X(20).
       01  WS-F2       PIC X(20).
       01  WS-N        PIC 9(4).
       01  WS-I        PIC 9(4) VALUE 0.
       01  WS-SUM      PIC 9(8) VALUE 0.
       01  WS-TICK     PIC 9(4) VALUE 0.
       01  WS-MODE     PIC X.
       01  WS-OUT-SUM  PIC Z(7)9.
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-RAW FROM CONSOLE
           UNSTRING WS-RAW DELIMITED BY "," INTO WS-F1 WS-F2
           END-UNSTRING
           COMPUTE WS-N = FUNCTION NUMVAL(WS-F1)
           MOVE FUNCTION TRIM(WS-F2) TO WS-MODE
           PERFORM ACCUM-PARA VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-N
           EVALUATE WS-MODE
               WHEN "G"
                   CONTINUE
               WHEN "P"
                   PERFORM NOOP-PARA
               WHEN OTHER
                   ADD 1 TO WS-TICK
           END-EVALUATE
           MOVE WS-SUM TO WS-OUT-SUM
           DISPLAY "TICK=" WS-TICK " SUM=" FUNCTION TRIM(WS-OUT-SUM)
                   " MODE=" WS-MODE
           IF WS-MODE = "P"
               MOVE 0 TO RETURN-CODE
               GOBACK
           END-IF
           GOBACK.
       ACCUM-PARA.
           IF WS-I > 9000
               CONTINUE
           ELSE
               ADD WS-I TO WS-SUM
           END-IF
           ADD 1 TO WS-TICK.
       NOOP-PARA.
           EXIT.
