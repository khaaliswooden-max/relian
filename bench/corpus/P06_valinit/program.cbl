       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALINIT01.
      * ORACLE (WP-1.5.4) -- promoted to corpus for the v1.2 sealing
      * prep. UNSEALED until Khaalis signs the v1.2 ledger (ZCS-6
      * Phase 4, R7); the sealed bench commit must predate any grammar
      * or dispatch-table merge claiming these constructs.
      * Exercises the VALUE clause everywhere C1 currently discards it:
      *   - numeric VALUE (packed and display)
      *   - alphanumeric VALUE
      *   - COMP-3 field with VALUE
      *   - group-level VALUE spread over subordinate fields
      *   - 88-level condition names with single and multiple VALUEs
      * Every DISPLAY depends on an initial value, so a migration that
      * zero-initializes fields fails behaviorally on vector 1.
      * Input : DELTA,CODE     e.g. "100.00,A"
      * Output: STATE=s BAL=b / RATE=r NAME=n G1=x G2=y G3=z
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RAW      PIC X(80).
       01  WS-F1       PIC X(20).
       01  WS-F2       PIC X(20).
       01  WS-DELTA    PIC S9(7)V99 COMP-3.
       01  WS-BAL      PIC S9(7)V99 COMP-3 VALUE 1234.56.
       01  WS-RATE     PIC 9V9(4) VALUE 0.0725.
       01  WS-NAME     PIC X(10) VALUE "ACME LTD".
       01  WS-GRP      VALUE "AB12CD".
           05  WS-G1   PIC X(2).
           05  WS-G2   PIC 9(2).
           05  WS-G3   PIC X(2).
       01  WS-CODE     PIC X.
           88  CODE-ACTIVE  VALUE "A".
           88  CODE-CLOSED  VALUE "C" "X".
       01  WS-OUT-BAL  PIC -(7)9.99.
       01  WS-OUT-RATE PIC 9.9999.
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-RAW FROM CONSOLE
           UNSTRING WS-RAW DELIMITED BY "," INTO WS-F1 WS-F2
           END-UNSTRING
           COMPUTE WS-DELTA = FUNCTION NUMVAL(WS-F1)
           MOVE FUNCTION TRIM(WS-F2) TO WS-CODE
           ADD WS-DELTA TO WS-BAL
           MOVE WS-BAL TO WS-OUT-BAL
           MOVE WS-RATE TO WS-OUT-RATE
           IF CODE-ACTIVE
               DISPLAY "STATE=ACTIVE BAL=" FUNCTION TRIM(WS-OUT-BAL)
           ELSE
               IF CODE-CLOSED
                   DISPLAY "STATE=CLOSED BAL=" FUNCTION TRIM(WS-OUT-BAL)
               ELSE
                   DISPLAY "STATE=OTHER BAL=" FUNCTION TRIM(WS-OUT-BAL)
               END-IF
           END-IF
           DISPLAY "RATE=" WS-OUT-RATE " NAME=" FUNCTION TRIM(WS-NAME)
                   " G1=" WS-G1 " G2=" WS-G2 " G3=" WS-G3
           STOP RUN.
