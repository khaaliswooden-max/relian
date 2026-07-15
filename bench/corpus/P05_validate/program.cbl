       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDAT01.
      * Record validation. Exercises EVALUATE TRUE, INSPECT,
      * reference modification, error-code accumulation.
      * Input : ACCTNO,AMOUNT,STATECODE
      * Output : RC=x MSG=y
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RAW      PIC X(80).
       01  WS-F1       PIC X(20).
       01  WS-F2       PIC X(20).
       01  WS-F3       PIC X(20).
       01  WS-ACCT     PIC X(20).
       01  WS-AMT      PIC S9(7)V99 COMP-3.
       01  WS-STATE    PIC X(2).
       01  WS-RC       PIC 9(3).
       01  WS-MSG      PIC X(24).
       01  WS-DIGITS   PIC 9(3).
       01  WS-LEN      PIC 9(3).
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-RAW FROM CONSOLE
           UNSTRING WS-RAW DELIMITED BY "," INTO
               WS-F1 WS-F2 WS-F3 END-UNSTRING
           MOVE FUNCTION TRIM(WS-F1) TO WS-ACCT
           COMPUTE WS-AMT = FUNCTION NUMVAL(WS-F2)
           MOVE FUNCTION TRIM(WS-F3) TO WS-STATE
           MOVE ZERO TO WS-DIGITS
           INSPECT WS-ACCT TALLYING WS-DIGITS
               FOR ALL "0" "1" "2" "3" "4"
                       "5" "6" "7" "8" "9"
           COMPUTE WS-LEN = FUNCTION LENGTH(FUNCTION TRIM(WS-ACCT))
           MOVE 0 TO WS-RC
           MOVE "OK" TO WS-MSG
           EVALUATE TRUE
               WHEN WS-LEN NOT = 8
                   MOVE 101 TO WS-RC
                   MOVE "BAD ACCT LENGTH" TO WS-MSG
               WHEN WS-DIGITS NOT = 8
                   MOVE 102 TO WS-RC
                   MOVE "ACCT NOT NUMERIC" TO WS-MSG
               WHEN WS-AMT = 0
                   MOVE 201 TO WS-RC
                   MOVE "ZERO AMOUNT" TO WS-MSG
               WHEN WS-AMT < 0
                   MOVE 202 TO WS-RC
                   MOVE "NEGATIVE AMOUNT" TO WS-MSG
               WHEN WS-AMT > 10000.00
                   MOVE 203 TO WS-RC
                   MOVE "EXCEEDS LIMIT" TO WS-MSG
               WHEN WS-STATE = "  "
                   MOVE 301 TO WS-RC
                   MOVE "MISSING STATE" TO WS-MSG
           END-EVALUATE
           DISPLAY "RC=" WS-RC " MSG=" FUNCTION TRIM(WS-MSG)
           STOP RUN.
