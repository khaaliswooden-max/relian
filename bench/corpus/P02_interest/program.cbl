       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEREST01.
      * Compound interest w/ periodic deposit. Exercises PERFORM
      * VARYING, iterative rounding accumulation, COMP-3.
      * Input : PRINCIPAL,ANNUALRATEPCT,YEARS,MONTHLYADD
      * Output : BAL=x INT=y
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RAW      PIC X(80).
       01  WS-F1       PIC X(20).
       01  WS-F2       PIC X(20).
       01  WS-F3       PIC X(20).
       01  WS-F4       PIC X(20).
       01  WS-PRIN     PIC 9(9)V99 COMP-3.
       01  WS-RATE     PIC 9(3)V9(4) COMP-3.
       01  WS-YEARS    PIC 9(3).
       01  WS-ADD      PIC 9(7)V99 COMP-3.
       01  WS-BAL      PIC 9(11)V99 COMP-3.
       01  WS-MRATE    PIC 9(3)V9(8) COMP-3.
       01  WS-INT      PIC 9(11)V99 COMP-3.
       01  WS-TOTADD   PIC 9(11)V99 COMP-3.
       01  I           PIC 9(5).
       01  WS-MONTHS   PIC 9(5).
       01  WS-O-BAL    PIC Z(10)9.99.
       01  WS-O-INT    PIC Z(10)9.99.
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-RAW FROM CONSOLE
           UNSTRING WS-RAW DELIMITED BY "," INTO
               WS-F1 WS-F2 WS-F3 WS-F4 END-UNSTRING
           COMPUTE WS-PRIN  = FUNCTION NUMVAL(WS-F1)
           COMPUTE WS-RATE  = FUNCTION NUMVAL(WS-F2)
           COMPUTE WS-YEARS = FUNCTION NUMVAL(WS-F3)
           COMPUTE WS-ADD   = FUNCTION NUMVAL(WS-F4)
           COMPUTE WS-MRATE = WS-RATE / 100 / 12
           COMPUTE WS-MONTHS = WS-YEARS * 12
           MOVE WS-PRIN TO WS-BAL
           MOVE ZERO TO WS-TOTADD
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-MONTHS
               COMPUTE WS-BAL ROUNDED = WS-BAL * (1 + WS-MRATE)
               ADD WS-ADD TO WS-BAL
               ADD WS-ADD TO WS-TOTADD
           END-PERFORM
           COMPUTE WS-INT = WS-BAL - WS-PRIN - WS-TOTADD
           MOVE WS-BAL TO WS-O-BAL
           MOVE WS-INT TO WS-O-INT
           DISPLAY "BAL=" FUNCTION TRIM(WS-O-BAL)
                   " INT=" FUNCTION TRIM(WS-O-INT)
           STOP RUN.
