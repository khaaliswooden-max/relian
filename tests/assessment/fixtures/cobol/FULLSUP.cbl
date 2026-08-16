       IDENTIFICATION DIVISION.
       PROGRAM-ID. FULLSUP.
      * Every statement here is inside the C1 supported set.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-LINE   PIC X(80).
       01 WS-A      PIC X(10).
       01 WS-B      PIC X(10).
       01 WS-N      PIC 9(3).
       01 WS-AMT    PIC S9(5)V99.
       01 WS-FLAG   PIC X(1).
          88 WS-YES VALUE "Y".
       01 WS-TBL.
          05 WS-ELEM PIC 9(3) OCCURS 5.
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-LINE
           UNSTRING WS-LINE DELIMITED BY "," INTO WS-A WS-B
           MOVE 0 TO WS-N
           COMPUTE WS-AMT ROUNDED = 100 / 3
           ADD 1 TO WS-N
           SET BI TO 1
           INSPECT WS-LINE TALLYING WS-N FOR ALL "X"
           IF WS-N > 0
               DISPLAY WS-N
           ELSE
               DISPLAY WS-A
           END-IF
           EVALUATE TRUE
           WHEN WS-N > 5
               DISPLAY WS-B
           WHEN OTHER
               DISPLAY WS-A
           END-EVALUATE
           PERFORM VARYING WS-N FROM 1 BY 1 UNTIL WS-N > 3
               DISPLAY WS-N
           END-PERFORM
           SEARCH WS-TBL
           AT END DISPLAY WS-A
           WHEN WS-ELEM(BI) = 1
               DISPLAY WS-B
           END-SEARCH
           STOP RUN.
