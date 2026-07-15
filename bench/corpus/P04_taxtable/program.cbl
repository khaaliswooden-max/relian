       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAXTBL01.
      * Graduated tax via OCCURS table + SEARCH. Exercises tables,
      * indexes, SEARCH, cumulative bracket arithmetic.
      * Input : TAXABLEINCOME,FILINGSTATUS(S/M)
      * Output : TAX=x BRACKET=y MARGINAL=z
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RAW      PIC X(80).
       01  WS-F1       PIC X(20).
       01  WS-F2       PIC X(20).
       01  WS-INC      PIC 9(9)V99 COMP-3.
       01  WS-ST       PIC X.
       01  WS-BRACKETS.
           05  WS-BR OCCURS 5 TIMES INDEXED BY BI.
               10  BR-CEIL  PIC 9(9)V99 COMP-3.
               10  BR-RATE  PIC 9V9(4) COMP-3.
               10  BR-BASE  PIC 9(9)V99 COMP-3.
       01  WS-TAX      PIC 9(9)V99 COMP-3.
       01  WS-IDX      PIC 9.
       01  WS-PREV     PIC 9(9)V99 COMP-3.
       01  WS-MULT     PIC 9V99 COMP-3.
       01  WS-O-TAX    PIC Z(8)9.99.
       01  WS-O-MARG   PIC Z9.9999.
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-RAW FROM CONSOLE
           UNSTRING WS-RAW DELIMITED BY "," INTO WS-F1 WS-F2
           END-UNSTRING
           COMPUTE WS-INC = FUNCTION NUMVAL(WS-F1)
           MOVE FUNCTION TRIM(WS-F2) TO WS-ST
           IF WS-ST = "M"
               MOVE 2.00 TO WS-MULT
           ELSE
               MOVE 1.00 TO WS-MULT
           END-IF
           COMPUTE BR-CEIL(1) = 11600.00 * WS-MULT
           MOVE 0.1000 TO BR-RATE(1)
           MOVE 0.00   TO BR-BASE(1)
           COMPUTE BR-CEIL(2) = 47150.00 * WS-MULT
           MOVE 0.1200 TO BR-RATE(2)
           COMPUTE BR-BASE(2) = 1160.00 * WS-MULT
           COMPUTE BR-CEIL(3) = 100525.00 * WS-MULT
           MOVE 0.2200 TO BR-RATE(3)
           COMPUTE BR-BASE(3) = 5426.00 * WS-MULT
           COMPUTE BR-CEIL(4) = 191950.00 * WS-MULT
           MOVE 0.2400 TO BR-RATE(4)
           COMPUTE BR-BASE(4) = 17168.50 * WS-MULT
           COMPUTE BR-CEIL(5) = 999999999.00
           MOVE 0.3200 TO BR-RATE(5)
           COMPUTE BR-BASE(5) = 39110.50 * WS-MULT
           SET BI TO 1
           SEARCH WS-BR
               AT END MOVE 5 TO WS-IDX
               WHEN WS-INC <= BR-CEIL(BI)
                   SET WS-IDX TO BI
           END-SEARCH
           IF WS-IDX = 1
               MOVE ZERO TO WS-PREV
           ELSE
               COMPUTE WS-PREV = BR-CEIL(WS-IDX - 1)
           END-IF
           COMPUTE WS-TAX ROUNDED =
               BR-BASE(WS-IDX) + ((WS-INC - WS-PREV)
               * BR-RATE(WS-IDX))
           MOVE WS-TAX TO WS-O-TAX
           MOVE BR-RATE(WS-IDX) TO WS-O-MARG
           DISPLAY "TAX=" FUNCTION TRIM(WS-O-TAX)
                   " BRACKET=" WS-IDX
                   " MARGINAL=" FUNCTION TRIM(WS-O-MARG)
           STOP RUN.
