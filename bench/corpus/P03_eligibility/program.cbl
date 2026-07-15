       IDENTIFICATION DIVISION.
       PROGRAM-ID. ELIGIBLE01.
      * Benefits eligibility determination (FPL-style).
      * Exercises 88-level condition names, nested IF, EVALUATE.
      * Input : ANNUALINCOME,HOUSEHOLDSIZE,AGE,DISABLED(Y/N)
      * Output : STATUS=x CATEGORY=y FPLPCT=z
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RAW      PIC X(80).
       01  WS-F1       PIC X(20).
       01  WS-F2       PIC X(20).
       01  WS-F3       PIC X(20).
       01  WS-F4       PIC X(20).
       01  WS-INCOME   PIC 9(9)V99 COMP-3.
       01  WS-SIZE     PIC 9(2).
       01  WS-AGE      PIC 9(3).
       01  WS-DIS      PIC X.
           88  IS-DISABLED  VALUE "Y".
       01  WS-FPL      PIC 9(9)V99 COMP-3.
       01  WS-PCT      PIC 9(5)V99 COMP-3.
       01  WS-STATUS   PIC X(10).
       01  WS-CAT      PIC X(12).
       01  WS-O-PCT    PIC Z(4)9.99.
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-RAW FROM CONSOLE
           UNSTRING WS-RAW DELIMITED BY "," INTO
               WS-F1 WS-F2 WS-F3 WS-F4 END-UNSTRING
           COMPUTE WS-INCOME = FUNCTION NUMVAL(WS-F1)
           COMPUTE WS-SIZE   = FUNCTION NUMVAL(WS-F2)
           COMPUTE WS-AGE    = FUNCTION NUMVAL(WS-F3)
           MOVE FUNCTION TRIM(WS-F4) TO WS-DIS
           COMPUTE WS-FPL = 15060 + ((WS-SIZE - 1) * 5380)
           COMPUTE WS-PCT ROUNDED = (WS-INCOME / WS-FPL) * 100
           MOVE "DENIED" TO WS-STATUS
           MOVE "NONE" TO WS-CAT
           EVALUATE TRUE
               WHEN WS-AGE < 19 AND WS-PCT <= 212.00
                   MOVE "APPROVED" TO WS-STATUS
                   MOVE "CHILD" TO WS-CAT
               WHEN IS-DISABLED AND WS-PCT <= 200.00
                   MOVE "APPROVED" TO WS-STATUS
                   MOVE "DISABILITY" TO WS-CAT
               WHEN WS-AGE >= 65 AND WS-PCT <= 150.00
                   MOVE "APPROVED" TO WS-STATUS
                   MOVE "AGED" TO WS-CAT
               WHEN WS-PCT <= 138.00
                   MOVE "APPROVED" TO WS-STATUS
                   MOVE "EXPANSION" TO WS-CAT
               WHEN WS-PCT <= 200.00
                   MOVE "PENDING" TO WS-STATUS
                   MOVE "REVIEW" TO WS-CAT
           END-EVALUATE
           MOVE WS-PCT TO WS-O-PCT
           DISPLAY "STATUS=" FUNCTION TRIM(WS-STATUS)
                   " CATEGORY=" FUNCTION TRIM(WS-CAT)
                   " FPLPCT=" FUNCTION TRIM(WS-O-PCT)
           STOP RUN.
