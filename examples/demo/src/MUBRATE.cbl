      *================================================================*
      * MUBRATE  -  MERIDIAN MUNICIPAL UTILITY DISTRICT                *
      *             TIERED CONSUMPTION RATING MODULE                   *
      *                                                                *
      * SYNTHETIC DEMONSTRATION CODE.  NOT PRODUCTION.  NOT DERIVED    *
      * FROM ANY CUSTOMER OR BENCHMARK SOURCE.                         *
      *                                                                *
      * FUNCTION                                                       *
      *   READS ONE COMMA DELIMITED METER RECORD FROM SYSIN, DERIVES   *
      *   BILLABLE CONSUMPTION, APPLIES THE SIX BLOCK INCLINING RATE   *
      *   SCHEDULE, ADDS THE CLASS BASE CHARGE PLUS THE REGULATORY     *
      *   SERVICE FACTOR, AND PRINTS THE RATED LINE.                   *
      *                                                                *
      * TIER (RELIAN DEMO):  A - WHOLLY INSIDE THE C1 SUBSET           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MUBRATE.
       AUTHOR. MERIDIAN-MUD-DP.
       DATE-WRITTEN. 1987-04-12.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      *---------------------------------------------------------------*
      * INPUT AREA.  THE CYCLE EXTRACT IS A FLAT COMMA DELIMITED LINE  *
      * BECAUSE THE 1987 CONVERSION NEVER GOT ITS VSAM CLUSTER.        *
      *---------------------------------------------------------------*
       01  WS-INPUT-LINE               PIC X(80).
       01  WS-METER-REC.
           05  WS-IN-ACCOUNT           PIC X(10).
           05  WS-IN-CYCLE             PIC X(02).
           05  WS-IN-CLASS             PIC X(01).
           05  WS-IN-CURR              PIC X(09).
           05  WS-IN-PRIOR             PIC X(09).
      *
      *---------------------------------------------------------------*
      * RATE CLASS.  CODES ARE CARRIED IN THE CUSTOMER MASTER AND ARE  *
      * VALIDATED UPSTREAM BY MUBSURC.                                 *
      *---------------------------------------------------------------*
       01  WS-CLASS-CODE               PIC X(01).
           88  CLASS-RESIDENTIAL       VALUE "R".
           88  CLASS-COMMERCIAL        VALUE "C".
           88  CLASS-INDUSTRIAL        VALUE "I".
           88  CLASS-IRRIGATION        VALUE "G".
           88  CLASS-MUNICIPAL         VALUE "M".
           88  CLASS-NONBILLABLE       VALUE "X" "Z".
      *
      *---------------------------------------------------------------*
      * SIX BLOCK INCLINING RATE SCHEDULE, ORD. 1986-114 AS AMENDED.   *
      * LIMIT IS THE BLOCK WIDTH IN GALLONS, CUM IS THE RUNNING TOP    *
      * OF BLOCK USED BY THE TIER LOCATE.                              *
      *---------------------------------------------------------------*
       01  WS-TIER-TABLE.
           05  WS-TIER-LIMIT   PIC 9(06) OCCURS 6 INDEXED BY BI.
           05  WS-TIER-RATE    PIC 9(02)V9(04) OCCURS 6.
           05  WS-TIER-CUM     PIC 9(07) OCCURS 6.
      *
      *---------------------------------------------------------------*
      * COMPUTATION AREA.  PACKED DECIMAL THROUGHOUT, AS THE ORIGINAL  *
      * SHOP STANDARD REQUIRED FOR ANY MONEY OR QUANTITY FIELD.        *
      *---------------------------------------------------------------*
       01  WS-USAGE                    PIC S9(07)V99 COMP-3.
       01  WS-REMAINING                PIC S9(07)V99 COMP-3.
       01  WS-BLOCK-QTY                PIC S9(07)V99 COMP-3.
       01  WS-TIER-CHG                 PIC S9(07)V99 COMP-3.
       01  WS-BASE-CHG                 PIC S9(05)V99 COMP-3.
       01  WS-SVC-CHG                  PIC S9(05)V99 COMP-3.
       01  WS-TOTAL-CHG                PIC S9(07)V99 COMP-3.
       01  WS-TIER-IX                  PIC 9(02).
       01  WS-TIER-FOUND               PIC 9(02) VALUE ZERO.
       01  WS-PAD-COUNT                PIC 9(03) VALUE ZERO.
      *
      *---------------------------------------------------------------*
      * REPORT EDIT FIELDS.                                            *
      *---------------------------------------------------------------*
       01  WS-EDT-USAGE                PIC ZZZZZZ9.99.
       01  WS-EDT-TIER                 PIC ZZZZZZ9.99.
       01  WS-EDT-BASE                 PIC ZZZZZZ9.99.
       01  WS-EDT-TOTAL                PIC ZZZZZZ9.99.
      *
       PROCEDURE DIVISION.
      *
       0000-MAIN.
           DISPLAY "MUBRATE   MERIDIAN MUD CONSUMPTION RATING"
           ACCEPT WS-INPUT-LINE
           UNSTRING WS-INPUT-LINE DELIMITED BY ","
               INTO WS-IN-ACCOUNT WS-IN-CYCLE WS-IN-CLASS
                    WS-IN-CURR WS-IN-PRIOR
           END-UNSTRING
           MOVE WS-IN-CLASS TO WS-CLASS-CODE
           INSPECT WS-IN-ACCOUNT TALLYING WS-PAD-COUNT FOR ALL " "
           CONTINUE.
      *
       0100-EDIT-INPUT.
           COMPUTE WS-USAGE =
               FUNCTION NUMVAL(WS-IN-CURR)
               - FUNCTION NUMVAL(WS-IN-PRIOR)
           IF WS-USAGE < 0
               DISPLAY "E001 NEGATIVE CONSUMPTION - METER ROLLOVER"
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
           IF WS-PAD-COUNT > 4
               DISPLAY "E002 ACCOUNT KEY SHORT - CHECK EXTRACT"
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
           IF CLASS-NONBILLABLE
               DISPLAY "I003 NON BILLABLE CLASS - NO CHARGE RAISED"
               MOVE 4 TO RETURN-CODE
               GOBACK
           END-IF.
      *
       1000-LOAD-RATE-TABLE.
           MOVE 002000 TO WS-TIER-LIMIT(1)
           MOVE 005000 TO WS-TIER-LIMIT(2)
           MOVE 008000 TO WS-TIER-LIMIT(3)
           MOVE 010000 TO WS-TIER-LIMIT(4)
           MOVE 025000 TO WS-TIER-LIMIT(5)
           MOVE 999999 TO WS-TIER-LIMIT(6)
           MOVE 0.0231 TO WS-TIER-RATE(1)
           MOVE 0.0348 TO WS-TIER-RATE(2)
           MOVE 0.0479 TO WS-TIER-RATE(3)
           MOVE 0.0612 TO WS-TIER-RATE(4)
           MOVE 0.0844 TO WS-TIER-RATE(5)
           MOVE 0.1105 TO WS-TIER-RATE(6)
           MOVE 0002000 TO WS-TIER-CUM(1)
           MOVE 0007000 TO WS-TIER-CUM(2)
           MOVE 0015000 TO WS-TIER-CUM(3)
           MOVE 0025000 TO WS-TIER-CUM(4)
           MOVE 0050000 TO WS-TIER-CUM(5)
           MOVE 9999999 TO WS-TIER-CUM(6).
      *
       2000-BASE-CHARGE.
           EVALUATE TRUE
               WHEN CLASS-RESIDENTIAL
                   MOVE 14.75 TO WS-BASE-CHG
               WHEN CLASS-COMMERCIAL
                   MOVE 38.50 TO WS-BASE-CHG
               WHEN CLASS-INDUSTRIAL
                   MOVE 145.00 TO WS-BASE-CHG
               WHEN CLASS-IRRIGATION
                   MOVE 22.00 TO WS-BASE-CHG
               WHEN CLASS-MUNICIPAL
                   MOVE 0 TO WS-BASE-CHG
               WHEN OTHER
                   MOVE 14.75 TO WS-BASE-CHG
           END-EVALUATE.
      *
       3000-RATE-BLOCKS.
           MOVE ZERO TO WS-TIER-CHG
           MOVE WS-USAGE TO WS-REMAINING
           PERFORM VARYING WS-TIER-IX FROM 1 BY 1
                   UNTIL WS-TIER-IX > 6
               IF WS-REMAINING > 0
                   COMPUTE WS-BLOCK-QTY = WS-TIER-LIMIT(WS-TIER-IX)
                   IF WS-REMAINING < WS-BLOCK-QTY
                       COMPUTE WS-BLOCK-QTY = WS-REMAINING
                   END-IF
                   COMPUTE WS-TIER-CHG ROUNDED = WS-TIER-CHG
                       + WS-BLOCK-QTY * WS-TIER-RATE(WS-TIER-IX)
                   COMPUTE WS-REMAINING = WS-REMAINING - WS-BLOCK-QTY
               END-IF
           END-PERFORM.
      *
       4000-LOCATE-TIER.
           SET BI TO 1
           SEARCH WS-TIER-CUM
               AT END DISPLAY "W004 USAGE ABOVE TOP RATE BLOCK"
               WHEN WS-TIER-CUM(BI) >= WS-USAGE
                   SET WS-TIER-FOUND TO BI
           END-SEARCH.
      *
       5000-ASSEMBLE-TOTAL.
           COMPUTE WS-SVC-CHG ROUNDED = WS-BASE-CHG * 1.0725
           COMPUTE WS-TOTAL-CHG ROUNDED = WS-TIER-CHG + WS-SVC-CHG
           MOVE WS-USAGE TO WS-EDT-USAGE
           MOVE WS-TIER-CHG TO WS-EDT-TIER
           MOVE WS-BASE-CHG TO WS-EDT-BASE
           MOVE WS-TOTAL-CHG TO WS-EDT-TOTAL.
      *
       6000-REPORT.
           DISPLAY "ACCOUNT    " WS-IN-ACCOUNT
           DISPLAY "CYCLE      " WS-IN-CYCLE
           DISPLAY "CLASS      " WS-CLASS-CODE
           DISPLAY "USAGE GAL  " WS-EDT-USAGE
           DISPLAY "TIER INDEX " WS-TIER-FOUND
           DISPLAY "BASE CHG   " WS-EDT-BASE
           DISPLAY "BLOCK CHG  " WS-EDT-TIER
           DISPLAY "TOTAL DUE  " WS-EDT-TOTAL
           MOVE 0 TO RETURN-CODE
           STOP RUN.
