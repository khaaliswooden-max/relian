      *================================================================*
      * MUBBILL  -  MERIDIAN MUNICIPAL UTILITY DISTRICT                *
      *             BILL ASSEMBLY AND PRINT LINE FORMATTER             *
      *                                                                *
      * SYNTHETIC DEMONSTRATION CODE.  NOT PRODUCTION.  NOT DERIVED    *
      * FROM ANY CUSTOMER OR BENCHMARK SOURCE.                         *
      *                                                                *
      * FUNCTION                                                       *
      *   DRIVES THE RATING AND SURCHARGE MODULES, NETS THE PRIOR      *
      *   BALANCE AGAINST PAYMENTS RECEIVED, AND BUILDS THE REMITTANCE *
      *   STUB LINE FOR THE LASER PRINT STREAM.                        *
      *                                                                *
      * TIER (RELIAN DEMO):  B - MOSTLY INSIDE THE C1 SUBSET.          *
      *   THE ARITHMETIC AND CONTROL FLOW ARE SUPPORTED.  THE MODULE   *
      *   LINKAGE (CALL), THE TEXT ASSEMBLY (STRING), THE ALTERNATE    *
      *   ARITHMETIC FORMS (SUBTRACT / MULTIPLY / DIVIDE / ADD GIVING) *
      *   AND THE PARAGRAPH PERFORM ARE NOT.  THIS PROGRAM EXISTS TO   *
      *   SHOW A PARTIAL-COVERAGE RESULT AND AN HONEST REFUSAL, NOT A  *
      *   SUCCESSFUL MIGRATION.                                        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MUBBILL.
       AUTHOR. MERIDIAN-MUD-DP.
       DATE-WRITTEN. 1989-11-08.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
           COPY MUBCONS.
           COPY MUBBREC.
      *
       01  WS-INPUT-LINE               PIC X(80).
       01  WS-BILL-IN.
           05  WS-IN-ACCOUNT           PIC X(10).
           05  WS-IN-CLASS             PIC X(01).
           05  WS-IN-CURRENT           PIC X(11).
           05  WS-IN-PRIOR-BAL         PIC X(11).
           05  WS-IN-PAYMENTS          PIC X(11).
      *
       01  WS-CURRENT-CHG              PIC S9(09)V99 COMP-3.
       01  WS-PRIOR-BAL                PIC S9(09)V99 COMP-3.
       01  WS-PAYMENTS                 PIC S9(09)V99 COMP-3.
       01  WS-NET-PRIOR                PIC S9(09)V99 COMP-3.
       01  WS-SURCHARGE                PIC S9(07)V99 COMP-3.
       01  WS-TAX-BASE                 PIC S9(09)V99 COMP-3.
       01  WS-FRANCHISE-TAX            PIC S9(07)V99 COMP-3.
       01  WS-AMOUNT-DUE               PIC S9(09)V99 COMP-3.
       01  WS-UNIT-COST                PIC S9(05)V9(04) COMP-3.
       01  WS-USAGE-UNITS              PIC S9(07) COMP-3.
      *
       01  WS-STUB-LINE                PIC X(72).
       01  WS-EDT-DUE                  PIC ZZZZZZZZ9.99.
       01  WS-EDT-NET                  PIC ZZZZZZZZ9.99.
      *
       PROCEDURE DIVISION.
      *
       0000-MAIN.
           DISPLAY "MUBBILL   BILL ASSEMBLY"
           ACCEPT WS-INPUT-LINE
           UNSTRING WS-INPUT-LINE DELIMITED BY ","
               INTO WS-IN-ACCOUNT WS-IN-CLASS WS-IN-CURRENT
                    WS-IN-PRIOR-BAL WS-IN-PAYMENTS
           END-UNSTRING
           COMPUTE WS-CURRENT-CHG = FUNCTION NUMVAL(WS-IN-CURRENT)
           COMPUTE WS-PRIOR-BAL = FUNCTION NUMVAL(WS-IN-PRIOR-BAL)
           COMPUTE WS-PAYMENTS = FUNCTION NUMVAL(WS-IN-PAYMENTS).
      *
       0100-CALL-SURCHARGE.
      *    ---- MODULE LINKAGE.  NOT IN THE C1 SUBSET. ----------------
           CALL "MUBSURC" USING WS-IN-ACCOUNT WS-SURCHARGE
           IF WS-SURCHARGE < 0
               MOVE ZERO TO WS-SURCHARGE
           END-IF.
      *
       0200-NET-PRIOR-BALANCE.
      *    ---- ALTERNATE ARITHMETIC FORMS.  NOT IN THE SUBSET. -------
           SUBTRACT WS-PAYMENTS FROM WS-PRIOR-BAL
               GIVING WS-NET-PRIOR
           IF WS-NET-PRIOR < 0
               MOVE ZERO TO WS-NET-PRIOR
           END-IF
           ADD WS-CURRENT-CHG WS-SURCHARGE GIVING WS-TAX-BASE
           MULTIPLY WS-TAX-BASE BY 0.0625 GIVING WS-FRANCHISE-TAX
                    ROUNDED
           DIVIDE WS-CURRENT-CHG BY WS-USAGE-UNITS
               GIVING WS-UNIT-COST ROUNDED.
      *
       0300-TOTAL.
           COMPUTE WS-AMOUNT-DUE ROUNDED =
               WS-CURRENT-CHG + WS-NET-PRIOR + WS-SURCHARGE
               + WS-FRANCHISE-TAX
           MOVE WS-AMOUNT-DUE TO WS-EDT-DUE
           MOVE WS-NET-PRIOR TO WS-EDT-NET.
      *
       0400-BUILD-STUB.
      *    ---- TEXT ASSEMBLY.  NOT IN THE C1 SUBSET. -----------------
           STRING WS-IN-ACCOUNT DELIMITED BY SIZE
                  "  DUE " DELIMITED BY SIZE
                  WS-EDT-DUE DELIMITED BY SIZE
                  "  PRIOR " DELIMITED BY SIZE
                  WS-EDT-NET DELIMITED BY SIZE
               INTO WS-STUB-LINE
           END-STRING
           PERFORM 0500-PRINT-STUB.
      *
       0500-PRINT-STUB.
      *    ---- PARAGRAPH PERFORM.  ONLY PERFORM VARYING IS SUBSET. ---
           DISPLAY WS-STUB-LINE
           DISPLAY "ACCOUNT     " WS-IN-ACCOUNT
           DISPLAY "AMOUNT DUE  " WS-EDT-DUE
           MOVE 0 TO RETURN-CODE
           STOP RUN.
