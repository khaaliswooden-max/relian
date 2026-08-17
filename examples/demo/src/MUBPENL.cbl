      *================================================================*
      * MUBPENL  -  MERIDIAN MUNICIPAL UTILITY DISTRICT                *
      *             DELINQUENCY PENALTY AND INTEREST ASSESSMENT        *
      *                                                                *
      * SYNTHETIC DEMONSTRATION CODE.  NOT PRODUCTION.  NOT DERIVED    *
      * FROM ANY CUSTOMER OR BENCHMARK SOURCE.                         *
      *                                                                *
      * FUNCTION                                                       *
      *   ASSESSES THE ORDINANCE PENALTY PERCENTAGE FOR THE ACCOUNT    *
      *   STATUS, ACCRUES SIMPLE DAILY INTEREST ON THE OPEN BALANCE,   *
      *   AND DETERMINES DEFERRED PAYMENT PLAN ELIGIBILITY.            *
      *                                                                *
      * NOTE ON ROUNDING                                               *
      *   THE ASSESSED INTEREST IS ROUNDED (HALF UP) BUT THE SHADOW    *
      *   FIGURE CARRIED FOR THE GENERAL LEDGER RECONCILIATION IS AN   *
      *   UNROUNDED COMPUTE AND THEREFORE TRUNCATES TOWARD ZERO.  THE  *
      *   TWO ARE DELIBERATELY DIFFERENT AND HAVE BEEN SINCE THE 1991  *
      *   AUDIT FINDING.                                               *
      *                                                                *
      * TIER (RELIAN DEMO):  A - WHOLLY INSIDE THE C1 SUBSET           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MUBPENL.
       AUTHOR. MERIDIAN-MUD-DP.
       DATE-WRITTEN. 1991-09-30.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-DISTRICT-NAME    PIC X(24) VALUE "MERIDIAN MUD".
      *
       01  WS-INPUT-LINE               PIC X(80).
       01  WS-PENL-REC.
           05  WS-IN-ACCOUNT           PIC X(10).
           05  WS-IN-STATUS            PIC X(01).
           05  WS-IN-DAYS              PIC X(04).
           05  WS-IN-BALANCE           PIC X(11).
           05  WS-IN-PAYCNT            PIC X(02).
           05  WS-IN-NAME              PIC X(30).
      *
      *---------------------------------------------------------------*
      * COLLECTION STATUS.  W AND U BOTH MEAN WRITTEN OFF - U WAS THE  *
      * CODE USED BEFORE THE 1994 UNIFORM CHART OF ACCOUNTS AND WAS    *
      * NEVER CONVERTED IN THE HISTORY FILE.                           *
      *---------------------------------------------------------------*
       01  WS-STATUS-CODE              PIC X(01).
           88  STAT-CURRENT            VALUE "C".
           88  STAT-ARREARS            VALUE "A".
           88  STAT-FINAL              VALUE "F".
           88  STAT-LIEN               VALUE "L".
           88  STAT-WRITTEN-OFF        VALUE "W" "U".
      *
      *---------------------------------------------------------------*
      * ASSESSMENT WORK AREA.                                          *
      *---------------------------------------------------------------*
       01  WS-BALANCE                  PIC S9(09)V99 COMP-3.
       01  WS-DAYS-LATE                PIC S9(04)    COMP-3.
       01  WS-PAY-COUNT                PIC 9(02).
       01  WS-NAME-WIDTH               PIC 9(03) VALUE ZERO.
       01  WS-PENALTY-PCT              PIC S9(01)V9(04) COMP-3.
       01  WS-PENALTY                  PIC S9(07)V99 COMP-3.
       01  WS-ANNUAL-PCT   PIC S9V9(04) COMP-3 VALUE 0.1800.
       01  WS-DAILY-RATE               PIC S9(01)V9(06) COMP-3.
       01  WS-INTEREST-RND             PIC S9(07)V99 COMP-3.
       01  WS-INTEREST-GL              PIC S9(07)V99 COMP-3.
       01  WS-PLAN-MONTHS              PIC 9(02) VALUE ZERO.
       01  WS-PLAN-AMT                 PIC S9(07)V99 COMP-3.
       01  WS-TOTAL-DUE                PIC S9(09)V99 COMP-3.
      *
       01  WS-EDT-BALANCE              PIC ZZZZZZZZ9.99.
       01  WS-EDT-PENALTY              PIC ZZZZZZZZ9.99.
       01  WS-EDT-INT-RND              PIC ZZZZZZZZ9.99.
       01  WS-EDT-INT-GL               PIC ZZZZZZZZ9.99.
       01  WS-EDT-TOTAL                PIC ZZZZZZZZ9.99.
       01  WS-EDT-PLAN                 PIC ZZZZZZZZ9.99.
      *
       PROCEDURE DIVISION.
      *
       0000-MAIN.
           DISPLAY "MUBPENL   DELINQUENCY ASSESSMENT FOR "
                   WS-DISTRICT-NAME
           ACCEPT WS-INPUT-LINE
           UNSTRING WS-INPUT-LINE DELIMITED BY ","
               INTO WS-IN-ACCOUNT WS-IN-STATUS WS-IN-DAYS
                    WS-IN-BALANCE WS-IN-PAYCNT WS-IN-NAME
           END-UNSTRING
           MOVE WS-IN-STATUS TO WS-STATUS-CODE
           COMPUTE WS-NAME-WIDTH = FUNCTION LENGTH(WS-DISTRICT-NAME)
           COMPUTE WS-BALANCE = FUNCTION NUMVAL(WS-IN-BALANCE)
           COMPUTE WS-DAYS-LATE = FUNCTION NUMVAL(WS-IN-DAYS)
           COMPUTE WS-PAY-COUNT = FUNCTION NUMVAL(WS-IN-PAYCNT).
      *
       0100-SCREEN-ACCOUNT.
           IF STAT-WRITTEN-OFF
               DISPLAY "I010 ACCOUNT WRITTEN OFF - NOT ASSESSED"
               MOVE 4 TO RETURN-CODE
               GOBACK
           END-IF
           IF WS-BALANCE <= 0
               DISPLAY "I011 NO OPEN BALANCE - NOTHING TO ASSESS"
               MOVE 0 TO RETURN-CODE
               GOBACK
           END-IF
           IF WS-DAYS-LATE < 30
               DISPLAY "I012 INSIDE ORDINANCE GRACE PERIOD"
               MOVE 0 TO RETURN-CODE
               GOBACK
           END-IF.
      *
       0200-PENALTY-RATE.
           EVALUATE WS-STATUS-CODE
               WHEN "C"
                   MOVE 0.0000 TO WS-PENALTY-PCT
               WHEN "A"
                   MOVE 0.0150 TO WS-PENALTY-PCT
               WHEN "F"
                   MOVE 0.0500 TO WS-PENALTY-PCT
               WHEN "L"
                   MOVE 0.1000 TO WS-PENALTY-PCT
               WHEN OTHER
                   MOVE 0.0150 TO WS-PENALTY-PCT
           END-EVALUATE
           COMPUTE WS-PENALTY ROUNDED =
               WS-BALANCE * WS-PENALTY-PCT.
      *
       0300-INTEREST-ACCRUAL.
           COMPUTE WS-DAILY-RATE = WS-ANNUAL-PCT / 365
           COMPUTE WS-INTEREST-RND ROUNDED =
               WS-BALANCE * WS-DAILY-RATE * WS-DAYS-LATE
           COMPUTE WS-INTEREST-GL =
               WS-BALANCE * WS-DAILY-RATE * WS-DAYS-LATE.
      *
       0400-PAYMENT-PLAN.
           COMPUTE WS-TOTAL-DUE ROUNDED =
               WS-BALANCE + WS-PENALTY + WS-INTEREST-RND
           IF WS-BALANCE > 250.00 AND WS-PAY-COUNT >= 6
               MOVE 12 TO WS-PLAN-MONTHS
           ELSE
               IF WS-BALANCE > 100.00 AND WS-PAY-COUNT >= 3
                   MOVE 6 TO WS-PLAN-MONTHS
               ELSE
                   MOVE 0 TO WS-PLAN-MONTHS
               END-IF
           END-IF
           IF WS-PLAN-MONTHS > 0
               COMPUTE WS-PLAN-AMT ROUNDED =
                   WS-TOTAL-DUE / WS-PLAN-MONTHS
           ELSE
               MOVE ZERO TO WS-PLAN-AMT
           END-IF.
      *
       0500-EDIT-REPORT.
           MOVE WS-BALANCE      TO WS-EDT-BALANCE
           MOVE WS-PENALTY      TO WS-EDT-PENALTY
           MOVE WS-INTEREST-RND TO WS-EDT-INT-RND
           MOVE WS-INTEREST-GL  TO WS-EDT-INT-GL
           MOVE WS-TOTAL-DUE    TO WS-EDT-TOTAL
           MOVE WS-PLAN-AMT     TO WS-EDT-PLAN
           DISPLAY "ACCOUNT     " WS-IN-ACCOUNT
           DISPLAY "CUSTOMER    " FUNCTION TRIM(WS-IN-NAME)
           DISPLAY "STATUS      " WS-STATUS-CODE
           DISPLAY "DAYS LATE   " WS-DAYS-LATE
           DISPLAY "BALANCE     " WS-EDT-BALANCE
           DISPLAY "PENALTY     " WS-EDT-PENALTY
           DISPLAY "INTEREST    " WS-EDT-INT-RND
           DISPLAY "INT GL MEMO " WS-EDT-INT-GL
           DISPLAY "TOTAL DUE   " WS-EDT-TOTAL
           DISPLAY "PLAN MONTHS " WS-PLAN-MONTHS
           DISPLAY "PLAN AMOUNT " WS-EDT-PLAN
           DISPLAY "NAME WIDTH  " WS-NAME-WIDTH.
      *
       0600-SET-RETURN.
           IF WS-DAYS-LATE >= 90 AND WS-PLAN-MONTHS = 0
               DISPLAY "W013 REFER TO COLLECTIONS"
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           IF WS-STATUS-CODE NOT = "C"
               DISPLAY "I014 ACCOUNT STATUS IS NOT CURRENT"
           END-IF
           STOP RUN.
