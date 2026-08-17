      *================================================================*
      * MUBPOST  -  MERIDIAN MUNICIPAL UTILITY DISTRICT                *
      *             NIGHTLY CASH POSTING AND MASTER FILE UPDATE        *
      *                                                                *
      * SYNTHETIC DEMONSTRATION CODE.  NOT PRODUCTION.  NOT DERIVED    *
      * FROM ANY CUSTOMER OR BENCHMARK SOURCE.                         *
      *                                                                *
      * FUNCTION                                                       *
      *   MATCHES THE LOCKBOX PAYMENT FILE AGAINST THE INDEXED         *
      *   CUSTOMER MASTER, POSTS CASH, WRITES THE AUDIT TRAIL, AND     *
      *   SPILLS UNMATCHED ITEMS TO THE SUSPENSE FILE.                 *
      *                                                                *
      * TIER (RELIAN DEMO):  C - OUTSIDE THE C1 SUBSET.                *
      *   FILE HANDLING (SELECT / FD / OPEN / READ / WRITE / REWRITE / *
      *   CLOSE / START), UNSTRUCTURED FLOW (GO TO, GO TO DEPENDING,   *
      *   ALTER), STORAGE OVERLAYS (REDEFINES), AND VARIABLE TABLES    *
      *   (OCCURS DEPENDING ON) ARE ALL PRESENT.  THIS PROGRAM MUST    *
      *   PRODUCE A REFUSAL AND A CONSTRUCT INVENTORY, NOT JAVA.       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MUBPOST.
       AUTHOR. MERIDIAN-MUD-DP.
       DATE-WRITTEN. 1984-02-19.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-MASTER ASSIGN TO "CUSTMAST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CM-ACCOUNT-KEY
               FILE STATUS IS WS-MASTER-STATUS.
           SELECT PAYMENT-FILE ASSIGN TO "LOCKBOX"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PAY-STATUS.
           SELECT SUSPENSE-FILE ASSIGN TO "SUSPENSE"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-SUS-STATUS.
           SELECT AUDIT-FILE ASSIGN TO "AUDITRL"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-AUD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  CUSTOMER-MASTER.
       01  CUSTOMER-RECORD.
           05  CM-ACCOUNT-KEY          PIC X(10).
           05  CM-NAME                 PIC X(30).
           05  CM-CLASS                PIC X(01).
           05  CM-BALANCE              PIC S9(09)V99 COMP-3.
           05  CM-LAST-PAY-DATE        PIC 9(08).
           05  CM-CYCLE                PIC 9(02).
           05  CM-HIST-COUNT           PIC 9(02).
           05  CM-HIST-ENTRY OCCURS 1 TO 24 TIMES
                             DEPENDING ON CM-HIST-COUNT.
               10  CM-HIST-PERIOD      PIC 9(06).
               10  CM-HIST-AMOUNT      PIC S9(07)V99 COMP-3.
      *
       FD  PAYMENT-FILE.
       01  PAYMENT-RECORD.
           05  PR-ACCOUNT              PIC X(10).
           05  PR-AMOUNT               PIC S9(09)V99 COMP-3.
           05  PR-POST-DATE            PIC 9(08).
           05  PR-SOURCE               PIC X(02).
      *
       FD  SUSPENSE-FILE.
       01  SUSPENSE-RECORD             PIC X(80).
      *
       FD  AUDIT-FILE.
       01  AUDIT-RECORD                PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *
           COPY MUBCUST.
           COPY MUBCONS.
      *
       01  WS-MASTER-STATUS            PIC X(02).
           88  MASTER-OK               VALUE "00".
           88  MASTER-EOF              VALUE "10".
           88  MASTER-NOTFOUND         VALUE "23".
       01  WS-PAY-STATUS               PIC X(02).
       01  WS-SUS-STATUS               PIC X(02).
       01  WS-AUD-STATUS               PIC X(02).
      *
      *---------------------------------------------------------------*
      * THE POSTING DATE IS CARRIED BOTH AS A NUMBER AND AS THREE      *
      * SEPARATE FIELDS.  THE OVERLAY HAS BEEN HERE SINCE 1984 AND     *
      * EVERY DOWNSTREAM PROGRAM DEPENDS ON IT.                        *
      *---------------------------------------------------------------*
       01  WS-POST-DATE                PIC 9(08).
       01  WS-POST-DATE-R REDEFINES WS-POST-DATE.
           05  WS-PD-CENTURY           PIC 9(02).
           05  WS-PD-YEAR              PIC 9(02).
           05  WS-PD-MONTH             PIC 9(02).
           05  WS-PD-DAY               PIC 9(02).
      *
       01  WS-BRANCH-SWITCH            PIC 9(01) VALUE 1.
       01  WS-POSTED-COUNT             PIC 9(07) VALUE ZERO.
       01  WS-SUSPENSE-COUNT           PIC 9(07) VALUE ZERO.
       01  WS-POSTED-AMOUNT            PIC S9(11)V99 COMP-3.
       01  WS-EOF-FLAG                 PIC X(01) VALUE "N".
           88  END-OF-PAYMENTS         VALUE "Y".
      *
       PROCEDURE DIVISION.
      *
       0000-CONTROL.
           OPEN I-O    CUSTOMER-MASTER
           OPEN INPUT  PAYMENT-FILE
           OPEN OUTPUT SUSPENSE-FILE
           OPEN OUTPUT AUDIT-FILE
           IF WS-MASTER-STATUS NOT = "00"
               DISPLAY "E100 MASTER OPEN FAILED " WS-MASTER-STATUS
               MOVE 12 TO RETURN-CODE
               GO TO 9000-ABEND
           END-IF
           ALTER 0100-DISPATCH TO PROCEED TO 0200-READ-PAYMENT.
      *
       0100-DISPATCH.
           GO TO 0200-READ-PAYMENT.
      *
       0200-READ-PAYMENT.
           READ PAYMENT-FILE
               AT END
                   MOVE "Y" TO WS-EOF-FLAG
                   GO TO 8000-WRAP-UP
           END-READ
           MOVE PR-ACCOUNT TO CM-ACCOUNT-KEY
           START CUSTOMER-MASTER KEY IS EQUAL TO CM-ACCOUNT-KEY
               INVALID KEY
                   GO TO 0700-SUSPEND
           END-START
           READ CUSTOMER-MASTER
               INVALID KEY
                   GO TO 0700-SUSPEND
           END-READ
           GO TO WS-BRANCH-SWITCH
               DEPENDING ON 0300-POST-CASH 0400-POST-ADJUST.
      *
       0300-POST-CASH.
           SUBTRACT PR-AMOUNT FROM CM-BALANCE
           MOVE PR-POST-DATE TO CM-LAST-PAY-DATE
           ADD 1 TO CM-HIST-COUNT
           MOVE PR-POST-DATE TO CM-HIST-PERIOD(CM-HIST-COUNT)
           MOVE PR-AMOUNT TO CM-HIST-AMOUNT(CM-HIST-COUNT)
           REWRITE CUSTOMER-RECORD
               INVALID KEY
                   GO TO 0700-SUSPEND
           END-REWRITE
           ADD 1 TO WS-POSTED-COUNT
           ADD PR-AMOUNT TO WS-POSTED-AMOUNT
           GO TO 0600-WRITE-AUDIT.
      *
       0400-POST-ADJUST.
           ADD PR-AMOUNT TO CM-BALANCE
           REWRITE CUSTOMER-RECORD
           ADD 1 TO WS-POSTED-COUNT
           GO TO 0600-WRITE-AUDIT.
      *
       0600-WRITE-AUDIT.
           MOVE SPACES TO AUDIT-RECORD
           STRING PR-ACCOUNT DELIMITED BY SIZE
                  " POSTED " DELIMITED BY SIZE
                  PR-SOURCE DELIMITED BY SIZE
               INTO AUDIT-RECORD
           END-STRING
           WRITE AUDIT-RECORD
           GO TO 0100-DISPATCH.
      *
       0700-SUSPEND.
           MOVE SPACES TO SUSPENSE-RECORD
           MOVE PAYMENT-RECORD TO SUSPENSE-RECORD
           WRITE SUSPENSE-RECORD
           ADD 1 TO WS-SUSPENSE-COUNT
           GO TO 0100-DISPATCH.
      *
       8000-WRAP-UP.
           CLOSE CUSTOMER-MASTER
           CLOSE PAYMENT-FILE
           CLOSE SUSPENSE-FILE
           CLOSE AUDIT-FILE
           DISPLAY "POSTED    " WS-POSTED-COUNT
           DISPLAY "SUSPENDED " WS-SUSPENSE-COUNT
           MOVE 0 TO RETURN-CODE
           GOBACK.
      *
       9000-ABEND.
           DISPLAY "E999 ABNORMAL TERMINATION"
           MOVE 12 TO RETURN-CODE
           GOBACK.
