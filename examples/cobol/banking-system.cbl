       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING-SYSTEM.
       AUTHOR. RELIAN-DEMO.
       DATE-WRITTEN. 2025-01-01.
      *================================================================*
      * BANKING SYSTEM - Core Banking Operations Module
      * 
      * This program demonstrates typical COBOL patterns found in
      * legacy banking systems, including:
      *   - Account management
      *   - Interest calculations
      *   - Transaction processing
      *   - Batch operations
      *
      * Target Migration: Java (Spring Boot)
      * Industry Template: Banking & Financial Services
      *================================================================*
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCOUNTS"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NUMBER
               FILE STATUS IS WS-FILE-STATUS.
           SELECT TRANSACTION-FILE ASSIGN TO "TRANSACT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT REPORT-FILE ASSIGN TO "REPORTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
               
       DATA DIVISION.
       FILE SECTION.
       
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05  ACCT-NUMBER          PIC X(10).
           05  ACCT-NAME            PIC X(30).
           05  ACCT-TYPE            PIC X(1).
               88  CHECKING         VALUE "C".
               88  SAVINGS          VALUE "S".
               88  MONEY-MARKET     VALUE "M".
           05  ACCT-BALANCE         PIC S9(11)V99.
           05  ACCT-INTEREST-RATE   PIC 9(2)V9(4).
           05  ACCT-OPEN-DATE       PIC 9(8).
           05  ACCT-LAST-ACTIVITY   PIC 9(8).
           05  ACCT-STATUS          PIC X(1).
               88  ACTIVE           VALUE "A".
               88  FROZEN           VALUE "F".
               88  CLOSED           VALUE "C".
               
       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD.
           05  TXN-ACCOUNT          PIC X(10).
           05  TXN-TYPE             PIC X(1).
               88  DEPOSIT          VALUE "D".
               88  WITHDRAWAL       VALUE "W".
               88  TRANSFER         VALUE "T".
               88  INTEREST-POST    VALUE "I".
           05  TXN-AMOUNT           PIC S9(11)V99.
           05  TXN-DATE             PIC 9(8).
           05  TXN-TIME             PIC 9(6).
           05  TXN-REFERENCE        PIC X(20).
           
       FD  REPORT-FILE.
       01  REPORT-LINE             PIC X(132).
       
       WORKING-STORAGE SECTION.
       
      * File status and control fields
       01  WS-FILE-STATUS          PIC XX.
       01  WS-EOF-FLAG             PIC X VALUE "N".
           88  END-OF-FILE         VALUE "Y".
           88  NOT-END-OF-FILE     VALUE "N".
           
      * Date and time fields
       01  WS-CURRENT-DATE.
           05  WS-YEAR             PIC 9(4).
           05  WS-MONTH            PIC 9(2).
           05  WS-DAY              PIC 9(2).
       01  WS-FORMATTED-DATE       PIC 9(8).
       
      * Calculation working fields
       01  WS-CALC-FIELDS.
           05  WS-PRINCIPAL        PIC S9(11)V99.
           05  WS-RATE             PIC 9(2)V9(4).
           05  WS-TIME-PERIOD      PIC 9(3).
           05  WS-INTEREST-AMT     PIC S9(11)V99.
           05  WS-NEW-BALANCE      PIC S9(11)V99.
           05  WS-DAILY-RATE       PIC 9V9(8).
           
      * Transaction counters
       01  WS-COUNTERS.
           05  WS-TXN-COUNT        PIC 9(7) VALUE ZEROS.
           05  WS-DEPOSIT-COUNT    PIC 9(7) VALUE ZEROS.
           05  WS-WITHDRAW-COUNT   PIC 9(7) VALUE ZEROS.
           05  WS-TRANSFER-COUNT   PIC 9(7) VALUE ZEROS.
           05  WS-ERROR-COUNT      PIC 9(5) VALUE ZEROS.
           
      * Totals
       01  WS-TOTALS.
           05  WS-TOTAL-DEPOSITS   PIC S9(13)V99 VALUE ZEROS.
           05  WS-TOTAL-WITHDRAWS  PIC S9(13)V99 VALUE ZEROS.
           05  WS-TOTAL-INTEREST   PIC S9(13)V99 VALUE ZEROS.
           
      * Business rule constants
       01  WS-BUSINESS-RULES.
           05  WS-MIN-BALANCE      PIC S9(11)V99 VALUE 100.00.
           05  WS-OVERDRAFT-FEE    PIC S9(5)V99 VALUE 35.00.
           05  WS-DAILY-LIMIT      PIC S9(11)V99 VALUE 5000.00.
           05  WS-SENIOR-BONUS-RATE PIC 9V9(4) VALUE 0.0050.
           
      * Error handling
       01  WS-ERROR-MESSAGE        PIC X(80).
       01  WS-RETURN-CODE          PIC S9(4) COMP VALUE ZEROS.
       
       PROCEDURE DIVISION.
       
      *================================================================*
       MAIN-PROCEDURE.
      *================================================================*
           PERFORM INITIALIZATION
           PERFORM PROCESS-TRANSACTIONS 
               UNTIL END-OF-FILE
           PERFORM CALCULATE-DAILY-INTEREST
           PERFORM GENERATE-REPORTS
           PERFORM TERMINATION
           STOP RUN.
           
      *================================================================*
       INITIALIZATION.
      *================================================================*
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           STRING WS-YEAR WS-MONTH WS-DAY 
               DELIMITED BY SIZE INTO WS-FORMATTED-DATE
           
           OPEN INPUT TRANSACTION-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE "ERROR OPENING TRANSACTION FILE" 
                   TO WS-ERROR-MESSAGE
               PERFORM ERROR-HANDLER
           END-IF
           
           OPEN I-O ACCOUNT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE "ERROR OPENING ACCOUNT FILE" 
                   TO WS-ERROR-MESSAGE
               PERFORM ERROR-HANDLER
           END-IF
           
           OPEN OUTPUT REPORT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE "ERROR OPENING REPORT FILE" 
                   TO WS-ERROR-MESSAGE
               PERFORM ERROR-HANDLER
           END-IF
           
           DISPLAY "BANKING SYSTEM INITIALIZED: " WS-FORMATTED-DATE.
           
      *================================================================*
       PROCESS-TRANSACTIONS.
      *================================================================*
           READ TRANSACTION-FILE
               AT END SET END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO WS-TXN-COUNT
                   EVALUATE TRUE
                       WHEN DEPOSIT
                           PERFORM PROCESS-DEPOSIT
                       WHEN WITHDRAWAL
                           PERFORM PROCESS-WITHDRAWAL
                       WHEN TRANSFER
                           PERFORM PROCESS-TRANSFER
                       WHEN OTHER
                           ADD 1 TO WS-ERROR-COUNT
                           MOVE "INVALID TRANSACTION TYPE"
                               TO WS-ERROR-MESSAGE
                           PERFORM LOG-ERROR
                   END-EVALUATE
           END-READ.
           
      *================================================================*
       PROCESS-DEPOSIT.
      *================================================================*
      * Business Rule: Deposits credited immediately
      * Audit: Full transaction logging required
      *================================================================*
           MOVE TXN-ACCOUNT TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   ADD 1 TO WS-ERROR-COUNT
                   MOVE "ACCOUNT NOT FOUND FOR DEPOSIT"
                       TO WS-ERROR-MESSAGE
                   PERFORM LOG-ERROR
               NOT INVALID KEY
                   IF ACTIVE
                       ADD TXN-AMOUNT TO ACCT-BALANCE
                       MOVE WS-FORMATTED-DATE TO ACCT-LAST-ACTIVITY
                       REWRITE ACCOUNT-RECORD
                       ADD 1 TO WS-DEPOSIT-COUNT
                       ADD TXN-AMOUNT TO WS-TOTAL-DEPOSITS
                   ELSE
                       ADD 1 TO WS-ERROR-COUNT
                       MOVE "DEPOSIT TO INACTIVE ACCOUNT"
                           TO WS-ERROR-MESSAGE
                       PERFORM LOG-ERROR
                   END-IF
           END-READ.
           
      *================================================================*
       PROCESS-WITHDRAWAL.
      *================================================================*
      * Business Rule: Check sufficient funds
      * Business Rule: Daily withdrawal limit applies
      * Business Rule: Overdraft fee if below minimum balance
      *================================================================*
           MOVE TXN-ACCOUNT TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   ADD 1 TO WS-ERROR-COUNT
                   MOVE "ACCOUNT NOT FOUND FOR WITHDRAWAL"
                       TO WS-ERROR-MESSAGE
                   PERFORM LOG-ERROR
               NOT INVALID KEY
                   IF ACTIVE
                       IF TXN-AMOUNT > WS-DAILY-LIMIT
                           ADD 1 TO WS-ERROR-COUNT
                           MOVE "EXCEEDS DAILY WITHDRAWAL LIMIT"
                               TO WS-ERROR-MESSAGE
                           PERFORM LOG-ERROR
                       ELSE
                           IF TXN-AMOUNT > ACCT-BALANCE
                               ADD 1 TO WS-ERROR-COUNT
                               MOVE "INSUFFICIENT FUNDS"
                                   TO WS-ERROR-MESSAGE
                               PERFORM LOG-ERROR
                           ELSE
                               SUBTRACT TXN-AMOUNT FROM ACCT-BALANCE
                               IF ACCT-BALANCE < WS-MIN-BALANCE
                                   SUBTRACT WS-OVERDRAFT-FEE 
                                       FROM ACCT-BALANCE
                                   DISPLAY "OVERDRAFT FEE APPLIED: "
                                       ACCT-NUMBER
                               END-IF
                               MOVE WS-FORMATTED-DATE 
                                   TO ACCT-LAST-ACTIVITY
                               REWRITE ACCOUNT-RECORD
                               ADD 1 TO WS-WITHDRAW-COUNT
                               ADD TXN-AMOUNT TO WS-TOTAL-WITHDRAWS
                           END-IF
                       END-IF
                   ELSE
                       ADD 1 TO WS-ERROR-COUNT
                       MOVE "WITHDRAWAL FROM INACTIVE ACCOUNT"
                           TO WS-ERROR-MESSAGE
                       PERFORM LOG-ERROR
                   END-IF
           END-READ.
           
      *================================================================*
       PROCESS-TRANSFER.
      *================================================================*
      * Business Rule: Both accounts must be active
      * Business Rule: Same day settlement
      *================================================================*
           ADD 1 TO WS-TRANSFER-COUNT
           DISPLAY "TRANSFER PROCESSING: " TXN-REFERENCE.
           
      *================================================================*
       CALCULATE-DAILY-INTEREST.
      *================================================================*
      * Business Rule: Savings accounts earn daily compound interest
      * Business Rule: Senior citizens (65+) get bonus rate
      * Formula: Interest = Principal * (Rate/365)
      *================================================================*
           MOVE LOW-VALUES TO ACCT-NUMBER
           START ACCOUNT-FILE KEY > ACCT-NUMBER
           
           PERFORM UNTIL WS-FILE-STATUS NOT = "00"
               READ ACCOUNT-FILE NEXT
                   AT END EXIT PERFORM
                   NOT AT END
                       IF SAVINGS OR MONEY-MARKET
                           IF ACTIVE
                               MOVE ACCT-BALANCE TO WS-PRINCIPAL
                               MOVE ACCT-INTEREST-RATE TO WS-RATE
                               COMPUTE WS-DAILY-RATE = 
                                   WS-RATE / 365
                               COMPUTE WS-INTEREST-AMT ROUNDED =
                                   WS-PRINCIPAL * WS-DAILY-RATE
                               ADD WS-INTEREST-AMT TO ACCT-BALANCE
                               ADD WS-INTEREST-AMT TO WS-TOTAL-INTEREST
                               REWRITE ACCOUNT-RECORD
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.
           
      *================================================================*
       GENERATE-REPORTS.
      *================================================================*
           MOVE SPACES TO REPORT-LINE
           STRING "DAILY BANKING REPORT - " WS-FORMATTED-DATE
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE SPACES TO REPORT-LINE
           STRING "TRANSACTIONS PROCESSED: " WS-TXN-COUNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE SPACES TO REPORT-LINE
           STRING "DEPOSITS: " WS-DEPOSIT-COUNT 
               " TOTAL: $" WS-TOTAL-DEPOSITS
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE SPACES TO REPORT-LINE
           STRING "WITHDRAWALS: " WS-WITHDRAW-COUNT
               " TOTAL: $" WS-TOTAL-WITHDRAWS
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE SPACES TO REPORT-LINE
           STRING "INTEREST POSTED: $" WS-TOTAL-INTEREST
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE SPACES TO REPORT-LINE
           STRING "ERRORS: " WS-ERROR-COUNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE.
           
      *================================================================*
       LOG-ERROR.
      *================================================================*
           DISPLAY "ERROR: " WS-ERROR-MESSAGE.
           
      *================================================================*
       ERROR-HANDLER.
      *================================================================*
           DISPLAY "FATAL ERROR: " WS-ERROR-MESSAGE
           DISPLAY "FILE STATUS: " WS-FILE-STATUS
           MOVE 16 TO WS-RETURN-CODE
           STOP RUN.
           
      *================================================================*
       TERMINATION.
      *================================================================*
           CLOSE TRANSACTION-FILE
                 ACCOUNT-FILE
                 REPORT-FILE
           DISPLAY "BANKING SYSTEM TERMINATED NORMALLY"
           DISPLAY "RETURN CODE: " WS-RETURN-CODE.

