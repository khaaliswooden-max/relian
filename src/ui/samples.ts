// Sample COBOL-85 programs within the supported subset, for the demo.

export interface Sample {
    id: string;
    label: string;
    description: string;
    code: string;
}

const INTEREST_CALC = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEREST-CALC.
       AUTHOR. RELIAN-DEMO.
      *
      * Banking interest calculation. COMP-3 arithmetic with
      * explicit COBOL rounding semantics (COMPUTE = truncate/DOWN).
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PRINCIPAL       PIC 9(10)V99 VALUE ZEROS.
       01  WS-RATE            PIC 9(2)V9(4) VALUE ZEROS.
       01  WS-TIME            PIC 9(3) VALUE ZEROS.
       01  WS-INTEREST        PIC 9(12)V99 VALUE ZEROS.
       01  WS-AMOUNT          PIC 9(12)V99 VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "INTEREST CALCULATION PROGRAM".
           MOVE 10000.00 TO WS-PRINCIPAL.
           MOVE 5.25 TO WS-RATE.
           MOVE 12 TO WS-TIME.
           PERFORM CALCULATE-INTEREST.
           DISPLAY "PRINCIPAL: " WS-PRINCIPAL.
           DISPLAY "INTEREST: " WS-INTEREST.
           DISPLAY "TOTAL: " WS-AMOUNT.
           STOP RUN.

       CALCULATE-INTEREST.
           COMPUTE WS-AMOUNT ROUNDED = WS-PRINCIPAL *
               (1 + WS-RATE / 100) ** WS-TIME.
           SUBTRACT WS-PRINCIPAL FROM WS-AMOUNT
               GIVING WS-INTEREST.
`;

const GRADE_EVALUATE = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADE-BANDS.
      *
      * EVALUATE-driven branching over a numeric score.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SCORE   PIC 9(3) VALUE 82.
       01  WS-GRADE   PIC X(1) VALUE SPACE.

       PROCEDURE DIVISION.
       MAIN-PARA.
           EVALUATE TRUE
               WHEN WS-SCORE >= 90
                   MOVE "A" TO WS-GRADE
               WHEN WS-SCORE >= 80
                   MOVE "B" TO WS-GRADE
               WHEN WS-SCORE >= 70
                   MOVE "C" TO WS-GRADE
               WHEN OTHER
                   MOVE "F" TO WS-GRADE
           END-EVALUATE.
           DISPLAY "GRADE: " WS-GRADE.
           STOP RUN.
`;

const TABLE_SEARCH = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. RATE-TABLE.
      *
      * OCCURS table with a serial SEARCH.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RATES.
           05  WS-RATE-ENTRY OCCURS 5 TIMES INDEXED BY R-IDX.
               10  WS-TERM    PIC 9(2).
               10  WS-APR     PIC 9(2)V99.
       01  WS-WANT    PIC 9(2) VALUE 24.
       01  WS-FOUND   PIC 9(2)V99 VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 12 TO WS-TERM (1). MOVE 4.50 TO WS-APR (1).
           MOVE 24 TO WS-TERM (2). MOVE 5.25 TO WS-APR (2).
           MOVE 36 TO WS-TERM (3). MOVE 5.99 TO WS-APR (3).
           SET R-IDX TO 1.
           SEARCH WS-RATE-ENTRY
               AT END DISPLAY "NO RATE"
               WHEN WS-TERM (R-IDX) = WS-WANT
                   MOVE WS-APR (R-IDX) TO WS-FOUND
           END-SEARCH.
           DISPLAY "APR: " WS-FOUND.
           STOP RUN.
`;

// Deliberately out-of-scope: exercises the HONEST FAILURE path (embedded SQL /
// CICS are not in the supported subset). The pipeline should FAIL with a
// construct inventory rather than emit placeholder output.
const OUT_OF_SCOPE = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICS-INQUIRY.
      *
      * Uses CICS + embedded SQL — OUTSIDE the supported COBOL-85 subset.
      * Expect an honest FAILED migration, not fabricated output.
      *
       PROCEDURE DIVISION.
       MAIN-PARA.
           EXEC CICS RECEIVE MAP('INQMAP') END-EXEC.
           EXEC SQL
               SELECT BALANCE INTO :WS-BAL
               FROM ACCOUNTS WHERE ACCT_ID = :WS-ACCT
           END-EXEC.
           EXEC CICS SEND MAP('INQMAP') END-EXEC.
           STOP RUN.
`;

export const SAMPLES: Sample[] = [
    {
        id: 'interest',
        label: 'Interest calculation',
        description: 'COMP-3 arithmetic, COMPUTE ROUNDED',
        code: INTEREST_CALC,
    },
    {
        id: 'evaluate',
        label: 'Grade bands',
        description: 'EVALUATE TRUE branching',
        code: GRADE_EVALUATE,
    },
    {
        id: 'search',
        label: 'Rate table',
        description: 'OCCURS / SEARCH',
        code: TABLE_SEARCH,
    },
    {
        id: 'out-of-scope',
        label: 'Out of scope (CICS/SQL)',
        description: 'Triggers the honest-failure path',
        code: OUT_OF_SCOPE,
    },
];

export const DEFAULT_SAMPLE = SAMPLES[0];
