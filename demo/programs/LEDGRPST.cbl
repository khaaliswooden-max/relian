       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEDGRPST.
      *****************************************************************
      * Ledger posting -- the ROUGH-EDGE case, included deliberately.
      *
      * Valid COBOL-85; GnuCOBOL compiles and runs it. Every verb it
      * uses is inside the committed subset EXCEPT one: it PERFORMs a
      * named paragraph (PERFORM POST-LINE), which the C1 transpiler
      * does not support.
      *
      * The designed behavior for an unsupported construct is a clean
      * UnsupportedConstruct naming the verb, line and paragraph --
      * which is what CUSTUPD.cbl gets. This program does NOT get that:
      * PERFORM has a handler, and that handler assumes the inline
      * PERFORM ... UNTIL / VARYING forms, so a paragraph-name operand
      * makes its regex return None and the transpile dies with an
      * unhandled AttributeError instead of a diagnosis.
      *
      * The outcome is still safe -- no Java is emitted and nothing is
      * attested. But two things go wrong on the way there, and the
      * demo names both rather than dressing them up:
      *
      *   1. A crash is not a refusal. The verdict is
      *      TRANSPILE_CRASHED, not REFUSED_UNSUPPORTED.
      *   2. The read-only assessment does NOT catch this in advance.
      *      It classifies statements by bare verb, and PERFORM is in
      *      the dispatch table, so it reports 1.0000 transpilable and
      *      is wrong. Coverage over-reports wherever only some forms
      *      of a verb are supported.
      *
      * Input  (stdin, CSV): AMOUNT,ACCOUNT,PERIOD
      * Output (stdout)    : ACCT=x PERIOD=y POSTED=z
      *****************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RAW            PIC X(80).
       01  WS-F1             PIC X(20).
       01  WS-F2             PIC X(20).
       01  WS-F3             PIC X(20).
       01  WS-AMOUNT         PIC 9(7)V99 COMP-3.
       01  WS-ACCOUNT        PIC 9(6).
       01  WS-PERIOD         PIC 9(2).
       01  WS-POSTED         PIC 9(7)V99 COMP-3.
       01  WS-O-ACCT         PIC 9(6).
       01  WS-O-POSTED       PIC Z(6)9.99.
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-RAW FROM CONSOLE
           UNSTRING WS-RAW DELIMITED BY ","
               INTO WS-F1 WS-F2 WS-F3
           END-UNSTRING
           COMPUTE WS-AMOUNT  = FUNCTION NUMVAL(WS-F1)
           COMPUTE WS-ACCOUNT = FUNCTION NUMVAL(WS-F2)
           COMPUTE WS-PERIOD  = FUNCTION NUMVAL(WS-F3)

      *    Every verb above is inside the subset. This one is not:
      *    PERFORM of a named paragraph, the commonest control-flow
      *    idiom in production COBOL.
           PERFORM POST-LINE

           MOVE WS-ACCOUNT TO WS-O-ACCT
           MOVE WS-POSTED TO WS-O-POSTED
           DISPLAY "ACCT=" WS-O-ACCT " PERIOD=" WS-PERIOD
                   " POSTED=" WS-O-POSTED
           MOVE 0 TO RETURN-CODE
           STOP RUN.

       POST-LINE.
           IF WS-PERIOD > 12
               MOVE 0 TO WS-POSTED
           ELSE
               COMPUTE WS-POSTED = WS-AMOUNT * 1.00
           END-IF.
