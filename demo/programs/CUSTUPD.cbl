       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTUPD.
      *****************************************************************
      * Customer balance maintenance -- the HONEST-REFUSAL case.
      *
      * This is valid COBOL-85. GnuCOBOL compiles and runs it. It is
      * here precisely BECAUSE Relian's C1 transpiler cannot migrate
      * it: it uses SUBTRACT, MULTIPLY, DIVIDE, STRING, GO TO,
      * performed paragraphs and paragraph EXIT -- none of which are
      * in the committed COBOL-85 subset.
      *
      * The demo's point is what Relian does about that. The first
      * out-of-subset verb reached is SUBTRACT, in MAIN-PARA, and the
      * transpiler stops there: it names the verb, the line and the
      * paragraph, emits no Java, and issues no attestation. A tool
      * that produced plausible-looking output here would be the exact
      * failure mode this project exists to eliminate (rule R2 --
      * honest failure is a feature).
      *
      * Input  (stdin, CSV): BALANCE,PAYMENT,RATECODE
      * Output (stdout)    : STATUS=x BAL=y FEE=z
      *****************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RAW            PIC X(80).
       01  WS-F1             PIC X(20).
       01  WS-F2             PIC X(20).
       01  WS-F3             PIC X(20).
       01  WS-BALANCE        PIC S9(7)V99 COMP-3.
       01  WS-PAYMENT        PIC 9(7)V99 COMP-3.
       01  WS-RATECODE       PIC 9(2).
       01  WS-FEE            PIC 9(5)V99 COMP-3.
       01  WS-INTEREST       PIC 9(5)V99 COMP-3.
       01  WS-STATUS         PIC X(8).
       01  WS-MSG            PIC X(40).
       01  WS-O-BAL          PIC -(6)9.99.
       01  WS-O-FEE          PIC Z(4)9.99.
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-RAW FROM CONSOLE
           UNSTRING WS-RAW DELIMITED BY ","
               INTO WS-F1 WS-F2 WS-F3
           END-UNSTRING
           COMPUTE WS-BALANCE = FUNCTION NUMVAL(WS-F1)
           COMPUTE WS-PAYMENT = FUNCTION NUMVAL(WS-F2)
           COMPUTE WS-RATECODE = FUNCTION NUMVAL(WS-F3)

      *    SUBTRACT is a COBOL-85 arithmetic verb with no handler in
      *    the committed subset. This is the line the transpiler stops
      *    on, and the line the report names.
           SUBTRACT WS-PAYMENT FROM WS-BALANCE

           IF WS-BALANCE < 0
               MOVE "CREDIT" TO WS-STATUS
           ELSE
               MOVE "OPEN" TO WS-STATUS
           END-IF

           PERFORM ASSESS-FEE
           PERFORM FORMAT-LINE

           MOVE 0 TO RETURN-CODE
           STOP RUN.

      * MULTIPLY / DIVIDE: arithmetic verbs outside the subset.
       ASSESS-FEE.
           MULTIPLY WS-BALANCE BY 0.015 GIVING WS-INTEREST ROUNDED
           DIVIDE WS-INTEREST BY 12 GIVING WS-FEE ROUNDED
           IF WS-RATECODE > 10
               GO TO FEE-DOUBLE
           END-IF
           GO TO ASSESS-EXIT.
       FEE-DOUBLE.
           MULTIPLY WS-FEE BY 2 GIVING WS-FEE.
       ASSESS-EXIT.
           EXIT.

      * STRING: not in the subset either.
       FORMAT-LINE.
           MOVE WS-BALANCE TO WS-O-BAL
           MOVE WS-FEE TO WS-O-FEE
           STRING "STATUS=" DELIMITED BY SIZE
                  WS-STATUS DELIMITED BY SPACE
                  INTO WS-MSG
           END-STRING
           DISPLAY WS-MSG " BAL=" WS-O-BAL " FEE=" WS-O-FEE.
