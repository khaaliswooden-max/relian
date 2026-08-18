       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEDGRPST.
      *****************************************************************
      * Ledger posting -- the NEARLY-TRANSPILABLE case.
      *
      * Valid COBOL-85; GnuCOBOL compiles and runs it. Every verb it
      * uses is inside the committed subset EXCEPT one: it PERFORMs a
      * named paragraph (PERFORM POST-LINE), the commonest control-flow
      * idiom in production COBOL.
      *
      * That single statement is why this program is here. 13 of its 14
      * statements are transpilable -- 0.9286 -- and Relian still
      * refuses the whole program, naming the verb and the line. There
      * is no partial migration, no "we did the 93% we could": a
      * migration that silently dropped one PERFORM would produce Java
      * that compiles, runs, and computes the wrong balance.
      *
      * This case previously documented two defects, both now fixed
      * (see PR #16):
      *
      *   1. The transpiler CRASHED here (AttributeError) instead of
      *      diagnosing, because PERFORM was registered as a bare verb
      *      while only the inline VARYING form had a handler.
      *   2. The assessment reported 1.0000 transpilable and was wrong,
      *      because it classifies by bare verb and read the same
      *      over-broad registration.
      *
      * Registering the qualified key "PERFORM VARYING" fixed both at
      * once. The demo still distinguishes a crash from a refusal
      * (verdict TRANSPILE_CRASHED) -- no shipped program triggers it
      * now, and a unit test exercises that path directly.
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
