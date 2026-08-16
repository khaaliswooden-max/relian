       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANTLRFIT.
      * Written to fit the reduced Cobol85 grammar bundled in this repo,
      * so that the antlr_tree analysis path is exercised by the suite.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-N PIC 9(3).
       01 WS-M PIC 9(3).
       01 WS-NAME PIC X(20).
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 1 TO WS-N.
           ADD WS-N TO WS-M.
           IF WS-N GREATER WS-M
               DISPLAY WS-NAME
           ELSE
               DISPLAY WS-N
           END-IF.
           PERFORM OTHER-PARA.
           CALL "SUBPGM".
           GO TO LAST-PARA.
       OTHER-PARA.
           DISPLAY WS-M.
       LAST-PARA.
           STOP RUN.
