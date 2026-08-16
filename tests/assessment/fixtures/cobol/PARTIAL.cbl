       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARTIAL.
      * Mix of supported and unsupported constructs.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-N      PIC 9(3).
       01 WS-PACKED PIC S9(5)V99 COMP-3.
       01 WS-NAME   PIC X(20).
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 1 TO WS-N.
           SUBTRACT 1 FROM WS-N.
           DISPLAY WS-N.
           CALL "SUBPGM" USING WS-N.
           STOP RUN.
       OTHER-PARA.
           GO TO MAIN-PARA.
