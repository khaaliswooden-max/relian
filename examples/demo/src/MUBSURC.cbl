      *================================================================*
      * MUBSURC  -  MERIDIAN MUNICIPAL UTILITY DISTRICT                *
      *             CONSERVATION SURCHARGE AND EXEMPTION ENGINE        *
      *                                                                *
      * SYNTHETIC DEMONSTRATION CODE.  NOT PRODUCTION.  NOT DERIVED    *
      * FROM ANY CUSTOMER OR BENCHMARK SOURCE.                         *
      *                                                                *
      * FUNCTION                                                       *
      *   TAKES TWELVE MONTHS OF CONSUMPTION HISTORY, DERIVES THE      *
      *   ROLLING AVERAGE AND THE PEAK MONTH, PLACES THE PREMISE IN A  *
      *   CONSERVATION SURCHARGE BAND, THEN LOOKS THE PREMISE UP IN    *
      *   THE STATUTORY EXEMPTION TABLE AND WAIVES THE SURCHARGE IF    *
      *   THE EXEMPTION IS ON FILE.                                    *
      *                                                                *
      *   CALLED AS A MODULE FROM MUBBILL.  ENDS WITH EXIT PROGRAM.    *
      *                                                                *
      * TIER (RELIAN DEMO):  A - WHOLLY INSIDE THE C1 SUBSET           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MUBSURC.
       AUTHOR. MERIDIAN-MUD-DP.
       DATE-WRITTEN. 2003-06-02.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-INPUT-LINE               PIC X(80).
       01  WS-HIST-REC.
           05  WS-IN-PREMISE           PIC X(10).
           05  WS-IN-EXEMPT            PIC X(04).
           05  WS-IN-M01               PIC X(07).
           05  WS-IN-M02               PIC X(07).
           05  WS-IN-M03               PIC X(07).
           05  WS-IN-M04               PIC X(07).
           05  WS-IN-M05               PIC X(07).
           05  WS-IN-M06               PIC X(07).
           05  WS-IN-M07               PIC X(07).
           05  WS-IN-M08               PIC X(07).
           05  WS-IN-M09               PIC X(07).
           05  WS-IN-M10               PIC X(07).
           05  WS-IN-M11               PIC X(07).
           05  WS-IN-M12               PIC X(07).
      *
      *---------------------------------------------------------------*
      * TWELVE MONTH ROLLING HISTORY AND THE STATUTORY EXEMPTION       *
      * TABLE.  BOTH ARE TWELVE ENTRIES - THE EXEMPTION TABLE WAS      *
      * SIZED TO MATCH THE HISTORY TABLE IN 1996 SO THAT THE SAME      *
      * INDEX REGISTER COULD DRIVE BOTH.                               *
      *---------------------------------------------------------------*
       01  WS-HISTORY-TABLE.
           05  WS-HIST-USAGE   PIC 9(07) OCCURS 12 INDEXED BY BI.
       01  WS-EXEMPT-TABLE.
           05  WS-EXMPT-CODE   PIC 9(04) OCCURS 12.
      *
       01  WS-EXEMPT-IN                PIC 9(04) VALUE ZERO.
       01  WS-EXEMPT-HIT               PIC 9(02) VALUE ZERO.
       01  WS-MON-IX                   PIC 9(02).
       01  WS-HIST-TOTAL               PIC 9(09) VALUE ZERO.
       01  WS-PEAK-USAGE               PIC 9(07) VALUE ZERO.
       01  WS-PEAK-MONTH               PIC 9(02) VALUE ZERO.
       01  WS-AVG-USAGE                PIC 9(07)V99 COMP-3.
       01  WS-CONS-PCT                 PIC S9V9(04) COMP-3.
       01  WS-BASE-AMOUNT              PIC S9(07)V99 COMP-3.
       01  WS-SURCHARGE                PIC S9(07)V99 COMP-3.
       01  WS-BLANK-COUNT              PIC 9(03) VALUE ZERO.
      *
       01  WS-BAND-NAME                PIC X(14).
      *
       01  WS-EDT-AVG                  PIC ZZZZZZ9.99.
       01  WS-EDT-PEAK                 PIC ZZZZZZ9.99.
       01  WS-EDT-SUR                  PIC ZZZZZZ9.99.
      *
       PROCEDURE DIVISION.
      *
       0000-MAIN.
           DISPLAY "MUBSURC   CONSERVATION SURCHARGE ENGINE"
           ACCEPT WS-INPUT-LINE
           UNSTRING WS-INPUT-LINE DELIMITED BY ","
               INTO WS-IN-PREMISE WS-IN-EXEMPT
                    WS-IN-M01 WS-IN-M02 WS-IN-M03 WS-IN-M04
                    WS-IN-M05 WS-IN-M06 WS-IN-M07 WS-IN-M08
                    WS-IN-M09 WS-IN-M10 WS-IN-M11 WS-IN-M12
           END-UNSTRING
           INSPECT WS-IN-PREMISE TALLYING WS-BLANK-COUNT FOR ALL " "
           IF WS-BLANK-COUNT > 4
               DISPLAY "E020 PREMISE KEY SHORT - RECORD REJECTED"
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
      *
       1000-LOAD-HISTORY.
           COMPUTE WS-HIST-USAGE(1)  = FUNCTION NUMVAL(WS-IN-M01)
           COMPUTE WS-HIST-USAGE(2)  = FUNCTION NUMVAL(WS-IN-M02)
           COMPUTE WS-HIST-USAGE(3)  = FUNCTION NUMVAL(WS-IN-M03)
           COMPUTE WS-HIST-USAGE(4)  = FUNCTION NUMVAL(WS-IN-M04)
           COMPUTE WS-HIST-USAGE(5)  = FUNCTION NUMVAL(WS-IN-M05)
           COMPUTE WS-HIST-USAGE(6)  = FUNCTION NUMVAL(WS-IN-M06)
           COMPUTE WS-HIST-USAGE(7)  = FUNCTION NUMVAL(WS-IN-M07)
           COMPUTE WS-HIST-USAGE(8)  = FUNCTION NUMVAL(WS-IN-M08)
           COMPUTE WS-HIST-USAGE(9)  = FUNCTION NUMVAL(WS-IN-M09)
           COMPUTE WS-HIST-USAGE(10) = FUNCTION NUMVAL(WS-IN-M10)
           COMPUTE WS-HIST-USAGE(11) = FUNCTION NUMVAL(WS-IN-M11)
           COMPUTE WS-HIST-USAGE(12) = FUNCTION NUMVAL(WS-IN-M12)
           COMPUTE WS-EXEMPT-IN = FUNCTION NUMVAL(WS-IN-EXEMPT).
      *
       1100-LOAD-EXEMPT-TABLE.
           MOVE 0100 TO WS-EXMPT-CODE(1)
           MOVE 0110 TO WS-EXMPT-CODE(2)
           MOVE 0125 TO WS-EXMPT-CODE(3)
           MOVE 0140 TO WS-EXMPT-CODE(4)
           MOVE 0155 TO WS-EXMPT-CODE(5)
           MOVE 0200 TO WS-EXMPT-CODE(6)
           MOVE 0215 TO WS-EXMPT-CODE(7)
           MOVE 0230 TO WS-EXMPT-CODE(8)
           MOVE 0245 TO WS-EXMPT-CODE(9)
           MOVE 0300 TO WS-EXMPT-CODE(10)
           MOVE 0315 TO WS-EXMPT-CODE(11)
           MOVE 9999 TO WS-EXMPT-CODE(12).
      *
       2000-SUMMARISE-HISTORY.
           PERFORM VARYING WS-MON-IX FROM 1 BY 1
                   UNTIL WS-MON-IX > 12
               COMPUTE WS-HIST-TOTAL =
                   WS-HIST-TOTAL + WS-HIST-USAGE(WS-MON-IX)
               IF WS-HIST-USAGE(WS-MON-IX) > WS-PEAK-USAGE
                   COMPUTE WS-PEAK-USAGE = WS-HIST-USAGE(WS-MON-IX)
                   COMPUTE WS-PEAK-MONTH = WS-MON-IX
               END-IF
           END-PERFORM
           COMPUTE WS-AVG-USAGE ROUNDED = WS-HIST-TOTAL / 12.
      *
       3000-BAND-PREMISE.
           EVALUATE TRUE
               WHEN WS-PEAK-USAGE > 40000 AND WS-AVG-USAGE > 20000
                   MOVE 0.2500 TO WS-CONS-PCT
                   MOVE "BAND-4 SEVERE" TO WS-BAND-NAME
               WHEN WS-PEAK-USAGE > 25000 AND WS-AVG-USAGE > 12000
                   MOVE 0.1500 TO WS-CONS-PCT
                   MOVE "BAND-3 HIGH" TO WS-BAND-NAME
               WHEN WS-AVG-USAGE > 8000
                   MOVE 0.0750 TO WS-CONS-PCT
                   MOVE "BAND-2 WATCH" TO WS-BAND-NAME
               WHEN OTHER
                   MOVE 0.0000 TO WS-CONS-PCT
                   MOVE "BAND-1 NONE" TO WS-BAND-NAME
           END-EVALUATE.
      *
       4000-EXEMPTION-LOOKUP.
           SET BI TO 1
           SEARCH WS-EXMPT-CODE
               AT END DISPLAY "I021 NO STATUTORY EXEMPTION ON FILE"
               WHEN WS-EXMPT-CODE(BI) = WS-EXEMPT-IN
                   SET WS-EXEMPT-HIT TO BI
           END-SEARCH.
      *
       5000-ASSESS-SURCHARGE.
           COMPUTE WS-BASE-AMOUNT ROUNDED = WS-AVG-USAGE * 0.0479
           IF WS-EXEMPT-HIT > 0
               DISPLAY "I022 EXEMPTION APPLIED - SURCHARGE WAIVED"
               MOVE ZERO TO WS-SURCHARGE
               MOVE "BAND-0 EXEMPT" TO WS-BAND-NAME
           ELSE
               COMPUTE WS-SURCHARGE ROUNDED =
                   WS-BASE-AMOUNT * WS-CONS-PCT
           END-IF.
      *
       6000-REPORT.
           MOVE WS-AVG-USAGE  TO WS-EDT-AVG
           MOVE WS-PEAK-USAGE TO WS-EDT-PEAK
           MOVE WS-SURCHARGE  TO WS-EDT-SUR
           DISPLAY "PREMISE     " WS-IN-PREMISE
           DISPLAY "EXEMPT CODE " WS-EXEMPT-IN
           DISPLAY "EXEMPT SLOT " WS-EXEMPT-HIT
           DISPLAY "AVG USAGE   " WS-EDT-AVG
           DISPLAY "PEAK USAGE  " WS-EDT-PEAK
           DISPLAY "PEAK MONTH  " WS-PEAK-MONTH
           DISPLAY "BAND        " WS-BAND-NAME
           DISPLAY "SURCHARGE   " WS-EDT-SUR
           MOVE 0 TO RETURN-CODE
           EXIT PROGRAM.
