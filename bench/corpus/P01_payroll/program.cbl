       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL01.
      *****************************************************************
      * Gross-to-net payroll. Exercises: ROUNDED half-up, overtime
      * decision logic, graduated withholding, COMP-3 packed decimal.
      * Input  (stdin, CSV): HOURS,RATE,DEPENDENTS
      * Output (stdout)    : GROSS=x NET=y TAX=z
      *****************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RAW            PIC X(80).
       01  WS-HOURS          PIC 9(3)V99 COMP-3.
       01  WS-RATE           PIC 9(3)V99 COMP-3.
       01  WS-DEPS           PIC 9(2).
       01  WS-REG-HOURS      PIC 9(3)V99 COMP-3.
       01  WS-OT-HOURS       PIC 9(3)V99 COMP-3.
       01  WS-GROSS          PIC 9(7)V99 COMP-3.
       01  WS-TAXABLE        PIC S9(7)V99 COMP-3.
       01  WS-TAX            PIC 9(7)V99 COMP-3.
       01  WS-NET            PIC S9(7)V99 COMP-3.
       01  WS-DED            PIC 9(5)V99 COMP-3.
       01  WS-F1             PIC X(20).
       01  WS-F2             PIC X(20).
       01  WS-F3             PIC X(20).
       01  WS-O-GROSS        PIC Z(6)9.99.
       01  WS-O-NET         PIC -(6)9.99.
       01  WS-O-TAX          PIC Z(6)9.99.
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-RAW FROM CONSOLE
           UNSTRING WS-RAW DELIMITED BY ","
               INTO WS-F1 WS-F2 WS-F3
           END-UNSTRING
           COMPUTE WS-HOURS = FUNCTION NUMVAL(WS-F1)
           COMPUTE WS-RATE  = FUNCTION NUMVAL(WS-F2)
           COMPUTE WS-DEPS  = FUNCTION NUMVAL(WS-F3)

           IF WS-HOURS > 40.00
               MOVE 40.00 TO WS-REG-HOURS
               COMPUTE WS-OT-HOURS = WS-HOURS - 40.00
           ELSE
               MOVE WS-HOURS TO WS-REG-HOURS
               MOVE ZERO TO WS-OT-HOURS
           END-IF

           COMPUTE WS-GROSS ROUNDED =
               (WS-REG-HOURS * WS-RATE)
             + (WS-OT-HOURS * WS-RATE * 1.5)

           COMPUTE WS-DED = WS-DEPS * 76.92
           COMPUTE WS-TAXABLE = WS-GROSS - WS-DED
           IF WS-TAXABLE < 0
               MOVE ZERO TO WS-TAXABLE
           END-IF

           EVALUATE TRUE
               WHEN WS-TAXABLE <= 500.00
                   COMPUTE WS-TAX ROUNDED = WS-TAXABLE * 0.10
               WHEN WS-TAXABLE <= 1500.00
                   COMPUTE WS-TAX ROUNDED =
                       50.00 + ((WS-TAXABLE - 500.00) * 0.22)
               WHEN OTHER
                   COMPUTE WS-TAX ROUNDED =
                       270.00 + ((WS-TAXABLE - 1500.00) * 0.32)
           END-EVALUATE

           COMPUTE WS-NET = WS-GROSS - WS-TAX
           MOVE WS-GROSS TO WS-O-GROSS
           MOVE WS-NET   TO WS-O-NET
           MOVE WS-TAX   TO WS-O-TAX
           DISPLAY "GROSS=" FUNCTION TRIM(WS-O-GROSS)
                   " NET="  FUNCTION TRIM(WS-O-NET)
                   " TAX="  FUNCTION TRIM(WS-O-TAX)
           STOP RUN.
