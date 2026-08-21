      *================================================================*
      * D07_odo  -  AXIS: OCCURS DEPENDING ON                          *
      * RELIAN-DISCOVERY-BENCH v0.1.  SYNTHETIC.  AUTHORED FOR THIS    *
      * BENCHMARK.  NOT DERIVED FROM ANY CUSTOMER OR BENCHMARK SOURCE. *
      *                                                                *
      * EXERCISES: a variable-occurrence table, probed at BOTH the     *
      * minimum and the maximum occurrence count.  The group length    *
      * moves; the per-occurrence offsets do not, and the oracle       *
      * records both lengths rather than picking one.                  *
      *                                                                *
      * NOTE: the ODO table is the LAST item in the group because      *
      * GnuCOBOL 3.1.2 rejects any item declared after it --           *
      * "'D07-ENTRY' cannot have OCCURS DEPENDING because of <name>".  *
      * ONE AXIS ONLY: no REDEFINES, no nesting.                       *
      *================================================================*
       01  D07-ODO.
           05  D07-COUNT               PIC 9(02).
           05  D07-HEADER              PIC X(04).
           05  D07-ENTRY OCCURS 1 TO 4 TIMES
                         DEPENDING ON D07-COUNT.
               10  D07-ENTRY-PERIOD    PIC 9(06).
               10  D07-ENTRY-AMOUNT    PIC S9(07)V99 COMP-3.
