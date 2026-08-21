      *================================================================*
      * D09_conditions  -  AXIS: LEVEL-88 CONDITION NAMES              *
      * RELIAN-DISCOVERY-BENCH v0.1.  SYNTHETIC.  AUTHORED FOR THIS    *
      * BENCHMARK.  NOT DERIVED FROM ANY CUSTOMER OR BENCHMARK SOURCE. *
      *                                                                *
      * EXERCISES: single VALUE, multi-VALUE, VALUE ... THRU on both   *
      * an alphanumeric and a DISPLAY numeric host, and a condition    *
      * on a COMP-3 host -- the case that matters because the host's   *
      * bytes are packed, not readable as text.                        *
      *                                                                *
      * A level-88 has NO STORAGE.  It carries offset null and length  *
      * null in a separate `conditions` array and appears in no field  *
      * row.  Zero is not an honest answer for a thing with no         *
      * extent (R1).  ONE AXIS ONLY.                                   *
      *================================================================*
       01  D09-CONDITIONS.
           05  D09-STATUS              PIC X(01).
               88  D09-ACTIVE           VALUE "A".
               88  D09-ANY-OPEN         VALUE "A", "B", "C".
               88  D09-FIRST-HALF       VALUE "A" THRU "M".
           05  D09-CODE                PIC 9(03).
               88  D09-CODE-LOW         VALUE 1 THRU 99.
               88  D09-CODE-KNOWN       VALUE 100, 200, 300.
           05  D09-AMOUNT              PIC S9(05) COMP-3.
               88  D09-AMOUNT-ZERO      VALUE ZERO.
               88  D09-AMOUNT-BAND      VALUE 100 THRU 999.
           05  D09-TRAILER             PIC X(02).
