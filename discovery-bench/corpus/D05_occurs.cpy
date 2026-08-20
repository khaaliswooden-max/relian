      *================================================================*
      * D05_occurs  -  AXIS: OCCURS, FIXED, SINGLE LEVEL               *
      * RELIAN-DISCOVERY-BENCH v0.1.  SYNTHETIC.  AUTHORED FOR THIS    *
      * BENCHMARK.  NOT DERIVED FROM ANY CUSTOMER OR BENCHMARK SOURCE. *
      *                                                                *
      * EXERCISES: a fixed OCCURS on an ELEMENTARY item and a fixed    *
      * OCCURS on a GROUP item, each bracketed by scalars so the       *
      * table's start and end offsets are both observable.  ONE AXIS   *
      * ONLY: no DEPENDING ON, no nesting, no INDEXED BY.              *
      *================================================================*
       01  D05-OCCURS.
           05  D05-HEADER              PIC X(04).
           05  D05-SLOT OCCURS 4 TIMES PIC X(05).
           05  D05-MIDDLE              PIC X(02).
           05  D05-ROW OCCURS 3 TIMES.
               10  D05-ROW-CODE        PIC X(02).
               10  D05-ROW-AMOUNT      PIC S9(05) COMP-3.
           05  D05-TRAILER             PIC X(03).
