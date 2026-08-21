      *================================================================*
      * D06_occurs_nested  -  AXIS: NESTED OCCURS, TWO DEEP, INDEXED   *
      * RELIAN-DISCOVERY-BENCH v0.1.  SYNTHETIC.  AUTHORED FOR THIS    *
      * BENCHMARK.  NOT DERIVED FROM ANY CUSTOMER OR BENCHMARK SOURCE. *
      *                                                                *
      * EXERCISES: OCCURS nested two deep with INDEXED BY at both      *
      * levels, so every innermost item needs a TWO-subscript          *
      * reference.  The outer element carries a scalar before the      *
      * inner table so the inner stride is not the outer stride.       *
      * ONE AXIS ONLY: fixed OCCURS, no DEPENDING ON, no REDEFINES.    *
      *================================================================*
       01  D06-NESTED.
           05  D06-OUTER OCCURS 3 TIMES
                         INDEXED BY D06-IX-OUTER.
               10  D06-OUTER-KEY       PIC X(02).
               10  D06-INNER OCCURS 2 TIMES
                             INDEXED BY D06-IX-INNER.
                   15  D06-INNER-CODE  PIC X(03).
                   15  D06-INNER-QTY   PIC S9(03) COMP-3.
           05  D06-TAIL                PIC X(02).
