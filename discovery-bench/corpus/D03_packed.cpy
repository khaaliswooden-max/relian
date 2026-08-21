      *================================================================*
      * D03_packed  -  AXIS: COMP-3 PACKED DECIMAL                     *
      * RELIAN-DISCOVERY-BENCH v0.1.  SYNTHETIC.  AUTHORED FOR THIS    *
      * BENCHMARK.  NOT DERIVED FROM ANY CUSTOMER OR BENCHMARK SOURCE. *
      *                                                                *
      * EXERCISES: COMP-3 across CONSECUTIVE odd and even digit        *
      * counts 1..8, which is where the ceil((d+1)/2) byte rule is     *
      * visible, plus an unsigned host and one V-scaled item.  ONE     *
      * AXIS ONLY: no binary, no OCCURS, no REDEFINES.                 *
      *================================================================*
       01  D03-PACKED.
           05  D03-P1                  PIC S9(01) COMP-3.
           05  D03-P2                  PIC S9(02) COMP-3.
           05  D03-P3                  PIC S9(03) COMP-3.
           05  D03-P4                  PIC S9(04) COMP-3.
           05  D03-P5                  PIC S9(05) COMP-3.
           05  D03-P6                  PIC S9(06) COMP-3.
           05  D03-P7                  PIC S9(07) COMP-3.
           05  D03-P8                  PIC S9(08) COMP-3.
           05  D03-UNSIGNED-3          PIC 9(03) COMP-3.
           05  D03-UNSIGNED-4          PIC 9(04) COMP-3.
           05  D03-SCALED              PIC S9(07)V99 COMP-3.
