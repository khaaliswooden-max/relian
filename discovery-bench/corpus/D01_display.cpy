      *================================================================*
      * D01_display  -  AXIS: USAGE DISPLAY NUMERIC                    *
      * RELIAN-DISCOVERY-BENCH v0.1.  SYNTHETIC.  AUTHORED FOR THIS    *
      * BENCHMARK.  NOT DERIVED FROM ANY CUSTOMER OR BENCHMARK SOURCE. *
      *                                                                *
      * EXERCISES: unsigned and signed DISPLAY integers, V implied     *
      * decimal, leading and trailing P scaling positions, the one-    *
      * digit and eighteen-digit ends of the range.  ONE AXIS ONLY:    *
      * no COMP, no OCCURS, no REDEFINES, so a round-trip failure      *
      * here names DISPLAY numeric and nothing else.                   *
      *================================================================*
       01  D01-DISPLAY.
           05  D01-UNSIGNED-1          PIC 9.
           05  D01-UNSIGNED-5          PIC 9(05).
           05  D01-SIGNED-5            PIC S9(05).
           05  D01-UNSIGNED-18         PIC 9(18).
           05  D01-SIGNED-18           PIC S9(18).
           05  D01-UNSIGNED-DEC        PIC 9(05)V99.
           05  D01-SIGNED-DEC          PIC S9(05)V99.
           05  D01-ALL-FRACTION        PIC SV9(04).
           05  D01-SCALE-TRAILING      PIC S9(04)PPP.
           05  D01-SCALE-LEADING       PIC SPPP9(04).
