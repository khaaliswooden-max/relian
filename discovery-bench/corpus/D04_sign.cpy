      *================================================================*
      * D04_sign  -  AXIS: THE SIGN CLAUSE ON DISPLAY NUMERICS         *
      * RELIAN-DISCOVERY-BENCH v0.1.  SYNTHETIC.  AUTHORED FOR THIS    *
      * BENCHMARK.  NOT DERIVED FROM ANY CUSTOMER OR BENCHMARK SOURCE. *
      *                                                                *
      * EXERCISES: SIGN LEADING SEPARATE and SIGN TRAILING SEPARATE    *
      * (which each add a byte) against both EMBEDDED forms, SIGN      *
      * LEADING and SIGN TRAILING (which do not), with the default     *
      * S9(n) as the control.  ONE AXIS ONLY.                          *
      *================================================================*
       01  D04-SIGN.
           05  D04-DEFAULT             PIC S9(04).
           05  D04-LEADING-EMB         PIC S9(04) SIGN LEADING.
           05  D04-LEADING-SEP         PIC S9(04) SIGN LEADING SEPARATE.
           05  D04-TRAILING-EMB        PIC S9(04) SIGN TRAILING.
           05  D04-TRAILING-SEP        PIC S9(04) SIGN TRAILING
                                       SEPARATE CHARACTER.
           05  D04-SEP-DEC             PIC S9(05)V99
                                       SIGN LEADING SEPARATE.
