      *================================================================*
      * UNSOURCED  -  WP-2.7 acceptance (2): the UNKNOWN class is      *
      * reachable and is NOT a guess.                                  *
      *                                                                *
      * SYNTHETIC.  Authored for WP-2.7.  NOT sealed.                  *
      *                                                                *
      * U-FLOAT is COMP-1.  RELIAN-DISCOVERY-BENCH v0.1 does not       *
      * measure COMP-1, so the GnuCOBOL side has no measured width     *
      * (R7) and there is nothing for a sourced IBM rule to be         *
      * compared against.  The field classifies UNKNOWN and the record *
      * length is undetermined rather than estimated (R1, R2).         *
      *================================================================*
       01  UNSOURCED-REC.
           05  U-NAME                  PIC X(04).
           05  U-FLOAT                 COMP-1.
           05  U-TAIL                  PIC X(02).
