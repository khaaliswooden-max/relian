      *================================================================*
      * D10_sync  -  AXIS: THE SYNCHRONIZED CLAUSE                     *
      * RELIAN-DISCOVERY-BENCH v0.1.  SYNTHETIC.  AUTHORED FOR THIS    *
      * BENCHMARK.  NOT DERIVED FROM ANY CUSTOMER OR BENCHMARK SOURCE. *
      *                                                                *
      * EXERCISES: SYNCHRONIZED (and the SYNC abbreviation) on binary  *
      * items deliberately preceded by odd numbers of character bytes, *
      * so each COMP item needs SLACK BYTES in front of it to reach    *
      * its natural boundary.                                          *
      *                                                                *
      * MEASURED: GnuCOBOL 3.1.2.0 aligns.  Declared field bytes sum   *
      * to 20; the group is 25.  The five slack bytes are the axis.    *
      * They belong to NO NAMED FIELD, cannot be the receiving item of *
      * a MOVE, and are therefore NOT PROBEABLE -- they are recovered  *
      * as `gap` spans, graded as derived rather than measured.  This  *
      * copybook is the reason the gap mechanism exists.               *
      * ONE AXIS ONLY: no OCCURS, no REDEFINES, no conditions.         *
      *================================================================*
       01  D10-SYNC.
           05  D10-FLAG                PIC X(01).
           05  D10-BIN-A               PIC S9(09) COMP SYNCHRONIZED.
           05  D10-CHAR-1              PIC X(01).
           05  D10-BIN-B               PIC S9(04) COMP SYNC.
           05  D10-CHAR-3              PIC X(03).
           05  D10-BIN-C               PIC S9(18) COMP SYNC.
           05  D10-TRAILER             PIC X(01).
