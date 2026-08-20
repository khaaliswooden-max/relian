      *================================================================*
      * D08_redefines  -  AXIS: REDEFINES                              *
      * RELIAN-DISCOVERY-BENCH v0.1.  SYNTHETIC.  AUTHORED FOR THIS    *
      * BENCHMARK.  NOT DERIVED FROM ANY CUSTOMER OR BENCHMARK SOURCE. *
      *                                                                *
      * EXERCISES all three shapes the WP names:                       *
      *   EQUAL LENGTH   D08-KEY-R over the elementary D08-KEY,        *
      *                  re-cut as a group.                            *
      *   SMALLER        D08-DATE-SHORT over D08-DATE.                 *
      *   OF A GROUP     D08-GRP-FLAT and D08-GRP-HEAD over the group  *
      *                  D08-GRP -- two redefinitions of one target.   *
      * The deliberate overlap is why the D9 tiling invariant is       *
      * conditioned on non-redefining fields.  ONE AXIS ONLY.          *
      *================================================================*
       01  D08-REDEFINES.
           05  D08-KEY                 PIC X(10).
           05  D08-KEY-R REDEFINES D08-KEY.
               10  D08-KR-PREFIX       PIC X(03).
               10  D08-KR-BODY         PIC X(07).
           05  D08-DATE                PIC 9(08).
           05  D08-DATE-SHORT REDEFINES D08-DATE
                                       PIC X(04).
           05  D08-GRP.
               10  D08-GRP-A           PIC X(04).
               10  D08-GRP-B           PIC S9(05) COMP-3.
           05  D08-GRP-FLAT REDEFINES D08-GRP
                                       PIC X(07).
           05  D08-GRP-HEAD REDEFINES D08-GRP
                                       PIC X(02).
           05  D08-TRAILER             PIC X(03).
