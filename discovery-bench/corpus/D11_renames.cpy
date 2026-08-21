      *================================================================*
      * D11_renames  -  AXIS: LEVEL-66 RENAMES                         *
      * RELIAN-DISCOVERY-BENCH v0.1.  SYNTHETIC.  AUTHORED FOR THIS    *
      * BENCHMARK.  NOT DERIVED FROM ANY CUSTOMER OR BENCHMARK SOURCE. *
      *                                                                *
      * EXERCISES: RENAMES of a single item, RENAMES ... THRU spanning *
      * two elementaries, and RENAMES ... THRU whose end point is a    *
      * GROUP.  A level-66 alias claims bytes it does not own, so like *
      * REDEFINES it is excluded from the D9 tiling projection and     *
      * asserted separately to lie within the extent it renames.       *
      * ONE AXIS ONLY.                                                 *
      *================================================================*
       01  D11-RENAMES.
           05  D11-ALPHA               PIC X(03).
           05  D11-BETA                PIC X(04).
           05  D11-GAMMA.
               10  D11-GAMMA-1         PIC X(02).
               10  D11-GAMMA-2         PIC X(02).
           05  D11-DELTA               PIC X(05).
       66  D11-JUST-GAMMA RENAMES D11-GAMMA.
       66  D11-ALPHA-BETA RENAMES D11-ALPHA THRU D11-BETA.
       66  D11-BETA-GAMMA RENAMES D11-BETA THRU D11-GAMMA.
