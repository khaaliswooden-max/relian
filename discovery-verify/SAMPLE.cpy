      *================================================================*
      * SAMPLE  -  specimen copybook for the Relian verification kit.  *
      *                                                                *
      * SYNTHETIC.  Authored for this kit.  Contains no customer data, *
      * no customer field names and nothing derived from any real      *
      * record layout.                                                 *
      *                                                                *
      * It exercises the constructs that DECIDE whether our model of   *
      * your compiler is right:                                        *
      *                                                                *
      *   SM-CTR-1    PIC S9(01) COMP  -- the dialect-sensitive one.   *
      *                GnuCOBOL 3.1.2 allocates 1 byte; IBM allocates  *
      *                a halfword (2 bytes) for 1 through 4 digits.    *
      *                Every field after it shifts by the difference.  *
      *   SM-CTR-4    PIC S9(04) COMP  -- 2 bytes under both.          *
      *   SM-AMOUNT   PIC S9(07)V99 COMP-3 -- packed, believed invariant
      *   SM-NAME     PIC X(10)        -- character, invariant.        *
      *   SM-ENTRY    OCCURS 4 TIMES   -- where a 1-byte difference is *
      *                multiplied by the occurrence count.             *
      *   SM-SYNC-A   PIC S9(09) COMP SYNC -- alignment, whose boundary*
      *                follows the width that may have just changed.   *
      *================================================================*
       01  SAMPLE-REC.
           05  SM-NAME                 PIC X(10).
           05  SM-CTR-1                PIC S9(01) COMP.
           05  SM-CTR-4                PIC S9(04) COMP.
           05  SM-AMOUNT               PIC S9(07)V99 COMP-3.
           05  SM-ENTRY                OCCURS 4 TIMES.
               10  SM-ENTRY-CTR        PIC S9(02) COMP.
               10  SM-ENTRY-CODE       PIC X(03).
           05  SM-FLAG                 PIC X(01).
           05  SM-SYNC-A               PIC S9(09) COMP SYNCHRONIZED.
           05  SM-TRAILER              PIC X(02).
