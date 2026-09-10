      *================================================================*
      * OCCURSHIFT  -  WP-2.7 acceptance (5): a DIALECT_SENSITIVE field*
      * INSIDE an OCCURS, so the one-byte widening multiplies by the   *
      * occurrence count instead of being added once.                  *
      *                                                                *
      * SYNTHETIC.  Authored for WP-2.7's classification tests.  NOT   *
      * part of RELIAN-DISCOVERY-BENCH and NOT sealed -- the sealed    *
      * corpus is frozen under rule 4 and is read, never written.      *
      *                                                                *
      * T-CTR is PIC S9(01) COMP: 1 byte under GnuCOBOL 3.1.2 whose    *
      * binary-size 1-2-4-8 gives 1-2 digits a single byte, 2 bytes    *
      * under IBM's halfword-for-1-through-4-digits rule.  The member  *
      * is 4 bytes measured and 5 projected, so the 40-entry table is  *
      * 160 measured and 200 projected: a +40 record delta from a      *
      * ONE-byte rule difference.  No SYNC, no REDEFINES.              *
      *================================================================*
       01  OCCURSHIFT-REC.
           05  T-HEADER                PIC X(02).
           05  T-ENTRY                 OCCURS 40 TIMES.
               10  T-CTR               PIC S9(01) COMP.
               10  T-NAME              PIC X(03).
           05  T-TRAILER               PIC X(01).
