      *================================================================*
      * SYNCCOUPLE  -  WP-2.7 acceptance (5): SYNC sensitivity COUPLED *
      * to member width, not independent of it.                        *
      *                                                                *
      * SYNTHETIC.  Authored for WP-2.7.  NOT sealed.                  *
      *                                                                *
      * S-CTR is PIC S9(01) COMP SYNCHRONIZED.  Under GnuCOBOL it is   *
      * 1 byte, and GnuCOBOL aligns a SYNC item to its OWN WIDTH -- a  *
      * 1-byte boundary is no constraint, so NO slack is inserted.     *
      * Under IBM the same item is 2 bytes AND IBM's m for a 1-digit   *
      * binary is a halfword, so slack appears where there was none.   *
      * The record grows by TWO bytes from a ONE-byte width rule: the  *
      * alignment boundary followed the width that changed.  That is   *
      * the coupling, and it is why D10 cannot be tested as though     *
      * alignment were independent of member width.                    *
      *================================================================*
       01  SYNCCOUPLE-REC.
           05  S-FLAG                  PIC X(01).
           05  S-CTR                   PIC S9(01) COMP SYNCHRONIZED.
           05  S-TAIL                  PIC X(04).
