      *================================================================*
      * MUBCONS  -  DISTRICT-WIDE RATING CONSTANTS                     *
      * COPYBOOK.  INCLUDED BY MUBBILL AND MUBPOST.                    *
      * SYNTHETIC DEMONSTRATION CODE.  NOT PRODUCTION.                 *
      *================================================================*
       01  MUB-CONSTANTS.
           05  MUB-FRANCHISE-RATE      PIC S9V9(04) COMP-3 VALUE 0.0625.
           05  MUB-SVC-FACTOR          PIC S9V9(04) COMP-3 VALUE 1.0725.
           05  MUB-STATE-TAX-RATE      PIC S9V9(04) COMP-3 VALUE 0.0825.
           05  MUB-GRACE-DAYS          PIC 9(03)           VALUE 030.
           05  MUB-DISTRICT-CODE       PIC X(04)           VALUE "MMUD".
