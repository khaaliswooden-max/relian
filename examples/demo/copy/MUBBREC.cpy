      *================================================================*
      * MUBBREC  -  BILL PRINT RECORD LAYOUT                           *
      * COPYBOOK.  INCLUDED BY MUBBILL.                                *
      * SYNTHETIC DEMONSTRATION CODE.  NOT PRODUCTION.                 *
      *================================================================*
       01  BILL-PRINT-RECORD.
           05  BPR-ACCOUNT             PIC X(10).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  BPR-NAME                PIC X(30).
           05  BPR-CLASS               PIC X(01).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  BPR-AMOUNT-DUE          PIC ZZZZZZZZ9.99.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  BPR-DUE-DATE            PIC X(08).
