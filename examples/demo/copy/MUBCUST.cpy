      *================================================================*
      * MUBCUST  -  CUSTOMER MASTER WORKING FIELDS                     *
      * COPYBOOK.  INCLUDED BY MUBPOST.                                *
      * SYNTHETIC DEMONSTRATION CODE.  NOT PRODUCTION.                 *
      *================================================================*
       01  WS-CUST-WORK.
           05  WS-CW-ACCOUNT           PIC X(10).
           05  WS-CW-NAME              PIC X(30).
           05  WS-CW-CLASS             PIC X(01).
           05  WS-CW-PRIOR-BALANCE     PIC S9(09)V99 COMP-3.
           05  WS-CW-NEW-BALANCE       PIC S9(09)V99 COMP-3.
           05  WS-CW-DELINQUENT-SW     PIC X(01).
               88  WS-CW-DELINQUENT     VALUE "Y".
               88  WS-CW-IN-GOOD-STAND  VALUE "N".
