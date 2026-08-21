      *================================================================*
      * D02_binary  -  AXIS: BINARY USAGES                             *
      * RELIAN-DISCOVERY-BENCH v0.1.  SYNTHETIC.  AUTHORED FOR THIS    *
      * BENCHMARK.  NOT DERIVED FROM ANY CUSTOMER OR BENCHMARK SOURCE. *
      *                                                                *
      * EXERCISES: COMP, COMP-4 and BINARY at the 1-, 2-, 4- and       *
      * 8-DIGIT storage boundaries of the 1-2-4-8 binary-size rule,    *
      * plus COMP-5 (native binary) at two widths and one unsigned     *
      * COMP.  ONE AXIS ONLY: no COMP-3, no OCCURS, no REDEFINES.      *
      *================================================================*
       01  D02-BINARY.
           05  D02-COMP-1              PIC S9(01) COMP.
           05  D02-COMP-2              PIC S9(02) COMP.
           05  D02-COMP-4              PIC S9(04) COMP.
           05  D02-COMP-8              PIC S9(08) COMP.
           05  D02-COMP-9              PIC S9(09) COMP.
           05  D02-COMP-18             PIC S9(18) COMP.
           05  D02-COMP4-4             PIC S9(04) COMP-4.
           05  D02-BINARY-9            PIC S9(09) BINARY.
           05  D02-COMP5-4             PIC S9(04) COMP-5.
           05  D02-COMP5-18            PIC S9(18) COMP-5.
           05  D02-COMP-UNSIGNED       PIC 9(04) COMP.
