      *================================================================*
      * D12_edited  -  AXIS: EDITED AND JUSTIFIED PICTURES             *
      * RELIAN-DISCOVERY-BENCH v0.1.  SYNTHETIC.  AUTHORED FOR THIS    *
      * BENCHMARK.  NOT DERIVED FROM ANY CUSTOMER OR BENCHMARK SOURCE. *
      *                                                                *
      * EXERCISES: BLANK WHEN ZERO on a numeric-edited and on a plain  *
      * DISPLAY host, alphanumeric-edited insertion characters B and   *
      * /, floating currency with a CR credit symbol, and JUSTIFIED    *
      * RIGHT on both X and A pictures.                                *
      *                                                                *
      * THIS IS THE COPYBOOK THAT BREAKS THE NAIVE PROBE.  A plain     *
      * `MOVE HIGH-VALUES TO <numeric-edited item>` compiles, runs,    *
      * and writes NO HIGH-VALUE BYTES AT ALL -- measured, see         *
      * SPEC.md "Why the probe reference-modifies".  The probe fills   *
      * through `<item> (1:)` for that reason.  ONE AXIS ONLY.         *
      *================================================================*
       01  D12-EDITED.
           05  D12-AMOUNT-EDIT         PIC ZZZ,ZZ9.99.
           05  D12-BWZ-EDITED          PIC ZZZZ9 BLANK WHEN ZERO.
           05  D12-BWZ-DISPLAY         PIC 9(05) BLANK WHEN ZERO.
           05  D12-ALNUM-EDIT          PIC XXBXXBXXXX.
           05  D12-SLASH-EDIT          PIC XXX/XX/XXXX.
           05  D12-CREDIT-EDIT         PIC $$,$$9.99CR.
           05  D12-JUST-X              PIC X(08) JUSTIFIED RIGHT.
           05  D12-JUST-A              PIC A(06) JUST RIGHT.
