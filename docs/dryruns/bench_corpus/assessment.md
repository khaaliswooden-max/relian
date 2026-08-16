# Legacy Code Assessment — bench/corpus
Schema `relian-assessment-1` · manifest `b3417ca506e34841569f0729088777ad61587348ef72fd7b3a80abb4216767d0`

Every number in this report is a measurement with a stated origin and a Trutina grade, or it is absent. Nothing here is a default, an estimate, or a target reported as a result.

## 1. Executive summary

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Portfolio construct coverage | 1.0 | PLAUSIBLE | 126/126 statements supported across 5 program(s) via SUPPORTED_STATEMENTS@de7f3d1 (c1_rulebased.py sha256:0bad5dd59b092e4b); method=token_scan |
| Quotable-today code lines | 281 | PLAUSIBLE | code lines (281) minus lines carrying an unsupported construct (0) across 5 program(s) |
| Code lines requiring grammar expansion | 0 | PLAUSIBLE | distinct code lines carrying >=1 construct outside SUPPORTED_STATEMENTS@de7f3d1 (c1_rulebased.py sha256:0bad5dd59b092e4b) across 5 program(s) |

**Grade:** PLAUSIBLE · **Provenance:** portfolio risk tier is a policy decision from the RISK_RULES table reproduced in the appendix; its inputs are VERIFIED measurements

| Measure | Value |
| --- | --- |
| Portfolio risk tier | LOW |
| Rule that fired | `LOW: worst program tier across 5 program(s) (5 at LOW)` |


## 2. Manifest

**Grade:** VERIFIED · **Provenance:** sha256 and size_bytes are of the raw bytes on disk; the manifest hash is sha256 of the canonical JSON of the sorted record list (= b3417ca506e34841569f0729088777ad61587348ef72fd7b3a80abb4216767d0)

| Path | Kind | Bytes | Line ending | sha256 |
| --- | --- | --- | --- | --- |
| P01_payroll/payroll01 | other | 117024 | LF | `14e39988df161145` |
| P01_payroll/program.cbl | program | 2941 | LF | `a4b0d32852a107ab` |
| P01_payroll/vectors/public.jsonl | other | 881 | LF | `3f05eeb7b05a2a63` |
| P02_interest/program.cbl | program | 2009 | LF | `eca429658d4d4882` |
| P02_interest/run | other | 111760 | LF | `a3e6ebf713b39544` |
| P02_interest/vectors/public.jsonl | other | 879 | LF | `2a979ac28cfeb511` |
| P03_eligibility/program.cbl | program | 2429 | LF | `4d4fcb294665e67b` |
| P03_eligibility/run | other | 115800 | mixed | `951145751057a251` |
| P03_eligibility/vectors/public.jsonl | other | 1061 | LF | `3cb04465419c38fb` |
| P04_taxtable/program.cbl | program | 2727 | LF | `6dfa8a0461a45bd5` |
| P04_taxtable/run | other | 116328 | LF | `8f6a7121a1ca2ba4` |
| P04_taxtable/vectors/public.jsonl | other | 922 | LF | `dd57934029330e26` |
| P05_validate/program.cbl | program | 2167 | LF | `65f5d5c5402effa0` |
| P05_validate/run | other | 110464 | LF | `b9831074458427d5` |
| P05_validate/vectors/public.jsonl | other | 882 | LF | `5298baaa3edd4710` |


## 3. LOC inventory

**Grade:** VERIFIED · **Provenance:** line categories counted per the rules in appendix A; logical statements come from the same extraction as the coverage map, and are absent where no statements could be recovered

| Program | Physical | Comment | Blank | Code | Logical | Method | Dead paragraphs |
| --- | --- | --- | --- | --- | --- | --- | --- |
| P01_payroll/program.cbl | 74 | 6 | 5 | 63 | 25 | token_scan | — |
| P02_interest/program.cbl | 49 | 4 | 0 | 45 | 19 | token_scan | — |
| P03_eligibility/program.cbl | 58 | 4 | 0 | 54 | 24 | token_scan | — |
| P04_taxtable/program.cbl | 71 | 4 | 0 | 67 | 33 | token_scan | — |
| P05_validate/program.cbl | 56 | 4 | 0 | 52 | 25 | token_scan | — |

Portfolio totals — physical 308, code 281, comment 22, blank 5, logical 126 (5 program(s) measured, 0 not measured).


## 4. Coverage map

| Program | Value | Grade | Provenance |
| --- | --- | --- | --- |
| P01_payroll/program.cbl | 1.0 | PLAUSIBLE | 25/25 statements supported via SUPPORTED_STATEMENTS@de7f3d1 (c1_rulebased.py sha256:0bad5dd59b092e4b) on P01_payroll/program.cbl (sha256:a4b0d32852a107ab); method=token_scan, source_format=fixed; antlr_syntax_errors=11 |
| P02_interest/program.cbl | 1.0 | PLAUSIBLE | 19/19 statements supported via SUPPORTED_STATEMENTS@de7f3d1 (c1_rulebased.py sha256:0bad5dd59b092e4b) on P02_interest/program.cbl (sha256:eca429658d4d4882); method=token_scan, source_format=fixed; antlr_syntax_errors=8 |
| P03_eligibility/program.cbl | 1.0 | PLAUSIBLE | 24/24 statements supported via SUPPORTED_STATEMENTS@de7f3d1 (c1_rulebased.py sha256:0bad5dd59b092e4b) on P03_eligibility/program.cbl (sha256:4d4fcb294665e67b); method=token_scan, source_format=fixed; antlr_syntax_errors=4 |
| P04_taxtable/program.cbl | 1.0 | PLAUSIBLE | 33/33 statements supported via SUPPORTED_STATEMENTS@de7f3d1 (c1_rulebased.py sha256:0bad5dd59b092e4b) on P04_taxtable/program.cbl (sha256:6dfa8a0461a45bd5); method=token_scan, source_format=fixed; antlr_syntax_errors=8 |
| P05_validate/program.cbl | 1.0 | PLAUSIBLE | 25/25 statements supported via SUPPORTED_STATEMENTS@de7f3d1 (c1_rulebased.py sha256:0bad5dd59b092e4b) on P05_validate/program.cbl (sha256:65f5d5c5402effa0); method=token_scan, source_format=fixed; antlr_syntax_errors=2 |


### Portfolio

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Coverage ratio | 1.0 | PLAUSIBLE | 126/126 statements supported across 5 program(s) via SUPPORTED_STATEMENTS@de7f3d1 (c1_rulebased.py sha256:0bad5dd59b092e4b); method=token_scan |


## 5. Unsupported-construct inventory

No construct outside the supported set was found.


## 6. DATA DIVISION features found

**Grade:** VERIFIED · **Provenance:** occurrence counts from source; each status is probed against the transpiler itself, not asserted — `accepted_ignored` means the clause parses but is discarded, so generated code cannot depend on it

| Feature | Occurrences | C1 status |
| --- | --- | --- |
| 88-level condition name | 1 | supported |
| OCCURS fixed size | 1 | supported |
| USAGE COMP-3 (packed decimal) | 27 | accepted_ignored |


## 7. Complexity findings

**Grade:** VERIFIED · **Provenance:** computed per the formulas in appendix B; no threshold is applied here

| Program | Cyclomatic | Statements | GO TO | GO TO density | ALTER | EXEC CICS | EXEC SQL | Max nesting |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| P01_payroll/program.cbl | 6 | 25 | 0 | 0.0 | no | 0 | 0 | 1 |
| P02_interest/program.cbl | 3 | 19 | 0 | 0.0 | no | 0 | 0 | 0 |
| P03_eligibility/program.cbl | 9 | 24 | 0 | 0.0 | no | 0 | 0 | 1 |
| P04_taxtable/program.cbl | 5 | 33 | 0 | 0.0 | no | 0 | 0 | 1 |
| P05_validate/program.cbl | 7 | 25 | 0 | 0.0 | no | 0 | 0 | 1 |


## 8. Risk tiers

**Grade:** PLAUSIBLE · **Provenance:** a published policy (RISK_RULES, appendix C), not a measurement; every input to it is VERIFIED

| Program | Tier | Rule that fired |
| --- | --- | --- |
| P01_payroll/program.cbl | LOW | `LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4` |
| P02_interest/program.cbl | LOW | `LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4` |
| P03_eligibility/program.cbl | LOW | `LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4` |
| P04_taxtable/program.cbl | LOW | `LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4` |
| P05_validate/program.cbl | LOW | `LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4` |

| Tier | Programs |
| --- | --- |
| BLOCKED | 0 |
| HIGH | 0 |
| LOW | 5 |
| MED | 0 |


## 9. Migration-scope recommendation

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Quotable-today code lines | 281 | PLAUSIBLE | code lines (281) minus lines carrying an unsupported construct (0) across 5 program(s) |
| Code lines requiring grammar expansion | 0 | PLAUSIBLE | distinct code lines carrying >=1 construct outside SUPPORTED_STATEMENTS@de7f3d1 (c1_rulebased.py sha256:0bad5dd59b092e4b) across 5 program(s) |

Attribution is by source line: a code line requires grammar expansion if it carries at least one construct the deterministic transpiler cannot handle. This report does not price the work and does not state a schedule.


## 10. Appendices


### Appendix A — LOC counting rules

```
WP-1.4 — LOC inventory.

Pricing reads these numbers, so the counting rules are stated exactly and are
reproduced verbatim in the report appendix. Every rule below is implemented in
:func:`count`; nothing is rounded, estimated, or inferred.

COUNTING RULES
--------------

**physical**
    Every line in the file, counted after normalising ``\r\n`` and bare
    ``\r`` to ``\n``. A trailing newline does not create an extra line: a
    file ending ``"…RUN.\n"`` has the same physical count as one ending
    ``"…RUN."``. Line-ending style therefore never changes the price (R8).

**comment**
    A line whose indicator column (column 7 in fixed format) is ``*`` or ``/``,
    or, in free format, whose first non-blank characters are ``*`` or ``*>``.

**blank**
    A line that is not a comment and whose code area contains only whitespace.
    In fixed format the code area is columns 8–72, so a line carrying only a
    sequence number in columns 1–6 is **blank**, not code.

**code**
    ``physical − comment − blank``. Reported implicitly; the three counted
    categories partition the file exactly, and this is asserted by a test.

**logical**
    The number of COBOL *statements*, taken from the same statement extraction
    the coverage analyzer uses — the ANTLR tree when the program parses
    cleanly, otherwise the documented token scan. It is **not** a regex over
    lines, and it is **not** a count of periods. ``logical_method`` records
    which extraction produced it, and ``logical`` is ``None`` when neither
    could recover statements (R1).

**dead_paragraphs**
    Paragraphs that no control-flow construct can reach. A paragraph is
    considered *reached* if:

    * it is the first paragraph of the PROCEDURE DIVISION (the entry point), or
    * its name appears as a target of ``PERFORM`` or ``GO TO`` anywhere in the
      program, including inside a ``PERFORM … THRU …`` span, in which case every
      paragraph textually between the two endpoints is reached, or
    * it is reachable by fall-through from a reached paragraph — that is, the
      previous paragraph is reached and does not end in an unconditional
      transfer (``GO TO``, ``STOP RUN``, ``GOBACK``, ``EXIT PROGRAM``).

    Reachability is computed to a fixed point, so a chain of PERFORMs is
    followed. ``ALTER`` defeats this analysis by design: a program containing
    ``ALTER`` can redirect a ``GO TO`` at run time, so when ``ALTER`` is present
    no paragraph is reported dead and ``note`` says why. Reporting a paragraph
    dead when ``ALTER`` could reach it would be a guess presented as a finding.
```


### Appendix B — complexity formulas

```
WP-1.5 — complexity metrics.

Every formula is stated here and reproduced verbatim in the report appendix.
**No thresholds live in this module** — nothing here decides whether a number
is good or bad. Thresholds are policy and belong to :mod:`risk`.

FORMULAS
--------

``decision_points``
    Count of branch-introducing constructs in the PROCEDURE DIVISION:
    ``IF``, ``WHEN`` (each ``EVALUATE`` branch, including ``WHEN OTHER``),
    ``UNTIL``, ``VARYING``, ``TIMES``, ``AT END``, ``NOT AT END``,
    ``INVALID KEY``, ``NOT INVALID KEY``, ``ON SIZE ERROR``,
    ``ON OVERFLOW``, ``ON EXCEPTION``, and each ``AND`` / ``OR`` appearing in a
    condition. ``ELSE`` is **not** counted: it is the other side of a branch
    already counted at its ``IF``.

``cyclomatic``
    ``decision_points + 1``. This is McCabe's formula for a single connected
    unit. It is computed per paragraph and, separately, for the whole program
    from the program's own decision points — the program figure is
    ``program_decision_points + 1``, not the sum of the paragraph figures,
    because summing would count each paragraph's ``+1`` again.

``goto_count``
    Occurrences of ``GO TO``. ``GO TO … DEPENDING ON`` counts once per target,
    because each target is a distinct edge.

``goto_density``
    ``goto_count / statements``. ``None`` when the statement count could not be
    measured — never 0.0 as a stand-in.

``alter_present``
    Whether any ``ALTER`` statement appears. Boolean, not a count, because one
    ``ALTER`` is already enough to make static control flow undecidable.

``perform_thru_spans``
    Each ``PERFORM x THRU y`` as the string ``"x THRU y"``. These spans are why
    paragraph boundaries cannot be treated as function boundaries.

``exec_cics_count`` / ``exec_sql_count``
    ``EXEC CICS`` and ``EXEC SQL`` statement occurrences.

``copybook_fan_out``
    Distinct ``COPY`` targets named by this program, quotes stripped. Fan-**in**
    is a portfolio-level inversion of this map and is computed by
    :func:`copybook_fan_in` over all programs.

``call_targets``
    Distinct ``CALL`` targets. Literal targets are recorded as written;
    identifier targets (dynamic CALL) are recorded as the identifier name.

``max_nesting_depth``
    Maximum depth of open scopes, incremented on ``IF`` / ``EVALUATE`` /
    inline ``PERFORM`` and decremented on the matching ``END-…``. A period that
    closes an unterminated ``IF`` also closes the scope.
```


### Appendix C — RISK_RULES, verbatim and in evaluation order

```
BLOCKED: coverage not measured (program did not yield statements)
BLOCKED: coverage<0.60
BLOCKED: ALTER present (static control flow is undecidable)
HIGH: EXEC CICS present
HIGH: EXEC SQL present
HIGH: coverage<0.80
HIGH: cyclomatic>50
HIGH: goto_density>0.10
MED: coverage<1.00
MED: external CALL present
MED: PERFORM THRU span present
MED: cyclomatic>20
MED: max_nesting_depth>4
LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4
```


### Appendix D — coverage method and its limits

```
WP-1.3 — construct coverage: what fraction of a program C1 can transpile.

Two analysis methods, and the result always says which one produced it.

``antlr_tree`` (graded VERIFIED)
    ``src/parsers/antlr/cobol`` is walked and every ``StatementContext`` is
    classified. Used **only** when the parse produced zero syntax errors, so a
    tree assembled by error recovery is never passed off as a clean parse.

``token_scan`` (graded PLAUSIBLE)
    A documented lexical scan, used when the ANTLR parse reports errors.

The fallback is not a nicety. The grammar bundled in this repo
(``src/parsers/grammars/Cobol85.g4``) is a **reduced** COBOL-85 subset, not the
full standard grammar: it requires the ``USAGE`` keyword before ``COMP-3``,
requires ``TIMES`` after ``OCCURS``, has no ``ALTER``/``EXEC``/``ACCEPT`` rules,
and its ``computeStatement`` cannot parse ``COMPUTE X = A + B``. Measured
against this repo's own bench corpus, it reports syntax errors on 5 of 5
programs and recovers **zero** statements from every one of them, because a
DATA DIVISION error resynchronises past the entire PROCEDURE DIVISION. An
analyzer that only used the tree would therefore return "no data" for every
real program. So both methods exist, every result is labelled with the one that
ran, and only the tree path is graded VERIFIED (R1/R9).

Token-scan counting rules (reproduced verbatim in the report appendix):

1. Source format is detected per file: **fixed** if any line carries ``*`` or
   ``/`` in column 7, or if at least 80% of non-blank lines are at least 7
   characters long with columns 1–6 either blank or all digits (a sequence
   number) and column 7 blank, ``*``, ``/`` or ``-``; otherwise **free**. In
   fixed format the code area is columns 8–72 and column 7 is the indicator;
   in free format the whole line is code.
2. A line is a comment if its indicator column is ``*`` or ``/``, or if the
   line's first non-blank characters are ``*>``.
3. Only the PROCEDURE DIVISION is scanned for statements.
4. A statement is counted at each verb token that appears in a
   *statement-start position*: the first token of a line, or a token
   immediately following ``.``, ``THEN``, ``ELSE``, or an ``END-…`` scope
   terminator. This deliberately under-counts verbs buried mid-clause (e.g.
   ``WHEN 1 DISPLAY X``); under-counting a construct is a smaller lie than
   guessing at one, and the grade says PLAUSIBLE.
5. ``EXEC CICS`` / ``EXEC SQL`` / ``EXEC DLI`` count as one statement with verb
   ``EXEC`` and the product recorded as its context.
6. A paragraph label is a line whose code area is a single name followed by a
   period; a section header additionally has ``SECTION`` before the period.

A statement is SUPPORTED iff its verb is in
:func:`src.assessment.supported.supported_verbs`, which reads the transpiler's
dispatch table. Nothing here maintains its own opinion of what C1 supports.
```


### Appendix E — supported set, read from the transpiler

Registry: `SUPPORTED_STATEMENTS@de7f3d1 (c1_rulebased.py sha256:0bad5dd59b092e4b)`

Supported statement keywords: `ACCEPT`, `ADD`, `COMPUTE`, `DISPLAY`, `ELSE`, `END-EVALUATE`, `END-IF`, `END-PERFORM`, `EVALUATE`, `IF`, `INSPECT`, `MOVE`, `PERFORM`, `SEARCH`, `SET`, `STOP`, `UNSTRING`, `WHEN`

Statement-boundary tokens that are **not** supported: `AT`, `END-SEARCH`, `END-UNSTRING`, `SUBTRACT`

| DATA DIVISION feature | C1 status |
| --- | --- |
| 88-level condition name | supported |
| FILE SECTION (FD) record | unsupported |
| OCCURS DEPENDING ON (variable size) | accepted_ignored |
| OCCURS fixed size | supported |
| PIC 9 unsigned integer | supported |
| PIC 9V9 implied decimal | supported |
| PIC A alphabetic | unsupported |
| PIC S9 signed | supported |
| PIC X alphanumeric | supported |
| PIC with CR / DB sign | accepted_ignored |
| PIC with check protect (*) | unsupported |
| REDEFINES | accepted_ignored |
| SIGN IS SEPARATE | accepted_ignored |
| USAGE COMP / BINARY | accepted_ignored |
| USAGE COMP-3 (packed decimal) | accepted_ignored |
| VALUE clause on a data item | accepted_ignored |
| edited picture (Z / - / .) | supported |


### Appendix F — tool versions

| Component | Version |
| --- | --- |
| antlr4-python3-runtime | unknown |
| cli | cli.py |
| platform | Linux |
| python | 3.11.15 |
| python-docx | 1.2.0 |
| relian_transpiler | SUPPORTED_STATEMENTS@de7f3d1 (c1_rulebased.py sha256:0bad5dd59b092e4b) |
| schema | relian-assessment-1 |


### Appendix G — notes on this run

- coverage was derived by the documented token scan for at least one program because the bundled ANTLR grammar could not parse it without syntax errors; those figures are graded PLAUSIBLE, not VERIFIED

