# Legacy Code Assessment — examples/cobol
Schema `relian-assessment-1` · manifest `20da747f78d10e1b9512059e23077ee171b8fb164f4811a8256319374a6dd252`

Every number in this report is a measurement with a stated origin and a Trutina grade, or it is absent. Nothing here is a default, an estimate, or a target reported as a result.

## 1. Executive summary

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Portfolio construct coverage | 0.7545 | PLAUSIBLE | 83/110 statements supported across 1 program(s) via SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b); method=token_scan |
| Quotable-today code lines | 241 | PLAUSIBLE | code lines (268) minus lines carrying an unsupported construct (27) across 1 program(s) |
| Code lines requiring grammar expansion | 27 | PLAUSIBLE | distinct code lines carrying >=1 construct outside SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b) across 1 program(s) |

**Grade:** PLAUSIBLE · **Provenance:** portfolio risk tier is a policy decision from the RISK_RULES table reproduced in the appendix; its inputs are VERIFIED measurements

| Measure | Value |
| --- | --- |
| Portfolio risk tier | HIGH |
| Rule that fired | `HIGH: worst program tier across 1 program(s) (1 at HIGH)` |


## 2. Manifest

**Grade:** VERIFIED · **Provenance:** sha256 and size_bytes are of the raw bytes on disk; the manifest hash is sha256 of the canonical JSON of the sorted record list (= 20da747f78d10e1b9512059e23077ee171b8fb164f4811a8256319374a6dd252)

| Path | Kind | Bytes | Line ending | sha256 |
| --- | --- | --- | --- | --- |
| banking-system.cbl | program | 15069 | LF | `24ba36227dc35845` |


## 3. LOC inventory

**Grade:** VERIFIED · **Provenance:** line categories counted per the rules in appendix A; logical statements come from the same extraction as the coverage map, and are absent where no statements could be recovered

| Program | Physical | Comment | Blank | Code | Logical | Method | Dead paragraphs |
| --- | --- | --- | --- | --- | --- | --- | --- |
| banking-system.cbl | 361 | 56 | 37 | 268 | 110 | token_scan | — |

Portfolio totals — physical 361, code 268, comment 56, blank 37, logical 110 (1 program(s) measured, 0 not measured).


## 4. Coverage map

| Program | Value | Grade | Provenance |
| --- | --- | --- | --- |
| banking-system.cbl | 0.7545 | PLAUSIBLE | 83/110 statements supported via SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b) on banking-system.cbl (sha256:24ba36227dc35845); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |


### Portfolio

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Coverage ratio | 0.7545 | PLAUSIBLE | 83/110 statements supported across 1 program(s) via SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b); method=token_scan |


## 5. Unsupported-construct inventory

**Grade:** VERIFIED · **Provenance:** occurrence counts of constructs absent from SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b), counted over the statements listed in the coverage map

| Construct | Occurrences |
| --- | --- |
| STRING | 7 |
| WRITE | 6 |
| READ | 4 |
| OPEN | 3 |
| REWRITE | 3 |
| SUBTRACT | 2 |
| CLOSE | 1 |
| START | 1 |


### Occurrences

| File | Line | Paragraph | Construct | Context |
| --- | --- | --- | --- | --- |
| banking-system.cbl | 140 | INITIALIZATION | STRING | — |
| banking-system.cbl | 143 | INITIALIZATION | OPEN | — |
| banking-system.cbl | 150 | INITIALIZATION | OPEN | — |
| banking-system.cbl | 157 | INITIALIZATION | OPEN | — |
| banking-system.cbl | 169 | PROCESS-TRANSACTIONS | READ | — |
| banking-system.cbl | 195 | PROCESS-DEPOSIT | READ | — |
| banking-system.cbl | 205 | PROCESS-DEPOSIT | REWRITE | — |
| banking-system.cbl | 224 | PROCESS-WITHDRAWAL | READ | — |
| banking-system.cbl | 244 | PROCESS-WITHDRAWAL | SUBTRACT | — |
| banking-system.cbl | 246 | PROCESS-WITHDRAWAL | SUBTRACT | — |
| banking-system.cbl | 253 | PROCESS-WITHDRAWAL | REWRITE | — |
| banking-system.cbl | 283 | CALCULATE-DAILY-INTEREST | START | — |
| banking-system.cbl | 286 | CALCULATE-DAILY-INTEREST | READ | — |
| banking-system.cbl | 299 | CALCULATE-DAILY-INTEREST | REWRITE | — |
| banking-system.cbl | 309 | GENERATE-REPORTS | STRING | — |
| banking-system.cbl | 311 | GENERATE-REPORTS | WRITE | — |
| banking-system.cbl | 314 | GENERATE-REPORTS | STRING | — |
| banking-system.cbl | 316 | GENERATE-REPORTS | WRITE | — |
| banking-system.cbl | 319 | GENERATE-REPORTS | STRING | — |
| banking-system.cbl | 322 | GENERATE-REPORTS | WRITE | — |
| banking-system.cbl | 325 | GENERATE-REPORTS | STRING | — |
| banking-system.cbl | 328 | GENERATE-REPORTS | WRITE | — |
| banking-system.cbl | 331 | GENERATE-REPORTS | STRING | — |
| banking-system.cbl | 333 | GENERATE-REPORTS | WRITE | — |
| banking-system.cbl | 336 | GENERATE-REPORTS | STRING | — |
| banking-system.cbl | 338 | GENERATE-REPORTS | WRITE | — |
| banking-system.cbl | 356 | TERMINATION | CLOSE | — |


## 6. DATA DIVISION features found

**Grade:** VERIFIED · **Provenance:** occurrence counts from source; each status is probed against the transpiler itself, not asserted — `accepted_ignored` means the clause parses but is discarded, so generated code cannot depend on it

| Feature | Occurrences | C1 status |
| --- | --- | --- |
| 88-level condition name | 12 | supported |
| FILE SECTION (FD) record | 3 | unsupported |
| USAGE COMP / BINARY | 1 | accepted_ignored |
| VALUE clause on a data item | 14 | accepted_ignored |


## 7. Complexity findings

**Grade:** VERIFIED · **Provenance:** computed per the formulas in appendix B; no threshold is applied here

| Program | Cyclomatic | Statements | GO TO | GO TO density | ALTER | EXEC CICS | EXEC SQL | Max nesting |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| banking-system.cbl | 26 | 110 | 0 | 0.0 | no | 0 | 0 | 4 |


## 8. Risk tiers

**Grade:** PLAUSIBLE · **Provenance:** a published policy (RISK_RULES, appendix C), not a measurement; every input to it is VERIFIED

| Program | Tier | Rule that fired |
| --- | --- | --- |
| banking-system.cbl | HIGH | `HIGH: coverage<0.80` |

| Tier | Programs |
| --- | --- |
| BLOCKED | 0 |
| HIGH | 1 |
| LOW | 0 |
| MED | 0 |


## 9. Migration-scope recommendation

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Quotable-today code lines | 241 | PLAUSIBLE | code lines (268) minus lines carrying an unsupported construct (27) across 1 program(s) |
| Code lines requiring grammar expansion | 27 | PLAUSIBLE | distinct code lines carrying >=1 construct outside SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b) across 1 program(s) |

Attribution is by source line: a code line requires grammar expansion if it carries at least one construct the deterministic transpiler cannot handle. This report does not price the work and does not state a schedule.


### By construct — what grammar work would unlock

**Grade:** VERIFIED · **Provenance:** occurrences of each unsupported construct across the portfolio

| Construct | Occurrences |
| --- | --- |
| STRING | 7 |
| WRITE | 6 |
| READ | 4 |
| OPEN | 3 |
| REWRITE | 3 |
| SUBTRACT | 2 |
| CLOSE | 1 |
| START | 1 |


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
    Maximum depth of open scopes, tracked with a **stack** rather than a
    counter. A scope is opened by ``IF``, ``EVALUATE``, ``SEARCH``, or an
    *inline* ``PERFORM`` — one whose loop body is written in place, recognised
    as ``PERFORM UNTIL``, ``PERFORM VARYING``, ``PERFORM WITH TEST``,
    ``PERFORM FOREVER``, or ``PERFORM <n> TIMES``. A ``PERFORM <paragraph>``
    transfers control elsewhere and opens no scope here, so it does not count.

    A scope is closed by its own ``END-…`` terminator, and by nothing else: an
    ``END-…`` whose opener is not on the stack is **ignored** rather than
    decrementing the depth. That distinction is load-bearing — with a plain
    counter, an ``END-PERFORM`` or ``END-READ`` sitting inside an outer ``IF``
    cancels the ``IF``'s own depth, and every construct nested after it in that
    ``IF`` is undercounted. ``END-READ``, ``END-CALL``, ``END-STRING`` and
    ``END-UNSTRING`` are therefore inert here, because the statements they
    terminate are not counted as opening a scope in the first place.

    Openers and closers are processed in the order they appear on the line, so
    a scope opened and closed on one line still registers its depth. A period
    ends the sentence and closes every scope still open.
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

Registry: `SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b)`

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
| relian_transpiler | SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b) |
| schema | relian-assessment-1 |


### Appendix G — notes on this run

- coverage was derived by the documented token scan for at least one program because the bundled ANTLR grammar could not parse it without syntax errors; those figures are graded PLAUSIBLE, not VERIFIED

