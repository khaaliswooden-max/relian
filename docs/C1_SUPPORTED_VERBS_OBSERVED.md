# C1 transpiler — observed supported constructs and failure behavior

**WP-0.3 deliverable. Observation only — no code was changed to produce this document.**

Source read: `transpiler/c1_rulebased.py` (HEAD `9d364d8`), specifically
`Transpiler._statements` (L338–349), `Transpiler.stmt` (L398–554),
`Transpiler.store` (L355–370), `parse_pic` (L48–69),
`parse_working_storage` (L72–105), `ExprTx` (L110–221), `cond_tx` (L226–257).

Every claim below was confirmed by executing the transpiler on probe programs
(the probe transcript is reproduced in §4). Nothing here is inferred from prose.

---

## 1. What `stmt()` handles

`stmt()` is a linear `if/elif` chain keyed on `s.split()[0]` — the first
whitespace-delimited token of the statement. There is no table; the supported
set is implicit in the chain's structure. This is exactly the drift risk WP-1.2
exists to remove.

| Verb (dispatch key) | Accepted form (regex in the branch) | Emits |
|---|---|---|
| `ACCEPT` | `ACCEPT <name>` | `<name> = _sc.nextLine();` |
| `UNSTRING` | `UNSTRING <src> DELIMITED BY "<lit>" INTO <t1> <t2> …[ END-UNSTRING]` | `R.unstring(...)` + per-target assigns |
| `COMPUTE` | `COMPUTE <name>[(sub)] [ROUNDED] = <expr>` | `store()` with HALF_UP if ROUNDED else DOWN |
| `MOVE` | `MOVE <src> TO <name>[(sub)]` | literal / numeric / alpha / edited store |
| `ADD` | `ADD <expr> TO <name>` | `store(name, name.add(expr))` |
| `SET` | `SET <name> TO <value>` | index assign (`BI = …`) or numeric store |
| `IF` | `IF <cond>` | `if (…) {` , indent += 1 |
| `ELSE` | bare | `} else {` |
| `END-IF` | bare | `}` , indent −= 1 |
| `EVALUATE` | bare (subject ignored) | sets `_first_when`; emits nothing |
| `WHEN` | `WHEN <cond>` / `WHEN OTHER` | `if`/`} else if`/`} else` chain |
| `END-EVALUATE` | bare | `}` |
| `PERFORM` | **only** `PERFORM VARYING <v> FROM <n> BY <n> UNTIL <cond>` | `for (…) {` |
| `END-PERFORM` | bare | `}` |
| `SEARCH` | `SEARCH <name>` + following `AT END` / `WHEN` up to `END-SEARCH` | linear scan loop over `BI` |
| `INSPECT` | `INSPECT <src> TALLYING <cnt> FOR ALL "<lit>"…` | inline char-count block |
| `DISPLAY` | `DISPLAY <lit \| name \| FUNCTION TRIM(x)>…` | `System.out.println(a + b + …)` |
| `STOP` | `STOP [RUN]` | `return;` |

Verbs recognized only as *statement boundaries* (`VERBS`, L24–27) and not as
dispatch targets: `AT` (consumed inside `SEARCH`), `END-UNSTRING`,
`SUBTRACT` (**in `VERBS` but has no `stmt()` branch — it starts a new
statement and is then silently discarded**; see §3, case A).

### Expression- and condition-level support
- Arithmetic: `+ - * /`, parentheses, numeric literals, field references,
  `FUNCTION NUMVAL`, `FUNCTION LENGTH`, `FUNCTION TRIM`, single-subscript
  `NAME(expr)` on `OCCURS` fields. Division is `divide(…, 20, DOWN)`; the one
  scale fix happens at store time.
- Conditions (`cond_tx`): `AND`-separated relations with `<= >= < > = NOT =`,
  alphanumeric compares via `R.eq`, bare 88-level condition names, `ZERO`/`ZEROS`
  literal on the RHS. **`OR`, `NOT` as a prefix, and parenthesized condition
  groups are not handled** — `OR` is not split on, so an `OR` condition lands in
  the LHS of the relation regex and silently produces a wrong comparison or the
  literal string `false /* unparsed cond */`.

## 2. Data-division subset (`parse_working_storage` / `parse_pic`)

Accepted: levels `01`–`49` with `PIC`, `88` condition names with a quoted
`VALUE`, group-level `OCCURS n`, and inline `OCCURS n` on a PIC line.
`PIC` forms: `S`, `9`, `X`, `V`, `Z`, `-`, `.`, and `(n)` repetition.
Kinds derived: `num` / `alpha` / `edit` (edited iff `Z`, `-`, or `.` present).

Not represented anywhere in the parsed model — i.e. accepted syntactically and
then **ignored**:
- `USAGE COMP-3` / `COMP` / `BINARY` / `PACKED-DECIMAL`. The literature and the
  bench SPEC describe COMP-3 as exercised, and it is — but only because the
  emitted `BigDecimal` arithmetic is storage-agnostic. The clause itself is
  dropped by the `PIC` regex, so a COMP-3 field is indistinguishable from
  DISPLAY in the model.
- `OCCURS … DEPENDING ON` (the `OCCURS\s+(\d+)` regex takes the fixed bound and
  drops the `DEPENDING ON` clause — **no error**).
- `REDEFINES`, `RENAMES` (66), `VALUE` on non-88 levels, `SIGN` / `SEPARATE`,
  `JUSTIFIED`, `SYNCHRONIZED`, `BLANK WHEN ZERO`, multi-dimension `OCCURS`,
  `INDEXED BY`, FILE SECTION / `FD` records, LINKAGE SECTION.
- Any `PIC` containing `A`, `P`, `CR`, `DB`, `/`, `0`, `B`, or `*` — these
  characters are not in the regex character class, so the clause fails to match
  and **the field is dropped from `fields` entirely**, which surfaces later as a
  `KeyError` if it is ever referenced.

## 3. Behavior on an unrecognized statement — the precise mechanism

There is **no `else` branch in `stmt()`**. Control falls off the bottom of the
`if/elif` chain to `return i + 1`. There is no `unsupported()` function, no
warning, no counter, and no diagnostic anywhere in the module. What an operator
actually observes depends on *where* the unsupported text sits, and there are
four distinct behaviors:

**A. Unknown verb that begins a statement → silently discarded.**
`_statements()` starts a new statement when the first token is in `VERBS` *or*
the output list is empty. So an unsupported verb at the very start of the
PROCEDURE DIVISION becomes its own statement, falls through `stmt()`, and emits
nothing. The program transpiles "successfully" with that logic missing. Same for
`SUBTRACT`, which is in `VERBS` but has no branch. **This is the R2 violation
WP-1.2 must make visible.**

**B. Unknown verb that follows a statement → absorbed as a continuation.**
Because the first token is not in `VERBS`, `_statements()` appends the line to
the *previous* statement (L346–348). `GO TO OTHER-PARA` after a `MOVE` produces
the single statement `MOVE 5 TO WS-A CALL "SUBPROG" USING WS-A GO TO OTHER-PARA
…`. The corrupted statement then either raises downstream (observed:
`KeyError: 'MAIN-PARA'`) or emits wrong code. The failure is loud but the
*reported cause is misleading* — it names a data field, not the unsupported verb.

**C. Supported verb in an unsupported form → `AttributeError`.**
Every branch does `re.match(...)` then immediately `m.group(n)` with no None
check. `PERFORM OTHER-PARA` (a paragraph PERFORM) raises
`AttributeError: 'NoneType' object has no attribute 'groups'`;
`ADD 1 TO WS-A GIVING WS-A` raises the same on `.group`. The exception escapes
`transpile()`.

**D. Supported verb whose regex matches the wrong slice → invalid Java, silently.**
`MOVE CORRESPONDING WS-A TO WS-A` matches the `MOVE` regex with
`src = "CORRESPONDING WS-A"` and emits `WS_A = R.take(CORRESPONDING WS_A, 999);`
— not valid Java. Nothing in the Python layer notices; only `javac` does.

### Where honest failure actually lives today
Only at **program granularity, and only in the callers**:
- `c1_rulebased.main()` (L573–575) catches `Exception` and writes
  `// TRANSPILE FAILED: <exc>` into the `.java` file.
- `src/core/orchestrator.py:_transform_to_java` catches it and fails the
  migration with *"deterministic transform does not support this program's
  constructs (…); refusing to emit a placeholder in place of a migration"*
  (observed end-to-end on `examples/cobol/banking-system.cbl`).

So: **cases C and D fail honestly at the program level (C loudly, D at javac).
Cases A and B do not** — A is a silent omission and B is a misattributed error.
There is no construct inventory in any path.

## 4. Probe transcript (reproducible)

```
$ python3 - <<'PY'   # abridged; full driver in the WP-0 session log
=== unknown verb FIRST (nothing to absorb into)
  stmts: ['GO TO OTHER-PARA', 'DISPLAY WS-A', 'STOP RUN']
  BODY:  System.out.println(R.dnumU(WS_A, 3));   <-- GO TO vanished, no error
=== known verb, unparseable form (MOVE CORRESPONDING)
  BODY:  WS_A = R.take(CORRESPONDING WS_A, 999);  <-- invalid Java, no error
=== ADD with GIVING (known verb, unsupported form)
  RAISED AttributeError: 'NoneType' object has no attribute 'group'
=== PERFORM paragraph (known verb, non-VARYING form)
  RAISED AttributeError: 'NoneType' object has no attribute 'groups'
=== unknown verb mid-paragraph (CALL/GO TO/EXEC SQL/ALTER after a MOVE)
  stmts: ['MOVE 5 TO WS-A CALL "SUBPROG" USING WS-A GO TO OTHER-PARA EXEC SQL … ALTER …']
  RAISED KeyError: 'MAIN-PARA'                    <-- misattributed cause
PY
```

## 5. Consequences for WP-1.2

1. The dispatch table must key on the same token `stmt()` keys on today
   (`s.split()[0]`), so `supported_verbs()` is exactly the set of verbs that
   reach a branch. `SUBTRACT` must **not** appear in it — it is a boundary
   token, not a supported verb, and shipping it in the registry would be a
   constant pretending to be a measurement (R1).
2. `unsupported(verb, line_no)` replaces the fall-through. To stay
   behavior-preserving (the WP-1.2 gate: zero output bytes changed), its default
   action must remain *emit nothing* while recording the occurrence. Turning
   case A into a hard failure is the correct end state under R2 but it **is** a
   behavior change and therefore an operator decision, not an agent decision —
   it is exposed as an opt-in strict mode, default off, and flagged for
   escalation.
3. Case C (regex-match-then-None) is the honest path and is left as-is; the
   assessment engine models it as "supported verb, unsupported form" only via
   the cross-check test, not by pattern-matching prose.
4. `supported_data_features()` must be derived from the same explicit-registry
   pattern, and must record COMP-3 / OCCURS DEPENDING as **accepted-but-ignored**
   rather than "supported", because ignoring a clause is not support.
