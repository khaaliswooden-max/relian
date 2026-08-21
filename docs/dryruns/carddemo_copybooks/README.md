# WP-2.2 dry run — CardDemo copybook resolution

**Resolver only.** No layout claim is made about any CardDemo *program*: program
parse is still blocked on WP-2.5, and §7 established the blockers are dialect as
well as `COPY`. Copybook resolution is unaffected by that (D17) — this run needs
no preprocessor and no compiler.

| | |
|---|---|
| Corpus | `github.com/aws-samples/aws-mainframe-modernization-carddemo` |
| Commit | `59cc6c2fd7ebd7ef7925cad552a01a4b8b6e4d5e` |
| Licence | Apache-2.0 |
| Clone location | `~/corpora/carddemo` — **outside** the repository; no CardDemo bytes are committed here |
| Command | `python3 -m src.discovery.cli resolve ~/corpora/carddemo` |
| Raw output | [`resolution.json`](resolution.json) |

## Counts — measured in this run

Every figure below is produced by the command above. None is transcribed from
WP-2.0.0 §0.4; the §0.4 column is shown only so drift would be visible.

| Row | **This run** | WP-2.0.0 §0.4 | Drift |
|---|---|---|---|
| Source files scanned (`.cbl` + `.cpy`) | **106** | 44 + 62 = 106 | none |
| Files with ≥1 `COPY` | **40** | 40 of 44 | none |
| `COPY` directive sites | **346** | not recorded | — |
| Distinct copybook names referenced | **67** | 67 | none |
| — resolvable to a member in the tree | **59** | 59 | none |
| — **not** resolvable | **8** | 8 | none |
| (program, copybook) edges | **306** | 306 | none |
| Maximum fan-out | **18**, `app/cbl/COACTUPC.cbl` | 18, `COACTUPC.cbl` | none |
| `COPY … REPLACING` sites | **40** | 40 | none |
| Members present but never referenced | **3** (`CSDB2RPY`, `CSDB2RWY`, `UNUSED1Y`) | 3, same three | none |
| Cycles | **0** | not recorded | — |

The run and the log agree on every row §0.4 recorded. Had they disagreed, the
run would be authoritative and this table would say so (WP-2.2 §5).

Two rows §0.4 did not carry are worth naming. **346 directive
sites against 306 edges**: a program that `COPY`s the same member twice
is two sites and one edge, and conflating them would make the edge count and the
`REPLACING`-site count the same kind of number when they are not.
**0 cycles**: CardDemo's copybook graph is acyclic. The resolver
detects cycles and reports them without following them, so a cyclic corpus would
produce a finding rather than a stack overflow.

## The missing-copybook table (D20)

This is the deliverable, not an errata section. CardDemo is **not
self-contained**, which is the normal condition of real mainframe code — and
"here are the copybooks you are missing and who references them, before you
start" is a finding a customer pays for. Most vendors report it as a crash.

| Missing copybook | Referenced by | `COPY` sites | Likely source |
|---|---|---|---|
| `DFHAID` | 21 programs | 21 | CICS-supplied |
| `DFHBMSCA` | 21 programs | 21 | CICS-supplied |
| `CMQGMOV` | 3 programs | 3 | IBM MQ-supplied |
| `CMQMDV` | 3 programs | 4 | IBM MQ-supplied |
| `CMQODV` | 3 programs | 4 | IBM MQ-supplied |
| `CMQPMOV` | 3 programs | 3 | IBM MQ-supplied |
| `CMQTML` | 3 programs | 3 | IBM MQ-supplied |
| `CMQV` | 3 programs | 3 | IBM MQ-supplied |

Every one of the eight resolves at the customer site and at neither of ours:
`DFHAID` and `DFHBMSCA` ship with CICS, the six `CMQ*` members with IBM MQ.
Each `Unresolved` record also carries the full referrer list and the count of
directories searched — "missing" without "we looked here" is an accusation
rather than a finding.

## Fan-in — the most-shared members

| Copybook | Referenced by |
|---|---|
| `COCOM01Y` | 21 |
| `COTTL01Y` | 21 |
| `CSDAT01Y` | 21 |
| `CSMSG01Y` | 21 |
| `DFHAID` | 21 |
| `DFHBMSCA` | 21 |

`DFHAID` and `DFHBMSCA` sit at 21 alongside the four most-shared members that
*do* resolve. A vendor-supplied member with the same fan-in as the application's
own shared copybooks is the shape of the risk: eight absences reach 21 programs.

## The `REPLACING` false positive, in production

All 40 `COPY … REPLACING` sites name the same member,
`CSSETATY` — 39 in `app/cbl/COACTUPC.cbl` and 1 in
`app/app-transaction-type-db2/cbl/COTRTUPC.cbl`. Each spans four lines and
carries three pseudo-text pairs, so the resolver has to join program-text lines
before matching and substitute before layout. All 40 parsed completely: zero
directives came back with `replacing_parsed = False`.

The distinct-name count is 67 rather than 68 because the `COPY` regex refuses
to match the tail of a hyphenated identifier. Measured in this run by scanning
the same corpus twice with patterns that differ **only** in the boundary
assertion — same operand grammar, same fixed-format margins, `\b` in one and
`(?<![A-Za-z0-9$_-])` in the other:

| Pattern | Distinct names |
|---|---|
| `\bCOPY\s+…` | **68** |
| `(?<![A-Za-z0-9$_-])COPY(?![A-Za-z0-9$_-])\s+…` | **67** |
| Only in the naive set | `REPLACING` |
| Only in the strict set | — none |

The phantom is matched out of

```
INITIALIZE REQUEST-MSG-COPY  REPLACING NUMERIC BY ZEROES
```

at `app/app-vsam-mq/cbl/CODATE01.cbl:294` — and, measured here rather than
cited, at `app/app-vsam-mq/cbl/COACCT01.cbl:345` as well. WP-2.0.0 §0.4 named
one site; there are two. `\b` sits happily between `-` and `C`; the lookbehind
does not.

`CODATE01.cbl:294` is a fixture in `tests/test_discovery_copybook.py`, along
with its converse — a real `COPY` in the same file must still be found, because
a guard that suppressed both would also be "green".

## What this run does NOT license

* **No layout claim about any CardDemo record.** The resolver ran; the engine
  did not. Layouts here would be graded against nothing.
* **No IBM Enterprise COBOL claim.** Everything the layout engine is verified
  against is GnuCOBOL 3.1.2.0 behaviour on the sealed 15-copybook corpus.
  CardDemo is IBM-dialect source; its resolution is dialect-independent, its
  layout would not be.
