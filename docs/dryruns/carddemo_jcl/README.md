# WP-2.4 dry run — CardDemo file inventory, lineage, and the LRECL cross-check

**JCL, SELECT/FD, lineage and target-schema DDL.** Every number below was
measured in this run, on 2026-08-22. Where a figure disagrees with the one
recorded in `docs/PHASE2_LOG.md` §0.4, both are shown and the difference is
accounted for — a baseline transcribed rather than measured makes every delta
wrong, and the same applies to a corpus count.

| | |
|---|---|
| Corpus | `github.com/aws-samples/aws-mainframe-modernization-carddemo` |
| Commit | `59cc6c2fd7ebd7ef7925cad552a01a4b8b6e4d5e` |
| License | Apache-2.0 |
| Clone location | `~/corpora/carddemo` — **outside** the repository; no CardDemo bytes are committed here |
| Commands | `src.discovery.jcl.scan`, `files.scan_programs`, `files.build_inventory`, `lineage.build_graph` |
| Toolchain | CPython 3.11.15, GnuCOBOL 3.1.2.0, javac 21.0.10 |

**No dataset was opened and no customer record was read.** Everything below
comes from JCL, COBOL and copybook source text (R12). CardDemo ships EBCDIC
data files under `app/data/`; none was read, and
`tests/test_discovery_reads_no_data.py` asserts that property of the pipeline
under a `sys.addaudithook` with decoy datasets planted.

---

## 1. JCL inventory — measured, against §0.4

| Row | §0.4 recorded | **Measured in this run** | Drift |
|---|---|---|---|
| `.jcl` members | 55 | **55** | — |
| Members containing a DD statement | 53 of 55 | **53 of 55** | — |
| DD statements | 524 | **555** | **+31** |
| — named DD statements | (not recorded) | **519** | — |
| — concatenated (`//    DD`) | (not recorded) | **36** | — |
| DD statements with `DSN=`/`DSNAME=` | 245 | **242** | **−3** |
| Members with `DSN=` | 42 | **42** | — |
| DD statements with `DCB=` | 43 | **43** | — |
| Members with `DCB=` | 20 | **20** | — |
| — of those, declaring an `LRECL` | (not recorded) | **38** | — |
| DD statements assembled from >1 record | (not recorded) | **126** | — |

**The run is authoritative and the log is stale.** Both differences are fully
accounted for, and neither is a defect in either figure — they count different
things.

**DD statements, 524 → 555.** §0.4's number is reproducible exactly as
`grep -cE '^//\S+\s+DD\s'` summed across the 55 members, which is the command
§0.6 records for the demo corpus. That regex differs from a parse in two ways
that happen to pull in opposite directions:

* it counts **5 commented-out DD statements**, because `\S+` matches the
  `*DDPAUTP0` in `//*DDPAUTP0   DD DSN=OEM.IMS.IMSP.PAUTHDB,DISP=SHR`
  (`app/app-authorization-ims-db2-mq/jcl/LOADPADB.JCL:40-43`,
  `UNLDGSAM.JCL:45`, `UNLDPADB.JCL:61`); and
* it misses all **36 concatenated DD statements** — the unnamed
  `//           DD DISP=SHR,DSN=…` records that supply a second and third
  dataset under one DD name, as at
  `app/app-authorization-ims-db2-mq/jcl/CBPAUP0J.jcl:27`.

So `524 = 519 real named DDs + 5 comments`, and `555 = 519 named + 36
concatenated`. A file inventory must count the concatenations: each names its
own dataset, and a concatenation is precisely the case where one DD name maps
to several datasets.

**`DSN=`, 245 → 242.** 245 is the number of *lines* mentioning `DSN=`. Two of
those are inside comment records. One more, at `app/jcl/CREASTMT.JCL:91`, is an
orphaned continuation: the record above it
(`app/jcl/CREASTMT.JCL:87`) is corrupted **in the corpus itself** —

```
//         SPACE=(CYL,(1,1),RLSE), 00,RECFM=FB), ATA.VSAM.KSDS
```

— which ends without a continuation comma, so the `DSN=` record that follows
belongs to no statement. `245 = 242 DD statements + 2 comments + 1 orphan`.

### Parser notes — three, all of them findings about the corpus

The parser emitted exactly three notes across 55 members. All three are real
properties of CardDemo, not parser limitations:

1. `app/app-transaction-type-db2/jcl/MNTTRDB2.jcl` — an in-stream `DD *` block
   runs to end of file with no `/*` delimiter. Two records treated as data.
2. `app/jcl/CREASTMT.JCL:87` — the corrupted record quoted above; unbalanced
   `)` reported rather than repaired.
3. `app/jcl/CREASTMT.JCL:91` — the orphaned continuation that follows from it.

The visible consequence is honest degradation: `CBSTM03A`'s `STMTFILE` DD is
reported with the `DCB=(LRECL=80,…)` it *does* declare and with **no dataset**,
because the DSN was on the stranded record. It is not guessed at.

### Two defects this run found in the parser itself

Both were found by the parser's own notes on first contact with the corpus, and
both are fixed and regression-tested:

* **Qualified DD names.** `//PRC001.FILEIN DD` overrides `FILEIN` inside
  procedure step `PRC001` (`app/jcl/PRTCATBL.jcl:31`). An unqualified name
  pattern did not merely mis-name it — it failed to recognise the record as a
  statement at all, and stranded the record after it. Five members were
  affected.
* **Orphaned continuations reported as operations.** A stranded
  `//   DSN=AWS.M2…` was being reported as an unmodelled JCL *operation* named
  `DSN=AWS.M2…`, which is nonsense dressed as a finding.

### V6 confirmed

`app/jcl/READACCT.jcl` — the statement begins at line 37 and the `DCB=` is on
line 39, so recovering it requires the continuation join:

```
//OUTFILE  DD DSN=AWS.M2.CARDDEMO.ACCTDATA.PSCOMP,      <- line 37
//            DISP=(NEW,CATLG,DELETE),                  <- line 38
//            DCB=(LRECL=107,RECFM=FB,DSORG=PS,BLKSIZE=0),
//            UNIT=SYSAD,SPACE=(CYL,(1,2),RLSE)         <- line 40
```

parsed to `LRECL=107, RECFM=FB, DSORG=PS, BLKSIZE=0`, bound to step `STEP05`,
program `CBACT01C`, over lines 37–40.

---

## 2. The cross-check (D29)

44 programs scanned, 17 declaring a `FILE-CONTROL` paragraph. The scan is wired
to WP-2.2's copybook resolver (100 members indexed, 8 unresolvable).

| Outcome | Count |
|---|---|
| `AGREE` | **13** |
| `DISAGREE` | **0** |
| `NO_LRECL` | 41 |
| `NO_LAYOUT` | 194 |

> **Correction.** The first version of this run reported 9 / 0 / 35 / 204. It
> was executed **without passing the copybook resolver** to `build_inventory`,
> so every FD whose record arrives through `COPY` came back with
> *"no copybook resolution was provided to this scan"* and was counted as
> `NO_LAYOUT`. That was a defect in the dry run, not in the tool: it understated
> the cross-check's reach by four agreements. The figures above are the
> resolver-wired run and supersede the earlier ones. The earlier ones are
> recorded here rather than deleted, because a number that changed silently is
> worth less than one whose change is accounted for.

### The thirteen agreements

Each row states its own comparison basis. Note `VBRCFILE`: computed 80 against
a declared `LRECL` of 84, and they **agree** — `RECFM=VB` means z/OS counts the
4-byte Record Descriptor Word inside `LRECL`. That adjustment is applied and
stated in the row rather than folded in silently.

| Program | DD | Record | Via | Computed | RECFM | RDW | Adjusted | LRECL | Δ |
|---|---|---|---|---|---|---|---|---|---|
| CBACT01C | OUTFILE | OUT-ACCT-REC | inline | 107 | FB | 0 | 107 | 107 | 0 |
| CBACT01C | ARRYFILE | ARR-ARRAY-REC | inline | 110 | FB | 0 | 110 | 110 | 0 |
| CBACT01C | VBRCFILE | VBR-REC | inline | 80 | **VB** | **4** | **84** | 84 | 0 |
| CBACT04C | TRANSACT | FD-TRANFILE-REC | inline | 350 | F | 0 | 350 | 350 | 0 |
| CBIMPORT | CUSTOUT | CUSTOMER-RECORD | `COPY CVCUS01Y` | 500 | FB | 0 | 500 | 500 | 0 |
| CBIMPORT | ACCTOUT | ACCOUNT-RECORD | `COPY CVACT01Y` | 300 | FB | 0 | 300 | 300 | 0 |
| CBIMPORT | XREFOUT | CARD-XREF-RECORD | `COPY CVACT03Y` | 50 | FB | 0 | 50 | 50 | 0 |
| CBIMPORT | TRNXOUT | TRAN-RECORD | `COPY CVTRA05Y` | 350 | FB | 0 | 350 | 350 | 0 |
| CBIMPORT | ERROUT | ERROR-OUTPUT-RECORD | inline | 132 | FB | 0 | 132 | 132 | 0 |
| CBSTM03A | STMTFILE | FD-STMTFILE-REC | inline | 80 | FB | 0 | 80 | 80 | 0 |
| CBSTM03A | HTMLFILE | FD-HTMLFILE-REC | inline | 100 | FB | 0 | 100 | 100 | 0 |
| CBTRN02C | DALYREJS | FD-REJS-RECORD | inline | 430 | F | 0 | 430 | 430 | 0 |
| CBTRN03C | TRANREPT | FD-REPTFILE-REC | inline | 133 | FB | 0 | 133 | 133 | 0 |

The four `COPY`-supplied rows are the strongest evidence here: the copybook
resolver, the layout engine and the JCL parser are three independent pieces of
machinery, and all three have to be right for the row to come out zero.

**Zero disagreements is a result, not an absence of one.** Thirteen
independently authored copybooks each computed to exactly the length the job
stream declares for the dataset holding them. Two artifacts that share no code
and no author agreeing to the byte on thirteen records is evidence the layout
engine computes IBM-dialect record lengths correctly on a corpus it was never
fitted to.

It is **not** evidence that the check would catch a disagreement. That is what
the fixtures are for: `tests/test_discovery_files.py` carries a deliberately
mismatched pair (12 computed against `LRECL=107` declared) and asserts both
numbers survive into the report, that the conflict names them, and that no
resolving accessor exists on `CrossCheck` at all.

### What the cross-check can be claimed to reach

The only rows the cross-check can possibly evaluate are DD statements that
carry a program, a DSN **and** a declared `LRECL`. CardDemo has 31 such DD
statements, collapsing to **21 distinct (program, DD) pairs**.

| | |
|---|---|
| (program, DD) pairs with a program, a DSN and a declared `LRECL` | 21 |
| — cross-checked | **12** |
| — not reached | 9 |
| Additionally cross-checked with no DSN to pair on | 1 |

The one extra is `CBSTM03A`/`STMTFILE`: it has an `LRECL` and no DSN, because
the DSN sat on the record stranded by the corruption at
`app/jcl/CREASTMT.JCL:87`. It still cross-checks, on the `DCB=` it does have.

**All nine unreached pairs belong to steps that are not COBOL programs:**

| Step program | DDs | What it is |
|---|---|---|
| `DFSRRC00` | DFSURGU1, OUTFIL1, OUTFIL2 | IMS region controller |
| `IEFBR14` | DD1, HTMLFILE, STMTFILE | z/OS null program (allocation only) |
| `IDCAMS` | SYSPRINT | access-method utility |
| `IEBGENER` | SYSUT2 | copy utility |
| `SORT` | SORTOUT | DFSORT/SYNCSORT |

None has COBOL source in the tree, so no `SELECT` or `FD` exists to describe
its records. That is not a limitation of the cross-check — it is the correct
answer: *no COBOL we can see describes these datasets.* Restricted to steps
whose program has COBOL source in the tree, the cross-check reaches **12 of
12**.

### The 194 `NO_LAYOUT` rows, by cause

This is the number that governs the claim above, so it is broken down by cause
rather than reported as one bucket.

| Cause | Count |
|---|---|
| No `SELECT` in any program in scope declares this DD | **194** |
| Unresolvable copybook blocking an FD | **0** |
| Program parse failure | **0** |
| Layout-engine refusal with the construct named | **0** |

**Every one of the 194 is a scope fact, not a failure of the resolver, the
parser or the engine.** Each was checked directly rather than inferred from the
total:

* **Unresolvable copybooks — 0.** The resolver reports 8 unresolvable names
  (`DFHAID`, `DFHBMSCA`, `CMQV`, `CMQODV`, `CMQMDV`, `CMQGMOV`, `CMQPMOV`,
  `CMQTML`). All are CICS and MQ system copybooks that ship with the product
  rather than with the sample. **No `FD` in the corpus references any of
  them** — they are `WORKING-STORAGE` structures — so not one blocks a record
  description.
* **Program parse failures — 0.** 44 programs parsed, 44 `PROGRAM-ID`s
  recovered, 0 programs with a `SELECT` but no `FD`. Two programs declare a
  `FILE-CONTROL` paragraph and yield zero `SELECT`s, and both are correct
  refusals rather than misses: `CBPAUP0C`'s `FILE-CONTROL.` is **empty**, and
  every `SELECT` in `DBUNLDGS` is **commented out** in column 7. The second
  closes a loop with the table above — `DFSRRC00`'s `OUTFIL1`/`OUTFIL2` appear
  as undeclared datasets precisely because the only program that ever named
  them has those `SELECT`s commented out.
* **Layout-engine refusals — 0.** Zero records produced a computed layout with
  no length, and zero layouts came back other than `COMPLETE`. Every record
  description the engine was handed, it measured.

Sub-dividing the 194 by what kind of DD it is:

| | LRECL declared | no LRECL |
|---|---|---|
| Infrastructure DD (STEPLIB, SYSOUT, SYSPRINT, IMS, …) | 13 | 103 |
| Data-shaped DD | **6** | 72 |

Only the 6 data-shaped rows with a declared `LRECL` are findings a migration
team would act on — a dataset with a stated record length that nothing in scope
describes — and all 6 are the utility and IMS steps listed above. The 175 rows
with no `LRECL` at all check nothing; they are reported so they are not
mistaken for checks that passed.

### Findings from keeping the three sources apart (D28)

| Finding | Count |
|---|---|
| `DATASET_NOT_DECLARED` — a dataset the job stream supplies that no `SELECT` in scope declares | 194 |
| `FILE_NOT_SUPPLIED` — a `SELECT` whose DD no step running that program supplies | 16 |

Both are invisible in a design that merges the three sources into one row.

## 3. Lineage (D30)

> 44 program(s) scanned; 17 declare a FILE-CONTROL paragraph; 15 contain at
> least one OPEN; **27 program-to-dataset edge(s) recovered across 20
> dataset(s)**. This graph is not complete and is not offered as complete: it
> shows the flows that static analysis of COBOL and JCL can see.

5 program-to-program pairs are derivable through a shared dataset. All 5 are
emitted separately and labelled `INFERRED`; none is an edge. `A → dataset → B`
is observed, `A → B` assumes an execution order this analysis never measured.

Six categories of flow are structurally invisible to this method and are named
in the output itself: dynamic allocation, GDG relative references, TSO-driven
datasets, unparsed utilities (IDCAMS/DFSORT/IEBGENER), CICS and IMS resource
access, and programs outside the scanned tree.

---

## 4. Target-schema DDL and the load gate

The DDL corpus is the sealed 15-copybook RELIAN-DISCOVERY-BENCH v0.1 set (read
only; nothing written back). No CardDemo record layout is emitted as DDL here,
for the same reason `docs/dryruns/carddemo_copybooks/README.md` makes no layout
claim about CardDemo: the layout engine is verified against GnuCOBOL 3.1.2.0
and CardDemo is IBM-dialect source.

| | |
|---|---|
| Tables generated | 15 |
| — `COMPLETE` | 12 |
| — `PARTIAL` | 3 |
| Statements executed against `postgres:16` | 16 |
| Execution errors | **0** |
| Columns claimed by this repository | 122 |
| Columns observed in `information_schema` | 122 |
| Columns reconciled | 122 |
| Mismatches | **0** |
| Ruleset | `relian-ddl-map-v1.0` |

The three `PARTIAL` tables are `D07_odo` (the extent is a runtime property),
`D08_redefines` (same bytes, two interpretations) and `D11_renames`. Every one
names its reason in the emitted SQL.

**Verified by PostgreSQL, not by re-reading our own output.** The
reconciliation compares `data_type`, `character_maximum_length`,
`numeric_precision` and `numeric_scale` as PostgreSQL reports them. Proven to
bite: with the rendered DDL sabotaged to execute `CHAR(99)` while continuing to
claim `CHAR(1)`, the run fails on the reconciliation and names all four
affected columns.

---

## 5. What this dry run does not establish

* **IBM Enterprise COBOL equivalence** — still unmeasured, still disclosed. The
  thirteen agreements are evidence about record *lengths*, not about every
  byte of every field.
* **That CardDemo's copybooks describe CardDemo's data.** They describe the
  lengths the job stream declares. Whether the bytes on the volume match is not
  determinable from source, and this tool never opens a volume to find out.
* **Completeness of the lineage graph.** See §3 and the six disclosed
  categories.
