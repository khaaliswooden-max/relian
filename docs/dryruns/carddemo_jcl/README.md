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

44 programs scanned, 17 declaring a `FILE-CONTROL` paragraph.

| Outcome | Count |
|---|---|
| `AGREE` | **9** |
| `DISAGREE` | **0** |
| `NO_LRECL` | 35 |
| `NO_LAYOUT` | 204 |

### The nine agreements

Each row states its own comparison basis. Note `VBRCFILE`: the computed record
length is 80, the declared `LRECL` is 84, and they **agree** — because
`RECFM=VB` means z/OS counts the 4-byte Record Descriptor Word inside `LRECL`.
That adjustment is stated in the row rather than folded in silently.

| Program | DD | Record | Computed | RECFM | RDW | Adjusted | LRECL | Δ |
|---|---|---|---|---|---|---|---|---|
| CBACT01C | OUTFILE | OUT-ACCT-REC | 107 | FB | 0 | 107 | 107 | 0 |
| CBACT01C | ARRYFILE | ARR-ARRAY-REC | 110 | FB | 0 | 110 | 110 | 0 |
| CBACT01C | VBRCFILE | VBR-REC | 80 | **VB** | **4** | **84** | 84 | 0 |
| CBACT04C | TRANSACT | FD-TRANFILE-REC | 350 | F | 0 | 350 | 350 | 0 |
| CBIMPORT | ERROUT | ERROR-OUTPUT-RECORD | 132 | FB | 0 | 132 | 132 | 0 |
| CBSTM03A | STMTFILE | FD-STMTFILE-REC | 80 | FB | 0 | 80 | 80 | 0 |
| CBSTM03A | HTMLFILE | FD-HTMLFILE-REC | 100 | FB | 0 | 100 | 100 | 0 |
| CBTRN02C | DALYREJS | FD-REJS-RECORD | 430 | F | 0 | 430 | 430 | 0 |
| CBTRN03C | TRANREPT | FD-REPTFILE-REC | 133 | FB | 0 | 133 | 133 | 0 |

**Zero disagreements is a result, not an absence of one.** Nine independently
authored copybooks each computed to exactly the length the job stream declares
for the dataset holding them — including one that only agrees after a 4-byte
RDW adjustment. Two artifacts that share no code and no author agreeing to the
byte on nine records is evidence the layout engine computes IBM-dialect record
lengths correctly, on a corpus it was never fitted to.

It is **not** evidence that the check would catch a disagreement. That is what
the fixtures are for: `tests/test_discovery_files.py` carries a deliberately
mismatched pair (12 computed against `LRECL=107` declared) and asserts both
numbers survive into the report, that the conflict names them, and that no
resolving accessor exists on `CrossCheck` at all.

### The other two outcomes

* **`NO_LRECL` (35)** — a record layout with no `DCB=` anywhere in the job
  stream to check it against. Mostly VSAM KSDS files, which carry their record
  length in the catalog rather than in JCL. Reported as *unverified against any
  external source*, not as verified.
* **`NO_LAYOUT` (204)** — overwhelmingly the `DATASET_NOT_DECLARED` finding
  below: a DD the job stream allocates that no `SELECT` in scope assigns to.

### Findings from keeping the three sources apart (D28)

| Finding | Count |
|---|---|
| `DATASET_NOT_DECLARED` — a dataset the job stream supplies that no `SELECT` in scope declares | 194 |
| `FILE_NOT_SUPPLIED` — a `SELECT` whose DD no step running that program supplies | 16 |

Both are invisible in a design that merges the three sources into one row. The
194 is dominated by `STEPLIB`/`JOBLIB`/`SYSPRINT`-class DDs and by utility
steps (IDCAMS, SORT) whose programs are not COBOL in this tree — which is
itself the honest reading: *no COBOL we can see declares these*.

---

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
  nine agreements are evidence about record *lengths*, not about every byte of
  every field.
* **That CardDemo's copybooks describe CardDemo's data.** They describe the
  lengths the job stream declares. Whether the bytes on the volume match is not
  determinable from source, and this tool never opens a volume to find out.
* **Completeness of the lineage graph.** See §3 and the six disclosed
  categories.
