# WP-2.3 dry runs — the signed Data Discovery report

Two end-to-end runs of the full pipeline: resolve → build the canonical
`report.json` → render the unsigned Markdown → build the manifest of digests →
take the **instance** Ed25519 signature → verify all four layers → emit the
countersignature request line.

**The countersignature step is deliberately not executed here.** It is an
operator key session, the same as a benchmark seal: the release key is in
Khaalis's custody (R4) and never enters this repository, CI, or an agent
session. Both runs therefore stop at `VALID AND UNATTESTED`, which is the
state the customer holds before delivery, and the request lines below are the
only thing that would cross the perimeter to change that.

| | |
|---|---|
| Anchor | `3ac6ee5` (merge of PR #30, WP-2.2) |
| Verifier | `tools/verify_report.py`, sha256 `edfdcf49e57f9f0ace727fc98c7fff30c203667e7158b7e61a87bc01a7889b97` |
| Countersign tool | `tools/countersign.py`, sha256 `b58718fd62ebf8af7b85670c5d6f6f53f0ab67c223abe2ea3d472c873ed46e87` |
| Instance key fingerprint (both runs) | `98616f9c7543d293` |
| Instance key location | `/root/relian-dryrun-home/.relian/instance-ed25519.pem`, mode 0600, generated on the first of these two runs |

The instance key is a **dry-run key generated in an ephemeral container**. It is
disclosed here for the same reason it is disclosed in the artifact: a signature
under a fingerprint nobody has seen is the defect WP-2.1 recorded in
`bench/harness/commit.py`, and disclosure is the whole difference. It signs
nothing that is delivered to anyone and it does not appear in this repository —
only its public fingerprint does.

---

## Run 1 — `examples/demo` (3 copybooks, all resolvable)

    python3 -m src.discovery.cli report build examples/demo \
        --out docs/dryruns/report_demo \
        --customer "Example Municipal Utility (dry run)" \
        --engagement "Data Discovery — WP-2.3 dry run" \
        --contract-vehicle "internal dry run, not a delivered engagement" \
        --root-label "examples/demo"

Artifacts: [`report.json`](report_demo/report.json) (signed),
[`report.md`](report_demo/report.md) (derived, unsigned),
[`report.manifest.json`](report_demo/report.manifest.json).

| | |
|---|---|
| report id | `017f5ff0fdc4457770ee467c7e9ff073` |
| manifest sha256 | `f36b5dd137575325f752f64c65dce90ec616edd9c065ad8bfa435f6da305de3a` |
| record layouts | **included** — 3 records, all `COMPLETE` |
| missing copybooks | none; every referenced member resolved |
| verifier result | FILES **PASS** · MANIFEST **PASS** · INSTANCE **PASS** · COUNTERSIGNATURE **ABSENT** → `VALID AND UNATTESTED`, exit **3** |

Counts, every one measured in the run:

| Measure | Value |
|---|---|
| source files scanned | 8 |
| files with at least one `COPY` | 2 |
| `COPY` directive sites | 4 |
| distinct names referenced | 3 |
| resolvable | 3 |
| unresolvable | 0 |
| edges | 4 |
| cycles | 0 |

Countersignature request line, verbatim:

    relian-countersign-request/1 manifest_sha256=f36b5dd137575325f752f64c65dce90ec616edd9c065ad8bfa435f6da305de3a report_id=017f5ff0fdc4457770ee467c7e9ff073 instance_fingerprint=98616f9c7543d293

---

## Run 2 — AWS CardDemo copybook set (67 names, 8 unresolvable)

    python3 -m src.discovery.cli report build ~/corpora/carddemo \
        --out docs/dryruns/report_carddemo \
        --customer "AWS CardDemo (public sample, dry run)" \
        --engagement "Data Discovery — WP-2.3 dry run" \
        --contract-vehicle "internal dry run, not a delivered engagement" \
        --root-label "aws-mainframe-modernization-carddemo @ 59cc6c2" \
        --skip-layouts "Resolver-only by decision, not by failure. …"

Corpus: `github.com/aws-samples/aws-mainframe-modernization-carddemo` at
`59cc6c2fd7ebd7ef7925cad552a01a4b8b6e4d5e`, Apache-2.0, cloned to
`~/corpora/carddemo` — **outside** this repository. No CardDemo bytes are
committed here; the report records digests and paths, not source.

Artifacts: [`report.json`](report_carddemo/report.json) (signed),
[`report.md`](report_carddemo/report.md) (derived, unsigned),
[`report.manifest.json`](report_carddemo/report.manifest.json).

| | |
|---|---|
| report id | `2fff4d2377c2cc199d6271bd1c21f885` |
| manifest sha256 | `04b599fa41e984fc54eba4c48d6bfdb43de8ab74194fb59b9b127b5c8f3cecac` |
| record layouts | **not included, by decision** — see below |
| missing copybooks | **8**, populated with counts measured in this run |
| verifier result | FILES **PASS** · MANIFEST **PASS** · INSTANCE **PASS** · COUNTERSIGNATURE **ABSENT** → `VALID AND UNATTESTED`, exit **3** |

### Counts — measured in this run, and they agree with WP-2.2

Nothing below is transcribed from the WP-2.2 dry run; the WP-2.2 column is
shown only so drift would be visible.

| Row | **This run** | WP-2.2 | Drift |
|---|---|---|---|
| Source files scanned | **106** | 106 | none |
| Files with ≥1 `COPY` | **40** | 40 | none |
| `COPY` directive sites | **346** | 346 | none |
| Distinct names referenced | **67** | 67 | none |
| — resolvable | **59** | 59 | none |
| — **not** resolvable | **8** | 8 | none |
| Edges | **306** | 306 | none |
| Maximum fan-out | **18**, `app/cbl/COACTUPC.cbl` | 18, same file | none |
| `COPY … REPLACING` sites | **40** | 40 | none |
| Members present but unreferenced | **3** | 3 | none |
| Cycles | **0** | 0 | none |

Had they disagreed, the run would be authoritative and this table would say so.

### The missing-copybook table — the finding, and it leads the report (D26)

| Missing copybook | Referenced by (files) | `COPY` sites | Directories searched |
|---|---|---|---|
| `DFHAID` | 21 | 21 | 51 |
| `DFHBMSCA` | 21 | 21 | 51 |
| `CMQGMOV` | 3 | 3 | 51 |
| `CMQMDV` | 3 | 4 | 51 |
| `CMQODV` | 3 | 4 | 51 |
| `CMQPMOV` | 3 | 3 | 51 |
| `CMQTML` | 3 | 3 | 51 |
| `CMQV` | 3 | 3 | 51 |

**The report does not print a "likely source" column, and the WP-2.2 dry-run
note did.** That is a deliberate narrowing. "CICS-supplied" and "IBM
MQ-supplied" are correct, and they are also *inferences about a member this
engine never saw*, printed in a table where every other cell is a count taken
in the run. In an internal dry-run note that is a useful annotation; in a
customer artifact governed by R1 it is an inference wearing the costume of a
finding. What is measured — the referrer list and the number of directories
searched — is in the report; what is inferred stays in the engagement
conversation where it can be attributed to a person.

### Why section 4 is empty here

Resolver-only, **by decision rather than by failure**, and the report says which
decision in the section itself:

> Resolver-only by decision, not by failure. WP-2.2 section 10 established that
> no layout claim is made about any CardDemo record: the layout engine is
> verified against GnuCOBOL 3.1.2.0 and CardDemo is IBM-dialect source, so a
> layout here would be graded against nothing. Copybook resolution is
> dialect-independent and is unaffected.

`Report.build` **refuses** to emit an empty layout section over a tree with
resolvable copybooks unless a reason is supplied. An empty section with no
stated reason reads as a clean result (R2), and that refusal is tested.

Countersignature request line, verbatim:

    relian-countersign-request/1 manifest_sha256=04b599fa41e984fc54eba4c48d6bfdb43de8ab74194fb59b9b127b5c8f3cecac report_id=2fff4d2377c2cc199d6271bd1c21f885 instance_fingerprint=98616f9c7543d293

---

## What the operator does next, and what does not travel

    python3 tools/countersign.py \
        --manifest-hash <the 64 hex from the line above> \
        --report-id <the report id from the line above> \
        --instance-fingerprint 98616f9c7543d293 \
        --key ~/zil-keys/visionblox-release-key-v1.pem \
        --out report.countersig.json

Then `report.countersig.json` goes back to the customer, who drops it next to
`report.json` and re-runs the verifier — which now returns **0** and
`VALID AND ATTESTED`.

Three digests cross the perimeter. No file, no path fragment, no field name and
no customer identifier. `tests/test_discovery_report.py` builds a report from a
tree whose paths, copybook names, field names and every free-text field carry
distinctive marker strings, and asserts that none of them reaches the request
line — so a leak is a red test rather than a review catch.

The release key never enters this repository. `tools/countersign.py` refuses an
absent key with no generation fallback, refuses a hash that is not exactly 64
hexadecimal characters, and refuses the benchmark key `233bb4406e2de606`
outright: reports and benchmarks are different claim classes and a report-key
rotation must never put a sealed benchmark in question (D23).

## Verifying these committed artifacts yourself

    python3 tools/verify_report.py --report-dir docs/dryruns/report_demo \
        --pin-fingerprint 91e3a404155ba4dd
    # → VALID AND UNATTESTED, exit 3

Both directories are re-verified by `tests/test_verify_report.py` on every run
of the suite, so an edit to either committed `report.json` turns the build red.
