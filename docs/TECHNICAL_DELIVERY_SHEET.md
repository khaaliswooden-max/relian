# Relian — Technical Delivery Sheet

**What this sheet is for.** Everything a recipient needs to check a Relian
artifact without trusting Visionblox: which key signs which class of claim,
which tool checks it, and what each tool's own SHA-256 is so the recipient can
confirm they are running the tool that was shipped to them.

It closes **D13**, outstanding since WP-2.1: the signing fingerprints needed to
reach a published surface alongside the verification command, rather than
living in a commit message.

---

## 1. Signing keys — two keys, two claim classes (D23)

| Fingerprint | Key | Signs | Does **not** sign |
|---|---|---|---|
| `233bb4406e2de606` | RELIAN benchmark seal key | `bench/LEDGER_relian-bench-v1.2.json`, `discovery-bench/LEDGER_relian-discovery-bench-v0.1.json` | customer deliverables of any kind |
| `91e3a404155ba4dd` | Visionblox release key | the countersignature on a Data Discovery report (`report.countersig.json`) | benchmark seals |

**Why two keys and not one.** Reports and benchmarks are different claim
classes. If the report key is ever rotated or compromised, that event must not
put a sealed benchmark in question, and the reverse is equally true. A single
key would make every such event contaminate both. `tools/countersign.py`
refuses the benchmark fingerprint outright when asked to sign a report.

**Provenance of each value, stated rather than assumed (R9).**

| Value | Grade | Basis |
|---|---|---|
| `233bb4406e2de606` | VERIFIED | it is the fingerprint recorded in the two sealed ledgers in this repository, and `tools/verify_manifest.py --pin-fingerprint` checks against it on every CI run |
| `91e3a404155ba4dd` | PLAUSIBLE | specified by the operator in the WP-2.3 brief (D22/D23). The release key is in the operator's custody and never enters this repository, CI, or an agent session (R4), so nothing in this repository has computed this fingerprint from a key. It is transcribed, and it is checked the first time a real countersignature is produced — `tools/countersign.py` refuses to sign if the key it is handed does not fingerprint to this value |

The second row is the honest statement of what is and is not known here. A
recipient who pins `91e3a404155ba4dd` is pinning a value published by
Visionblox; the pin proves the countersignature came from the key Visionblox
named, which is exactly the claim a countersignature is for.

---

## 2. Tools, and their own digests

| Tool | SHA-256 | Checks |
|---|---|---|
| `tools/verify_report.py` | `43f0bf20a79bb5d07aecb619dc2b7364469769ea2d81a4679e9b1b588bc4cbd5` | a Data Discovery report: files, manifest, instance signature, countersignature |
| `tools/countersign.py` | `c245426e706cb477f5cd32bac0b019a04f1effdf2fbc45d39b36240d9600b6e1` | operator-side only: produces a countersignature from a manifest hash |
| `tools/verify_manifest.py` | `898a268e8c51e408ea92bfb910d742f57d66fea7f84b6d33cbf1563f21517c2a` | a sealed benchmark ledger: tree, payload, signature |

These digests are **pinned by the test suite**
(`tests/test_delivery_sheet.py`), so editing a tool without updating this sheet
turns the build red. A digest published in a document that can go stale in
silence is a digest nobody should rely on.

`verify_report.py` prints its own SHA-256 on every run. Compare it with the
table above before trusting anything else it says.

---

## 3. Verifying a Data Discovery report

    python3 tools/verify_report.py \
        --report-dir <the delivered directory> \
        --pin-fingerprint 91e3a404155ba4dd

`--pin-fingerprint` is **required and has no default**. Without a pin, a report
re-signed under any key at all passes every internally-consistent check: the
files match the manifest, the manifest hash recomputes, and the signature
verifies under the key the forger generated. The pin is the only layer that
sees it.

Four layers are reported, each named and each failing independently:

| Layer | What it proves |
|---|---|
| FILES | every artifact recorded in the manifest is on disk and hashes to its recorded digest |
| MANIFEST | the manifest has not been edited since it was signed |
| INSTANCE | the report came out of the Relian installation the manifest records, and has not changed since |
| COUNTERSIGNATURE | Visionblox attests to this report |

### Exit codes

| Code | Meaning |
|---|---|
| 0 | `VALID AND ATTESTED` — all four layers passed |
| 3 | `VALID AND UNATTESTED` — the report is intact and produced by the recorded Relian installation, and Visionblox has **not** attested to it |
| 1 | a named layer failed |
| 2 | the deliverable could not be read at all |

**Exit 3 is not a warning to be ignored.** The instance key is generated on the
customer's own machine and Visionblox never holds it, so the instance layer
proves integrity and provenance-of-tool — not identity of signer. A report with
a good instance signature and no countersignature has not been attested to by
anyone but the tool that produced it. Reading it as a Visionblox signature
would be a misstatement about who stands behind the document.

---

## 4. Verifying a sealed benchmark

    python3 tools/verify_manifest.py \
        --ledger discovery-bench/LEDGER_relian-discovery-bench-v0.1.json \
        --from-manifest --pin-fingerprint 233bb4406e2de606

    python3 tools/verify_manifest.py \
        --ledger bench/LEDGER_relian-bench-v1.2.json \
        --root bench --include-dirs corpus,harness --include-files SPEC.md \
        --pin-fingerprint 233bb4406e2de606

The v1.2 invocation carries its include rules on the command line because the
v1.2 manifest does not record them; v0.1 records its own inside the signed
payload, so `--from-manifest` is enough. That difference is a known and stated
limitation of the older format, not an oversight in the newer one.

---

## 5. What Relian does not claim

Repeated here because a capability matrix is only useful where a reader will
meet it.

* **No risk scoring of any kind.** There is no model. A number produced without
  one would be a number with no measurement behind it.
* **No claim about any language pair other than COBOL.** No benchmark, no
  transpiler, no claim.
* **No claim of equivalence with IBM Enterprise COBOL.** The layout engine is
  verified byte-for-byte against GnuCOBOL 3.1.2.0 on
  RELIAN-DISCOVERY-BENCH v0.1, 186 of 186 comparisons at tolerance zero. IBM
  equivalence is UNMEASURED and every layout is graded PLAUSIBLE for that
  reason, with the limitation stated in the report's top matter.
* **The ledger is an Ed25519 hash chain.** It is not anchored to any external
  system, and nothing in this product is.
