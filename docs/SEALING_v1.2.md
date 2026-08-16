# RELIAN-BENCH v1.2 — sealing prep (Part A) and signing runbook (Part B)

**Status: Part A complete (this PR, unsealed). Part B is Khaalis-only
(ZCS-6 Phase 4, R4 key custody, R3 private-repo custody).** Nothing in
this PR touches `relian-bench-private`, signing keys, or CI secrets.

---

## A0 — Ground truth (verified against the working tree, 2026-08-16)

### (a) Exact paths of the three tools

| Tool | Path | Facts |
|---|---|---|
| Vector generator | **NOT in this repository.** `gen_vectors.py` + seed live only in `relian-bench-private` (CLAUDE.md rule 6) | The v1.1 ledger's file manifest nevertheless lists `harness/gen_vectors.py` — at signing time it sits in the signing machine's working tree at `bench/harness/gen_vectors.py` and is hash-committed (sha256 in the manifest) without being committed to git. `.gitignore` now guards that path. `bench/SPEC.md` §12 references `python3 harness/gen_vectors.py` in its reproduce block; that path intentionally does not resolve in a public checkout. |
| Manifest builder / hasher | `bench/harness/commit.py` | `build_manifest()` walks `INCLUDE_DIRS = ["corpus", "harness"]` and `INCLUDE_FILES = ["SPEC.md"]` (paths relative to `bench/`), records per-file sha256 with `Path.as_posix()` paths sorted by posix string (R8), and computes `payload_sha256`. `manifest_hash()` = sha256 over the sorted-keys JSON of the manifest minus its `signature` block. Exclusions: `__pycache__`, `_classes`, `.pyc`/`.o`, and binaries named `payroll01`/`run`. |
| Signing / verify | `bench/harness/commit.py` — `sign(manifest, key_path)` and `verify(manifest)` | Ed25519. `__main__` signs with `Path.home() / "zil-keys" / "relian-bench-v1.pem"` — key custody is Khaalis-only (R4); the key never enters this repo or CI. CI verifies via `from harness.commit import verify` (bench.yml step “Verify committed ledger signature”). |

### (b) Ledger reference the gate reads (`.github/workflows/bench.yml`)

Both steps hard-code the ledger **filename**; there is no indirection:

```python
# step "Verify committed ledger signature"
m = json.load(open('bench/LEDGER_relian-bench-v1.1.json'))
assert verify(m), 'LEDGER signature invalid — benchmark tampered'
print('ledger verified:', m['signature']['manifest_sha256'][:16])

# step "Score candidate on HELD-OUT split"
ledger = json.load(open('bench/LEDGER_relian-bench-v1.1.json'))
thresholds = ledger['thresholds']
...
'ledger_ref': {
    'tag': ledger.get('tag'),
    'manifest_sha256': ledger['signature']['manifest_sha256'],
},
```

Field names: top-level **`tag`** (currently `"relian-bench-v1.1"`) and
**`signature.manifest_sha256`** (currently
`8756173e6fc136f6fa374ddc2f592fa5e69d8e29c7aece32f65de998603a7189`).
Thresholds consumed: `ber_heldout_min`, `build_rate_min`,
`branch_coverage_min`. The gate verifies the ledger's **signature over
itself**; it does not re-hash the working tree against the manifest —
which is why this PR's corpus changes leave CI green, and why Part B's
re-hash + re-sign is what actually seals them.

### (c) The v1.1 re-version rule (quoted)

`bench/harness/commit.py`, `THRESHOLDS["changelog_v1_1"]`:

> "ADDITIVE: JaCoCo branch-coverage measurement wired into the runner.
> Thresholds unchanged. Corpus and vectors unchanged (hash-identical).
> Reason for re-version: harness files changed; ZCS-6 forbids silent
> edits to a committed benchmark."

Same rule in `bench/SPEC.md`, v1.1 Addendum: "Re-versioned and re-signed
because harness files changed — ZCS-6 forbids silent edits to a committed
benchmark." v1.2 triggers the rule twice over: harness files changed
after v1.1 (WP-1.5.0d touched `harness/runner.py` and
`harness/coverage.py`, disclosed in `docs/PHASE1_LOG.md`) **and** the
corpus changes in this PR.

### (d) How a new program lands in both splits

The join key is the **program directory name**, which must satisfy all
four of these simultaneously:

1. **Public split**: `bench/corpus/<NAME>/vectors/public.jsonl` in this
   repo (with `bench/corpus/<NAME>/program.cbl` as the oracle source).
2. **Held-out split**: `corpus/<NAME>/vectors/heldout.jsonl` in
   `relian-bench-private`. CI copies it in with
   `for d in /tmp/bench-private/corpus/P*/; do n=$(basename $d);
   cp $d/vectors/heldout.jsonl bench/corpus/$n/vectors/; done` —
   so the directory name must match **exactly** and must match the
   **`P*` glob** (as must `bench/corpus/P*/` in the “Build oracles”
   step: `cobc -x program.cbl -o oracle`).
3. **`bench/harness/mains.json`**: `{"<NAME>": "<JavaMainClass>"}` entry —
   this map drives both candidate generation (bench.yml) and scoring
   (`harness/runner.py::run_candidate`).
4. **Ledger**: `build_manifest()`'s `vector_counts` picks up every
   `corpus/**/vectors/*.jsonl` present in the signing working tree, and
   the file manifest hashes both splits (held-out by hash only).

Hence the promotion names `P06_valinit` and `P07_exitflow`.
**`bench/harness/mains.json` is deliberately NOT updated in this PR**: CI
scores every program in that map on `heldout.jsonl`, which for P06/P07
exists only after Part B — adding them now would make the gate red with
`FileNotFoundError`, not with an honest measurement. The proposed v1.2
map is committed at `scripts/mains_v1.2_proposed.json`, and
`scripts/bench_public.py --mains` scores against it locally.

---

## What Part A changed (all unsealed until Part B)

- **Corpus 5 → 7.** `bench/candidates/drafts/{VALINIT01,EXITFLW01}` →
  `bench/corpus/P06_valinit/`, `bench/corpus/P07_exitflow/`. EXITFLW01
  revised to **rev 3**: modes `E`/`W` exercise `GOBACK` with explicit
  nonzero `RETURN-CODE` (8/4), legitimate since WP-1.5.0d. 12 public
  vectors per program, generated from GnuCOBOL **3.1.2** oracles
  (`cobc (GnuCOBOL) 3.1.2.0`, same as CI and the v1.1 ledger toolchain).
  P07 includes **4 nonzero-exit vectors** (2× exit 8, 2× exit 4). No
  vector depends on the lone-`EXIT PROGRAM`-in-`WHEN` chaining quirk
  (structurally excluded — `EXIT PROGRAM` sits inside a multi-statement
  `IF`).
- **`expected_exit` materialized** on every public vector (this is the
  intended v1.2 divergence from v1.1, which recorded exit expectations
  only implicitly). Existing 60 vectors: re-verified against freshly
  compiled oracles this session — all 60 reproduce with exit 0 —
  then rewritten with explicit `"expected_exit": 0`. New vectors:
  measured.
- **P04 +5 SEARCH-exhaust vectors** in the public split (17 total),
  re-measured from the oracle; expected outputs byte-match the draft
  analysis (`bench/candidates/drafts/P04_search_exhaust.md`).
- **Held-out input proposals** (inputs only, UNSEALED):
  `bench/candidates/heldout_proposals_v1.2/` — 60 + 60 + 5, disjoint from
  the public splits, executability-checked.
- **Guard rails**: `.gitignore` now also covers
  `bench/harness/gen_vectors.py`; `scripts/bench_public.py` gained
  `--mains` and an uncompilable failure stub mirroring bench.yml's (so a
  transpile failure is visible in `build_rate`, not hidden by a
  comment-only compilation unit).

### Measured pre-implementation baseline (public split, this session)

`python3 scripts/bench_public.py --out /tmp/c1_v12 --mains
scripts/mains_v1.2_proposed.json`:

| Program | build | BER (public) | branch cov |
|---|---|---|---|
| P01–P03, P05 | ok | 1.0 (12/12 each) | 1.0 / 1.0 / 0.8 / 0.8182 |
| P04_taxtable | ok | **1.0 (17/17)** | **0.8125** (11/16 → 13/16, as the draft predicted) |
| P06_valinit | ok (compiles) | **0.0 (0/12)** — C1 discards VALUE; zero-init diverges on vector 1 | 0.8 |
| P07_exitflow | **transpile failed** (honest uncompilable stub) | 0 over 12 | — |
| **Aggregate** | **0.8571 (6/7)** | **0.7303 (65/89)** | 0.8333 |

The five sealed programs' generated Java is **byte-identical** to the
WP-1.5.0d baseline (all five sha256 match `docs/PHASE1_LOG.md`). The red
on P06/P07 is the point: bench lands before handlers (R7).

---

## Part B — signing runbook (Khaalis-only; exact steps)

**In `relian-bench-private`:**

1. Add `corpus/P06_valinit/` and `corpus/P07_exitflow/` (copy the two
   `program.cbl` from this PR's merge candidate, byte-exact).
2. Run the private generator to produce `heldout.jsonl` for both (60
   vectors each, matching existing per-program counts), with explicit
   `expected_exit` on every vector. The UNSEALED input proposals in
   `bench/candidates/heldout_proposals_v1.2/` may be used, subset, or
   ignored. For P07 include nonzero-exit vectors (modes `E`→8, `W`→4);
   exclude anything depending on the lone-`EXIT PROGRAM` quirk.
3. Decide the P04 held-out additions (5 in-window inputs proposed,
   60 → 65) and regenerate P04's `heldout.jsonl` accordingly.
4. Materialize `expected_exit: 0` in the five existing programs'
   `heldout.jsonl` (regenerate, or rewrite — the harness default makes
   this behavior-neutral, but v1.2 should carry it explicitly on both
   splits).

**In this repo, on this PR branch (signing machine):**

5. Place the private files in the working tree (never `git add` them):
   `bench/harness/gen_vectors.py` and each
   `bench/corpus/*/vectors/heldout.jsonl` — both now gitignored; the
   manifest hash-commits them. Verify with `git status` before every
   commit.
6. `bench/harness/mains.json` ← contents of
   `scripts/mains_v1.2_proposed.json` (7 programs).
7. `bench/harness/commit.py`:
   - `build_manifest()`: `"version": "1.2.0"`, `"tag": "relian-bench-v1.2"`.
   - `THRESHOLDS`: add `changelog_v1_2` (text below). Thresholds
     themselves unchanged.
   - `__main__`: fix the output path — it still writes
     `LEDGER_relian-bench-v1.0.json` (latent bug; the v1.1 file was
     evidently renamed by hand). Point it at
     `LEDGER_relian-bench-v1.2.json`.
8. `python3 bench/harness/commit.py` (key at
   `~/zil-keys/relian-bench-v1.pem`). Confirm `verify(): True` and that
   `vector_counts` shows public 12/12/12/17/12/12/12 and held-out
   60×5 (+P06/P07 60 each, P04 per step 3).
9. Commit `bench/LEDGER_relian-bench-v1.2.json` +
   `bench/harness/{mains.json,commit.py}`; update **both** ledger
   references in `.github/workflows/bench.yml`
   (`LEDGER_relian-bench-v1.1.json` → `LEDGER_relian-bench-v1.2.json`,
   in the verify step and the scoring step). Keep
   `LEDGER_relian-bench-v1.1.json` in place as the archived prior
   version (pattern: `LEDGER_archive_v1.0.json`).
10. Push to this branch. **Expected CI outcome: the held-out gate goes
    honestly RED** (C1 has no VALUE or EXIT-flow handlers yet — expected
    ≈ build_rate 6/7, BER ≈ 300/420 ≈ 0.71; expected-not-measured until
    the run). Merging a red gate is the operator's explicit bench-first
    decision (R7): the sealed bench commit must predate the handler
    merges that will turn it green.

### Proposed `changelog_v1_2` text (for `THRESHOLDS`)

> "v1.2: (1) Behavioral equivalence now includes RETURN-CODE — the
> harness compares process exit codes (WP-1.5.0d) and every vector
> carries an explicit expected_exit (0 for pre-existing vectors, which
> were all recorded against zero-exit runs; measured for new ones).
> (2) Corpus 5 → 7: P06_valinit (VALUE-clause semantics) and
> P07_exitflow (CONTINUE / EXIT PROGRAM / GOBACK, including nonzero
> RETURN-CODE 4/8). (3) P04_taxtable +5 SEARCH AT-END window vectors —
> note: WHEN and AT END print identically at the bracket edge, so these
> prove branch exercise and behavioral agreement but cannot alone
> distinguish a hard-coded bracket-5 fallback; a discriminating vector
> is a v1.3 candidate. (4) Harness files re-hashed: runner.py and
> coverage.py changed post-v1.1 (WP-1.5.0d). Thresholds unchanged.
> Re-version per the v1.1 rule: no silent edits to a committed
> benchmark."
