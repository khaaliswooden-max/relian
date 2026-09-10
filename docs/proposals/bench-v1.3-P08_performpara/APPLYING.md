# Applying `seal-v1.3.patch`

> ## ⚠ VERSION NUMBER SUPERSEDED — this proposal is now v1.4, not v1.3
>
> **Read this before applying anything in this directory.**
>
> `v1.3` was taken by **WP-2.6**, a pure RE-SEAL of the existing tree with no
> corpus change. That re-seal is not optional and could not wait for this
> proposal: commit `153f40f` removed `sign()`'s silent-keygen fallback from
> `bench/harness/commit.py`, `bench/harness/` is an include dir of the v1.2
> seal, and the tree therefore stopped matching the manifest. `main` went red
> and the re-seal is what fixes it.
>
> **This proposal's substance is unaffected and still stands.** Out-of-line
> `PERFORM` remains the largest single blocker and R7 still requires sealed
> coverage before any implementation merges. Only the version number moves:
> this becomes **v1.4**, sealed on top of v1.3.
>
> **Two things in `seal-v1.3.patch` are now actively wrong, not merely stale:**
>
> 1. **It edits `bench/harness/commit.py`** to bump the version, tag and output
>    path. That is precisely the loop WP-2.6 exists to break -- `commit.py` is
>    inside the manifest it produces, so editing it to seal a version
>    invalidates the version it just sealed, and the next commit needs another
>    re-seal. Seal with `tools/seal.py --config bench/seal.toml` instead, which
>    is unsealed, parameterised, and refuses both an absent key and an
>    `UNAVAILABLE` toolchain probe. A v1.4 needs a new `[seal]` block in that
>    config, not a patch to `commit.py`.
>
> 2. **Its "two things this patch deliberately does NOT do" section says
>    `sign()` still mints a fresh keypair when the key is absent.** That was
>    true when written and is not true now -- `153f40f` fixed it, and fixing it
>    is what caused WP-2.6.
>
> Also note `baselines_recorded`: a v1.4 must carry v1.2's block forward
> byte-identically, as v1.3 does (`[carry_forward]` in `bench/seal.toml`). It
> is the measured floor from before any solution work, and `commit.py`
> re-derives it from the unsealed, mutable `bench/results/` at seal time.
>
> Verified 2026-09-10: `git apply --check seal-v1.3.patch` still succeeds, so
> this banner is the only thing standing between that patch and a re-broken
> seal.

A ready-to-apply patch for the two mechanical harness edits the v1.3 seal needs.
It is a **proposal document** — the agent did not and cannot edit `bench/` (rule
4). You apply it, with your own hands, so the sealed state is authored by you.

## What the patch changes

Two files, four lines, no logic:

- `bench/harness/mains.json` — adds `"P08_performpara": "Perfpar01"`.
- `bench/harness/commit.py` — `version` `1.2.0 → 1.3.0`, `tag`
  `relian-bench-v1.2 → relian-bench-v1.3`, and the output ledger filename
  `LEDGER_relian-bench-v1.2.json → LEDGER_relian-bench-v1.3.json`.

Verified with `git apply --check` against this branch head — it applies cleanly.

## Where it sits in the full seal (see README.md for the whole runbook)

This patch is **step 3 + step 4** only. It does not, and must not, do the parts
that are yours alone:

1. Generate the held-out split with the private generator (`relian-bench-private`).
2. Place `program.cbl` + `vectors/public.jsonl` + `run` into
   `bench/corpus/P08_performpara/`.
3. **← this patch:** register the main in `mains.json`.
4. **← this patch:** bump the version constants in `commit.py`.
5. Re-point `.github/workflows/bench.yml` (ledger filename + held-out copy step).
6. Sign with your custody key present, and confirm the printed `key_fingerprint`
   is yours.
7. Commit and tag `relian-bench-v1.3` — before any transpiler change claiming
   performed-paragraph support (R7).

## Apply

```bash
git checkout main && git pull                 # seal on the protected base
git apply --check docs/proposals/bench-v1.3-P08_performpara/seal-v1.3.patch
git apply         docs/proposals/bench-v1.3-P08_performpara/seal-v1.3.patch
```

## Two things this patch deliberately does NOT do

- **No changelog note.** `THRESHOLDS` carries a `changelog_v1_1` entry; a
  `changelog_v1_3` documenting the corpus growth (P08 added for out-of-line
  `PERFORM` + paragraph `EXIT`) is good provenance, but the wording is yours —
  left out so the diff stays minimal and low-risk.

- **No fix to `sign()`'s missing-key behavior.** As written, `sign()` mints a
  fresh keypair when `~/zil-keys/relian-bench-v1.pem` is absent (commit.py
  L138–147), so a run without your key produces a real-looking ledger signed by
  the wrong key. Before you seal, confirm the key is present and the printed
  `key_fingerprint` matches. Hardening `sign()` to hard-fail on a missing key is
  worth a separate operator commit; it is out of scope for this patch.
