# Applying `seal-v1.3.patch`

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
