# Held-out input PROPOSALS for v1.2 — UNSEALED, NOT VECTORS

**These files are input lists only. They carry NO expected outputs and are
NOT part of any benchmark split.** They exist to save keystrokes at the
v1.2 sealing (Part B): Khaalis regenerates the authoritative held-out
vectors — inputs and oracle-measured expected outputs — with the private
generator in `relian-bench-private`, which remains the sole source of
held-out vectors (R3; CLAUDE.md rules 1 and 6). The private generator is
free to use, subset, or ignore these proposals.

| File | Inputs | Matches |
|---|---|---|
| `P06_valinit.inputs.UNSEALED.jsonl` | 60 | existing per-program held-out count (60) |
| `P07_exitflow.inputs.UNSEALED.jsonl` | 60 | existing per-program held-out count (60) |
| `P04_taxtable.inputs.UNSEALED.jsonl` | 5 | the five SEARCH-exhaust additions (draft `P04_search_exhaust.md`) |

Properties, all checked this session against GnuCOBOL 3.1.2 oracles
compiled from the committed `program.cbl` sources:

- Every input executes (SPEC §3: if the legacy will not execute for an
  input, no vector is emitted).
- All inputs are disjoint from the corresponding public split.
- `P07_exitflow` proposals include nonzero-RETURN-CODE modes (`E` → 8,
  `W` → 4), legitimate since WP-1.5.0d; none depends on the lone-`EXIT
  PROGRAM`-in-`WHEN` chaining quirk (structurally impossible — the
  program keeps `EXIT PROGRAM` inside a multi-statement `IF`).
- `P06_valinit` deltas keep `|BAL| ≤ 9999999.99`, so no vector depends on
  unspecified overflow behavior.
- `P04_taxtable` proposals sit inside the AT END window
  `(999999999.00, 999999999.99]` identified in the sealing-review draft.

Expected outputs were measured locally only to confirm executability and
were deliberately **not** recorded here. Files are named `*.inputs.*` —
nothing in this directory may ever be named `heldout.jsonl`.
