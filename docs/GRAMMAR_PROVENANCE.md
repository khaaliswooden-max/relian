# Grammar provenance

Every third-party grammar file in this repository, where it came from, and
under what licence. Written for WP-2.0, which replaced the reduced in-house
COBOL-85 grammar with the upstream ProLeap grammar.

A vendored file is a file this project redistributes. The claim that it may be
redistributed has to rest on something checkable, so each row below carries the
commit it was taken from and the SHA-256 of the bytes as vendored — not a
version label, which can move.

---

## Vendored files

| | |
|---|---|
| **File** | `src/parsers/grammars/Cobol85.g4` |
| **Author** | Ulrich Wolffgang `<ulrich.wolffgang@proleap.io>` |
| **Copyright year** | 2017 |
| **Licence** | MIT — see `docs/licenses/proleap-cobol85-MIT.txt` |
| **Upstream project** | https://github.com/uwol/proleap-cobol-parser (the grammar's own header names its predecessor, https://github.com/uwol/cobol85parser) |
| **Vendored from** | https://github.com/antlr/grammars-v4, path `cobol85/Cobol85.g4` |
| **Repo commit** | `aca577d9e30e591eacbc414f1280f22645412af4` (2026-08-18) |
| **File's own last commit** | `753536777d827ccc0c9b108531ea67375c2039ac` (2023-11-29, "Reformatting all grammars (#3843)") |
| **sha256** | `c338bff84b5a7d89113dacdff69764593688fd0915f24fba2f07a5fec2063e35` |
| **Size** | 5654 lines · 595 parser rules · 565 lexer rules |
| **Modified?** | No. Byte-identical to upstream, header block intact. |

| | |
|---|---|
| **File** | `src/parsers/grammars/Cobol85Preprocessor.g4` |
| **Author** | Ulrich Wolffgang `<ulrich.wolffgang@proleap.io>` |
| **Copyright year** | 2017 |
| **Licence** | MIT — see `docs/licenses/proleap-cobol85-MIT.txt` |
| **Upstream project** | https://github.com/uwol/proleap-cobol-parser |
| **Vendored from** | https://github.com/antlr/grammars-v4, path `cobol85/Cobol85Preprocessor.g4` |
| **Repo commit** | `aca577d9e30e591eacbc414f1280f22645412af4` (2026-08-18) |
| **File's own last commit** | `753536777d827ccc0c9b108531ea67375c2039ac` (2023-11-29, "Reformatting all grammars (#3843)") |
| **sha256** | `8d88a679ae574a2645c827c21f467031669e2713d149c8fec46bc0dab86b4841` |
| **Size** | 1902 lines · 30 parser rules · 292 lexer rules |
| **Modified?** | No. Byte-identical to upstream, header block intact. |

| | |
|---|---|
| **File** | `docs/licenses/proleap-cobol85-MIT.txt` |
| **Content** | The MIT licence text the two grammar headers point at, verbatim |
| **Fetched from** | https://github.com/uwol/proleap-cobol-parser/blob/main/LICENSE |
| **sha256** | `5de028e49764aa5f3212085092085b3c26350cb68d73535264667f96b05a98ac` |
| **Modified?** | No. 21 lines, as served. |

Reproduce the hashes:

```bash
sha256sum src/parsers/grammars/Cobol85.g4 \
          src/parsers/grammars/Cobol85Preprocessor.g4 \
          docs/licenses/proleap-cobol85-MIT.txt
```

---

## The licence question, and how it was closed

`docs/PHASE2_LOG.md` §0.7 left this **UNRESOLVED** before vendoring, and the
objection was correct: `antlr/grammars-v4` carries **no `LICENSE` file** at its
repository root or in `cobol85/`, so the MIT designation rested only on a file
header pointing at a licence file that is not in that repository.

It is resolved by looking where the header actually points. The header names
the ProLeap COBOL parser as the grammar's home; that project **does** carry the
MIT text, and it is the file archived above. Two things were checked before
either grammar was copied, and both are mechanical rather than a reading of
intent:

**1. The copyright block is byte-identical across the two grammars.** The work
package made vendoring the preprocessor conditional on it carrying the same
`Copyright (C) 2017, Ulrich Wolffgang` / MIT block as the main grammar. It
does — not merely equivalent, identical:

```bash
sha256sum <(sed -n '1,7p' Cobol85.g4) <(sed -n '1,7p' Cobol85Preprocessor.g4)
→ 614ee811d5e6ce31a3f2bc511901aed2828ca3e5d27f591cf0ac2b61291fd2e3  (both)
diff <(sed -n '1,7p' Cobol85.g4) <(sed -n '1,7p' Cobol85Preprocessor.g4)
→ (no output)
```

**2. The licence text was fetched from the project the header names**, not
from a search result, and archived verbatim rather than retyped. It is the
standard MIT text, `Copyright (c) 2017 Ulrich Wolffgang`.

Both header blocks are kept intact in the vendored files, which is what the
MIT licence requires of a redistribution ("The above copyright notice and this
permission notice shall be included in all copies").

---

## Generated code

`src/parsers/antlr/cobol/` is **generated** from `Cobol85.g4`, not written. It
is committed so that a checkout can parse COBOL without a Java toolchain, and
it is derived from an MIT-licensed grammar, so the same licence and attribution
apply to it.

Committed generated code is only trustworthy while it still matches its source.
Two things enforce that:

* `tools/regen_parser.sh` is the only sanctioned way to produce it. It pins
  ANTLR **4.13.2** by SHA-256 (`eae2dfa1…4d76`) and verifies the jar on every
  run, because the serialized ATN differs between ANTLR releases and an
  unpinned generator would produce an unexplained diff.
* `.github/workflows/tests.yml`, job **`parser-regen`**, runs
  `tools/regen_parser.sh --check` on every push and pull request. It
  regenerates into a temporary directory and fails the build on any byte
  difference, on a missing file, or on an unexpected extra file in the output
  directory.

| | |
|---|---|
| **Generator** | ANTLR 4.13.2, `antlr-4.13.2-complete.jar` |
| **Jar sha256** | `eae2dfa119a64327444672aff63e9ec35a20180dc5b8090b7a6ab85125df4d76` |
| **Jar source** | https://www.antlr.org/download/antlr-4.13.2-complete.jar — verified byte-identical to Maven Central's `org/antlr/antlr4/4.13.2/antlr4-4.13.2-complete.jar` |
| **Runtime** | `antlr4-python3-runtime==4.13.2`, pinned in `requirements.lock` |
| **Command** | `tools/regen_parser.sh` |

The jar itself is **not** committed; `tools/regen_parser.sh` fetches it to
`.antlr/` (git-ignored) and checks its hash before use.

---

## Why the preprocessor grammar is vendored but not generated

`Cobol85Preprocessor.g4` is the other half of the upstream pair. `Cobol85.g4`'s
own header says it is "to be used in conjunction with the provided
preprocessor, which executes COPY and REPLACE statements", and that is not
advisory: **`COPY` is a lexer token in `Cobol85.g4` that no parser rule
references.** A token no rule can consume is a guaranteed syntax error, so a
COPY-bearing program cannot parse cleanly under the main grammar alone. The
measurement behind that claim is in `docs/PHASE2_LOG.md` §0.3.

It is vendored so that the pair travels together, with provenance recorded
once, rather than being re-fetched later from a commit nobody wrote down. It is
**not** generated, because nothing imports it yet and committing an unused
1900-rule parser would be dead weight that the `parser-regen` gate would then
have to police. Wiring it in is its own work package.

Two consequences are live today and are handled explicitly rather than silently:

* `src/assessment/coverage.py` falls back to its documented `token_scan`
  method, graded PLAUSIBLE, for any program that reports syntax errors —
  COPY-bearing ones included.
* `src/parsers/cobol.py` recovers `COPY <name>` dependency edges from the
  **token stream** rather than the tree, and stamps every such node
  `recovered_by="token_scan"` so a consumer can tell a scanned edge from a
  parsed one.

The preprocessor would also be needed for comment entries — the free text after
`AUTHOR.`, `INSTALLATION.` and friends is reachable only through a `*>CE`
marker that upstream's preprocessor inserts.

---

## What was replaced

Before WP-2.0 this repository carried a hand-written reduced COBOL-85 subset at
the same path, of unrecorded provenance:

| | |
|---|---|
| **File** | `src/parsers/grammars/Cobol85.g4` (superseded) |
| **sha256** | `eb88e8c1a8d570c59271924e547983a107405752bf971fdbd2f3f2ac787a89bf` |
| **Size** | 376 lines · 119 parser rules · 201 lexer rules |

It parsed **zero** of the seven bench programs without error. The vendored
grammar parses all seven cleanly. The before/after measurements, and the
constructs that still defeat it, are in `docs/PHASE2_LOG.md` under WP-2.0.
