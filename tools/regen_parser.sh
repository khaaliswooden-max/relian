#!/usr/bin/env bash
#
# Regenerate the committed ANTLR COBOL-85 parser from the vendored grammar.
#
# The generated Python under src/parsers/antlr/cobol/ is committed so that a
# checkout parses COBOL without a Java toolchain. Committed generated code is
# only trustworthy if it can be shown to still match its source, so this script
# is the single definition of how it is produced, and CI re-runs it and fails
# the build on any byte difference (.github/workflows/tests.yml, job
# `parser-regen`). Nothing here may be run by hand with different flags: the
# flags below are part of the artifact.
#
# Usage:
#   tools/regen_parser.sh              # regenerate in place
#   tools/regen_parser.sh --check      # regenerate to a temp dir and diff
#
# Requires: a JRE, and the pinned ANTLR jar (fetched to ANTLR_JAR if absent).

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# --- pins ------------------------------------------------------------------
# The ANTLR version is a pin, not a floor: the generated files carry the
# generator version in their header and the serialized ATN differs between
# releases, so an unpinned ANTLR breaks the byte-identity check by design.
ANTLR_VERSION="4.13.2"
ANTLR_SHA256="eae2dfa119a64327444672aff63e9ec35a20180dc5b8090b7a6ab85125df4d76"
ANTLR_URL="https://www.antlr.org/download/antlr-${ANTLR_VERSION}-complete.jar"
ANTLR_JAR="${ANTLR_JAR:-${REPO_ROOT}/.antlr/antlr-${ANTLR_VERSION}-complete.jar}"

# Path is relative to REPO_ROOT and is baked into the generated files' header
# comment ("# Generated from <this path> by ANTLR <version>"), so it is part of
# the byte-identity contract and must not be changed to an absolute path.
GRAMMAR="src/parsers/grammars/Cobol85.g4"
OUT_DIR="src/parsers/antlr/cobol"

# Cobol85Preprocessor.g4 is vendored alongside Cobol85.g4 (it is the other half
# of the upstream pair, and Cobol85.g4's own header requires it for COPY and
# REPLACE) but is deliberately NOT generated here: nothing imports it yet, and
# committing an unused 1900-rule parser would be dead weight. Preprocessor
# integration is its own work package. See docs/GRAMMAR_PROVENANCE.md.

GENERATED=(
    "Cobol85Lexer.py"
    "Cobol85Parser.py"
    "Cobol85Listener.py"
    "Cobol85Visitor.py"
    "Cobol85Lexer.interp"
    "Cobol85Lexer.tokens"
    "Cobol85.interp"
    "Cobol85.tokens"
)

# --- toolchain -------------------------------------------------------------
if ! command -v java >/dev/null 2>&1; then
    echo "regen_parser: java not found on PATH; a JRE is required" >&2
    exit 1
fi

fetch_jar() {
    mkdir -p "$(dirname "$ANTLR_JAR")"
    echo "regen_parser: fetching ANTLR ${ANTLR_VERSION} -> ${ANTLR_JAR}" >&2
    curl -fsSL -o "$ANTLR_JAR" "$ANTLR_URL"
}

[ -f "$ANTLR_JAR" ] || fetch_jar

# Verify the jar every run, not only after fetching. A cached jar from another
# version generates a different ATN and would surface as an unexplained diff.
actual_sha="$(sha256sum "$ANTLR_JAR" | cut -d' ' -f1)"
if [ "$actual_sha" != "$ANTLR_SHA256" ]; then
    echo "regen_parser: ANTLR jar sha256 mismatch" >&2
    echo "  expected ${ANTLR_SHA256}" >&2
    echo "  actual   ${actual_sha}" >&2
    echo "  jar      ${ANTLR_JAR}" >&2
    exit 1
fi

# --- generate --------------------------------------------------------------
generate_into() {
    local dest="$1"
    mkdir -p "$dest"
    java -jar "$ANTLR_JAR" \
        -Dlanguage=Python3 \
        -visitor \
        -listener \
        -o "$dest" \
        -Xexact-output-dir \
        "$GRAMMAR"
}

if [ "${1:-}" = "--check" ]; then
    tmp="$(mktemp -d)"
    trap 'rm -rf "$tmp"' EXIT
    generate_into "$tmp"

    status=0
    for f in "${GENERATED[@]}"; do
        if [ ! -f "$OUT_DIR/$f" ]; then
            echo "regen_parser: MISSING from the working tree: $OUT_DIR/$f" >&2
            status=1
        elif ! cmp -s "$tmp/$f" "$OUT_DIR/$f"; then
            echo "regen_parser: DIFFERS from a fresh generation: $OUT_DIR/$f" >&2
            # sed, not head: `head` closes the pipe early, and under
            # `set -o pipefail` that turns a reportable diff into a
            # SIGPIPE exit that hides the remaining files.
            diff -u "$OUT_DIR/$f" "$tmp/$f" | sed -n '1,40p' >&2
            status=1
        fi
    done

    # An extra generated-looking file in the tree is also a mismatch: it means
    # the committed set and the script's idea of it have drifted apart.
    while IFS= read -r present; do
        case " ${GENERATED[*]} " in
            *" $present "*) ;;
            *) echo "regen_parser: UNEXPECTED file in $OUT_DIR: $present" >&2; status=1 ;;
        esac
    done < <(cd "$OUT_DIR" && ls -1 | grep -E '\.(py|interp|tokens)$' | grep -v '^__init__\.py$')

    if [ "$status" -eq 0 ]; then
        echo "regen_parser: OK — committed parser is byte-identical to a fresh generation"
    else
        echo "regen_parser: FAIL — run tools/regen_parser.sh and commit the result" >&2
    fi
    exit "$status"
fi

generate_into "$OUT_DIR"
echo "regen_parser: regenerated $OUT_DIR from $GRAMMAR with ANTLR ${ANTLR_VERSION}"
sha256sum "${GENERATED[@]/#/$OUT_DIR/}"
