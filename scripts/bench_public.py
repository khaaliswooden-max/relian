#!/usr/bin/env python3
"""Public-split bench leg for the C1 transpiler.

Why this script exists: `bench/` is read-only for agents (CLAUDE.md rule 4)
and `bench/harness/runner.py`'s __main__ defaults to the HELD-OUT split,
which this environment must never touch (rule 1). This driver lives outside
`bench/`, regenerates the C1 candidate from `transpiler/c1_rulebased.py` into
a scratch directory, and scores it against the PUBLIC vectors only.

It emits, for each program: the sha256 of the generated Java (so a refactor
can be proven byte-identical) and the measured public-split BER.

Usage:
    python3 scripts/bench_public.py --out /tmp/c1_after [--json out.json]
"""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT))
sys.path.insert(0, str(ROOT / "bench"))

from transpiler.c1_rulebased import Transpiler  # noqa: E402


def _failure_stub(cls: str, exc: Exception) -> str:
    """Java that does NOT compile, mirroring the gate's failure_stub in
    bench.yml: a comment-only .java file is a legal empty compilation unit
    and javac exits 0 on it, which would leave build_ok true for a program
    that produced nothing runnable. The unresolved symbol makes the failure
    visible in build_rate. Message flattened and '/' replaced so it cannot
    collide with the harness's anti-gaming token list."""
    import re
    msg = re.sub(r"\s+", " ", f"{type(exc).__name__}: {exc}").replace("/", "|")
    return (f"// TRANSPILE FAILED: {msg[:300]}\n"
            f"// Deliberately uncompilable: see _failure_stub.\n"
            f"public class {cls} {{\n"
            f"    public static void main(String[] args) {{\n"
            f"        RELIAN_TRANSPILE_FAILED unresolved = null;\n"
            f"    }}\n"
            f"}}\n")


def generate(out_root: Path, mains: dict) -> dict:
    """Transpile every corpus program; return {program: sha256-of-java}."""
    hashes = {}
    for prog in sorted(mains):
        cls = mains[prog]
        src = (ROOT / "bench" / "corpus" / prog / "program.cbl").read_text()
        try:
            java = Transpiler(src, cls).transpile()
        except Exception as exc:  # honest failure, visible in build_rate
            java = _failure_stub(cls, exc)
        d = out_root / prog
        d.mkdir(parents=True, exist_ok=True)
        (d / f"{cls}.java").write_text(java)
        hashes[prog] = hashlib.sha256(java.encode()).hexdigest()
    return hashes


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True, help="scratch dir for generated Java")
    ap.add_argument("--json", help="write the full report here")
    ap.add_argument("--no-score", action="store_true",
                    help="generate + hash only (skip javac/java)")
    ap.add_argument("--mains", help="alternate mains JSON (e.g. "
                    "scripts/mains_v1.2_proposed.json to score the v1.2 "
                    "corpus before bench/harness/mains.json is re-sealed)")
    args = ap.parse_args()

    mains_path = Path(args.mains) if args.mains else (
        ROOT / "bench" / "harness" / "mains.json")
    mains = json.loads(mains_path.read_text())
    out_root = Path(args.out).resolve()
    hashes = generate(out_root, mains)

    result = {"generated_sha256": hashes}
    if not args.no_score:
        from harness.runner import run_candidate
        rep = run_candidate("C1_rulebased@public", out_root, mains, split="public")
        result["report"] = rep.to_dict()
        result["ber_overall"] = rep.ber_overall
        result["build_rate"] = rep.build_rate
        result["valid"] = rep.valid

    text = json.dumps(result, indent=2, sort_keys=True)
    if args.json:
        Path(args.json).write_text(text + "\n")
    print(json.dumps({k: v for k, v in result.items() if k != "report"},
                     indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
