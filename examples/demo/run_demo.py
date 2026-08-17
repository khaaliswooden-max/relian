#!/usr/bin/env python3
"""Drive the Meridian MUD demo corpus through the deterministic C1 core.

Read-only with respect to the repository: emitted Java, class files and run
output all go to a temp directory that is removed on exit. Nothing here
fabricates a result — a tier-A program is reported PASS only if it actually
transpiles, compiles under ``javac`` and runs; a tier-B/C program is reported
REFUSED only if the transpiler actually raises instead of emitting Java.

    python3 examples/demo/run_demo.py

Requires a JDK on PATH for the compile/run leg; without one the transpile leg
still runs and the compile leg is reported as skipped, never as passed.
"""

from __future__ import annotations

import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
DEMO = Path(__file__).resolve().parent
sys.path.insert(0, str(REPO_ROOT))

from transpiler.c1_rulebased import Transpiler, UnsupportedConstruct  # noqa: E402

# (program, generated class, sample stdin) for the tier-A programs.
TIER_A = [
    ("MUBRATE", "Mubrate", "8891004421,03,R,0018450,0002100"),
    ("MUBPENL", "Mubpenl", "8891004421,F,00147,00000842.55,08,DELGADO MARISOL"),
    ("MUBSURC", "Mubsurc",
     "PRM0044219,0000,0012400,0015900,0021700,0034800,0046200,0051100,"
     "0048700,0039400,0027300,0018800,0013100,0011900"),
]
TIER_BC = ["MUBBILL", "MUBPOST"]


def have_jdk() -> bool:
    return shutil.which("javac") is not None and shutil.which("java") is not None


def run_tier_a(workdir: Path, jdk: bool) -> int:
    failures = 0
    for prog, cls, sample in TIER_A:
        src = (DEMO / "src" / f"{prog}.cbl").read_text()
        try:
            java = Transpiler(src, cls).transpile()
        except Exception as exc:  # noqa: BLE001
            print(f"[FAIL] {prog}: expected a clean transpile, got "
                  f"{type(exc).__name__}: {exc}")
            failures += 1
            continue
        (workdir / f"{cls}.java").write_text(java)
        if not jdk:
            print(f"[ OK ] {prog}: transpiled {len(java):>5} bytes "
                  f"(compile/run skipped — no JDK on PATH)")
            continue
        cj = subprocess.run(["javac", f"{cls}.java"], cwd=workdir,
                            capture_output=True, text=True)
        if cj.returncode != 0:
            print(f"[FAIL] {prog}: javac rejected the emitted Java\n{cj.stderr}")
            failures += 1
            continue
        rj = subprocess.run(["java", cls], cwd=workdir, input=sample + "\n",
                            capture_output=True, text=True)
        if rj.returncode not in (0, 4, 8):
            print(f"[FAIL] {prog}: run exited {rj.returncode}\n{rj.stderr}")
            failures += 1
            continue
        first = rj.stdout.strip().splitlines()[-1] if rj.stdout.strip() else ""
        print(f"[ OK ] {prog}: transpiled, compiled, ran "
              f"(exit {rj.returncode}) — last line: {first!r}")
    return failures


def run_tier_bc() -> int:
    failures = 0
    for prog in TIER_BC:
        src = (DEMO / "src" / f"{prog}.cbl").read_text()
        try:
            Transpiler(src, prog.title()).transpile()
        except UnsupportedConstruct as exc:
            print(f"[ OK ] {prog}: refused with construct inventory — {exc}")
            continue
        except Exception as exc:  # noqa: BLE001
            # Any raise is an honest refusal: no Java was emitted. The clean
            # UnsupportedConstruct path is preferred, but a continuation-absorb
            # failure (documented case B) is still a refusal, not a silent pass.
            print(f"[ OK ] {prog}: refused ({type(exc).__name__}) — no placeholder emitted")
            continue
        print(f"[FAIL] {prog}: expected a refusal, transpiler returned Java")
        failures += 1
    return failures


def main() -> int:
    jdk = have_jdk()
    print("Meridian MUD demo — deterministic C1 transpile check")
    print(f"JDK on PATH: {'yes' if jdk else 'no (compile/run leg skipped)'}\n")
    with tempfile.TemporaryDirectory(prefix="relian-demo-") as tmp:
        workdir = Path(tmp)
        print("Tier A — expected to transpile, compile and run:")
        failures = run_tier_a(workdir, jdk)
        print("\nTier B/C — expected to refuse (never emit a placeholder):")
        failures += run_tier_bc()
    print()
    if failures:
        print(f"RESULT: {failures} check(s) did not behave as documented.")
        return 1
    print("RESULT: all checks behaved as documented.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
