"""
Branch coverage measurement via JaCoCo. v1.1 addition.

Cardinal rule applies: coverage is reported ONLY when the JaCoCo agent
actually instrumented the runs and the CLI produced a report. Any failure
in that chain -> None. A candidate with zero branches (e.g., straight-line
placeholder code) reports None with an explanatory note, because 0/0 is
undefined -- not 100%, not 0%.
"""

import csv
import subprocess
from pathlib import Path
from typing import Dict, Optional, Tuple

TOOLS = Path(__file__).resolve().parent / "tools"
AGENT = TOOLS / "jacocoagent.jar"
CLI = TOOLS / "jacococli.jar"


def available() -> bool:
    return AGENT.exists() and CLI.exists()


def run_with_coverage(classes: Path, main_cls: str, stdin_line: str,
                      exec_file: Path) -> Tuple[Optional[str], Optional[int]]:
    """Run one vector under the JaCoCo agent, appending to exec_file.

    Returns (stdout, exit_code); (None, None) if the process could not run.
    WP-1.5.0d: mirrors runner._run_java -- the exit code is captured for the
    scorer to compare, not filtered here.
    """
    try:
        p = subprocess.run(
            ["java",
             f"-javaagent:{AGENT}=destfile={exec_file},append=true",
             "-cp", str(classes), main_cls],
            input=stdin_line + "\n", capture_output=True, text=True, timeout=30,
        )
    except Exception:
        return None, None
    return p.stdout.strip(), p.returncode


def report(classes: Path, exec_file: Path) -> Tuple[Optional[float], Dict]:
    """Produce branch coverage from the accumulated exec file.

    Returns (branch_coverage or None, detail dict).
    """
    detail: Dict = {"tool": "jacoco-0.8.12"}
    if not exec_file.exists():
        detail["note"] = "no exec data recorded"
        return None, detail
    csv_out = exec_file.with_suffix(".csv")
    p = subprocess.run(
        ["java", "-jar", str(CLI), "report", str(exec_file),
         "--classfiles", str(classes), "--csv", str(csv_out)],
        capture_output=True, text=True, timeout=120,
    )
    if p.returncode != 0 or not csv_out.exists():
        detail["note"] = f"jacoco report failed: {p.stderr.strip()[:150]}"
        return None, detail
    b_missed = b_covered = l_missed = l_covered = 0
    with open(csv_out) as f:
        for row in csv.DictReader(f):
            b_missed += int(row["BRANCH_MISSED"])
            b_covered += int(row["BRANCH_COVERED"])
            l_missed += int(row["LINE_MISSED"])
            l_covered += int(row["LINE_COVERED"])
    detail["branches_covered"] = b_covered
    detail["branches_total"] = b_covered + b_missed
    detail["line_coverage"] = (
        round(l_covered / (l_covered + l_missed), 4)
        if (l_covered + l_missed) else None
    )
    if b_covered + b_missed == 0:
        detail["note"] = ("candidate code contains zero branches; branch "
                          "coverage is undefined (None), not 100%")
        return None, detail
    return round(b_covered / (b_covered + b_missed), 4), detail
