"""Suite-wide fixtures (WP-2.0.-1 §3).

Records the system toolchain the run actually measured into the test report,
so a result is never separable from the environment that produced it. The
Phase 2 baseline is quoted as a triple -- ``286 passed, 10 skipped, 0 failed``
as of WP-2.0, previously ``285`` and ``288`` -- but that triple only means something
alongside the interpreter, GnuCOBOL and JDK versions it was measured on, which
is what these hooks emit.
"""

from __future__ import annotations

import platform
import sys
from pathlib import Path
from typing import List

import pytest

# `tests/` is on sys.path already by virtue of this conftest, but tests under
# `tests/demo/` import the same module and are not guaranteed to be collected
# after it, so make the path explicit rather than order-dependent.
_TESTS_DIR = Path(__file__).resolve().parent
if str(_TESTS_DIR) not in sys.path:
    sys.path.insert(0, str(_TESTS_DIR))

import toolchain  # noqa: E402  (needs the sys.path insert above)


def pytest_report_header(config) -> List[str]:
    """Print the measured toolchain at the top of every run."""
    del config  # unused
    header = [
        f"interpreter: {platform.python_implementation()} "
        f"{platform.python_version()} ({sys.executable})",
        f"platform: {platform.platform()}",
    ]
    header.extend(toolchain.summary_lines())
    if toolchain.MISSING:
        # Loud, because an incomplete toolchain turns demo tests into skips and
        # the CI skip-count assertion is what catches that. Say it up front so a
        # local run is not read as equivalent to a CI run.
        header.append(
            "TOOLCHAIN INCOMPLETE -- "
            + ", ".join(f"{name} missing" for name in toolchain.MISSING)
            + "; toolchain-dependent tests will SKIP, not fail"
        )
    return header


@pytest.fixture(scope="session", autouse=True)
def record_toolchain(record_testsuite_property):
    """Write the measured versions into the JUnit XML as testsuite properties.

    Session-scoped and autouse so the properties are attached to every run,
    including one that ends in failure -- the environment of a red run is at
    least as worth recording as that of a green one. ``record_testsuite_property``
    degrades to a no-op when the run was started without ``--junitxml``, so this
    is safe in a plain ``pytest -q`` invocation.
    """
    record_testsuite_property("python_version", platform.python_version())
    record_testsuite_property("python_executable", sys.executable)
    record_testsuite_property("platform", platform.platform())
    for tool in toolchain.TOOLCHAIN:
        # R1: absent tools record "absent"/"unknown", never a plausible-looking
        # version string. An unmeasured version is not a measurement.
        record_testsuite_property(
            f"{tool.name}_version",
            tool.version if tool.present else "absent",
        )
        record_testsuite_property(
            f"{tool.name}_path", tool.path if tool.present else "absent"
        )
    record_testsuite_property(
        "toolchain_complete", "yes" if not toolchain.MISSING else "no"
    )
    return None
