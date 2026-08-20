"""System-toolchain detection for the test suite (WP-2.0.-1 §3).

The demo tests do not exercise Python alone: they compile COBOL with ``cobc``
and Java with ``javac``, then run both and compare. That makes the external
toolchain part of the environment under test, and it therefore has to be
detected explicitly rather than assumed.

Why this module replaced the ``needs_cobc`` marker that used to live in
``tests/demo/test_demo.py``:

    ``needs_cobc`` guarded only half the toolchain. On a box with GnuCOBOL but
    no JDK the guard did not fire, twelve demo tests ran, and every one of them
    failed on the ``javac`` invocation -- an environment gap reported as twelve
    product defects. On a box missing *both* the guard did fire, and the same
    twelve tests skipped: green by skip. One missing tool produced a red suite,
    two missing tools produced a green one, which is exactly backwards.

``needs_toolchain`` fires when *either* tool is absent, so an incomplete
environment always produces the same, honestly-labelled outcome: skipped, with
the missing tool named in the skip reason.

Skipping is only safe because the skip *count* is pinned. ``.github/workflows/
tests.yml`` asserts the suite reports exactly the expected number of skips and
fails if it changes, so a CI runner that silently loses ``cobc`` or ``javac``
turns twelve skips into a failed build rather than a green one. Neither control
works without the other: the guard without the count is green-by-skip, and the
count without the guard is the twelve spurious failures.
"""

from __future__ import annotations

import shutil
import subprocess
from typing import NamedTuple, Optional, Tuple

import pytest

# Version probes are cheap, but a wedged binary should not hang collection.
_PROBE_TIMEOUT_S = 15


class Tool(NamedTuple):
    """A system executable the suite depends on, and its measured version."""

    name: str
    path: Optional[str]
    version: Optional[str]

    @property
    def present(self) -> bool:
        return self.path is not None

    def describe(self) -> str:
        if not self.present:
            return f"{self.name}: NOT FOUND"
        # R1: the version is whatever the tool reported, or None. A tool that
        # is on PATH but would not answer --version is not given an invented
        # version string; the report says so.
        return f"{self.name}: {self.version or 'present, version not reported'}"


def _probe(name: str, args: Tuple[str, ...]) -> Tool:
    """Locate ``name`` on PATH and read back the version it reports."""
    path = shutil.which(name)
    if path is None:
        return Tool(name, None, None)
    try:
        proc = subprocess.run(
            [path, *args],
            capture_output=True,
            text=True,
            timeout=_PROBE_TIMEOUT_S,
        )
    except (OSError, subprocess.SubprocessError):
        return Tool(name, path, None)
    # `javac -version` writes to stdout on JDK 9+ and to stderr on JDK 8;
    # `cobc --version` writes a multi-line banner to stdout. Take the first
    # non-empty line of whichever stream produced one.
    for stream in (proc.stdout, proc.stderr):
        for line in (stream or "").splitlines():
            if line.strip():
                return Tool(name, path, line.strip())
    return Tool(name, path, None)


COBC = _probe("cobc", ("--version",))
JAVAC = _probe("javac", ("-version",))

#: Every external tool the demo tests need, in the order they are used.
TOOLCHAIN: Tuple[Tool, ...] = (COBC, JAVAC)

#: Names of the tools that are not installed. Empty tuple == complete toolchain.
MISSING: Tuple[str, ...] = tuple(tool.name for tool in TOOLCHAIN if not tool.present)

_REASON = (
    "incomplete system toolchain: "
    + ", ".join(f"{name} not installed" for name in MISSING)
    + " (both GnuCOBOL `cobc` and a JDK `javac` are required to build and run "
      "the two sides of a differential comparison)"
) if MISSING else ""

#: Skip a test that needs the full COBOL+Java toolchain, naming what is absent.
needs_toolchain = pytest.mark.skipif(bool(MISSING), reason=_REASON)

# Three demo tests stub the COBOL oracle out entirely (`oracle.detect` is
# monkeypatched to report "absent") in order to assert that a run with no
# GnuCOBOL still behaves honestly: no invented metric, no execution outage,
# exit status 0. Those are meaningful on a box that genuinely has no `cobc`,
# so they must NOT be gated on the full toolchain -- gating them there would
# skip the offline-mode tests exactly on the machines where offline mode is
# the real configuration.
#
# They do still need `javac`, because the pipeline builds the Java side before
# it discovers there is nothing to compare against. Without it the pipeline
# returns BUILD_FAILED and all three fail on an assertion about the verdict --
# measured: `test_no_metric_is_invented_without_the_oracle`,
# `test_offline_run_is_not_an_execution_outage` and `test_offline_run_exits_zero`
# report BUILD_FAILED/rc=1 with `javac` off PATH. That is a missing JDK
# reported as three product defects, which is the same fault `needs_cobc` had,
# so they get a guard too -- just a narrower one.
needs_javac = pytest.mark.skipif(
    not JAVAC.present,
    reason=(
        "incomplete system toolchain: javac not installed (the pipeline builds "
        "the Java side before it reports on the COBOL oracle, so even an "
        "oracle-absent run needs a JDK)"
    ),
)


def summary_lines() -> Tuple[str, ...]:
    """One line per tool, for the test report header."""
    return tuple(tool.describe() for tool in TOOLCHAIN)
