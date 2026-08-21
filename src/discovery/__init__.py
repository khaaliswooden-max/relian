"""WP-2.2 — discovery: copybook resolution and static record layout.

This package computes COBOL record layouts **from source text alone**. It never
invokes a compiler, and ``tests/test_discovery_is_compiler_free.py`` walks this
package's AST to assert it. That constraint is not stylistic:

* RELIAN-DISCOVERY-BENCH v0.1's oracle is GnuCOBOL's own byte layout. If the
  engine shelled out to ``cobc`` the round-trip would be 100% by construction,
  the oracle would be grading itself, and the circularity would be invisible in
  a green result. The two artifacts agree *because* they share no code and no
  process.
* The product runs inside the customer perimeter (R12), on machines that have
  no GnuCOBOL and whose COBOL is IBM Enterprise. An engine that needs a
  compiler is not shippable.

WP-2.3 adds the report surface: :mod:`src.discovery.report` builds the
canonical ``report.json`` and its unsigned Markdown rendering, and
:mod:`src.discovery.signing` builds the manifest of digests and takes the
per-installation instance signature over it. Both keep the same discipline as
the engine below them — no compiler, no network, and no number without a grade.
``cryptography`` is imported inside functions there so that importing this
package stays as light as the layout engine needs it to be.

Scope of what a passing round-trip licenses (R11): the engine reproduces
**GnuCOBOL 3.1.2.0's** byte layout on the sealed 15-copybook corpus. IBM
Enterprise COBOL equivalence is UNMEASURED and belongs in the customer report
as a stated limitation, not in the quotable-capability matrix.
"""

from .copybook import (
    CopyDirective,
    CopybookSource,
    Resolution,
    Unresolved,
    apply_replacing,
    code_lines,
    find_copy_directives,
    replacing_limitations,
    resolve,
)
from .report import (
    FORBIDDEN_VOCABULARY,
    Report,
    ReportError,
    ReportMeta,
    lint_measured_fields,
    lint_report_text,
    render_markdown,
)
from .signing import (
    InstanceKey,
    SigningError,
    build_manifest,
    countersign_request_line,
    load_or_create_instance_key,
    manifest_hash,
    sign_manifest,
)
from .layout import (
    Condition,
    Field,
    Gap,
    Layout,
    LayoutStatus,
    compute,
    compute_text,
    lint_layout,
)

__all__ = [
    "CopyDirective",
    "CopybookSource",
    "Resolution",
    "Unresolved",
    "apply_replacing",
    "code_lines",
    "find_copy_directives",
    "replacing_limitations",
    "resolve",
    "Condition",
    "Field",
    "Gap",
    "Layout",
    "LayoutStatus",
    "compute",
    "compute_text",
    "lint_layout",
    "FORBIDDEN_VOCABULARY",
    "Report",
    "ReportError",
    "ReportMeta",
    "lint_measured_fields",
    "lint_report_text",
    "render_markdown",
    "InstanceKey",
    "SigningError",
    "build_manifest",
    "countersign_request_line",
    "load_or_create_instance_key",
    "manifest_hash",
    "sign_manifest",
]
