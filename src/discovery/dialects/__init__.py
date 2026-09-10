"""WP-2.7 — the dialect profile registry.

``--dialect gnucobol-3.1.2`` is the default and returns the **measured**
layout. Any other profile returns a **projection**: the layout implied by
applying a sourced rule table to the same parse (D36).

:func:`engine_profile` is the piece worth reading. It maps the measured profile
onto ``None`` -- the layout engine's own default path -- rather than onto the
GnuCOBOL profile object. The two are byte-identical, and
``tests/test_dialect_roundtrip_gate.py`` asserts that on every copybook at
every ODO extent. Routing the default through ``None`` anyway means the
measured path cannot drift *by construction*: the code that produced the 186/186
round-trip before this work package existed is the code that runs when nobody
asks for a projection. That is acceptance (1) enforced structurally instead of
by vigilance.
"""

from __future__ import annotations

from typing import Dict, Optional, Tuple

from .base import (
    Citation,
    CitationError,
    DialectProfile,
    ProfileKind,
    Rule,
    RuleKind,
    Width,
)
from .classify import (
    Classification,
    FieldSensitivity,
    SensitivityReport,
    analyse_path,
    analyse_text,
    classify_field,
    lint_sensitivity,
)
from .gnucobol_3_1_2 import PROFILE as GNUCOBOL_3_1_2
from .ibm_enterprise_cobol import PROFILE as IBM_ENTERPRISE_COBOL
from .render import (
    Block,
    BlockKind,
    lint_projected_text,
    lint_projection_render,
    render_sensitivity_blocks,
    render_sensitivity_markdown,
)

#: The default. It is the measured one, and it is the default *because* it is
#: the measured one.
DEFAULT_DIALECT = GNUCOBOL_3_1_2.id

PROFILES: Dict[str, DialectProfile] = {
    GNUCOBOL_3_1_2.id: GNUCOBOL_3_1_2,
    IBM_ENTERPRISE_COBOL.id: IBM_ENTERPRISE_COBOL,
}


class UnknownDialect(KeyError):
    """A dialect id that is not in the table.

    Refused rather than fallen back to the default: silently returning the
    measured layout when a customer asked for a projection would answer a
    different question than the one they asked, and they would have no way to
    tell from the output.
    """


def dialect_ids() -> Tuple[str, ...]:
    return tuple(sorted(PROFILES))


def resolve(dialect: str) -> DialectProfile:
    try:
        return PROFILES[dialect]
    except KeyError:
        raise UnknownDialect(
            f"unknown dialect {dialect!r}; known: {', '.join(dialect_ids())}"
        ) from None


def engine_profile(profile: DialectProfile) -> Optional[DialectProfile]:
    """What to hand :func:`src.discovery.layout.compute` for ``profile``.

    ``None`` for the measured profile -- see the module docstring.
    """
    return None if profile.measured else profile


__all__ = [
    "Block", "BlockKind", "Citation", "CitationError", "Classification",
    "DEFAULT_DIALECT", "DialectProfile", "FieldSensitivity",
    "GNUCOBOL_3_1_2", "IBM_ENTERPRISE_COBOL", "PROFILES", "ProfileKind",
    "Rule", "RuleKind", "SensitivityReport", "UnknownDialect", "Width",
    "analyse_path", "analyse_text", "classify_field", "dialect_ids",
    "engine_profile", "lint_projected_text", "lint_projection_render",
    "lint_sensitivity", "render_sensitivity_blocks",
    "render_sensitivity_markdown", "resolve",
]
