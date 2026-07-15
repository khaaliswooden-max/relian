"""Piranha adapter — feature flag / dead code cleanup for Python and Java."""

import logging
import sys
from pathlib import Path
from typing import Any, List, Optional

from src.parsers.base import ASTNode, LanguageParser, NodeType
from src.plugins.base import PluginType, RelianPlugin

logger = logging.getLogger(__name__)

_SUBMODULE_ROOT = Path(__file__).parents[4] / "repos" / "deterministic" / "piranha"


def _try_import_piranha() -> Optional[Any]:
    """Attempt to import polyglot_piranha from submodule path or pip install."""
    submodule_src = _SUBMODULE_ROOT / "src" / "python"
    if submodule_src.exists():
        if str(submodule_src) not in sys.path:
            sys.path.insert(0, str(submodule_src))
    try:
        import polyglot_piranha  # type: ignore[import]
        return polyglot_piranha
    except ImportError:
        return None


class PiranhaParser(LanguageParser):
    """
    Wraps Piranha to parse Python source and identify dead-code branches
    tied to feature flags.

    The ASTNode output marks stale flag branches as CONDITIONAL nodes
    with metadata["dead_branch"] = True so downstream transformers can
    remove them.
    """

    def __init__(self, piranha_module: Any) -> None:
        self._piranha = piranha_module

    def parse_file(self, filepath: str) -> ASTNode:
        with open(filepath, "r", encoding="utf-8") as fh:
            return self.parse_string(fh.read())

    def parse_string(self, source_code: str) -> ASTNode:
        """
        Run Piranha in analysis-only mode and annotate the AST.

        Returns a PROGRAM ASTNode whose CONDITIONAL children are branches
        Piranha identified as dead code.
        """
        try:
            summary = self._piranha.execute_piranha(
                source_code=source_code,
                language="python",
                cleanup_comments=False,
            )
            dead_branches = [
                ASTNode(
                    node_type=NodeType.CONDITIONAL,
                    name=entry.get("flag_name", "unknown_flag"),
                    children=[],
                    metadata={
                        "dead_branch": True,
                        "flag_value": entry.get("flag_value"),
                    },
                )
                for entry in (summary.get("stale_flags") or [])
            ]
            return ASTNode(
                node_type=NodeType.PROGRAM,
                name="PiranhaAnalysis",
                children=dead_branches,
                metadata={"source": "piranha", "language": "python"},
            )
        except Exception as exc:
            logger.warning("Piranha analysis failed: %s", exc)
            return ASTNode(
                node_type=NodeType.PROGRAM,
                name="PiranhaAnalysis",
                children=[],
                metadata={"source": "piranha_fallback"},
            )

    def extract_functions(self, ast: ASTNode) -> List[ASTNode]:
        # Piranha is flag-cleanup focused, not function extraction
        return []

    def extract_variables(self, ast: ASTNode) -> List[ASTNode]:
        return []

    def get_dependencies(self, ast: ASTNode) -> List[str]:
        return []


class PiranhaAdapter(RelianPlugin):
    """
    Plugin adapter for Uber's Piranha feature flag cleanup tool.

    Supports Python analysis; Java support requires the submodule to be
    initialised and built with Maven/Bazel.
    """

    name = "piranha"
    plugin_type = PluginType.PARSER
    supported_languages = ["python"]

    def __init__(self) -> None:
        self._module = _try_import_piranha()

    @property
    def is_available(self) -> bool:
        return self._module is not None

    def get_parser(self) -> Optional[PiranhaParser]:
        if not self.is_available:
            return None
        return PiranhaParser(self._module)
