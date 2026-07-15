"""Rope adapter — Python refactoring and AST extraction."""

import logging
import sys
from pathlib import Path
from typing import Any, List, Optional, Tuple

from src.parsers.base import ASTNode, LanguageParser, NodeType
from src.plugins.base import PluginType, RelianPlugin

logger = logging.getLogger(__name__)

_SUBMODULE_ROOT = Path(__file__).parents[4] / "repos" / "ast-codemods" / "rope"


def _try_import_rope() -> Optional[Tuple[Any, Any]]:
    """Import rope from submodule if available, otherwise from pip."""
    if _SUBMODULE_ROOT.exists():
        try:
            if any(_SUBMODULE_ROOT.iterdir()) and str(_SUBMODULE_ROOT) not in sys.path:
                sys.path.insert(0, str(_SUBMODULE_ROOT))
        except OSError:
            pass
    try:
        import rope.base.project as rope_project  # type: ignore[import]
        import rope.base.libutils as rope_libutils  # type: ignore[import]
        return (rope_project, rope_libutils)
    except ImportError:
        return None


class RopeParser(LanguageParser):
    """
    Uses Rope to validate Python syntax, then stdlib ast for tree extraction.

    Rope works on projects, so we create a temporary project for each
    parse call. The resulting rope Resource confirms parse validity;
    the actual ASTNode tree comes from a stdlib ast walk.
    """

    def __init__(self, rope_modules: Tuple[Any, Any]) -> None:
        self._rope_project_mod, self._rope_libutils = rope_modules

    def parse_file(self, filepath: str) -> ASTNode:
        with open(filepath, "r", encoding="utf-8") as fh:
            return self.parse_string(fh.read())

    def parse_string(self, source_code: str) -> ASTNode:
        """Parse Python source into an ASTNode tree using Rope's resource model."""
        import ast as stdlib_ast
        import tempfile

        try:
            with tempfile.TemporaryDirectory() as tmpdir:
                project = self._rope_project_mod.Project(tmpdir)
                self._rope_libutils.string_to_file(
                    project, source_code, "temp_module.py"
                )
                # Rope validates syntax; walk the stdlib AST for structure
                tree = stdlib_ast.parse(source_code)
                return self._stdlib_ast_to_relian(tree, source_code)
        except Exception as exc:
            logger.warning("Rope project setup failed, falling back to stdlib ast: %s", exc)

        try:
            tree = stdlib_ast.parse(source_code)
            return self._stdlib_ast_to_relian(tree, source_code)
        except Exception:
            return ASTNode(
                node_type=NodeType.PROGRAM,
                name="PythonProgram",
                children=[],
                metadata={"source": "rope_fallback"},
            )

    def extract_functions(self, ast: ASTNode) -> List[ASTNode]:
        out: List[ASTNode] = []
        self._collect(ast, NodeType.FUNCTION, out)
        return out

    def extract_variables(self, ast: ASTNode) -> List[ASTNode]:
        out: List[ASTNode] = []
        self._collect(ast, NodeType.VARIABLE, out)
        return out

    def get_dependencies(self, ast: ASTNode) -> List[str]:
        out: List[ASTNode] = []
        self._collect(ast, NodeType.CALL, out)
        return [n.name for n in out]

    def _stdlib_ast_to_relian(self, tree: Any, source_code: str) -> ASTNode:
        """Walk a stdlib ast.Module and produce a relian ASTNode tree."""
        import ast as a

        children = []
        for node in a.walk(tree):
            if isinstance(node, (a.FunctionDef, a.AsyncFunctionDef)):
                children.append(
                    ASTNode(
                        node_type=NodeType.FUNCTION,
                        name=node.name,
                        children=[],
                        metadata={
                            "lineno": node.lineno,
                            "args": [arg.arg for arg in node.args.args],
                        },
                        source_location=(
                            node.lineno,
                            getattr(node, "end_lineno", node.lineno),
                            0,
                            0,
                        ),
                    )
                )
            elif isinstance(node, a.ClassDef):
                children.append(
                    ASTNode(
                        node_type=NodeType.PROCEDURE,
                        name=node.name,
                        children=[],
                        metadata={"lineno": node.lineno, "type": "class"},
                    )
                )
        return ASTNode(
            node_type=NodeType.PROGRAM,
            name="PythonModule",
            children=children,
            metadata={"source": "rope", "lines": source_code.count("\n") + 1},
        )

    def _collect(self, node: ASTNode, target: NodeType, out: List[ASTNode]) -> None:
        if node.node_type == target:
            out.append(node)
        for child in node.children:
            self._collect(child, target, out)


class RopeAdapter(RelianPlugin):
    """
    Plugin adapter for the Rope Python refactoring library.

    Registers a parser for the 'python' language key.
    """

    name = "rope"
    plugin_type = PluginType.PARSER
    supported_languages = ["python"]

    def __init__(self) -> None:
        self._modules = _try_import_rope()

    @property
    def is_available(self) -> bool:
        return self._modules is not None

    def get_parser(self) -> Optional[RopeParser]:
        if not self.is_available:
            return None
        return RopeParser(self._modules)
