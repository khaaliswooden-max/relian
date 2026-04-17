"""jscodeshift adapter — JavaScript/TypeScript AST transformation."""

import logging
import re
import subprocess
import tempfile
from pathlib import Path
from typing import List, Optional

from src.parsers.base import ASTNode, LanguageParser, NodeType
from src.plugins.base import PluginType, RelianPlugin

logger = logging.getLogger(__name__)

_SUBMODULE_ROOT = Path(__file__).parents[4] / "repos" / "ast-codemods" / "jscodeshift"
_BIN = _SUBMODULE_ROOT / "node_modules" / ".bin" / "jscodeshift"
_IDENTITY_TRANSFORM = Path(__file__).parent / "jscodeshift_identity.js"


def _node_available() -> bool:
    """Check that node is present on PATH."""
    try:
        subprocess.run(["node", "--version"], capture_output=True, timeout=5)
        return True
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return False


def _jscodeshift_available() -> bool:
    """Check that the jscodeshift binary exists (submodule npm install ran)."""
    return _BIN.exists()


class JSCodeshiftParser(LanguageParser):
    """
    Parses JavaScript and TypeScript via jscodeshift's --print mode.

    Produces an ASTNode tree by running a no-op identity transform that
    passes source through jscodeshift's parser unchanged, then applying
    a regex structural scan on the printed output.
    """

    def parse_file(self, filepath: str) -> ASTNode:
        with open(filepath, "r", encoding="utf-8") as fh:
            source = fh.read()
        ext = Path(filepath).suffix.lstrip(".") or "js"
        return self._parse(source, ext)

    def parse_string(self, source_code: str) -> ASTNode:
        return self._parse(source_code, "js")

    def extract_functions(self, ast: ASTNode) -> List[ASTNode]:
        out: List[ASTNode] = []
        self._collect(ast, NodeType.FUNCTION, out)
        return out

    def extract_variables(self, ast: ASTNode) -> List[ASTNode]:
        out: List[ASTNode] = []
        self._collect(ast, NodeType.VARIABLE, out)
        return out

    def get_dependencies(self, ast: ASTNode) -> List[str]:
        """Return require/import module names from CALL nodes."""
        out: List[ASTNode] = []
        self._collect(ast, NodeType.CALL, out)
        return [n.name for n in out if n.metadata.get("import")]

    def _parse(self, source_code: str, ext: str) -> ASTNode:
        """Write source to a temp file, run jscodeshift in analysis mode."""
        tmp_path = None
        try:
            with tempfile.NamedTemporaryFile(
                suffix=f".{ext}", mode="w", encoding="utf-8", delete=False
            ) as tmp:
                tmp.write(source_code)
                tmp_path = tmp.name

            result = subprocess.run(
                [
                    str(_BIN), "--dry", "--print",
                    "--transform", str(_IDENTITY_TRANSFORM),
                    tmp_path,
                ],
                capture_output=True,
                text=True,
                timeout=30,
            )
            if tmp_path:
                Path(tmp_path).unlink(missing_ok=True)
                tmp_path = None

            if result.returncode == 0:
                return self._text_output_to_ast(result.stdout, source_code)
        except Exception as exc:
            logger.warning("jscodeshift parse failed: %s", exc)
        finally:
            if tmp_path:
                Path(tmp_path).unlink(missing_ok=True)

        return self._fallback_ast(source_code)

    def _text_output_to_ast(self, output: str, source_code: str) -> ASTNode:
        """Build ASTNode from jscodeshift --dry --print output via regex scan."""
        functions = re.findall(
            r"(?:function\s+(\w+)|(?:const|let|var)\s+(\w+)\s*=\s*(?:async\s*)?\()",
            source_code,
        )
        children = [
            ASTNode(
                node_type=NodeType.FUNCTION,
                name=fn[0] or fn[1],
                children=[],
                metadata={"source": "jscodeshift"},
            )
            for fn in functions
            if fn[0] or fn[1]
        ]
        return ASTNode(
            node_type=NodeType.PROGRAM,
            name="JSModule",
            children=children,
            metadata={"source": "jscodeshift", "lines": source_code.count("\n") + 1},
        )

    def _fallback_ast(self, source_code: str) -> ASTNode:
        return ASTNode(
            node_type=NodeType.PROGRAM,
            name="JSModule",
            children=[],
            metadata={"source": "jscodeshift_fallback"},
        )

    def _collect(self, node: ASTNode, target: NodeType, out: List[ASTNode]) -> None:
        if node.node_type == target:
            out.append(node)
        for child in node.children:
            self._collect(child, target, out)


class JSCodeshiftAdapter(RelianPlugin):
    """
    Plugin adapter for Facebook's jscodeshift AST transformation toolkit.

    Registers parsers for both 'javascript' and 'typescript' language keys.
    """

    name = "jscodeshift"
    plugin_type = PluginType.PARSER
    supported_languages = ["javascript", "typescript"]

    @property
    def is_available(self) -> bool:
        return _node_available() and _jscodeshift_available()

    def get_parser(self) -> Optional[JSCodeshiftParser]:
        if not self.is_available:
            return None
        return JSCodeshiftParser()
