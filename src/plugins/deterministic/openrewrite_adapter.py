"""OpenRewrite adapter — Java AST parsing + recipe-based transformation."""

import json
import logging
import subprocess
import tempfile
from pathlib import Path
from typing import List, Optional

from src.parsers.base import ASTNode, LanguageParser, NodeType
from src.plugins.base import PluginType, RelianPlugin

logger = logging.getLogger(__name__)

_SUBMODULE_ROOT = Path(__file__).parents[4] / "repos" / "deterministic" / "rewrite"


class OpenRewriteParser(LanguageParser):
    """
    Thin wrapper that invokes OpenRewrite via subprocess and converts its
    JSON diagnostic output into the relian ASTNode tree.

    Requires: JDK 11+ and Gradle on PATH; submodule initialised.
    """

    def __init__(self, submodule_root: Path = _SUBMODULE_ROOT) -> None:
        self._root = submodule_root

    def parse_file(self, filepath: str) -> ASTNode:
        """Parse a Java source file via OpenRewrite."""
        with open(filepath, "r", encoding="utf-8") as fh:
            source = fh.read()
        return self.parse_string(source)

    def parse_string(self, source_code: str) -> ASTNode:
        """
        Write source to a temp file, run OpenRewrite LST export, parse output.

        Falls back to a skeleton ASTNode on any subprocess failure so the
        rest of the pipeline can still run.
        """
        tmp_path = None
        try:
            with tempfile.NamedTemporaryFile(
                suffix=".java", mode="w", encoding="utf-8", delete=False
            ) as tmp:
                tmp.write(source_code)
                tmp_path = tmp.name

            result = subprocess.run(
                [
                    "./gradlew", "rewriteRun",
                    f"-Psource={tmp_path}", "--quiet", "--json-output",
                ],
                cwd=str(self._root),
                capture_output=True,
                text=True,
                timeout=120,
            )
            if result.returncode == 0 and result.stdout:
                return self._json_to_ast(json.loads(result.stdout), source_code)
        except Exception as exc:
            logger.warning("OpenRewrite subprocess failed: %s", exc)
        finally:
            if tmp_path:
                Path(tmp_path).unlink(missing_ok=True)

        return self._fallback_ast(source_code)

    def extract_functions(self, ast: ASTNode) -> List[ASTNode]:
        """Return all METHOD nodes from the AST."""
        results: List[ASTNode] = []
        self._collect(ast, NodeType.FUNCTION, results)
        return results

    def extract_variables(self, ast: ASTNode) -> List[ASTNode]:
        """Return all VARIABLE nodes from the AST."""
        results: List[ASTNode] = []
        self._collect(ast, NodeType.VARIABLE, results)
        return results

    def get_dependencies(self, ast: ASTNode) -> List[str]:
        """Return import statements captured in CALL nodes."""
        results: List[ASTNode] = []
        self._collect(ast, NodeType.CALL, results)
        return [n.name for n in results]

    def _json_to_ast(self, data: dict, source_code: str) -> ASTNode:
        """Convert OpenRewrite JSON diagnostic output to ASTNode tree."""
        class_name = data.get("className", "JavaClass")
        methods = [
            ASTNode(
                node_type=NodeType.FUNCTION,
                name=m.get("name", "unknown"),
                children=[],
                metadata={
                    "return_type": m.get("returnType", ""),
                    "modifiers": m.get("modifiers", []),
                },
            )
            for m in data.get("methods", [])
        ]
        fields = [
            ASTNode(
                node_type=NodeType.VARIABLE,
                name=f.get("name", "field"),
                children=[],
                metadata={"type": f.get("type", "")},
            )
            for f in data.get("fields", [])
        ]
        return ASTNode(
            node_type=NodeType.PROGRAM,
            name=class_name,
            children=fields + methods,
            metadata={"source": "openrewrite"},
        )

    def _fallback_ast(self, source_code: str) -> ASTNode:
        """Minimal AST when OpenRewrite is unavailable."""
        return ASTNode(
            node_type=NodeType.PROGRAM,
            name="JavaProgram",
            children=[],
            metadata={"source": "fallback", "lines": source_code.count("\n") + 1},
        )

    def _collect(self, node: ASTNode, target: NodeType, out: List[ASTNode]) -> None:
        if node.node_type == target:
            out.append(node)
        for child in node.children:
            self._collect(child, target, out)


class OpenRewriteAdapter(RelianPlugin):
    """
    Plugin adapter for the OpenRewrite Java transformation framework.

    Provides Java parsing via OpenRewrite's LST (Lossless Semantic Tree).
    Gracefully degrades when the submodule is not initialised or JDK is absent.
    """

    name = "openrewrite"
    plugin_type = PluginType.PARSER
    supported_languages = ["java"]

    @property
    def is_available(self) -> bool:
        """True if the submodule directory is non-empty and java is on PATH."""
        if not _SUBMODULE_ROOT.exists():
            return False
        try:
            if not any(_SUBMODULE_ROOT.iterdir()):
                return False
        except OSError:
            return False
        try:
            subprocess.run(["java", "-version"], capture_output=True, timeout=5)
            return True
        except (FileNotFoundError, subprocess.TimeoutExpired):
            return False

    def get_parser(self) -> Optional[OpenRewriteParser]:
        """Return parser if available."""
        if not self.is_available:
            return None
        return OpenRewriteParser()
