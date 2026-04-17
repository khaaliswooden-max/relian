"""Tests for the plugin subsystem."""

import sys
import unittest
from unittest.mock import MagicMock, patch


class TestPluginBase(unittest.TestCase):
    """Test PluginType enum and RelianPlugin ABC."""

    def test_plugin_type_values(self):
        """All expected PluginType variants exist."""
        from src.plugins.base import PluginType

        self.assertEqual(PluginType.PARSER.value, "parser")
        self.assertEqual(PluginType.TRANSFORMER.value, "transformer")
        self.assertEqual(PluginType.ANALYZER.value, "analyzer")

    def test_relian_plugin_is_abstract(self):
        """Cannot instantiate RelianPlugin directly."""
        from src.plugins.base import RelianPlugin

        with self.assertRaises(TypeError):
            RelianPlugin()  # type: ignore

    def test_concrete_plugin_implements_contract(self):
        """A concrete plugin can be instantiated and queried."""
        from src.plugins.base import PluginType, RelianPlugin

        class _MockPlugin(RelianPlugin):
            name = "mock"
            plugin_type = PluginType.PARSER
            supported_languages = ["cobol"]

            @property
            def is_available(self) -> bool:
                return True

            def get_parser(self):
                return MagicMock()

        plugin = _MockPlugin()
        self.assertTrue(plugin.is_available)
        self.assertIsNotNone(plugin.get_parser())
        meta = plugin.get_metadata()
        self.assertEqual(meta["name"], "mock")
        self.assertEqual(meta["supported_languages"], ["cobol"])

    def test_get_metadata_includes_availability(self):
        """get_metadata() reports current availability."""
        from src.plugins.base import PluginType, RelianPlugin

        class _UnavailablePlugin(RelianPlugin):
            name = "offline"
            plugin_type = PluginType.PARSER
            supported_languages = ["ada"]

            @property
            def is_available(self) -> bool:
                return False

            def get_parser(self):
                return None

        meta = _UnavailablePlugin().get_metadata()
        self.assertFalse(meta["available"])


class TestPluginRegistry(unittest.TestCase):
    """Test PluginRegistry wiring logic."""

    def setUp(self):
        from src.core.orchestrator import MigrationOrchestrator

        self.orchestrator = MigrationOrchestrator()

    def test_registry_loads_without_error(self):
        """Registry loads even when all submodules are absent."""
        from src.plugins.registry import PluginRegistry

        registry = PluginRegistry()
        # Should not raise — all ImportErrors caught internally
        registry.load_all(self.orchestrator)

    def test_unavailable_plugin_not_registered(self):
        """A plugin whose is_available is False does not register."""
        from src.plugins.base import PluginType, RelianPlugin
        from src.plugins.registry import PluginRegistry

        class _UnavailablePlugin(RelianPlugin):
            name = "unavailable"
            plugin_type = PluginType.PARSER
            supported_languages = ["fortran"]

            @property
            def is_available(self) -> bool:
                return False

            def get_parser(self):
                return MagicMock()

        registry = PluginRegistry()
        registry.register_plugin(_UnavailablePlugin())
        registry.load_all(self.orchestrator)
        self.assertNotIn("fortran", self.orchestrator._parsers)

    def test_available_plugin_registers_parser(self):
        """A plugin whose is_available is True registers its parser."""
        from src.plugins.base import PluginType, RelianPlugin
        from src.plugins.registry import PluginRegistry

        mock_parser = MagicMock()
        mock_parser.parse_string = MagicMock(return_value=None)

        class _AvailablePlugin(RelianPlugin):
            name = "available"
            plugin_type = PluginType.PARSER
            supported_languages = ["mumps"]

            @property
            def is_available(self) -> bool:
                return True

            def get_parser(self):
                return mock_parser

        registry = PluginRegistry()
        registry.register_plugin(_AvailablePlugin())
        registry.load_all(self.orchestrator)
        self.assertIn("mumps", self.orchestrator._parsers)

    def test_status_report_structure(self):
        """Status report has the expected keys."""
        from src.plugins.registry import PluginRegistry

        registry = PluginRegistry()
        registry.load_all(self.orchestrator)
        report = registry.get_status_report()
        self.assertIn("total_plugins_attempted", report)
        self.assertIn("loaded", report)
        self.assertIn("details", report)

    def test_load_plugins_orchestrator_method(self):
        """orchestrator.load_plugins() returns a valid status report."""
        report = self.orchestrator.load_plugins()
        self.assertIn("loaded", report)
        self.assertIn("total_plugins_attempted", report)

    def test_load_plugins_does_not_raise(self):
        """load_plugins() never raises even if plugin import fails completely."""
        with patch("src.plugins.registry.PluginRegistry") as mock_registry_cls:
            mock_registry_cls.side_effect = RuntimeError("registry broken")
            report = self.orchestrator.load_plugins()
            self.assertEqual(report["loaded"], 0)


class TestOrchestratorIntegration(unittest.TestCase):
    """Test that plugin loading does not break the existing COBOL pipeline."""

    def test_cobol_parser_unaffected_by_load_plugins(self):
        """load_plugins() does not overwrite a manually registered parser."""
        from src.core.orchestrator import MigrationOrchestrator
        from src.parsers.base import LanguageParser, ASTNode, NodeType

        class _MockCobolParser(LanguageParser):
            def parse_file(self, filepath):
                return ASTNode(NodeType.PROGRAM, "root", [], {})
            def parse_string(self, source_code):
                return ASTNode(NodeType.PROGRAM, "root", [], {})
            def extract_functions(self, ast):
                return []
            def extract_variables(self, ast):
                return []
            def get_dependencies(self, ast):
                return []

        orch = MigrationOrchestrator()
        mock_parser = _MockCobolParser()
        orch.register_parser("cobol", mock_parser)
        orch.load_plugins()  # no built-in plugin targets "cobol"
        self.assertIn("cobol", orch._parsers)
        self.assertIs(orch._parsers["cobol"], mock_parser)

    def test_multiple_load_plugins_calls_safe(self):
        """Calling load_plugins() twice does not error."""
        from src.core.orchestrator import MigrationOrchestrator

        orch = MigrationOrchestrator()
        r1 = orch.load_plugins()
        r2 = orch.load_plugins()
        self.assertIn("loaded", r1)
        self.assertIn("loaded", r2)


class TestAdapterGracefulDegradation(unittest.TestCase):
    """Test each adapter returns None gracefully when submodule is absent."""

    def test_openrewrite_unavailable_returns_none(self):
        """OpenRewriteAdapter.get_parser() returns None when submodule is absent."""
        from src.plugins.deterministic.openrewrite_adapter import OpenRewriteAdapter

        adapter = OpenRewriteAdapter()
        if not adapter.is_available:
            self.assertIsNone(adapter.get_parser())

    def test_openrewrite_plugin_type(self):
        """OpenRewriteAdapter has PARSER plugin type."""
        from src.plugins.base import PluginType
        from src.plugins.deterministic.openrewrite_adapter import OpenRewriteAdapter

        self.assertEqual(OpenRewriteAdapter.plugin_type, PluginType.PARSER)
        self.assertIn("java", OpenRewriteAdapter.supported_languages)

    def test_piranha_plugin_type(self):
        """PiranhaAdapter has PARSER plugin type."""
        from src.plugins.base import PluginType
        from src.plugins.deterministic.piranha_adapter import PiranhaAdapter

        self.assertEqual(PiranhaAdapter.plugin_type, PluginType.PARSER)
        self.assertIn("python", PiranhaAdapter.supported_languages)

    def test_piranha_unavailable_returns_none(self):
        """PiranhaAdapter.get_parser() returns None when piranha not importable."""
        # Patch polyglot_piranha to be unimportable
        with patch.dict(sys.modules, {"polyglot_piranha": None}):
            import importlib
            import src.plugins.deterministic.piranha_adapter as pa
            importlib.reload(pa)
            adapter = pa.PiranhaAdapter()
            if not adapter.is_available:
                self.assertIsNone(adapter.get_parser())

    def test_rope_plugin_type(self):
        """RopeAdapter has PARSER plugin type targeting python."""
        from src.plugins.base import PluginType
        from src.plugins.ast_codemods.rope_adapter import RopeAdapter

        self.assertEqual(RopeAdapter.plugin_type, PluginType.PARSER)
        self.assertIn("python", RopeAdapter.supported_languages)

    def test_rope_unavailable_returns_none(self):
        """RopeAdapter.get_parser() returns None when rope not importable."""
        with patch.dict(
            sys.modules,
            {
                "rope": None,
                "rope.base": None,
                "rope.base.project": None,
                "rope.base.libutils": None,
            },
        ):
            import importlib
            import src.plugins.ast_codemods.rope_adapter as ra
            importlib.reload(ra)
            adapter = ra.RopeAdapter()
            if not adapter.is_available:
                self.assertIsNone(adapter.get_parser())

    def test_jscodeshift_unavailable_returns_none(self):
        """JSCodeshiftAdapter.get_parser() returns None when node_modules absent."""
        from src.plugins.ast_codemods.jscodeshift_adapter import JSCodeshiftAdapter

        adapter = JSCodeshiftAdapter()
        if not adapter.is_available:
            self.assertIsNone(adapter.get_parser())

    def test_jscodeshift_plugin_type(self):
        """JSCodeshiftAdapter covers javascript and typescript."""
        from src.plugins.ast_codemods.jscodeshift_adapter import JSCodeshiftAdapter

        self.assertIn("javascript", JSCodeshiftAdapter.supported_languages)
        self.assertIn("typescript", JSCodeshiftAdapter.supported_languages)


if __name__ == "__main__":
    unittest.main()
