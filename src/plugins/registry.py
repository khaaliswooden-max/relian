"""Plugin registry — wires available plugins into the orchestrator."""

import importlib
import logging
from typing import TYPE_CHECKING, Dict, List

from src.plugins.base import PluginType, RelianPlugin

if TYPE_CHECKING:
    from src.core.orchestrator import MigrationOrchestrator

logger = logging.getLogger(__name__)


class PluginRegistry:
    """
    Discovers and registers plugins with a MigrationOrchestrator instance.

    Usage::

        registry = PluginRegistry()
        registry.load_all(orchestrator)

    Each plugin is tried independently; a failure in one never aborts the
    others, preserving the existing pipeline's graceful-degradation contract.
    """

    def __init__(self) -> None:
        self._plugins: List[RelianPlugin] = []
        self._registration_report: Dict[str, bool] = {}

    # ------------------------------------------------------------------
    # Plugin collection
    # ------------------------------------------------------------------

    def register_plugin(self, plugin: RelianPlugin) -> None:
        """Add a plugin instance to the registry."""
        self._plugins.append(plugin)

    def _collect_default_plugins(self) -> None:
        """
        Import and instantiate every built-in adapter.

        Import errors are caught individually so an uninitialised submodule
        does not prevent other plugins from loading.
        """
        adapters_to_try = [
            ("src.plugins.deterministic.openrewrite_adapter", "OpenRewriteAdapter"),
            ("src.plugins.deterministic.piranha_adapter", "PiranhaAdapter"),
            ("src.plugins.ast_codemods.rope_adapter", "RopeAdapter"),
            ("src.plugins.ast_codemods.jscodeshift_adapter", "JSCodeshiftAdapter"),
        ]
        for module_path, class_name in adapters_to_try:
            try:
                mod = importlib.import_module(module_path)
                cls = getattr(mod, class_name)
                self._plugins.append(cls())
            except Exception as exc:
                logger.debug("Plugin %s skipped: %s", class_name, exc)

    # ------------------------------------------------------------------
    # Wiring into the orchestrator
    # ------------------------------------------------------------------

    def load_all(self, orchestrator: "MigrationOrchestrator") -> None:
        """
        Collect all built-in plugins and wire available ones into *orchestrator*.

        Called once during orchestrator startup via
        ``orchestrator.load_plugins()``.
        """
        self._collect_default_plugins()
        for plugin in self._plugins:
            try:
                self._wire(plugin, orchestrator)
            except Exception as exc:
                logger.warning("Failed to wire plugin %s: %s", plugin.name, exc)
                self._registration_report[plugin.name] = False

    def _wire(self, plugin: RelianPlugin, orchestrator: "MigrationOrchestrator") -> None:
        """Register a single plugin's capabilities with the orchestrator."""
        if not plugin.is_available:
            logger.info("Plugin %s: backing library not available, skipping.", plugin.name)
            self._registration_report[plugin.name] = False
            return

        if plugin.plugin_type == PluginType.PARSER:
            parser = plugin.get_parser()
            if parser is not None:
                for lang in plugin.supported_languages:
                    orchestrator.register_parser(lang, parser)
                    logger.info("Plugin %s registered parser for: %s", plugin.name, lang)
                self._registration_report[plugin.name] = True
            else:
                self._registration_report[plugin.name] = False

    def get_status_report(self) -> Dict[str, object]:
        """Return a summary of what loaded and what did not."""
        return {
            "total_plugins_attempted": len(self._plugins),
            "loaded": sum(1 for v in self._registration_report.values() if v),
            "details": {
                p.name: {**p.get_metadata(), "registered": self._registration_report.get(p.name, False)}
                for p in self._plugins
            },
        }
